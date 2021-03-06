
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL_Util;

with Maths;
with Utilities;

with Blade_Types;
with E3GA;
with E3GA_Utilities;
with GA_Maths;
--  with GA_Utilities;
with Geosphere;
with Multivectors;
with Shader_Manager;

package body GA_Draw is

    Count : Integer := 0;

    procedure Draw_Circle (Palet_Type          : Palet.Colour_Palet;
                           Method              : GA_Draw.Method_Type);

    --  ------------------------------------------------------------------------

    procedure Draw_Base (Render_Program    : GL.Objects.Programs.Program;
                         Scale             : Float) is
        use GL.Objects.Buffers;
        use GA_Maths.Float_Functions;
        Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        S_Scale              : constant Single := Single (5.0 / Scale);
        Z                    : float := 0.0;
        Num_Steps            : constant int := 32;
        Rotor_Step           : constant float :=
                                 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Fan                  : Singles.Vector3_Array (1 .. Num_Steps + 1);
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Fan (1) := (S_Scale, 0.0, -0.25);
        for Count in 2 .. Num_Steps + 1 loop
            Fan (Count) := (S_Scale * Single (Cos (Z)), S_Scale * Single (Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);
        GL.Objects.Programs.Use_Program (Render_Program);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                              First => 0,
                                              Count => Num_Steps);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Base.");
            raise;
    end Draw_Base;

    --  ------------------------------------------------------------------------
    --  Draw_Bivector corresponds to draw.draw_Bivector of draw.cpp
    --  The parameter names correspond of those in draw.h!
    procedure Draw_Bivector (Render_Program                 : GL.Objects.Programs.Program;
                             Base, Normal, Ortho_1, Ortho_2 : C3GA.Vector_E3;
                             Palet_Type                     : Palet.Colour_Palet;
                             Scale                          : float := 1.0;
                             Method                         : Method_Type :=
                               Draw_Bivector_Circle) is
        use GA_Maths;
        use Float_Functions;
        use GL.Types.Singles;
        --          Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
        --          Cords                : Float_3D := (0.0, 0.0, 0.0);
        --          Translate            : Vector3 :=  (0.0, 0.0, 0.0);
        --          O2                   : Multivectors.M_Vector := Ortho_2;
        Model_Matrix          : GL.Types.Singles.Matrix4 := Identity4;
        Position_Norm         : Float := C3GA.Norm_E2 (Base);
        Normal_Norm           : constant Float := C3GA.Norm_E2 (Normal);
        Base_Coords           : constant GA_Maths.Float_3D :=
                                  C3GA.Get_Coords (Base);
        Normal_Coords         : constant GA_Maths.Float_3D :=
                                  C3GA.Get_Coords (Normal);
        Ortho_1_Coords        : constant GA_Maths.Float_3D :=
                                  C3GA.Get_Coords (Ortho_1);
        Ortho_2_Coords        : constant GA_Maths.Float_3D :=
                                  C3GA.Get_Coords (Ortho_2);
        Translation_Vector    : Vector3 :=  (0.0, 0.0, 0.0);
        MV_Normal             : constant Multivectors.M_Vector
          := Multivectors.New_Vector
            (Normal_Coords (1), Normal_Coords (2), Normal_Coords (3));
        MV_Ortho_1            : constant Multivectors.M_Vector
          := Multivectors.New_Vector
            (Ortho_1_Coords (1), Ortho_1_Coords (2), Ortho_1_Coords (3));
        MV_Ortho_2            : constant Multivectors.M_Vector
          := Multivectors.New_Vector
            (Ortho_2_Coords (1), Ortho_2_Coords (2), Ortho_2_Coords (3));
        Rotation_Matrix       : Matrix4 := Identity4;
        Scaling_Matrix        : Matrix4;
        Translation_Matrix    : Matrix4 := Identity4;
        BV_Scale              : Single := Single (Scale);
        RT                    : Multivectors.Rotor;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        --  Set position
        if Position_Norm > 0.0 then
            Translation_Vector :=
              (Single (Base_Coords (1)), Single (Base_Coords (2)),
               Single (Base_Coords (3)));
            Translation_Matrix := Maths.Translation_Matrix (Translation_Vector);
        end if;
        Shader_Manager.Set_Translation_Matrix (Translation_Matrix);

        if Method /= Draw_Bivector_Parallelogram and then
          Method /= Draw_Bivector_Parallelogram_No_Vectors then

            --  Rotate e3 to normal direction
            if Normal_Norm > 0.0 then
                Rotation_Matrix := GA_Maths.Vector_Rotation_Matrix ((0.0, 0.0, 1.0), Normal);
            end if;
            RT := E3GA_Utilities.Rotor_Vector_To_Vector
              (Multivectors.Basis_Vector (Blade_Types.E3_e3), MV_Normal);
            GL_Util.Rotor_GL_Multiply (RT, Rotation_Matrix);

        else  --  Draw_Bivector_Parallelogram
            Position_Norm :=
              C3GA.Norm_E2 (C3GA.To_VectorE3GA
                            ((Multivectors.Outer_Product (MV_Ortho_1,
                               MV_Ortho_2))));
            BV_Scale := Single (Sqrt (Pi / Position_Norm)) * BV_Scale;
        end if;

        Shader_Manager.Set_Rotation_Matrix (Rotation_Matrix);
        Scaling_Matrix := Maths.Scaling_Matrix ((BV_Scale, BV_Scale, BV_Scale));
        Model_Matrix := Scaling_Matrix * Model_Matrix;
        Shader_Manager.Set_Model_Matrix (Model_Matrix);

        case Method is
            when Draw_Bivector_Circle |
                 Draw_Bivector_Circle_Outline =>
                Draw_Circle (Palet_Type, Method);
            when others => null;
        end case;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    --     procedure Draw_Bivector (Render_Program         : GL.Objects.Programs.Program;
    --                              Base, Ortho_1, Ortho_2 : Multivectors.M_Vector;
    --                              Palet_Type             : Palet.Colour_Palet;
    --                              Scale                  : float := 1.0;
    --                              Method                 : Method_Type := Draw_Bivector_Circle) is
    --        use GA_Maths;
    --        use GL.Types.Singles;
    --
    --        Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    --        --          Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
    --        Scale_S              : constant GL.Types.Single := GL.Types.Single (Scale);
    --        Translate            : Vector3 :=  (0.0, 0.0, 0.0);
    --        --          O2                   : Multivectors.M_Vector := Ortho_2;
    --        --          Model_Matrix    : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    --        MVP_Matrix           : Matrix4 := Singles.Identity4;
    --        Scaled               : GL.Types.Single;
    --        E2_Norm              : Float;
    --     begin
    --        GL.Objects.Programs.Use_Program (Render_Program);
    --        Vertex_Array_Object.Initialize_Id;
    --        Vertex_Array_Object.Bind;
    --
    --        Shader_Manager.Set_Ambient_Colour ((1.0, 1.0, 1.0, 1.0));
    --
    --        --          MVP_Matrix := Model_Matrix;
    --        Translate := (Single (C3GA.e1 (Base)),
    --                      Single (C3GA.e2 (Base)),
    --                      Single (C3GA.e3 (Base)));
    --        E2_Norm := Multivectors.Norm_E2 (Base);
    --        if  E2_Norm /= 0.0  then
    --           MVP_Matrix := Maths.Translation_Matrix (Translate) * MVP_Matrix;
    --        end if;
    --
    --        if  Method = Draw_Bivector_Parallelogram and then
    --          Method = Draw_Bivector_Parallelogram_No_Vectors then
    --           MVP_Matrix := Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S)) * MVP_Matrix;
    --        else
    --           E2_Norm := Multivectors.Norm_E2 (Multivectors.Outer_Product (Ortho_1, Ortho_2));
    --           Scaled := GL.Types.Single (Scale * Float_Functions.Sqrt (pi / E2_Norm));
    --           MVP_Matrix := Maths.Scaling_Matrix ((Scaled, Scaled, Scaled))
    --             * MVP_Matrix;
    --        end if;
    --
    --        Case Method is
    --           when Draw_Bivector_Circle |
    --                Draw_Bivector_Circle_Outline =>
    --              Draw_Circle (Render_Program, MVP_Matrix, Palet_Type, Method);
    --           when others => null;
    --        end case;
    --     exception
    --        when  others =>
    --           Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
    --           raise;
    --     end Draw_Bivector;

    --  ----------------------------------------------------------------------

    --      procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
    --               Model_Matrix : GL.Types.Singles.Matrix4;
    --               MV             : E2GA.Multivector;
    --               Colour         : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
    --               Scale          : GL.Types.Single) is
    --      --   L          : boolean;
    --          Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
    --          --   X          : float;
    --          --   Y          : float;
    --          MVP_Matrix : Singles.Matrix4 := Singles.Identity4;
    --          Rotor      : E2GA.Rotor;
    --
    --      begin
    --
    --      GL.Objects.Programs.Use_Program (Render_Program);
    --      Vertex_Array_Object.Initialize_Id;
    --      Vertex_Array_Object.Bind;
    --          --          if E3GA.Norm_E2 (MV).M_C1 = 0.0  then
    --          --              Maths.Translation_Matrix (Get_Coord_1 (Base),
    --          --                Get_Coord_2 (Base), Get_Coord_3 (Base)) * MVP_Matrix;
    --          --          end if;
    --      exception
    --          when  others =>
    --              Put_Line ("An exception occurred in Draw_Object.Draw_Multivector.");
    --              raise;
    --      end Draw_Multivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Circle (Palet_Type          : Palet.Colour_Palet;
                           Method              : GA_Draw.Method_Type) is
        use GA_Maths;
        use GA_Maths.Float_Functions;
        use GL.Objects.Buffers;
        use Singles;
        use Palet;
        type Circle_Part is (Back_Part, Front_Part, Outline_Part);
        Vertex_Array   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

        procedure Draw_Part (Part : Circle_Part) is
            Vertex_Buffer  : GL.Objects.Buffers.Buffer;
            Normals_Buffer : GL.Objects.Buffers.Buffer;
            Num_Steps      : constant int := 256;
            VB_Size        : Int := Num_Steps;
            Angle          : float := 0.0;
            Rotor_Step     : constant float :=
                               2.0 * Ada.Numerics.Pi / float (Num_Steps);
            Norm_Z         : Single;
            Draw_Hooks     : constant Boolean :=
                               Part = Outline_Part and Get_Draw_Mode.Orientation;
        begin
            Vertex_Array.Initialize_Id;
            Vertex_Array.Bind;
            Vertex_Buffer.Initialize_Id;
            Normals_Buffer.Initialize_Id;

            case Part is
                when Back_Part | Outline_Part =>
                    Norm_Z := 1.0;
                when Front_Part =>
                    Norm_Z := -1.0;
            end case;

            if Draw_Hooks then
                VB_Size := Num_Steps + 12;
            end if;

            declare
                Fan            : Vector3_Array (1 .. VB_Size);
                Normal         : Vector3_Array (1 .. VB_Size) :=
                                   (others => (0.0, 0.0, 1.0));
                Hooks          : Vector3_Array (1 .. 2) := ((1.0, 0.0, 0.0),
                                                            (1.0, -0.5, 0.0));
                Hook_Rotation  : constant Matrix3 := Rotation_Matrix
                  (Maths.Radian (Ada.Numerics.Pi / 3.0), (0.0, 0.0, 1.0));
                Hook_Index     : Int := 1;
            begin
                for Count in 1 .. Num_Steps loop
                    case Part is
                    when Back_Part | Outline_Part =>
                        Fan (Count) := (Single (Cos (Angle)), Single (Sin (Angle)), 0.0);
                    when Front_Part =>
                        Fan (Count) := (-Single (Cos (Angle)), Single (Sin (Angle)), 0.0);
                    end case;
                    Normal (Count) := (0.0, 0.0, Norm_Z);
                    Angle := Angle + Rotor_Step;
                end loop;

                if Draw_Hooks then
                    --  draw six 'hooks' along the edge of the circle
                    while Hook_Index < 12 loop
                        Fan (Num_Steps + Hook_Index) := Hooks (1);
                        Fan (Num_Steps + Hook_Index + 1) := Hooks (2);
                        Normal (Num_Steps + Hook_Index) := (0.0, 0.0, Norm_Z);
                        Normal (Num_Steps + Hook_Index + 1) := (0.0, 0.0, Norm_Z);
                        Hooks (1) := Hook_Rotation * Hooks (1);
                        Hooks (2) := Hook_Rotation * Hooks (2);
                        Hook_Index := Hook_Index + 2;
                    end loop;
                end if;

                Vertex_Array.Bind;
                Array_Buffer.Bind (Vertex_Buffer);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

                Array_Buffer.Bind (Normals_Buffer);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Normal, Static_Draw);

                GL.Attributes.Enable_Vertex_Attrib_Array (0);
                Array_Buffer.Bind (Vertex_Buffer);
                GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

                GL.Attributes.Enable_Vertex_Attrib_Array (1);
                Array_Buffer.Bind (Normals_Buffer);
                GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 0);

                if Part = Back_Part or Part = Front_Part then
                    if Part = Back_Part and then Get_Draw_Mode.Orientation then
                        GL.Rasterization.Set_Polygon_Mode (GL.Rasterization.Line);
                    end if;
                    GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                                          First => 0,
                                                          Count => VB_Size);
                else  --  Outline part
                    GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Line_Loop,
                                                          First => 0,
                                                          Count => Num_Steps);
                    if Draw_Hooks then
                        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                                              First => Num_Steps,
                                                              Count => 12);
                    end if;
                end if;
                GL.Attributes.Disable_Vertex_Attrib_Array (0);
                GL.Attributes.Disable_Vertex_Attrib_Array (1);
            end;  --  declare block
        end Draw_Part;

    begin
        Shader_Manager.Set_Model_Matrix (Identity4);
        if (Method = Draw_Bivector_Circle)  and then
          (Palet_Type = Is_Null or
             Palet.Foreground_Alpha (Palet_Type) > 0.0000001) then
            Draw_Part (Back_Part);
            Draw_Part (Front_Part);
        end if;

        if Palet_Type /= Is_Null then
            Set_Outline_Colour (Palet_Type);
        end if;
        Draw_Part (Outline_Part);

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Circle.");
            raise;
    end Draw_Circle;

    --  ------------------------------------------------------------------------

    procedure Draw_Cone (Render_Program    : GL.Objects.Programs.Program;
                         Scale             : Float) is
        use GL.Objects.Buffers;
        use GA_Maths.Float_Functions;
        Vertex_Array   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer  : GL.Objects.Buffers.Buffer;
        S_Scale        : constant Single := Single (5.0 / Scale);
        Z              : float := 0.0;
        Num_Steps      : constant int := 256;
        Rotor_Step     : constant float :=
                           2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Fan            : Singles.Vector3_Array (1 .. Num_Steps);
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;

        Fan (1) := (0.0, 0.0, 0.0);
        for Count in 2 .. Num_Steps loop
            Fan (Count) := (S_Scale * Single (Cos (Z)), S_Scale * Single (Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

        GL.Objects.Programs.Use_Program (Render_Program);
        --          Shader_Manager.Set_Model_Matrix (Model_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                              First => 0,
                                              Count => Num_Steps);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Cone.");
            raise;
    end Draw_Cone;

    --  ------------------------------------------------------------------------

    procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
                         Direction      : C3GA.Vector_E3;
                         Weight         : Float := 1.0) is
        use GL.Objects.Buffers;
        use GL.Types.Singles;
        use Shader_Manager;
        Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        --  Scale_Constant and Step_Size are used for building Line_Strip
        Scale_Constant       : constant Single := Single (Palet.Line_Length);  --  6.0
        Step_Size            : constant Single := 0.1;
        Step_Length          : constant Single := Scale_Constant * Step_Size;
        Num_Steps            : constant Int := Int (2.0 / Step_Size + 0.5) + 1;
        Num_Vertices         : constant Int := Num_Steps + 1;
        Length_Vertices      : Singles.Vector3_Array (1 .. Num_Steps);
        C_Steps              : constant Int := Int (1.0 / Step_Size + 0.5) + 1;
        C_Rotation_Matrix    : Matrix4 := Identity4;
        Rotation_Matrix      : Matrix4 := Identity4;
        Translation_Matrix   : Matrix4 := Identity4;
        Scale_Matrix         : Matrix4 :=
                                 Maths.Scaling_Matrix (Single (Weight));
        Model_Matrix         : Matrix4 := Identity4;
        Z                    : Single := -Scale_Constant;
        C                    : Single := 0.0;
        Num_C_Vertices       : constant Int := 3 * C_Steps + 1;
        C_Vertices           : Singles.Vector3_Array (1 .. Num_C_Vertices);
        C_Index              : Int := 0;
        C_Vertex1            : constant Singles.Vector3 := (-0.25, 0.0, -1.0);
        C_Vertex2            : constant Singles.Vector3 := (0.0, 0.0, 0.0);
        C_Vertex3            : constant Singles.Vector3 := (0.25, 0.0, -1.0);
    begin
        --  aPoint and Direction are model coordinates
        GL.Objects.Programs.Use_Program (Render_Program);
        --          Utilities.Print_Vector ("GA_Draw.Draw_Line aPoint", aPoint);
        --          Utilities.Print_Vector ("GA_Draw.Draw_Line Direction", Direction);
        for index in 1 .. Num_Steps  loop
            Length_Vertices (index) := (0.0, 0.0, Z);
            Z := Z + Step_Length;
        end loop;

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Length_Vertices, Static_Draw);

        --  rotate e3 to line direction
        Rotation_Matrix :=
          GA_Maths.Vector_Rotation_Matrix ((0.0, 0.0, 1.0), Direction);
        Set_Rotation_Matrix (Rotation_Matrix);
        Set_Translation_Matrix (Translation_Matrix);
        Model_Matrix := Scale_Matrix * Model_Matrix;
        --  translate to point on line
        --        Translation_Matrix :=
        --          Maths.Translation_Matrix ((aPoint (GL.X), aPoint (GL.Y), aPoint (GL.Z)));
        --        Model_Matrix := Translation_Matrix * Model_Matrix;
        Set_Model_Matrix (Model_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Line_Strip,
                                              First => 0, Count => Num_Vertices);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

        if Palet.Get_Draw_Mode.Orientation then
            if Palet.Get_Draw_Mode.Magnitude then
                Scale_Matrix :=
                  Maths.Scaling_Matrix (Single (0.5 * Abs (Weight)));
            else
                Scale_Matrix := Maths.Scaling_Matrix (0.5);
            end if;
            Set_Model_Matrix (Scale_Matrix);

            Translation_Matrix :=
              Maths.Translation_Matrix ((0.0, 0.0, -Scale_Constant));

            while C < 1.0  loop
                C_Index := C_Index + 1;
                C_Vertices (C_Index) := C_Vertex1;
                C_Index := C_Index + 1;
                C_Vertices (C_Index) := C_Vertex2;
                C_Index := C_Index + 1;
                C_Vertices (C_Index) := C_Vertex3;
                C := C + Step_Size;

                Utilities.Load_Vertex_Buffer (Array_Buffer, C_Vertices, Static_Draw);
                Set_Rotation_Matrix (C_Rotation_Matrix);
                Set_Translation_Matrix (Translation_Matrix);

                GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
                GL.Attributes.Enable_Vertex_Attrib_Array (0);

                GL.Objects.Vertex_Arrays.Draw_Arrays
                  (Mode  => Line_Strip, First => 0, Count => Num_C_Vertices);
                GL.Attributes.Disable_Vertex_Attrib_Array (0);

                C_Rotation_Matrix :=
                  Maths.Rotation_Matrix (Maths.Degree (90.0), (0.0, 0.0, 1.0));
                Translation_Matrix :=
                  Maths.Translation_Matrix ((0.0, 0.0, 2.0 * Step_Length));
            end loop;
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
            raise;
    end Draw_Line;

    --  ------------------------------------------------------------------------

    procedure Draw_Parallelepiped (Render_Program    : GL.Objects.Programs.Program;
                                   Model_Matrix      : GL.Types.Singles.Matrix4;
                                   MVC               : Multivector_Analyze.E3_Vector_Array;
                                   Scale             : Float;
                                   Method            : Method_Type) is
        use GL.Objects.Buffers;
        use Singles;
        Vertex_Array     : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer    : GL.Objects.Buffers.Buffer;
        Normals_Buffer   : GL.Objects.Buffers.Buffer;
        Scale_Matrix     : Matrix4;
        Scale_Sign       : GL.Types.Single;
        Polygon          : constant Ints.Vector4_Array (1 .. 6) :=
                             ((0, 1, 5, 4),
                              (0, 4, 7, 3),
                              (4, 5, 6, 7),
                              (1, 2, 6, 5),
                              (6, 2, 3, 7),
                              (0, 3, 2, 1));
        Vertex           : Vector3_Array (1 .. 8) :=
                             (others => (others => 0.0));
        GL_Vertices      : Vector3_Array (1 .. 4) :=
                             (others => (others => 0.0));
        aVertex          : Vector3;
        Vertex_Vectors   : constant Ints.Vector3_Array (1 .. 8) :=
                             ((-1, -1, -1),  --  -
                              (0, -1, -1),   --  0
                              (0, 1, -1),    --  0 + 1
                              (1, -1, -1),   --  1
                              (2, -1, -1),   --  2
                              (0, 2, -1),    --  0 + 2
                              (0, 1, 2),     --  0 + 1 + 2
                              (1, 2, -1));   --  1 + 2
        Vertex_Index     : Int := 0;
        GL_Normals       : Vector3_Array (1 .. 6) :=
                             (others => (others => 0.0));
        V1               : E3GA.E3_Vector;
        V2               : E3GA.E3_Vector;
        V3               : E3GA.E3_Vector;
        Stride           : constant Int := 0;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        --          for index in 1 .. MVC'Length loop
        --              VC (Int (index)) := E3GA.Get_Coords (MVC (index));
        --          end loop;
        if Scale >= 0.0 then
            Scale_Sign := 1.0;
        else
            Scale_Sign := -1.0;
        end if;
        if Palet.Get_Draw_Mode.Orientation then
            Scale_Matrix := Maths.Scaling_Matrix (Scale_Sign);
        end if;

        for Row in Int range 1 .. 8 loop
            aVertex := (0.0, 0.0, 0.0);
            for Col in GL.Index_Homogeneous range GL.X .. GL.Z loop
                Vertex_Index := Vertex_Vectors (Row) (Col);
                if Vertex_Index >= 0 then
                    aVertex := aVertex + MVC (Integer (Vertex_Index));
                    Vertex (Row) := aVertex;
                end if;
            end loop;
        end loop;

        for Index in GL_Normals'Range loop
            V1 := Vertex (Polygon (Index) (GL.X));
            V2 := Vertex (Polygon (Index) (GL.Y));
            V3 := Vertex (Polygon (Index) (GL.W));
            GL_Normals (Index) :=
              (Scale_Sign * E3GA.Outer_Product ((V2 - V1), (V3 - V1)));

            if Scale >= 0.0 then
                for GL_Index in Int range 1 .. 3 loop
                    GL_Vertices (GL_Index) :=
                      Vertex (Polygon (Index)
                              (GL.Index_Homogeneous'Enum_Val (GL_Index)));
                end loop;
            else
                for GL_Index in reverse Int range  3 .. 1 loop
                    GL_Vertices (GL_Index) :=
                      Vertex (Polygon (Index)
                              (GL.Index_Homogeneous'Enum_Val (GL_Index)));
                end loop;
            end if;
        end loop;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, GL_Vertices, Static_Draw);

        Normals_Buffer.Initialize_Id;
        Array_Buffer.Bind (Normals_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, GL_Normals, Static_Draw);

        if Method = Draw_TV_Parellelepiped then
            Draw_Parallelepiped
              (Render_Program, Model_Matrix, MVC, Scale, Method);
        end if;

        GL.Objects.Programs.Use_Program (Render_Program);
        Shader_Manager.Set_Ambient_Colour ((1.0, 1.0, 1.0, 1.0));
        Shader_Manager.Set_Model_Matrix (Scale_Matrix * Model_Matrix);

        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, Stride, 0);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                              First => 0,
                                              Count => 1 * 3);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Parallelepiped.");
            raise;
    end Draw_Parallelepiped;

    --  ------------------------------------------------------------------------
    --  Based on draw.cpp DrawState::drawSphere(e3ga::mv::Float normal)
    procedure Draw_Sphere (Render_Program : GL.Objects.Programs.Program;
                           Normal         : GL.Types.Single) is
        use Geosphere;
        Sphere : constant Geosphere.Geosphere := Palet.Current_Sphere;
    begin
        Count := Count + 1;
        Geosphere.New_Sphere_List (Sphere);
        --  gsDraw(m_sphere, 0.0f);
        Put_Line ("GA_Draw.Draw_Sphere calling GS_Draw Count : " &
                    Integer'Image (Count));
        Geosphere.GS_Draw (Render_Program, Sphere);

        if Normal = 0.0 then
            Draw_Sphere_List (Render_Program);
        else
            Geosphere.GS_Draw (Render_Program, Sphere, Normal);
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Sphere.");
            raise;
    end Draw_Sphere;

    --  ------------------------------------------------------------------------

    --     procedure Draw_Sphere (Render_Program : GL.Objects.Programs.Program;
    --                            Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
    --                            Normal : E3GA.Vector; Scale : float := 1.0;
    --                            Colour : GL.Types.Colors.Color) is
    --     begin
    --        if G_Draw_State.Max_Vertices = 0 then
    --           Geosphere.GS_Compute (G_Draw_State.M_Sphere, 4);
    --        end if;
    --     exception
    --        when  others =>
    --           Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
    --           raise;
    --     end Draw_Sphere;

    --  ------------------------------------------------------------------------
    --  Based on draw.cpp drawTriVector
    procedure Draw_Trivector (Render_Program    : GL.Objects.Programs.Program;
                              Base              : C3GA.Vector_E3;
                              Scale             : float := 1.0;
                              Palet_Type        : Palet.Colour_Palet;
                              Method            : Method_Type := Draw_TV_Sphere) is
--          use GL.Types.Singles;
--          use Ada.Numerics.Elementary_Functions;  --  needed for fractional powers
--          use GA_Maths;
        --        Vector_Base       : constant Singles.Vector3 := (0.0, 0.0, 0.0);
--          Scale_Sign        : Float;
--          P_Scale           : Float;
        Normal            : Single;
--          Base_Coords       : constant GA_Maths.Float_3D := C3GA.Get_Coords (Base);
--          Translation       : Singles.Vector3;
--          MV_Matrix         : Matrix4 := Identity4;
    begin
--          if Scale >= 0.0 then
--              Scale_Sign := 1.0;
--          else
--              Scale_Sign := -1.0;
--          end if;

        if Method = Draw_TV_Parellelepiped or
          Method = Draw_TV_Parellelepiped_No_Vectors then
            Put_Line ("GA_Draw.Draw_Trivector, Draw_TV_Parellelepiped or Draw_TV_Parellelepiped_No_Vectors");
            Draw_Trivector (Render_Program, Base, Scale,
                            Palet_Type, Draw_TV_Sphere) ;
--              P_Scale :=  Scale_Sign * ((Scale_Sign * Scale) ** 1.0 / 3.0);
--          else
--              P_Scale :=  Scale_Sign * ((Scale_Sign * Scale / (4.0 / 3.0 * GA_Maths.PI)) ** 1.0 / 3.0);
        end if;

--          if C3GA.Norm_e2 (Base) /= 0.0 then
--              Translation := (Single (Base_Coords (1)), Single (Base_Coords (2)), Single (Base_Coords (3)));
--              MV_Matrix := Maths.Translation_Matrix (Translation) * MV_Matrix;
--          end if;
--
--          MV_Matrix := Maths.Scaling_Matrix (Single (P_Scale)) * MV_Matrix;

        case Method is
        when Draw_TV_Sphere =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Sphere.");
            if Palet.Get_Draw_Mode.Orientation then
                Normal := 0.1;
            else
                Normal := 0.0;
            end if;
            --  g_drawState.drawSphere (s)
            Draw_Sphere (Render_Program, Normal);
        when Draw_TV_Cross =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Cross.");
            null;
        when Draw_TV_Curly_Tail =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Curly_Tail.");
            null;
        when Draw_TV_Parellelepiped =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Parellelepiped.");
            --              Draw_Vector (Render_Program    => Render_Program,
            --                           Model_Matrix => MV_Matrix,
            --                           Tail              => Vector_Base,
            --                           Direction         => ,
            --                           Scale             => );
        when Draw_TV_Parellelepiped_No_Vectors =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Parellelepiped_No_Vectors.");
            --              Draw_Parallelepiped (Render_Program, MV_Matrix, V, Scale,
            --                                   Draw_TV_Parellelepiped_No_Vectors);
        when others => null;
            Put_Line ("GA_Draw.Draw_Trivector others.");
        end case;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
            raise;
    end Draw_Trivector;

    --  ------------------------------------------------------------------------

    procedure Draw_Trivector (Render_Program    : GL.Objects.Programs.Program;
                              Base              : C3GA.Vector_E3; Scale : float := 1.0;
                              V                 : Multivector_Analyze.E3_Vector_Array;
                              --                               Palet_Type        : Palet.Colour_Palet;
                              Method            : Method_Type := Draw_TV_Sphere) is
        use GL.Types.Singles;
        use Ada.Numerics.Elementary_Functions;  --  needed for fractional powers
        use GA_Maths;
        Scale_Sign        : Float;
        P_Scale           : Float;
        Normal            : Single;
        Base_Coords       : constant GA_Maths.Float_3D := C3GA.Get_Coords (Base);
        Translation       : Singles.Vector3;
        MV_Matrix         : Matrix4 := Identity4;
    begin
        if Scale >= 0.0 then
            Scale_Sign := 1.0;
        else
            Scale_Sign := -1.0;
        end if;

        if Method = Draw_TV_Parellelepiped or
          Method = Draw_TV_Parellelepiped_No_Vectors then
            Put_Line ("GA_Draw.Draw_Trivector, Draw_TV_Parellelepiped or Draw_TV_Parellelepiped_No_Vectors");
            P_Scale :=  Scale_Sign * ((Scale_Sign * Scale) ** 1.0 / 3.0);
        else
            P_Scale :=  Scale_Sign * ((Scale_Sign * Scale / (4.0 / 3.0 * GA_Maths.PI)) ** 1.0 / 3.0);
        end if;

        if C3GA.Norm_e2 (Base) /= 0.0 then
            Translation := (Single (Base_Coords (1)), Single (Base_Coords (2)), Single (Base_Coords (3)));
            MV_Matrix := Maths.Translation_Matrix (Translation) * MV_Matrix;
        end if;

        MV_Matrix := Maths.Scaling_Matrix (Single (P_Scale)) * MV_Matrix;

        case Method is
        when Draw_TV_Sphere =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Sphere.");
            if Palet.Get_Draw_Mode.Orientation then
                Normal := 0.1;
            else
                Normal := 0.0;
            end if;
            --  g_drawState.drawSphere (s)
            Draw_Sphere (Render_Program, Normal);
        when Draw_TV_Cross =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Cross.");
            null;
        when Draw_TV_Curly_Tail =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Curly_Tail.");
            null;
        when Draw_TV_Parellelepiped =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Parellelepiped.");
            Draw_Parallelepiped (Render_Program, MV_Matrix, V, Scale,
                                 Draw_TV_Parellelepiped);
        when Draw_TV_Parellelepiped_No_Vectors =>
            Put_Line ("GA_Draw.Draw_Trivector Draw_TV_Parellelepiped_No_Vectors.");
            --              Draw_Parallelepiped (Render_Program, MV_Matrix, V, Scale,
            --                                   Draw_TV_Parellelepiped_No_Vectors);
        when others => null;
            Put_Line ("GA_Draw.Draw_Trivector others.");
        end case;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
            raise;
    end Draw_Trivector;

    --  ------------------------------------------------------------------------

    procedure Draw_Vector (Render_Program    : GL.Objects.Programs.Program;
                           Tail, Direction   : C3GA.Vector_E3;
                           Scale             : float := 1.0) is
        use GL.Culling;
        use GL.Toggles;
        use GL.Types.Singles;
        use Multivectors;
        use Maths.Single_Math_Functions;

        Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        MV_Matrix            : Matrix4 := Identity4;
        Dir_Coords           : constant GA_Maths.Float_3D :=
                                 C3GA.Get_Coords (Direction);
        --        GL_Tail              : constant Vector3 := GL_Util.To_GL (Tail);
        --        GL_Dir               : constant Vector3 := GL_Util.To_GL (Direction);
        MV_Dir               : constant Multivectors.M_Vector :=
                                 New_Vector (Dir_Coords (1), Dir_Coords (2), Dir_Coords (3));
        aRotor               : Rotor;
        Saved_Cull_Face      : constant Face_Selector := Cull_Face;
        Saved_Front_Face     : constant GL.Types.Orientation := GL.Culling.Front_Face;
    begin
        if Scale /= 0.0 then
            Vertex_Array_Object.Initialize_Id;
            Vertex_Array_Object.Bind;

            GL.Objects.Programs.Use_Program (Render_Program);

            if C3GA.Norm_e2 (Tail) /= 0.0 then
                MV_Matrix := Maths.Translation_Matrix (Tail) * MV_Matrix;
            end if;
            MV_Matrix := Maths.Scaling_Matrix (Single (Scale)) * MV_Matrix;

            --  Draw the stick of the vector
            Draw_Line (Render_Program, (0.98 * Direction (GL.X),
                       0.98 * Direction (GL.Y), 0.98 * Direction (GL.Z)), Scale);

            --  Setup translation matrix for arrow head
            --  rotate e3 to vector direction
            MV_Matrix := Maths.Translation_Matrix (Direction) * MV_Matrix;
            aRotor := E3GA_Utilities.Rotor_Vector_To_Vector
              (Basis_Vector (Blade_Types.E3_e3), MV_Dir);
            if Scale > 1.2 then
                MV_Matrix := Maths.Scaling_Matrix (Single (1.2 / Scale)) * MV_Matrix;
                MV_Matrix := Maths.Scaling_Matrix (1.1 / Sqrt (Single (Scale))) * MV_Matrix;
            end if;

            GL_Util.Rotor_GL_Multiply (aRotor, MV_Matrix);

            if C3GA.Norm_e2 (Tail) /= 0.0 then
                MV_Matrix := Maths.Translation_Matrix (Tail) * MV_Matrix;
            end if;
            --  Translate to head of vector
            MV_Matrix := Maths.Translation_Matrix
              (Single (Scale) * Direction) * MV_Matrix;
            Shader_Manager.Set_Model_Matrix (MV_Matrix);

            Enable (Cull_Face);
            Set_Front_Face (GL.Types.Clockwise);
            Set_Cull_Face (Front);

            Draw_Cone (Render_Program, Scale);
            Draw_Base (Render_Program, Scale);

            Set_Front_Face (Saved_Front_Face);
            Set_Cull_Face (Saved_Cull_Face);
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Vector.");
            raise;
    end Draw_Vector;

    --  -----------------------------------------------------------------------

end GA_Draw;
