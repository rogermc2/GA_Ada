
--  Based on libgasandbox.draw.h and draw.cpp

--  with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
--  with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Culling;
--  with GL.Immediate;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Window;
with Utilities;

with Maths;
with GA_Maths;
--  with GA_Utilities;

--  with Blade;
with Blade_Types;
with E3GA;
with E3GA_Utilities;
--  with Geosphere;
with GL_Util;

package body GA_Draw is

    Palet          : Colour_Palet;
    pragma Unreferenced (Palet);
    G_Draw_State   : Draw_State;

    procedure Draw_Circle (Render_Program    : GL.Objects.Programs.Program;
                           Model_View_Matrix : GL.Types.Singles.Matrix4);

    --  ------------------------------------------------------------------------

    procedure Draw_Base (Render_Program    : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Scale             : Float) is

        use GL.Objects.Buffers;
        use GA_Maths.Float_Functions;
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        S_Scale              : constant Single := Single (5.0 / Scale);
        Z               : float := 0.0;
        Num_Steps       : constant int := 32;
        Rotor_Step      : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Vertex_Buffer   : GL.Objects.Buffers.Buffer;
        Fan             : Singles.Vector3_Array (1 .. Num_Steps + 1);
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Fan (1) := (S_Scale, 0.0, -0.25);
        for Count in 2 .. Num_Steps + 1 loop
            Fan (Count) := (S_Scale * Single (Cos (Z)), S_Scale * Single (Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

        Init_Projection_Matrix (Projection_Matrix);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
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
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Translation_Matrix : GL.Types.Singles.Matrix4;
                             Normal, Ortho_1, Ortho_2 : Multivectors.Vector;
                             Colour : GL.Types.Colors.Color; Scale  : float := 1.0;
                             Method : Bivector_Method_Type := Draw_Bivector_Circle) is
        use GA_Maths;
        use GL.Types.Singles;
        Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        --          Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
        Scale_S              : constant GL.Types.Single := GL.Types.Single (Scale);
        --          Cords                : Array_3D := (0.0, 0.0, 0.0);
        --          Translate            : Vector3 :=  (0.0, 0.0, 0.0);
        --          O2                   : Multivectors.Vector := Ortho_2;
        MVP_Matrix           : Matrix4 := Singles.Identity4;
        Scaled               : GL.Types.Single;
        Normed_E2            : Float;
        RT                   : Multivectors.Rotor;
        OK                   : Boolean := True;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Vertex_Array_Object.Initialize_Id;
        Vertex_Array_Object.Bind;

        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Init_Projection_Matrix (Projection_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
        GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

        if  Method /= Draw_Bivector_Parallelogram and then
          Method /= Draw_Bivector_Parallelogram_No_Vectors then
            MVP_Matrix := Translation_Matrix * Maths.Scaling_Matrix (Scale_S);
            --  Rotate e3 to normal direction
            RT := E3GA_Utilities.Rotor_Vector_To_Vector
              (Multivectors.Basis_Vector (Blade_Types.E3_e3), Normal);
            OK := GL_Util.Rotor_GL_Multiply (RT, MVP_Matrix);
        else
            Normed_E2 := Multivectors.Norm_E2 (Multivectors.Outer_Product (Ortho_1, Ortho_2));
            Scaled := GL.Types.Single (Scale * Float_Functions.Sqrt (Pi / Normed_E2));
            MVP_Matrix := Translation_Matrix * Maths.Scaling_Matrix ((Scaled, Scaled, Scaled))
              * MVP_Matrix;
        end if;

        if OK then
            case Method is
            when Draw_Bivector_Circle |
                 Draw_Bivector_Circle_Outline =>
                Draw_Circle (Render_Program, MVP_Matrix);
            when others => null;
            end case;
        else
            Put_Line ("Draw_Object.Draw_Bivector, Invertible rotor RT.");
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Base, Ortho_1, Ortho_2 : Multivectors.Vector;
                             Colour : GL.Types.Colors.Color; Scale  : float := 1.0;
                             Method : Bivector_Method_Type := Draw_Bivector_Circle) is
        use GA_Maths;
        use GL.Types.Singles;

        Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        --          Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
        Scale_S              : constant GL.Types.Single := GL.Types.Single (Scale);
        Translate            : Vector3 :=  (0.0, 0.0, 0.0);
        --          O2                   : Multivectors.Vector := Ortho_2;
        --          Model_View_Matrix    : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
        MVP_Matrix           : Matrix4 := Singles.Identity4;
        Scaled               : GL.Types.Single;
        E2_Norm              : Float;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Vertex_Array_Object.Initialize_Id;
        Vertex_Array_Object.Bind;

        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Init_Projection_Matrix (Projection_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
        GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

        --          MVP_Matrix := Model_View_Matrix;
        Translate := (Single (C3GA.e1 (Base)),
                      Single (C3GA.e2 (Base)),
                      Single (C3GA.e3 (Base)));
        E2_Norm := Multivectors.Norm_E2 (Base);
        if  E2_Norm /= 0.0  then
            MVP_Matrix := Maths.Translation_Matrix (Translate) * MVP_Matrix;
        end if;

        if  Method = Draw_Bivector_Parallelogram and then
          Method = Draw_Bivector_Parallelogram_No_Vectors then
            MVP_Matrix := Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S)) * MVP_Matrix;
        else
            E2_Norm := Multivectors.Norm_E2 (Multivectors.Outer_Product (Ortho_1, Ortho_2));
            Scaled := GL.Types.Single (Scale * Float_Functions.Sqrt (pi / E2_Norm));
            MVP_Matrix := Maths.Scaling_Matrix ((Scaled, Scaled, Scaled))
              * MVP_Matrix;
        end if;

        Case Method is
            when Draw_Bivector_Circle |
                 Draw_Bivector_Circle_Outline =>
                Draw_Circle (Render_Program, MVP_Matrix);
            when others => null;
        end case;
    exception
        when  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    --      procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
    --               Model_View_Matrix : GL.Types.Singles.Matrix4;
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

    procedure Draw_Circle (Render_Program    : GL.Objects.Programs.Program;
                           Model_View_Matrix : GL.Types.Singles.Matrix4) is
        use GA_Maths;
        use GL.Objects.Buffers;
        use GA_Maths.Float_Functions;

        type Circle_Part is (Back_Part, Front_Part, Outline_Part);

        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        Angle                : float := 0.0;
        Num_Steps            : constant int := 256;
        Rotor_Step           : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        Fan                  : Singles.Vector3_Array (1 .. Num_Steps);

        procedure Draw_Part (Part : Circle_Part) is
        --           Normal : Multivector.Vector;
        --              Norm_Z : float;
        begin
            --              Case Part is
            --              when Back_Part | Outline_Part => Norm_Z := 1.0;
            --              when Front_Part => Norm_Z := -1.0;
            --              end case;
            --           Multivector.Add_Blade (Normal, Blade.E3_e3, Norm_Z);
            --           E3GA.Set_Coords (Normal, 0.0, 0.0, Norm_Z);

            Fan (1) := (0.0, 0.0, 0.0);
            for Count in 2 .. Num_Steps loop
                Fan (Count) := (Single (Cos (Angle)), Single (Sin (Angle)), 0.0);
                Angle := Angle + Rotor_Step;
            end loop;

            Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

            Init_Projection_Matrix (Projection_Matrix);
            GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
            GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

            GL.Attributes.Enable_Vertex_Attrib_Array (0);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
            if Part = Back_Part or Part = Front_Part then
                GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                                      First => 0,
                                                      Count => Num_Steps);
            else
                GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Line_Loop,
                                                      First => 0,
                                                      Count => Num_Steps);
            end if;
        end Draw_Part;
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Draw_Part (Back_Part);
        Draw_Part (Front_Part);
        Draw_Part (Outline_Part);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Circle.");
            raise;
    end Draw_Circle;

    --  ------------------------------------------------------------------------

    procedure Draw_Cone (Render_Program    : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Scale             : Float) is

        use GL.Objects.Buffers;
        use GA_Maths.Float_Functions;

        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        S_Scale              : constant Single := Single (5.0 / Scale);
        Z                    : float := 0.0;
        Num_Steps            : constant int := 256;
        Rotor_Step           : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        Fan                  : Singles.Vector3_Array (1 .. Num_Steps);
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Fan (1) := (0.0, 0.0, 0.0);
        for Count in 2 .. Num_Steps loop
            Fan (Count) := (S_Scale * Single (Cos (Z)), S_Scale * Single (Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

        Init_Projection_Matrix (Projection_Matrix);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

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
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         aPoint : C3GA.Vector_E3GA; Direction : Multivectors.Vector;
                         Colour : GL.Types.Colors.Color) is
    --                           Weight : Float; Colour : GL.Types.Colors.Color) is

        use GL.Objects.Buffers;
        use GL.Types.Singles;
        use Maths;
        use C3GA;
        use Multivectors;
        Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        Scale                : constant Single := Single (GA_Draw.Get_Line_Length);
        Step                 : constant Single := 0.5;
        Num_Points           : constant Int := Int (2.0 * Scale / Step);
        aRotor               : Rotor;
        MV_Matrix            : Matrix4 := Model_View_Matrix;
        Point_Pos            : constant E3GA.Vector := Vector_To_E3GA (aPoint);
        Translate            : constant Vector3 :=
                                 (Single (Point_Pos (1)), Single (Point_Pos (2)),
                                  Single (Point_Pos (3)));
        Vertices             : Singles.Vector3_Array (1 .. Num_Points);
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        Pos                  : Single := - Scale;
    begin
        --          GA_Utilities.Print_Multivector ("C3GA_Draw.Draw_Line aPoint", aPoint);
        Utilities.Print_Matrix ("C3GA_Draw.Draw_Line Initial MV_Matrix", MV_Matrix);
        Utilities.Print_Vector ("C3GA_Draw.Draw_Line Translate", Translate);
        GL.Objects.Programs.Use_Program (Render_Program);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);

        for Index in 1 .. Num_Points loop
            --           Put_Line ("C3GA_Draw.Draw_Line Pos  " & Single'Image (Pos));
            Vertices (Index) := (0.0, 0.0, Pos);
            Pos := Pos + Step * Scale;
        end loop;
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

        MV_Matrix := Translation_Matrix (Translate) * MV_Matrix;
        --          Utilities.Print_Matrix ("C3GA_Draw.Draw_Line Translated MV_Matrix", MV_Matrix);
        --  rotate e3 to line direction
        aRotor := E3GA_Utilities.Rotor_Vector_To_Vector
          (Basis_Vector (Blade_Types.E3_e3), To_Vector (Unit_e (Direction)));
        if GL_Util.Rotor_GL_Multiply (aRotor, MV_Matrix) then
            Utilities.Print_Matrix ("C3GA_Draw.Draw_Line MV_Matrix", MV_Matrix);
            GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
            GL.Uniforms.Set_Single (MV_Matrix_ID, MV_Matrix);
            Init_Projection_Matrix (Projection_Matrix);
            GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);
            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Line_Strip,
                                                  First => 0,
                                                  Count => Num_Points);
            GL.Attributes.Disable_Vertex_Attrib_Array (0);
        else
            Put_Line ("C3GA_Draw.Draw_Line MV_Matrix, aRotor is not invertable");
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Line.");
            raise;
    end Draw_Line;

    --  ------------------------------------------------------------------------

    procedure Draw_Line (Render_Program    : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         aPoint, Direction : C3GA.Vector_E3GA;
                         Weight : GL.Types.Single; Colour : GL.Types.Colors.Color) is
        use GL.Objects.Buffers;
        --          use GL.Toggles;
        --          use GL.Types.Colors;
        use GL.Types.Singles;

        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : GL.Types.Singles.Matrix4;
        GL_Dir               : constant Vector3 := GL_Util.To_GL (Direction);
        Dir_e1               : constant Single := GL_Dir (GL.X);
        Dir_e2               : constant Single := GL_Dir (GL.Y);
        Dir_e3               : constant Single := GL_Dir (GL.Z);
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        Vertices             : Singles.Vector3_Array (1 .. 2);
    begin
        Vertices := ((0.0, 0.0, 0.0),
                     (0.98 * Dir_e1, 0.98 * Dir_e2, 0.98 * Dir_e3));

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

        Init_Projection_Matrix (Projection_Matrix);
        GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                              First => 0,
                                              Count => 1 * 3);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
            raise;
    end Draw_Line;

    --  ------------------------------------------------------------------------

    procedure Draw_Parallelepiped (Render_Program    : GL.Objects.Programs.Program;
                                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                                   V_Coords          : C3GA.Vector_E3GA;
                                   Scale             : Float;
                                   Method            : Trivector_Method_Type;
                                   Colour            : GL.Types.Colors.Color :=
                                     (0.0, 0.0, 0.0, 0.0)) is

        use GL.Objects.Buffers;
        --          use GL.Toggles;
        --          use GL.Types.Colors;
        use Singles;
        use E3GA;

        E3_V_Coords          : constant E3GA.Vector :=
                                 C3GA.Vector_To_E3GA (V_Coords);
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Projection_Matrix    : Matrix4;
        Scale_Matrix         : Matrix4;
        Scale_Sign           : GL.Types.Single;
        Vertex_Buffer        : GL.Objects.Buffers.Buffer;
        Normals_Buffer       : GL.Objects.Buffers.Buffer;
        Polygon              : Ints.Vector4_Array (1 .. 6);
        Vertices             : Vector3_Array (1 .. 8) :=
                                 (others => (others => 0.0));
        GL_Vertices          : Vector3_Array (1 .. 4) :=
                                 (others => (others => 0.0));
        Vertex               : Vector3;
        Vertex_Vectors       : Ints.Vector3_Array (1 .. 8);
        Vertex_Index         : Int := 0;
        GL_Normals           : Vector3_Array (1 .. 6) :=
                                 (others => (others => 0.0));
        V1                   : E3GA.Vector;
        V2                   : E3GA.Vector;
        V3                   : E3GA.Vector;
        Stride               : constant Int := 0;
    begin
        if Scale >= 0.0 then
          Scale_Sign := 1.0;
        else
           Scale_Sign := -1.0;
        end if;
        if G_Draw_State.M_Draw_Mode = OD_Orientation then
            Scale_Matrix := Maths.Scaling_Matrix (Scale_Sign);
        end if;

        Vertex_Vectors := ((-1, -1, -1),  --  -
                           (0, -1, -1),   --  0
                           (0, 1, -1),    --  0 + 1
                           (1, -1, -1),   --  1
                           (2, -1, -1),   --  2
                           (0, 2, -1),    --  0 + 2
                           (0, 1, 2),     --  0 + 1 + 2
                           (1, 2, -1));   --  1 + 2
        Polygon := ((0, 1, 5, 4),
                    (0, 4, 7, 3),
                    (4, 5, 6, 7),
                    (1, 2, 6, 5),
                    (6, 2, 3, 7),
                    (0, 3, 2, 1));

        for Row in Int range 1 .. 8 loop
            Vertex := (0.0, 0.0, 0.0);
            for Col in GL.Index_Homogeneous range GL.X .. GL.Z loop
                Vertex_Index := Vertex_Vectors (Row) (Col);
                if Vertex_Index >= 0 then
                    Vertex (GL.X) := Vertex (GL.X) + Single (E3_V_Coords (1));
                    Vertex (GL.Y) := Vertex (GL.Y) + Single (E3_V_Coords (2));
                    Vertex (GL.Z) := Vertex (GL.Z) + Single (E3_V_Coords (3));
                    Vertices (Row) := Vertex;
                end if;
            end loop;
        end loop;

        for Index in GL_Normals'Range loop
            V1 := GL_Util.From_GL (Vertices (Polygon (Index) (GL.X)));
            V2 := GL_Util.From_GL (Vertices (Polygon (Index) (GL.Y)));
            V3 := GL_Util.From_GL (Vertices (Polygon (Index) (GL.W)));
            GL_Normals (Index) :=
              (Scale_Sign * GL_Util.To_GL (Outer_Product ((V2 - V1), (V3 - V1))));

            if Scale >= 0.0 then
                for GL_Index in Int range 1 .. 3 loop
                    GL_Vertices (GL_Index) :=
                      Vertices (Polygon (Index) (GL.Index_Homogeneous'Enum_Val (GL_Index)));
                end loop;
            else
                for GL_Index in reverse Int range  3 .. 1 loop
                    GL_Vertices (GL_Index) :=
                      Vertices (Polygon (Index) (GL.Index_Homogeneous'Enum_Val (GL_Index)));
                end loop;
            end if;
        end loop;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                  Projection_Matrix_ID, Colour_Location);
        Utilities.Load_Vertex_Buffer (Array_Buffer, GL_Vertices, Static_Draw);

        Normals_Buffer.Initialize_Id;
        Array_Buffer.Bind (Normals_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, GL_Normals, Static_Draw);

        if Method = Draw_TV_Parellelepiped then
            Draw_Vector (Render_Program => Render_Program,
                         MV_Matrix      => Model_View_Matrix,
                         Tail           => GL_Util.From_GL (Vertices (1)),
                         Direction      => V_Coords,
                         Colour         => Colour,
                         Scale          => Scale);
        end if;

        Init_Projection_Matrix (Projection_Matrix);
        GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
        GL.Uniforms.Set_Single (MV_Matrix_ID, Scale_Matrix * Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

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
                           MV_Matrix : GL.Types.Singles.Matrix4;
                           Normal : GL.Types.Single;
                           Colour : GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0)) is
        use Geosphere;
        --        Sphere : Geosphere.Geosphere;
    begin
        if Sphere_State_Null (G_Draw_State.M_Sphere) then
            Geosphere.GS_Compute (G_Draw_State.M_Sphere, 4);
            Geosphere.New_Sphere_List (G_Draw_State.M_Sphere);
            --  gsDraw(m_sphere, 0.0f);
            Geosphere.GS_Draw (Render_Program, MV_Matrix, G_Draw_State.M_Sphere,
                               0.0, Colour);
        end if;

        if Normal = 0.0 then
            Draw_Sphere_List (Render_Program, MV_Matrix, Colour);
        else
            Geosphere.GS_Draw (Render_Program, MV_Matrix, G_Draw_State.M_Sphere,
                               Normal, Colour);
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
    --      procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
    --                                Model_View_Matrix : GL.Types.Singles.Matrix4;
    --                                Position : Multivectors.Multivector; Colour : GL.Types.Colors.Color;
    --                                Scale : float;
    --                                Method : Trivector_Method_Type := Draw_TV_Sphere) is
    --          use GL.Types.Singles;
    --          Scale_S             : Single := Single (Abs (Scale));
    --          Z_Max               : constant Single := 4.0 * Single (GA_Maths.Pi);
    --          Normal              : Single;  -- s
    --          Translation_Matrix  : Matrix4 := Identity4;
    --          Scaling_Matrix      : Matrix4 := Identity4;
    --          MV_Matrix           : Matrix4;
    --      begin
    --          --  scaleSign = (scale < 0.0f) ? -1.0f : 1.0f;
    --          --  adjust scale for sphere
    --          Scale_S := Abs (Maths.Cube_Root
    --                          (Scale_S / ((4.0 / 3.0) * Single (GA_Maths.Pi))));
    --          --  main part of draw.cpp drawTriVector
    --          --  s = (scale < 0.0f) ? -1.0f : 1.0f, f;
    --          if Multivectors.Norm_E2 (Position) >= 0.0 then
    --              Translation_Matrix :=
    --                Maths.Translation_Matrix ((Single (C3GA.e1 (Position)),
    --                                          Single (C3GA.e2 (Position)),
    --                                          Single (C3GA.e3 (Position))));
    --          end if;
    --          Utilities.Print_Matrix ("GA_Draw.Draw_Trivector Translation_Matrix", Translation_Matrix);
    --
    --          Scaling_Matrix := Maths.Scaling_Matrix (Scale_S);
    --          MV_Matrix := Translation_Matrix * Scaling_Matrix * Model_View_Matrix;
    --
    --          case Method is
    --              when DRAW_TV_SPHERE =>
    --                  --  g_drawState.drawSphere
    --                  --  (((g_drawState.getDrawMode() & OD_ORIENTATION) ?
    --                  --     s * 0.1f : 0.0f));
    --                  if Get_Draw_Mode = OD_Orientation then
    --                      Normal := 0.1;
    --                  else
    --                      Normal := 0.0;
    --                  end if;
    --                  --  g_drawState.drawSphere (s)
    --                  Draw_Sphere (Render_Program, MV_Matrix, Normal, Colour);
    --              when others => null;
    --          end case;
    --
    --      exception
    --          when  others =>
    --              Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
    --              raise;
    --      end Draw_Trivector;

    --  ------------------------------------------------------------------------

    procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                              Model_View_Matrix : GL.Types.Singles.Matrix4;
                              Base   : C3GA.Vector_E3GA; Scale : float := 1.0;
                              V      : C3GA.Vector_E3GA;
                              Colour : GL.Types.Colors.Color;
                              Method : Trivector_Method_Type := Draw_TV_Sphere) is
        use GL.Types.Singles;
        use Ada.Numerics.Elementary_Functions;  --  needed for fractional powers
        use GA_Maths;
        VC                : constant Array_3D := C3GA.Get_Coords (V);
        Scale_Sign        : Float;
        P_Scale           : Float;
        Normal            : Single;
        --          Z_Max             : constant Float := 4.0 * GA_Maths.PI;
        Base_Coords       : constant GA_Maths.Array_3D := C3GA.Get_Coords (Base);
        MV_Matrix         : Matrix4 := Model_View_Matrix;
    begin
        if Scale >= 0.0 then
            Scale_Sign := 1.0;
        else
            Scale_Sign := -1.0;
        end if;

        if Method = Draw_TV_Parellelepiped or
          Method = Draw_TV_Parellelepiped_No_Vectors then
            if VC = (0.0, 0.0, 0.0) then
                Draw_Trivector (Render_Program, Model_View_Matrix,
                                Base, Scale, V, Colour, Draw_TV_Sphere) ;
            end if;
            P_Scale :=  Scale_Sign * ((Scale_Sign * Scale) ** 1.0 / 3.0);
        else
            P_Scale :=  Scale_Sign * ((Scale_Sign * Scale / (4.0 / 3.0 * GA_Maths.PI)) ** 1.0 / 3.0);
        end if;

        if C3GA.Norm_e2 (Base) /= 0.0 then
            MV_Matrix := Maths.Translation_Matrix
              ((Single (Base_Coords (1)), Single (Base_Coords (2)), Single (Base_Coords (3)))) *
                Maths.Scaling_Matrix (Single (P_Scale)) * MV_Matrix;
        end if;

        case Method is
        when Draw_TV_Sphere =>
            if Get_Draw_Mode = OD_Orientation then
                Normal := 0.1;
            else
                Normal := 0.0;
            end if;
            --  g_drawState.drawSphere (s)
            Draw_Sphere (Render_Program, Model_View_Matrix, Normal);
        when Draw_TV_Cross =>
            null;
        when Draw_TV_Curly_Tail =>
            null;
        when Draw_TV_Parellelepiped =>
            Draw_Parallelepiped (Render_Program, Model_View_Matrix, V, Scale,
                                 Draw_TV_Parellelepiped, Colour);

        when Draw_TV_Parellelepiped_No_Vectors =>
            Draw_Parallelepiped (Render_Program, Model_View_Matrix, V, Scale,
                                 Draw_TV_Parellelepiped_No_Vectors, Colour);
        end case;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
            raise;
    end Draw_Trivector;

    --  ------------------------------------------------------------------------

    procedure Draw_Vector (Render_Program  : GL.Objects.Programs.Program;
                           MV_Matrix       : GL.Types.Singles.Matrix4;
                           Tail, Direction : C3GA.Vector_E3GA;
                           Colour          : GL.Types.Colors.Color;
                           Scale           : float := 1.0) is
        use GL.Culling;
        use GL.Toggles;
        use GL.Types.Singles;
        use Multivectors;

        Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        MV_Matrix_ID         : GL.Uniforms.Uniform;
        Projection_Matrix_ID : GL.Uniforms.Uniform;
        Colour_Location      : GL.Uniforms.Uniform;
        Model_View_Matrix    : Matrix4;
        Projection_Matrix    : Matrix4;
        GL_Tail              : Vector3;
        GL_Dir               : Vector3;
        aRotor               : Rotor;
        Saved_Cull_Face      : constant Face_Selector := Cull_Face;
        Saved_Front_Face     : constant GL.Types.Orientation := GL.Culling.Front_Face;
    begin
        GL_Tail := GL_Util.To_GL (Tail);
        GL_Dir := GL_Util.To_GL (Direction);
        if Scale /= 0.0 then
            GL.Objects.Programs.Use_Program (Render_Program);
            Vertex_Array_Object.Initialize_Id;
            Vertex_Array_Object.Bind;

            Init_Projection_Matrix (Projection_Matrix);
            Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                      Projection_Matrix_ID, Colour_Location);
            GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
            Model_View_Matrix := MV_Matrix;

            if Maths.Norm (GL_Tail) /= 0.0 then
                Model_View_Matrix := Maths.Translation_Matrix (GL_Tail) * Model_View_Matrix;
            end if;

            Draw_Line (Render_Program, Model_View_Matrix, Tail, Direction, 1.0, Colour);

            --  Setup translation matrix for arrow head
            --  rotate e3 to vector direction
            aRotor := E3GA_Utilities.Rotor_Vector_To_Vector
              (Basis_Vector (Blade_Types.E3_e3), E3GA.Unit_E (Direction));
            Model_View_Matrix := Identity4;
            if GL_Util.Rotor_GL_Multiply (aRotor, Model_View_Matrix) then
                Model_View_Matrix := MV_Matrix * Model_View_Matrix;

                if Norm_e2 (Tail) /= 0.0 then
                    Model_View_Matrix := Maths.Translation_Matrix (GL_Tail) * Model_View_Matrix;
                end if;
                --  Translate to head of vector
                Model_View_Matrix := Maths.Translation_Matrix (Single (Scale) * GL_Dir) * Model_View_Matrix;

                Enable (Cull_Face);
                Set_Front_Face (GL.Types.Clockwise);
                Set_Cull_Face (Front);

                Draw_Cone (Render_Program, Model_View_Matrix, Scale);
                Draw_Base (Render_Program, Model_View_Matrix, Scale);

                Set_Front_Face (Saved_Front_Face);
                Set_Cull_Face (Saved_Cull_Face);
            else
                Put_Line ("GA_Draw.Draw_Vector, aRotor is not invertible.");
            end if;
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Vector.");
            raise;
    end Draw_Vector;

    --  -----------------------------------------------------------------------

    function Get_Draw_Mode return Draw_Mode is
    begin
        return G_Draw_State.M_Draw_Mode;
    end Get_Draw_Mode;

    --  ------------------------------------------------------------------------

    function Get_Line_Length return Float is
    begin
        return G_Draw_State.Line_Length;
    end Get_Line_Length;

    --  ------------------------------------------------------------------------

    procedure Graphic_Shader_Locations (Render_Program : GL.Objects.Programs.Program;
                                        MV_Matrix_ID, Projection_Matrix_ID,
                                        Colour_Location : out GL.Uniforms.Uniform) is
    begin
        MV_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MV_Matrix");
        Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "Proj_Matrix");
        Colour_Location := GL.Objects.Programs.Uniform_Location
          (Render_Program, "vector_colour");

    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Graphic_Shader_Locations.");
            raise;
    end Graphic_Shader_Locations;

    --  -------------------------------------------------------------------------

    function Point_Size return Float is
    begin
        return G_Draw_State.Point_Size;
    end Point_Size;

    --  ------------------------------------------------------------------------

    --      procedure Set_Background_Colour (Back_Colour : Color) is
    --      begin
    --          Palet.Background_Colour := Back_Colour;
    --      end Set_Background_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Draw_Mode (Mode : Draw_Mode) is
    begin
        G_Draw_State.M_Draw_Mode := Mode;
    end Set_Draw_Mode;

    --  ------------------------------------------------------------------------

    procedure Set_Foreground_Colour (Fore_Colour : Color) is
    begin
        Palet.Foreground_Colour := Fore_Colour;
    end Set_Foreground_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Ol_Colour (Ol_Colour : Color) is
    begin
        Palet.Ol_Colour := Ol_Colour;
    end Set_Ol_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Point_Size (Point_Size : Float) is
    begin
        G_Draw_State.Point_Size := Point_Size;
    end Set_Point_Size;

    --  ------------------------------------------------------------------------

    procedure Init_Projection_Matrix (Proj_Matrix : out GL.Types.Singles.Matrix4;
                                      Near : GL.Types.Single := -100.0;
                                      Far  : GL.Types.Single := 100.0) is
        VP_X      : Int;
        VP_Y      : Int;
        VP_Height : Int;
        VP_Width  : Int;
    begin
        GL.Window.Get_Viewport (VP_X, VP_Y, VP_Width, VP_Height);
        --  Init_Orthographic_Transform
        --  (Top, Bottom, Left, Right, Z_Near, Z_Far, Proj_Matrixles.Matrix4)
        Maths.Init_Orthographic_Transform (Single (VP_Y), Single (VP_Y + VP_Height),
                                           Single (VP_X), Single (VP_X + VP_Width),
                                           Near, Far, Proj_Matrix);
    exception
        when  others =>
            Put_Line ("An exception occurred in GA_Draw.Set_Projection_Matrix.");
            raise;
    end Init_Projection_Matrix;

    --  ------------------------------------------------------------------------

end GA_Draw;
