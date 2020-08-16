
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with Utilities;

with Maths;

with GA_Utilities;
with Palet;
with Shader_Manager;

package body Plane is

    type Surface_Type is (Back_Surface, Front_Surface);

    --  ------------------------------------------------------------------------

    procedure Add_To_Array (theArray : in out Singles.Vector3_Array;
                            Current_Index : Int;
                            Addition : Singles.Vector3_Array) is
    begin
        for index in Int range 1 .. 6 loop
            theArray (Current_Index + index) := Addition (index);
        end loop;

    end Add_To_Array;

    --  ------------------------------------------------------------------------

    function Build_Quad_Vertices (Bottom_Left : Singles.Vector3;
                                  Step_Size : Single)
                                  return Singles.Vector3_Array is
        X : constant Single := Bottom_Left (GL.X);
        Y : constant Single := Bottom_Left (GL.Y);
        Z : constant Single := Bottom_Left (GL.Z);
        Quad_Vertices : constant Singles.Vector3_Array (1 .. 6) :=
                           (Bottom_Left,
                           (X, Y + Step_Size, Z),
                           (X + Step_Size, Y + Step_Size, Z),

                           (X + Step_Size, Y + Step_Size, Z),
                           (X + Step_Size, Y, Z),
                           (Bottom_Left));
    begin
        return Quad_Vertices;
    end Build_Quad_Vertices;

    --  ------------------------------------------------------------------------

    procedure Display_Plane (Mode  : Connection_Mode; Num_Vertices : Int) is
    begin
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Mode,
                                              First => 0,
                                              Count => Num_Vertices);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);

    exception
        when  others =>
            Put_Line ("An exception occurred in Plane.Display_Plane.");
            raise;
    end Display_Plane;

    --  ------------------------------------------------------------------------

    procedure Draw_Plane (Render_Program              : GL.Objects.Programs.Program;
                          Point, X_Dir, Y_Dir, Normal : C3GA.Vector_E3;
                          Weight                      : Float := 1.0) is
    --  Attitude: Normal is perpendicular to plane of Ortho_1 and Ortho_2.
        use GL.Objects.Buffers;
        use Singles;
        Vertex_Array     : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer    : GL.Objects.Buffers.Buffer;
        Normals_Buffer   : GL.Objects.Buffers.Buffer;
        Model_Matrix     : Matrix4 := Identity4;
        Plane_Size       : constant Single := Single (Palet.Get_Plane_Size);  --  6.0
        Scale_Magnitude  : constant Single := Single (Weight);
        Step_Size        : constant Single := 0.1;
        Scaled_Step_Size : constant Single := Step_Size * Plane_Size;
        Num_Vertices     : constant Int :=
                             Int (2.0 * Plane_Size / Step_Size);
        Scale_Matrix     : constant Matrix4 := Maths.Scaling_Matrix
          ((Scale_Magnitude, Scale_Magnitude, Scale_Magnitude));
        V_Index          : Int := 0;
        Vertices         : Vector3_Array (1 .. Num_Vertices) :=
                             (others => (others => 0.0));
        Normals          : Vector3_Array (1 .. 6) := (others => Normal);
        X                : Single;
        Y                : Single;
        YY_Dir           : Vector3;
        QY               : Vector3;
        Quad_Vertices    : Singles.Vector3_Array (1 .. 6);
        Quad_Normals    : Singles.Vector3_Array (1 .. 6);

    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        GL.Objects.Programs.Use_Program (Render_Program);
        if Palet.Get_Draw_Mode.Magnitude then
            Model_Matrix := Scale_Matrix * Model_Matrix;
        end if;
        Shader_Manager.Set_Model_Matrix (Model_Matrix);

        GA_Utilities.Print_E3_Vector ("Plane Point", Point);
        GA_Utilities.Print_E3_Vector ("Plane X_Dir", X_Dir);
        GA_Utilities.Print_E3_Vector ("Plane Y_Dir", Y_Dir);

        --  draw both front and back side individually
        for Surface in Surface_Type'Range loop
            case Surface is
            when Front_Surface =>
                null;
            when Back_Surface =>
                for n in Int range 1 .. 6 loop
                    Normals (n) := -Normals (n);
                end loop;
            end case;

            Y := -Plane_Size;
            while Y < Plane_Size - Scaled_Step_Size loop
                V_Index := 0;
                YY_Dir := Y * Y_Dir;
                X := -Plane_Size;
                --  for each Y value draw a triangle strip (rectangle) of
                --  "length" 2 Plane_Size and "height" YY_Dir
                --  The "length" in Scaled_Step_Size ^ 2 quad increments
                --  with each quad drawn as 2 triangles
                --  X_Dir and Y_Dir are orthogonal
                while X < Plane_Size - Scaled_Step_Size loop
                    case Surface is
                    when Front_Surface =>
                        QY := YY_Dir;
                    when Back_Surface =>
                        QY := Scaled_Step_Size * YY_Dir;
                    end case;

                    Quad_Vertices := Build_Quad_Vertices
                      (0.1 * (Point + X * X_Dir + QY), Scaled_Step_Size);
                    Add_To_Array (Vertices, V_Index, Quad_Vertices);
                    if Palet.Get_Draw_Mode.Magnitude then
                        Quad_Normals := Build_Quad_Vertices
                          (Point + X * Normal + YY_Dir, Scaled_Step_Size);
                        Add_To_Array (Normals, V_Index, Quad_Normals);
                    end if;
                    X := X + Scaled_Step_Size;
                    V_Index := V_Index + 6;
                end loop;

                Utilities.Print_GL_Array3 ("Plane.Draw_Plane, Vertices", Vertices);
                Vertex_Buffer.Initialize_Id;
                Array_Buffer.Bind (Vertex_Buffer);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

                if Palet.Get_Draw_Mode.Magnitude then  --  draw normals
                    Normals_Buffer.Initialize_Id;
                    Array_Buffer.Bind (Normals_Buffer);
                    Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
                end if;

                Display_Plane (Triangle_Strip, Num_Vertices);
                Y := Y + Scaled_Step_Size;
            end loop;
        end loop;

    exception
        when  others =>
            Put_Line ("An exception occurred in Plane.Draw_Plane.");
            raise;
    end Draw_Plane;

    --  ------------------------------------------------------------------------

end Plane;
