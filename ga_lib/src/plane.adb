
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with Utilities;

with Maths;

--  with GA_Utilities;
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

    function Build_Quad_Vertices (Bottom_Left : Singles.Vector3; Step_Size : Single)
                                  return Singles.Vector3_Array is
        X : constant Single := Bottom_Left (GL.X);
        Y : constant Single := Bottom_Left (GL.X);
        Z : constant Single := Bottom_Left (GL.X);
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

    procedure Display_Plane (Mode  : Connection_Mode; Num_Vertices : Int;
                             Index : GL.Attributes.Attribute ) is
    begin
        GL.Attributes.Enable_Vertex_Attrib_Array (Index);
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index, 3, Single_Type, 0, 0);

        Put_Line ("Plane.Display_Plane drawing arrays.");
        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Mode,
                                              First => 0,
                                              Count => Num_Vertices);
        Put_Line ("Plane.Display_Plane arrays drawn.");
        GL.Attributes.Disable_Vertex_Attrib_Array (Index);

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
        Scale_Matrix     : Matrix4;
        Model_Matrix     : constant Matrix4 := Identity4;
        Plane_Size       : constant Single := Single (Palet.Get_Plane_Size);  --  6.0
        Scale_Magnitude  : constant Single := Single (Weight);
        Step_Size        : constant Single := 0.1;
        Scaled_Step_Size : constant Single := Step_Size * Plane_Size;
        Num_Vertices     : constant Int :=
                             Int (2.0 * Plane_Size / Step_Size);
        GL_Normal        : constant Vector3 := Vector3 (Normal);
        GL_Point         : constant Vector3 := Vector3 (Point);
        V_Index          : Int := 0;
        Vertices         : Vector3_Array (1 .. Num_Vertices) :=
                             (others => (others => 0.0));
        GL_Normals       : Vector3_Array (1 .. 6) := (others => Normal);
        X                : Single;
        Y                : Single;
        YY_Dir           : Vector3;
        QY               : Vector3;
        Quad_Vertices    : Singles.Vector3_Array (1 .. 6);

    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        GL.Objects.Programs.Use_Program (Render_Program);
        Shader_Manager.Set_Ambient_Colour ((1.0, 1.0, 1.0, 1.0));
        Shader_Manager.Set_Model_Matrix (Model_Matrix);

        --  draw both front and back side individually
        for Surface in Surface_Type'Range loop
            case Surface is
            when Front_Surface =>
                null;
            when Back_Surface =>
                for n in Int range 1 .. 6 loop
                    GL_Normals (n) := -GL_Normals (n);
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
--                      Put_Line ("Plane.Draw_Plane, V_Index " & Int'Image (V_Index));

                    case Surface is
                    when Front_Surface =>
                        QY := YY_Dir;
                    when Back_Surface =>
                        QY := Scaled_Step_Size * YY_Dir;
                    end case;

                    Quad_Vertices := Build_Quad_Vertices
                      ((GL_Point + X * X_Dir + QY), Scaled_Step_Size);
                    Add_To_Array (Vertices, V_Index, Quad_Vertices);
                    X := X + Scaled_Step_Size;
                    V_Index := V_Index + 6;
                end loop;
--                  Put_Line ("Plane.Draw_Plane, V_Index " & Int'Image (V_Index));

                Vertex_Buffer.Initialize_Id;
                Array_Buffer.Bind (Vertex_Buffer);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
                Utilities.Print_GL_Array3 ("Plane.Draw_Plane, Vertices", Vertices);
                Display_Plane (Triangle_Strip, 6, 0);
                Y := Y + Scaled_Step_Size;
            end loop;
        end loop;
--          Put_Line ("Plane.Draw_Plane, Num_Vertices " & Int'Image (Num_Vertices));

        if Palet.Get_Draw_Mode.Magnitude then  --  draw normals
            Scale_Matrix := Maths.Scaling_Matrix
              ((Scale_Magnitude, Scale_Magnitude, Scale_Magnitude));
            Shader_Manager.Set_Model_Matrix (Scale_Matrix * Model_Matrix);

            V_Index := 0;
            X := -Plane_Size;
            Y := -Plane_Size;
            while Y < Plane_Size loop
                YY_Dir := Y * Y_Dir;
                while X < Plane_Size loop
                    V_Index := V_Index + 1;
                    Vertices (V_Index) := GL_Point + X * GL_Normal + YY_Dir;
                    V_Index := V_Index + 1;
                    Vertices (V_Index) := GL_Point + X * GL_Normal + YY_Dir -
                      Scale_Magnitude * Normal;
                    X := X + Scaled_Step_Size;
                end loop;
                Y := Y + Scaled_Step_Size;
            end loop;

            Normals_Buffer.Initialize_Id;
            Array_Buffer.Bind (Normals_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, GL_Normals, Static_Draw);
            Display_Plane (Lines, 3, 0);
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Plane.Draw_Plane.");
            raise;
    end Draw_Plane;

    --  ------------------------------------------------------------------------

end Plane;
