
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Framebuffer;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Pixels;
with GL.Types.Colors;
with GL.Uniforms;

with Program_Loader;
with Utilities;

package body Pick_Manager is

    type Pixels_Array is array (Positive range <>) of aliased GL.Types.UByte;

    Max_Items          : constant GL.Types.Int := 100;
    White              : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
    Picking_Program    : GL.Objects.Programs.Program;
    Picking_Colour_ID  : GL.Uniforms.Uniform;
    Picking_Matrix_ID  : GL.Uniforms.Uniform;
    Vertex_Buffer      : GL.Objects.Buffers.Buffer;
    Element_Buffer     : GL.Objects.Buffers.Buffer;
    Pick_Data          : GL_Pick;

    --  ------------------------------------------------------------------------

    procedure Init_Pick_Manager is
      use GL.Objects.Shaders;
    begin
        Picking_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/picking_vertex_shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/picking_fragment_shader.glsl",
             Fragment_Shader)));

        Picking_Colour_ID := GL.Objects.Programs.Uniform_Location
          (Picking_Program, "Picking_Colour");
        Picking_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Picking_Program, "MVP");

    exception
        when others =>
            Put_Line ("An exception occurred in Init_Pick_Manager.");
            raise;
    end Init_Pick_Manager;

    --  ------------------------------------------------------------------------

    procedure Read_Pix is new
          GL.Framebuffer.Read_Pixels (Element_Type => GL.Types.UByte,
                                      Index_Type   => Positive,
                                      Array_Type   => Pixels_Array);
    --  ------------------------------------------------------------------------

    procedure Pick (Window           : in out Glfw.Windows.Window;
                    Positions        : GL.Types.Singles.Vector3_Array;
                    Orientations     : Orientation_Array;
                    Indices_Size     : GL.Types.Int;
                    View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4) is
        use Interfaces;
        use GL.Types;
        use GL.Types.Singles;
        Model_Matrix    : Matrix4;
        Rot_Matrix      : Matrix4;
        Trans_Matrix    : Matrix4;
        MVP_Matrix      : Singles.Matrix4;
        R               : Single;
        G               : Single;
        B               : Single;
        Window_Width    : Glfw.Size;
        Window_Height   : Glfw.Size;
        Pixel_Data      : Pixels_Array (1 .. 4);
        Picked_ID       : Int;
        --        Message         : Ada.Strings.Unbounded.Unbounded_String;
    begin
        Utilities.Clear_Background_Colour_And_Depth (White);
        GL.Objects.Programs.Use_Program (Picking_Program);
        --  Only the positions are needed (not the UVs and normals)
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        for count in GL.Types.Int range 1 .. Max_Items loop
            Rot_Matrix := Maths.Rotation_Matrix (Orientations (count).Angle,
                                                 Orientations (count).Axis);
            Trans_Matrix := Maths.Translation_Matrix (Positions (count));
            Model_Matrix := Trans_Matrix * Rot_Matrix;
            MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
            GL.Uniforms.Set_Single (Picking_Matrix_ID, MVP_Matrix);

            --  Convert count, the integer mesh ID, into an RGB color
            R :=  Single (Unsigned_32 (count) and 16#FF#) / 255.0;
            G :=  Single (Shift_Right (Unsigned_32 (count) and 16#FF00#, 8)) / 255.0;
            B :=  Single (Shift_Right (Unsigned_32 (count) and 16#FF0000#, 16)) / 255.0;
            GL.Uniforms.Set_Single (Picking_Colour_ID, R, G, B, 1.0);

            GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, 0, 0);

            GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
            GL.Objects.Buffers.Draw_Elements (Mode       => Triangles,
                                              Count      => Indices_Size,
                                              Index_Type => UInt_Type);
        end loop;

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Flush;

        GL.Pixels.Set_Pack_Alignment (GL.Pixels.Unpack_Alignment);
        Window'Access.Get_Size (Window_Width, Window_Height);
        --  Read the pixel at the center of the screen
        Read_Pix (Int (Window_Width) / 2, Int (Window_Height) / 2, 1, 1,
                  GL.Pixels.RGBA, GL.Pixels.Float, Pixel_Data);
        Put_Line ("Pick R" & UByte'Image (Pixel_Data (1)) & UByte'Image (Pixel_Data (2))
                  & UByte'Image (Pixel_Data (3)));
        --   Convert the color back to an integer ID
        Picked_ID := Int (Pixel_Data (1)) + 256 * Int (Pixel_Data (2)) +
          256 * 256 * Int (Pixel_Data (3));
        if Picked_ID = 16#00FFFFFF# then  --  Full white, must be the background!
            Put_Line ("Background " & Int'Image (Picked_ID));
            --           Message := Ada.Strings.Unbounded.To_Unbounded_String ("background");
        else
            Put_Line ("Mesh " & Int'Image (Picked_ID));
            --           Message := Ada.Strings.Unbounded.To_Unbounded_String ("");
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Pick.");
            raise;
    end Pick;

    --  ------------------------------------------------------------------------

    function Pick_Active return Boolean is
    begin
       return Pick_Data.Pick_Active;
    end Pick_Active;

    --  ------------------------------------------------------------------------

end Pick_Manager;
