
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

package body Shader_Manager is

    Black           : constant GL.Types.Singles.Vector4 := (0.0, 0.0, 0.0, 0.0);
    Render_Uniforms : Shader_Uniforms;

    procedure Init (Render_Program : in out GL.Objects.Programs.Program) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
        Light     : constant Singles.Vector3 := (0.0, 4.0, 1.0);
        Direction : constant Singles.Vector3 := (0.0, 0.0, 1.0);
    begin
        Render_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

        Render_Uniforms.View_Matrix_ID :=
          Uniform_Location (Render_Program, "view_matrix");
        Render_Uniforms.Model_Matrix_ID :=
          Uniform_Location (Render_Program, "model_matrix");
        Render_Uniforms.Projection_Matrix_ID :=
          Uniform_Location (Render_Program, "projection_matrix");
        Render_Uniforms.Rotation_Matrix_ID :=
          Uniform_Location (Render_Program, "rotation_matrix");
        Render_Uniforms.Translation_Matrix_ID :=
          Uniform_Location (Render_Program, "translation_matrix");
        Render_Uniforms.Light_Direction_ID :=
          Uniform_Location (Render_Program, "light_direction");
        Render_Uniforms.Light_Position_ID :=
          Uniform_Location (Render_Program, "light_position");
        Render_Uniforms.Line_Width_ID :=
          Uniform_Location (Render_Program, "line_width");

        Render_Uniforms.Ambient_Colour_ID :=
          Uniform_Location (Render_Program, "Ambient_Colour");
        Render_Uniforms.Diffuse_Colour_ID :=
          Uniform_Location (Render_Program, "Diffuse_Colour");
        Render_Uniforms.Drawing_Colour_ID :=
          Uniform_Location (Render_Program, "Drawing_Colour");

        Use_Program (Render_Program);
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Direction_ID, Direction);
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_ID, Light);
        GL.Uniforms.Set_Single (Render_Uniforms.Line_Width_ID, 1.0);
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Projection_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Rotation_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Translation_Matrix_ID, Identity4);

        GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Colour_ID, Black);
        GL.Uniforms.Set_Single (Render_Uniforms.Diffuse_Colour_ID, Black);
        GL.Uniforms.Set_Single (Render_Uniforms.Drawing_Colour_ID, Black);

    exception
        when others =>
            Put_Line ("An exception occurred in Shader_Manager.Init.");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_Ambient_Colour (Ambient_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Colour_ID, Ambient_Colour);
    end Set_Ambient_Colour;

    --  -------------------------------------------------------------------------

    procedure Set_Diffuse_Colour (Diffuse_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Diffuse_Colour_ID, Diffuse_Colour);
    end Set_Diffuse_Colour;

    --  -------------------------------------------------------------------------

    procedure Set_Drawing_Colour (Drawing_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Drawing_Colour_ID, Drawing_Colour);
    end Set_Drawing_Colour;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Direction_Vector (Light_Direction : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Direction_ID, Light_Direction);
    end Set_Light_Direction_Vector;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Position_Vector (Light_Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Position_ID, Light_Position);
    end Set_Light_Position_Vector;

    --  -------------------------------------------------------------------------

    procedure Set_Line_Width (Width : Single) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Line_Width_ID, Width);
    end Set_Line_Width;

    --  -------------------------------------------------------------------------

    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
    end Set_Model_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_View_Matrix (View_Matrix  : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, View_Matrix);
    end Set_View_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Projection_Matrix_ID, Projection_Matrix);
    end Set_Projection_Matrix;

   --  -------------------------------------------------------------------------

    procedure Set_Rotation_Matrix (Rotation_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Rotation_Matrix_ID, Rotation_Matrix);
    end Set_Rotation_Matrix;

   --  -------------------------------------------------------------------------

    procedure Set_Translation_Matrix (Translation_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Translation_Matrix_ID, Translation_Matrix);
    end Set_Translation_Matrix;

    --  -------------------------------------------------------------------------

end Shader_Manager;
