
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

package body Shader is

    procedure Init (Render_Program  : in out GL.Objects.Programs.Program;
                    Render_Uniforms : out Shader_Uniforms) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use Program_Loader;
        Light   : constant Singles.Vector3 := (0.0, 4.0, 1.0);
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
        Render_Uniforms.Model_View_Matrix_ID :=
          Uniform_Location (Render_Program, "mv_matrix");
        Render_Uniforms.Light_Position_ID :=
          Uniform_Location (Render_Program, "light_position");

        Set_Light_Position_Vector (Render_Uniforms, Light);
    exception
        when others =>
            Put_Line ("An exception occurred in Shader.Init.");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_View_Matrix (Render_Uniforms : Shader_Uniforms;
                               View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, View_Matrix);
    end Set_View_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Position_Vector (Render_Uniforms : Shader_Uniforms;
                                         Light_Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Position_ID, Light_Position (GL.X),
          Light_Position (GL.Y), Light_Position (GL.Z));
    end Set_Light_Position_Vector;

    --  -------------------------------------------------------------------------

    procedure Set_Model_Matrix (Render_Uniforms : Shader_Uniforms;
                                Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
    end Set_Model_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Model_View_Matrix (Render_Uniforms : Shader_Uniforms;
                                     Model_View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Model_View_Matrix_ID, Model_View_Matrix);
    end Set_Model_View_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Projection_Matrix (Render_Uniforms : Shader_Uniforms;
                                     Projection_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Projection_Matrix_ID, Projection_Matrix);
    end Set_Projection_Matrix;

    --  -------------------------------------------------------------------------

end Shader;
