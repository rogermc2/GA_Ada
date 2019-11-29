
with GL.Objects.Shaders;
with Program_Loader;

package body Shader is

    procedure Init (Render_Program  : in out GL.Objects.Programs.Program;
                    Render_Uniforms : out Uniform_IDs) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Render_Program := Program_From
          ((Src ("src/shaders/shadowmap_scene.vs", Vertex_Shader),
           Src ("src/shaders/shadowmap_scene.fs", Fragment_Shader)));

        Render_Uniforms.View_Matrix_ID :=
          Uniform_Location (Render_Program, "view_matrix");
        Render_Uniforms.Model_Matrix_ID :=
          Uniform_Location (Render_Program, "model_matrix");
        Render_Uniforms.Projection_Matrix_ID :=
          Uniform_Location (Render_Program, "projection_matrix");
        Render_Uniforms.Model_View_Matrix_ID :=
          Uniform_Location (Render_Program, "mv_matrix");
        Render_Uniforms.Light_Position_Matrix_ID :=
          Uniform_Location (Render_Program, "light_position");

        GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_Matrix_ID,
                                4.0, 4.0, 4.0);
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4;
                               Render_Uniforms : Uniform_IDs) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, View_Matrix);
    end Set_View_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4;
                                Render_Uniforms : Uniform_IDs) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
    end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4;
                                     Render_Uniforms   : Uniform_IDs) is
    begin
      GL.Uniforms.Set_Single
        (Render_Uniforms.Projection_Matrix_ID, Projection_Matrix);
    end Set_Projection_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Model_View_Matrix (MV_Matrix  : Singles.Matrix4;
                                     Render_Uniforms : Uniform_IDs) is
    begin
      GL.Uniforms.Set_Single
        (Render_Uniforms.Model_View_Matrix_ID, MV_Matrix);
    end Set_Model_View_Matrix;

   --  -------------------------------------------------------------------------

    procedure Set_Light_Position_Matrix (Light_Position_Matrix : Singles.Matrix4;
                                         Render_Uniforms       : Uniform_IDs) is
    begin
      GL.Uniforms.Set_Single
        (Render_Uniforms.Light_Position_Matrix_ID, Light_Position_Matrix);
    end Set_Light_Position_Matrix;

    --  -------------------------------------------------------------------------

end Shader;
