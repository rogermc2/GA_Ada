
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Shader is

    type Light_Uniform_IDs is record
        MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
    end record;

    type Shader_Uniforms is record
        View_Matrix_ID        : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID       : GL.Uniforms.Uniform := 0;
        Projection_Matrix_ID  : GL.Uniforms.Uniform := 0;
        Model_View_Matrix_ID  : GL.Uniforms.Uniform := 0;
        Light_Position_ID     : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Render_Program  : in out GL.Objects.Programs.Program;
                    Render_Uniforms : out Shader_Uniforms);
    procedure Set_Light_Position_Vector (Render_Uniforms : Shader_Uniforms;
                                         Light_Position : Singles.Vector3);
    procedure Set_Model_Matrix (Render_Uniforms : Shader_Uniforms;
                                Model_Matrix : Singles.Matrix4);
    procedure Set_Model_View_Matrix (Render_Uniforms : Shader_Uniforms;
                                     Model_View_Matrix : Singles.Matrix4);
    procedure Set_Projection_Matrix (Render_Uniforms : Shader_Uniforms;
                                     Projection_Matrix : Singles.Matrix4);
    procedure Set_View_Matrix (Render_Uniforms : Shader_Uniforms;
                               View_Matrix : Singles.Matrix4);

end Shader;
