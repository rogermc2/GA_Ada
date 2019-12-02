
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Shader_Manager is

    type Light_Uniform_IDs is record
        MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
    end record;

    type Shader_Uniforms is record
        Ambient_Colour_ID    : GL.Uniforms.Uniform := 0;
        Diffuse_Colour_ID    : GL.Uniforms.Uniform := 0;
        Light_Position_ID    : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID      : GL.Uniforms.Uniform := 0;
        Model_View_Matrix_ID : GL.Uniforms.Uniform := 0;
        Projection_Matrix_ID : GL.Uniforms.Uniform := 0;
        View_Matrix_ID       : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Render_Program : in out GL.Objects.Programs.Program);
    procedure Set_Ambient_Colour (Ambient_Colour : Singles.Vector4);
    procedure Set_Diffuse_Colour (Diffuse_Colour : Singles.Vector4);
    procedure Set_Light_Position_Vector (Light_Position : Singles.Vector3);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Model_View_Matrix (Model_View_Matrix : Singles.Matrix4);
    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4);
    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);

end Shader_Manager;
