
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Shader is

   type Light_Uniform_IDs is record
      MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
   end record;

   type Uniform_IDs is record
      View_Matrix_ID           : GL.Uniforms.Uniform := 0;
      Model_Matrix_ID          : GL.Uniforms.Uniform := 0;
      Projection_Matrix_ID     : GL.Uniforms.Uniform := 0;
      Model_View_Matrix_ID     : GL.Uniforms.Uniform := 0;
      Light_Position_Matrix_ID : GL.Uniforms.Uniform := 0;
   end record;

   procedure Init (Render_Program : in out GL.Objects.Programs.Program;
                   Render_Uniforms : out Uniform_IDs);
   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4;
                               Render_Uniforms : Uniform_IDs);
   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4;
                              Render_Uniforms : Uniform_IDs);

end Shader;
