
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Shader_Manager is

   type Light_Uniform_IDs is record
      MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
   end record;

   type Pick_Uniforms_IDs is record
      MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
      Pick_Colour_ID : GL.Uniforms.Uniform := 0;
   end record;

   type Shader_Uniforms is record
      Ambient_Colour_ID     : GL.Uniforms.Uniform := 0;
      Diffuse_Colour_ID     : GL.Uniforms.Uniform := 0;
      Drawing_Colour_ID     : GL.Uniforms.Uniform := 0;
      Light_Direction_ID    : GL.Uniforms.Uniform := 0;
      Light_Position_ID     : GL.Uniforms.Uniform := 0;
      Line_Width_ID         : GL.Uniforms.Uniform := 0;
      Model_Matrix_ID       : GL.Uniforms.Uniform := 0;
      Projection_Matrix_ID  : GL.Uniforms.Uniform := 0;
      Rotation_Matrix_ID    : GL.Uniforms.Uniform := 0;
      Translation_Matrix_ID : GL.Uniforms.Uniform := 0;
      View_Matrix_ID        : GL.Uniforms.Uniform := 0;
   end record;

   procedure Init (Render_Program : in out GL.Objects.Programs.Program);
   procedure Set_Ambient_Colour (Ambient_Colour : Singles.Vector4);
   procedure Set_Diffuse_Colour (Diffuse_Colour : Singles.Vector4);
   procedure Set_Drawing_Colour (Drawing_Colour : Singles.Vector4);
   procedure Set_Light_Direction_Vector (Light_Direction : Singles.Vector3);
   procedure Set_Light_Position_Vector (Light_Position : Singles.Vector3);
   procedure Set_Line_Width (Width : Single);
   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
   procedure Set_Pick_Colour (Pick_Colour : Singles.Vector4);
   procedure Set_Pick_Matrix (Pick_Matrix : Singles.Matrix4);
   procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4);
   procedure Set_Rotation_Matrix (Rotation_Matrix : Singles.Matrix4);
   procedure Set_Translation_Matrix (Translation_Matrix : Singles.Matrix4);
   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);

end Shader_Manager;
