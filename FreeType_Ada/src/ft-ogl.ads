
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

package FT.OGL is
   type Character_Record is private;

   function Character_Data_To_String (Char : Character;
                                      Data : Character_Record) return String;
   procedure Initialize_Font_Data (Font_File : String);
   procedure Print_Character_Metadata (Data : Character_Record);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID, Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4);
private
   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Width     : GL.Types.Int := 0;
      Rows      : GL.Types.Int := 0;
      Left      : GL.Types.Int := 0;
      Top       : GL.Types.Int := 0;
      Advance_X : GL.Types.Int := 0;
   end record;

end FT.OGL;
