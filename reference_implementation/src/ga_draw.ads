
--  Based on libgasandbox.draw.h

with GL.Types.Colors; use GL.Types.Colors;
with GL.Objects.Programs;
with GL.Uniforms;

with GA_Maths;
with Geosphere;
with E2GA;
with E3GA;

package GA_Draw is

   type Colour_Palet is private;

   type Bivector_Method_Type is (Draw_Bivector_Circle, Draw_Bivector_Parallelogram,
                                 Draw_Bivector_Parallelogram_No_Vectors,
                                 Draw_Bivector_Cross, Draw_Bivector_Curly_Tail,
                                 Draw_Bivector_Swirl, Draw_Bivector_Circle_Outline);
   type Trivector_Method_Type is (Draw_TV_Sphere, Draw_TV_Cross, Draw_TV_Curly_Tail,
                                  Draw_TV_Parellelepiped,
                                  Draw_TV_Parellelepiped_No_Vectors);
   type Draw_Mode is (OD_Shade, OD_Wireframe, OD_Magnitude, OD_Orientation);

   type Draw_State (Max_Vertices : GL.Types.Int := 0;
                    Max_Faces : GL.Types.Int := 0) is private;

   procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                          MV_Matrix, Proj_Matrix : GL.Types.Singles.Matrix4;
                          Tail, Direction : E3GA.Vector;
                          Colour : Color; Scale : float);

   --  Draw_Bivector draws a bivector at Base (:= null for origin).
   --  The bivector is specified by Normal, Factor1, Factor1 and Scale.
   procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                            Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                            Normal, Ortho_1, Ortho_2 : E3GA.Vector;
                            Colour : GL.Types.Colors.Color; Scale : float := 1.0;
                            Method : Bivector_Method_Type := Draw_Bivector_Circle);
   procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                            Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                            Base, Normal, Ortho_1, Ortho_2 : E3GA.Vector;
                            Colour : GL.Types.Colors.Color; Scale : float := 1.0;
                            Method : Bivector_Method_Type := Draw_Bivector_Circle);
   procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                             Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                             Base : E3GA.Vector; Scale : float := 1.0;
                             Colour : GL.Types.Colors.Color; V : E3GA.Vector;
                             Method : Trivector_Method_Type := Draw_TV_Sphere);
   procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                             Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                             Base : E3GA.Vector; Scale : float := 1.0;
                             Colour : GL.Types.Colors.Color;
                             Method : Trivector_Method_Type := Draw_TV_Sphere);

   function Get_Draw_Mode return Draw_Mode;
   procedure Graphic_Shader_Locations (Render_Program : GL.Objects.Programs.Program;
                                       MV_Matrix_ID, Projection_Matrix_ID,
                                       Colour_Location : out GL.Uniforms.Uniform);
   function Point_Size return GL.Types.Single;
   procedure Set_Foreground_Colour (Fore_Colour : Color);
   procedure Set_Background_Colour (Back_Colour : Color);
   procedure Set_Draw_Mode (Mode : Draw_Mode);
   procedure Set_Ol_Colour (Ol_Colour : Color);
   procedure Set_Point_Size (Point_Size : GL.Types.Single);

private
   type Colour_Palet is record
      Foreground_Colour : Color := (1.0, 0.0, 1.0, 1.0);
      Background_Colour : Color := (0.0, 1.0, 0.0, 1.0);
      Ol_Colour         : Color := (0.0, 0.0, 0.0, 1.0);
   end record;

   type Draw_State
     (Max_Vertices : GL.Types.Int := 0; Max_Faces : GL.Types.Int := 0) is record
      Ambient      : GL.Types.Single := 1.0;
      Diffuse      : GL.Types.Single := 1.0;
      Point_Size   : GL.Types.Single := 0.2;
      Line_Length  : GL.Types.Single := 6.0;
      Plane_Size   : GL.Types.Single := 6.0;
      M_Draw_Mode  : Draw_Mode := OD_Magnitude;
      M_Sphere     : Geosphere.Geosphere (Max_Vertices, Max_Faces);
      --  M_Sphere_GL_List : GL.Types.UInt;
   end record;

end GA_Draw;
