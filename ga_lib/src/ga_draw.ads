
--  Based on libgasandbox.draw.h

with GL.Types.Colors; use GL.Types.Colors;
with GL.Objects.Programs;
with GL.Types; use GL.Types;

with C3GA;
--  with GA_Maths;
with Geosphere;
with Multivectors;

package GA_Draw is

    type Bivector_Method_Type is (Draw_Bivector_Circle, Draw_Bivector_Parallelogram,
                                  Draw_Bivector_Parallelogram_No_Vectors,
                                  Draw_Bivector_Cross, Draw_Bivector_Curly_Tail,
                                  Draw_Bivector_Swirl, Draw_Bivector_Circle_Outline);

    type Trivector_Method_Type is (Draw_TV_Sphere, Draw_TV_Cross, Draw_TV_Curly_Tail,
                                   Draw_TV_Parellelepiped,
                                   Draw_TV_Parellelepiped_No_Vectors);
    type Draw_Mode is (OD_Shade, OD_Wireframe, OD_Magnitude, OD_Orientation);

    type Draw_State is private;

    --  Draw_Bivector draws a bivector at Base (:= null for origin).
    --  The bivector is specified by Normal, Factor1, Factor1 and Scale.
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Translation_Matrix : GL.Types.Singles.Matrix4;
                             Normal, Ortho_1, Ortho_2 : Multivectors.Vector;
                             Colour : GL.Types.Colors.Color; Scale : float := 1.0;
                             Method : Bivector_Method_Type := Draw_Bivector_Circle);
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Base, Ortho_1, Ortho_2 : Multivectors.Vector;
                             Colour : GL.Types.Colors.Color; Scale : float := 1.0;
                             Method : Bivector_Method_Type := Draw_Bivector_Circle);
    procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         aPoint, Direction : C3GA.Vector_E3GA;
                         Weight : GL.Types.Single;
                         Colour : GL.Types.Colors.Color);
    procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                              Model_View_Matrix : GL.Types.Singles.Matrix4;
                              Base : C3GA.Vector_E3GA; Scale : float := 1.0;
                              V : C3GA.Vector_E3GA;
                              Colour : GL.Types.Colors.Color;
                              Method  : Trivector_Method_Type := Draw_TV_Sphere);
    procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                           MV_Matrix : GL.Types.Singles.Matrix4;
                           Tail, Direction : C3GA.Vector_E3GA;
                           Colour : Color; Scale : float := 1.0);
    function Get_Draw_Mode return Draw_Mode;
    function Get_Line_Length return Float;
--      procedure Graphic_Shader_Locations (Render_Program : GL.Objects.Programs.Program;
--                                          MV_Matrix_ID, Projection_Matrix_ID,
--                                          Colour_Location : out GL.Uniforms.Uniform);
    function Point_Size return Float;
    procedure Set_Foreground_Colour (Fore_Colour : Color);
    --     procedure Set_Background_Colour (Back_Colour : Color);
    procedure Set_Draw_Mode (Mode : Draw_Mode);
    procedure Set_Ol_Colour (Ol_Colour : Color);
    procedure Set_Point_Size (Point_Size : Float);
    procedure Init_Projection_Matrix (Proj_Matrix : out GL.Types.Singles.Matrix4;
                                      Near : GL.Types.Single := -100.0;
                                      Far  : GL.Types.Single := 100.0);
private

    type Draw_State is record
        Ambient      : Float := 1.0;
        Diffuse      : Float := 0.0;
        Point_Size   : Float := 0.2;
        Line_Length  : Float := 6.0;
        Plane_Size   : Float := 6.0;
        M_Draw_Mode  : Draw_Mode := OD_Magnitude;
        M_Sphere     : Geosphere.Geosphere;
        --  M_Sphere_GL_List : GL.Types.UInt;
    end record;

end GA_Draw;
