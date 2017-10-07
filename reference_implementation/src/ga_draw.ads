
--  Based on libgasandbox.draw.h

with GL.Types.Colors; use GL.Types.Colors;
with GL.Objects.Programs;

with GA_Maths;
with E2GA;

package GA_Draw is
    type Colour_Palet is private;

    type Bivector_Method_Type is (Draw_Bivector_Circle, Draw_Bivector_Parallelogram,
                                  Draw_Bivector_Parallelogram_No_Vectors,
                                  Draw_Bivector_Cross, Draw_Bivector_Curly_Tail,
                                  Draw_Bivector_Swirl, Draw_Bivector_Circle_Outline);
    type Draw_Mode is (OD_Shade, OD_Wireframe, OD_Magnitude, OD_Orientation);

    procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                   MV_Matrix, Proj_Matrix : GL.Types.Singles.Matrix4;
                   Tail, Direction : GA_Maths.Vector;
                   Colour : Color;
                   Scale : Gl.Types.Single);

    --  procedure Draw_Bivector (Base : E3GA.Vector, Normal : E3GA.Vector,
    --                           Factor1 : E3GA.Vector, Factor2 : Vector,
    --                           Scale _ float, Method : Draw_Mode := Draw_By_Circle;
    --                           Palet : Integer'access := null);
    --  Draw_Bivector draws a bivetor at Base (:= null for origin).
    --  The bivector is specified by Normal, Factor1, Factor1 and Scale.
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   Base, Factor_1, Factor_2 : GA_Maths.Vector;
                   Scale  : GL.Types.Single;
                   Method : Bivector_Method_Type;
                   Colour : Color := (1.0, 1.0, 1.0, 1.0));

    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   BV             : E2GA.Bivector;
                   Colour         : Color := (1.0, 1.0, 1.0, 1.0);
                   Scale          : GL.Types.Single);
--      procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
--                               MV             : E2GA.Multivector;
--                               Colour         : Color := (1.0, 1.0, 1.0, 1.0);
--                               Scale          : GL.Types.Single := 1.0;
    function Get_Draw_Mode return Draw_Mode;
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

    type Draw_State is record
        Diffuse     : GL.Types.Single := 1.0;
        Sphere      : GL.Types.Single := 0.0;
        Point_Size  : GL.Types.Single := 0.2;
        Line_Length : GL.Types.Single := 6.0;
        Plane_Size  : GL.Types.Single := 6.0;
    end record;
end GA_Draw;
