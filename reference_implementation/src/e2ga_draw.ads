
with GL.Types.Colors;
with GL.Objects.Programs;

with E2GA;
with GA_Draw;
with GA_Maths;

package E2GA_Draw is
    --  procedure Draw (X : E2GA.Multivector, Method : Integer := 0;
    --                  Palet : Integer'access := null);
    --  Method is dependent on what 'X' represents.
    --  It is forwarded to drawVector, drawBivector, drawTrivector.
    --  Currently, 'method' is some integer in range [0, n), where each
    --  integer indicates a different way of drawing the multivector.
    --  Palet can be used to specify foreground, background and outline color.
    --  Uses g_drawState for some extra flags or allow DrawState to be
    --  passed as an argument (and also integrate 'Palet')
    --  Each procedure Draw implements a separate case of the draw in e2ga_Draw.cpp
    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                    MV : in out E2GA.Multivector;
                    Method : GA_Draw.Bivector_Method_Type
                               := GA_Draw.Draw_Bivector_Circle;
                    Colour : GL.Types.Colors.Color := (0.0, 0.5, 0.5, 1.0));
   --  Case Vector
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                    aVector : E2GA.Vector; Colour : GL.Types.Colors.Color;
                    Scale : float := 1.0);
   --  Case Bivector
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   BV : E2GA.Bivector;
                   Method_Type : GA_Draw.Bivector_Method_Type
                               := GA_Draw.Draw_Bivector_Circle);
end E2GA_Draw;
