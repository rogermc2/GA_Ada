
with Ada.Text_IO; use Ada.Text_IO;

with GA_Draw;

package body E2GA_Draw is
--  Method & flags are dependent on what 'X' represents.
--  They are forwarded to drawVector, drawBivector, drawTrivector.
--
--  Currently, 'method' is some integer in range [0, n), where each
--  integer indicates a different way of drawing the multivector.
--
--  The Palet can be used to specify foreground, background and outline color.
--
--  Uses g_drawState for some extra flags!
--  If this gets annoying, allow DrawState to be passed along
--  as argument (and also integrate 'Palet')
    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                    aVector : GA_Maths.Vector_2D; Colour : GL.Types.Colors.Color;
                    Scale : GL.Types.Single := 0.0) is
      Vec_3D  : GA_Maths.Vector := GA_Maths.To_3D (aVector);
      Tail    : GA_Maths.Vector;
    begin
        --  MV_Analysis (MV) declares A as a variable of class mvAnalysis
        --  constructed from v1
        GA_Maths.Set_Coords (Tail, 0.0, 0.0, 0.0);
        GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                             Tail, Vec_3D, Colour, Scale);
    end Draw;

--  ----------------------------------------------------------------------------

    procedure Draw (Render_Program                       : GL.Objects.Programs.Program;
                    Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                    BV                                   : E2GA.Bivector;
                    Colour                               : GL.Types.Colors.Color;
                    Scale                                : GL.Types.Single := 0.0) is
    begin
        GA_Draw.Draw_Bivector (Render_Program, Model_View_Matrix,
                               Projection_Matrix, BV, Colour, Scale);
    end Draw;

--  --------------------------------------------------------------------------------

end E2GA_Draw;
