
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
                    Scale : float := 1.0) is
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

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                    BV     : E2GA.Bivector;
                    Method_Type : GA_Draw.Bivector_Method_Type
                               := GA_Draw.Draw_Bivector_Circle;
                   Colour : GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0)) is
      use GA_Draw;
      use GA_Maths;
      Scale : float;
    begin
      if Get_Draw_Mode = OD_Magnitude then
         Scale := Float_Functions.Sqrt (Abs (float (BV.e1e2_Coord (1)))) / Pi;
      else
         Scale := 1.0;
      end if;
      Draw_Bivector (Render_Program, Model_View_Matrix, Projection_Matrix,
                     E2GA.Dual(BV), Ortho_1, Ortho_2, Scale);

    end Draw;

--  --------------------------------------------------------------------------------

end E2GA_Draw;
