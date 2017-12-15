
with Ada.Text_IO; use Ada.Text_IO;

with E3GA;
with GA_Draw;

package body C3GA_Draw is
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      Vec_3D  : E3GA.Vector;
      Tail    : E3GA.Vector;
   begin
      --  MV_Analysis (MV) declares A as a variable of class mvAnalysis
      --  constructed from v1
      E3GA.Set_Coords (Vec_3D, C3GA.Get_Coord_1 (aVector),
                       C3GA.Get_Coord_2 (aVector), 0.0);
      E3GA.Set_Coords (Tail, 0.0, 0.0, 0.0);
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                           Tail, Vec_3D, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw 1.");
         raise;
   end Draw;
end C3GA_Draw;
