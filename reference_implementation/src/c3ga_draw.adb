
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with E3GA;
with GA_Draw;
with GA_Maths;

package body C3GA_Draw is

   procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Position : C3GA.Multivector; Colour : GL.Types.Colors.Color);

   --  -------------------------------------------------------------------------

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      Vec_3D  : E3GA.Vector;
      Tail    : E3GA.Vector;
   begin
      E3GA.Set_Coords (Vec_3D, C3GA.Get_Coord_1 (aVector),
                       C3GA.Get_Coord_2 (aVector), 0.0);
      E3GA.Set_Coords (Tail, 0.0, 0.0, 0.0);
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
                           Tail, Vec_3D, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw vector.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------                Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
  --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   Point_Position : C3GA.Normalized_Point; Colour : GL.Types.Colors.Color) is
      MV : C3GA.Multivector (2);
   begin
      C3GA.Set_Multivector (MV, Point_Position);
      Draw_Point (Render_Program, Model_View_Matrix, MV, Colour);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw point.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------                Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
   --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
   procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Position : C3GA.Multivector;
                         Colour : GL.Types.Colors.Color) is
      use GL.Types;
      Scale : Float := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
      Pos   : E3GA.Vector;
   begin
      E3GA.Set_Coords (Pos, Position.Coordinates (1), Position.Coordinates (2),
                       Position.Coordinates (3));
      GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                              Pos, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_Point.");
         raise;
   end Draw_Point;

   --  -------------------------------------------------------------------------                Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;

end C3GA_Draw;
