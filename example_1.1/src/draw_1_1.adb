
with C3GA;
with C3GA_Draw;

package body Draw_1_1 is

   function Draw_Line (Render_Program : GL.Objects.Programs.Program;
                       P1, P2          : Normalized_Point) return Line is
      aLine : constant Line := C3GA.Set_Line (P1, P2);
   begin
      C3GA_Draw.Draw (Render_Program, aLine);
      return aLine;
   end Draw_Line;

   --  ---------------------------------------------------------------------

   function Draw_Circle (Render_Program : GL.Objects.Programs.Program;
                         C1, C2, C3     : Normalized_Point) return Circle is
      OP      : Multivector;
      aCircle : Circle;
   begin
      OP := Outer_Product (C1, Outer_Product (C2, C3));
      aCircle := To_Circle (OP);
      C3GA_Draw.Draw (Render_Program, aCircle);
      return aCircle;
   end Draw_Circle;

   --  ---------------------------------------------------------------------

end Draw_1_1;
