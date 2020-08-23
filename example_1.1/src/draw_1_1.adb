
--  with Blade_Types;
with C3GA;
with C3GA_Draw;
with GA_Maths;
--  with GA_Utilities;
with Metric;
with Multivector_Utilities;

package body Draw_1_1 is

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

   function Draw_Line (Render_Program  : GL.Objects.Programs.Program;
                       P1, P2          : Normalized_Point) return Line is
      aLine : constant Line := C3GA.Set_Line (P1, P2);
   begin
      C3GA_Draw.Draw (Render_Program, aLine);
      return aLine;
   end Draw_Line;

   --  ---------------------------------------------------------------------

   procedure Draw_Plane (Render_Program : GL.Objects.Programs.Program;
                         DP : Dual_Plane) is
   begin
      C3GA_Draw.Draw (Render_Program, DP);
   end Draw_Plane;

   --  ---------------------------------------------------------------------

   procedure Draw_Reflected_Circle (Render_Program : GL.Objects.Programs.Program;
                                    C              : Circle; DP  : Dual_Plane) is
   begin
      C3GA_Draw.Draw (Render_Program, Multivector_Utilities.Reflect (C, DP));
   end Draw_Reflected_Circle;

   --  ---------------------------------------------------------------------

   procedure Draw_Reflected_Line (Render_Program : GL.Objects.Programs.Program;
                                  L              : Line; DP    : Dual_Plane) is
   begin
      C3GA_Draw.Draw (Render_Program, Multivector_Utilities.Reflect (L, DP));
   end Draw_Reflected_Line;

   --  ---------------------------------------------------------------------

   function Draw_Rotated_Circle
     (Render_Program : GL.Objects.Programs.Program;
                       C: Circle;  RV : TR_Versor) return Circle is
      MV : constant Multivector :=
             Multivector_Utilities.Rotate (Multivector (C), RV);
      RC : constant Circle := To_Circle (MV);
   begin
      C3GA_Draw.Draw (Render_Program, RC);
      return RC;
   end Draw_Rotated_Circle;

   --  --------------------------------------------------------------------

   function New_Dual_Plane (P1 : Normalized_Point; Normal : E3GA.E3_Vector)
                             return Dual_Plane is
      OP : Multivector := Outer_Product (E3GA.To_MV_Vector (Normal), C3GA.ni);
   begin
      OP := Left_Contraction (P1, OP, Metric.C3_Metric);
      --  Checked OK against C++
      return To_Dual_Plane (OP);
   end New_Dual_Plane;

   --  ---------------------------------------------------------------------

   function New_TR_Versor (L1 : Line) return TR_Versor is
      use Metric;
      Phi    : constant Float := 0.5 * GA_Maths.Pi;
      Exp_MV : constant Multivector :=
                 Exp (0.5 * Phi * Dual (L1, C3_Metric), C3_Metric);
      LR     :  constant Dual_Line := To_Dual_Line (Exp_MV);
   begin
      return To_TRversor (LR);
   end New_TR_Versor;

   --  ---------------------------------------------------------------------
end Draw_1_1;
