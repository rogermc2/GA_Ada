
with GL.Objects.Programs;

with E3GA;
with Multivectors; use Multivectors;

package Draw_1_1 is

   function Draw_Circle (Render_Program : GL.Objects.Programs.Program;
                         C1, C2, C3     : Normalized_Point) return Circle;
   function Draw_Line (Render_Program : GL.Objects.Programs.Program;
                       P1, P2         : Normalized_Point) return Line;
   procedure Draw_Reflected_Circle (Render_Program : GL.Objects.Programs.Program;
                                     C: Circle; DP : Dual_Plane);
   procedure Draw_Reflected_Line (Render_Program : GL.Objects.Programs.Program;
                                  L: Line; DP : Dual_Plane);
   procedure Draw_Rotated_Circle (Render_Program : GL.Objects.Programs.Program;
                                  C: Circle;  RV : TR_Versor);
   function New_Dual_Plane (P1 : Normalized_Point; Normal : E3GA.E3_Vector)
                            return Dual_Plane;
    function New_TR_Versor (L1 : Line) return TR_Versor;

end Draw_1_1;
