
with GL.Objects.Programs;
with Multivectors; use Multivectors;

package Draw_1_1 is

   function Draw_Circle (Render_Program : GL.Objects.Programs.Program;
                         C1, C2, C3     : Normalized_Point) return Circle;
   function Draw_Line (Render_Program : GL.Objects.Programs.Program;
                       P1, P2         : Normalized_Point) return Line;

end Draw_1_1;
