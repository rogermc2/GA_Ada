
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

with C3GA;

package C3GA_Draw is
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0);
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   Point_Position : C3GA.Normalized_Point;
                   Colour : GL.Types.Colors.Color);
end C3GA_Draw;
