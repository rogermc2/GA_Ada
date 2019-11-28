
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

with C3GA;
with Multivectors;

package C3GA_Draw is
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   MV                : Multivectors.Multivector;
                   Colour            : GL.Types.Colors.Color);
   procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Position          : C3GA.Normalized_Point;
                         Colour            : GL.Types.Colors.Color);

end C3GA_Draw;
