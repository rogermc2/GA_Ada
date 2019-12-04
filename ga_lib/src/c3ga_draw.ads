
with GL.Objects.Programs;
with GL.Types;

with C3GA;
with GA_Draw;
with Multivectors;
with Palet;

package C3GA_Draw is
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   MV                : Multivectors.Multivector;
                   Palet_Type        : Palet.Colour_Palet;
                   Method            : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle);
   procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Position          : C3GA.Normalized_Point;
                         Palet_Type        : Palet.Colour_Palet);

end C3GA_Draw;
