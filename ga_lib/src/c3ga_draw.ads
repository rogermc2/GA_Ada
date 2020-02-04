
with GL.Objects.Programs;
with GL.Types;

with GA_Draw;
with Multivector_Analyze;
with Multivectors;
with Palet;

package C3GA_Draw is
   procedure Draw (Render_Program    : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   MV                : Multivectors.Multivector;
                   Palet_Type        : Palet.Colour_Palet;
                   Method            : GA_Draw.Method_Type := GA_Draw.Draw_TV_Sphere);
   procedure Draw_Point (Render_Program    : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analysis          : Multivector_Analyze.MV_Analysis;
                         Palet_Type        : Palet.Colour_Palet);

end C3GA_Draw;
