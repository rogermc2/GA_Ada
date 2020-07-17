
--  Based on libgasandbox.draw.h

with GL.Objects.Programs;
with GL.Types; use GL.Types;

with C3GA;
with Multivector_Analyze;
with Palet;

package GA_Draw is

    type Method_Type is (Draw_Method_Undefined, Draw_Bivector_Circle,
                         Draw_Bivector_Parallelogram,
                         Draw_Bivector_Parallelogram_No_Vectors,
                         Draw_Bivector_Cross, Draw_Bivector_Curly_Tail,
                         Draw_Bivector_Swirl, Draw_Bivector_Circle_Outline,
                         Draw_TV_Sphere, Draw_TV_Cross, Draw_TV_Curly_Tail,
                         Draw_TV_Parellelepiped,
                         Draw_TV_Parellelepiped_No_Vectors);
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Model_View_Matrix : GL.Types.Singles.Matrix4;
                             Base, Normal, Ortho_1, Ortho_2 : C3GA.Vector_E3;
                             Palet_Type               : Palet.Colour_Palet;
                             Scale : float := 1.0;
                             Method : Method_Type := Draw_Bivector_Circle);
    procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         aPoint, Direction : C3GA.Vector_E3;
                         Weight            : Float := 1.0);
    procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                              Model_View_Matrix : GL.Types.Singles.Matrix4;
                              Base : C3GA.Vector_E3; Scale : float := 1.0;
                              Palet_Type : Palet.Colour_Palet;
                              Method  : Method_Type := Draw_TV_Sphere);
    procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                              Model_View_Matrix : GL.Types.Singles.Matrix4;
                              Base : C3GA.Vector_E3; Scale : float := 1.0;
                              V : Multivector_Analyze.E3_Vector_Array;
--                                Palet_Type : Palet.Colour_Palet;
                              Method  : Method_Type := Draw_TV_Sphere);
    procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                           Model_View_Matrix : GL.Types.Singles.Matrix4;
                           Tail, Direction : C3GA.Vector_E3;
                           Scale : float := 1.0);

end GA_Draw;
