
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Blade;
--  with E3GA_Utilities;
--  with E3GA;
with GA_Draw;
with GA_Maths;
with Multivector_Analyze;
--  with Multivector_Type;

package body C3GA_Draw is

    procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analyzed_MV : Multivector_Analyze.MV_Analysis;
                         Colour : GL.Types.Colors.Color);
    procedure Draw_Flat (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analysis : Multivector_Analyze.MV_Analysis;
                         Colour : GL.Types.Colors.Color);
    procedure Draw_Round (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Analysis : Multivector_Analyze.MV_Analysis;
                          Colour : GL.Types.Colors.Color);

    --  -------------------------------------------------------------------------

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix : GL.Types.Singles.Matrix4;
                    MV : Multivectors.Multivector; Colour : GL.Types.Colors.Color) is
        Analyzed_MV : Multivector_Analyze.MV_Analysis;
        NO          : constant Multivectors.Multivector := C3GA.no;
    begin
        Multivector_Analyze.Analyze (Analyzed_MV, MV, NO);
        Draw_C3GA (Render_Program, Model_View_Matrix, Analyzed_MV, Colour);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw MV.");
            raise;
    end Draw;

    --  -------------------------------------------------------------------------

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix : GL.Types.Singles.Matrix4;
                    aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color) is
        use Multivectors;
        Vec_3D  : constant Vector := New_Vector (C3GA.Get_Coord_1 (aVector),
                                        C3GA.Get_Coord_2 (aVector), 0.0);
        Tail    : constant Vector := New_Vector (0.0, 0.0, 0.0);
    begin
        GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
                             Tail, Vec_3D, Colour);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw vector.");
            raise;
    end Draw;

    --  -------------------------------------------------------------------------

    procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analyzed_MV : Multivector_Analyze.MV_Analysis;
                         Colour : GL.Types.Colors.Color) is
        use Multivector_Analyze;
--          MV_Info : Multivector_Type.MV_Type_Record := Analyzed_MV.M_MV_Type;
    begin
--          GA_Utilities.Print_Multivector_Info ("C3GA_Draw.Draw_C3GA", MV_Info);
--          New_Line;
        if Analyzed_MV.M_Type.Model_Kind = Conformal_Model then
            case Analyzed_MV.M_Type.Blade_Class is
            when Flat_Blade =>
                Put_Line ("C3GA_Draw.Draw_C3GA Flat.");
                Draw_Flat (Render_Program, Model_View_Matrix, Analyzed_MV, Colour);
            when Free_Blade => null;
                Put_Line ("C3GA_Draw.Draw_C3GA Free.");
            when Round_Blade =>
                Put_Line ("C3GA_Draw.Draw_C3GA Round.");
                Draw_Round (Render_Program, Model_View_Matrix, Analyzed_MV, Colour);
            when Tangent_Blade =>
                Put_Line ("C3GA_Draw.Draw_C3GA Tangent.");
            when others =>
                Put_Line ("C3GA_Draw.Draw_C3GA Others.");
                null;
            end case;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_C3GA.");
            raise;
    end Draw_C3GA;

    --  -------------------------------------------------------------------------

    procedure Draw_Flat (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analysis : Multivector_Analyze.MV_Analysis;
                         Colour : GL.Types.Colors.Color) is
        use Multivector_Analyze;
        Point_Pos  : constant C3GA.Vector_E3GA :=
                       C3GA.To_VectorE3GA (Analysis.M_Points (1));
        Scale       : Float;
        V           : C3GA.Vector_E3GA;
    begin
        case Analysis.M_Type.Blade_Subclass is
            when Line_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Line.");
                GA_Draw.Draw_Line (Render_Program, Model_View_Matrix,
                                   Point_Pos, Analysis.M_Vectors (1),
                                   Colour);
--                                     Analysis.M_Scalors (1));
            when Plane_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Plane.");
            when Point_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Point.");
                Scale := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
                GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                        Point_Pos, Scale, V, Colour);
            when others => null;
        end case;

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Flat.");
            raise;
    end Draw_Flat;

    --  -------------------------------------------------------------------------
    --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
    procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         L : Multivectors.Vector;
                         Colour : GL.Types.Colors.Color) is
        Scale : constant Float := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
        V     : C3GA.Vector_E3GA;
    begin
        GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                C3GA.To_VectorE3GA (L), Scale, V, Colour);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Line.");
            raise;
    end Draw_Line;

    --  -------------------------------------------------------------------------
    --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
    procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Position : C3GA.Normalized_Point;
                          Colour : GL.Types.Colors.Color) is
        Scale : constant Float := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
        V     : C3GA.Vector_E3GA;
    begin
        --        E3GA_Utilities.Print_Vector ("Draw_Point, Pos", Pos);
        GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                C3GA.To_VectorE3GA (Position), Scale, V, Colour);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Point.");
            raise;
    end Draw_Point;

    --  -------------------------------------------------------------------------

    procedure Draw_Round (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Analysis : Multivector_Analyze.MV_Analysis;
                          Colour : GL.Types.Colors.Color) is
        use Multivector_Analyze;
        Point_Pos  : constant C3GA.Vector_E3GA :=
                       C3GA.To_VectorE3GA (Analysis.M_Points (1));
        P_Scale    : constant Float :=
                       4.0 / 3.0 * GA_Maths.PI * (GA_Draw.Point_Size ** 3);
        V          : C3GA.Vector_E3GA;
    begin
        case Analysis.M_Type.Blade_Subclass is
            when Point_Pair_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Round Point Pair.");
                GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                        Point_Pos, P_Scale, V, Colour);
            when Circle_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Round Circle.");
            when Sphere_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Round Sphere.");
                GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                        Point_Pos, P_Scale, V, Colour);
            when others => null;
        end case;

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Round.");
            raise;
    end Draw_Round;

    --  -------------------------------------------------------------------------

end C3GA_Draw;
