
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;
--  with E3GA_Utilities;
with GA_Maths;
with Multivector_Analyze;
--  with Multivector_Type;

package body C3GA_Draw is

    procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analyzed_MV : Multivector_Analyze.MV_Analysis;
                         Palet_Type : Palet.Colour_Palet;
                         Method : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle);
    procedure Draw_Flat (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analysis : Multivector_Analyze.MV_Analysis;
                         Palet_Type : Palet.Colour_Palet);
    procedure Draw_Round (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Analysis : Multivector_Analyze.MV_Analysis;
                          Palet_Type : Palet.Colour_Palet;
                          Method : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle);

    --  -------------------------------------------------------------------------

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    Model_View_Matrix : GL.Types.Singles.Matrix4;
                    MV                : Multivectors.Multivector;
                    Palet_Type : Palet.Colour_Palet;
                    Method : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle) is
        Analyzed_MV : Multivector_Analyze.MV_Analysis;
    begin
        Multivector_Analyze.Analyze (Analyzed_MV, MV, C3GA.no);
        Put_Line ("C3GA_Draw.Draw, Analyzed_MV.Points, " &
        Integer'Image (Multivector_Analyze.Number_Of_Points) & " points:");
        for index in 1 .. Multivector_Analyze.Number_Of_Points loop
            Put_Line (GL.Types.Single'Image (Analyzed_MV.Points (index) (GL.X)) & "  " &
            GL.Types.Single'Image (Analyzed_MV.Points (index) (GL.Y)) & "  " &
            GL.Types.Single'Image (Analyzed_MV.Points (index) (GL.Z)));
        end loop;
        Draw_C3GA (Render_Program, Model_View_Matrix, Analyzed_MV, Palet_Type, Method);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw MV.");
            raise;
    end Draw;

    --  -------------------------------------------------------------------------

--      procedure Draw (Render_Program : GL.Objects.Programs.Program;
--                      Model_View_Matrix : GL.Types.Singles.Matrix4;
--                      aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color) is
--          use Multivectors;
--          Vec_3D  : constant Vector := New_Vector (C3GA.Get_Coord_1 (aVector),
--                                          C3GA.Get_Coord_2 (aVector), 0.0);
--          Tail    : constant Vector := New_Vector (0.0, 0.0, 0.0);
--      begin
--          GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
--                               Tail, Vec_3D, Colour);
--
--      exception
--          when others =>
--              Put_Line ("An exception occurred in C3GA_Draw.Draw vector.");
--              raise;
--      end Draw;

    --  -------------------------------------------------------------------------

    procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Analyzed_MV : Multivector_Analyze.MV_Analysis;
                         Palet_Type : Palet.Colour_Palet;
                         Method : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle) is
        use Multivector_Analyze;
--          MV_Info : Multivector_Type.MV_Type_Record := Analyzed_MV.M_MV_Type;
    begin
--          GA_Utilities.Print_Multivector_Info ("C3GA_Draw.Draw_C3GA", MV_Info);
--          New_Line;
        if Analyzed_MV.M_Type.Model_Kind = Conformal_Model then
            case Analyzed_MV.M_Type.Blade_Class is
            when Flat_Blade =>
               Put_Line ("C3GA_Draw.Draw_C3GA Flat.");
               --  Draw_Flat doesn't use method
               Draw_Flat (Render_Program, Model_View_Matrix, Analyzed_MV, Palet_Type);
            when Free_Blade => null;
               Put_Line ("C3GA_Draw.Draw_C3GA Free.");
            when Round_Blade =>
               Put_Line ("C3GA_Draw.Draw_C3GA Round.");
               --  Draw_Round uses method
               Draw_Round (Render_Program, Model_View_Matrix, Analyzed_MV, Palet_Type, Method);
            when Tangent_Blade =>
                Put_Line ("C3GA_Draw.Draw_C3GA Tangent.");
            when others =>
                Put_Line ("C3GA_Draw.Draw_C3GA Others.");
                null;
            end case;
        else
            Put_Line ("C3GA_Draw.Draw_C3GA only processes conformal models.");
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
                         Palet_Type : Palet.Colour_Palet) is
        use Multivector_Analyze;
        Point_Pos  : constant C3GA.Vector_E3GA :=
                       C3GA.To_VectorE3GA (Analysis.Points (1));
        Direction  : constant C3GA.Vector_E3GA :=
                       C3GA.To_VectorE3GA (Analysis.Points (1));
        Scale      : Float;
        V          : constant C3GA.Vector_E3GA := (0.0, 0.0, 0.0);
    begin
        case Analysis.M_Type.Blade_Subclass is
            when Line_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Line.");
                --  Draw_Line doesn't use method
                GA_Draw.Draw_Line (Render_Program, Model_View_Matrix,
                                   Point_Pos, Direction);
            when Plane_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Plane.");
            when Point_Subclass =>
                Put_Line ("C3GA_Draw.Draw_Flat Point.");
                Scale := 4.0 / 3.0 * GA_Maths.PI * Palet.Point_Size ** 3;
                GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                        Point_Pos, Scale, V, Palet_Type);
            when others => null;
        end case;

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Flat.");
            raise;
    end Draw_Flat;

    --  -------------------------------------------------------------------------
    --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
--      procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
--                           Model_View_Matrix : GL.Types.Singles.Matrix4;
--                           L : Multivectors.Vector;
--                           Colour : GL.Types.Colors.Color) is
--          Scale : constant Float := 4.0 / 3.0 * GA_Maths.PI * Palet.Point_Size ** 3;
--          V     : C3GA.Vector_E3GA;
--      begin
--          GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
--                                  C3GA.To_VectorE3GA (L), Scale, V, Colour);
--
--      exception
--          when others =>
--              Put_Line ("An exception occurred in C3GA_Draw.Draw_Line.");
--              raise;
--      end Draw_Line;

    --  -------------------------------------------------------------------------
    --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
    procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Position : C3GA.Normalized_Point;
                          Palet_Type : Palet.Colour_Palet) is
        Scale : constant Float := 4.0 / 3.0 * GA_Maths.PI * Palet.Point_Size ** 3;
        V     : constant C3GA.Vector_E3GA := (0.0, 0.0, 0.0);
    begin
        --        E3GA_Utilities.Print_Vector ("Draw_Point, Pos", Pos);
        GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                C3GA.To_VectorE3GA (Position), Scale, V, Palet_Type);

    exception
        when others =>
            Put_Line ("An exception occurred in C3GA_Draw.Draw_Point.");
            raise;
    end Draw_Point;

    --  -------------------------------------------------------------------------

    procedure Draw_Round (Render_Program : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Analysis : Multivector_Analyze.MV_Analysis;
                          Palet_Type : Palet.Colour_Palet;
                          Method : GA_Draw.Method_Type :=
                            GA_Draw.Draw_Bivector_Circle) is
        use GL.Types;
        use GL.Types.Singles;
        use Multivector_Analyze;
        use C3GA;
        Point_Pos  : constant Vector_E3GA :=
                       To_VectorE3GA (Analysis.Points (1));
        P_Scale    : constant Float :=
                       4.0 / 3.0 * GA_Maths.PI * (Palet.Point_Size ** 3);
        M_Vectors  : constant Vector_Array := Analysis.M_Vectors;
        M_Vec1     : constant Vector_E3GA := To_VectorE3GA (M_Vectors(1));
        M_Vec2     : constant Vector_E3GA := To_VectorE3GA (M_Vectors(2));
        M_Vec3     : constant Vector_E3GA := To_VectorE3GA (M_Vectors(3));
        Radius     : constant Single := Single (Analysis.Scalors (1));
        VC         : constant Vector_E3GA := (0.0, 0.0, 0.0);
    begin
      case Analysis.M_Type.Blade_Subclass is
         when Point_Pair_Subclass =>
            Put_Line ("C3GA_Draw.Draw_Round Point Pair.");
            GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                    Point_Pos + Radius * M_Vec1,
                                    P_Scale, VC, Palet_Type,
                                    GA_Draw.Draw_TV_Sphere);
            GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                                    Point_Pos - Radius * M_Vec1,
                                    P_Scale, VC, Palet_Type,
                                    GA_Draw.Draw_TV_Sphere);
         when Circle_Subclass =>
            Put_Line ("C3GA_Draw.Draw_Round Circle.");
            GA_Draw.Draw_Bivector (Render_Program => Render_Program,
                                   Model_View_Matrix => Model_View_Matrix,
                                   Base              => Point_Pos,
                                   Normal            => M_Vec3,
                                   Ortho_1           => M_Vec1,
                                   Ortho_2           => M_Vec2,
                                   Palet_Type        => Palet_Type,
                                   Scale             => Float (Radius),
                                   Method            => Method);
         when Sphere_Subclass =>
            Put_Line ("C3GA_Draw.Draw_Round Sphere.");
            GA_Draw.Draw_Trivector
              (Render_Program, Model_View_Matrix,
               Point_Pos, P_Scale, VC, Palet_Type, GA_Draw.Draw_TV_Sphere);
         when others => null;
      end case;

    exception
      when others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_Round.");
         raise;
    end Draw_Round;

    --  -------------------------------------------------------------------------

end C3GA_Draw;
