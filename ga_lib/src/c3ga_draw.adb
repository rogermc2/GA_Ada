
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Blade;
with E3GA_Utilities;
with E3GA;
with GA_Draw;
with GA_Maths;
with Multivector_Analyze;

package body C3GA_Draw is

   procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Analysis : Multivector_Analyze.MV_Analysis;
                        Colour : GL.Types.Colors.Color;
                        Scale : float := 1.0);

   --  -------------------------------------------------------------------------

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   MV : Multivectors.Multivector; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      theAnalysis : Multivector_Analyze.MV_Analysis;
   begin
      Multivector_Analyze.Analyze (theAnalysis, MV);
      Draw_C3GA (Render_Program, Model_View_Matrix, theAnalysis, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw MV.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   aVector : C3GA.Vector_E3GA; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      use Multivectors;
      Vec_3D  : Vector := New_Vector (C3GA.Get_Coord_1 (aVector),
                       C3GA.Get_Coord_2 (aVector), 0.0);
      Tail    : Vector := New_Vector (0.0, 0.0, 0.0);
   begin
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
                           Tail, Vec_3D, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw vector.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------

   procedure Draw_Flat (Render_Program : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Analysis : Multivector_Analyze.MV_Analysis;
                        Colour : GL.Types.Colors.Color;
                        Scale : float := 1.0) is
      use Multivector_Analyze;
   begin
      case Analysis.M_Type.Blade_Subclass is
         when Line_Subclass
            => GA_Draw.Draw_Line (Render_Program, Model_View_Matrix,
                                  Analysis.M_Points (1), Analysis.M_Vectors (1),
                                  Analysis.M_Scalors (1), Colour);
         when Plane_Subclass
            => null;
         when Point_Subclass
            => null;
         when others => null;
      end case;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_Flat.");
         raise;
   end Draw_Flat;

   --  -------------------------------------------------------------------------
   --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
   procedure Draw_Line (Render_Program : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        L : Multivectors.Vector;
                        Colour : GL.Types.Colors.Color) is
      use GL.Types;
      Scale : Float := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
   begin

      GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                              Multivectors.Multivector (L), Colour, 1.0 * Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_Line.");
         raise;
   end Draw_Line;

   --  -------------------------------------------------------------------------
   --  Based on c3ga_draw.drawFlat A.bladeSubclass() == mvAnalysis::POINT
   procedure Draw_Point (Render_Program : GL.Objects.Programs.Program;
                         Model_View_Matrix : GL.Types.Singles.Matrix4;
                         Position : C3GA.Normalized_Point;
                         Colour : GL.Types.Colors.Color) is
      use GL.Types;
      Scale : Float := 4.0 / 3.0 * GA_Maths.PI * GA_Draw.Point_Size ** 3;
--        Pos   : E3GA.Vector;
   begin
--        E3GA.Set_Coords (Pos, Position.Coordinates (2), Position.Coordinates (3),
--                         Position.Coordinates (4));
--        E3GA_Utilities.Print_Vector ("Draw_Point, Pos", Pos);

      GA_Draw.Draw_Trivector (Render_Program, Model_View_Matrix,
                              Multivectors.Multivector (Position), Colour, 1.0 * Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_Point.");
         raise;
   end Draw_Point;

   --  -------------------------------------------------------------------------

   procedure Draw_C3GA (Render_Program : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Analysis : Multivector_Analyze.MV_Analysis;
                        Colour : GL.Types.Colors.Color;
                        Scale : float := 1.0) is
      use Multivector_Analyze;
   begin
      if Analysis.M_Type.Model_Kind = Conformal_Model then
         case Analysis.M_Type.Blade_Class is
            when Flat_Blade
                  =>  Draw_Flat (Render_Program, Model_View_Matrix, Analysis, Colour, Scale);
            when Free_Blade
                  => null;
            when Round_Blade
                  => null;
            when Tangent_Blade
                  => null;
            when others => null;
         end case;
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA_Draw.Draw_C3GA.");
         raise;
   end Draw_C3GA;

   --  -------------------------------------------------------------------------

end C3GA_Draw;
