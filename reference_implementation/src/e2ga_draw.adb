
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Vertex_Arrays;
with GL.Uniforms;

with Blade;
with E3GA;
with GA_Draw;
with GA_Utilities;
with Multivector_Analyze;

package body E2GA_Draw is
--  Method & flags are dependent on what 'X' represents.
--  They are forwarded to drawVector, drawBivector, drawTrivector.
--
--  Currently, 'method' is some integer in range [0, n), where each
--  integer indicates a different way of drawing the multivector.
--
--  The Palet can be used to specify foreground, background and outline color.
--
--  Uses g_drawState for some extra flags!
--  If this gets annoying, allow DrawState to be passed along
--  as argument (and also integrate 'Palet')

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   MV : in out Multivector.Multivector;
                   Method : GA_Draw.Bivector_Method_Type
                            := GA_Draw.Draw_Bivector_Circle;
                   Colour : GL.Types.Colors.Color := (0.0, 0.5, 0.5, 1.0)) is
      use GA_Draw;
      use GA_Maths;
      use Multivector;
      use Multivector_Analyze;
      A         : MV_Analysis;
      V1        : E2GA.Vector;
      V2        : E2GA.Vector;
      OP        : E2GA.Bivector;
      Normal    : E3GA.Vector;
      Direction : E3GA.Vector;
      Scale     : float := 1.0;
   begin
      Analyze (A, MV);
      GA_Utilities.Print_Analysis ("E2GA Draw Analysis", A);
      Put_Line ("E2GA_Draw Draw 2.");
      if isBlade (A) then
         Put_Line ("E2GA_Draw isBlade.");
         case Blade_Subclass (A) is
            when Vector_Subclass =>
               Put_Line (" E2GA_Draw Vector_Subclass.");
               E3GA.Set_Coords (Direction, 0.0, 0.0, Float (A.M_Scalors (1)));
               Draw_Vector (Render_Program, Model_View_Matrix,
                            A.M_Vectors (1), Direction, Colour, Scale);
            when Bivector_Subclass =>
               Put_Line (" E2GA_Draw Bivector_Subclass.");
               if Get_Draw_Mode = OD_Magnitude then
                  Scale := Float_Functions.Sqrt (Float (Abs (A.M_Scalors (1)))) / Pi;
               end if;

               V1 := E3GA.To_2D (A.M_Vectors (1));
               V2 := E3GA.To_2D (A.M_Vectors (2));
               OP := Multivector.Outer_Product (V1, V2);
               declare
                  use Multivector;
                  use Blade_List_Package;
                  Blades    : constant Blade_List := Get_Blade_List (OP);
                  aBlade    : Blade.Basis_Blade;
                  Curs      : Cursor := Blades.Last;
                  OP_MV     : Multivector.Multivector := OP;
                  Normal_MV : Multivector.Multivector := OP;
               begin
                  Normal_MV := Multivector.Dual (OP_MV);
                  E3GA.Set_Coords (Normal, 0.0, 0.0, Blade.Weight (Element (curs)));
--                                     Normal_MV.Coordinates (Length (Blades)));
                  GA_Draw.Draw_Bivector (Render_Program,
                                         Model_View_Matrix, Normal,
                                         A.M_Vectors (1), A.M_Vectors (2),
                                         Colour, Scale, Method);
               end;
            when Even_Versor_Subclass => null;
            when Unspecified_Subclass => null;
         end case;
      elsif isVersor (A) and then A.M_Scalors (1) > 0.0001 then
         Put_Line ("E2GA_Draw isVersor.");
      end if;
      Put_Line ("E2GA_Draw No true case.");

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw 2.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------

   procedure Draw_Bivector (Render_Program  : GL.Objects.Programs.Program;
                   Translation_Matrix : GL.Types.Singles.Matrix4;
                   BV : E2GA.Bivector; Colour : GL.Types.Colors.Color;
                   Method_Type : GA_Draw.Bivector_Method_Type
                                 := GA_Draw.Draw_Bivector_Circle) is
      use GA_Maths.Float_Functions;
      use GL.Types.Colors;
      Radius   : Float := Sqrt (Abs (E2GA.Get_Coord (BV)));
      Scale    : Float := 20.0;
      Ortho_1  : E3GA.Vector;
      Ortho_2  : E3GA.Vector;
      Normal   : E3GA.Vector := E3GA.e3;  --  Default: (0.0, 0.0, 0.0)
   begin
      E3GA.Set_Coords (Ortho_1, Radius, 0.0, 0.0);
      E3GA.Set_Coords (Ortho_2, 0.0, Radius, 0.0);
      GA_Draw.Draw_Bivector (Render_Program, Translation_Matrix,
                             Normal, Ortho_1, Ortho_2,
                             Colour, Scale, Method_Type);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw 3.");
         raise;
   end Draw_Bivector;

   --  -------------------------------------------------------------------------

   procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   aVector : E2GA.Vector; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      Vec_3D  : E3GA.Vector;
      Tail    : E3GA.Vector;
   begin
      --  MV_Analysis (MV) declares A as a variable of class mvAnalysis
      --  constructed from v1
      E3GA.Set_Coords (Vec_3D, E2GA.Get_Coord_1 (aVector),
                       E2GA.Get_Coord_2 (aVector), 0.0);
      E3GA.Set_Coords (Tail, 0.0, 0.0, 0.0);
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
                           Tail, Vec_3D, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw 1.");
         raise;
   end Draw_Vector;

   --  -------------------------------------------------------------------------

end E2GA_Draw;
