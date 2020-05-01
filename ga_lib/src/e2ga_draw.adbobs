
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Vertex_Arrays;
with GL.Uniforms;

with Blade;
with Blade_Types;
with E2GA;
with E3GA;
with GA_Draw;
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
                   MV     : in out Multivectors.Multivector;
                   Method : GA_Draw.Bivector_Method_Type
                            := GA_Draw.Draw_Bivector_Circle;
                   Colour : GL.Types.Colors.Color := (0.0, 0.5, 0.5, 1.0)) is
      use GA_Draw;
      use GA_Maths;
      use Multivectors;
      use Multivector_Analyze;
      A         : MV_Analysis;
      V1        : Vector;
      V2        : Vector;
      OP        : Multivector;
      Normal    : Vector;
      Direction : Vector;
      Scale     : float := 1.0;
   begin
      Analyze (A, MV);
      Print_Analysis ("E2GA Draw Analysis", A);
      if isBlade (A) then
         Put_Line ("E2GA_Draw isBlade.");
         case Blade_Subclass (A) is
            when Vector_Subclass =>
               Put_Line (" E2GA_Draw Vector_Subclass.");
--                 E3GA.Set_Coords (Direction, 0.0, 0.0, Float (A.M_Scalors (1)));
--                 Direction := New_Vector (0.0, 0.0, Float (A.M_Scalors (1)));
               Add_Blade (Direction, Blade_Types.E3_e3, Float (A.M_Scalors (1)));
               Draw_Vector (Render_Program, Model_View_Matrix,
                            A.M_Vectors (1), Direction, Colour, Scale);
            when Bivector_Subclass =>
               Put_Line (" E2GA_Draw Bivector_Subclass.");
               if Get_Draw_Mode = OD_Magnitude then
                  Scale := Float_Functions.Sqrt (Float (Abs (A.M_Scalors (1)))) / Pi;
               end if;

--                 V1 := E3GA.To_2D (A.M_Vectors (1));
--                 V2 := E3GA.To_2D (A.M_Vectors (2));
               V1 := A.M_Vectors (1);
               V2 := A.M_Vectors (2);
               OP := Outer_Product (V1, V2);
               declare
                  use Blade_List_Package;
                  Blades    : constant Blade_List := Get_Blade_List (OP);
                  aBlade    : Blade.Basis_Blade;
                  Curs      : Cursor := Blades.Last;
                  OP_MV     : Multivector := OP;
                  Normal_MV : Multivector := OP;
               begin
                  Normal_MV := Dual (OP_MV);
                  Add_Blade (Normal, Blade_Types.E3_e3, Blade.Weight (Element (curs)));
--                    E3GA.Set_Coords (Normal, 0.0, 0.0, Blade.Weight (Element (curs)));
--                                     Normal_MV.Coordinates (Length (Blades)));
                  GA_Draw.Draw_Bivector (Render_Program,
                                         Model_View_Matrix, Normal,
                                         A.M_Vectors (1), A.M_Vectors (2),
                                         Colour, Scale, Method);
               end;
            when others => null;
         end case;

      elsif isVersor (A) and then A.M_Scalors (1) > 0.0001 then
         Put_Line ("E2GA_Draw isVersor.");
      end if;
      Put_Line ("E2GA_Draw No true case.");

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------

   procedure Draw_Bivector (Render_Program  : GL.Objects.Programs.Program;
                   Translation_Matrix : GL.Types.Singles.Matrix4;
                   BV : Multivectors.Bivector; Colour : GL.Types.Colors.Color;
                   Method_Type : GA_Draw.Bivector_Method_Type
                                 := GA_Draw.Draw_Bivector_Circle) is
      use GA_Maths.Float_Functions;
      use GL.Types.Colors;
      use Multivectors;
      Radius   : constant Float := Sqrt (Abs (E2GA.Get_Coord (BV)));
      Scale    : constant Float := 20.0;
      Ortho_1  : constant Vector := New_Vector (Radius, 0.0, 0.0);
      Ortho_2  : constant Vector := New_Vector (0.0, Radius, 0.0);
      Normal   : Vector := New_Vector (0.0, 0.0);
   begin
      Add_Blade (Normal, Blade.New_Basis_Blade (Blade_Types.E3_e3));
      GA_Draw.Draw_Bivector (Render_Program, Translation_Matrix,
                             Normal, Ortho_1, Ortho_2,
                             Colour, Scale, Method_Type);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw_Bivector.");
         raise;
   end Draw_Bivector;

   --  -------------------------------------------------------------------------

   procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix : GL.Types.Singles.Matrix4;
                   Direction : Multivectors.Vector; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      use Multivectors;
      Vec_3D  : Vector;
      Tail    : constant Vector := New_Vector (0.0, 0.0, 0.0);
   begin
      if E2GA.e1 (Direction) /= 0.0 then
         Add_Blade (Vec_3D, Blade_Types.E3_e1, E2GA.e1 (Direction));
      end if;
      if E2GA.e2 (Direction) /= 0.0 then
         Add_Blade (Vec_3D, Blade_Types.E3_e2, E2GA.e2 (Direction));
      end if;
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix,
                           Tail, Vec_3D, Colour, Scale);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw_Vector.");
         raise;
   end Draw_Vector;

   --  -------------------------------------------------------------------------

end E2GA_Draw;
