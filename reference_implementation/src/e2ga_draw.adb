
with Ada.Text_IO; use Ada.Text_IO;

with E3GA;
with E3GA_Utilities;
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

   --  Draw Vector
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
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
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                           Tail, Vec_3D, Colour, Scale);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA_Draw.Draw 1.");
         raise;
   end Draw;

   --  -------------------------------------------------------------------------

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix,
                   Projection_Matrix : GL.Types.Singles.Matrix4;
                   MV : in out E2GA.Multivector;
                   Method : GA_Draw.Bivector_Method_Type
                            := GA_Draw.Draw_Bivector_Circle;
                   Colour : GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0)) is
      use GA_Draw;
      use GA_Maths;
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
      Put_Line ("E2GA_Draw Draw 2.");
      if isBlade (A) then
         Put_Line ("E2GA_Draw isBlade.");
         case Blade_Subclass (A) is
            when Vector_Subclass =>
               E3GA.Set_Coords (Direction, 0.0, 0.0, A.M_Scalors (1));
               Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                            A.M_Vectors (1), Direction, Colour, Scale);
            when Bivector_Subclass =>
               Put_Line (" E2GA_Draw Bivector_Subclass.");
               if Get_Draw_Mode = OD_Magnitude then
                  Scale := Float_Functions.Sqrt (Abs (A.M_Scalors (1))) / Pi;
               end if;

               V1 := E3GA.To_2D (A.M_Vectors (1));
               V2 := E3GA.To_2D (A.M_Vectors (2));
               OP := E2GA.Outer_Product (V1, V2);
               declare
                  OP_MV     : E2GA.Multivector := E2GA.Set_Multivector (OP);
                  Normal_MV : E2GA.Multivector := E2GA.Set_Multivector (OP);
               begin
                  Normal_MV := E2GA.Dual (OP_MV);
                  E3GA.Set_Coords (Normal, 0.0, 0.0, Normal_MV.Coordinates (1));
                  GA_Draw.Draw_Bivector (Render_Program,
                                         Model_View_Matrix, Projection_Matrix,
                                         Normal, A.M_Vectors (1), A.M_Vectors (2),
                                         Scale, Method, Colour);
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

   --  Draw Bivector
   procedure Draw (Render_Program  : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   BV : E2GA.Bivector;  Method_Type : GA_Draw.Bivector_Method_Type
                                                      := GA_Draw.Draw_Bivector_Circle;
                   Colour  : GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0)) is
      MV : E2GA.Multivector := E2GA.Set_Multivector (BV);
   begin
      Draw (Render_Program , Model_View_Matrix, Projection_Matrix,
             MV, Method_Type, Colour);
   end Draw;

   --  -------------------------------------------------------------------------

end E2GA_Draw;
