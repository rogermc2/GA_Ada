
with Ada.Text_IO; use Ada.Text_IO;

with E3GA;
with GA_Draw;
with Multivector_Analysis;

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
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   aVector : E2GA.Vector; Colour : GL.Types.Colors.Color;
                   Scale : float := 1.0) is
      Vec_3D  : E3GA.Vector_3D;
      Tail    : E3GA.Vector_3D;
   begin
      --  MV_Analysis (MV) declares A as a variable of class mvAnalysis
      --  constructed from v1
      E3GA.Set_Coords (Vec_3D, aVector.Coordinates (1),
                                   aVector.Coordinates (2), 0.0);
      E3GA.Set_Coords (Tail, 0.0, 0.0, 0.0);
      GA_Draw.Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                           Tail, Vec_3D, Colour, Scale);
   end Draw;

   --  ----------------------------------------------------------------------------

   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                   MV : in out E2GA.Multivector;
                   Method : GA_Draw.Bivector_Method_Type
                              := GA_Draw.Draw_Bivector_Circle;
                   Colour : GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0)) is
      use GA_Draw;
      use GA_Maths;
      use Multivector_Analysis;
      A         : constant MV_Analysis := Analyze (MV);
      AM_V1     : constant E3GA.Vector_3D := A.M_Vectors (1);
      AM_V2     : constant E3GA.Vector_3D := A.M_Vectors (2);
      V1        : E2GA.Vector_2D;
      OP        : E2GA.Bivector;
      Normal    : E3GA.Vector_3D;
      Normal_BV : E2GA.Bivector;
      Direction : E3GA.Vector_3D;
      Scale     : float := 1.0;
   begin
      E2GA.Set_Coords (V1, E3GA.Get_Coord_1 (AM_V1), E3GA.Get_Coord_2 (AM_V1));
      if isBlade (A) then
         case Blade_Subclass (A) is
            when Vector_Subclass =>
               E3GA.Set_Coords (Direction, 0.0, 0.0, A.M_Scalors (1));
               Draw_Vector (Render_Program, Model_View_Matrix, Projection_Matrix,
                     AM_V1, Direction, Colour, Scale);
            when Bivector_Subclass =>
               if Get_Draw_Mode = OD_Magnitude then
                  Scale := Float_Functions.Sqrt (Abs (A.M_Scalors (1))) / Pi;
               end if;
               OP := E2GA.Outer_Product (V1, V1);
               Normal_BV := E2GA.Dual (OP);
               E3GA.Set_Coords (Normal, 0.0, 0.0, Normal_BV.Coordinates (1));
               GA_Draw.Draw_Bivector (Render_Program,
                    Model_View_Matrix, Projection_Matrix, Normal,
                    AM_V1, AM_V2, Scale, Method, Colour);
            When Even_Versor_Subclass => null;
         end case;
      elsif isVersor (A) then
         null;
      end if;
   end Draw;

   --  ----------------------------------------------------------------------------

   --      procedure Draw (Render_Program : GL.Objects.Programs.Program;
   --                      Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
   --                      BV     : E2GA.Bivector;
   --                      Method_Type : GA_Draw.Bivector_Method_Type
   --                                 := GA_Draw.Draw_Bivector_Circle;
   --                     Colour : GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0)) is
   --        use GA_Draw;
   --        use GA_Maths;
   --        Scale : float;
   --      begin
   --        if Get_Draw_Mode = OD_Magnitude then
   --           Scale := Float_Functions.Sqrt (Abs (float (BV.Coordinates (1)))) / Pi;
   --        else
   --           Scale := 1.0;
   --        end if;
   --        Draw_Bivector (Render_Program, Model_View_Matrix, Projection_Matrix,
   --                       BV, Scale);
   --      end Draw;

   --  --------------------------------------------------------------------------------

end E2GA_Draw;
