
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with Multivector_Type_Base;

package body C3GA is

   MV_Space_Dimension  : constant Integer := 5;
   MV_Metric_Euclidean : constant Boolean := False;

   --  This array can be used to lookup the number of coordinates for a grade part of a general multivector
   MV_Grade_Size : constant array (1 ..6) of Integer := (1, 5, 10, 10, 5, 1 );

   no_basis : constant Vector := (1.0, 0.0, 0.0, 0.0, -1.0);
   e1_basis : constant Vector := (0.0, 1.0, 0.0, 0.0, 0.0);
   e2_basis : constant Vector := (0.0, 0.0, 1.0, 0.0, 0.0);
   e3_basis : constant Vector := (0.0, 0.0, 0.0, 1.0, 0.0);
   ni_basis : constant Vector := (-1.0, 0.0, 0.0, 0.0, 0.0);

   NI_Value : NI_T := GA_Maths.NI;
   NO_Value : NI_T := 1.0;

   function Init (MV : Multivector; Epsilon : float;
                  Use_Algebra_Metric : Boolean;
               GU_Count : Integer) return E2GA.MV_Type;

   --  -------------------------------------------------------------------------

   function "*" (L : Line; S : Float) return Line is
   begin
      return (L.E1_E2_NI * S, L.E1_E3_NI * S, L.E2_E3_NI * S,
              L.E1_NO_NI * S, L.E2_NO_NI * S, L.E3_NO_NI * S);
   end  "*";

   --  -------------------------------------------------------------------------

   function "*" (S : Float; L : Line) return Line is
   begin
      return L * S;
   end  "*";

   --  -------------------------------------------------------------------------

   function C3GA_Point (V : Vector_E3GA) return Normalized_Point is
      NO       : constant float := 1.0;
      thePoint : Normalized_Point;
      Const    : constant float :=
        NO + 0.5 * Norm_E2(V).Coordinates (1) * NI_Value;
   begin
      thePoint.E1 := V.Coordinates (1) + Const;
      thePoint.E2 := V.Coordinates (2) + Const;
      thePoint.E3 := V.Coordinates (3) + Const;
      thePoint.NI := NI_Value;
      return thePoint;
   end C3GA_Point;

    --  ------------------------------------------------------------------------

   function Coord (S : Scalar) return float is
   begin
      return S.Coordinates (1);
   end Coord;

   --  -------------------------------------------------------------------------

   function Init (MV : Multivector; Epsilon : float := 0.0) return E2GA.MV_Type is
      use Interfaces;
      use GA_Maths;
      use  Multivector_Type_Base;
      MV_Info            : E2GA.MV_Type;
      GU                 : GA_Maths.Grade_Usage := MV.Grade_Use;
      GU_1               : constant GA_Maths.Grade_Usage := 1;
      Count              : array (Unsigned_Integer range 1 .. 2) of Integer := (0, 0);
      Count_Index        : Unsigned_Integer := 0;
      Done               : Boolean := False;
   begin
      --  e2ga.cpp line 1631
      MV_Info.M_Type := Multivector_Object;
      MV_Info.M_Grade_Use := GU;
      --  count grade part usage
      while GU /= 0 loop
         if (GU and GU_1) /= 0 then  --  e2ga.cpp line 1678
            Count (Count_Index + 1 and US_1) := Count (Count_Index + 1 and US_1) + 1;
         end if;
         GU := Unsigned_Integer (Shift_Right (Unsigned_32 (GU), 1));
         MV_Info.M_Grade := Integer (Count_Index);
         Count_Index := Count_Index + 1;
      end loop;

      --  if no grade part in use: zero blade
      if Count (1) = 0 and then Count (2) = 0  then  --  this is a zero blade
         Put_Line ("E2GA.Init 2 Setting zero blade.");
         Set_Type_Base (MV_Info, True, Blade_MV, 0, GU, Even_Parity);
         Done := True;
      else
         --  Base.M_Zero = False by default
         if Count (1) /= 0 and then Count (2) /= 0  then
            --  Base.M_Parity = No_Parity by default
            Done := True;
         else
            if Count (1) = 0 then
               Put_Line ("E2GA.Init 1 Setting even parity.");
               MV_Info.M_Parity := Even_Parity;
            else
               --                 Put_Line ("E2GA.Init 1 Setting odd parity.");
               MV_Info.M_Parity := Odd_Parity;
            end if;
         end if;
      end if;
      if not Done then
         MV_Info := Init (MV, Epsilon, True, Count (1) + Count (2));
      end if;
      return MV_Info;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA.Init 1.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   function Init (MV : Multivector; Epsilon : float;
                  Use_Algebra_Metric : Boolean;
                  GU_Count : Integer) return E2GA.MV_Type is
      MV_Info : E2GA.MV_Type;
   begin
      --  To be completed.
      return MV_Info;
   end Init;

   --  -------------------------------------------------------------------------

   function E1_E2_NI (C : Circle) return float is
   begin
      return C.E1_E2_NI;
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E3_E1_NI (C : Circle) return float is
   begin
      return C.E3_E1_NI;
   end E3_E1_NI;

   --  -------------------------------------------------------------------------

   function E1_E2_E3 (C : Circle) return float is
   begin
      return C.E1_E2_E3;
   end E1_E2_E3;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (C : Circle) return float is
   begin
      return C.E2_E3_NI;
   end E2_E3_NI;

   --  -------------------------------------------------------------------------

   function Get_Coord_1 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (1);
   end Get_Coord_1;

   --  ------------------------------------------------------------------------

   function Get_Coord_2 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (2);
   end Get_Coord_2;

   --  ------------------------------------------------------------------------

   function Get_Coord_3 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (3);
   end Get_Coord_3;

   --  ------------------------------------------------------------------------

   function Get_Coords (V : Vector_E3GA) return GA_Maths.Array_3D is
   begin
      return (V.Coordinates (1), V.Coordinates (2), V.Coordinates (3));
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function NI return NI_T is
   begin
      return NI_Value;
   end NI;

   --  -------------------------------------------------------------------------

   function NO return NO_T is
   begin
      return NO_Value;
   end NO;

   --  -------------------------------------------------------------------------

   function NO_E1_E2 (C : Circle) return float is
   begin
      return C.NO_E1_E2;
   end NO_E1_E2;

   --  -------------------------------------------------------------------------

   function NO_E1_E3 (C : Circle) return float is
   begin
      return C.NO_E1_E3;
   end NO_E1_E3;

   --  -------------------------------------------------------------------------

   function NO_E1_NI (C : Circle) return float is
   begin
      return C.NO_E1_NI;
   end NO_E1_NI;

   --  -------------------------------------------------------------------------

   function NO_E2_E3 (C : Circle) return float is
   begin
      return C.NO_E2_E3;
   end NO_E2_E3;

   --  -------------------------------------------------------------------------

   function NO_E2_NI (C : Circle) return float is
   begin
      return C.NO_E2_NI;
   end NO_E2_NI;

   --  -------------------------------------------------------------------------

   function NO_E3_NI (C : Circle) return float is
   begin
      return C.NO_E3_NI;
   end NO_E3_NI;

   --  -------------------------------------------------------------------------

   function E1 (DP : Dual_Plane) return float is
   begin
      return DP.E1;
   end E1;

   --  -------------------------------------------------------------------------

   function E2 (DP : Dual_Plane) return float is
   begin
      return DP.E2;
   end E2;

   --  -------------------------------------------------------------------------

   function E3 (DP : Dual_Plane) return float is
   begin
      return DP.E3;
   end E3;

   --  -------------------------------------------------------------------------

   function NI (DP : Dual_Plane) return NI_T is
   begin
      return DP.NI;
   end NI;

   --  -------------------------------------------------------------------------

   function E1_E2_NI (L : Line) return float is
   begin
      return L.E1_E2_NI;
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E1_E3_NI (L : Line) return float is
   begin
      return L.E1_E3_NI;
   end E1_E3_NI;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (L : Line) return float is
   begin
      return L.E2_E3_NI;
   end E2_E3_NI;

   --  -------------------------------------------------------------------------

   function E1_NO_NI (L : Line) return float is
   begin
      return L.E1_NO_NI;
   end E1_NO_NI;

   --  -------------------------------------------------------------------------

   function E2_NO_NI (L : Line) return float is
   begin
      return L.E2_NO_NI;
   end E2_NO_NI;

   --  -------------------------------------------------------------------------

   function E3_NO_NI (L : Line) return float is
   begin
      return L.E3_NO_NI;
   end E3_NO_NI;

   --  -------------------------------------------------------------------------

   function E1 (NP : Normalized_Point) return float is
   begin
      return NP.E1;
   end E1;

   --  -------------------------------------------------------------------------

   function E2 (NP : Normalized_Point) return float is
   begin
      return NP.E2;
   end E2;

   --  -------------------------------------------------------------------------

   function E3 (NP : Normalized_Point) return float is
   begin
      return NP.E3;
   end E3;

   --  -------------------------------------------------------------------------

   function NI (NP : Normalized_Point) return NI_T is
   begin
      return NP.NI;
   end NI;

   --  -------------------------------------------------------------------------

   function NO (NP : Normalized_Point) return NO_T is
   begin
      return 1.0;
   end NO;

   --  -------------------------------------------------------------------------

   function E1_E2_E3_NI (S : Sphere) return float is
   begin
      return S.E1_E2_E3_NI;
   end E1_E2_E3_NI;

   --  -------------------------------------------------------------------------

   function E1_E2_NO_NI (S : Sphere) return float is
   begin
      return S.E1_E2_NO_NI;
   end E1_E2_NO_NI;

   --  -------------------------------------------------------------------------

   function E1_E3_NO_NI (S : Sphere) return float is
   begin
      return S.E1_E3_NO_NI;
   end E1_E3_NO_NI;

   --  -------------------------------------------------------------------------

   function E2_E3_NO_NI (S : Sphere) return float is
   begin
      return S.E2_E3_NO_NI;
   end E2_E3_NO_NI;

   --  -------------------------------------------------------------------------

   function E1_E2_E3_NO (S : Sphere) return float is
   begin
      return S.E1_E2_E3_NO;
   end E1_E2_E3_NO;

   --  -------------------------------------------------------------------------

   function Norm_E2 (V : Vector_E3GA) return Scalar is
      theNorm : Scalar;
   begin
      theNorm.Coordinates (1) := V.Coordinates (1) * V.Coordinates (1) +
        V.Coordinates (2) * V.Coordinates (2) +
        V.Coordinates (3) * V.Coordinates (3);
      return theNorm;
   end Norm_E2;

   --  -------------------------------------------------------------------------
   procedure Set_Coords (V : out Vector_E3GA; C1, C2, C3 : float) is
   begin
      V.Coordinates (1) := C1;
      V.Coordinates (2) := C2;
      V.Coordinates (3) := C3;
   end Set_Coords;

   --  -------------------------------------------------------------------------

   function Set_Coords (C1, C2, C3 : float) return Vector_E3GA is
      Vec : Vector_E3GA;
   begin
      Vec.Coordinates :=  (C1, C2, C3);
     return Vec;
   end Set_Coords;

   --  -------------------------------------------------------------------------

   procedure Set_Coords (P : out Point; Origin, C1, C2, C3, Inf : float) is
   begin
      P.NO := Origin;
      P.E1 := C1;
      P.E2 := C2;
      P.E3 := C3;
      P.NI := Inf;
   end Set_Coords;

   --  -------------------------------------------------------------------------

   procedure Set_Multivector (MV : out Multivector; Point : Normalized_Point) is
   begin
      MV.Coordinates (1) := Point.E1;
      MV.Coordinates (2) := Point.E2;
      MV.Coordinates (3) := Point.E3;
      MV.Coordinates (4) := Point.NI;
   end Set_Multivector;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (E1, E2, E3 : float; Inf : float := NI)
                               return Normalized_Point is
   begin
      return (E1, E2, E3, Inf);
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (Point : GA_Maths.Array_3D;
                                  Inf : float := NI)
                               return Normalized_Point is
   begin
      return (Point (1), Point (2), Point (3), Inf);
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function Unit_R (L : Line) return Line is
      use GA_Maths.Float_Functions;
      R_Sq : constant float := -(L.E1_NO_NI * L.E1_NO_NI +
                                   L.E2_NO_NI * L.E2_NO_NI + L.E3_NO_NI * L.E3_NO_NI);
      Inv  : constant float := 1.0 / Sqrt (Abs (R_Sq));
   begin
      return L * Inv;
   end Unit_R;

   --  -------------------------------------------------------------------------

   function US_Normalized_Point (N : Normalized_Point) return Normalized_Point is
      thePoint : Normalized_Point := N;
   begin
      thePoint.NI := 0.0;
      return thePoint;
   end US_Normalized_Point;

   --  -------------------------------------------------------------------------

   function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point is
   begin
      return (E1, E2, E3, 0.0);
   end US_Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function US_Set_Normalized_Point (Point : Vector_E3GA) return Normalized_Point is
   begin
      return (Point.Coordinates (1), Point.Coordinates (2), Point.Coordinates (3), 0.0);
   end US_Set_Normalized_Point;

   --  -------------------------------------------------------------------------

end C3GA;
