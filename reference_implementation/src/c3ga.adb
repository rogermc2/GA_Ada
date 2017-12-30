
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with Multivector_Type_Base;

package body C3GA is

   MV_Space_Dimension  : constant Integer := 5;
   MV_Metric_Euclidean : constant Boolean := False;

   --  This array can be used to lookup the number of coordinates for a grade part of a general multivector
   MV_Grade_Size : constant array (1 ..6) of Integer := (1, 5, 10, 10, 5, 1 );

   no_basis : constant Vector := (0.0, 0.0, 0.0, 0.0, 1.0);
   e1_basis : constant Vector := (0.0, 1.0, 0.0, 0.0, 0.0);
   e2_basis : constant Vector := (0.0, 0.0, 1.0, 0.0, 0.0);
   e3_basis : constant Vector := (0.0, 0.0, 0.0, 1.0, 0.0);
   ni_basis : constant Vector := (1.0, 0.0, 0.0, 0.0, 0.0);

   function Init (MV : Multivector; Epsilon : float;
                  Use_Algebra_Metric : Boolean;
               GU_Count : Integer) return MV_Type;

   --  -------------------------------------------------------------------------

   function "+" (V1 : Vector; V2 : Vector) return Vector is
   begin
      return (V1 (1) + V2 (1), V1 (2) + V2 (2), V1 (3) + V2 (3),
              V1 (4) + V2 (4), V1 (5) + V2 (5));
   end  "+";

   --  -------------------------------------------------------------------------

   function "*" (F : Float; V : Vector) return Vector is
   begin
      return (F * V (1), F * V (2), F * V (3), F * V (4), F * V (5));
   end  "*";

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
      thePoint : Normalized_Point;
      Inf      : GA_Base_Types.NI_T;
      Const    : constant float :=
        GA_Base_Types.NO + 0.5 * Norm_E2(V).Coordinates (1) * GA_Base_Types.NI;
   begin
      thePoint.E1 := V.Coordinates (1) + Const;
      thePoint.E2 := V.Coordinates (2) + Const;
      thePoint.E3 := V.Coordinates (3) + Const;
      thePoint.Inf := Inf;
      return thePoint;
   end C3GA_Point;

    --  ------------------------------------------------------------------------

   function Coord (S : Scalar) return float is
   begin
      return S.Coordinates (1);
   end Coord;

   --  -------------------------------------------------------------------------

   function Init (MV : Multivector; Epsilon : float := 0.0) return MV_Type is
      use Interfaces;
      use GA_Maths;
      use  Multivector_Type_Base;
      MV_Info            : MV_Type;
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
                  GU_Count : Integer) return MV_Type is
      MV_Info : MV_Type;
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

   function Get_Coords (NP : Normalized_Point) return Vector is
   begin
      return (1.0, NP.E1, NP.E2, NP.E3, GA_Base_Types.NI (NP.Inf));
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coords (NP : Normalized_Point)
                        return GA_Maths.Coords_Continuous_Array is
      Coords : GA_Maths.Coords_Continuous_Array (1 .. 4)
        :=  (NP.E1, NP.E2, NP.E3, GA_Base_Types.NI (NP.Inf));
   begin
      return Coords;
   end Get_Coords;

   --  ------------------------------------------------------------------------

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

   function NI (DP : Dual_Plane) return GA_Base_Types.NI_T is
   begin
      return DP.Inf;
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

   function NI (NP : Normalized_Point) return Float is
   begin
      return GA_Base_Types.NI (NP.Inf);
   end NI;

   --  -------------------------------------------------------------------------

   function NO (NP : Normalized_Point) return Float is
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
      use GA_Base_Types;
   begin
      Set_NO (P.Origin, Origin);
      P.E1 := C1;
      P.E2 := C2;
      P.E3 := C3;
      Set_NI (P.Inf, Inf);
   end Set_Coords;

   --  -------------------------------------------------------------------------

   procedure Set_Multivector (MV : out Multivector; NP : Normalized_Point) is
      New_MV : Multivector (2);
   begin
      New_MV.Coordinates (1) := 1.0;
      New_MV.Coordinates (2) := NP.E1;
      New_MV.Coordinates (3) := NP.E2;
      New_MV.Coordinates (4) := NP.E3;
      New_MV.Coordinates (5) := GA_Base_Types.NI (NP.Inf);
      MV := New_MV;
   end Set_Multivector;

   --  -------------------------------------------------------------------------

   procedure Set_Multivector (MV : out Multivector; N : GA_Base_Types.NI_T) is
      New_MV : Multivector (2);
   begin
      New_MV.Coordinates (1) := 0.0;
      New_MV.Coordinates (2) := 0.0;
      New_MV.Coordinates (3) := 0.0;
      New_MV.Coordinates (4) := 0.0;
      New_MV.Coordinates (5) := GA_Base_Types.NI (N);
      MV := New_MV;
   end Set_Multivector;

   --  -------------------------------------------------------------------------

   procedure Set_Multivector (MV : out Multivector; N : GA_Base_Types.NO_T) is
      New_MV : Multivector (2);
   begin
      New_MV.Coordinates (1) := GA_Base_Types.NO (N);
      New_MV.Coordinates (2) := 0.0;
      New_MV.Coordinates (3) := 0.0;
      New_MV.Coordinates (4) := 0.0;
      New_MV.Coordinates (5) := 0.0;
      MV := New_MV;
   end Set_Multivector;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (E1, E2, E3 : float; Inf : float := 1.0)
                                  return Normalized_Point is
      Point : Normalized_Point;
   begin
      Point.E1 := E1;
      Point.E2 := E2;
      Point.E3 := E3;
      GA_Base_Types.Set_NI (Point.Inf, Inf);
      return Point;
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (Point : GA_Maths.Array_3D;
                                  Inf : float := 1.0)
                               return Normalized_Point is
      NP : Normalized_Point;
   begin
      NP.E1 := Point (1);
      NP.E2 := Point (2);
      NP.E3 := Point (3);
      GA_Base_Types.Set_NI (NP.Inf, Inf);
      return NP;
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
      GA_Base_Types.Set_NI (thePoint.Inf, 0.0);
      return thePoint;
   end US_Normalized_Point;

   --  -------------------------------------------------------------------------

   function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point is
      NP : Normalized_Point;
   begin
      NP.E1 := E1;
      NP.E2 := E2;
      NP.E3 := E3;
      GA_Base_Types.Set_NI (NP.Inf, 0.0);
      return NP;
   end US_Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function US_Set_Normalized_Point (Point : Vector_E3GA) return Normalized_Point is
      NP : Normalized_Point;
   begin
      NP.E1 := Point.Coordinates (1);
      NP.E2 := Point.Coordinates (2);
      NP.E3 := Point.Coordinates (3);
      GA_Base_Types.Set_NI (NP.Inf, 0.0);
      return NP;
   end US_Set_Normalized_Point;

   --  -------------------------------------------------------------------------

end C3GA;
