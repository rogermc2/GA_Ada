
package body C3GA is

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

   function NI (DP : Dual_Plane) return float is
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

   function NI (NP : Normalized_Point) return float is
   begin
      return NP.NI;
   end NI;

   --  -------------------------------------------------------------------------

   function NO (NP : Normalized_Point) return float is
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

   function Set_Normalized_Point (E1, E2, E3, NI : float := GA_Maths.NI)
                                  return Normalized_Point is
   begin
      return (E1, E2, E3, NI);
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (Point : GA_Maths.Array_3D;
                                  NI : float := GA_Maths.NI)
                                  return Normalized_Point is
   begin
      return (Point (1), Point (1), Point (1), NI);
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

end C3GA;
