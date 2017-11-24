
package body C3GA is

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

end C3GA;
