
package body C3GA is

   function E1_E2_NI (C : Circle) return float is
   begin
      return C.Coordinates (3);
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E3_E1_NI (C : Circle) return float is
   begin
      return C.Coordinates (2);
   end E3_E1_NI;

   --  -------------------------------------------------------------------------

   function E1_E2_E3 (C : Circle) return float is
   begin
      return C.Coordinates (10);
   end E1_E2_E3;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (C : Circle) return float is
   begin
      return C.Coordinates (1);
   end E2_E3_NI;

   --  -------------------------------------------------------------------------

   function NO_E1_E2 (C : Circle) return float is
   begin
      return C.Coordinates (9);
   end NO_E1_E2;

   --  -------------------------------------------------------------------------

   function NO_E1_E3 (C : Circle) return float is
   begin
      return C.Coordinates (8);
   end NO_E1_E3;

   --  -------------------------------------------------------------------------

   function NO_E1_NI (C : Circle) return float is
   begin
      return C.Coordinates (5);
   end NO_E1_NI;

   --  -------------------------------------------------------------------------

   function NO_E2_E3 (C : Circle) return float is
   begin
      return C.Coordinates (7);
   end NO_E2_E3;

   --  -------------------------------------------------------------------------

   function NO_E2_NI (C : Circle) return float is
   begin
      return C.Coordinates (6);
   end NO_E2_NI;

   --  -------------------------------------------------------------------------

   function NO_E3_NI (C : Circle) return float is
   begin
      return C.Coordinates (4);
   end NO_E3_NI;

   --  -------------------------------------------------------------------------

   function E1 (DP : Dual_Plane) return float is
   begin
      return DP.Coordinates (1);
   end E1;

   --  -------------------------------------------------------------------------

   function E2 (DP : Dual_Plane) return float is
   begin
      return DP.Coordinates (2);
   end E2;

   --  -------------------------------------------------------------------------

   function E3 (DP : Dual_Plane) return float is
   begin
      return DP.Coordinates (3);
   end E3;

   --  -------------------------------------------------------------------------

   function NI (DP : Dual_Plane) return float is
   begin
      return DP.Coordinates (4);
   end NI;

   --  -------------------------------------------------------------------------

end C3GA;
