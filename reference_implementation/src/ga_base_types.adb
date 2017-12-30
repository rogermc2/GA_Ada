
package body GA_Base_Types is

   function "*" (I1, I2 : NI_T) return NI_T is
      I : NI_T;
   begin
      I.Inf := 0.0;
      return I;
   end "*";

   --  -------------------------------------------------------------------------

   function "*" (I : NI_T; O : NO_T) return float is
   begin
      return -I.Inf * O.Origin;
   end "*";

   --  -------------------------------------------------------------------------

   function "*" (O : NO_T; I : NI_T) return float is
   begin
      return -I.Inf * O.Origin;
   end "*";

   --  -------------------------------------------------------------------------

   function "*" (O1, O2 : NO_T) return NO_T is
      O : NO_T;
   begin
      O.Origin := 0.0;
      return O;
   end "*";

   --  -------------------------------------------------------------------------

   function NI return float is
   begin
      return 1.0;
   end NI;

   --  ------------------------------------------------------------------------

   function NI (N : NI_T) return float is
   begin
      return N.Inf;
   end NI;

   --  ------------------------------------------------------------------------

   function NO return float is
   begin
      return 1.0;
   end NO;

   --  ------------------------------------------------------------------------

   function NO (N : NO_T) return float is
   begin
      return N.Origin;
   end NO;

   --  ------------------------------------------------------------------------

   procedure Set_NI  (N : out NI_T; Inf : float) is
   begin
      N.Inf := Inf;
   end Set_NI;

   --  -------------------------------------------------------------------------

   procedure Set_NO  (N : out NO_T; Origin : float) is
   begin
      N.Origin := Origin;
   end Set_NO;

   --  -------------------------------------------------------------------------

end GA_Base_Types;
