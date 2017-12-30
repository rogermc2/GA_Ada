
package body GA_Base_Types is

   function "*" (I1, I2 : NI_T) return NI_T is
      I : NI_T;
   begin
      I.Inf := -I1.Inf * I2.Inf;
      return I;
   end "*";

   --  -------------------------------------------------------------------------

   function "*" (O1, O2 : NO_T) return NO_T is
      O : NO_T;
   begin
      O.Origin := -O1.Origin * O2.Origin;
      return O;
   end "*";

   --  -------------------------------------------------------------------------

   function NI (N : NI_T)  return float is
   begin
      return N.Inf;
   end NI;

   --  ------------------------------------------------------------------------

   function NO (N : NO_T)  return float is
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
