
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

end GA_Base_Types;
