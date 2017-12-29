
package GA_Base_Types is

   type NI_T is private;
   type NO_T is private;

   function "*" (I1, I2 : NI_T) return NI_T;
   function "*" (O1, O2 : NO_T) return NO_T;

private
   type NI_T is record
      Inf : float := 1.0;
   end record;

   type NO_T is record
      Origin : float := 0.0;
   end record;

end GA_Base_Types;
