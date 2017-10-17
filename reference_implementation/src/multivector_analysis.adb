
package body Multivector_Analysis is

   --  --------------------------------------------------------------------------

   function Analyze (MV      : in out E2GA.Multivector; Flags : Flag_Type;
                     Epsilon : float := Default_Epsilon) return MV_Analysis is
      theAnalysis : MV_Analysis;
   begin
      theAnalysis.Flag.Dual := Flags.Dual;
      if Flags.Dual then
         MV := E2GA.Dual(MV);
      end if;
      return theAnalysis;
   end Analyze;

   --  --------------------------------------------------------------------------

   function Default_Epsilon return float is
   begin
      return Default_Epsilon_Value;
   end Default_Epsilon;

   function Num_Points return integer is
   begin
      return Number_Of_Points;
   end Num_Points;

   --  --------------------------------------------------------------------------

   function Num_Vectors return integer is
   begin
      return Number_Of_Vectors;
   end Num_Vectors;

   --  --------------------------------------------------------------------------

   function Num_Scalars return integer is
   begin
      return Number_Of_Scalars;
   end Num_Scalars;

   --  --------------------------------------------------------------------------

end Multivector_Analysis;
