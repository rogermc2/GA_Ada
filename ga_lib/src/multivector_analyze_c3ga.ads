
with C3GA;
with Multivectors;
with Multivector_Analyze; use Multivector_Analyze;

package Multivector_Analyze_C3GA is

--     procedure Analyze (Analysis : in out MV_Analysis;
   function Analyze (MV      : Multivectors.Multivector;
                     Probe   : Multivectors.Normalized_Point := C3GA.no;
                     Flags   : Flag_Type := (Flag_Invalid, false);
                     Epsilon : float := Default_Epsilon) return MV_Analysis;

   Analyze_C3GA_Exception : Exception;

end Multivector_Analyze_C3GA;
