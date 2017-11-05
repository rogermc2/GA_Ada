
with E2GA;
with Multivector_Analyze; use Multivector_Analyze;

package Multivector_Analyze_E2GA is

    procedure Analyze (theAnalysis : in out MV_Analysis;
                       MV      : in out E2GA.Multivector;
                       Flags   : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon);

end Multivector_Analyze_E2GA;
