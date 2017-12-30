
with C3GA;
with Multivector_Analyze; use Multivector_Analyze;

package Multivector_Analyze_C3GA is

    procedure Analyze (theAnalysis : in out MV_Analysis;
                       MV      : C3GA.Multivector;
                       Probe : C3GA.Normalized_Point;
                       Flags   : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon);

end Multivector_Analyze_C3GA;
