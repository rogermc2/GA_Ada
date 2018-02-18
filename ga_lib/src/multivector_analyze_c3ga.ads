
with C3GA;
with Multivectors;
with Multivector_Analyze; use Multivector_Analyze;

package Multivector_Analyze_C3GA is

    procedure Analyze (Analysis : in out MV_Analysis;
                       MV      : Multivectors.Multivector;
                       Probe   : C3GA.Normalized_Point;
                       Flags   : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon);

end Multivector_Analyze_C3GA;
