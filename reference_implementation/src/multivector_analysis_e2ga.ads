
with E2GA;
with Multivector_Analysis; use Multivector_Analysis;

package Multivector_Analysis_E2GA is

    procedure Analyze (MV : in out E2GA.Multivector;
                       Flags : Flags_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon);

end Multivector_Analysis_E2GA;
