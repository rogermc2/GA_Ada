
with Ada.Text_IO; use Ada.Text_IO;

with E2GA;
with E3GA;
with GA_Maths;
with Multivector_Type_Base;

package body Multivector_Analyze_C3GA is

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : C3GA.Multivector;
                       Probe : C3GA.Normalized_Point;
                       Flags : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type_Base;

      MV_X      : C3GA.Multivector := MV;
      MV_Info   : E2GA.MV_Type;
      Analysis  : MV_Analysis;
   begin
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Conformal_Model;

      if Flags.Dual then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Is Dual.");
         Analysis.M_Flags.Dual := True;
--           MV_X := C3GA.Dual (MV_X);
      end if;

      MV_Info:= C3GA.Init (MV_X, Epsilon);
      Analysis.M_MV_Type := MV_Info;
      Analysis.M_Type.Multivector_Kind := MV_Info.M_Type;
      --  Check for zero blade
      if Analysis.M_MV_Type.M_Zero then
         Put_Line ("Multivector_Analyze_E2GA.Analyze Zero_Blade.");
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.M_Scalors (1) := 0.0;

      elsif Analysis.M_MV_Type.M_Type = Versor_MV then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
         Analysis.M_Type.Blade_Subclass := Even_Versor_Subclass;
         Analysis.M_Vectors (1) := E3GA.e1;

         if MV_X.Coordinates (1) < 0.0 then
            Analysis.M_Vectors (2) := E3GA.e2;
         else
            declare
               use E3GA;
            begin
               Analysis.M_Vectors (2) := -E3GA.e2;
            end;
         end if;

         --  TO BE COMPLETED
      else
         Put_Line ("Multivector_Analyze_C3GA.Analyze Multivector Type.");
      end if;
   end Analyze;

end Multivector_Analyze_C3GA;
