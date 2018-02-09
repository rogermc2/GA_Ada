
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with E3GA;
with GA_Maths;
with Multivector_Type;

package body Multivector_Analyze_C3GA is

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivector.Multivector;
                       Probe : C3GA.Normalized_Point;
                       Flags : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type;
      use GA_Maths;

      MV_X      : Multivector.Multivector := MV;
      MV_Info   : Multivector_Type.MV_Type_Record;
      Analysis  : MV_Analysis;

      procedure Classify is
         use Multivector;
         OP_Nix : constant Float := Norm_E (Outer_Product (C3GA.ni, MV_X));
         IP_Nix : constant Float := Norm_E (Left_Contraction (C3GA.ni, MV_X));
         X2     : constant Float := E3GA.Norm_R2 (MV_X);
      begin
         null;
      end Classify;

   begin
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Conformal_Model;

      if Flags.Dual then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Is Dual.");
         Analysis.M_Flags.Dual := True;
--           MV_X := C3GA.Dual (MV_X);
      end if;

--        MV_Info := C3GA.Init (MV_X, Epsilon);
      MV_Info := Init (MV_X);
      Analysis.M_MV_Type := MV_Info;
      Print_Multivector_Info ("Multivector_Analyze_C3GA.Analyze MV_Info", MV_Info);
     New_Line;
      --        Analysis.M_Type.Multivector_Kind := MV_Info.M_Type;

      --  Check for zero blade
--        if Analysis.M_MV_Type.M_Zero then
      if Zero (Analysis.M_MV_Type) then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Zero_Blade.");
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.M_Scalors (1) := 0.0;

      elsif MV_Kind (Analysis.M_MV_Type) = Versor_MV then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
         Analysis.M_Type.Blade_Subclass := Even_Versor_Subclass;
         Analysis.M_Vectors (1) := E3GA.e1;

      elsif Grade_Use (Analysis.M_MV_Type) = 1 then  --  Grade 0
         Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 1.");
         Analysis.M_Type.Blade_Class := Scalar_Blade;
         Analysis.M_Type.M_Grade := 1;
--           Analysis.M_Scalors (1) := MV_X.Coordinates (1);

      elsif Grade_Use (Analysis.M_MV_Type) = 6 then  --  Grade 5
         Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 6.");
         Analysis.M_Type.Blade_Class := Scalar_Blade;
         Analysis.M_Type.M_Grade := 6;
         Analysis.M_Scalors (1) := C3GA.NO_E1_E2_E3_NI (MV);
      else
         Classify;
         --  TO BE COMPLETED
         Put_Line ("Multivector_Analyze_C3GA.Analyze Multivector Type.");
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze.");
         raise;
   end Analyze;

end Multivector_Analyze_C3GA;
