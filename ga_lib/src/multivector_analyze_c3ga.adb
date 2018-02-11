
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with E3GA;
with GA_Maths;
with GA_Utilities;
with Multivector_Type;

package body Multivector_Analyze_C3GA is

   procedure Analyze_Free (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                       Flags : Flag_Type; Epsilon : float);

   --  -------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                       Probe : C3GA.Normalized_Point;
                       Flags : Flag_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type;
      use GA_Maths;

      MV_X      : Multivectors.Multivector := MV;
      MV_Info   : Multivector_Type.MV_Type_Record;
      Analysis  : MV_Analysis;

      procedure Classify is
         use Multivectors;
         OP_Nix_Val : constant Float := Norm_E (Outer_Product (C3GA.ni, MV_X));
         IP_Nix_Val : constant Float := Norm_E (Left_Contraction (C3GA.ni, MV_X));
         X2_Val     : constant Float := E3GA.Norm_R2 (MV_X);
         OP_Nix : constant Boolean := Abs (OP_Nix_Val) >= Epsilon;
         IP_Nix  : constant Boolean := Abs (IP_Nix_Val) >= Epsilon;
         X2      : constant Boolean := Abs (X2_Val) >= Epsilon;
      begin
         if not OP_Nix and not IP_Nix then
            Analyze_Free (theAnalysis, MV ,Flags, Epsilon);
         elsif not OP_Nix and IP_Nix then
            null;
         elsif OP_Nix and not IP_Nix then
            null;
         elsif OP_Nix and IP_Nix and not X2 then
            null;
         elsif OP_Nix and IP_Nix and X2 then
            null;
         end if;
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

--  ----------------------------------------------------------------------------

   procedure Analyze_Free (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                           Flags : Flag_Type; Epsilon : float) is
      use Multivectors;
      Grade    : constant GA_Maths.Unsigned_Integer :=
        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Weight   : constant Float := Norm_E (MV);
      Attitude : Multivector := MV;
      No       : constant Vector := Get_Basis_Vector (Blade.C3_no);
   begin
      theAnalysis.M_Points (1) := Get_Basis_Vector (Blade.C3_no);
      theAnalysis.M_Scalors (1) := Weight;
      case Grade is
         when 2 =>  --  F Vector
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (No, MV));
         when 3 =>  --  F Bivector
            declare
               Factor : array (1 .. 5) of C3GA.Dual_Sphere;
               Blade_Grade   : GA_Maths.Unsigned_Integer := 2;
               Blade_Factors : Multivectors.Multivector_List;
               Scales        : GA_Utilities.Scale_Array (1 .. 5);
            begin
               Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scales);
            end;
         when 4 =>  --  F Trivector
            theAnalysis.M_Vectors (1) := Get_Basis_Vector (Blade.E3_e1);
            theAnalysis.M_Vectors (2) := Get_Basis_Vector (Blade.E3_e2);
            theAnalysis.M_Vectors (3) := Get_Basis_Vector (Blade.E3_e3);
         when others => null;
      end case;
   end Analyze_Free;

--  ----------------------------------------------------------------------------

end Multivector_Analyze_C3GA;
