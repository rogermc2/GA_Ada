
with Ada.Text_IO; use Ada.Text_IO;

with E3GA_Utilities;
with Multivector;
with Multivector_Analyze_E2GA;
with Multivector_Analyze_C3GA;

package body Multivector_Analyze is

   Default_Epsilon_Value : float := 10.0 ** (-5);

   --  --------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivector.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, False);
                      Epsilon : float := Default_Epsilon) is
   begin
      Multivector_Analyze_E2GA.Analyze (theAnalysis, MV, Flags, Epsilon);
   end Analyze;

   --  --------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivector.Multivector;
                      Probe : C3GA.Normalized_Point;
                      Flags : Flag_Type := (Flag_Invalid, False);
                      Epsilon : float := Default_Epsilon) is
   begin
      Multivector_Analyze_C3GA.Analyze (theAnalysis, MV, Probe, Flags, Epsilon);
   end Analyze;

   --  -------------------------------------------------------------------------

   function Blade_Subclass (A : MV_Analysis) return Blade_Subclass_Type is
   begin
      return  A.M_Type.Blade_Subclass;
   end Blade_Subclass;

   --  --------------------------------------------------------------------------

   function Default_Epsilon return float is
   begin
      return Default_Epsilon_Value;
   end Default_Epsilon;

   --  --------------------------------------------------------------------------

   function isValid (A : MV_Analysis) return Boolean is
   begin
      return A.M_Flags.Valid = Flag_Valid;
   end isValid;

   --  --------------------------------------------------------------------------

   function isDual (A : MV_Analysis) return Boolean is
   begin
      return A.M_Flags.Dual;
   end isDual;

   --  --------------------------------------------------------------------------

   function isBlade (A : MV_Analysis) return Boolean is
      use Multivector_Type_Base;
   begin
--        return A.M_Type.Multivector_Kind
      return A.M_MV_Type.M_Type = Multivector_Type_Base.Blade_MV;
   end isBlade;

   --  --------------------------------------------------------------------------

   function isVersor (A : MV_Analysis) return Boolean is
   begin
      return A.Versor_Kind /= Invalid_Versor;
   end isVersor;

   --  --------------------------------------------------------------------------

   function isNull (A : MV_Analysis) return Boolean is
      --  {return ((type() == BLADE) && (bladeClass() == ZERO));}
   begin
      return isBlade (A) and A.M_Type.Blade_Class = Zero_Blade;
   end isNull;

   --  --------------------------------------------------------------------------

   function isZero (A : MV_Analysis)  return Boolean is
      --  {return ((type() == BLADE) && (bladeClass() == ZERO));}
   begin
      return isBlade (A) and A.M_Type.Blade_Class = Zero_Blade;
   end isZero;

   --  --------------------------------------------------------------------------

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

   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type is
   begin
      return Blade_Subclass (A);
   end Versor_Subclass;

   --  --------------------------------------------------------------------------

end Multivector_Analyze;
