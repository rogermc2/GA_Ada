
with Ada.Text_IO; use Ada.Text_IO;

with E3GA_Utilities;
with Multivectors;
with Multivector_Analyze_E2GA;
with Multivector_Analyze_C3GA;

package body Multivector_Analyze is

   Default_Epsilon_Value : float := 10.0 ** (-5);

   --  --------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, False);
                      Epsilon : float := Default_Epsilon) is
   begin
      Multivector_Analyze_E2GA.Analyze (theAnalysis, MV, Flags, Epsilon);
   end Analyze;

   --  --------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Probe : C3GA.Normalized_Point;
--                        Probe : C3GA.Normalized_Point := C3GA.Probe (Blade.C3_no));
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
      use Multivector_Type;
   begin
--        return A.M_Type.Multivector_Kind
      return A.M_Type.Blade_Class /= Non_Blade;
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

   procedure Print_Analysis (Name : String; Info : MV_Analysis) is
      use Multivector_Type;
      use Multivector_Type_Base;
   begin
      Put_Line (Name);
      Put_Line ("Valid Flag    " & boolean'Image (Info.M_Flags.Valid));
      Put_Line ("Dual Flag     " & boolean'Image (Info.M_Flags.Dual));
      Print_Multivector_Info (Name & " M_MV_Type data", Info.M_MV_Type);
      Put_Line ("Conformal Type     " & Conformal_Type'Image (Info.Conformal_Kind));
      Put_Line ("Epsilon    " & Float'Image (Info.Epsilon));
      Put_Line ("Pseudo_Scalar    " & boolean'Image (Info.Pseudo_Scalar));
      Put_Line ("Versor_Kind    " & Versor_Subclass_Type'Image (Info.Versor_Kind));
      Put_Line ("Pseudo_Scalar    " & boolean'Image (Info.Pseudo_Scalar));
      Put_Line ("Points array length    " & integer'Image (Info.M_Points'Length));
      Put_Line ("Scalars array length    " & integer'Image (Info.M_Scalors'Length));
      Put_Line ("Vectors array length    " & integer'Image (Info.M_Vectors'Length));
      New_Line;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector_Analyze.Print_Analysis.");
         raise;
   end Print_Analysis;

   --  ------------------------------------------------------------------------

   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type is
   begin
      return Blade_Subclass (A);
   end Versor_Subclass;

   --  --------------------------------------------------------------------------

end Multivector_Analyze;
