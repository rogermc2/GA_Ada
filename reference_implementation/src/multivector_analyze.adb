
with Ada.Text_IO; use Ada.Text_IO;

package body Multivector_Analyze is

   --  --------------------------------------------------------------------------

   function Analyze (MV      : in out E2GA.Multivector;
                     Flags : Flag_Type := (Flag_Invalid, False);
                     Epsilon : float := Default_Epsilon) return MV_Analysis is
      theAnalysis : MV_Analysis;
   begin
      theAnalysis.Flag.Dual := Flags.Dual;
      if Flags.Dual then
         MV := E2GA.Dual(MV);
      end if;
      return theAnalysis;
   end Analyze;

   --  --------------------------------------------------------------------------

   function Blade_Subclass (A : MV_Analysis) return Blade_Subclass_Type is

   begin
      return  A.Analysis_Type.Blade_Subclass;
   end Blade_Subclass;

   --  --------------------------------------------------------------------------

   function Default_Epsilon return float is
   begin
      return Default_Epsilon_Value;
   end Default_Epsilon;

   --  --------------------------------------------------------------------------

   function Num_Points return integer is
   begin
      return Number_Of_Points;
   end Num_Points;

   --  --------------------------------------------------------------------------

   function isValid (A : MV_Analysis) return Boolean is
   begin
      return A.Flag.Valid = Flag_Valid;
   end isValid;

   --  --------------------------------------------------------------------------

   function isDual (A : MV_Analysis) return Boolean is
   begin
      return A.Flag.Dual;
   end isDual;

   --  --------------------------------------------------------------------------

   function isBlade (A : MV_Analysis) return Boolean is
      use Multivector_Type_Base;
   begin
      Put_Line ("MV Analysis checking for blade");
      return A.MV_Kind = Multivector_Type_Base.Blade;
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
      return isBlade (A) and A.Analysis_Type.Blade_Class = Zero_Blade;
   end isNull;

   --  --------------------------------------------------------------------------

   function isZero (A : MV_Analysis)  return Boolean is
      --  {return ((type() == BLADE) && (bladeClass() == ZERO));}
   begin
      return isBlade (A) and A.Analysis_Type.Blade_Class = Zero_Blade;
   end isZero;

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