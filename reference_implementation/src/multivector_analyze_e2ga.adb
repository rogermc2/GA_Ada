
with E3GA;
with GA_Maths;
with Multivector_Type_Base;

package body Multivector_Analyze_E2GA is

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : in out E2GA.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type_Base;

      Base      : MV_Typebase;

--     type Multivector_Analyze.M_Type is record
--        Model_Kind       : Model_Type := Vector_Space;
--        Multivector_Kind : Multivector_Type_Base.M_Type_Type :=
--                             Multivector_Type_Base.Invalid_Base_Type;
--        Blade_Class      : Blade_Type;
--        Blade_Subclass   : Blade_Subclass_Type;
--        Versor_Subclass  : Versor_Type;
--        Round_Kind       : Round_Type := Round_Invalid;
--     end record;

      --  Init_MV_Type corresponds to e2ga::mvType constructor.
--        type Type_Base is record
--          M_Zero        : boolean := False; -- True if multivector is zero
--          M_Type        : Object_Type := Multivector;
--          M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
--          M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
--          M_Parity      : Parity := None;
--        end record;
      Analysis   : MV_Analysis;
   begin
      E2GA.Init (MV, Epsilon);
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Vector_Space;

      if Flags.Dual then
         Analysis.M_Flags.Dual := True;
         MV := E2GA.Dual (MV);
      end if;

      Analysis.M_Type.Multivector_Kind := Analysis.M_MV_Type.M_Type;
      --  Check for zero blade
      if Analysis.M_MV_Type.M_Zero then
         --           Model.Model_Kind := M_Zero;
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.M_Scalors (1) := 0.0;
      elsif Analysis.M_Type.Multivector_Kind = Versor_Object then
         Analysis.M_Type.Blade_Subclass := Even_Versor_Subclass;
         Analysis.M_Vectors (1) := E3GA.e1;

         if MV.Coordinates (1) < 0.0 then
            Analysis.M_Vectors (2) := E3GA.e2;
         else
            declare
               use E3GA;
            begin
               Analysis.M_Vectors (2) := -E3GA.e2;
            end;
         end if;

         Analysis.M_Vectors (2) := E3GA.e1;
         Analysis.M_Scalors (1) := E2GA.Get_Coord (E2GA.Norm_E2 (MV));
         Analysis.M_Scalors (2) := 2.0 * GA_Maths.Float_Functions.Arctan
             (E2GA.Get_Coord (E2GA.Norm_E2 (MV)), MV.Coordinates (1));
      elsif Analysis.M_Type.Multivector_Kind = Blade_Object then
         case Analysis.M_MV_Type.M_GU of
            when 0 =>
            Analysis.M_Type.Blade_Subclass := Analysis.M_MV_Type.M_GU;
         end case;
         Analysis.M_Scalors (1) := E2GA.Get_Coord (E2GA.Norm_E (MV));
      end if;
   end Analyze;

end Multivector_Analyze_E2GA;
