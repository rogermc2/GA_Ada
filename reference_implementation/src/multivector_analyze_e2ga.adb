
with Multivector_Type_Base;

package body Multivector_Analyze_E2GA is

   procedure Analyze (MV : in out E2GA.Multivector;
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
      Model     : M_Type;  --  m_type

      --  Init_MV_Type corresponds to e2ga::mvType constructor.
--        type Type_Base is record
--          M_Zero        : boolean := False; -- True if multivector is zero
--          M_Type        : Object_Type := Multivector;
--          M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
--          M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
--          M_Parity      : Parity := None;
--        end record;
      M_MV_Type  : MV_Typebase;
      Analysis   : MV_Analysis;
   begin
      E2GA.Init (MV, Epsilon);
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Model.Model_Kind := Multivector_Analyze.Vector_Space;

      if Flags.Dual then
         Analysis.M_Flags.Dual := True;
         MV := E2GA.Dual (MV);
      end if;
      --  Multivector_Kind : Multivector_Type_Base.Multivector_Type_Base.Object_Type

      Model.Multivector_Kind := M_MV_Type.M_Type;
      if M_MV_Type.M_Zero then
--           Model.Model_Kind := M_Zero;
         Analysis.M_Scalors (1) := 0.0;
      end if;
   end Analyze;

end Multivector_Analyze_E2GA;
