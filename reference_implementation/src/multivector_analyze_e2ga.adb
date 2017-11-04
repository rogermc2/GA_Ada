
with Multivector_Type_Base;

package body Multivector_Analyze_E2GA is

   procedure Analyze (MV : in out E2GA.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type_Base;

      Base      : Type_Base;
      Model     : M_Type;  --  m_type
      --  Init_MV_Type corresponds to e2ga::mvType constructor.
      M_MV_Type : E2GA.MV_Type := E2GA.Init (MV, Epsilon);
      Analysis  : MV_Analysis;
   begin
      --  Initialize Multivector_Type_Base.Current_Type_Base
--        type Type_Base is record
--          M_Zero        : boolean := False; -- True if multivector is zero
--          M_Type        : Object_Type := Multivector;
--          M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
--          M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
--          M_Parity      : Parity := None;
--        end record;

      Analysis.M_Flags.Valid := True;
      Model.Model_Kind := Multivector_Analyze.Vector_Space;

      if Flags.Dual then
--           Set_M_Flags (Flags.Valid, Current_Flags.Dual xor Flags.Dual);
         Analysis.M_Flags.Dual := True;
         MV := E2GA.Dual (MV);
      end if;
      Model.Model_Kind := M_MV_Type.M_Type;
      if M_MV_Type.M_Zero then
         Model.Model_Kind := M_Zero;
      end if;
   end Analyze;

end Multivector_Analyze_E2GA;
