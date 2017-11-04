
with Multivector_Type_Base;

package body Multivector_Analyze_E2GA is

   procedure Analyze (MV : in out E2GA.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type_Base;
      Base      : Type_Base;
      Model     : M_Type;  --  m_type
      M_MV_Type : E2GA.MV_Type;
   begin
      --  Initialize Multivector_Type_Base.Current_Type_Base
--        type Type_Base is record
--          M_Zero        : boolean := False; -- True if multivector is zero
--          M_Type        : Object_Type := Multivector;
--          M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
--          M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
--          M_Parity      : Parity := None;
--        end record;

      Set_M_Type (Base, Vector_Space_Model);
      Set_M_Type (Base, Unused_Type);
      if Flags.Dual then
         Set_M_Flags (Flags.Valid, Current_Flags.Dual xor Flags.Dual);
         MV := E2GA.Dual (MV);
      end if;
      Model.Model_Kind := Vector+Space_Model
      Set_M_Multivector_Type (Get_Multivector_Type (MV, Epsilon));
   end Analyze;

end Multivector_Analyze_E2GA;
