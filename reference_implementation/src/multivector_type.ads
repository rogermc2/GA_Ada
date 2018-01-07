
with GA_Maths;
with Multivector;

package Multivector_Type is

   type MV_Type is (Unspecified_MV_Type, Multivector_Type, Versor_MV, Blade_MV);
   type Parity_Type is (Odd_Parity, Even_Parity, No_Parity);
   type MV_Type_Record is private;

   function Init (MV : Multivector.Multivector) return MV_Type_Record;
   procedure Print_Multivector_Info (Name : String; Info : MV_Type_Record);

private

   type MV_Type_Record is record
      MV_Kind          : MV_Type := Unspecified_MV_Type;
      Zero             : Boolean;
      Top_Grade        : GA_Maths.Unsigned_Integer := 0;
      Grade_Use        : GA_Maths.Grade_Usage;
      Parity           : Parity_Type := No_Parity;
   end record;

end Multivector_Type;
