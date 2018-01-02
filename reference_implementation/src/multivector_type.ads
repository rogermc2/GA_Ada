
with GA_Maths;
with Multivector;
with Multivector_Type_Base;

package Multivector_Type is

   type MV_Type_Record is private;

   function Init (MV : Multivector.Multivector) return MV_Type_Record;

private

   type MV_Type_Record is record
      Multivector_Kind : Multivector_Type_Base.Object_Type :=
                           Multivector_Type_Base.Multivector_Object;
      Top_Grade        : GA_Maths.Unsigned_Integer;
      Grade_Use        : GA_Maths.Grade_Usage;
      Parity           :Boolean;
   end record;

end Multivector_Type;
