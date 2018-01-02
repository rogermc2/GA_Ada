
package body Multivector_Type is

   function Init (MV : Multivector.Multivector) return MV_Type_Record is
      Rec : MV_Type_Record;
      Grade : GA_Maths.Unsigned_Integer := Multivector.Top_Grade_Index (MV);
   begin
      return Rec;
   end Init;

   --  -------------------------------------------------------------------------

end Multivector_Type;
