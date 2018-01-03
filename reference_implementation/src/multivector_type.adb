
with Interfaces;

package body Multivector_Type is

   function Init (MV : Multivector.Multivector) return MV_Type_Record is
      use Interfaces;
      use GA_Maths;
      Rec        : MV_Type_Record;
      Grade      : Unsigned_Integer := Multivector.Top_Grade_Index (MV);
      Usage      : Grade_Usage := Multivector.Grade_Use (MV);
      Count      : array (1 .. 2) of Unsigned_Integer := (0, 0);
      Index      : Integer := 1;
      GU         : Unsigned_32 := Unsigned_32 (Usage);
      Versor_Inv : Multivector.Multivector;
      Grade_Inv  : Multivector.Multivector;
   begin
      while GU /= 0 loop
         if (GU and 1) /= 0 then
            Count (Index) := Count (Index) + 1;
         end if;
         Count (Index) := Count (Index) or 1;
         GU := Shift_Right (GU, 1);
      end loop;

      Rec.Zero := (Count (1) and Count (2)) = 0;
      if Rec.Zero then
         --  multivector = zero blade
         Rec.MV_Kind := Blade_MV;
         Rec.Parity := Even_Parity;
      elsif (Count (1) /= 0) and (Count (2) /= 0) then
         Rec.MV_Kind := Multivector_Type;
         Rec.Parity := No_Parity;
      else
         if Count (2) /= 0 then
            Rec.Parity := Odd_Parity;
         else
            Rec.Parity := Even_Parity;
         end if;
         Versor_Inv := Multivector.Versor_Inverse (MV);

      end if;

      return Rec;
   end Init;

   --  -------------------------------------------------------------------------

end Multivector_Type;
