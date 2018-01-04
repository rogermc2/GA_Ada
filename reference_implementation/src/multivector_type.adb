
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

package body Multivector_Type is

   function Init (MV : Multivector.Multivector) return MV_Type_Record is
      use Interfaces;
      use GA_Maths;
      use Multivector;
      Rec        : MV_Type_Record;
      Grade      : Unsigned_Integer := Top_Grade_Index (MV);
      Count      : array (1 .. 2) of Unsigned_Integer := (0, 0);
      Index      : Integer := 1;
      GU         : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
      Versor_Inv : Multivector.Multivector;
      Grade_Inv  : Multivector.Multivector;
   begin
      Rec.Grade_Use := Grade_Use (MV);
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
         Versor_Inv := Versor_Inverse (MV);
         Grade_Inv := Grade_Inversion (MV);
         if Geometric_Product (Versor_Inv, Grade_Inv) =
           Geometric_Product (Grade_Inv, Versor_Inv) then
           --  multivector = multivector
            Rec.MV_Kind := Multivector_Type;
         elsif Bit_Count (Grade_Use (MV)) = 1 then
            Rec.MV_Kind := Blade_MV;
         else
            Rec.MV_Kind := Versor_MV;
         end if;
      end if;

      return Rec;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Print_Multivector_Info (Name : String; Info : MV_Type_Record) is
   begin
      Put_Line (Name);
      Put_Line ("Type      " & MV_Type'Image (Info.MV_Kind));
      Put_Line ("Zero      " & boolean'Image (Info.Zero));
      Put_Line ("Top Grade " & GA_Maths.Unsigned_Integer'Image (Info.Top_Grade));
      Put_Line ("Grade Use " & GA_Maths.Grade_Usage'Image (Info.Grade_Use));
      Put_Line ("Parity    " & Parity_Type'Image (Info.Parity));
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector_Type.Print_Multivector_Info.");
         raise;
   end Print_Multivector_Info;

   --  ------------------------------------------------------------------------

end Multivector_Type;
