
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

package body GA_Maths is

   function Bit_Count (Bits : Unsigned_Integer) return Natural is
      use Interfaces;
      Count   : Unsigned_32 := Interfaces.Unsigned_32 (Bits);
   begin
      Count := Count - ((Shift_Right (Count, 1)) and 16#55555555#);
      Count := (Count and 16#33333333#) + (Shift_Right (Count, 2) and 16#33333333#);
      Count := (Count + Shift_Right (Count, 4)) and 16#0F0F0F0F#;
      Count := Count + (Shift_Right (Count, 8));
      Count := Count + (Shift_Right (Count, 16));
      --  Return count in range 0 to 31.
      return Natural (Count and 16#0000003F#);
   end Bit_Count;

   --  ------------------------------------------------------------------------

   function Grade (Bits : Unsigned_Integer) return Natural is
   begin
      --  Return the grade as a number in  the range 0 to 31.
      return Bit_Count (Bits);
   end Grade;

   --  ------------------------------------------------------------------------

   function Maximum (I1, I2 : Integer) return Integer is
      Max : Integer;
   begin
      if I1 > I2 then
         Max := I1;
      else
         Max := I2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Maximum (I1, I2 : Float) return Float is
      Max : Float;
   begin
      if I1 > I2 then
         Max := I1;
      else
         Max := I2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Minimum (I1, I2 : Integer) return Integer is
      Min : Integer;
   begin
      if I1 < I2 then
         Min := I1;
      else
         Min := I2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

   function Minimum (I1, I2 : Float) return Float is
      Min : Float;
   begin
      if I1 < I2 then
         Min := I1;
      else
         Min := I2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

end GA_Maths;
