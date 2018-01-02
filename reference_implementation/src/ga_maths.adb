
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

package body GA_Maths is

   function Bit_Count (Bits : Unsigned_Integer) return Natural is
      use Interfaces;
      Bits_64 : Unsigned_64 := Interfaces.Unsigned_64 (Bits);
      Count   : Unsigned_64 := 0;
   begin
      Bits_64 := Bits_64 - (Shift_Right (Bits_64, 1)) and 16#55555555#;
      Bits_64 := (Bits_64 and 16#33333333#) +
                 (Shift_Right (Bits_64, 2) and 16#33333333#);
      Bits_64 := (Bits_64 + Shift_Right (Bits_64, 4)) and 16#0F0F0F0F#;
      Bits_64 := Bits_64 + (Shift_Right (Bits_64, 8));
      Bits_64 := Bits_64 + (Shift_Right (Bits_64, 16));
      return Natural (Bits_64 and 16#0000003F#);
   end Bit_Count;

   --  ------------------------------------------------------------------------

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer is
   begin
      return BB.Bitmap;
   end Bitmap;

   --  ------------------------------------------------------------------------

   function Blade_Scale (BB : Basis_Blade) return Float is
   begin
      return BB.Scale;
   end Blade_Scale;

   --  ------------------------------------------------------------------------

   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float is
      use Interfaces;
      A     : Unsigned_32 := Shift_Right (Unsigned_32 (Map_A), 1);
      B     : Unsigned_32 := Unsigned_32 (Map_B);
      Swaps : Natural := 0;
   begin
      while A /= 0 loop
         Swaps := Swaps + Bit_Count (Unsigned_Integer (A and B));
         A := Shift_Right (Unsigned_32 (A), 1);
      end loop;

      if Swaps mod 2 = 0 then  -- an even number of swaps
         return 1.0;
      else  -- an odd number of swaps
         return -1.0;
      end if;
   end Canonical_Reordering_Sign;

   --  ------------------------------------------------------------------------

   function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade is
      New_Blade : Basis_Blade;
      Sign      : Float;
   begin
      if Outer and ((BA.Bitmap and BB.Bitmap) /= 0) then
         New_Blade.Scale := 0.0;
      else
         New_Blade.Bitmap := BA.Bitmap or BB.Bitmap;
         Sign := Canonical_Reordering_Sign (BA.Bitmap, BB.Bitmap);
         New_Blade.Scale := Sign * BA.Scale * BB.Scale;
      end if;

      return New_Blade;
   end GP_OP;

   --  ------------------------------------------------------------------------

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade is
   begin
      return GP_OP (BA, BB, True);
   end Outer_Product;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : Integer; Scale : Float := 1.0) return Basis_Blade is
      use Interfaces;
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := Unsigned_Integer (Shift_Left (Unsigned_32 (1), Index));
      Blade.Scale :=Scale;
      return Blade;
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Scale : Float) is
   begin
      BB.Scale := Scale;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer) is
   begin
      BB.Bitmap := Bitmap;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Scale : Float) is
   begin
      BB.Bitmap := Bitmap;
      BB.Scale := Scale;
   end Update_Blade;

   --  ------------------------------------------------------------------------

end GA_Maths;
