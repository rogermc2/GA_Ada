
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

package body GA_Maths is

   function Bit_Count (Value : Interfaces.Unsigned_32) return Natural is
   begin
      return 0;
   end Bit_Count;

   --  ------------------------------------------------------------------------

   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float is
      use Interfaces;
      A     : Unsigned_32 := Shift_Right (Unsigned_32 (Map_A), 1);
      B     : Unsigned_32 := Unsigned_32 (Map_B);
      Swaps : Natural := 0;
   begin
      while A /= 0 loop
         Swaps := Swaps + Bit_Count (A and B);
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

end GA_Maths;
