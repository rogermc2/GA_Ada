
with Interfaces;

package body Blade is

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer is
   begin
      return BB.Bitmap;
   end Bitmap;

   --  ------------------------------------------------------------------------

   function Weight (BB : Basis_Blade) return Float is
   begin
      return BB.Weight;
   end Weight;

   --  ------------------------------------------------------------------------

   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float is
      use GA_Maths;
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
         New_Blade.Weight := 0.0;
      else
         New_Blade.Bitmap := BA.Bitmap or BB.Bitmap;
         Sign := Canonical_Reordering_Sign (BA.Bitmap, BB.Bitmap);
         New_Blade.Weight := Sign * BA.Weight * BB.Weight;
      end if;

      return New_Blade;
   end GP_OP;

   --  ------------------------------------------------------------------------

   function Grade_Inversion (B : Basis_Blade) return Basis_Blade is
      W : constant float
        := Float (Minus_1_Power (Grade (B.Bitmap)) * Unsigned_Integer (B.Weight));
   begin
      return New_Basis_Blade (B.Bitmap, W);
   end Grade_Inversion;

   --  ------------------------------------------------------------------------

   function Minus_1_Power (Number : Unsigned_Integer) return Unsigned_Integer is
   begin
      if (Number and 1) = 0 then
         return 1;
      else
         return -1;
      end if;
   end Minus_1_Power;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : Base; Weight : Float := 1.0) return Basis_Blade is
      use Interfaces;
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := Unsigned_Integer (Shift_Left (Unsigned_32 (1), Index'Enum_Rep));
      Blade.Weight := Weight;
      return Blade;
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Bitmap : Unsigned_Integer; Weight : Float := 1.0)
                             return Basis_Blade is
      use Interfaces;
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := Bitmap;
      Blade.Weight := Weight;
      return Blade;
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade is
   begin
      return GP_OP (BA, BB, True);
   end Outer_Product;

   --  ------------------------------------------------------------------------

   function Reverse_Blade (B : Basis_Blade) return Basis_Blade is
      W : constant float
        := Float (Minus_1_Power
                  ((Grade (B.Bitmap) * (Grade (B.Bitmap) - 1) / 2)
                     * Unsigned_Integer (B.Weight)));
   begin
      return New_Basis_Blade (B.Bitmap, W);
   end Reverse_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Weight : Float) is
   begin
      BB.Weight := Weight;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer) is
   begin
      BB.Bitmap := Bitmap;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Weight : Float) is
   begin
      BB.Bitmap := Bitmap;
      BB.Weight := Weight;
   end Update_Blade;

   --  ------------------------------------------------------------------------

end Blade;
