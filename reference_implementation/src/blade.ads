
with Interfaces;

with GA_Maths; use GA_Maths;

package Blade is

   type Basis_Blade is private;
   type Base is (no, e1, e2, e3, ni);

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer;
   function Blade_Scale (BB : Basis_Blade) return Float;
   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float;
   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function New_Basis_Blade (Index : Base; Scale : Float := 1.0) return Basis_Blade;

   procedure Update_Blade (BB : in out Basis_Blade; Scale : Float);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Scale : Float);

private
   type Basis_Blade is record
      Bitmap : Unsigned_Integer := 0;
      Scale  : Float := 1.0;
   end record;

end Blade;
