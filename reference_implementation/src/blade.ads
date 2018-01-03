
with Interfaces;

with GA_Maths; use GA_Maths;

package Blade is

   type Basis_Blade is private;
   type Base is (no, e1, e2, e3, ni);
   type Contraction_Type is (Left_Contraction, Right_Contraction,
                             Hestenes_Inner_Product,
                             Modified_Hestenes_Inner_Product);

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer;
   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float;
   function Geometric_Product (BB : Basis_Blade; Sc : Float) return Basis_Blade;
   function Geometric_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function Grade (BB : Basis_Blade) return Unsigned_Integer;
   function Grade_Inversion (B : Basis_Blade) return Basis_Blade;
   function Inner_Product (BA, BB : Basis_Blade; Cont : Contraction_Type)
                           return Basis_Blade;
   function Minus_1_Power (Number : Unsigned_Integer) return Unsigned_Integer;
   function New_Basis_Blade (Bitmap : Unsigned_Integer; Weight : Float := 1.0)
                             return Basis_Blade;
   function New_Basis_Blade (Index : Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Scalar_Blade (Weight : Float := 1.0) return Basis_Blade;
   function New_Zero_Blade return Basis_Blade;

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function Reverse_Blade (B : Basis_Blade) return Basis_Blade;
   procedure Update_Blade (BB : in out Basis_Blade; Weight : Float);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Weight : Float);
   function Weight (BB : Basis_Blade) return Float;

private
   type Basis_Blade is record
      Bitmap : Unsigned_Integer := 0;
      Weight : Float := 0.0;
   end record;

end Blade;
