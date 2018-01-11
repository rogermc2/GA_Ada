
with Interfaces;

with Ada.Strings.Unbounded;

with GA_Base_Types;
with GA_Maths; use GA_Maths;

package Blade is

   type Basis_Blade is private;

   type E2_Base is (E2_e1, E2_e2);
   type E3_Base is (E3_e1, E3_e2, E3_e3);
   type C3_Base is (C3_no, C3_e1, C3_e2, C3_e3, C3_ni);
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
   function Minus_1_Power (Number : Integer) return Integer;
   function New_Basis_Blade (Bitmap : Unsigned_Integer; Weight : Float := 1.0)
                             return Basis_Blade;
   function New_Basis_Blade (Index : E2_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Basis_Blade (Index : E3_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Basis_Blade (Index : C3_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Scalar_Blade (Weight : Float := 1.0) return Basis_Blade;
   function New_Zero_Blade return Basis_Blade;

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function Reverse_Blade (B : Basis_Blade) return Basis_Blade;
   function To_String (aBlade : Basis_Blade; BV_Names : GA_Base_Types.Basis_Vector_Names)
                       return Ada.Strings.Unbounded.Unbounded_String;
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
