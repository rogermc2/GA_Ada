
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Blade_Types; use Blade_Types;
with GA_Maths; use GA_Maths;
with Metric;

package Blade is

   use Ada.Strings.Unbounded;

   package Names_Package is new
     Ada.Containers.Vectors (Natural, Unbounded_String);
   type Basis_Vector_Names is new Names_Package.Vector with null record;

   type Basis_Blade is private;
   type Complex_Basis_Blade is private;

   type Contraction_Type is (Left_Contraction, Right_Contraction,
                             Hestenes_Inner_Product,
                             Modified_Hestenes_Inner_Product);

   function "*" (S : Float; BB : Basis_Blade) return Basis_Blade;
   function "*" (BB : Basis_Blade; S : Float) return Basis_Blade;
   function Bitmap (BB : Basis_Blade) return Unsigned_Integer;
   function Blade_String (aBlade : Basis_Blade; BV_Names : Basis_Vector_Names)
                          return Ada.Strings.Unbounded.Unbounded_String;
   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float;
   function Inner_Product_NP (BA, BB : Basis_Blade; Cont : Contraction_Type)
                              return Basis_Blade;
   function Geometric_Product (BB : Basis_Blade; Sc : Float) return Basis_Blade;
   function Geometric_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function Geometric_Product (BA, BB : Basis_Blade; Met : Metric.Metric_Record)
                               return Basis_Blade;
   function Grade (BB : Basis_Blade) return Integer;
   function Grade_Inversion (B : Basis_Blade) return Basis_Blade;
   function Inner_Product (BA, BB : Basis_Blade; Cont : Contraction_Type)
                           return Basis_Blade;
   function Minus_1_Power (Power : Integer) return Integer;
   function New_Basis_Blade (Bitmap : Unsigned_Integer; Weight : Float := 1.0)
                             return Basis_Blade;

   function New_Basis_Blade (Index : BV_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Basis_Blade (Index : E2_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Basis_Blade (Index : E3_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Basis_Blade (Index : C3_Base; Weight : Float := 1.0) return Basis_Blade;
   function New_Complex_Basis_Blade (Index  : C3_Base;
                                     Weight : Complex_Types.Complex := (0.0, 1.0))
                                     return Complex_Basis_Blade;
   function New_Scalar_Blade (Weight : Float := 1.0) return Basis_Blade;
   function New_Zero_Blade return Basis_Blade;

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
   procedure Print_Blade (Name : String; B : Basis_Blade);
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

   type Complex_Basis_Blade is record
      Bitmap : Unsigned_Integer := 0;
      Weight : Complex_Types.Complex := (0.0, 0.0);
   end record;

end Blade;
