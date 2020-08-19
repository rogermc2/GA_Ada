
with Interfaces; use Interfaces;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Blade_Types; use Blade_Types;
with GA_Maths;
with Metric;

package Blade is

    use Ada.Strings.Unbounded;

    type Basis_Blade is record
        Bitmap : Interfaces.Unsigned_32 := 0;
        Weight : Float := 0.0;
    end record;

    type Complex_Basis_Blade is private;

    type Basis_Blade_Array is array (Integer range <>) of Basis_Blade;

    package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Basis_Blade);
    type Blade_List is new Blade_List_Package.List with null record;

    package Blade_Vector_Package is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Basis_Blade);
    type Blade_Vector is new Blade_Vector_Package.Vector with null record;

    function "<" (Left, Right : Blade.Basis_Blade) return Boolean;

    package Blade_Sort_Package is new
      Blade_List_Package.Generic_Sorting ("<");

    type Contraction_Type is (Left_Contraction, Right_Contraction,
                              Hestenes_Inner_Product,
                              Modified_Hestenes_Inner_Product);
    Blade_Exception : Exception;

    function "*" (S : Float; BB : Basis_Blade) return Basis_Blade;
    function "*" (BB : Basis_Blade; S : Float) return Basis_Blade;

    procedure Add_Blade (Blades : in out Blade_List; BB : Basis_Blade);
    procedure Add_Blade (Blades : in out Blade_Vector;
                         Index  : Natural; BB : Basis_Blade);
    procedure Add_Blades (Blades : in out Blade_List; More_Blades : Blade_List);
    function BB_First (BB_List : Blade_List) return Basis_Blade;
    function BB_Item (BB_List : Blade_List; Index : Integer) return Basis_Blade;
    function Bitmap (BB : Basis_Blade) return Unsigned_32;
    function Blade_String (aBlade : Basis_Blade; BV_Names : Basis_Vector_Names)
                          return Ada.Strings.Unbounded.Unbounded_String;
    function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_32) return Float;
    function Geometric_Product (BA, BB : Basis_Blade) return Basis_Blade;
    function Geometric_Product (BB : Basis_Blade; Sc : Float) return Basis_Blade;
    function Geometric_Product (BA, BB : Basis_Blade;
                                Eigen_Vals : GA_Maths.Float_Array_Package.Real_Vector)
                                return Basis_Blade;
    function Geometric_Product (BA, BB : Basis_Blade; Met : Metric.Metric_Record)
                               return Blade_List;
    function Grade (BB : Basis_Blade) return Integer;
    function Grade_Inversion (B : Basis_Blade) return Basis_Blade;
    function Inner_Product (BA, BB : Basis_Blade; Cont : Contraction_Type)
                           return Basis_Blade;
    function Inner_Product (BA, BB : Basis_Blade; Met : Metric.Metric_Record;
                            Cont   : Contraction_Type) return Blade_List;
    function List_Length (Blades : Blade_List) return Integer;
    function Minus_1_Power (Power : Integer) return Integer;
    function New_Basis_Blade (Bitmap : Unsigned_32; Weight : Float := 1.0)
                             return Basis_Blade;
    function New_Basis_Blade (Weight : Float := 0.0) return Basis_Blade;

    function New_Basis_Blade (Index : BV_Base; Weight : Float := 1.0)
                              return Basis_Blade;
    function New_Basis_Blade (Index : E2_Base; Weight : Float := 1.0)
                              return Basis_Blade;
    function New_Basis_Blade (Index : E3_Base; Weight : Float := 1.0)
                              return Basis_Blade;
    function New_Basis_Blade (Index : C3_Base; Weight : Float := 1.0)
                              return Basis_Blade;
    function New_Blade (Bitmap : Unsigned_32; Weight : Float := 1.0)
                        return Basis_Blade;
    function New_Complex_Basis_Blade (Index  : C3_Base;
                                      Weight : GA_Maths.Complex_Types.Complex
                                      := (0.0, 1.0))
                                       return Complex_Basis_Blade;
    function New_Scalar_Blade (Weight : Float := 1.0) return Basis_Blade;
    function New_Zero_Blade return Basis_Blade;

    function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
    function Reverse_Blade (B : Basis_Blade) return Basis_Blade;
    procedure Simplify (Blades : in out Blade_List);
    procedure Update_Blade (BB : in out Basis_Blade; Weight : Float);
    procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_32);
    procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_32;
                            Weight : Float);
    function Weight (BB : Basis_Blade) return Float;

private
    type Complex_Basis_Blade is record
        Bitmap : Unsigned_32 := 0;
        Weight : GA_Maths.Complex_Types.Complex := (0.0, 0.0);
    end record;

end Blade;
