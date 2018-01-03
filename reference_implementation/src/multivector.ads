
with Interfaces;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;

with Blade;
with GA_Maths;

package Multivector is
   use Blade;
   package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Blade.Basis_Blade);
   type Blade_List is new Blade_List_Package.List with null record;

   type Multivector is private;

   MV_Exception : Exception;

   function C3_Multivector return Multivector;
   function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector;
   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector;
   function Get_Basis_Vector (Index : Base) return Multivector;
   function Get_Blade_List (MV : Multivector) return Blade_List;
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage;
   function Grade_Inversion (MV : Multivector) return Multivector;
   function Inner_Product (MV1, MV2 : Multivector; Cont : Contraction_Type)
                           return Multivector;
   function Largest_Grade_Part (MV : Multivector) return Multivector;
   function New_Multivector (Scale : Float) return Multivector;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;
   function Reverse_MV (MV : Multivector) return Multivector;
   function Scalar_Part (MV : Multivector) return Float;
   function Scalar_Product (MV1, MV2 : Multivector) return float;
   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer;
   function Unit_E (MV : Multivector) return Multivector;
   function Unit_R (MV : Multivector) return Multivector;
   function Versor_Inverse (MV : Multivector) return Multivector;

private
   type Multivector is record
      Blades : Blade_List;
      Sorted : Boolean := False;
   end record;

end Multivector;
