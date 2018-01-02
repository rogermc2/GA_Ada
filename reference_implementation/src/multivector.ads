
with Interfaces;

with Ada.Containers.Doubly_Linked_Lists;

with Blade;
with GA_Maths;

package Multivector is
   use Blade;
   package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Blade.Basis_Blade);
   type Blade_List is new Blade_List_Package.List with null record;

   type Multivector is private;

   function C3_Multivector return Multivector;
   function Get_Basis_Vector (Index : Integer) return Multivector;
   function Get_Blade_List (MV : Multivector) return Blade_List;
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage;
   function Largest_Grade_Part (MV : Multivector) return Multivector;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;
   function Scalar_Part (MV : Multivector) return Float;
   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer;

private
   type Multivector is record
      Blades : Blade_List;
      Sorted : Boolean := False;
   end record;

end Multivector;
