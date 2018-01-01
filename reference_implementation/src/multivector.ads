
with Ada.Containers.Doubly_Linked_Lists;

with GA_Maths;

package Multivector is
   use GA_Maths;
   package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => GA_Maths.Basis_Blade);
   type Blade_List is new Blade_List_Package.List with null record;

   type Multivector is private;

   function Get_Basis_Vector (Index : Integer) return Multivector;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;

private
   type Multivector is record
      Blades : Blade_List;
      Sortes : Boolean := False;
   end record;

end Multivector;
