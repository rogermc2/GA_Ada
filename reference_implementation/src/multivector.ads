
with Interfaces;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;

with Blade;
with GA_Maths;

package Multivector is
   use Blade;
   package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Blade.Basis_Blade);
   type Blade_List is new Blade_List_Package.List with null record;

   type Multivector is private;
   type Scalar is new Float;

   MV_Exception : Exception;

   function Add (MV : Multivector; S : Float) return Multivector;
   function "+" (MV1, MV2 : Multivector) return Multivector;
   function "-" (MV : Multivector) return Multivector;
   function "-" (MV1, MV2 : Multivector) return Multivector;
   procedure Add_Blade (MV : in out Multivector; aBlade : Blade.Basis_Blade);
   function Blades (MV : Multivector) return Blade_List;
   function C3_Multivector return Multivector;
   function Dual (MV : Multivector) return Multivector;
   function Dual (MV : Multivector; Dim : Integer) return Multivector;
   function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector;
   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector;
   --  Get_Basis_Vector returns multivector of the required base.
   function Get_Basis_Vector (Index : E2_Base) return Multivector;
   function Get_Basis_Vector (Index : E3_Base) return Multivector;
   function Get_Basis_Vector (Index : C3_Base) return Multivector;
   function Get_Blade_List (MV : Multivector) return Blade_List;
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage;
   function Grade_Inversion (MV : Multivector) return Multivector;
   function Inner_Product (MV1, MV2 : Multivector; Cont : Contraction_Type)
                           return Multivector;
   function Largest_Grade_Part (MV : Multivector) return Multivector;
   --  New_Multivector returns a multivector with a scalar blade only
   function New_Multivector (Scalar_Weight : Float) return Multivector;
   function Norm_E (MV : Multivector) return Scalar;
   function Norm_E2 (MV : Multivector) return Scalar;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;
   function Reverse_MV (MV : Multivector) return Multivector;
   function Scalar_Part (MV : Multivector) return Float;
   function Scalar_Product (MV1, MV2 : Multivector) return float;
   function Size (MV : Multivector) return Natural;
   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer;
   function To_String (MV : Multivector; BV_Names : Blade.Basis_Vector_Names)
                       return Ada.Strings.Unbounded.Unbounded_String;
   function Unit_E (MV : Multivector) return Multivector;
   function Unit_R (MV : Multivector) return Multivector;
   function Versor_Inverse (MV : Multivector) return Multivector;

private
   type Multivector is record
      Blades : Blade_List;
      Sorted : Boolean := False;
   end record;

end Multivector;
