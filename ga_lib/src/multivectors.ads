
with Interfaces;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;

with Blade;
with Blade_Types; use Blade_Types;
with GA_Maths;
with Metric;

package Multivectors is
   use Blade;
   package Blade_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Blade.Basis_Blade);
   type Blade_List is new Blade_List_Package.List with null record;

   type Multivector is private;
   type Multivector_List is private;
   subtype Bivector is Multivector;
   subtype Rotor is Multivector;
   subtype Scalar is Multivector;
   subtype Vector is Multivector;

   MV_Exception : Exception;

   function  "+" (MV : Multivector; S : Float) return Multivector;
   function  "+" (S : Float; MV : Multivector) return Multivector;
   function  "-" (MV : Multivector; S : Float) return Multivector;
   function  "-" (S : Float; MV : Multivector) return Multivector;
   function "+" (MV1, MV2 : Multivector) return Multivector;
   function "-" (MV : Multivector) return Multivector;
   function "-" (MV1, MV2 : Multivector) return Multivector;
   function "*" (Scale : float; MV : Multivector) return Multivector;
   function "*" (MV : Multivector; Scale : float) return Multivector;
   function "/" (MV : Multivector; Scale : float) return Multivector;
   procedure Add_Blade (MV : in out Multivector; aBlade : Blade.Basis_Blade);
   procedure Add_Blade (MV : in out Multivector; Index : E2_Base; Value : Float);
   procedure Add_Blade (MV : in out Multivector; Index : E3_Base; Value : Float);
   procedure Add_Blade (MV : in out Multivector; Index : C3_Base; Value : Float);
   procedure Add_Multivector (MV_List : in out Multivector_List; MV : Multivector);
   function Blades (MV : Multivector) return Blade_List;
--     function C3_Multivector return Multivector;
   function Component  (MV : Multivector; BM : GA_Maths.Unsigned_Integer;
                        Value : out Float) return Boolean;
   function Cosine (MV : Multivector) return Multivector;
   function Cosine (MV : Multivector; Order : Integer) return Multivector;
   function Dot (MV1, MV2 : Multivector) return Multivector;
   function Dual (MV : Multivector) return Multivector;
   function Dual (MV : Multivector; Dim : Integer) return Multivector;
   function Extract_Grade (MV : Multivector; Index : integer) return Multivector;
   function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector;
   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector;
   function General_Inverse (MV : Multivector) return Multivector;
   function General_Inverse (MV : Multivector;
                             Met : Metric.Metric_Record) return Multivector;
   --  Get_Basis_Vector returns multivector of the required base.
   function Basis_Vector (Index : BV_Base) return Multivector;
   function Basis_Vector (Index : E2_Base) return Multivector;
   function Basis_Vector (Index : E3_Base) return Multivector;
   function Basis_Vector (Index : C3_Base) return Multivector;
   function Get_Blade (MV : Multivector; Index : GA_Maths.Unsigned_Integer)
                       return Blade.Basis_Blade;
   function Get_Blade (MV : Multivector; theBlade : out Multivector;
                       Index : GA_Maths.Unsigned_Integer) return Boolean;

   function Get_Blade_List (MV : Multivector) return Blade_List;
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage;
   function Grade_Inversion (MV : Multivector) return Multivector;
   function Inner_Product (MV1, MV2 : Multivector; Cont : Contraction_Type)
                           return Multivector;
   function Is_Null (MV : Multivector) return Boolean;
   function Is_Null (MV : Multivector; Epsilon : Float) return Boolean;
   function Is_Scalar (MV : Multivector) return Boolean;
   function Largest_Basis_Blade (MV : Multivector) return Blade.Basis_Blade;
   function Left_Contraction (MV1, MV2 : Multivector) return Multivector;
   function Multivector_String (MV : Multivector; BV_Names : Blade.Basis_Vector_Names)
                                return Ada.Strings.Unbounded.Unbounded_String;
   function MV_First (MV_List : Multivector_List) return Multivector;
   function MV_Item (MV_List : Multivector_List; Index : Integer) return Multivector;
   function Negate (MV : Multivector) return Multivector;
   function New_Bivector (V1, V2 : Vector) return Bivector;
   function New_Bivector (e1e2, e2e3, e3e1 : Float) return Bivector;
   --  New_Multivector returns a multivector with a scalar blade only
   function New_Multivector (Scalar_Weight : Float) return Multivector;
   function New_Multivector (aBlade : Basis_Blade) return Multivector;
   function New_Rotor return Rotor;
   function New_Rotor (Scalar_Weight : Float) return Rotor;
   function New_Rotor (Scalar_Weight : Float; BV : Bivector) return Rotor;
   function New_Rotor (Scalar_Weight, e1, e2, e3 : Float) return Rotor;
   function New_Vector (e1, e2 : Float) return Vector;
   function New_Vector (e1, e2, e3 : Float) return Vector;
   function Norm_E (MV : Multivector) return Float;
   function Norm_E2 (MV : Multivector) return Float;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;
   function Reverse_MV (MV : Multivector) return Multivector;
   function Right_Contraction (MV1, MV2 : Multivector) return Multivector;
   function Scalar_Part (MV : Multivector) return Float;
   function Scalar_Product (MV1, MV2 : Multivector) return float;
   procedure Simplify (MV : in out Multivector);
   function Sine (MV : Multivector) return Multivector;
   function Sine (MV : Multivector; Order : Integer) return Multivector;
   function Size (MV : Multivector) return Natural;
   function Space_Dimension (MV : Multivector) return Integer;
   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer;
   function Unit_E (MV : Multivector) return Multivector;
   function Unit_R (MV : Multivector) return Multivector;
   procedure Update (MV : in out Multivector; Blades : Blade_List;
                     Sorted : Boolean := False);
   procedure Update_Scalar_Part (MV : in out Multivector; Value : Float);
   function Versor_Inverse (MV : Multivector) return Multivector;

private
   type Multivector is record
      Blades : Blade_List;
      Sorted : Boolean := False;
   end record;

   package MV_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Multivector);
   type Multivector_List is new MV_List_Package.List with null Record;

end Multivectors;
