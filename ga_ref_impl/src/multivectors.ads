with Interfaces;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with Blade;
with Blade_Types; use Blade_Types;
with GA_Maths;
with Metric;

package Multivectors is

   type Geometry_Type is (Geometry_Not_Set, E1_Geometry, E2_Geometry,
                          E3_Geometry, H3_Geometry, C3_Geometry);
   type MV_Type is (MV_Multivector, MV_Scalar, MV_Vector, MV_Bivector,
                    MV_Trivector, MV_Rotor, MV_Point, MV_Normalized_Point,
                    MV_Line, MV_Circle, MV_Sphere, MV_Dual_Line, MV_Dual_Plane,
                    MV_Dual_Sphere, MV_TR_Versor);
   type Grade_Status is (Grade_OK, Grade_Null, Grade_Inhomogeneous);
   type Multivector is private;
   type Multivector_List is private;
   subtype Bivector is Multivector;
   subtype Circle is Multivector;
   subtype Dual_Line is Multivector;
   subtype Dual_Plane is Multivector;
   subtype Line is Multivector;
   subtype Normalized_Point is Multivector;
   subtype Rotor is Multivector;
   subtype Scalar is Multivector;
   subtype TR_Versor is Multivector;
   subtype M_Vector is Multivector;

   MV_Exception : Exception;

   function "+" (MV : Multivector; S : Float) return Multivector;
   function "+" (S : Float; MV : Multivector) return Multivector;
   function "+" (MV1, MV2 : Multivector) return Multivector;
   function "-" (MV : Multivector; S : Float) return Multivector;
   function "-" (S : Float; MV : Multivector) return Multivector;
   function "-" (MV : Multivector) return Multivector;
   function "-" (MV1, MV2 : Multivector) return Multivector;
   function "*" (Scale : float; MV : Multivector) return Multivector;
   function "*" (MV : Multivector; Scale : float) return Multivector;
   function "/" (MV : Multivector; Scale : float) return Multivector;
   procedure Add_Blade (MV : in out Multivector; aBlade : Blade.Basis_Blade);
   procedure Add_Blade (MV : in out Multivector; Index : E2_Base; Value : Float);
   procedure Add_Blade (MV : in out Multivector; Index : E3_Base; Value : Float);
   procedure Add_Blade (MV : in out Multivector; Index : C3_Base; Value : Float);
--     procedure Add_Complex_Blade (MV     : in out Multivector; Index : C3_Base;
--                                  Value : GA_Maths.Complex_Types.Complex);
   procedure Add_Multivector (MV_List : in out Multivector_List; MV : Multivector);
   function Basis_Vector (Index : BV_Base) return M_Vector;
   function Basis_Vector (Index : E2_Base) return M_Vector;
   function Basis_Vector (Index : E3_Base) return M_Vector;
   function Basis_Vector (Index : C3_Base) return M_Vector;
   function Blades (MV : Multivector) return Blade.Blade_List;
   function Component  (MV : Multivector; BM : Interfaces.Unsigned_32)
                        return Float;
   procedure Compress (MV : in out Multivector; Epsilon : Float);
   function Cosine (MV : Multivector) return Multivector;
   function Cosine (MV : Multivector; Order : Integer) return Multivector;
   function Dot (MV1, MV2 : Multivector) return Multivector;
   function Dual (MV : Multivector; Met : Metric.Metric_Record) return Multivector;
   function Dual (MV : Multivector; Dim : Integer) return Multivector;
   function Exp (MV : Multivector; Met : Metric.Metric_Record) return Multivector;
   function Extract_Grade (MV : Multivector; Index : integer) return Multivector;
   function From_Vector (V : M_Vector) return Multivector;
   function General_Inverse (MV : Multivector) return Multivector;
   function General_Inverse (MV : Multivector;  Met : Metric.Metric_Record)
                             return Multivector;
   function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
   function Geometric_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                               return Multivector;
   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector;
   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector;
   --  Get_Basis_Vector returns multivector of the required base.
   function Get_Blade (MV : Multivector; Index : Interfaces.Unsigned_32)
                       return Blade.Basis_Blade;
   function Get_Blade (MV : Multivector; theBlade : out Multivector;
                       Index : Interfaces.Unsigned_32) return Boolean;

   function Get_Blade_List (MV : Multivector) return Blade.Blade_List;
   function Get_Random_Blade (Dim, Grade : Integer; Scale : Float)
                              return Multivector;
   function Get_Random_Vector (Dim : Integer; Scale : Float)  return Multivector;
   function Grade (MV : Multivector; theGrade : out Integer) return Grade_Status;
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage;
   function Grade_Inversion (MV : Multivector) return Multivector;
   function Highest_Grade (MV : Multivector) return Integer;
   function Inner_Product (MV1, MV2 : Multivector; Cont : Blade.Contraction_Type)
                           return Multivector;
   function Inner_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record;
                           Cont : Blade.Contraction_Type) return Multivector;
   function Inverse (theScalar : Scalar) return Scalar;
   function Is_Null (MV : Multivector) return Boolean;
   function Is_Null (MV : Multivector; Epsilon : Float) return Boolean;
   function Is_Scalar (MV : Multivector) return Boolean;
   function Largest_Basis_Blade (MV : Multivector) return Blade.Basis_Blade;
   function Left_Contraction (MV1, MV2 : Multivector) return Multivector;
   function Left_Contraction (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                              return Multivector;
   function List_Length (MV_List : Multivector_List) return Integer;
   function Multivector_String (MV : Multivector; BV_Names : Blade.Basis_Vector_Names)
                                return Ada.Strings.Unbounded.Unbounded_String;
   function MV_First (MV_List : Multivector_List) return Multivector;
   function MV_Item (MV_List : Multivector_List; Index : Integer) return Multivector;
   function MV_Kind (MV : Multivector) return MV_Type;
   function MV_Size (MV : Multivector) return Natural;
   function Negate (MV : Multivector) return Multivector;
   function New_Bivector (V1, V2 : M_Vector) return Bivector;
   function New_Bivector (e1e2, e2e3, e3e1 : Float) return Bivector;
   function New_Circle return Circle;
   function New_Dual_Line return Dual_Line;
   function New_Dual_Plane return Dual_Plane;
   function New_MV_Line return Line;
   --  New_Multivector returns a multivector with a scalar blade only
   function New_Multivector (Scalar_Weight : Float) return Multivector;
   function New_Multivector (aBlade : Blade.Basis_Blade) return Multivector;
   function New_Multivector (Blades : Blade.Blade_List) return Multivector;
   function New_Normalized_Point return Normalized_Point;
   function New_Normalized_Point (e1, e2, e3 : Float) return Normalized_Point;
   function New_Rotor return Rotor;
   function New_Rotor (Scalar_Weight : Float) return Rotor;
   function New_Rotor (Scalar_Weight : Float; BV : Bivector) return Rotor;
   function New_Rotor (Scalar_Weight, e1, e2, e3 : Float) return Rotor;
   function New_Scalar  (Scalar_Weight : Float := 0.0) return Scalar;
   function New_TR_Versor (Scalar_Weight : Float := 0.0) return TR_Versor;
   function New_Vector return M_Vector;
   function New_Vector (e1, e2 : Float) return M_Vector;
   function New_Vector (e1, e2, e3 : Float) return M_Vector;
   function Norm_E (MV : Multivector) return Float;
   function Norm_Esq (MV : Multivector) return Float;
   function Outer_Product (MV1, MV2 : Multivector) return Multivector;
   function Random_Blade (Dim, Grade : Integer; Scale : Float) return Multivector;
   function Reverse_MV (MV : Multivector) return Multivector;
--     function Rotor_Inverse (R : Rotor; IR : out Rotor) return Boolean;
   function Right_Contraction (MV1, MV2 : Multivector) return Multivector;
   function Right_Contraction (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                               return Multivector;
   function Scalar_Part (MV : Multivector) return Float;
   function Scalar_Product (MV1, MV2 : Multivector) return float;
   function Scalar_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                            return float;
   procedure Set_Geometry (theGeometry : Geometry_Type);
   procedure Simplify (MV : in out Multivector);
   function Sine (MV : Multivector) return Multivector;
   function Sine (MV : Multivector; Order : Integer) return Multivector;
   function Space_Dimension return Natural;
   function Top_Grade_Index (MV : Multivector) return Integer;
   procedure To_Circle (MV : in out Multivector);
   procedure To_Dual_Plane (MV : in out Multivector);
   procedure To_Line (MV : in out Multivector);
   function To_Rotor (MV : Multivector) return Rotor;
   function To_Vector (MV : Multivector) return M_Vector;
   function Unit_E (MV : Multivector) return Multivector;
   function Unit_R (MV : Multivector) return Multivector;
   function Unit_R (MV : Multivector; Met : Metric.Metric_Record)
                    return Multivector;
   procedure Update (MV : in out Multivector; Blades : Blade.Blade_List;
                     Sorted : Boolean := False);
   procedure Update_Scalar_Part (MV : in out Multivector; Value : Float);
   function Versor_Inverse (MV : Multivector) return Multivector;
   function Versor_Inverse (MV : Multivector; Met : Metric.Metric_Record)
                            return Multivector;

private
   type Multivector is record
      Type_Of_MV : MV_Type := MV_Multivector;
      Blades     : Blade.Blade_List;
      Sorted     : Boolean := False;
   end record;

   package MV_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Multivector);
   type Multivector_List is new MV_List_Package.List with null Record;

end Multivectors;
