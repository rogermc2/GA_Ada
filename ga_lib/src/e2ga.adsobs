
with GL.Types;

with GA_Maths;
with Multivectors; use Multivectors;
with Multivector_Type_Base;

package E2GA is
--     e1_bv   : Multivector.Multivector := Multivector.Get_Basis_Vector (Blade.E2_e1);
--     e2_bv   : Multivector.Multivector := Multivector.Get_Basis_Vector (Blade.E2_e2);

--     subtype Rotor_Coords is GA_Maths.Coords_Continuous_Array (1 .. 2);
--     subtype Vector_Coords is GA_Maths.Coords_Continuous_Array (1 .. 2);

   subtype Scalar is GL.Types.Single;
   subtype Rotor is GL.Types.Singles.Vector2;

   --  Vector corresponds to e2ga.Vector coordinate storage float m_c[2]
--     type Vector is private;

   --  Outermorphism types
   type OM_Type is (OMT_None, OMT_OM, OMT_Last);

   type E2_Bit_Map is new integer range 0 .. 4;

   --  Multivector types
   type G2_Type is (MVT_None, MVT_E1_T, MVT_E2_T, MVT_Scalar, MVT_Vector,
                    MVT_Bivector, MVT_Rotor, MVT_E1_CT, MVT_E2_CT,
                    MVT_I2_CT, MVT_I2I_CT, MVT_MV, MVT_Last);

   --  The outer product of P vectors is called a grade P multivector or a P-vector
   --  In E2, MV = u^v = det(u,v)e1^e2. This "bivector" is the only MV in E2.

   subtype MV_Type is Multivector_Type_Base.MV_Typebase;

--     type Multivector (Grade_Use : GA_Maths.Grade_Usage) is record
--        Coordinates : GA_Maths.Coords_Continuous_Array (1 .. 4) := (others => 0.0);   --  m_c[4]
--     end record;

   --  A bivector is defined here as the outerproduct of two vectors.
   --  2-blade is the more correct terminology; that is, a 2-vector
   --  is not necessarilly a 2-blade.
--     type Bivector is record
--        Coordinates : GA_Maths.Bivector_Coords;   --  m_c[1]
--     end record;

   --  Joinable grade definitions
   Grade_0 : constant GA_Maths.Unsigned_Integer := 1;
   Grade_1 : constant GA_Maths.Unsigned_Integer := 2;
   Grade_2 : constant GA_Maths.Unsigned_Integer := 4;

   E2_Exception : Exception;

--     function "+" (V1, V2 : Vector) return Vector;
--     function "-" (V1, V2 : Vector) return Vector;
--     function "*" (Weight : float; V : Vector) return Vector;
--     function "*" (V1, V2 : Vector) return Vector;
--     function "+" (MV1, MV2 : Multivector) return Multivector;
--     function "-" (MV1, MV2 : Multivector) return Multivector;

   function Bivector_String (BV : Multivectors.Bivector) return String;
--     function Dot_Product (V1, V2 : Vector) return float;
--     function Dot_Product (R1, R2 : Rotor) return float;
--     function Dual (MV : Multivector) return Multivector;
   function e1 return Multivectors.Vector;
   function e2 return Multivectors.Vector;
   function e1 (MV : Multivectors.Multivector) return float;
   function e2 (MV : Multivectors.Multivector) return float;
   function e1_e2 (BV : Multivectors.Bivector) return float;
--     function Get_Basis_Vector_Names return Blade.Basis_Vector_Names;
   function Get_Coord (BV : Bivector) return float;
   function Get_Coord_1 (V : Multivectors.Vector) return float;
   function Get_Coord_2 (V : Multivectors.Vector) return float;
--     function Get_Coords (MV : Multivector) return GA_Maths.Coords_Continuous_Array;
--     function Get_Size (MV : Multivector) return Integer;
--     function Get_Coords (BV : Bivector) return GA_Maths.Coords_Continuous_Array;
   function Get_Coords (V : Multivectors.Vector) return GA_Maths.Array_F2;

--     function Get_Size (BV : Bivector) return Integer;
--     function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
--     function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer;
--     function Init (MV : Multivector; Epsilon : float:= 0.0) return MV_Type;
--     function Largest_Basis_Blade (Map : Bit_Map) return float;
   function Largest_Coordinate return float;
--     function Magnitude (V : Vector) return float;
--     function Multivector_String (MV : Multivector.Multivector;
--                                  Text : String := "") return String;
--     function Norm_E (MV : Multivector) return Scalar;
--     function Norm_E2 (MV : E2GA.Multivector) return Scalar;
--     function Outer_Product (V1, V2 : Vector) return Bivector;
--     function Scalar_Product (V1, V2 : Vector) return Scalar;
--     function Set_Bivector (V1, V2 : Vector) return Bivector;
--     function Set_Bivector (MV : Multivector) return Bivector;
   procedure Set_Coords (V : out Multivectors.Vector; C1, C2 : float);
   --  The Set_Multivector functions correspond to equivalent e2ga mv::set functions
--     function Set_Multivector (S1 : Scalar) return Multivector;
--     function Set_Multivector (V : Vector) return Multivector;
--     function Set_Multivector (BV : Bivector) return Multivector;
--     function Set_Multivector (R : Rotor) return Multivector;
--     function Set_Bivector (S1 : Scalar) return Bivector;
--     function Set_Bivector (V : Vector) return Bivector;
--     function Set_Bivector (R : Rotor) return Bivector;
   function Set_Rotor (E1_E2 : float) return Rotor;
   function Unit_E (V : Multivectors.Vector) return Multivectors.Vector;
--     function Unit_E (MV : Multivector) return Vector;
--     function Unit_E (BV : Bivector) return Vector;

end E2GA;
