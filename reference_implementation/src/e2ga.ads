
with Ada.Strings.Unbounded;

with GA_Maths;
with Multivector_Type_Base;

package E2GA is

   type Rotor is private;
   type Scalar is private;
   --  Vector corresponds to e2ga.Vector coordinate storage float m_c[2]
   type Vector is private;

   --     type Coords3_Array is array (integer range <>) of Vector;
   type Coords_Continuous_Array is array (integer range <>) of float;
   subtype Bivector_Coords is Coords_Continuous_Array (1 .. 1);
   subtype Rotor_Coords is Coords_Continuous_Array (1 .. 2);
   subtype Scalar_Coords is Coords_Continuous_Array (1 .. 1);
   subtype Vector_Coords is Coords_Continuous_Array (1 .. 2);

   --  Outermorphism types
   type OM_Type is (OMT_None, OMT_OM, OMT_Last);

   type Bit_Map is new integer range 0 .. 2 ** 30;
   type Bit_Map_Array is array (integer range <>) of Bit_Map;

   type E2_Bit_Map is new integer range 0 .. 4;

   --  Multivector types
   type G2_Type is (MVT_None, MVT_E1_T, MVT_E2_T, MVT_Scalar, MVT_Vector,
                    MVT_Bivector, MVT_Rotor, MVT_E1_CT, MVT_E2_CT,
                    MVT_I2_CT, MVT_I2I_CT, MVT_MV, MVT_Last);

   --  The outer product of P vectors is called a grade P multivector or a P-vector
   --  In E2, MV = u^v = det(u,v)e1^e2. This "bivector" is the only MV in E2.

--     type MV_Type is record
--         --  MV_Type extends MV_Typebase
--        Type_Base : Multivector_Type_Base.MV_Typebase;
--     end record;

   type Multivector (MV_Size : integer; Grade_Use : GA_Maths.Grade_Usage) is record
      --  Multivector extends MV_Typebase
      M_Type_Record : Multivector_Type_Base.MV_Typebase;
      Coordinates   : Coords_Continuous_Array (1 .. MV_Size);   --  m_c[4]
   end record;

   --  A bivector as defined here the outerproduct of two vectors.
   --  2-blade is the more correct terminology; that is, a 2-vector
   --  is not necessarilly a 2-blade.
   type Bivector is record
      Coordinates : Bivector_Coords;   --  m_c[1]
   end record;

   --  Joinable grade definitions
   Grade_0 : constant GA_Maths.Unsigned_Integer := 1;
   Grade_1 : constant GA_Maths.Unsigned_Integer := 2;
   Grade_2 : constant GA_Maths.Unsigned_Integer := 4;

--     subtype Scalar_MV is Multivector (Integer (Grade_0), 1);
--     subtype Vector_MV is Multivector (Integer (Grade_1), 2);
--     subtype Bivector_MV is Multivector (Integer (Grade_2), 4);
--     subtype Rotor is Multivector (Integer (Grade_1), 5);

   function "+" (V1, V2 : Vector) return Vector;
   function "-" (V1, V2 : Vector) return Vector;
   function "*" (Weight : float; V : Vector) return Vector;
   function "*" (V1, V2 : Vector) return Vector;
   function "+" (MV1, MV2 : Multivector) return Multivector;
   function "-" (MV1, MV2 : Multivector) return Multivector;

   function Bivector_String (BV : E2GA.Bivector; Text : String := "") return String;
   function Dot_Product (V1, V2 : Vector) return float;
   function Dot_Product (R1, R2 : Rotor) return float;
   function Dual (MV : Multivector) return Multivector;
   function e1 return Vector;
   function e2 return Vector;
   function E1_E2 (BV : Bivector) return float;
   function Get_Coord_1 (R : Rotor) return float;
   function Get_Coord_2 (R : Rotor) return float;
   function Get_Coord_1 (V : Vector) return float;
   function Get_Coord_2 (V : Vector) return float;
   function Get_Coords (MV : Multivector) return Coords_Continuous_Array;
   function Get_Size (MV : Multivector) return Integer;
   function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
   function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer;
   procedure Init (MV : in out Multivector; Epsilon : float);
   function Largest_Basis_Blade (Map : Bit_Map) return float;
   function Largest_Coordinate return float;
   function Magnitude (V : Vector) return float;
   function Multivector_String (MV : Multivector; Text : String := "") return String;
   function Norm_E (MV : Multivector) return Scalar;
   --     function Norm_E2 (BV : Bivector) return Scalar;
   --     function Norm_E2 (V2 : Vector) return Scalar;     function Norm_E2 (MV : Multivector) return Scalar_MV;
   function Outer_Product (V1, V2 : Vector) return Bivector;
   function Scalar_Product (V1, V2 : Vector) return Scalar;
   function Set_Bivector (V1, V2 : Vector) return Bivector;
   procedure Set_Coords (V : out Vector; C1, C2 : float);
   --  The Set_Multivector functions correspond to equivalent e2ga mv::set functions
   function Set_Multivector (S1 : Scalar) return Multivector;
   function Set_Multivector (V : Vector) return Multivector;
   function Set_Multivector (BV : Bivector) return Multivector;
   function Set_Multivector (R : Rotor) return Multivector;
   function Set_Rotor (E1_E2 : float) return Rotor;
--     function Unit_E (MV : Multivector) return Multivector;
   function Unit_E (V : Vector) return Vector;

private
   type Scalar is record
      Coordinates : Scalar_Coords;   --  m_c[1]
   end record;

   type Rotor is record
      Coordinates : Rotor_Coords;   --  m_c[2]
   end record;

   type Vector is record
      Coordinates : Vector_Coords;   --  m_c[2]
   end record;

end E2GA;
