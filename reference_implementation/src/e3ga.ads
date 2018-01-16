
with Ada.Strings.Unbounded;

with Blade;
with GA_Maths; use GA_Maths;
with Multivector; use Multivector;

package E3GA is

   type Array_19F is array (1 .. 19) of float;
   subtype Vector_Coords_3D is GA_Maths.Coords_Continuous_Array (1 .. 3);

   --  Multivector types
   type G2_Type is (MVT_None, MVT_E1_T, MVT_E2_T, MVT_E3_T, MVT_Scalar,
                    MVT_Vector_2D, MVT_Vector, MVT_Bivector, MVT_Trivector,
                    MVT_Rotor, MVT_E1_CT, MVT_E2_CT, MVT_E3_CT,
                    MVT_I3_CT, MVT_I3I_CT, MVT_MV, MVT_Last);
   --  Outermorphism types
   type OM_Type is (OMT_None, OMT_OM, OMT_Last);
   --    type Rotor_Coordinates_Type is (Rotor_Scalar_e1e2_e2e3_e3e1);

--     type Scalar is private;
--     type Bivector is private;
   type Outermorphism is private;
   type Syn_SMultivector is private;
--     type Rotor is private;
--     type Trivector is private;

   --  Vector corresponds to e3ga.vector coordinate storage float m_c[3]
--     type Vector is private;
   type MV_Coordinate_Array is new GA_Maths.Coords_Continuous_Array (1 .. 8);

--     type Multivector (Grade_Use : Grade_Usage) is record
--        Coordinates : MV_Coordinate_Array  := (others => 0.0);  --  m_c[8]
--     end record;

   --  Joinable grade definitions
   Grade_0 : constant integer := 1;
   Grade_1 : constant integer := 2;
   Grade_2 : constant integer := 4;
   Grade_3 : constant integer := 8;

   --  ------------------------------------------------------------------------

--     function "=" (V1, V2 : Vector) return Boolean;
--     function "+" (V1, V2 : Vector) return Vector;
--     function "-" (V : Vector) return Vector;
--     function "-" (V1, V2 : Vector) return Vector;
--     function "*" (Weight : float; V : Vector) return Vector;

--     function "*" (Weight : float; BV : Bivector) return Bivector;
--     function "*" (R1, R2 : Rotor) return Rotor;
--     function "*" (R : Rotor; V : Vector) return Rotor;
--     function "*" (V : Vector; R : Rotor) return Rotor;
--     function "/" (R : Rotor; S : float) return Rotor;
--     function "+" (W : float; BV : BiVector) return Rotor;
--     function "+" (W : float; R : Rotor) return Rotor;
--     function "-" (W : float; R : Rotor) return Rotor;

--     function e1 (V : E2GA.Vector) return float;
--     function e2 (V : E2GA.Vector) return float;

--     function e1 return Vector;
--     function e2 return Vector;
--     function e3 return Vector;

   function E1 (MV : Multivector.Multivector) return float;
   function E2 (MV : Multivector.Multivector) return float;
   function E1_E2 (MV : Multivector.Multivector) return float;

   function e1e2 (R : Rotor) return float;
   function e2e3 (R : Rotor) return float;
   function e3e1 (R : Rotor) return float;
   function R_Scalar (R : Rotor) return float;

--     function Apply_Outermorphism (OM : Outermorphism; BV : Bivector) return Bivector;
--     function Apply_Outermorphism (OM : Outermorphism; V : Vector) return Vector;
--     function Dot_Product (R1, R2 : Rotor) return float;
--     function Dot_Product (V1, V2 : Vector) return float;
    function Get_Coord (S : Scalar) return float;
--     function Get_Coords (BV : Bivector) return Array_3D;
--     function Get_Coords (MV : Multivector) return MV_Coordinate_Array;
   function Get_Coords (R : Rotor) return Array_4D;
    function Get_Coord_1 (V : Multivector.Vector) return float;
    function Get_Coord_2 (V : Multivector.Vector) return float;
    function Get_Coord_3 (V : Multivector.Vector) return float;
   function Get_Coords (V : Multivector.Vector) return Array_3D;
   function Get_Coords (SMV : Syn_SMultivector) return Array_4D;
   function Get_Outermorphism (OM : Outermorphism) return Array_19F;
--      function Get_Size (MV : Multivector) return Integer;
--     function Geometric_Product (BV : Bivector; R : Rotor) return Rotor;
--     function Geometric_Product (R : Rotor; BV : Bivector) return Rotor;
--     function Geometric_Product (V : Vector; R : Rotor) return Syn_SMultivector;
--     function Geometric_Product (R : Rotor; MV : Syn_SMultivector) return Syn_SMultivector;
--     function Geometric_Product (V : Vector; MV : Syn_SMultivector) return Rotor;
--     function Geometric_Product (R : Rotor; V : Vector) return Syn_SMultivector;
--     function Geometric_Product (R1, R2 : Rotor) return Rotor;
--     function Geometric_Product (V1, V2 : Vector) return Rotor;
--     function Grade_Use (BV : Bivector) return GA_Maths.Unsigned_Integer;
--     function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer;
   function Inverse (aRotor : Rotor) return Rotor;
--     function Inverse (V : Vector) return Vector;
--     function Left_Contraction (BV1, BV2 : Bivector) return Scalar;
--     function Left_Contraction (MV1, MV2 : Multivector) return Multivector;
--     function Left_Contraction (V : Vector; BV : Bivector) return Vector;
--     function Left_Contraction (V1 : Vector; V2 : Vector) return Scalar;
--     function Magnitude (V : Vector) return float;
--     function MV_String (MV : Multivector; Text : String := "")
--                         return Ada.Strings.Unbounded.Unbounded_String;
--     function Outer_Product (V1, V2 : Vector) return Bivector;

--     function Norm_E2 (BV : Bivector) return Scalar;
--     function Norm_E2 (V : Vector) return Scalar;
--     function Norm_E2 (MV : E2GA.Multivector) return Scalar;
--     function Norm_E2 (R : Rotor) return Scalar;
--     function Norm_E2 (TV : Trivector) return Scalar;
--     function Norm_R (BV : Bivector) return Scalar;
--     function Norm_R2 (BV : Bivector) return Scalar;
--      procedure Set_Coords (V : out Vector; C1, C2, C3 : float);
    procedure Set_Coords (MV : out Multivector.Multivector; C1, C2, C3 : float);
--     function Scalar_Product (V1, V2 : Vector) return Scalar;
--     procedure Set_Bivector (BV : out Bivector; C1, C2, C3 : float);
   procedure Set_Rotor (X : out Rotor; C_Scalar, C2, C3, C4 : float);
   procedure Set_Rotor (X : out Rotor; C_Scalar : float);
--     procedure Set_Rotor (X : out Rotor; MV : Multivector);
--     procedure Set_Rotor (X : out Rotor; BV : Bivector);
--     procedure Set_Rotor (X : out Rotor; C_Scalar : float; BV : Bivector);
--      procedure Set_Scalar (S : out Scalar; Value : float);
--      function To_Unsigned (V : Vector) return Vector_Unsigned;
--      function To_2D (V : Vector) return E2GA.Vector;
--      function To_3D (V : E2GA.Vector) return Vector;
--     function To_Vector (MV : Syn_SMultivector) return Vector;
   --  Unit_e normalizes rotor R
   function Unit_e (R : Rotor) return Rotor;
   --  Unit_e normalizes Vector X
--     function Unit_e (X : Vector) return Vector;

private
--     type Scalar is record
--        Coordinates : Scalar_Coords;  --  m_c[1]
--     end record;

   --  Vector corresponds to e3ga.vector coordinate storage float m_c[3]
--     type Vector is record
--        Coordinates : Vector_Coords_3D := (0.0, 0.0, 0.0);   --  m_c[3]
--     end record;
--
--     type Bivector is record
--        Grade_Use   : Grade_Usage := 7;  -- 2^2 + 2^1 +2^0
--        C1_e1e2     : float := 0.0;
--        C2_e2e3     : float := 0.0;
--        C3_e3e1     : float := 0.0;
--     end record;

   type Outermorphism is new Array_19F;

--     type Rotor is record
--        --        Coords_Type : Rotor_Coordinates_Type := Rotor_Scalar_e1e2_e2e3_e3e1;
--        C1_Scalar   : float := 0.0;
--        C2_e1e2     : float := 0.0;
--        C3_e2e3     : float := 0.0;
--        C4_e3e1     : float := 0.0;
--     end record;

   type Syn_SMultivector is record
      C1_e1       : float := 0.0;
      C2_e2       : float := 0.0;
      C3_e3       : float := 0.0;
      C4_e1e2e3   : float := 0.0;
   end record;

end E3GA;
