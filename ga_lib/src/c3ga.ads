
with GL.Types;

with GA_Maths;

with Blade_Types;
with E3GA;
with Multivectors;
with Multivector_Type_Base;

package C3GA is

   --  Vector_E3GA corresponds to c3ga.vectorE3GA coordinate storage float m_c[3]
   subtype Vector_E3GA is GL.Types.Singles.Vector3;
   subtype Vector_E3GA_Array is GL.Types.Singles.Vector3_Array;

   subtype MV_Type is Multivector_Type_Base.MV_Typebase;

--     subtype Circle_Coords is E2GA.Coords_Continuous_Array (1 .. 10);
--     subtype Dual_Plane_Coords is E2GA.Coords_Continuous_Array (1 .. 4);
--     subtype Line_Coords is E2GA.Coords_Continuous_Array (1 .. 6);
--     subtype Sphere_Coords is E2GA.Coords_Continuous_Array (1 .. 5);

   --  user constants
   --	__ni_ct__ ni; declared in c3ga.cpp infinitiy
   --	__no_ct__ no; declared in c3ga.cpp origin

--     type Point is private;  -- 5D conformal null vector
--     type Scalar is private;
--     type Vector is private;

   subtype Point is Multivectors.Multivector;
   subtype Sphere is Multivectors.Multivector;
   subtype Dual_Sphere is Multivectors.Multivector;
   type Dual_Sphere_Array is array (integer range <>) of Dual_Sphere;
   type C3GA_Normalized_Point is array (1 .. 5) of Float;

   --  Joinable grade definitions
   Grade_0 : constant GA_Maths.Unsigned_Integer := 1;
   Grade_1 : constant GA_Maths.Unsigned_Integer := 2;
   Grade_2 : constant GA_Maths.Unsigned_Integer := 4;
   Grade_3 : constant GA_Maths.Unsigned_Integer := 8;
   Grade_4 : constant GA_Maths.Unsigned_Integer := 16;
   Grade_5 : constant GA_Maths.Unsigned_Integer := 32;

--     function "+" (L, R : Vector_E3GA) return Vector_E3GA;
--     function "-" (L, R : Vector_E3GA) return Vector_E3GA;
   function "*" (L : float; R : Vector_E3GA) return Vector_E3GA;
--     function "*" (L : Vector_E3GA; R : float) return Vector_E3GA;
--     function Coord (S : Multivectors.Scalar) return float;
--     function Init (MV : Multivectors.Multivector; Epsilon : float:= 0.0) return MV_Type;

   --  Basis Vectors
   function e1 return Multivectors.Multivector;
   function e2 return Multivectors.Multivector;
   function e3 return Multivectors.Multivector;
   function ni return Multivectors.Multivector;
   function no return Multivectors.Normalized_Point;

   --  Component getters
   function e1 (MV :  Multivectors.Multivector) return float;
   function e2 (MV : Multivectors.Multivector) return float;
   function e3 (MV : Multivectors.Multivector) return float;
   function e1_e2 (MV : Multivectors.Multivector) return float;
   function e1_e3 (MV : Multivectors.Multivector) return float;
   function e2_e3 (MV : Multivectors.Multivector) return float;
   function e1_e2_e3 (MV : Multivectors.Multivector ) return float;

   function E1_E2_NI (C : Multivectors.Circle) return float;
--     function E1_E2_E3 (C :  Multivectors.Circle) return float;
   function E1_E3_NI (C :  Multivectors.Circle) return float;
   function E2_E3_NI (C :  Multivectors.Circle) return float;
   function Get_Coords (V : Vector_E3GA) return GA_Maths.Array_3D;
--     function Get_Coords (NP : Normalized_Point) return Vector;
   function Get_Coords (NP : Multivectors.Normalized_Point)
                        return GA_Maths.Coords_Continuous_Array;
   function NO_E1_E2 (C :  Multivectors.Circle) return float;
   function NO_E1_E3 (C :  Multivectors.Circle) return float;
   function NO_E1_NI (C :  Multivectors.Circle) return float;
   function NO_E2_E3 (C :  Multivectors.Circle) return float;
   function NO_E2_NI (C :  Multivectors.Circle) return float;
   function NO_E3_NI (C :  Multivectors.Circle) return float;

--     function E1b (DP :  Multivectors.Dual_Plane) return float;
--     function E2b (DP :  Multivectors.Dual_Plane) return float;
--     function E3b (DP :  Multivectors.Dual_Plane) return float;
--     function NIb (DP :  Multivectors.Dual_Plane) return GA_Base_Types.NI_T;

--     function E1_E2_NI (L :  Multivectors.Line) return float;
--     function E1_E3_NI (L :  Multivectors.Line) return float;
--     function E2_E3_NI (L :  Multivectors.Line) return float;
   function E1_NO_NI (L :  Multivectors.Line) return float;
   function E2_NO_NI (L :  Multivectors.Line) return float;
   function E3_NO_NI (L :  Multivectors.Line) return float;

   function NO_E1_E2_E3_NI (MV : Multivectors.Multivector) return float;

   function E1b (NP : Multivectors.Normalized_Point) return float;
   function E2b (NP : Multivectors.Normalized_Point) return float;
   function E3b (NP : Multivectors.Normalized_Point) return float;
   function NIb (NP : Multivectors.Normalized_Point) return Float;
   function NOb (NP : Multivectors.Normalized_Point) return Float;

   function E1_E2_E3_NI (S : Sphere) return float;
   function E1_E2_NO_NI (S : Sphere) return float;
   function E1_E3_NO_NI (S : Sphere) return float;
   function E2_E3_NO_NI (S : Sphere) return float;
   function E1_E2_E3_NO (S : Sphere) return float;

   function C3GA_Point (V : Vector_E3GA) return C3GA_Normalized_Point;
--     function Outer_Product (NP1, NP2 : Normalized_Point) return Line;
--     function Outer_Product (NP : Normalized_Point; MV : Multivectors.Multivector)
--                             return Normalized_Point;
--     function Outer_Product (L1, L2 : Line) return Line;
   function Norm_E2 (V : Vector_E3GA) return Float;
   function Norm_R (V : Vector_E3GA) return Float;
   function Norm_R2 (V : Vector_E3GA) return Float;
--     function Norm_R (MV : Multivectors.Multivector) return Float;
--     function Norm_Rsq (MV : Multivectors.Multivector) return Float;
   function Probe (Pr : Blade_Types.C3_Base) return Multivectors.Normalized_Point;

   function Set_Circle (P1, P2, P3 : Multivectors.Normalized_Point) return  Multivectors.Circle;
--     procedure Set_Coords (P : out Multivectors.Point; Origin, C1, C2, C3, Inf : float);
   procedure Set_Coords (V : out Vector_E3GA; C1, C2, C3 : float);
--     function Set_Coords (C1, C2, C3 : float) return Vector_E3GA;
   function Set_Dual_Plane (P1 : Multivectors.Normalized_Point; Dir : Multivectors.Vector)
                            return Multivectors.Dual_Plane;
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; NP : Normalized_Point);
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; N : GA_Base_Types.NO_T);
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; N : GA_Base_Types.NI_T);
--     function Set_Line (E1_E2_NI, E1_E3_NI, E2_E3_NI,
--                        E1_NO_NI, E2_NO_NI, E3_NO_NI : Float) return  Multivectors.Line;
   function Set_Line (P1, P2 : Multivectors.Normalized_Point) return  Multivectors.Line;
   function Set_Normalized_Point (E1, E2, E3 : Float) return Multivectors.Normalized_Point;
   function Set_Normalized_Point (V : Vector_E3GA) return Multivectors.Normalized_Point;
   function Set_Normalized_Point (Point : GA_Maths.Array_3D) return Multivectors.Normalized_Point;
--     function Outer_Product (MV1, MV2 : Multivectors.Multivector) return Multivectors.Multivector;
--     function Unit_R (L :  Multivectors.Line) return  Multivectors.Line;

   --  Underscore constructors
   function To_VectorE3GA (MV : Multivectors.Multivector) return Vector_E3GA;
   function To_VectorE3GA (Vec : E3GA.E3_Vector) return Vector_E3GA;
   function NP_To_VectorE3GA (NP : Multivectors.Normalized_Point) return Vector_E3GA;
--     function US_Normalized_Point (N : Normalized_Point) return Normalized_Point;
--     function US_Set_Normalized_Point (Point : Vector_E3GA) return Normalized_Point;
--     function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point;
    function Unit_E (X : C3GA.Vector_E3GA) return GL.Types.Singles.Vector3;

private

--     type Scalar is record
--        Coordinates : GA_Maths.Scalar_Coords;  --  m_c[1]
--     end record;

--     type Circle is record   --  m_c[10]
--        E1_E2_NI, E1_E2_E3, E2_E3_NI, E3_E1_NI, NO_E1_E2 : float := 0.0;
--        NO_E1_E3, NO_E1_NI, NO_E2_E3, NO_E2_NI, NO_E3_NI : float := 0.0;
--     end record;

--     type Dual_Sphere is record   --  m_c[4]
--        NO, E1, E2, E3, NI : float := 0.0;
--        Inf                : GA_Base_Types.NI_T;
--     end record;

--     type Line is record   --  m_c[6]
--        E1_E2_NI, E1_E3_NI, E2_E3_NI : float := 0.0;
--        E1_NO_NI, E2_NO_NI, E3_NO_NI : float := 0.0;
--     end record;

--     type Normalized_Point is record     --  m_c[4
--        --  Origin             : float := 1.0;      constant
--        E1, E2, E3 : float := 0.0;
--        Inf        : float := 0.0;
--     end record;

--     type Point is record   --  m_c[5]
--        Origin     : GA_Base_Types.NO_T;
--        E1, E2, E3 : float := 0.0;
--        Inf        : float := 0.0;
--     end record;

--     type Sphere is record   --  m_c[5]
--        E1_E2_E3_NI, E1_E2_NO_NI, E1_E3_NO_NI : float := 0.0;
--        E2_E3_NO_NI, E1_E2_E3_NO              : float := 0.0;
--     end record;

   type Vector is new GA_Maths.Coords_Continuous_Array (1 .. 5);

end C3GA;
