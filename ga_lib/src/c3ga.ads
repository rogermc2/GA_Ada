
with GA_Base_Types;
with GA_Maths;

with Blade;
with Blade_Types;
with E3GA;
with Multivectors;
with Multivector_Type_Base;

package C3GA is

   type Vector_E3GA is private;

   subtype MV_Type is Multivector_Type_Base.MV_Typebase;

--     subtype Circle_Coords is E2GA.Coords_Continuous_Array (1 .. 10);
--     subtype Dual_Plane_Coords is E2GA.Coords_Continuous_Array (1 .. 4);
--     subtype Line_Coords is E2GA.Coords_Continuous_Array (1 .. 6);
--     subtype Sphere_Coords is E2GA.Coords_Continuous_Array (1 .. 5);

   --  user constants
   --	__ni_ct__ ni; declared in c3ga.cpp infinitiy
   --	__no_ct__ no; declared in c3ga.cpp origin

   type Point is private;  -- 5D conformal null vector
   type Scalar is private;
   type Vector is private;

   type Line is new Multivectors.Multivector;
   type Circle is new Multivectors.Multivector;
   type Sphere is new Multivectors.Multivector;
   type Dual_Plane is new Multivectors.Multivector;
   type Dual_Sphere is new Multivectors.Multivector;
   type Dual_Sphere_Array is array (integer range <>) of Dual_Sphere;
--     type Normalized_Point is private;
   type Normalized_Point is new Multivectors.Multivector;

--     type Multivector (Grade_Use : GA_Maths.Grade_Usage) is record
--        Coordinates : GA_Maths.MV_Coordinate_Array := (others => 0.0);  --  m_c[32]
--     end record;

   --  Joinable grade definitions
   Grade_0 : constant GA_Maths.Unsigned_Integer := 1;
   Grade_1 : constant GA_Maths.Unsigned_Integer := 2;
   Grade_2 : constant GA_Maths.Unsigned_Integer := 4;
   Grade_3 : constant GA_Maths.Unsigned_Integer := 8;
   Grade_4 : constant GA_Maths.Unsigned_Integer := 16;
   Grade_5 : constant GA_Maths.Unsigned_Integer := 32;

   function Set_Normalized_Point (V : Vector_E3GA) return Normalized_Point;
   function Coord (S : Scalar) return float;
--     function Init (MV : Multivectors.Multivector; Epsilon : float:= 0.0) return MV_Type;

   function e1 return Multivectors.Multivector;
   function e2 return Multivectors.Multivector;
   function e3 return Multivectors.Multivector;
   function ni return Multivectors.Multivector;
   function no return Multivectors.Multivector;

   function e1 (MV : Multivectors.Multivector) return float;
   function e2 (MV : Multivectors.Multivector) return float;
   function e3 (MV : Multivectors.Multivector) return float;
   function e1_e2 (MV : Multivectors.Multivector) return float;
   function e1_e3 (MV : Multivectors.Multivector) return float;
   function e2_e3 (MV : Multivectors.Multivector) return float;
   function e1_e2_e3 (MV : Multivectors.Multivector) return float;

   function E1_E2_NI (C : Circle) return float;
   function E1_E2_E3 (C : Circle) return float;
   function E1_E3_NI (C : Circle) return float;
   function E2_E3_NI (C : Circle) return float;
   function Get_Coord_1 (V : Vector_E3GA) return float;
   function Get_Coord_2 (V : Vector_E3GA) return float;
   function Get_Coord_3 (V : Vector_E3GA) return float;
   function Get_Coords (V : Vector_E3GA) return GA_Maths.Array_3D;
--     function Get_Coords (NP : Normalized_Point) return Vector;
   function Get_Coords (NP : Normalized_Point)
                        return GA_Maths.Coords_Continuous_Array;
   function NO_E1_E2 (C : Circle) return float;
   function NO_E1_E3 (C : Circle) return float;
   function NO_E1_NI (C : Circle) return float;
   function NO_E2_E3 (C : Circle) return float;
   function NO_E2_NI (C : Circle) return float;
   function NO_E3_NI (C : Circle) return float;

--     function E1b (DP : Dual_Plane) return float;
--     function E2b (DP : Dual_Plane) return float;
--     function E3b (DP : Dual_Plane) return float;
--     function NIb (DP : Dual_Plane) return GA_Base_Types.NI_T;

   function E1_E2_NI (L : Line) return float;
   function E1_E3_NI (L : Line) return float;
   function E2_E3_NI (L : Line) return float;
   function E1_NO_NI (L : Line) return float;
   function E2_NO_NI (L : Line) return float;
   function E3_NO_NI (L : Line) return float;

   function NO_E1_E2_E3_NI (MV : Multivectors.Multivector) return float;

   function E1b (NP : Normalized_Point) return float;
   function E2b (NP : Normalized_Point) return float;
   function E3b (NP : Normalized_Point) return float;
   function NIb (NP : Normalized_Point) return Float;
   function NOb (NP : Normalized_Point) return Float;

   function E1_E2_E3_NI (S : Sphere) return float;
   function E1_E2_NO_NI (S : Sphere) return float;
   function E1_E3_NO_NI (S : Sphere) return float;
   function E2_E3_NO_NI (S : Sphere) return float;
   function E1_E2_E3_NO (S : Sphere) return float;

   function Outer_Product (NP1, NP2 : Normalized_Point) return Line;
   function Outer_Product (NP : Normalized_Point; MV : Multivectors.Multivector)
                           return Normalized_Point;
   function Outer_Product (L1, L2 : Line) return Line;
--     function Norm_E (MV : Multivectors.Multivector) return Scalar;
   function Norm_E2 (V : Vector_E3GA) return Float;
   function Probe (Pr : Blade_Types.C3_Base) return Normalized_Point;

   function Set_Circle (P1, P2, P3 : Normalized_Point) return Circle;
   procedure Set_Coords (P : out Point; Origin, C1, C2, C3, Inf : float);
   procedure Set_Coords (V : out Vector_E3GA; C1, C2, C3 : float);
   function Set_Coords (C1, C2, C3 : float) return Vector_E3GA;
   function Set_Dual_Plane (P1, P2 : Normalized_Point) return Dual_Plane;
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; NP : Normalized_Point);
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; N : GA_Base_Types.NO_T);
--     procedure Set_Multivector (MV : out  Multivectors.Multivector; N : GA_Base_Types.NI_T);
   function Set_Line (E1_E2_NI, E1_E3_NI, E2_E3_NI,
                      E1_NO_NI, E2_NO_NI, E3_NO_NI : Float) return Line;
   function Set_Line (P1, P2 : Normalized_Point) return Line;
   function Set_Normalized_Point (E1, E2, E3 : Float; Inf : float := 1.0)
                                  return Normalized_Point;
   function Set_Normalized_Point (Point : GA_Maths.Array_3D;
                                  Inf : float := 1.0)
                                  return Normalized_Point;
--     function Outer_Product (MV1, MV2 : Multivectors.Multivector) return Multivectors.Multivector;
   function Unit_R (L : Line) return Line;

   --  Underscore functions
--     function US_Normalized_Point (N : Normalized_Point) return Normalized_Point;
--     function US_Set_Normalized_Point (Point : Vector_E3GA) return Normalized_Point;
--     function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point;

private

   type Scalar is record
      Coordinates : GA_Maths.Scalar_Coords;  --  m_c[1]
   end record;

   --  Vector_E3GA corresponds to c3ga.vectorE3GA coordinate storage float m_c[3]
   type Vector_E3GA is record
      Coordinates : E3GA.Vector_Coords_3D := (0.0, 0.0, 0.0);   --  m_c[3]
   end record;

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

   type Point is record   --  m_c[5]
      Origin     : GA_Base_Types.NO_T;
      E1, E2, E3 : float := 0.0;
      Inf        : float := 0.0;
   end record;

--     type Sphere is record   --  m_c[5]
--        E1_E2_E3_NI, E1_E2_NO_NI, E1_E3_NO_NI : float := 0.0;
--        E2_E3_NO_NI, E1_E2_E3_NO              : float := 0.0;
--     end record;

   type Vector is new GA_Maths.Coords_Continuous_Array (1 .. 5);

end C3GA;
