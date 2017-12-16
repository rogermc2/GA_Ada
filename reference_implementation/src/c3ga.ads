
with GA_Maths;

with E2GA;
with E3GA;

package C3GA is

   type Vector_E3GA is private;

--     subtype Circle_Coords is E2GA.Coords_Continuous_Array (1 .. 10);
--     subtype Dual_Plane_Coords is E2GA.Coords_Continuous_Array (1 .. 4);
--     subtype Line_Coords is E2GA.Coords_Continuous_Array (1 .. 6);
--     subtype Sphere_Coords is E2GA.Coords_Continuous_Array (1 .. 5);

   type Circle is private;
   type Dual_Plane is private;
   type Line is private;
   type Sphere is private;

   type Normalized_Point is private;
   --  user constants
   --	__ni_ct__ ni; declared in c3ga.cpp
   --	__no_ct__ no; declared in c3ga.cpp
   type No_T is record
      Coordinates : E2GA.Scalar_Coords := (others => 1.0);
   end record;

   type Ni_T is record
      Coordinates : E2GA.Scalar_Coords := (others => 1.0);
   end record;

   function E1_E2_NI (C : Circle) return float;
   function E1_E2_E3 (C : Circle) return float;
   function E2_E3_NI (C : Circle) return float;
   function E3_E1_NI (C : Circle) return float;
   function Get_Coord_1 (V : Vector_E3GA) return float;
   function Get_Coord_2 (V : Vector_E3GA) return float;
   function Get_Coord_3 (V : Vector_E3GA) return float;
   function Get_Coords (V : Vector_E3GA) return GA_Maths.Array_3D;
   function NO_E1_E2 (C : Circle) return float;
   function NO_E1_E3 (C : Circle) return float;
   function NO_E1_NI (C : Circle) return float;
   function NO_E2_E3 (C : Circle) return float;
   function NO_E2_NI (C : Circle) return float;
   function NO_E3_NI (C : Circle) return float;

   function E1 (DP : Dual_Plane) return float;
   function E2 (DP : Dual_Plane) return float;
   function E3 (DP : Dual_Plane) return float;
   function NI (DP : Dual_Plane) return float;

   function E1_E2_NI (L : Line) return float;
   function E1_E3_NI (L : Line) return float;
   function E2_E3_NI (L : Line) return float;
   function E1_NO_NI (L : Line) return float;
   function E2_NO_NI (L : Line) return float;
   function E3_NO_NI (L : Line) return float;

   function E1 (NP : Normalized_Point) return float;
   function E2 (NP : Normalized_Point) return float;
   function E3 (NP : Normalized_Point) return float;
   function NI (NP : Normalized_Point) return float;
   function NO (NP : Normalized_Point) return float;

   function E1_E2_E3_NI (S : Sphere) return float;
   function E1_E2_NO_NI (S : Sphere) return float;
   function E1_E3_NO_NI (S : Sphere) return float;
   function E2_E3_NO_NI (S : Sphere) return float;
   function E1_E2_E3_NO (S : Sphere) return float;

   procedure Set_Coords (V : out Vector_E3GA; C1, C2, C3 : float);
   function Set_Normalized_Point (E1, E2, E3 : Float; NI : float := GA_Maths.NI) return Normalized_Point;
   function Set_Normalized_Point (Point : GA_Maths.Array_3D; NI : float := GA_Maths.NI)
                                  return Normalized_Point;
   function Unit_R (L : Line) return Line;

   --  Underscore functions
   function US_Normalized_Point (N : Normalized_Point) return Normalized_Point;
   function US_Set_Normalized_Point (Point : GA_Maths.Array_3D) return Normalized_Point;
   function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point;

private
   --  Vector_E3GA corresponds to c3ga.vectorE3GA coordinate storage float m_c[3]
   type Vector_E3GA is record
      Coordinates : E3GA.Vector_Coords_3D := (0.0, 0.0, 0.0);   --  m_c[3]
   end record;

   type Circle is record   --  m_c[10]
      E1_E2_NI, E1_E2_E3, E2_E3_NI, E3_E1_NI, NO_E1_E2 : float := 0.0;
      NO_E1_E3, NO_E1_NI, NO_E2_E3, NO_E2_NI, NO_E3_NI : float := 0.0;
   end record;
   type Dual_Plane is record
      E1, E2, E3, NI : float := 0.0;   --  m_c[4]
   end record;
   type Line is record   --  m_c[6]
      E1_E2_NI, E1_E3_NI, E2_E3_NI : float := 0.0;
      E1_NO_NI, E2_NO_NI, E3_NO_NI : float := 0.0;
   end record;
   type Normalized_Point is record
      E1, E2, E3, NI : float := 0.0;   --  m_c[4]
   end record;
   type Sphere is record   --  m_c[5]
      E1_E2_E3_NI, E1_E2_NO_NI, E1_E3_NO_NI : float := 0.0;
      E2_E3_NO_NI, E1_E2_E3_NO              : float := 0.0;
   end record;

end C3GA;
