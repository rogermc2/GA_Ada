
with GA_Maths;

with E2GA;

package C3GA is

   subtype Circle_Coords is E2GA.Coords_Continuous_Array (1 .. 10);
   subtype Dual_Plane_Coords is E2GA.Coords_Continuous_Array (1 .. 4);
   subtype Line_Coords is E2GA.Coords_Continuous_Array (1 .. 6);
   subtype Sphere_Coords is E2GA.Coords_Continuous_Array (1 .. 5);

   type Circle is private;
   type Dual_Plane is private;
   type Line is private;
   type Sphere is private;

   type Normalized_Point is record
      Coordinates : GA_Maths.Array_F4 := (0.0, 0.0, 0.0, 0.0);
   end record;

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

private
   type Circle is record
      Coordinates : Circle_Coords := (others => 0.0);   --  m_c[10]
   end record;
   type Dual_Plane is record
      Coordinates : Dual_Plane_Coords := (others => 0.0);   --  m_c[4]
   end record;
   type Line is record
      Coordinates : Line_Coords := (others => 0.0);   --  m_c[6]
   end record;
   type Sphere is record
      Coordinates : Sphere_Coords := (others => 0.0);   --  m_c[5]
   end record;

end C3GA;
