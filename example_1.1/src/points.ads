
with GL.Types; use GL.Types;

with C3GA;
with E3GA;
with GA_Maths;

package Points is

   type Points_Array is array (integer range <>) of GA_Maths.Array_3D;

   Line_Points : Points_Array (1 .. 2) :=
                    ((-0.356756, -0.881980, 0.0),
                     (-0.725786,  0.934177, -0.366154));

   Circle_Points : Points_Array (1 .. 3) :=
                    ((2.612482, 1.495455, -2.704073),
                     (2.218644, 0.425753, -1.780935),
                     (0.865897, 0.629159, -1.438985));

   Plane_Point : GA_Maths.Array_3D := (2.846445, -1.112365, -0.366769);

   --  n is a direction vector
   n : constant E3GA.Vector := E3GA.e2;

   L1 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Line_Points (1));
   L2 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Line_Points (2));

   C1 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Circle_Points (1));
   C2 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Circle_Points (2));
   C3 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Circle_Points (3));

   P1 : constant C3GA.Normalized_Point := C3GA.Set_Normalized_Point (Plane_Point);

end Points;
