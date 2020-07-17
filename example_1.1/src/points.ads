
with GL.Types; use GL.Types;

with C3GA;
with E3GA;
with Multivectors;

package Points is
    use Multivectors;
   use C3GA;
   type Normalized_Points_Array is array (int range <>) of Normalized_Point;
   Num_Points : constant Int := 6;

   Point_Data : constant Vector_E3_Array (1 .. Num_Points) :=
                     --   E1         E2        E3
                    ((-0.356756, -0.881980,  0.0),         -- L1
                     (-0.725786,  0.934177, -0.366154),    -- L2
                     (2.612482,   1.495455, -2.704073),    -- C1
                     (2.218644,   0.425753, -1.780935),    -- C2
                     (0.865897,   0.629159, -1.438985),    -- C3
                     (2.846445,  -1.112365, -0.366769));   -- P1

   L0_Points : constant Vector_E3_Array (1 .. 2) :=
                     --   E1   E2    E3
                      ((0.0, -0.9, 0.0),  -- L0L
                       (0.0,  0.9, 0.0)); -- L0R

   type Colour_Data is new Singles.Vector4_Array (1 .. Num_Points);

   Line_Point_Index   : Int := 1;
   Circle_Point_Index : Int := 3;
   Plane_Point_Index  : Int := 6;

   --  A Normalized_Point consists of five blades representing the vectors:
   --  no = 1.0, E1, E2, E3, ni = Inf

   n  : constant M_Vector := E3GA.e2;  --  n is a direction vector
   L1 : constant Normalized_Point := Set_Normalized_Point (Point_Data (1));
   L2 : constant Normalized_Point := Set_Normalized_Point (Point_Data (2));

   C1 : constant Normalized_Point := Set_Normalized_Point (Point_Data (3));
   C2 : constant Normalized_Point := Set_Normalized_Point (Point_Data (4));
   C3 : constant Normalized_Point := Set_Normalized_Point (Point_Data (5));

   P1 : constant Normalized_Point := Set_Normalized_Point (Point_Data (6));

   L0L : constant Normalized_Point := Set_Normalized_Point (L0_Points (1));
   L0R : constant Normalized_Point := Set_Normalized_Point (L0_Points (2));

   Normalized_Points : constant Normalized_Points_Array (1 .. Num_Points) :=
                         (L1, L2, C1, C2, C3, P1);
   L0_Normalized_Points : constant Normalized_Points_Array (1 .. 2) :=
                            (L0L, L0R);

   function Set_Draw_Colour (Colour : Singles.Vector4) return Colour_Data;

end Points;
