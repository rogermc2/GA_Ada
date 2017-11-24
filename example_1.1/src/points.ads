
with GL.Types; use GL.Types;

package Points is
   Num_Points : GL.Types.Int := 6;

   Point_Data : GL.Types.Singles.Vector3_Array (1 .. Num_Points) :=
                    ((-0.356756, -0.881980, 0.0),
                     (-0.725786,  0.934177, -0.366154),
                     (2.612482, 1.495455, -2.704073),
                     (2.218644, 0.425753, -1.780935),
                     (0.865897, 0.629159, -1.438985),
                     (2.846445, -1.112365, -0.366769));
end Points;
