
with GL.Types;

with E2GA;

package Points is
   Num_Points : GL.Types.Int := 6;

   Point_Data : GL.Types.Singles.Vector3_Array (1 .. Num_Points) :=
                    (((- 0.356756 * E2GA.e1 - 0.881980 * E2GA.e2), 0.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0));

end Points;
