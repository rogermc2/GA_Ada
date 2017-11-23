
with GL.Types.Singles;

package Points is
   Num_Points : Integer := 6;

   Point_Data : Singles.Vector3_Array (1 .. Num_Points) :=
                    ((-1.0, -1.0, -1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0));

end Points;
