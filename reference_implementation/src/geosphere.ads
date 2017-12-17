
with GL.Types;

with E3GA;

package Geosphere is
   --  some very ancient code to compute a triangulated sphere

    type Geosphere (Max_Vertices : GL.Types.Int; Max_Faces : GL.Types.Int) is private;
    type Geosphere_Face is private;

    procedure GS_Compute (Sphere : Geosphere; Depth : GL.Types.Int);

private
    type V_Array is array (1 .. 3) of integer;
    type Child_Array is array (1 .. 3) of integer;
    type Neighbour_Array is array (1 .. 3) of integer;
    type Contour_Intersect_Array is array (1 .. 3) of integer;
    type Contour_Visited_Array is array (1 .. 3) of integer;

    type Geosphere_Face is record
        V                 : V_Array;
        Child             : Child_Array;
        Plane             : E3GA.Bivector;
        D                 : float;
        Depth             : integer;
        Neighbour         : Neighbour_Array;
        Contour_Intersect : Contour_Intersect_Array;
        Contour_Visited   : Contour_Visited_Array;
    end record;

   type Geosphere (Max_Vertices : GL.Types.Int; Max_Faces : GL.Types.Int) is record
      Num_Vertices    : GL.Types.Int;
      Num_Faces       : GL.Types.Int;
      Num_Primitives  : GL.Types.Int;
      Depth           : GL.Types.Int;
      Vertices        : GL.Types.Singles.Vector3_Array (1 .. Max_Vertices);
      Faces           : GL.Types.Singles.Vector3_Array (1 .. Max_Faces);
   end record;

end Geosphere;
