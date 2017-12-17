
with E3GA;

package Geosphere is
   --  some very ancient code to compute a triangulated sphere

    type Geosphere (Max_Vertices : Integer; Max_Faces : Integer) is private;
    type Geosphere_Face is private;

   Copy_Error  : exception;

    procedure GS_Compute (Sphere : Geosphere; Depth : Integer);

private
    subtype Int3_range is Integer range 1 .. 3;
    subtype Int4_range is Integer range 1 .. 4;
    type Child_Array is array (Int4_range) of Integer;
    type Indices is array (Int3_range) of Integer;
    type Indices_Array is array (Integer range <>) of Indices;
    type Vertices_Array is array (Integer range <>) of E3GA.Vector_Coords_3D;
    type Neighbour_Array is array (Int3_range) of Integer;
    type Contour_Intersect_Array is array (Int3_range) of Integer;
    type Contour_Visited_Array is array (Int3_range) of Integer;

    type Geosphere_Face is record
        Vertex_Indicies   : Indices;
        Child             : Child_Array;
        Plane             : E3GA.Bivector;
        D                 : float;
        Depth             : integer;
        Neighbour         : Neighbour_Array;
        Contour_Intersect : Contour_Intersect_Array;
        Contour_Visited   : Contour_Visited_Array;
    end record;
   type Geosphere_Face_Array is array (Integer range <>) of Geosphere_Face;

   type Geosphere (Max_Vertices : integer; Max_Faces : integer) is record
      Num_Vertices    : integer;
      Num_Faces       : integer;
      Num_Primitives  : integer;
      Depth           : integer;
      Vertices        : Vertices_Array (1 .. Max_Vertices);
      Faces           : Geosphere_Face_Array (1 .. Max_Faces);
   end record;

end Geosphere;
