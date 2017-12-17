
with Ada.Containers.Vectors;

with E3GA;

package Geosphere is

   --  some very ancient code to compute a triangulated sphere

   type Geosphere (Max_Vertices : integer; Max_Faces : integer) is private;
   type Geosphere_Face is private;

   type Geosphere_Access is access Geosphere;

   package Spherical_Vectors is new Ada.Containers.Vectors
     (Element_Type => Geosphere_Access, Index_Type => Positive);
   type S_Vector is new Spherical_Vectors.Vector with null record;

    procedure GS_Compute (Sphere : Geosphere; Depth : integer);

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

   type Geosphere (Max_Vertices : integer; Max_Faces : integer) is record
      Num_Vertices    : integer;
      Num_Faces       : integer;
      Num_Primitives  : integer;
      Depth           : integer;
      Vertices        : S_Vector;
      Faces           : S_Vector;
   end record;

end Geosphere;
