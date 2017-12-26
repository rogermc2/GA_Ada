
with Ada.Containers.Vectors;

with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

with E3GA;

package Geosphere is
   use E3GA;
   --  some very ancient code to compute a triangulated sphere

   type Geosphere is private;
   type Geosphere_Face is private;
   type Indices is array (1 .. 3) of integer;

   package Vertex_Vectors is new Ada.Containers.Vectors
     (Element_Type => E3GA.Vector, Index_Type => Positive);
   type V_Vector is new Vertex_Vectors.Vector with null record;

    procedure GS_Compute (Sphere : in out Geosphere; Depth : integer);
    procedure GS_Draw (Render_Program : GL.Objects.Programs.Program;
                       Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                       Sphere : Geosphere; Normal : GL.Types.Single;
                       Colour : GL.Types.Colors.Color);

private
   subtype Int3_Range is Integer range 1 .. 3;
   subtype Int4_Range is Integer range 1 .. 4;
    type Child_Array is array (Int4_Range) of integer;
    type Neighbour_Array is array  (Int3_Range)  of integer;
    type Contour_Intersect_Array is array  (Int3_Range)  of integer;
    type Contour_Visited_Array is array  (Int3_Range)  of integer;
    type V_Array is array  (Int3_Range) of integer;

    type Geosphere_Face is record
        Vertex_Indices    : V_Array;  --  Three indices into Vertices vector
        Child             : Child_Array := (0, 0, 0, 0);
        Plane             : E3GA.Bivector;
        D                 : float;
        Depth             : integer;
        Neighbour         : Neighbour_Array;
        Contour_Intersect : Contour_Intersect_Array;
        Contour_Visited   : Contour_Visited_Array;
    end record;

   package Face_Vectors is new Ada.Containers.Vectors
     (Element_Type => Geosphere_Face, Index_Type => Positive);
   type F_Vector is new Face_Vectors.Vector with null record;

   type Geosphere is record
--        Num_Vertices    : integer;
--        Num_Faces       : integer;
--        Num_Primitives  : integer;  --  Always = Num_Faces
      Depth           : integer;
      Vertices        : V_Vector;
      Faces           : F_Vector;
   end record;

end Geosphere;
