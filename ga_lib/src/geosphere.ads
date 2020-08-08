
with Ada.Containers.Vectors;

with GL.Objects.Programs;
with GL.Types;

with GA_Maths;
with Multivectors;

package Geosphere is
   --  some very ancient code to compute a triangulated sphere
   use Multivectors;

   type Geosphere is private;
   type Indices is array (1 .. 3) of integer;

   Geosphere_Exception : Exception;

   procedure Add_To_Sphere_List (Sphere : Geosphere);
   procedure Draw_Sphere_List (Render_Program : GL.Objects.Programs.Program;
                               MV_Matrix : GL.Types.Singles.Matrix4;
                               Normal : GL.Types.Single := 0.0);
   procedure GS_Compute (Sphere : in out Geosphere; Depth : integer);
   procedure GS_Draw (Render_Program : GL.Objects.Programs.Program;
                      Model_View_Matrix : GL.Types.Singles.Matrix4;
                      Sphere : Geosphere; Normal : GL.Types.Single := 0.0);
   procedure New_Sphere_List (Sphere : Geosphere);

private
   subtype Int3_Range is Integer range 1 .. 3;
   subtype Int4_Range is Integer range 1 .. 4;

   subtype Contour_Intersect_Array is GA_Maths.Integer_Array (Int3_Range);
   subtype Contour_Visited_Array is GA_Maths.Integer_Array (Int3_Range);
   subtype Neighbour_Array is GA_Maths.Integer_Array (Int3_Range);
   subtype Indices_Vector is GA_Maths.Integer_Array (Int3_Range);

   type Child_Array is array (Int4_Range) of Integer;

   type Geosphere_Face is record
      Indices           : Indices_Vector := (-1, -1, -1);  --  Three indices into Vertices vector
      Child             : Child_Array := (-1, -1, -1, -1);
      Plane             : Multivectors.Bivector;
      D                 : float := 0.0;
      Depth             : integer := 0;
      Neighbour         : Neighbour_Array := (-1, -1, -1);
      Contour_Intersect : Contour_Intersect_Array := (-1, -1, -1);
      Contour_Visited   : Contour_Visited_Array := (-1, -1, -1);
   end record;

   package Vertex_Vectors is new Ada.Containers.Vectors
     (Element_Type => Multivectors.M_Vector, Index_Type => Natural);
   type MV_Vector is new Vertex_Vectors.Vector with null record;

   package Face_Vectors is new Ada.Containers.Vectors
     (Element_Type => Geosphere_Face, Index_Type => Positive);
   type F_Vector is new Face_Vectors.Vector with null record;

   type Geosphere is record
      Depth      : integer := 0;
      Vertices   : MV_Vector;
      Faces      : F_Vector;
   end record;

end Geosphere;
