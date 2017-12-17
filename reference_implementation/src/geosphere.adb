
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;

package body Geosphere is

   function Ensure_Vertices (Sphere : in out Geosphere;
                             New_Index : Integer) return Boolean;
   procedure Refine_Face (Sphere : Geosphere; Face, Depth : Integer);

   --  -------------------------------------------------------------------------

   function Add_Vertex (Sphere : in out Geosphere; Pos : E3GA.Vector;
                        V_Index : out Integer) return Boolean is
      use Ada.Numerics;
      use E3GA;
      Coords : Vector_Coords_3D;
      V      : Vector;
      Found  : Boolean := False;
   begin
      V_Index := 0;
      for index in 1 .. Sphere.Num_Vertices loop
         Coords := Sphere.Vertices (index);
         Set_Coords (V, Coords (1), Coords (2), Coords (3));
         V := Pos - V;
         --  first check if vertex already exists
         Found := Get_Coord (Norm_E2 (V)) > e ** (-10);
         if Found then
            V_Index := index;
            exit;
         end if;
      end loop;
      if not Found and then
         Sphere.Num_Vertices + 1 <= Sphere.Max_Vertices then  --  create new vertex
         Sphere.Num_Vertices := Sphere.Num_Vertices  + 1;
         Sphere.Vertices (Sphere.Num_Vertices) (1) := Get_Coord_1 (Pos);
         Sphere.Vertices (Sphere.Num_Vertices) (2) := Get_Coord_2 (Pos);
         Sphere.Vertices (Sphere.Num_Vertices) (3) := Get_Coord_3 (Pos);
         V_Index := Sphere.Num_Vertices;
      end if;
     return Found;
   end Add_Vertex;

   --  -------------------------------------------------------------------------

   procedure  Copy_Sphere (Sphere : Geosphere; New_Sphere : in out Geosphere) is

   begin
      if New_Sphere.Max_Vertices >= Sphere.Max_Vertices or
         New_Sphere.Max_Faces >= Sphere.Max_Faces then
         New_Sphere.Num_Vertices := Sphere.Num_Vertices;
         New_Sphere.Num_Faces := Sphere.Num_Faces;
         New_Sphere.Num_Primitives := Sphere.Num_Primitives;
         New_Sphere.Depth := Sphere.Depth;
         New_Sphere.Vertices := Sphere.Vertices;
         New_Sphere.Faces := Sphere.Faces;
      else
         Put_Line ("Copy_Sphere, New_Sphere is too small.");
         raise Copy_Error;
      end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Copy_Sphere.");
            raise;
   end Copy_Sphere;

   --  -------------------------------------------------------------------------

   function Ensure_Vertices (Sphere : in out Geosphere;
                             New_Index : Integer) return Boolean is
      OK         : Boolean;
   begin
      OK := New_Index <= Sphere.Max_Vertices;
      if not OK then
            if 4 * Sphere.Max_Vertices > New_Index then
               declare
                  New_Sphere : Geosphere (4 * Sphere.Max_Vertices, Sphere.Max_Faces);
               begin
                  Copy_Sphere (Sphere, New_Sphere);
               end;
            else
               declare
                  New_Sphere : Geosphere (New_Index, Sphere.Max_Faces);
               begin
                  null;
               end;
            end if;
      end if;
      return OK;
   end Ensure_Vertices;

   --  -------------------------------------------------------------------------

    procedure GS_Compute (Sphere : Geosphere; Depth : Integer) is
      Num_Faces     : constant Integer := 8;
      Num_Vertices  : constant Integer := 6;
      New_Sphere    : Geosphere (Num_Vertices, Num_Faces);
      Faces         : constant Indices_Array (1 .. Num_Faces)
        := ((5, 0, 2),
	    (3, 2, 0),
	    (5, 4, 0),
	    (4, 3, 0),
	    (1, 5, 2),
	    (1, 2, 3),
	    (1, 3 ,4),
            (1, 4, 5));
      Vertices       : constant Vertices_Array (1 .. Num_Vertices)
        := ((0.0, -1.0, 0.0),
	    (0.0, 1.0, 0.0),
	    (0.707, 0.0, 0.707),
	    (0.707, 0.0, -0.707),
	    (-0.707, 0.0, -0.707),
	    (-0.707, 0.0, 0.707));

    begin
      --  set initial geometry
      New_Sphere.Num_Faces := Num_Faces;
      New_Sphere.Num_Primitives := Num_Faces;
      New_Sphere.Num_Vertices := Num_Vertices;

      for face in 1 .. Num_Faces loop
        New_Sphere.Faces (face).Vertex_Indicies := Faces (face);
	for Vec in Int3_range loop
            New_Sphere.Faces (face).Child (Vec) := -1;
            New_Sphere.Faces (face).Depth := 0;
        end loop;
      end loop;

      for vertex in 1 .. Num_Vertices loop
        New_Sphere.Vertices (vertex) := Vertices (vertex);
      end loop;

      for face in 1 .. Num_Faces loop
        Refine_Face (New_Sphere, face, Depth);
      end loop;

    end GS_Compute;

   --  -------------------------------------------------------------------------

   procedure Refine_Face (Sphere : Geosphere; Face, Depth : Integer) is
      use E3GA;
      theFace         : Geosphere_Face:= Sphere.Faces (Face);
      aFace           : Geosphere_Face;
      Vertex_Indicies : Indices := theFace.Vertex_Indicies;
      New_Vertex      : Indices;
      index_2         : Integer;
      Coords_1        : Vector_Coords_3D;
      Coords_2        : Vector_Coords_3D;
      Vertex_1        : Vector;
      Vertex_2        : Vector;
      V1              : Vector;
      V2              : Vector;
      Num_Faces       : Integer := Sphere.Num_Faces;
   begin
      if Depth > 0 then  --   create 3 vertices
         for index in 1 .. 3 loop
            if index < 3 then
               index_2 := Index + 1;
            else
               index_2 := 1;
            end if;
            Coords_1 := Sphere.Vertices (Vertex_Indicies (index));
            Coords_2 := Sphere.Vertices (Vertex_Indicies (index_2));
            Set_Coords (Vertex_1, Coords_1 (1), Coords_1 (2), Coords_1 (3));
            Set_Coords (Vertex_2, Coords_2 (1), Coords_2 (2), Coords_2 (3));
            V1 := Unit_e (Vertex_1 + Vertex_2);
         end loop;
      end if;

   end Refine_Face;

   --  -------------------------------------------------------------------------

end Geosphere;
