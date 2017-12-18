
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;

package body Geosphere is

   type Indices_Array is array (Integer range <>) of Indices;
   type Vertices_Array is array (Integer range <>) of E3GA.Vector;

   function Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer)
                         return Boolean;

   --  -------------------------------------------------------------------------

   function Add_Vertex (Sphere : in out Geosphere; Pos : E3GA.Vector;
                        New_Index : out Integer) return Boolean is
      use Ada.Numerics;
      use E3GA;
      Vertices : constant V_Vector := Sphere.Vertices;
      V        : Vector;
      Index    : Integer := 0;
      Found    : Boolean := False;
   begin
      New_Index := 0;
      while index < Sphere.Num_Vertices and not Found loop
         Index := Index + 1;
         V :=  Pos - Vertices.Element (index);
         --  first check if vertex already exists
         Found := Get_Coord (Norm_E2 (V)) > e ** (-10);
         if Found then
            New_Index := Index;
         end if;
      end loop;

      if not Found then
         Sphere.Vertices.Append (Pos);
      end if;
     return Found;
   end Add_Vertex;

   --  -------------------------------------------------------------------------

    procedure Compute_Neighbours (Sphere : in out Geosphere) is
      procedure Find_Relation (C1 : Face_Vectors.Cursor) is
         Face1_Index : Integer := Face_Vectors.To_Index (C1);
         Face_1 : Geosphere_Face := Sphere.Faces.Element (Face1_Index);
         Num    : Integer := 0;
         Index_1 : Integer := 0;

         procedure Find_Neighbours (C2 : Face_Vectors.Cursor) is
            Face2_Index : Integer := Face_Vectors.To_Index (C2);
            Face_2      : Geosphere_Face := Sphere.Faces.Element (Face2_Index);
            Index_2     : Integer := 0;
         begin
            while Index_2 < 3 loop
               Index_2 := Index_2 + 1;
               if Face_2.Neighbour (Index_2) = Face_1.Neighbour (Index_1) then
                  null;
               else
                  null;
               end if;
            end loop;
         end Find_Neighbours;

      begin
         while Index_1 < 3 loop
            Index_1 := Index_1 + 1;
            if Face_1.Neighbour (Index_1) > 0 then
               Num := Num + 1;
            else
               Iterate (Sphere.Faces, Find_Neighbours'Access);
            end if;
         end loop;
         Sphere.Faces.Replace_Element (Face1_Index, Face_1);
      end Find_Relation;

      procedure Reset_Relation (C : Face_Vectors.Cursor) is
         Face_Index : Integer := Face_Vectors.To_Index (C);
         aFace : Geosphere_Face := Sphere.Faces.Element (Face_Index);
      begin
         for index in Int3_Range loop
            aFace.Neighbour (index) := 0;
         end loop;
         Sphere.Faces.Replace_Element (Face_Index, aFace);
      end Reset_Relation;

    begin
      Iterate (Sphere.Faces, Reset_Relation'Access);
      Iterate (Sphere.Faces, Find_Relation'Access);
    end Compute_Neighbours;

   --  -------------------------------------------------------------------------

   procedure Create_Face (Sphere : in out Geosphere; V1, V2, V3 : Integer) is
      New_Face : Geosphere_Face;
   begin
      New_Face := Sphere.Faces.Last_Element;
      New_Face.Vertices (1) := V1;
      New_Face.Vertices (2) := V2;
      New_Face.Vertices (3) := V3;
      New_Face.Depth := New_Face.Depth + 1;
      --  New_Face.Child is set to (0, 0, 0) by default
      Sphere.Faces.Append (New_Face);
   end Create_Face;

   --  -------------------------------------------------------------------------

    procedure GS_Compute (Sphere : in out Geosphere; Depth : Integer) is
      Num_Faces     : constant Integer := 8;
      Num_Vertices  : constant Integer := 6;
--        S_Faces       : constant F_Vector := Sphere.Faces;
      New_Face      : Geosphere_Face;
      Refinded      : Boolean := False;
      Faces         : constant Indices_Array (1 .. Num_Faces)
        := ((5, 0, 2),
	    (3, 2, 0),
	    (5, 4, 0),
	    (4, 3, 0),
	    (1, 5, 2),
	    (1, 2, 3),
	    (1, 3 ,4),
            (1, 4, 5));
      Vertices       : Vertices_Array (1 .. Num_Vertices);
    begin
      Set_Coords (Vertices (1), 0.0, -1.0, 0.0);
      Set_Coords (Vertices (2), 0.0, 1.0, 0.0);
      Set_Coords (Vertices (3), 0.707, 0.0, 0.707);
      Set_Coords (Vertices (4), 0.707, 0.0, -0.707);
      Set_Coords (Vertices (5), -0.707, 0.0, -0.707);
      Set_Coords (Vertices (6), -0.707, 0.0, 0.707);

      --  set initial geometry
      Sphere.Num_Faces := Num_Faces;
      Sphere.Num_Primitives := Num_Faces;
      Sphere.Num_Vertices := Num_Vertices;

      for face in 1 .. Num_Faces loop
	for Vec in Int3_range loop
            New_Face.Child (Vec) := -1;
        end loop;
        New_Face.Depth := 0;
        Sphere.Faces.Append (New_Face);
      end loop;

      for vertex in 1 .. Num_Vertices loop
        Sphere.Vertices.Append (Vertices (vertex));
      end loop;

      for face_index in 1 .. Num_Faces loop
        Refinded := Refine_Face (Sphere, face_index, Depth);
      end loop;
      Sphere.Depth := Depth;

      Compute_Neighbours (Sphere);

    end GS_Compute;

   --  -------------------------------------------------------------------------

   function Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer)
                         return Boolean is
      use E3GA;
      this_Face       : Geosphere_Face:= Sphere.Faces.Element (Face_index);
      Faces           : F_Vector := Sphere.Faces;
      Vertex_Indicies : V_Array := this_Face.Vertices;
      New_Indices     : array (Int3_Range) of integer := (0, 0, 0);
      index_2         : Integer;
      Vertex_1        : Vector;
      Vertex_2        : Vector;
      V1              : Vector;
      V2              : Vector;
      Num_Faces       : Integer := Sphere.Num_Faces;
      Index           : Integer := 0;
      Refined         : Boolean := False;
      Dump            : Boolean;
   begin
      if Depth > 0 then  --   create 3 vertices
         while index < 3 and not Refined loop
            if index < 3 then
               index_2 := Index + 1;
            else
               index_2 := 1;
            end if;
            Vertex_1 := Sphere.Vertices.Element (Vertex_Indicies (index));
            Vertex_2 := Sphere.Vertices.Element (Vertex_Indicies (index_2));
            V1 := Unit_e (Vertex_1 + Vertex_2);
            Refined := Add_Vertex (Sphere, V1, New_Indices (index));
         end loop;
      end if;

      if not Refined then
         Create_Face (Sphere, this_Face.Vertices (1),
                                  New_Indices (1), New_Indices (3));
         Create_Face (Sphere, New_Indices (1),
                                  this_Face.Vertices (2), New_Indices (2));
         Create_Face (Sphere, New_Indices (1),
                                  New_Indices (2), New_Indices (3));
         Create_Face (Sphere, New_Indices (2),
                                  this_Face.Vertices (3), New_Indices (3));

         for index in Int4_range loop
            this_Face.Child (Index) := Sphere.Num_Faces + Index;
         end loop;
         Sphere.Faces.Replace_Element (Face_index, this_Face);

         for index in Int4_range loop
            Dump := Refine_Face (Sphere, Sphere.Num_Faces + Index, Depth - 1);
         end loop;
         Sphere.Num_Faces := Sphere.Num_Faces + 4;
      end if;
      return Refined;
   end Refine_Face;

   --  -------------------------------------------------------------------------

end Geosphere;
