
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;

with Maths;
with Utilities;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Windows;

with Blade;
with C3GA;
with GA_Draw;
with GL_Util;

package body Geosphere is

   package Sphere_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Geosphere);
   type Sphere_DL_List is new Sphere_List_Package.List with null record;

   type Indices_Array is array (Integer range <>) of Indices;
   type Vertices_Array is array (Integer range <>) of Vector;

   Sphere_List : Sphere_DL_List;

   function Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer)
                         return Boolean;

   --  -------------------------------------------------------------------------

   procedure Add_To_Sphere_List (Sphere : Geosphere) is
   begin
      Sphere_List.Append (Sphere);
   end Add_To_Sphere_List;

   --  -------------------------------------------------------------------------

   function Add_Vertex (Sphere : in out Geosphere; Pos : Vector;
                        New_Index : out Integer) return Boolean is
      use Ada.Numerics;
      Vertices : constant V_Vector := Sphere.Vertices;
      V        : Vector;
      Index    : Integer := 0;
      Found    : Boolean := False;
   begin
      --  first check if vertex already exists
      New_Index := 0;
      while index <= Sphere.Vertices.Last_Index and not Found loop
         Index := Index + 1;
         V :=  Pos - Vertices.Element (index);
         --  first check if vertex already exists
         Found := Norm_E2 (V) > e ** (-10);
      end loop;

      if not Found then  --  reate new vertex
         Sphere.Vertices.Append (Pos);
      end if;
      return Found;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Add_Vertex.");
         raise;
   end Add_Vertex;

   --  -------------------------------------------------------------------------

   procedure Compute_Neighbours (Sphere : in out Geosphere) is

      procedure Find_Relation (C1 : Face_Vectors.Cursor) is
         Face1_Index : Integer := Face_Vectors.To_Index (C1);  --  f
         Face_1      : Geosphere_Face := Sphere.Faces.Element (Face1_Index);
         Num         : Integer := 0;
         Index_1     : Integer := 0;  --  e

         --  -------------------------------------------------------------------

         procedure Find_Neighbours (C2 : Face_Vectors.Cursor) is
            Face2_Index : Integer := Face_Vectors.To_Index (C2);  --  i
            Face_2      : Geosphere_Face := Sphere.Faces.Element (Face2_Index);
            Index_2     : Integer := 0;  --  j
            Index_11    : Integer;
            Index_21    : Integer;
            Index_22    : Integer;
         begin
            while Index_2 < 3 loop
               Index_2 := Index_2 + 1;   --  j
               if Face_1.Vertex_Indices (Index_1) = Face_2.Vertex_Indices (Index_2) then
                  Index_11 :=  (Index_1 + 1) mod 3 + 1;  --  e + 1 mod 3
                  Index_21 :=  (Index_2 + 1) mod 3 + 1;  --  i + 1 mod 3
                  Index_22 :=  (Index_2 + 2) mod 3 + 1;  --  i + 2 mod 3
                  if Face_1.Vertex_Indices (Index_11) = Face_2.Vertex_Indices (Index_21) then
                     --  face[f].neighbour[e] = i;
                     --  face[i].neighbour[j] = f;
                     Face_1.Neighbour (Index_1) := Face2_Index;
                     Face_2.Neighbour (Index_2) := Face1_Index;
                     Num := Num + 1;
                  elsif Face_1.Vertex_Indices (Index_11) = Face_2.Vertex_Indices (Index_22) then
                     --  face[f].neighbour[e] = i
                     --  face[i].neighbour[(j-1+3)%3] = f;
                     Face_1.Neighbour (Index_1) := Face2_Index;
                     Face_2.Neighbour (Index_2) := Index_22;
                     Num := Num + 1;
                  end if;
               end if;
            end loop;

            Sphere.Faces.Replace_Element (Face2_Index, Face_2);
         exception
            when anError :  others =>
               Put_Line ("An exception occurred in Geosphere.Find_Neighbours.");
               raise;

         end Find_Neighbours;

         --  -------------------------------------------------------------------

      begin
         while Index_1 < 3 loop
            Index_1 := Index_1 + 1;  --  e
            if Face_1.Neighbour (Index_1) > 0 then
               Num := Num + 1;
            else
               Iterate (Sphere.Faces, Find_Neighbours'Access);
            end if;
         end loop;
         Sphere.Faces.Replace_Element (Face1_Index, Face_1);

         if Num /= 3 then
            Put_Line ("Geosphere.Find_Neighbours found only " & Integer'Image (Num)
                      & " neighbours of  face "  & Integer'Image (Face1_Index));
         end if;
      end Find_Relation;

      --  ---------------------------------------------------------------------

      procedure New_Sphere_List (Sphere : Geosphere) is
      begin
         Sphere_List.Clear;
         Sphere_List.Append (Sphere);
      end New_Sphere_List;

      --  ---------------------------------------------------------------------

      procedure Reset_Relation (C : Face_Vectors.Cursor) is
         Face_Index : Integer := Face_Vectors.To_Index (C);
         aFace : Geosphere_Face := Sphere.Faces.Element (Face_Index);
      begin
         for index in Int3_Range loop
            aFace.Neighbour (index) := 0;
         end loop;
         Sphere.Faces.Replace_Element (Face_Index, aFace);
      end Reset_Relation;

      --  ---------------------------------------------------------------------

   begin
      Iterate (Sphere.Faces, Reset_Relation'Access);
      Iterate (Sphere.Faces, Find_Relation'Access);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Compute_Neighbours.");
         raise;
   end Compute_Neighbours;

   --  -------------------------------------------------------------------------

   procedure Draw_Sphere_List (Render_Program : GL.Objects.Programs.Program;
                               MV_Matrix : GL.Types.Singles.Matrix4;
                               Colour : GL.Types.Colors.Color;
                               Normal : GL.Types.Single := 0.0) is
      use Sphere_List_Package;
      Curs : Cursor := Sphere_List.First;
   begin
      while Has_Element (Curs) loop
         GS_Draw (Render_Program, MV_Matrix, Element (Curs), Normal, Colour);
         Next (Curs);
      end loop;
   end Draw_Sphere_List;

   --  -------------------------------------------------------------------------

   procedure Create_Face (Sphere : in out Geosphere; V1, V2, V3 : Integer) is
      New_Face : Geosphere_Face;
   begin
      New_Face := Sphere.Faces.Last_Element;
      New_Face.Vertex_Indices (1) := V1;
      New_Face.Vertex_Indices (2) := V2;
      New_Face.Vertex_Indices (3) := V3;
      New_Face.Depth := New_Face.Depth + 1;
      --  New_Face.Child is set to (0, 0, 0) by default
      Sphere.Faces.Append (New_Face);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Create_Face.");
         raise;
   end Create_Face;

   --  -------------------------------------------------------------------------

   function Get_Vertex (Sphere : Geosphere; aFace : Geosphere_Face;
                        Vertex_Index : Positive) return GL.Types.Singles.Vector3 is
      use GL.Types;
      theVertex    : Singles.Vector3;
      GA_Vector    : Multivectors.Vector :=
        Sphere.Vertices.Element (Vertex_Index);
   begin
      theVertex (GL.X) := Single (C3GA.e1 (GA_Vector));
      theVertex (GL.Y) := Single (C3GA.e2 (GA_Vector));
      theVertex (GL.Z) := Single (C3GA.e3 (GA_Vector));
      return theVertex;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices 1.");
         raise;
   end Get_Vertex;

   --  -------------------------------------------------------------------------

   function Get_Vertex (Sphere : Geosphere; aFace : Geosphere_Face;
                        Vertex_Index : Positive) return Multivectors.Vector is
       GA_Vector    : Multivectors.Vector :=
        Sphere.Vertices.Element (Vertex_Index);
   begin
      return GA_Vector;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices 2.");
         raise;
   end Get_Vertex;

   --  -------------------------------------------------------------------------

   procedure Get_Vertices (Sphere : Geosphere; aFace : Geosphere_Face;
                           Vertices : in out GL.Types.Singles.Vector3_Array) is
      use GL.Types;
      Indices      : V_Array := aFace.Vertex_Indices;
      Vertex_Index : Positive;
   begin
      for index in Positive range 1 .. 3 loop
         Vertex_Index := Indices (index);
         Vertices (Int (index)) :=
           GL_Util.To_GL (Sphere.Vertices.Element (Vertex_Index));
      end loop;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices.");
         raise;
   end Get_Vertices;

   --  -------------------------------------------------------------------------

   procedure GS_Compute (Sphere : in out Geosphere; Depth : Integer) is
      Num_Faces     : constant Integer := 8;
      Num_Vertices  : constant Integer := 6;
      --        S_Faces       : constant F_Vector := Sphere.Faces;
      New_Face      : Geosphere_Face;
      Refinded      : Boolean := False;
      Faces         : constant Indices_Array (1 .. Num_Faces)
        := ((6, 1, 3),
            (4, 3, 1),
            (6, 5, 1),
            (5, 4, 1),
            (2, 6, 3),
            (2, 3, 4),
            (2, 4 ,5),
            (2, 5, 6));
      Vertices       : Vertices_Array (1 .. Num_Vertices);  --  array of V_Vector
   begin
      Vertices (1) := New_Vector (0.0, -1.0, 0.0);
      Vertices (2) := New_Vector (0.0, 1.0, 0.0);
      Vertices (3) := New_Vector (0.707, 0.0, 0.707);
      Vertices (4) := New_Vector (0.707, 0.0, -0.707);
      Vertices (5) := New_Vector (-0.707, 0.0, -0.707);
      Vertices (6) := New_Vector (-0.707, 0.0, 0.707);

      --  set initial geometry
--        Sphere.Num_Faces := Num_Faces;
--        Sphere.Num_Primitives := Num_Faces;
--        Sphere.Num_Vertices := Num_Vertices;

      for face in 1 .. Num_Faces loop
         New_Face.Vertex_Indices := V_Array (Faces (face));
         for index in Int4_range loop
            New_Face.Child (index) := 0;
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
      Sphere.isNull := False;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.GS_Compute.");
         raise;

   end GS_Compute;

   --  -------------------------------------------------------------------------
   --  Based on geosphere.cpp
   --  gsDraw(geosphere * sphere, mv::Float normal /*= 0.0*/)
   procedure GS_Draw (Render_Program : GL.Objects.Programs.Program;
                      MV_Matrix : GL.Types.Singles.Matrix4;
                      Sphere : Geosphere; Normal : GL.Types.Single := 0.0;
                      Colour : GL.Types.Colors.Color) is
      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Colors;
      use GL.Types.Singles;

      Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Model_View_Matrix    : Singles.Matrix4 := Singles.Identity4;
      Proj_Matrix          : GL.Types.Singles.Matrix4;

      --  gsDraw(geosphere * sphere, int f, mv::Float normal = 0.0)
      --  geosphere * sphere, int f is Face_Vectors.Cursor
      procedure Draw (C : Face_Vectors.Cursor) is
         Face_Index   : Integer := Face_Vectors.To_Index (C);
         thisFace     : Geosphere_Face := Sphere.Faces.Element (Face_Index);
         Vertex_Buffer   : GL.Objects.Buffers.Buffer;
         Num_Vertices    : GL.Types.Int :=
           GL.Types.Int (thisFace.Vertex_Indices'Last);

         procedure Draw (Face_Index : Integer) is
            thisFace : Geosphere_Face := Sphere.Faces.Element (Face_Index);
         begin
            null;
         end Draw;

         Vertices : Singles.Vector3_Array (1 .. 3);
         Lines    : Singles.Vector3_Array (1 .. 6);
         V1       : Singles.Vector3;
         V1_MV    : Multivectors.Vector;
      begin
         if thisFace.Child (1) > 0 then
            Put_Line ("Geosphere.GS_Draw has child");
            for index in 1 .. 4 loop
               Draw (thisFace.Child (index));
            end loop;
         else  --  no children
            --  Draw this face's triangle
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);

            Get_Vertices (Sphere, thisFace, Vertices);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

--              Put_Line ("Geosphere.GS_Draw face index " & Integer'Image (Face_Index));
--              Utilities.Print_GL_Array3 ("Number of vertices: " &
--                          GL.Types.Int'Image (Num_Vertices), Vertices);

            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);

            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => GL.Types.Triangles,
                                                  First => 0,
                                                  Count => 3);
            GL.Attributes.Disable_Vertex_Attrib_Array (0);

            if Normal /= 0.0 then
               --  Draw three lines
               for index in 1 .. 3 loop
                  V1_MV := Unit_E (Get_Vertex (Sphere, thisFace, index));
                  V1 := GL_Util.To_GL (V1_MV);
                  Lines (2 * Int (index - 1) + 1) := Get_Vertex (Sphere, thisFace, index);
                  Lines (2 * Int (index - 1) + 2) :=
                    Get_Vertex (Sphere, thisFace, index) + V1 * Normal;
               end loop;

               Utilities.Load_Vertex_Buffer (Array_Buffer, Lines, Static_Draw);
               GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, 0, 0);
               GL.Attributes.Enable_Vertex_Attrib_Array (0);

               GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => GL.Types.Lines,
                                                     First => 0,
                                                     Count => 2);
               GL.Attributes.Disable_Vertex_Attrib_Array (0);
            end if;
         end if;
      end Draw;

   begin
      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;

      GL.Objects.Programs.Use_Program (Render_Program);
      GA_Draw.Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                        Projection_Matrix_ID, Colour_Location);
      Model_View_Matrix := Maths.Scaling_Matrix (0.5) * MV_Matrix;
      GA_Draw.Init_Projection_Matrix (Proj_Matrix);
      Proj_Matrix := Maths.Translation_Matrix ((1.0, -1.0, 0.0)) *
        Proj_Matrix;
      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Proj_Matrix);
      GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

--        GL.Toggles.Disable (GL.Toggles.Cull_Face);
--        Draw (Sphere.Faces.First_Element);

      --  Implement for (i = 0; i < sphere->nbPrimitives; i++)
      --                 gsDraw(sphere, i, normal);
      Utilities.Print_Matrix ("Geosphere.GS_Draw Model_View_Matrix", Model_View_Matrix);
--        Utilities.Print_Matrix ("Geosphere.GS_Draw Proj_Matrix", Proj_Matrix);
--        Utilities.Print_Matrix ("Geosphere.GS_Draw MVP Matrix", Proj_Matrix * Model_View_Matrix);
      Iterate (Sphere.Faces, Draw'Access);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.GS_Draw.");
         raise;
   end GS_Draw;

   --  -------------------------------------------------------------------------

   procedure New_Sphere_List (Sphere : Geosphere) is
   begin
      Sphere_List.Clear;
      Sphere_List.Append (Sphere);
   end New_Sphere_List;

   --  -------------------------------------------------------------------------

   function Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer)
                         return Boolean is
      Faces           : F_Vector := Sphere.Faces;
      this_Face       : Geosphere_Face:= Faces.Element (Face_index);
      Vertex_Indicies : V_Array := this_Face.Vertex_Indices;
      New_Indices     : array (Int3_Range) of integer := (0, 0, 0);
      index_2         : Integer;
      Vertex_1        : Vector;
      Vertex_2        : Vector;
      V1              : Vector;
      V2              : Vector;
      Index           : Integer := 0;
      Refined         : Boolean := False;
      Dump            : Boolean;
   begin
      if Depth > 0 then  --   create 3 vertices
         while index < 3 and not Refined loop
            index := index + 1;

            if index < 3 then
               index_2 := Index + 1;
            else
               index_2 := 1;
            end if;
--              Put_Line ("Face_index: "&  Integer'Image (Face_index));
--              Put ("Geosphere.Refine_Face index: " &  Integer'Image (index));
--              Put_Line (" vertice: "&  Integer'Image (Vertex_Indicies (index)));
            Vertex_1 := Sphere.Vertices.Element (Vertex_Indicies (index));
            Vertex_2 := Sphere.Vertices.Element (Vertex_Indicies (index_2));
            V1 := Unit_e (Vertex_1 + Vertex_2);
            Refined := Add_Vertex (Sphere, V1, New_Indices (index));
         end loop;
      end if;

      if not Refined then
         Create_Face (Sphere, this_Face.Vertex_Indices (1),
                      New_Indices (1), New_Indices (3));
         Create_Face (Sphere, New_Indices (1),
                      this_Face.Vertex_Indices (2), New_Indices (2));
         Create_Face (Sphere, New_Indices (1),
                      New_Indices (2), New_Indices (3));
         Create_Face (Sphere, New_Indices (2),
                      this_Face.Vertex_Indices (3), New_Indices (3));

         for index in Int4_range loop
            this_Face.Child (Index) := Sphere.Faces.Last_Index + Index;
         end loop;
         Sphere.Faces.Replace_Element (Face_index, this_Face);

         for index in Int4_range loop
            Dump := Refine_Face (Sphere, Sphere.Faces.Last_Index + Index, Depth - 1);
         end loop;
      end if;
      return Refined;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Geosphere.Refine_Face.");
         raise;
   end Refine_Face;

   --  -------------------------------------------------------------------------

   function Sphere_State_Null (Sphere : Geosphere) return Boolean is
   begin
      return Sphere.isNull;
   end Sphere_State_Null;

   --  -------------------------------------------------------------------------

end Geosphere;
