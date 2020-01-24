
with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;

--  with GA_Utilities;
with C3GA;
with E3GA;
with GL_Util;

with Shader_Manager;

package body Geosphere is

   package Sphere_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Geosphere);
   type Sphere_DL_List is new Sphere_List_Package.List with null record;

   type Indices_Array is array (Integer range <>) of Indices;
   type Vertices_Array is array (Integer range <>) of Multivectors.Vector;

   Num_Faces     : constant Positive := 8;
   Num_Vertices  : constant Positive := 6;
   Sphere_List   : Sphere_DL_List;

   procedure Get_Vertices (Sphere   : Geosphere; Face : Geosphere_Face;
                           Vertices : in out GL.Types.Singles.Vector3_Array);
   procedure Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Natural);

   --  -------------------------------------------------------------------------

   procedure Add_To_Sphere_List (Sphere : Geosphere) is
   begin
      Sphere_List.Append (Sphere);
   end Add_To_Sphere_List;

   --  -------------------------------------------------------------------------

   procedure Add_Vertex (Sphere    : in out Geosphere; Pos : Multivectors.Vector;
                         V_Index   : out Integer) is
      use Ada.Numerics;
      Vertices : constant MV_Vector := Sphere.Vertices;
      MV       : Multivectors.Vector;
      Index    : Integer := 0;
      Found    : Boolean := False;
   begin
      --  first check if vertex already exists
      while Index <= Sphere.Vertices.Last_Index and not Found loop
         MV :=  Pos - Vertices.Element (Index);
         Found := Norm_Esq (MV) < e ** (-10);
         if Found then
            V_Index := Index;
         end if;
         Index := Index + 1;
      end loop;

      if not Found then
         Sphere.Vertices.Append (Pos);
         V_Index := Sphere.Vertices.Last_Index;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Add_Vertex.");
         raise;
   end Add_Vertex;

   --  -------------------------------------------------------------------------

   procedure Compute_Neighbours (Sphere : in out Geosphere) is

      procedure Find_Relation (Face_Cursor_F : Face_Vectors.Cursor) is
         Index_F : constant Integer := Face_Vectors.To_Index (Face_Cursor_F);  --  f
         Face_F  : Geosphere_Face := Sphere.Faces.Element (Index_F);
         Num     : Integer := 0;

         --  -------------------------------------------------------------------
         --  Find_Neighbours finds the neighbour faces of vertex E of Face I
         procedure Find_Neighbours (Index_FI, Index_VE : Integer) is
            Face_I         : Geosphere_Face := Sphere.Faces.Element (Index_FI);
            FE_Index       : constant Integer := Face_F.Indices (Index_VE);
            --  e + 1 mod 3:
            Index_VE1      : integer := Index_VE + 1;
            Next_FE_Index  : Integer;
            Vertex_J1      : Integer;
            Vertex_J2      : Integer;
         begin
            if Index_VE1 > 3 then
               Index_VE1 := 1;
            end if;
            Next_FE_Index := Face_F.Indices (Index_VE1);
            --  For each vertex j of Face_I find neighbours for face Face_I and
            --  neighbour E of Face_F
--              Put_Line ("Geosphere.Find_Neighbours find matching vertices of Face "  &
--                         Integer'Image (Index_F) & " vertex "&
--                         Integer'Image (Index_VE));

            for Vertex_J in 1 .. 3 loop --  j
               if Face_I.Indices (Vertex_J) = FE_Index then
                  --    same vertices found
                  Put_Line ("Geosphere.Find_Neighbours matching vertices E and J found, E:"  &
                           Integer'Image (Index_VE) & "  J:" &
                           Integer'Image (Vertex_J) & " vertex E:" &
                           Integer'Image (Face_F.Indices (Index_VE)) & " vertex J:" &
                           Integer'Image (Face_F.Indices (Vertex_J)));
                  Vertex_J1 :=  Vertex_J + 1;  --  i + 1 mod 3
                  if Vertex_J1 > 3 then
                     Vertex_J1 := 1;
                  end if;
                  Vertex_J2 := Vertex_J1 + 1;  --  i + 2 mod 3
                  if Vertex_J2 > 3 then
                     Vertex_J2 := 1;
                  end if;
                  if  Face_I.Indices (Vertex_J1) = Next_FE_Index then
                     --  next vertices also match
                     Put_Line ("Geosphere.Find_Neighbours matching vertices E1 and J1 found." );
                     Face_F.Neighbour (Index_VE) := Index_FI;
                     Face_I.Neighbour (Vertex_J) := Index_F;
                     Num := Num + 1;
                  elsif Face_I.Indices (Vertex_J2) = Next_FE_Index then
                     Put_Line ("Geosphere.Find_Neighbours matching vertices E1 and J2 found." );
                     --  next vertex of face[f] matches preceding vertex of face[i]
                     Face_F.Neighbour (Index_VE) := Index_FI;
                     Face_I.Neighbour (Vertex_J2) := Index_F;
                     Num := Num + 1;
                  end if;
--                 else
--                    Put_Line ("Geosphere.Find_Neighbours; no matching vertices E and J"  &
--                              Integer'Image (Vertex_E) & Integer'Image (Vertex_J));
               end if;
            end loop;
--              Put_Line ("Geosphere.Find_Neighbours num vertices found." &
--                       Integer'Image (Num));
            Sphere.Faces.Replace_Element (Index_FI, Face_I);

         exception
            when others =>
               Put_Line ("An exception occurred in Geosphere.Find_Neighbours.");
               raise;

         end Find_Neighbours;

         --  -------------------------------------------------------------------

      begin  --  Find_Relation
--           Put_Line ("Geosphere.Compute_Neighbours.Find_Relation Face_Index (Index_F)" &
--                    Positive'Image (Index_F));
         for Index_E in 1 .. 3 loop  --  e
            if Face_F.Neighbour (Index_E) >= 0 then
              Put_Line ("Geosphere.Find_Relation, has relation: Face "  &
                             Integer'Image (Index_F) & " vertex " &
                          Integer'Image (Index_E));
              Put_Line ("Geosphere.Find_Relation, neighbour index "  &
                             Integer'Image (Face_F.Neighbour (Index_E)));
              Num := Num + 1;
            else
               Put_Line ("Geosphere.Find_Relation Find_Neighbours of Face "  &
                           Integer'Image (Index_F) & " vertex " &
                           Integer'Image (Index_E));
               for Index_I in Index_F + 1 .. Sphere.Faces.Last_Index loop
--                    Put_Line ("Geosphere.Find_Relation check Face "  &
--                              Integer'Image (Index_I));
                  Find_Neighbours (Index_I, Index_E);
               end loop;
            end if;
         end loop;
         Sphere.Faces.Replace_Element (Index_F, Face_F);

         if Num /= 3 then
            Put ("Geosphere.Compute_Neighbours found");
            if Num < 3 then
               Put (" only");
            end if;
            Put_Line (Integer'Image (Num) & " neighbours of face "  &
                      Integer'Image (Index_F));
            raise Geosphere_Exception;
         end if;
      end Find_Relation;

      --  ---------------------------------------------------------------------

      --        procedure New_Sphere_List (Sphere : Geosphere) is
      --        begin
      --           Sphere_List.Clear;
      --           Sphere_List.Append (Sphere);
      --        end New_Sphere_List;

      --  ---------------------------------------------------------------------

      procedure Reset_Relation (C : Face_Vectors.Cursor) is
         Face_Index : constant Integer := Face_Vectors.To_Index (C);
         aFace      : Geosphere_Face := Sphere.Faces.Element (Face_Index);
      begin
--           Put_Line ("Geosphere.Compute_Neighbours.Reset_Relation Face_Index" &
--                    Positive'Image (Face_Index));
         aFace.Neighbour := (-1, -1, -1);
         Sphere.Faces.Replace_Element (Face_Index, aFace);
      end Reset_Relation;

      --  ---------------------------------------------------------------------

   begin
      Iterate (Sphere.Faces, Reset_Relation'Access);
      Iterate (Sphere.Faces, Find_Relation'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Compute_Neighbours.");
         raise;
   end Compute_Neighbours;

   --  -------------------------------------------------------------------------

   procedure Create_Child (Sphere : in out Geosphere; Face_Index : Natural;
                           Child_Num, V1, V2, V3, Depth : Integer) is
      Parent_Face : Geosphere_Face := Sphere.Faces.Element (Face_Index);
      newFace     : Geosphere_Face;
   begin
      newFace.Indices (1) := V1;
      newFace.Indices (2) := V2;
      newFace.Indices (3) := V3;
      newFace.Depth := Depth + 1;
      Sphere.Faces.Append (newFace);
      Parent_Face.Child (Child_Num) := Sphere.Faces.Last_Index;
      Sphere.Faces.Replace_Element (Face_Index, Parent_Face);

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Create_Child.");
         raise;
   end Create_Child;

   --  -------------------------------------------------------------------------

   procedure Draw_Sphere_List (Render_Program : GL.Objects.Programs.Program;
                               MV_Matrix      : GL.Types.Singles.Matrix4;
                               Normal         : GL.Types.Single := 0.0) is
      use Sphere_List_Package;
      Curs : Cursor := Sphere_List.First;
   begin
      if Sphere_List.Is_Empty then
         Put_Line ("Geosphere.Draw_Sphere_List, Sphere_List is empty.");
      else
         while Has_Element (Curs) loop
            GS_Draw (Render_Program, MV_Matrix, Element (Curs), Normal);
            Next (Curs);
         end loop;
      end if;
   end Draw_Sphere_List;

   --  -------------------------------------------------------------------------

   function Get_Vertex (Sphere : Geosphere; Vertex_Index : Natural)
                         return GL.Types.Singles.Vector3 is
      use GL.Types;
      theVertex    : Singles.Vector3;
      GA_Vector    : constant Multivectors.Vector :=
                       Sphere.Vertices.Element (Vertex_Index);
   begin
      theVertex (GL.X) := Single (C3GA.e1 (GA_Vector));
      theVertex (GL.Y) := Single (C3GA.e2 (GA_Vector));
      theVertex (GL.Z) := Single (C3GA.e3 (GA_Vector));
      return theVertex;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices 1.");
         raise;
   end Get_Vertex;

   --  -------------------------------------------------------------------------

   function Get_Vertex (Sphere : Geosphere; Vertex_Index : Natural)
                         return Multivectors.Vector is
      GA_Vector : constant Multivectors.Vector :=
                    Sphere.Vertices.Element (Vertex_Index);
   begin
      Put_Line ("Geosphere.Get_Vertex Vertex_Index 2." & Natural'Image (Vertex_Index));
      return GA_Vector;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices 2.");
         raise;
   end Get_Vertex;

   --  -------------------------------------------------------------------------

   procedure Get_Indices (Sphere  : Geosphere;
                          Indices : in out GL.Types.UInt_Array) is
      use GL.Types;
      Indice_Index : Int := 0;

      procedure Add_Index (C : Face_Vectors.Cursor) is
         Face_Index     : constant Integer := Face_Vectors.To_Index (C);
         thisFace       : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
         Vertex_Indices : Indices_Vector;
      begin
         Vertex_Indices := thisFace.Indices;
         Indice_Index := Indice_Index + 1;
         Indices (Indice_Index) := UInt (Vertex_Indices (1));
         Indice_Index := Indice_Index + 1;
         Indices (Indice_Index) := UInt (Vertex_Indices (2));
         Indice_Index := Indice_Index + 1;
         Indices (Indice_Index) := UInt (Vertex_Indices (3));
      end Add_Index;

   begin
      Iterate (Sphere.Faces, Add_Index'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Get_Indices.");
         raise;
   end Get_Indices;

   --  -------------------------------------------------------------------------

   --      procedure Get_Normals (Sphere : Geosphere;
   --                             Normals : in out GL.Types.Singles.Vector3_Array) is
   --          use GL.Types;
   --          use GL.Types.Singles;
   --          Normal_Index : Integer := 0;
   --
   --          procedure Process_Face (C : Face_Vectors.Cursor) is
   --              Face_Index  : constant Integer := Face_Vectors.To_Index (C);
   --              thisFace    : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
   --              Vertices    : Vector3_Array (1 .. Int( Length (Sphere.Vertices)));
   --              Vertex_1    : Vector3;
   --              Vertex_2    : Vector3;
   --              Vertex_3    : Vector3;
   --              thisNormal  : Vector3;
   --          begin
   --              Normal_Index := Normal_Index + 1;
   --              Get_Vertices (Sphere, thisFace, Vertices);
   --              Vertex_1 := Vertices (Int (thisFace.Vertex_Indices (1)));
   --              Vertex_2 := Vertices (Int (thisFace.Vertex_Indices (2)));
   --              Vertex_3 := Vertices (Int (thisFace.Vertex_Indices (3)));
   --              thisNormal := Singles.Cross_Product ((Vertex_2 - Vertex_1), (Vertex_3 - Vertex_1));
   --              Normals (Int (Normal_Index)) := thisNormal;
   --          end Process_Face;
   --
   --      begin
   --          Iterate (Sphere.Faces, Process_Face'Access);
   --
   --      exception
   --          when others =>
   --              Put_Line ("An exception occurred in Geosphere.Get_Normals.");
   --              raise;
   --      end Get_Normals;

   --  -------------------------------------------------------------------------

   procedure Get_Normals (Sphere  : Geosphere; Face : Geosphere_Face;
                          Normals : in out GL.Types.Singles.Vector3_Array) is
      use GL.Types;
      Normal_Index : Integer := 0;

      thisNormal  : constant Indices_Vector := Face.Indices;
      aVertex     : GL.Types.Singles.Vector3;
   begin
      Normal_Index := Normal_Index + 1;
      aVertex := Get_Vertex (Sphere, thisNormal (1));
      Normals (Int (Normal_Index)) := aVertex;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Get_Normals.");
         raise;
   end Get_Normals;

   --  -------------------------------------------------------------------------

   procedure Get_Vertices (Sphere   : Geosphere;
                           Vertices : in out GL.Types.Singles.Vector3_Array) is
      use GL.Types;
      Index : Int := 0;

      procedure Add_Vertex (C : Vertex_Vectors.Cursor) is
         Vertex_Index     : constant Natural := Vertex_Vectors.To_Index (C);
         thisVertex       : constant Multivectors.Vector :=
                              Sphere.Vertices.Element (Vertex_Index);
      begin
         Index := Index + 1;
         Vertices (Index) := GL_Util.To_GL (thisVertex);
      end Add_Vertex;

   begin
      Iterate (Sphere.Vertices, Add_Vertex'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Get_Vertices.");
         raise;
   end Get_Vertices;

   --  -------------------------------------------------------------------------

   procedure Get_Vertices (Sphere   : Geosphere; Face : Geosphere_Face;
                           Vertices : in out GL.Types.Singles.Vector3_Array) is
   begin
      for index in Int3_Range loop
         Vertices (GL.Types.Int (index)) :=
           Get_Vertex (Sphere, Face.Indices (index));
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.Add_Vertices.");
         raise;
   end Get_Vertices;

   --  -------------------------------------------------------------------------

   procedure GL_Draw (Render_Program                : GL.Objects.Programs.Program;
                      Model_View_Matrix             : GL.Types.Singles.Matrix4;
                      Sphere                        : Geosphere; Normal    : GL.Types.Single;
                      Vertex_Buffer, Normals_Buffer : GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Singles;
      Lines    : Singles.Vector3_Array (1 .. 6);
      V1       : Singles.Vector3;
      V1_MV    : Multivectors.Vector;
      Stride   : constant Int := 3;
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      Shader_Manager.Set_Model_View_Matrix (Model_View_Matrix);

      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, Stride, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Objects.Buffers.Array_Buffer.Bind (Normals_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, GL.Types.Single_Type, Stride, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);

      GL.Objects.Buffers.Draw_Elements (Mode           => Triangles,
                                        Count          => Int (Num_Faces) * 3,
                                        Index_Type     => UInt_Type,
                                        Element_Offset => 0);
      --              GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);

      if Normal /= 0.0 then
         Put_Line ("Geosphere.GL_Draw setting lines");
         --  Draw three lines
         for index in 1 .. 3 loop
            V1_MV := Unit_E (Get_Vertex (Sphere, index));
            V1 := GL_Util.To_GL (V1_MV);
            Lines (2 * Int (index - 1) + 1) := Get_Vertex (Sphere, index);
            Lines (2 * Int (index - 1) + 2) :=
              Get_Vertex (Sphere, index) + V1 * Normal;
         end loop;

         Utilities.Load_Vertex_Buffer (Array_Buffer, Lines, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, Stride, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => GL.Types.Lines,
                                               First => 0,
                                               Count => 3);
         GL.Attributes.Disable_Vertex_Attrib_Array (0);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.GL_Draw.");
         raise;
   end GL_Draw;

   --  -------------------------------------------------------------------------

   procedure GS_Compute (Sphere : in out Geosphere; Depth : Integer) is
      New_Face       : Geosphere_Face;
      Face_Indices   : constant Indices_Array (1 .. Num_Faces)
        := ((5, 0, 2),
            (3, 2, 0),
            (5, 4, 0),
            (4, 3, 0),
            (1, 5, 2),
            (1, 2, 3),
            (1, 3, 4),
            (1, 4, 5));
      Vertices       : Vertices_Array (1 .. Num_Vertices);
   begin
      Vertices (1) := Multivectors.New_Vector (0.0, -1.0, 0.0);
      Vertices (2) := New_Vector (0.0, 1.0, 0.0);
      Vertices (3) := New_Vector (0.707, 0.0, 0.707);
      Vertices (4) := New_Vector (0.707, 0.0, -0.707);
      Vertices (5) := New_Vector (-0.707, 0.0, -0.707);
      Vertices (6) := New_Vector (-0.707, 0.0, 0.707);

      --  set initial geometry
      --        Sphere.Num_Faces := Num_Faces;
      --        Sphere.Num_Primitives := Num_Faces;
      --        Sphere.Num_Vertices := Num_Vertices;
      Sphere.Faces.Clear;
      New_Line;
      for face in 1 .. Num_Faces loop
         New_Face.Indices := Indices_Vector (Face_Indices (face));
         Sphere.Faces.Append (New_Face);
      end loop;

      Sphere.Vertices.Clear;
      for vertex_index in 1 .. Num_Vertices loop
         Sphere.Vertices.Append (Vertices (vertex_index));
      end loop;

      for face_index in 1 .. Num_Faces loop
         Refine_Face (Sphere, face_index, Depth);
      end loop;

      Sphere.Depth := Depth;
      Compute_Neighbours (Sphere);
      Sphere.isNull := False;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.GS_Compute.");
         raise;
   end GS_Compute;

   --  -------------------------------------------------------------------------

   procedure GS_Draw_Children (Render_Program    : GL.Objects.Programs.Program;
                               Model_View_Matrix : GL.Types.Singles.Matrix4;
                               Sphere            : Geosphere;
                               thisFace          : Geosphere_Face; Normal : GL.Types.Single) is
      procedure Draw_Child (thisChild : Geosphere_Face; Normal : GL.Types.Single) is
         use GL.Objects.Buffers;
         use GL.Types;
         Vertex_Buffer  : GL.Objects.Buffers.Buffer;
         Indices_Buffer : GL.Objects.Buffers.Buffer;
         Normals_Buffer : GL.Objects.Buffers.Buffer;
         Vertices       : Singles.Vector3_Array (1 .. Int (Length (Sphere.Vertices)));
         Normals        : Singles.Vector3_Array (1 .. Int (Length (Sphere.Faces)));
         Indices        : UInt_Array (1 .. 3);
      begin
         Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffer);
         Get_Vertices (Sphere, thisChild, Vertices);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

         Indices_Buffer.Initialize_Id;
         Element_Array_Buffer.Bind (Indices_Buffer);
         for index in 1 .. 3 loop
            Indices (Int (index)) := UInt (thisFace.Indices (index));
         end loop;
         Utilities.Load_Element_Buffer
           (Element_Array_Buffer, Indices, Static_Draw);

         Normals_Buffer.Initialize_Id;
         Array_Buffer.Bind (Normals_Buffer);
         Get_Normals (Sphere, thisChild, Normals);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);

         GL_Draw (Render_Program, Model_View_Matrix, Sphere, Normal,
                  Vertex_Buffer, Normals_Buffer);
      end Draw_Child;
   begin
      if thisFace.Child /= (-1, -1, -1, -1) then
         for index in 1 .. 4 loop
            if thisFace.Child (index) /= -1 then
               Draw_Child (Sphere.Faces.Element (thisFace.Child (index)), Normal);
               --                 Draw_Child (thisFace.Child (index).all, Normal);
            end if;
         end loop;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Geosphere.GS_Draw_Children.");
         raise;
   end GS_Draw_Children;

   --  -------------------------------------------------------------------------
   --  Based on geosphere.cpp
   --  gsDraw(geosphere * sphere, mv::Float normal /*= 0.0*/)
   procedure GS_Draw (Render_Program    : GL.Objects.Programs.Program;
                      Model_View_Matrix : GL.Types.Singles.Matrix4;
                      Sphere            : Geosphere; Normal : GL.Types.Single := 0.0) is
      use Face_Vectors;

      procedure Draw_Face (C : Cursor) is
         use GL.Objects.Buffers;
         use GL.Types;
         Face_Index     : constant Integer := Face_Vectors.To_Index (C);
         thisFace       : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
         Vertex_Buffer  : GL.Objects.Buffers.Buffer;
         Indices_Buffer : GL.Objects.Buffers.Buffer;
         Normals_Buffer : GL.Objects.Buffers.Buffer;
         Vertices       : Singles.Vector3_Array (1 .. Int (Length (Sphere.Vertices)));
         Normals        : Singles.Vector3_Array (1 .. Int (Length (Sphere.Faces)));
         Indices        : UInt_Array (1 .. Int (3 * Int (Length (Sphere.Faces))));
      begin
         if thisFace.Child /= (-1, -1, -1, -1) then
            GS_Draw_Children (Render_Program, Model_View_Matrix, Sphere, thisFace, Normal);
         else  --  no children
            --                  Utilities.Print_Matrix ("Geosphere.GS_Draw Model_View_Matrix", Model_View_Matrix );
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Get_Vertices (Sphere, Vertices);
            --                  Utilities.Print_GL_Array3 ("Geosphere.GS_Draw Vertices", Vertices);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

            Indices_Buffer.Initialize_Id;
            Element_Array_Buffer.Bind (Indices_Buffer);
            Get_Indices (Sphere, Indices);
            Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

            Normals_Buffer.Initialize_Id;
            Array_Buffer.Bind (Normals_Buffer);
            Get_Normals (Sphere, thisFace, Normals);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);

            --                  Put_Line ("Geosphere.GS_Draw face index " & Integer'Image (Face_Index));
            --                  Utilities.Print_GL_Array3 ("Number of vertices: " &
            --                                               Integer'Image (Num_Vertices), Vertices);

            GL_Draw (Render_Program, Model_View_Matrix, Sphere, Normal,
                     Vertex_Buffer, Normals_Buffer);
         end if;
      end Draw_Face;

   begin
      Iterate (Sphere.Faces, Draw_Face'Access);

   exception
      when others =>
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

   procedure Refine_Face (Sphere : in out Geosphere; Face_index,
                          Depth  : Natural) is
      use E3GA;
      Faces           : constant F_Vector := Sphere.Faces;
      Num_Faces       : constant Positive := Positive (Faces.Length);
      this_Face       : constant Geosphere_Face := Faces.Element (Face_index);
      Vertex_Indicies : constant Indices_Vector := this_Face.Indices;
      Vertices        : constant MV_Vector := Sphere.Vertices;
      New_Indices     : Indices_Vector := (0, 0, 0);
      Index_2         : Natural;
      Vertex_1        : E3_Vector;
      Vertex_2        : E3_Vector;
      New_Vertex      : E3_Vector;  --  V1
   begin
      --  Refine_Face is recursive
--        New_Line;
--        Put_Line ("Geosphere.Refine_Face entered, Face_index: " &
--                    Integer'Image (Face_index));
      --       GA_Utilities.Print_Integer_Array ("Geosphere.Refine_Face this_Face.Indices",
      --                                         (this_Face.Indices));
--        Put_Line ("Geosphere.Refine_Face Vertices first, last index, depth: " &
--                    Integer'Image (Vertices.First_Index) &
--                    Integer'Image (Vertices.Last_Index) &
--                    Integer'Image (Depth));
      if Depth > 0 then
         --   create 3 new vertices
         for index in Int3_Range loop
            Index_2 := index + 1;
            if Index_2 > 3 then
               Index_2 := 1;
            end if;
            --              Put_Line ("Geosphere.Refine_Face index, index_2: " &  Integer'Image (index)
            --                        &  Integer'Image (index_2));
            --              Put_Line ("vertex index: "&  Integer'Image (Vertex_Indicies (index)));
            --              Put_Line ("vertex index 2: "&  Integer'Image (Vertex_Indicies (index_2)));
            Vertex_1 := E3GA.Get_Coords (Vertices.Element (Vertex_Indicies (index)));
            Vertex_2 := E3GA.Get_Coords (Vertices.Element (Vertex_Indicies (Index_2)));
            New_Vertex := Vertex_1 + Vertex_2;
            Utilities.Print_Vector ("Geosphere.Refine_Face, Vertex_1", Vertex_1);
            Utilities.Print_Vector ("Geosphere.Refine_Face, Vertex_2", Vertex_2);
            Utilities.Print_Vector ("Geosphere.Refine_Face, Vertex_1 + Vertex_2", (Vertex_1 + Vertex_2));
            Utilities.Print_Vector ("Geosphere.Refine_Face, New_Vertex", New_Vertex);
            New_Line;
            Add_Vertex (Sphere,
                        New_Vector (Float (New_Vertex (GL.X)), Float (New_Vertex (GL.Y)),
                          Float (New_Vertex (GL.Z))), New_Indices (index));
         end loop;

         --  allocate four new faces
         Create_Child (Sphere, Face_index, 1, this_Face.Indices (1), New_Indices (1),
                       New_Indices (3), this_Face.Depth);
         Create_Child (Sphere, Face_index, 2, New_Indices (1), this_Face.Indices (2),
                       New_Indices (2), this_Face.Depth);
         Create_Child (Sphere, Face_index, 3, New_Indices (1), New_Indices (2), New_Indices (3),
                       this_Face.Depth);
         Create_Child (Sphere, Face_index, 4, New_Indices (2), this_Face.Indices (3),
                       New_Indices (3), this_Face.Depth);

--           Put_Line ("Geosphere.Refine_Face Last_Index: " &
--                       Integer'Image (Sphere.Faces.Last_Index));

         for index in Integer range 0 .. 3 loop
--              Put_Line ("Geosphere.Refine_Face recursion index: " &
--                          Integer'Image (index));
            Refine_Face (Sphere, Num_Faces + index, Depth - 1);
         end loop;
      end if;

   exception
      when others =>
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
