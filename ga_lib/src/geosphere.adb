
with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;

with C3GA;
with GL_Util;

with Shader_Manager;

package body Geosphere is

    package Sphere_List_Package is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Geosphere);
    type Sphere_DL_List is new Sphere_List_Package.List with null record;

    type Indices_Array is array (Integer range <>) of Indices;
    type Vertices_Array is array (Integer range <>) of Vector;

    Num_Faces     : constant Integer := 8;
    Num_Vertices  : constant Integer := 6;
    Sphere_List   : Sphere_DL_List;

    procedure Get_Vertices (Sphere : Geosphere; Face : Geosphere_Face;
                            Vertices : in out GL.Types.Singles.Vector3_Array);
    procedure Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer);

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
            Found := Norm_Esq (V) > e ** (-10);
        end loop;

        if not Found then  --  reate new vertex
            Sphere.Vertices.Append (Pos);
        end if;
        return Found;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Add_Vertex.");
            raise;
    end Add_Vertex;

    --  -------------------------------------------------------------------------

    procedure Compute_Neighbours (Sphere : in out Geosphere) is

        procedure Find_Relation (C1 : Face_Vectors.Cursor) is
            Face1_Index : constant Integer := Face_Vectors.To_Index (C1);  --  f
            Face_1      : Geosphere_Face := Sphere.Faces.Element (Face1_Index);
            Num         : Integer := 0;
            Index_1     : Integer := 0;  --  e

            --  -------------------------------------------------------------------

            procedure Find_Neighbours (C2 : Face_Vectors.Cursor) is
                Face2_Index : constant Integer := Face_Vectors.To_Index (C2);  --  i
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
                when others =>
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

        --        procedure New_Sphere_List (Sphere : Geosphere) is
        --        begin
        --           Sphere_List.Clear;
        --           Sphere_List.Append (Sphere);
        --        end New_Sphere_List;

        --  ---------------------------------------------------------------------

        procedure Reset_Relation (C : Face_Vectors.Cursor) is
            Face_Index : constant Integer := Face_Vectors.To_Index (C);
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
        when others =>
            Put_Line ("An exception occurred in Geosphere.Compute_Neighbours.");
            raise;
    end Compute_Neighbours;

    --  -------------------------------------------------------------------------

    procedure Draw_Sphere_List (Render_Program : GL.Objects.Programs.Program;
                                MV_Matrix : GL.Types.Singles.Matrix4;
                                Normal : GL.Types.Single := 0.0) is
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

    procedure Create_Child (Face : in out Geosphere_Face;
                            Child_Index : integer; V1, V2, V3 : Integer) is
    begin
        Face.Child (Child_Index) := new Geosphere_Face;
        Face.Child (Child_Index).Vertex_Indices (1) := V1;
        Face.Child (Child_Index).Vertex_Indices (2) := V2;
        Face.Child (Child_Index).Vertex_Indices (3) := V3;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Create_Child.");
            raise;
    end Create_Child;

    --  -------------------------------------------------------------------------

    function Get_Vertex (Sphere : Geosphere; Vertex_Index : Positive)
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

    function Get_Vertex (Sphere : Geosphere; Vertex_Index : Positive)
                         return Multivectors.Vector is
        GA_Vector : constant Multivectors.Vector :=
                      Sphere.Vertices.Element (Vertex_Index);
    begin
        return GA_Vector;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Get_Vertices 2.");
            raise;
    end Get_Vertex;

    --  -------------------------------------------------------------------------

    procedure Get_Indices (Sphere : Geosphere;
                           Indices : in out GL.Types.UInt_Array) is
        use GL.Types;
        Indice_Index : Int := 0;

        procedure Add_Index (C : Face_Vectors.Cursor) is
            Face_Index     : constant Integer := Face_Vectors.To_Index (C);
            thisFace       : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
            Vertex_Indices : Indices_Vector;
        begin
            Vertex_Indices := thisFace.Vertex_Indices;
            Indice_Index := Indice_Index + 1;
            Indices (Indice_Index) := UInt (Vertex_Indices (1) - 1);
            Indice_Index := Indice_Index + 1;
            Indices (Indice_Index) := UInt (Vertex_Indices (2) - 1);
            Indice_Index := Indice_Index + 1;
            Indices (Indice_Index) := UInt (Vertex_Indices (3) - 1);
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

    procedure Get_Normals (Sphere : Geosphere; Face : Geosphere_Face;
                           Normals : in out GL.Types.Singles.Vector3_Array) is
        use GL.Types;
        Normal_Index : Integer := 0;

        thisNormal  : constant Indices_Vector := Face.Vertex_Indices;
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

    procedure Get_Vertices (Sphere : Geosphere;
                            Vertices : in out GL.Types.Singles.Vector3_Array) is
        use GL.Types;
        Index : Int := 0;

        procedure Add_Vertex (C : Vertex_Vectors.Cursor) is
            Vertex_Index     : constant Integer := Vertex_Vectors.To_Index (C);
            thisVertex       : constant Vector := Sphere.Vertices.Element (Vertex_Index);
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

    procedure Get_Vertices (Sphere : Geosphere; Face : Geosphere_Face;
                            Vertices : in out GL.Types.Singles.Vector3_Array) is
    begin
        for index in Int3_Range loop
            Vertices (GL.Types.Int (index)) :=
              Get_Vertex (Sphere, Face.Vertex_Indices (index));
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Add_Vertices.");
            raise;
    end Get_Vertices;

    --  -------------------------------------------------------------------------

    procedure GS_Compute (Sphere : in out Geosphere; Depth : Integer) is
    --        S_Faces       : constant F_Vector := Sphere.Faces;
        New_Face      : Geosphere_Face;
        --  1 is subtracted from each index in Get_Indices
        Faces         : constant Indices_Array (1 .. Num_Faces)
          := ((6, 1, 3),
              (4, 3, 1),
              (6, 5, 1),
              (5, 4, 1),
              (2, 6, 3),
              (2, 3, 4),
              (2, 4, 5),
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
            New_Face.Vertex_Indices := Indices_Vector (Faces (face));
            --              for index in Int4_range loop
            --                  New_Face.Child (index) := 0;
            --              end loop;
            New_Face.Depth := 0;
            Sphere.Faces.Append (New_Face);
        end loop;

        for vertex in 1 .. Num_Vertices loop
            Sphere.Vertices.Append (Vertices (vertex));
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

    procedure GL_Draw (Render_Program : GL.Objects.Programs.Program;
                            Model_View_Matrix : GL.Types.Singles.Matrix4;
                            Sphere : Geosphere; Normal : GL.Types.Single;
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

        GL.Objects.Buffers.Draw_Elements (Mode => Triangles,
                                          Count => Int (Num_Faces) * 3,
                                          Index_Type => UInt_Type,
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

    procedure GS_Draw_Children (Render_Program : GL.Objects.Programs.Program;
                                Model_View_Matrix : GL.Types.Singles.Matrix4;
                                Sphere : Geosphere;
                                thisFace : Geosphere_Face; Normal : GL.Types.Single) is
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
                Indices (Int (index)) := UInt (thisFace.Vertex_Indices (index));
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
        if thisFace.Child /= (null, null, null, null) then
            for index in 1 .. 4 loop
                if thisFace.Child (index) /= null then
                    Draw_Child (thisFace.Child (index).all, Normal);
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
    procedure GS_Draw (Render_Program : GL.Objects.Programs.Program;
                       Model_View_Matrix : GL.Types.Singles.Matrix4;
                       Sphere : Geosphere; Normal : GL.Types.Single := 0.0) is
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
            if thisFace.Child /= (null, null, null, null) then
                Put_Line ("Geosphere.GS_Draw has child");
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

    procedure Refine_Face (Sphere : in out Geosphere; Face_index, Depth : Integer) is
        Faces           : constant F_Vector := Sphere.Faces;
        this_Face       : Geosphere_Face := Faces.Element (Face_index);
        Vertex_Indicies : constant Indices_Vector := this_Face.Vertex_Indices;
        New_Indices     : array (Int3_Range) of integer := (0, 0, 0);
        index_2         : Integer;
        Vertex_1        : Vector;
        Vertex_2        : Vector;
        V1              : Vector;
        Index           : Integer := 0;
        Refined         : Boolean := False;
    begin
        if Depth > 0 then  --   create 3 new vertices
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
                V1 := To_Vector (Unit_e (From_Vector (Vertex_1 + Vertex_2)));
                Refined := Add_Vertex (Sphere, V1, New_Indices (index));
            end loop;
        end if;

        if not Refined then
            Create_Child (this_Face, 1, this_Face.Vertex_Indices (1),
                          New_Indices (1), New_Indices (3));
            Create_Child (this_Face, 2, New_Indices (1),
                          this_Face.Vertex_Indices (2), New_Indices (2));
            Create_Child (this_Face, 3, New_Indices (1),
                          New_Indices (2), New_Indices (3));
            Create_Child (this_Face, 4, New_Indices (2),
                          this_Face.Vertex_Indices (3), New_Indices (3));

            for index in Int4_range loop
                Refine_Face (Sphere, Sphere.Faces.Last_Index + Index, Depth - 1);
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
