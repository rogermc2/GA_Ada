
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;

--  with GNAT.OS_Lib;

with Utilities;

with GL.Attributes;
with GL.Objects.Buffers;

--  with GA_Utilities;
with E3GA;
with GL_Util;

with Shader_Manager;

package body Geosphere is

    package Sphere_List_Package is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Geosphere);
    type Sphere_DL_List is new Sphere_List_Package.List with null record;

    type Indices_Array is array (Integer range <>) of Indices;
    type Vertices_Array is array (Integer range <>) of Multivectors.Vector;
    Sphere_List   : Sphere_DL_List;

    procedure Get_Vertices (Sphere   : Geosphere; Face : Geosphere_Face;
                            Vertices : in out GL.Types.Singles.Vector3_Array);
    procedure Refine_Face (Sphere : in out Geosphere; Face_Index, Depth : Natural);

    --  -------------------------------------------------------------------------

    procedure Add_To_Sphere_List (Sphere : Geosphere) is
    begin
        Sphere_List.Append (Sphere);
    end Add_To_Sphere_List;

    --  -------------------------------------------------------------------------

    procedure Add_Vertex (Sphere    : in out Geosphere; Pos : Multivectors.Vector;
                          V_Index   : out Integer) is
        Vertices : constant MV_Vector := Sphere.Vertices;
        MV       : Multivectors.Vector;
        Index    : Integer := 0;
        Found    : Boolean := False;
    begin
        --  first check if vertex already exists
        while not Found and then Index <= Sphere.Vertices.Last_Index loop
            MV :=  Pos - Vertices.Element (Index);
--              GA_Utilities.Print_Multivector
--                ("Geosphere.Add_Vertex Index " & Integer'Image (Index) & " Element ", Vertices.Element (Index));
--              GA_Utilities.Print_Multivector
--                ("Geosphere.Add_Vertex Index " & Integer'Image (Index) & " Pos ", Pos);
--              GA_Utilities.Print_Multivector
--                ("Geosphere.Add_Vertex Index " & Integer'Image (Index), MV);
            Found := Norm_Esq (MV) < 10.0 ** (-5);
            if Found then
--                  GA_Utilities.Print_Vertex
--                    ("Geosphere.Add_Vertex vertex " & Integer'Image (Index) & " already exists",
--                     Vertices.Element (Index));
                V_Index := Index;
            end if;
            Index := Index + 1;
        end loop;

        if not Found then
            Sphere.Vertices.Append (Pos);
            V_Index := Sphere.Vertices.Last_Index;
--              GA_Utilities.Print_Vertex
--                    ("Geosphere.Add_Vertex addding vertex " & Integer'Image (V_Index),
--                     Sphere.Vertices.Element (V_Index));
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Add_Vertex.");
            raise;
    end Add_Vertex;

    --  -------------------------------------------------------------------------

    procedure Compute_Neighbours (Sphere : in out Geosphere) is

        procedure Find_Relation (Face_Cursor_F : Face_Vectors.Cursor) is
            Index_FF : constant Integer := Face_Vectors.To_Index (Face_Cursor_F);  --  f
            Face_F  : Geosphere_Face := Sphere.Faces.Element (Index_FF);
            Num     : Integer := 0;
            Found   : Boolean := False;

            --  -------------------------------------------------------------------
            --  Find_Neighbours finds the neighbour faces of vertex E of Face I
            procedure Find_Neighbours (Index_FI, Index_FI_VE : Integer) is
                Face_I         : Geosphere_Face := Sphere.Faces.Element (Index_FI);
                FE_Index       : constant Integer := Face_F.Indices (Index_FI_VE);
                --  e + 1 mod 3:
                Index_VE1      : integer := Index_FI_VE + 1;
                Next_FE_Index  : Integer;
                Vertex_J1      : Integer;
                Vertex_J2      : Integer;
            begin
                if Index_VE1 > 3 then
                    Index_VE1 := 1;
                end if;
                Next_FE_Index := Face_F.Indices (Index_VE1);
                --  For each vertex j of Face_I find edged common to face Face_I and
                --  neighbour E of Face_F
                --              Put_Line ("Geosphere.Find_Neighbours find matching vertices of Face "  &
                --                         Integer'Image (Index_F) & " vertex "&
                --                         Integer'Image (Index_FI_VE));

                for Vertex_J in 1 .. 3 loop --  j
                    if Face_I.Indices (Vertex_J) = FE_Index then
                        --    same vertices found
--                          Put_Line ("Geosphere.Find_Neighbours matching vertices E and J found, E:"  &
--                                      Integer'Image (Index_FI_VE) & "  J:" &
--                                      Integer'Image (Vertex_J) & " vertex E:" &
--                                      Integer'Image (Face_F.Indices (Index_FI_VE)) & " vertex J:" &
--                                      Integer'Image (Face_F.Indices (Vertex_J)));
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
                            --                              Put_Line ("Geosphere.Find_Neighbours matching vertices E1 and J1 found, E1:"  &
                            --                                          Integer'Image (Index_FI_VE) & "  J1:" &
                            --                                          Integer'Image (Vertex_J1) & " vertex E1:" &
                            --                                          Integer'Image (Face_F.Indices (Index_FI_VE)) & " vertex J1:" &
                            --                                          Integer'Image (Face_F.Indices (Vertex_J1)));
                            Face_F.Neighbour (Index_FI_VE) := Index_FI;
                            Face_I.Neighbour (Vertex_J) := Index_FF;
                            Found := True;
                            Num := Num + 1;
                        elsif Face_I.Indices (Vertex_J2) = Next_FE_Index then
                            --                              Put_Line ("Geosphere.Find_Neighbours matching vertices E1 and J2 found, E1:"  &
                            --                                          Integer'Image (Index_FI_VE) & "  J2:" &
                            --                                          Integer'Image (Vertex_J2) & " vertex E1:" &
                            --                                          Integer'Image (Face_F.Indices (Index_FI_VE)) & " vertex J2:" &
                            --                                          Integer'Image (Face_F.Indices (Vertex_J2)));
                            --  next vertex of face[f] matches preceding vertex of face[i]
                            Face_F.Neighbour (Index_FI_VE) := Index_FI;
                            Face_I.Neighbour (Vertex_J2) := Index_FF;
                            Found := True;
                            Num := Num + 1;
                        end if;
--                      else
--                          Put_Line ("Geosphere.Find_Neighbours; no matching vertices E and J"  &
--                                      Integer'Image (Index_FI_VE) & Integer'Image (Vertex_J));
                    end if;

                    if Found then
--                          Put_Line ("Geosphere.Find_Relation,  Face "  &
--                                      Integer'Image (Index_FF) &
--                                      " has relation, neighbour face "  &
--                                      Integer'Image (Index_FI));
--                          New_Line;
                        Sphere.Faces.Replace_Element (Index_FI, Face_I);
                        Found := False;
                    end if;
                end loop;

            exception
                when others =>
                    Put_Line ("An exception occurred in Geosphere.Find_Neighbours.");
                    raise;
            end Find_Neighbours;

            --  -------------------------------------------------------------------

        begin  --  Find_Relation
            --           Put_Line ("Geosphere.Compute_Neighbours.Find_Relation Face_Index (Index_F)" &
            --                    Positive'Image ());
            for Vertex_FE in 1 .. 3 loop  --  e
                if Face_F.Neighbour (Vertex_FE) >= 0 then
--                      Put_Line ("Geosphere.Find_Relation,  Face "  &
--                                  Integer'Image (Index_FF) & " vertex " &
--                                  Integer'Image (Vertex_FE) & " has existing relation, neighbour face "  &
--                                  Integer'Image (Face_F.Neighbour (Vertex_FE)));
--                      New_Line;
                    Num := Num + 1;
                else
--                      Put_Line ("Geosphere.Find_Relation Find_Neighbours of Face "  &
--                                  Integer'Image (Index_FF) & " vertex " &
--                                  Integer'Image (Vertex_FE));
                    for Index_FI in Index_FF + 1 .. Sphere.Faces.Last_Index loop
                        --                    Put_Line ("Geosphere.Find_Relation check Face "  &
                        --                              Integer'Image (Index_I));
                        Find_Neighbours (Index_FI, Vertex_FE);
                    end loop;
                end if;
            end loop;

            if Num /= 3 then
                Put ("Geosphere.Compute_Neighbours found");
                if Num < 3 then
                    Put (" only");
                end if;
                Put_Line (Integer'Image (Num) & " neighbours of face "  &
                            Integer'Image (Index_FF));
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
        theVertex : Singles.Vector3;
        GA_Vector : constant Multivectors.Vector :=
                      Sphere.Vertices.Element (Vertex_Index);
    begin
        theVertex (GL.X) := Single (E3GA.e1 (GA_Vector));
        theVertex (GL.Y) := Single (E3GA.e2 (GA_Vector));
        theVertex (GL.Z) := Single (E3GA.e3 (GA_Vector));
        return theVertex;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Get_Vertex.");
            raise;
    end Get_Vertex;

    --  -------------------------------------------------------------------------

    function Get_Vertex (Sphere : Geosphere; Vertex_Index : Natural)
                        return Multivectors.Vector is
        GA_Vector : constant Multivectors.Vector :=
                      Sphere.Vertices.Element (Vertex_Index);
    begin
        Put_Line ("Geosphere.Get_Vertex 2 Vertex_Index." & Natural'Image (Vertex_Index));
        return GA_Vector;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Get_Vertex 2.");
            raise;
    end Get_Vertex;

    --  -------------------------------------------------------------------------

--      procedure Get_Indices (Sphere  : Geosphere;
--                             Indices : in out GL.Types.UInt_Array) is
--          use GL.Types;
--          Indice_Index : Int := 0;
--
--          procedure Add_Index (C : Face_Vectors.Cursor) is
--              Face_Index     : constant Integer := Face_Vectors.To_Index (C);
--              thisFace       : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
--              Vertex_Indices : Indices_Vector;
--          begin
--              Vertex_Indices := thisFace.Indices;
--              Indice_Index := Indice_Index + 1;
--              Indices (Indice_Index) := UInt (Vertex_Indices (1));
--              Indice_Index := Indice_Index + 1;
--              Indices (Indice_Index) := UInt (Vertex_Indices (2));
--              Indice_Index := Indice_Index + 1;
--              Indices (Indice_Index) := UInt (Vertex_Indices (3));
--          end Add_Index;
--
--      begin
--          Iterate (Sphere.Faces, Add_Index'Access);
--
--      exception
--          when others =>
--              Put_Line ("An exception occurred in Geosphere.Get_Indices.");
--              raise;
--      end Get_Indices;

    --  -------------------------------------------------------------------------

    function Get_Indices (Face : Geosphere_Face) return GL.Types.UInt_Array is
        theIndices : GL.Types.UInt_Array (1 .. 3);
    begin
        theIndices (1) := GL.Types.UInt (Face.Indices (1));
        theIndices (2) := GL.Types.UInt (Face.Indices (2));
        theIndices (3) := GL.Types.UInt (Face.Indices (3));
        return theIndices;

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
            Put_Line ("An exception occurred in Geosphere.Get_Vertices 1.");
            raise;
    end Get_Vertices;

    --  -------------------------------------------------------------------------

    procedure Get_Vertices (Sphere   : Geosphere; Face : Geosphere_Face;
                            Vertices : in out GL.Types.Singles.Vector3_Array) is
    begin
--          GA_Utilities.Print_Integer_Array ("Geosphere.Get_Vertices Face.Indices", Face.Indices);
        for index in Int3_Range loop
            Vertices (GL.Types.Int (index)) :=
              Get_Vertex (Sphere, Face.Indices (index));
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.Get_Vertices 2.");
            raise;
    end Get_Vertices;

    --  -------------------------------------------------------------------------

    procedure GL_Draw (Render_Program                : GL.Objects.Programs.Program;
                       Model_View_Matrix             : GL.Types.Singles.Matrix4;
                       Sphere                        : Geosphere; Normal : GL.Types.Single;
                       Vertex_Buffer, Normals_Buffer, Indices_Buffer : GL.Objects.Buffers.Buffer;
                       Num_Faces : GL.Types.Size) is
        use GL.Objects.Buffers;
        use GL.Types;
        use GL.Types.Singles;
        Lines    : Singles.Vector3_Array (1 .. 6) := (others => (0.0, 0.0, 0.0));
        V1       : Singles.Vector3 := (0.0, 0.0, 0.0);
        V1_MV    : Multivectors.Vector;
--          MV       : constant GL.Types.Singles.Matrix4 := Maths.Scaling_Matrix (0.12);

    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Shader_Manager.Set_Model_View_Matrix (Model_View_Matrix);

        GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        GL.Objects.Buffers.Array_Buffer.Bind (Normals_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, GL.Types.Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);

                Element_Array_Buffer.Bind (Indices_Buffer);
        --  Draw_Elements uses Count sequential elements from an enabled array
        --  starting at Element_Offset to construct a sequence of Mode geometric primitives.
        GL.Objects.Buffers.Draw_Elements (Mode           => Triangles,
                                          Count          => Num_Faces,
                                          Index_Type     => UInt_Type,
                                          Element_Offset => 0);
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
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, GL.Types.Single_Type, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);

            --              GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => GL.Types.Lines,
            --                                                    First => 0,
            --                                                    Count => 3);
            GL.Attributes.Disable_Vertex_Attrib_Array (0);
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.GL_Draw.");
            raise;
    end GL_Draw;

    --  -------------------------------------------------------------------------

    procedure GS_Compute (Sphere : in out Geosphere; Depth : Integer) is
--          use Ada.Containers;
        Num_Faces     : constant Positive := 8;
        Num_Vertices  : constant Positive := 6;

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
--          Put_Line ("Geosphere.GS_Compute Face indices:");
        for face in Integer range 1 .. Integer (Sphere.Faces.Length) loop
            New_Face := Sphere.Faces.Element (face);
--              Put_Line (Integer'Image (face) & Integer'Image (New_Face.Indices (1)) &
--                          Integer'Image (New_Face.Indices (2)) &
--                          Integer'Image (New_Face.Indices (3)));
        end loop;

--          for index in 0 .. Integer (Sphere.Vertices.Length - 1) loop
--              GA_Utilities.Print_Vertex (Integer'Image (index), Sphere.Vertices.Element (index));
--          end loop;
--          GNAT.OS_Lib.OS_Exit (0);

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
            --  Each child (face) has three vertices
            Vertices       : Singles.Vector3_Array (1 .. 3);
            Normals        : Singles.Vector3_Array (1 .. 3);
            Indices        : UInt_Array (1 .. 3);
        begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Get_Vertices (Sphere, thisChild, Vertices);
--              Utilities.Print_GL_Array3 ("Draw_Child, thisChild, Vertices", Vertices);
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
                     Vertex_Buffer, Normals_Buffer, Indices_Buffer, 3);
        end Draw_Child;
    begin
        if thisFace.Child /= (-1, -1, -1, -1) then
            for index in 1 .. 4 loop
                if thisFace.Child (index) /= -1 then
                    Draw_Child (Sphere.Faces.Element (thisFace.Child (index)), Normal);
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

        procedure Draw_Face (Face_Cursor : Cursor) is
            use GL.Objects.Buffers;
            use GL.Types;
            Face_Index     : constant Integer := Face_Vectors.To_Index (Face_Cursor);
            thisFace       : constant Geosphere_Face := Sphere.Faces.Element (Face_Index);
            Vertex_Buffer  : GL.Objects.Buffers.Buffer;
            Indices_Buffer : GL.Objects.Buffers.Buffer;
            Normals_Buffer : GL.Objects.Buffers.Buffer;
            Vertices       : Singles.Vector3_Array (1 .. Int (Length (Sphere.Vertices)));
            Normals        : Singles.Vector3_Array (1 .. Int (Length (Sphere.Faces)));
--              Indices        : UInt_Array (1 .. Int (3 * Int (Length (Sphere.Faces))));
        begin
            if thisFace.Child /= (-1, -1, -1, -1) then
                GS_Draw_Children (Render_Program, Model_View_Matrix, Sphere, thisFace, Normal);
            else  --  no children
                Vertex_Buffer.Clear;
                Vertex_Buffer.Initialize_Id;
                Array_Buffer.Bind (Vertex_Buffer);
                Get_Vertices (Sphere, Vertices);
--                  Utilities.Print_GL_Array3 ("Geosphere.GS_Draw Vertices", Vertices);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
                --  Vertex_Attrib_Array buffer attributes are set in GL_Draw;

                Indices_Buffer.Clear;
                Indices_Buffer.Initialize_Id;
                Element_Array_Buffer.Bind (Indices_Buffer);

                Utilities.Load_Element_Buffer (Element_Array_Buffer, Get_Indices (thisFace), Static_Draw);

                Normals_Buffer.Clear;
                Normals_Buffer.Initialize_Id;
                Array_Buffer.Bind (Normals_Buffer);
                Get_Normals (Sphere, thisFace, Normals);
                Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);

                --                  Put_Line ("Geosphere.GS_Draw face index " & Integer'Image (Face_Index));
                --                  Utilities.Print_GL_Array3 ("Number of vertices: " &
                --                                               Integer'Image (Num_Vertices), Vertices);

                GL_Draw (Render_Program, Model_View_Matrix, Sphere, Normal, Vertex_Buffer,
                         Normals_Buffer, Indices_Buffer, 3);
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

    procedure New_Face (Sphere : in out Geosphere; V1, V2, V3, Depth : Integer) is
        newFace : Geosphere_Face;
    begin
        newFace.Indices (1) := V1;
        newFace.Indices (2) := V2;
        newFace.Indices (3) := V3;
        newFace.Depth := Depth + 1;
        Sphere.Faces.Append (newFace);

    exception
        when others =>
            Put_Line ("An exception occurred in Geosphere.New_Face.");
            raise;
    end New_Face;

    --  -------------------------------------------------------------------------

    procedure New_Sphere_List (Sphere : Geosphere) is
    begin
        Sphere_List.Clear;
        Sphere_List.Append (Sphere);
    end New_Sphere_List;

    --  -------------------------------------------------------------------------

    procedure Refine_Face (Sphere : in out Geosphere; Face_Index,
                           Depth  : Natural) is
        use Face_Vectors;
        use E3GA;
        Faces           : constant F_Vector := Sphere.Faces;
        Num_Faces       : constant Positive := Positive (Faces.Length);
        this_Face       : Geosphere_Face := Faces.Element (Face_index);
        Face_Cursor     : constant Cursor := Sphere.Faces.To_Cursor (Face_index);
        Vertex_Indicies : constant Indices_Vector := this_Face.Indices;
        Vertices        : constant MV_Vector := Sphere.Vertices;
        New_Indices     : Indices_Vector := (0, 0, 0);  -- v[3]
        Index_2         : Natural;
        Vertex_1        : E3_Vector;
        Vertex_2        : E3_Vector;
        New_Vertex      : E3_Vector;  --  V1
    begin
        --  Refine_Face is recursive
        --        New_Line;
--          Put_Line ("Geosphere.Refine_Face, Face_index: " &
--                      Integer'Image (Face_index) & " recursion depth" &
--                      Natural'Image (Depth));
        --       GA_Utilities.Print_Integer_Array ("Geosphere.Refine_Face this_Face.Indices",
        --                                         (this_Face.Indices));
        if Depth > 0 then
--              Put_Line ("Geosphere.Refine_Face, refining Face_index: " &
--                          Integer'Image (Face_index));
            --   create 3 new vertices
            for index in Int3_Range loop
                Index_2 := index + 1;
                if Index_2 > 3 then
                    Index_2 := 1;
                end if;
                Vertex_1 := E3GA.Get_Coords (Vertices.Element (Vertex_Indicies (index)));
                Vertex_2 := E3GA.Get_Coords (Vertices.Element (Vertex_Indicies (Index_2)));
                New_Vertex := E3GA.Unit_E (Vertex_1 + Vertex_2);
                Add_Vertex (Sphere,
                            New_Vector (Float (New_Vertex (GL.X)), Float (New_Vertex (GL.Y)),
                              Float (New_Vertex (GL.Z))), New_Indices (index));
            end loop;

--              GA_Utilities.Print_Integer_Array ("Geosphere.Refine_Face, New vertex indices",
--                                                New_Indices);
--              for index in Int3_Range loop
--                  GA_Utilities.Print_Vertex ("", Sphere.Vertices.Element (New_Indices (index)));
--              end loop;
            --  allocate four new faces
            New_Face (Sphere, this_Face.Indices (1), New_Indices (1),
                      New_Indices (3), this_Face.Depth);
            New_Face (Sphere, New_Indices (1), this_Face.Indices (2),
                      New_Indices (2), this_Face.Depth);
            New_Face (Sphere, New_Indices (1), New_Indices (2), New_Indices (3),
                      this_Face.Depth);
            New_Face (Sphere, New_Indices (2), this_Face.Indices (3),
                      New_Indices (3), this_Face.Depth);

            for index in Integer range 1 .. 4 loop
                this_Face.Child (index) := Num_Faces + index;
            end loop;
--              GA_Utilities.Print_Integer_Array ("Geosphere.Refine_Face child indices",
--                                                GA_Maths.Integer_Array (this_Face.Child));

            Sphere.Faces.Replace_Element (Face_Cursor, this_Face);
            for index in Integer range 1 .. 4 loop
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
