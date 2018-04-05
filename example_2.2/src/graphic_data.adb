
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL_Util;

with Maths;

with GA_Draw;
with GLUT_API;
with Multivectors;

package body Graphic_Data is
   use GL.Types;

   package Buffer_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Float);
   type Buffer_List is new Buffer_Package.List with null record;

   procedure Get_GLUT_Model_2D (Render_Program : GL.Objects.Programs.Program;
                                Model_Name : Ada.Strings.Unbounded.Unbounded_String;
                                Model_Rotor : Multivectors.Rotor) is
      use GL.Types.Singles;
      Screen_Width          : constant Float := 1600.0;
      MV_Matrix_ID          : GL.Uniforms.Uniform;
      Projection_Matrix_ID  : GL.Uniforms.Uniform;
      Colour_Location       : GL.Uniforms.Uniform;
      Model_View_Matrix     : GL.Types.Singles.Matrix4 :=
        GL.Types.Singles.Identity4;
      Translation_Matrix    : Singles.Matrix4;
      Projection_Matrix     : Singles.Matrix4;
      Feedback_Buffer       : GL.Objects.Buffers.Buffer;
      Feedback_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Colour                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);
      Index                 : Integer := 0;
   begin
      --  DONT cull faces (we will do this ourselves!)
      GL.Toggles.Disable (GL.Toggles.Cull_Face);
      --  fill all polygons (otherwise they get turned into LINES
      GL.Rasterization.Set_Polygon_Mode (GL.Rasterization.Fill);

      --  setup projection & transform for the model:
      --        glFrustum (-(float)g_viewportWidth / screenWidth, (float)g_viewportWidth / screenWidth,
      --  		-(float)g_viewportHeight / screenWidth, (float)g_viewportHeight / screenWidth,
      --  		1.0, 100.0);
      GA_Draw.Init_Projection_Matrix (Projection_Matrix, 1.0, 100.0);
      Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -10.0));

      GL_Util.Rotor_GL_Multiply (Model_Rotor, Model_View_Matrix);
      Model_View_Matrix := Translation_Matrix * Model_View_Matrix;

      GA_Draw.Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                        Projection_Matrix_ID, Colour_Location);
      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      --   buffer for OpenGL feedback, format will be:
      --   GL_POLYGON_TOKEN
      --   n (= 3)
      --   vertex 0 x, vertex 0 y
      --   vertex 1 x, vertex 1 y
      --   vertex 2 x, vertex 2 y
      --   GL_POLYGON_TOKEN etc etc
      --  	std::vector<GLfloat> buffer(300000); // more than enough for the GLUT primitives

      --  switch into feedback mode:
      --  	glFeedbackBuffer((GLsizei)buffer.size(), GL_2D, &(buffer[0]));
      --  	glRenderMode(GL_FEEDBACK);

      --  render model
      Feedback_Buffer.Initialize_Id;
      Feedback_Array_Object.Bind;
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind (Feedback_Buffer);
--        GL.Attributes.Enable_Vertex_Attrib_Array (0);

      if Model_Name = "teapot" then
         Solid_Teapot (1.0);
      elsif Model_Name = "cube" then
         Solid_Cube (1.0);
      elsif Model_Name = "sphere" then
         Solid_Sphere (1.0, 16, 8);
      elsif Model_Name =  "cone" then
         Solid_Cone (1.0, 2.0, 16, 8);
      elsif Model_Name =  "torus" then
         Solid_Torus (0.5, 1.0, 8, 16);
      elsif Model_Name =  "dodecahedron" then
         Solid_Dodecahedron;
      elsif Model_Name =  "octahedron" then
         Solid_Octahedron;
      elsif Model_Name =  "tetrahedron" then
         Solid_Tetrahedron;
      elsif Model_Name =  "icosahedron" then
         Solid_Icosahedron;
      end if;

--        GL.Attributes.Disable_Vertex_Attrib_Array (0);
      --  	int nbFeedback = glRenderMode(GL_RENDER);
      --
      --  	// parse the buffer:
      --  	g_polygons2D.clear();
      --  	g_vertices2D.clear();

      	while (idx < nbFeedback)
              {
      		// check for polygon:
      		if (buffer[idx] != GL_POLYGON_TOKEN)
                  {
      			fprintf(stderr, "Error parsing the feedback buffer!");
      			break;
                  }
      		idx++;

      		// number of vertices (3)
      		int n = (int)buffer[idx];
      		idx++;
      		std::vector<int> vtxIdx(n);

      		// get vertices:
      		// Maybe todo later: don't duplicate identical vertices  . . .
      		for (int i = 0; i < n; i++)
                  {
      			vtxIdx[i] = (int)g_vertices2D.size();
      			g_vertices2D.push_back(_vector(buffer[idx] * e1 + buffer[idx+1] * e2));
      			idx += 2;
                  }
      		g_polygons2D.push_back(vtxIdx);
              }

      --  	if (g_prevStatisticsModelName != modelName)
      --          {
      --  		printf("Model: %s, #polygons: %d, #vertices: %d\n", modelName.c_str(), g_polygons2D.size(), g_vertices2D.size());
      --  		g_prevStatisticsModelName = modelName;
      --          }

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Graphic_Data.Get_GLUT_Model_2D.");
         raise;

   end Get_GLUT_Model_2D;

   --  -------------------------------------------------------------------------

   procedure Solid_Cube (Size : Float) is
   begin
      GLUT_API.GLUT_Solid_Cube (Double (Size));
   end Solid_Cube;

   --  -------------------------------------------------------------------------

   procedure Solid_Cone (Base, Height : Float; Slices, Stacks : Integer) is
   begin
      GLUT_API.GLUT_Solid_Cone (Double (Base), Double (Height),
                                Int (Slices), Int (Stacks));
   end Solid_Cone;

   --  -------------------------------------------------------------------------

   procedure Solid_Dodecahedron is
   begin
      GLUT_API.GLUT_Solid_Dodecahedron;
   end Solid_Dodecahedron;

   --  -------------------------------------------------------------------------

   procedure Solid_Icosahedron is
   begin
      GLUT_API.GLUT_Solid_Icosahedron;
   end Solid_Icosahedron;

   --  -------------------------------------------------------------------------

   procedure Solid_Octahedron is
   begin
      GLUT_API.GLUT_Solid_Octahedron;
   end Solid_Octahedron;

   --  -------------------------------------------------------------------------

   procedure Solid_Sphere (Radius : Float; Slices, Stacks : Integer) is
   begin
      GLUT_API.GLUT_Solid_Sphere (Double (Radius), Int (Slices), Int (Stacks));
   end Solid_Sphere;

   --  -------------------------------------------------------------------------

   procedure Solid_Teapot (Size : Float) is
   begin
      GLUT_API.GLUT_Solid_Teapot (Double (Size));
   end Solid_Teapot;

   --  -------------------------------------------------------------------------

   procedure Solid_Tetrahedron is
   begin
      GLUT_API.GLUT_Solid_Tetrahedron;
   end Solid_Tetrahedron;

   --  -------------------------------------------------------------------------

   procedure Solid_Torus (Inner_Radius, Outer_Radius : Float;
                          Sides, Rings : Integer) is
   begin
      GLUT_API.GLUT_Solid_Torus (Double (Inner_Radius), Double (Outer_Radius),
                                Int (Sides), Int (Rings));
   end Solid_Torus;

   --  -------------------------------------------------------------------------

end Graphic_Data;
