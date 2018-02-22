
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
with GL.Objects.Programs;
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

   procedure Get_GLUT_Model_2D (Render_Program : GL.Objects.Programs.Program;
                                Model_Name : Ada.Strings.Unbounded.Unbounded_String;
                                Model_Rotor : Multivectors.Rotor) is
      use GL.Types.Singles;
      Screen_Width : Float := 1600.0;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Model_View_Matrix    : GL.Types.Singles.Matrix4 :=
        GL.Types.Singles.Identity4;
      Translation_Matrix   : Singles.Matrix4;
      Projection_Matrix    : Singles.Matrix4;
      Colour               : GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);
   begin
      --  DONT cull faces (we will do this ourselves!)
      GL.Toggles.Disable (GL.Toggles.Cull_Face);
      --  fill all polygons (otherwise they get turned into LINES
--        GL.Rasterization.Set_Polygon_Mode (GL.Culling.Front_And_Back, GL.Rasterization.Fill);

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
      if Model_Name = "teapot" then
         Solid_Teapot (1.0);
      elsif Model_Name = "cube" then
         Solid_Cube (1.0);
      elsif Model_Name = "sphere" then
         Solid_Sphere (1.0, 16, 8);
      end if;
      --  	else if (modelName == "sphere")
      --  		glutSolidSphere(1.0, 16, 8);
      --  	else if (modelName == "cone")
      --  		glutSolidCone(1.0, 2.0, 16, 8);
      --  	else if (modelName == "torus")
      --  		glutSolidTorus(0.5, 1.0, 8, 16);
      --  	else if (modelName == "dodecahedron")
      --  		glutSolidDodecahedron();
      --  	else if (modelName == "octahedron")
      --  		glutSolidOctahedron();
      --  	else if (modelName == "tetrahedron")
      --  		glutSolidTetrahedron();
      --  	else if (modelName == "icosahedron")
      --  		glutSolidIcosahedron();
      --
      --  	int nbFeedback = glRenderMode(GL_RENDER);
      --
      --  	// parse the buffer:
      --  	g_polygons2D.clear();
      --  	g_vertices2D.clear();
      --
      --  	int idx = 0;
      --  	while (idx < nbFeedback)
      --          {
      --  		// check for polygon:
      --  		if (buffer[idx] != GL_POLYGON_TOKEN)
      --              {
      --  			fprintf(stderr, "Error parsing the feedback buffer!");
      --  			break;
      --              }
      --  		idx++;
      --
      --  		// number of vertices (3)
      --  		int n = (int)buffer[idx];
      --  		idx++;
      --  		std::vector<int> vtxIdx(n);
      --
      --  		// get vertices:
      --  		// Maybe todo later: don't duplicate identical vertices  . . .
      --  		for (int i = 0; i < n; i++)
      --              {
      --  			vtxIdx[i] = (int)g_vertices2D.size();
      --  			g_vertices2D.push_back(_vector(buffer[idx] * e1 + buffer[idx+1] * e2));
      --  			idx += 2;
      --              }
      --  		g_polygons2D.push_back(vtxIdx);
      --          }
      --
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

   procedure Solid_Sphere (Radius : Float; Slices, Stacks : Integer) is
   begin
      GLUT_API.GLUT_Solid_Sphere (Double (Radius), Int (Slices), Int (Stacks));
   end Solid_Sphere;

   --  -------------------------------------------------------------------------

   procedure Solid_Teapot (Size : Float) is
   begin
      GLUT_API.GLUT_Solid_Teapot (Double (Size));
   end Solid_Teapot;

end Graphic_Data;
