
--  with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
with GL.Objects.Programs;
--  with GL.Text;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Utilities;

with Blade_Types;
with E3GA;
--  with C3GA;
--  with C3GA_Draw;
with C3GA_Utilities;
with GA_Maths;
with GA_Utilities;
with Geosphere;
with GL_Util;
with Metric;
with Multivectors;
--  with Multivector_Utilities;
with Palet;
with Pick_Manager;

with Draw_1_1;
with Shader_Manager;
--  with Silo;
--  with Text_Management;

with Points;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   --      Black          : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Red            : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
   Green          : constant GL.Types.Singles.Vector4  := (0.0, 0.5, 0.0, 1.0);
   Blue           : constant GL.Types.Singles.Vector4  := (0.0, 0.0, 0.5, 1.0);
   Magenta        : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 1.0, 1.0);
   White          : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
   Key_Pressed    : boolean := False;

   --      Rotate_Model    : boolean := False;
   --      Rotate_Model_Out_Of_Plane  : boolean := False;
   --      Pick            : GL_Util.GL_Pick;

   --      procedure Draw_Text (Window_Width, Window_Height : Glfw.Size;
   --                          theText         : String;
   --                          Render_Program  : GL.Objects.Programs.Program;
   --                          Text_X, Text_Y  : GL.Types.Single;
   --                          Text_Scale      : GL.Types.Single);
   --     procedure Text_Shader_Locations (Render_Text_Program : GL.Objects.Programs.Program;
   --                                      Projection_Matrix_ID, Texture_ID, Text_Dimesions_ID,
   --                                      Colour_ID : out GL.Uniforms.Uniform);

   --  -------------------------------------------------------------------------

   procedure Display (Window                 : in out Glfw.Windows.Window;
                      Render_Graphic_Program : GL.Objects.Programs.Program) is
      --                        Render_Text_Program    : GL.Objects.Programs.Program) is
      --          use GL.Objects.Buffers;
      --          use GL.Types.Colors;
      use GL.Types.Singles;     --  for matrix multiplicatio
--        use Metric;
      use Multivectors;

      --          Position_X        : integer := 0;
      --          Position_Y        : single := 160.0;
      --        Label             : Silo.Label_Data;
      --          Label_Position    : GL.Types.Singles.Vector2 := (0.0, 0.0);

      --        E11               : constant float := E3GA.Get_Coord_1 (E3GA.e1);
      --        E12               : constant float := E3GA.Get_Coord_2 (E3GA.e1);
      --        E21               : constant float := E3GA.Get_Coord_1 (E3GA.e2);
      --        E22               : constant float := E3GA.Get_Coord_2 (E3GA.e2);
      --          V1                : Multivectors.M_Vector; --  2D M_Vector (0, 0), (1, 0)
      --          V2                : Multivectors.M_Vector;

--        Point_Position      : Normalized_Point := New_Normalized_Point;
      aLine               : Multivectors.Line;
      aCircle             : Circle;
--        Circle_Rotated      : Circle;
      aDual_Plane         : Dual_Plane;
      --        Text_Coords           : GA_Maths.Array_3D := (0.0, 0.0, 0.0);
      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      --          Pick                : GL_Util.GL_Pick;
      Width               : GL.Types.Single;
      Height              : GL.Types.Single;
      --  rotor g_modelRotor(_rotor(1.0f))
      Model_Rotor         : constant Rotor := New_Rotor (1.0);
      Translation_Matrix  : Matrix4 := Identity4;
      Projection_Matrix   : Matrix4 := Identity4;
      View_Matrix         : constant Matrix4 := Identity4;
      Model_View_Matrix   : Matrix4 := Identity4;
      View_Angle          : constant Maths.Degree := 50.0;
      --          View_Matrix         : Matrix4 := Identity4;
      --          Camera_Position     : constant Vector3 := (0.0, 0.0, 5.0);
      --          Half_Pi             : constant Single := 0.5 * Ada.Numerics.Pi;
      --          Horizontal_Angle    : constant Single := Ada.Numerics.Pi;
      --          Vertical_Angle      : constant Single := 0.0;
      --          Direction           : Vector3;
      --          Right               : Vector3;
      --          Up                  : Vector3;
      N_E3_Vec            : constant E3GA.E3_Vector := (0.0, 1.0, 0.0);
      Phi                 : constant Float := 0.5 * GA_Maths.Pi;
      R_Versor            : TR_Versor;
--        Rotated_Circle      : Circle;
--        R_R_Circle          : Circle;
      DL                  : Dual_Line;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Width := Single (Window_Width);
      Height := Single (Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Width), Int (Height));

      --          GL_Util.Load_Pick_Matrix;

      Maths.Init_Perspective_Transform
        (View_Angle, Width, Height, 0.1, -100.0, Projection_Matrix);
      Shader_Manager.Set_Projection_Matrix (Projection_Matrix);

      Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -14.0));

      Shader_Manager.Set_View_Matrix (View_Matrix);
      Model_View_Matrix := Translation_Matrix * Model_View_Matrix;
      Shader_Manager.Set_Model_View_Matrix (Model_View_Matrix);
      --  View and model matrices are initilized to identity by
      --  shader initialization.
      Utilities.Clear_Background_Colour_And_Depth (White);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);

      Shader_Manager.Set_Line_Width (2.0);
      GL_Util.Rotor_GL_Multiply (Model_Rotor, Model_View_Matrix);

      Palet.Set_Draw_Mode_Off (Palet.OD_Magnitude);
      Palet.Set_Point_Size (0.1);

      --          GL_Util.GL_Color_3fm (1.0, 0.0, 0.0);

      --          Direction := (Cos (Vertical_Angle) * Sin (Horizontal_Angle),
      --                        Sin (Vertical_Angle),
      --                        Cos (Vertical_Angle) * Cos (Horizontal_Angle));
      --          Right := (Sin (Horizontal_Angle - Half_Pi), 0.0,
      --                    Cos (Horizontal_Angle - Half_Pi));
      --          Up := Singles.Cross_Product (Right, Direction);

      --          Maths.Init_Lookat_Transform (Camera_Position, Direction, Up, View_Matrix);

      --          Shader_Manager.Set_Light_Position_Vector ((0.0, 0.0, 10.0));
      --          Shader_Manager.Set_Drawing_Colour (Red);

      --          Shader_Manager.Set_Ambient_Colour (Green);
      --          Shader_Manager.Set_Diffuse_Colour (BLue);
      --           Test_MV := Multivectors.New_Vector;
      --           GA_Utilities.Print_Multivector ("Display, Test_MV:", Test_MV);
--        for count in 1 .. Points.Num_Points loop
         --           Label := Silo.Set_Data (Ada.Strings.Unbounded.To_Unbounded_String (Integer'Image (count)),
         --                                   Label_Position);
         --           Silo.Push (Label);

--           Point_Position := Points.Normalized_Points (count);
         --  Point_Position (L1, L2, C1, C2, C3, P1)
--           C3GA_Draw.Draw (Render_Graphic_Program, Model_View_Matrix,
--                           Point_Position);
--        end loop;
      --              GA_Utilities.Print_Multivector ("Display, Point_Position 1:", Points.L0_Normalized_Points (1));
      --              GA_Utilities.Print_Multivector ("Display, Point_Position 2:", Points.L0_Normalized_Points (2));
      --          GA_Utilities.Print_Multivector ("Display, L1.L1 ", Dot (Points.L1, Points.L1));
      --          GA_Utilities.Print_Multivector ("Display, L2.L2 ", Dot (Points.L1, Points.L2));
      --          GA_Utilities.Print_Multivector ("Display, L1 ", Points.L1);
      --          GA_Utilities.Print_Multivector ("Display, L2 ", Points.L2);
      if not Pick_Manager.Pick_Active then
--           GA_Utilities.Print_Multivector ("Main_Loop.Display Points.L1.", Points.L1);
         Put_Line ("Main_Loop.Display drawing aLine.");
         Shader_Manager.Set_Ambient_Colour (Red);
         aLine := Draw_1_1.Draw_Line (Render_Graphic_Program,
                                      Points.L1, Points.L2);
         Put_Line ("Main_Loop.Display drawing aCircle.");
         Shader_Manager.Set_Ambient_Colour (Green);
         aCircle := Draw_1_1.Draw_Circle (Render_Graphic_Program,
                                          Points.C1, Points.C2, Points.C3);
         GA_Utilities.Print_Multivector_String ("Main_Loop.Display aCircle",
                  aCircle, Blade_Types.Basis_Names_C3GA);

         --  N_E3_Vec is a direction vector
         aDual_Plane := Draw_1_1.New_Dual_Plane (Points.P1, N_E3_Vec);

         --  draw reflected line (magenta)
         Put_Line ("Main_Loop.Display drawing reflected line.");
         Shader_Manager.Set_Ambient_Colour (Magenta);
         Draw_1_1.Draw_Reflected_Line (Render_Graphic_Program, aLine, aDual_Plane);

         Put_Line ("Main_Loop.Display drawing reflected circle.");
         --  draw reflected circle (blue)
         Shader_Manager.Set_Ambient_Colour (Blue);
         Draw_1_1.Draw_Reflected_Circle
           (Render_Graphic_Program, aCircle, aDual_Plane);

         --  compute rotation versor
         DL := 0.5 * Phi * Dual (aLine, Metric.C3_Metric);
         R_Versor := To_TRversor (DL);
         R_Versor := C3GA_Utilities.Exp (DL);
         GA_Utilities.Print_Multivector_String
              ("Main_Loop.Display R_Versor",
                R_Versor, Blade_Types.Basis_Names_C3GA);

--           Put_Line ("Main_Loop.Display drawing rotated circle.");
--           --  draw rotated circle
--           Shader_Manager.Set_Ambient_Colour (Green);
--           Translation_Matrix := Maths.Translation_Matrix ((0.0, 2.0, 0.0));
--           Shader_Manager.Set_Model_View_Matrix
--                (Translation_Matrix * Model_View_Matrix);
--           Circle_Rotated := Draw_1_1.Draw_Rotated_Circle
--             (Render_Graphic_Program, aCircle, R_Versor);
--
--           Put_Line ("Main_Loop.Display drawing reflected, rotated circle.");
--           --  draw reflected, rotated circle (blue)
--           Shader_Manager.Set_Ambient_Colour (Blue);
--           Draw_1_1.Draw_Reflected_Circle
--             (Render_Graphic_Program, Circle_Rotated, aDual_Plane);

--           LR := C3GA_Utilities.Log_TR_Versor (R_Versor);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   --     procedure Draw_Text (Window_Width, Window_Height : Glfw.Size;
   --                          theText          : String;
   --                          Render_Program   : GL.Objects.Programs.Program;
   --                          Text_X, Text_Y   : GL.Types.Single;
   --                          Text_Scale       : GL.Types.Single) is
   --        Text_Dimesions_ID       : GL.Uniforms.Uniform;
   --        Text_Proj_Matrix_ID     : GL.Uniforms.Uniform;
   --        Text_Texture_ID         : GL.Uniforms.Uniform;
   --        Text_Colour_ID          : GL.Uniforms.Uniform;
   --        Text_Projection_Matrix  : GL.Types.Singles.Matrix4;
   --        Text_Colour             : constant Colors.Color := Black;
   --     begin
   --        Text_Shader_Locations (Render_Program, Text_Proj_Matrix_ID,
   --                               Text_Texture_ID, Text_Dimesions_ID, Text_Colour_ID);
   --        Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
   --                                           Single (Window_Width), 0.1, -100.0,
   --                                           Text_Projection_Matrix);
   --        Text_Management.Render_Text (Render_Program, theText, Text_X, Text_Y,
   --                                     Text_Scale, Text_Colour, Text_Texture_ID,
   --                                     Text_Proj_Matrix_ID, Text_Dimesions_ID,
   --                                     Text_Colour_ID, Text_Projection_Matrix);
   --     exception
   --        when anError :  others =>
   --           Put_Line ("An exception occurred in Main_Loop.Draw_Text.");
   --           raise;
   --     end Draw_Text;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Render_Program : out GL.Objects.Programs.Program) is
      --                              Render_Text_Program    : out GL.Objects.Programs.Program) is;
      --          use GL.Objects.Buffers;
      --        Font_File : string := "../fonts/Helvetica.ttc";
      Sphere : Geosphere.Geosphere;
--        Depth  : constant integer := 3;
   begin
      Shader_Manager.Init (Render_Program);
      Palet.Set_Point_Size (1.0);
--        Put_Line ("Main_Loop.Setup_Graphic calling GS_Compute.");
--        Geosphere.GS_Compute (Sphere, Depth);
--        Put_Line ("Main_Loop.Setup_Graphic calling Set_Current_Sphere.");
      Palet.Set_Current_Sphere (Sphere);

      --        Render_Text_Program := Program_Loader.Program_From
      --          ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
      --           Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
      --
      --        Text_Management.Setup (Font_File);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup_Graphic.");
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   --      procedure Text_Shader_Locations (Render_Text_Program : GL.Objects.Programs.Program;
   --                                       Projection_Matrix_ID, Texture_ID,
   --                                       Text_Dimesions_ID, Colour_ID : out GL.Uniforms.Uniform) is
   --      begin
   --          Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
   --            (Render_Text_Program, "mvp_matrix");
   --          Texture_ID := GL.Objects.Programs.Uniform_Location
   --            (Render_Text_Program, "text_sampler");
   --          Text_Dimesions_ID := GL.Objects.Programs.Uniform_Location
   --            (Render_Text_Program, "dimensions");
   --          Colour_ID := GL.Objects.Programs.Uniform_Location
   --            (Render_Text_Program, "text_colour");
   --      end Text_Shader_Locations;

   --  -------------------------------------------------------------------------

   use Glfw.Input;
   Render_Graphic_Program : GL.Objects.Programs.Program;
   --     Render_Text_Program : GL.Objects.Programs.Program;
   Running : Boolean := True;
   Key_Now : Button_State;
begin
   Multivectors.Set_Geometry (Multivectors.C3_Geometry);
   Utilities.Clear_Background_Colour_And_Depth (White);
   Main_Window.Set_Input_Toggle (Sticky_Keys, True);
   Glfw.Input.Poll_Events;
   Setup_Graphic (Render_Graphic_Program);
   --     Setup_Graphic (Main_Window, Render_Graphic_Program, Render_Text_Program);
   while Running loop
      --  Swap_Buffers first to display background colour on start up.
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Display (Main_Window, Render_Graphic_Program);
      --        Display (Main_Window, Render_Graphic_Program, Render_Text_Program);
      --        Delay (0.2);
      Glfw.Input.Poll_Events;
      Key_Now := Main_Window.Key_State (Glfw.Input.Keys.Space);
      if not Key_Pressed and Key_Now = Glfw.Input.Pressed then
         Key_Pressed := True;
      else
         Key_Pressed := Key_Now = Glfw.Input.Pressed;
      end if;
      --     Delay (3.0);
      Running := Running and then
        not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;
end Main_Loop;
