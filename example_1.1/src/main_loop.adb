
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Immediate;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders.Lists;
with GL.Raster;
with GL.Rasterization;
with GL.Text;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with GL.Window;
with Glfw.Windows.Context;
with Glfw.Windows.Hints;

with Maths;
with Program_Loader;
with Utilities;

with C3GA;
with C3GA_Draw;
with GA_Draw;
with GL_Util;
with E2GA;
with E2GA_Draw;
with E3GA;
with E3GA_Utilities;
with GA_Maths;
with Multivectors;

with Silo;
with Text_Management;

with Points;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   subtype tVec4f is Singles.Vector4;

   Black          : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Red            : constant Colors.Color := (1.0, 0.0, 0.0, 1.0);
   Green          : constant Colors.Color := (0.0, 1.0, 0.0, 1.0);
   Blue           : constant Colors.Color := (0.0, 0.0, 1.0, 1.0);
   Yellow         : constant Colors.Color := (1.0, 1.0, 0.0, 1.0);
   White          : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
   Key_Pressed    : boolean := False;

   --  rotor g_modelRotor(_rotor(1.0f))
   Model_Rotor     : Multivectors.Rotor;
   Rotate_Model    : boolean := False;
   Rotate_Model_Out_Of_Plane  : boolean := False;
   Pick            : GL_Util.GL_Pick;

   --  ni = einf = point at infinity
   aLine  : constant C3GA.Line :=
     C3GA.Set_Line (Points.L1, Points.L2);
   aCircle  : constant C3GA.Circle :=
      C3GA.Set_Circle (Points.C1, Points.C2, Points.C3);
   aDual_Plane  : constant C3GA.Dual_Plane :=
     C3GA.Set_Dual_Plane (Points.P1, Points.n);

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
      use GL.Objects.Buffers;
      use GL.Types.Colors;
      use GL.Types.Singles;     --  for matrix multiplication

      use Maths.Single_Math_Functions;

      use E2GA;
      use GA_Maths;
      use GA_Maths.Float_Functions;

      Scale             : constant float := 40.0;
      Scale_S           : constant single := single (Scale);
      Position_X        : integer := 0;
      Position_Y        : single := 160.0;
      --        Label             : Silo.Label_Data;
      Label_Position    : GL.Types.Singles.Vector2 := (0.0, 0.0);

      V1                : Multivectors.Vector; --  2D vector (0, 0), (1, 0)
      V2                : Multivectors.Vector;

      Point_Position      : C3GA.Normalized_Point;
      --        Text_Coords           : GA_Maths.Array_3D := (0.0, 0.0, 0.0);
      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      Pick                : GL_Util.GL_Pick;
      Translation_Matrix  : GL.Types.Singles.Matrix4;
      Model_View_Matrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
--        Projection_Matrix   : GL.Types.Singles.Matrix4;
      Vertex_Buffer       : GL.Objects.Buffers.Buffer;

   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (White);

      GL_Util.Rotor_GL_Multiply (Model_Rotor, Model_View_Matrix);
      Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -14.0));
      Model_View_Matrix := Maths.Scaling_Matrix (Scale_S) * Model_View_Matrix;
      Model_View_Matrix := Translation_Matrix * Model_View_Matrix;
--        GA_Draw.Set_Projection_Matrix (Projection_Matrix);

      --  The final MVP matrix is set up in the draw routines
--        Set_Coords (V1, E11, E12);
--           Set_Coords (V2, Cos (A) * E11 - Sin (A) * E21,
--                       Cos (A) * E21 - Sin (A) * E22);
      C3GA_Draw.Draw (Render_Graphic_Program, Model_View_Matrix,
                      Multivectors.Multivector (aLine), Red);
--        for count in 1 .. Points.Num_Points loop
--           Label := Silo.Set_Data (Ada.Strings.Unbounded.To_Unbounded_String (Integer'Image (count)),
--                                   Label_Position);
--           Silo.Push (Label);
--           Point_Position := Points.Point_Data (count);
--           C3GA_Utilities.Print_Vector ("Display, Point_Position", Point_Position);
--           C3GA_Draw.Draw (Render_Graphic_Program, Model_View_Matrix,
--                           Multivectors.Multivector (Point_Position), Red);
--        end loop;

   exception
      when anError :  others =>
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

   procedure Setup_Graphic (Window : in out Glfw.Windows.Window;
                            Render_Graphic_Program : out GL.Objects.Programs.Program) is
--                              Render_Text_Program    : out GL.Objects.Programs.Program) is
      use Glfw.Input;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;
--        Font_File : string := "../fonts/Helvetica.ttc";
   begin
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
--        GL.Toggles.Enable (GL.Toggles.Lighting);
--        GL.Toggles.Enable (GL.Toggles.Light0);
      --        GL.Toggles.Enable (GL.Toggles.Normalize);
      --  Line width > 1.0 fails. It may be clamped to an implementation-dependent maximum. Call glGet with GL_ALIASED_LINE_WIDTH_RANGE to determine the maximum width.
      GL.Rasterization.Set_Line_Width (1.0);
      GA_Draw.Set_Point_Size (0.05);
--        GA_Draw.Set_Point_Size (0.005);

      Model_Rotor := Multivectors.New_Rotor;

      Render_Graphic_Program := Program_Loader.Program_From
        ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

--        Render_Text_Program := Program_Loader.Program_From
--          ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
--           Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
--
--        Text_Management.Setup (Font_File);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Setup_Graphic.");
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   procedure Text_Shader_Locations (Render_Text_Program : GL.Objects.Programs.Program;
                                    Projection_Matrix_ID, Texture_ID,
                                    Text_Dimesions_ID, Colour_ID : out GL.Uniforms.Uniform) is
   begin
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "mvp_matrix");
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "text_sampler");
      Text_Dimesions_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "dimensions");
      Colour_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "text_colour");
   end Text_Shader_Locations;

   --  -------------------------------------------------------------------------

   use Glfw.Input;
   Render_Graphic_Program : GL.Objects.Programs.Program;
--     Render_Text_Program : GL.Objects.Programs.Program;
   Running : Boolean := True;
   Key_Now : Button_State;
begin
   Utilities.Clear_Background_Colour_And_Depth (White);
   Main_Window.Set_Input_Toggle (Sticky_Keys, True);
   Glfw.Input.Poll_Events;
   Setup_Graphic (Main_Window, Render_Graphic_Program);
--     Setup_Graphic (Main_Window, Render_Graphic_Program, Render_Text_Program);
   while Running loop
      --  Swap_Buffers first to display background colour on start up.
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Display (Main_Window, Render_Graphic_Program);
--        Display (Main_Window, Render_Graphic_Program, Render_Text_Program);
      Glfw.Input.Poll_Events;
      Key_Now := Main_Window.Key_State (Glfw.Input.Keys.Space);
      if not Key_Pressed and Key_Now = Glfw.Input.Pressed then
         Key_Pressed := True;
      else
         Key_Pressed := Key_Now = Glfw.Input.Pressed;
      end if;
      Running := Running and then
        not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;
end Main_Loop;
