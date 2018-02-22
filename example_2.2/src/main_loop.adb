
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
with GA_Utilities;
with GL_Util;
with E2GA;
with E2GA_Draw;
with E3GA;
with E3GA_Utilities;
with GA_Maths;
with Multivectors;

with Silo;
with Text_Management;

with Graphic_Data;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   subtype tVec4f is Singles.Vector4;

   Black          : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Red            : constant Colors.Color := (1.0, 0.0, 0.0, 1.0);
   Green          : constant Colors.Color := (0.0, 1.0, 0.0, 1.0);
   Blue           : constant Colors.Color := (0.0, 0.0, 1.0, 1.0);
   Yellow         : constant Colors.Color := (1.0, 1.0, 0.0, 1.0);
   White          : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
   Key_Pressed    : boolean := False;

   Prev_Mouse_Pos    : E3GA.Vector_Coords_3D := (0.0, 0.0, 0.0);
   Rotate_Model      : Boolean := False;
   Rotate_Model_Out_Of_Plane  : boolean := False;
   Init_Model_Needed : Boolean := True;
   Model_Name        : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("Sphere");
   Model_Rotor          : Multivectors.Rotor;

   Polygons_2D       : Multivectors.Vector;
   Vertices_2D       : Multivectors.Vector;
--
   Prev_Statistics_Model_Name : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("");

   --  -------------------------------------------------------------------------

   procedure Display (Window         : in out Glfw.Windows.Window;
                      Render_Program : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Types.Colors;
      use GL.Types.Singles;     --  for matrix multiplication

      use Maths.Single_Math_Functions;

      use E2GA;
      use GA_Maths;
      use GA_Maths.Float_Functions;

      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      Pick                : GL_Util.GL_Pick;
--        Translation_Matrix  : GL.Types.Singles.Matrix4;
      Model_View_Matrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      Vertex_Buffer       : GL.Objects.Buffers.Buffer;

   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (White);

      if Init_Model_Needed then
         Graphic_Data.Get_GLUT_Model_2D (Render_Program, Model_Name, Model_Rotor);
         Init_Model_Needed := False;
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Window : in out Glfw.Windows.Window;
                            Render_Program : out GL.Objects.Programs.Program) is
      use Glfw.Input;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      --  Line width > 1.0 fails. It may be clamped to an implementation-dependent maximum.
      --  Call glGet with GL_ALIASED_LINE_WIDTH_RANGE to determine the
      --  maximum width.
      GL.Rasterization.Set_Line_Width (1.0);
      GA_Draw.Set_Point_Size (0.9);
--        GA_Draw.Set_Point_Size (0.005);

      Model_Rotor := Multivectors.New_Rotor;

      Render_Program := Program_Loader.Program_From
        ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
          Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Setup_Graphic.");
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Render_Program : GL.Objects.Programs.Program;
   Running : Boolean := True;
   Key_Now : Button_State;
begin
   Utilities.Clear_Background_Colour_And_Depth (White);
   Main_Window.Set_Input_Toggle (Sticky_Keys, True);
   Glfw.Input.Poll_Events;
   Setup_Graphic (Main_Window, Render_Program);
   while Running loop
      --  Swap_Buffers first to display background colour on start up.
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Display (Main_Window, Render_Program);
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
