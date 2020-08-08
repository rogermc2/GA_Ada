
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with C3GA;
with GA_Draw;

with Shader_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Rendering_Program  : GL.Objects.Programs.Program;
   Vertex_Array       :  GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   procedure Test_Draw_Line is
      use GL.Types;
      use GL.Types.Singles;
      Back_Colour : constant GL.Types.Colors.Color := (0.7 , 0.7, 0.0, 1.0);
      MV_Matrix   : constant Matrix4 := Identity4;
      aPoint      : constant C3GA.Vector_E3GA := (-0.5 , -0.7, 0.0);
      Direction   : constant C3GA.Vector_E3GA := (0.5 , 0.5, 0.0);
      Weight      : constant Float := 2.0;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

      GA_Draw.Draw_Line (Rendering_Program, MV_Matrix, aPoint, Direction, Weight);
--        GL.Objects.Programs.Use_Program (Rendering_Program);
--        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Points, 0, 1);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Test_Draw_Line.");
         raise;
   end Test_Draw_Line;

   --  ----------------------------------------------------------------------------

   procedure Setup_Graphic is
   begin
      Shader_Manager.Init (Rendering_Program);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;
      --  Point size is set in the vertex shader
      GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup_Graphic;
   while Running loop
      Test_Draw_Line;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when Program_Loader.Shader_Loading_Error =>
      --  message was already written to stdout
      null;
end Main_Loop;
