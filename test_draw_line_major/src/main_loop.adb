
--  with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Culling;
with GL.Toggles;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with GL.Window;
with Glfw.Windows.Context;

--  with Maths;
with Program_Loader;
with Utilities;

with C3GA;
with C3GA_Draw;
with GA_Draw;
with Multivectors;
with Palet;

with Points;
with Shader_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Red            : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
   Green          : constant GL.Types.Singles.Vector4  := (0.0, 0.5, 0.0, 1.0);
   Blue           : constant GL.Types.Singles.Vector4  := (0.0, 0.0, 0.5, 1.0);

   Rendering_Program  : GL.Objects.Programs.Program;
   Vertex_Array       :  GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   procedure Test_Draw_Line is
      use GL.Types;
      use GL.Types.Singles;
      --          use Maths.Single_Math_Functions;
      use Multivectors;
      Back_Colour    : constant GL.Types.Colors.Color := (0.7 , 0.7, 0.7, 1.0);
      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
      --          View_Direction : Vector3;
      --          Right          : Vector3;
      --          Up             : Vector3;
      --          Half_Pi             : constant Single := 0.5 * Ada.Numerics.Pi;
      --          Horizontal_Angle    : constant Single := Ada.Numerics.Pi;
      --          Vertical_Angle      : constant Single := 0.0;

      Point_Position : Normalized_Point := New_Normalized_Point;
      aLine          : Multivectors.Line := New_MV_Line;
      MV_Matrix      : constant Matrix4 := Identity4;
      Palet_Data     : Palet.Colour_Palet;
      aPoint         : constant C3GA.Vector_E3GA := (0.0 , 0.25, 0.0);  --  1.0 , 0.25, 0.0
      Direction      : constant C3GA.Vector_E3GA := (0.25 , 0.0, 0.0);  --  0.25 , 0.0, 0.0
--        aPoint         : constant C3GA.Vector_E3GA := (0.0 , 0.0, -0.2);
--        Direction      : constant C3GA.Vector_E3GA := (0.1 , 0.1, 0.8);
--        Weight         : constant Float := 1.0;
--        P1             : constant Normalized_Point :=
--                           C3GA.Set_Normalized_Point (0.0 , 0.0, -0.2);
--        P2             : constant Normalized_Point :=
--                           C3GA.Set_Normalized_Point (0.9 , 0.9, -0.2);
   begin
      Main_Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      --          View_Direction := (Cos (Vertical_Angle) * Sin (Horizontal_Angle),
      --                             Sin (Vertical_Angle),
      --                             Cos (Vertical_Angle) * Cos (Horizontal_Angle));
      --          Right := (Sin (Horizontal_Angle - Half_Pi), 0.0,
      --                    Cos (Horizontal_Angle - Half_Pi));
      --          Up := Singles.Cross_Product (Right, Direction);

      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);

--        GL.Objects.Programs.Use_Program (Rendering_Program);
      Shader_Manager.Set_Drawing_Colour (Red);
      Shader_Manager.Set_Ambient_Colour (Green);
      Shader_Manager.Set_Diffuse_Colour (BLue);
      for count in 1 .. Points.Num_Points loop
         Point_Position := Points.Normalized_Points (count);
      end loop;
      aLine := C3GA.Set_Line (Points.L1, Points.L2);
--        aLine := C3GA.Set_Line (P1, P2);

      C3GA_Draw.Draw (Rendering_Program, MV_Matrix, aLine, Palet_Data);
      GA_Draw.Draw_Line (Rendering_Program, MV_Matrix, aPoint, Direction);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Test_Draw_Line.");
         raise;
   end Test_Draw_Line;

   --  ----------------------------------------------------------------------------

   procedure Setup_Graphic is
   begin
      Shader_Manager.Init (Rendering_Program);

      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;
      --  Point size is set in the vertex shader
      --        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
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
