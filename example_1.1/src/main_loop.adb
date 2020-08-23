
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

with E3GA;
with C3GA_Utilities;
with GA_Maths;
with Multivectors;
with Palet;
with Pick_Manager;

with Draw_1_1;
with Shader_Manager;

with Points;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Red            : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
   Green          : constant GL.Types.Singles.Vector4  := (0.0, 0.5, 0.0, 1.0);
   Blue           : constant GL.Types.Singles.Vector4  := (0.0, 0.0, 0.5, 1.0);
   Magenta        : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 1.0, 1.0);
   Yellow         : constant GL.Types.Singles.Vector4 := (1.0, 1.0, 0.0, 0.5);
   White          : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
   Key_Pressed    : boolean := False;

   --      Rotate_Model    : boolean := False;
   --      Rotate_Model_Out_Of_Plane  : boolean := False;
   --      Pick            : GL_Util.GL_Pick;

     --  -------------------------------------------------------------------------

   procedure Display (Window                 : in out Glfw.Windows.Window;
                      Render_Graphic_Program : GL.Objects.Programs.Program) is
      use GL.Types.Singles;     --  for matrix multiplication
      use Multivectors;

      --          Position_X        : integer := 0;
      --          Position_Y        : single := 160.0;
      aLine               : Multivectors.Line;
      aCircle             : Circle;
      Rotated_Circle      : Circle;
      aDual_Plane         : Dual_Plane;
      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      --          Pick                : GL_Util.GL_Pick;
      Width               : GL.Types.Single;
      Height              : GL.Types.Single;
      Translation_Matrix  : Matrix4 := Identity4;
      Projection_Matrix   : Matrix4 := Identity4;
      View_Matrix         : Matrix4 := Identity4;
      View_Angle          : constant Maths.Degree := 50.0;
      N_E3_Vec            : constant E3GA.E3_Vector := (0.0, 1.0, 0.0);
      Phi                 : constant Float := 0.5 * GA_Maths.Pi;
      R_Versor            : TR_Versor;
      DL                  : Dual_Line;
      LR                  : Multivector;
      Alpha               : Float;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Width := Single (Window_Width);
      Height := Single (Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Width), Int (Height));
      Utilities.Clear_Background_Colour_And_Depth (White);

      --          GL_Util.Load_Pick_Matrix;

      Maths.Init_Perspective_Transform
        (View_Angle, Width, Height, 0.1, -100.0, Projection_Matrix);
      Shader_Manager.Set_Projection_Matrix (Projection_Matrix);

      Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -20.0));

      View_Matrix := Translation_Matrix * View_Matrix;
      Shader_Manager.Set_View_Matrix (View_Matrix);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);

      Shader_Manager.Set_Line_Width (2.0);

      Palet.Set_Draw_Mode_Off (Palet.OD_Magnitude);
      Palet.Set_Point_Size (0.1);

      --              GA_Utilities.Print_Multivector ("Display, Point_Position 1:", Points.L0_Normalized_Points (1));
      --              GA_Utilities.Print_Multivector ("Display, Point_Position 2:", Points.L0_Normalized_Points (2));
      --          GA_Utilities.Print_Multivector ("Display, L1.L1 ", Dot (Points.L1, Points.L1));
      --          GA_Utilities.Print_Multivector ("Display, L2.L2 ", Dot (Points.L1, Points.L2));
      --          GA_Utilities.Print_Multivector ("Display, L1 ", Points.L1);
      --          GA_Utilities.Print_Multivector ("Display, L2 ", Points.L2);
      if not Pick_Manager.Pick_Active then
         Shader_Manager.Set_Model_Matrix (Identity4);
         New_Line;
         Put_Line ("Main_Loop.Display drawing a Line.");
         Shader_Manager.Set_Ambient_Colour (Red);
         aLine := Draw_1_1.Draw_Line (Render_Graphic_Program,
                                      Points.L1, Points.L2);
         New_Line;
         Put_Line ("Main_Loop.Display drawing a Circle.");
         Shader_Manager.Set_Ambient_Colour (Green);
         Palet.Set_Draw_Mode_On (Palet.OD_Orientation);
         aCircle := Draw_1_1.Draw_Circle (Render_Graphic_Program,
                                          Points.C1, Points.C2, Points.C3);
         Palet.Set_Draw_Mode_Off (Palet.OD_Orientation);
         --  N_E3_Vec is a direction vector
         aDual_Plane := Draw_1_1.New_Dual_Plane (Points.P1, N_E3_Vec);

         --  draw reflected line (magenta)
         New_Line;
         Put_Line ("Main_Loop.Display drawing a reflected line.");
         Shader_Manager.Set_Ambient_Colour (Magenta);
         Draw_1_1.Draw_Reflected_Line (Render_Graphic_Program, aLine, aDual_Plane);

         New_Line;
         Put_Line ("Main_Loop.Display drawing reflected circle.");
         --  draw reflected circle (blue)
         Shader_Manager.Set_Ambient_Colour (Blue);
         Palet.Set_Draw_Mode_On (Palet.OD_Orientation);
         Draw_1_1.Draw_Reflected_Circle
           (Render_Graphic_Program, aCircle, aDual_Plane);
         Palet.Set_Draw_Mode_Off (Palet.OD_Orientation);

         --  compute rotation versor
         DL := 0.5 * Phi * To_Dual_Line (Dual (aLine));
         R_Versor := To_TRversor (Multivectors.Exp (DL));

         New_Line;
         Put_Line ("Main_Loop.Display drawing rotated circle.");
         --  draw rotated circle
         Shader_Manager.Set_Ambient_Colour (Green);
         Translation_Matrix := Maths.Translation_Matrix ((0.0, 2.0, 10.0)) *
           Translation_Matrix;
         Shader_Manager.Set_Translation_Matrix (Translation_Matrix);
         Palet.Set_Draw_Mode_On (Palet.OD_Orientation);
         Rotated_Circle := Draw_1_1.Draw_Rotated_Circle
           (Render_Graphic_Program, aCircle, R_Versor);

         New_Line;
         Put_Line ("Main_Loop.Display drawing reflected, rotated circle.");
         --  draw reflected, rotated circle (blue)
         Shader_Manager.Set_Ambient_Colour (Blue);
         Draw_1_1.Draw_Reflected_Circle
           (Render_Graphic_Program, Rotated_Circle, aDual_Plane);
         Palet.Set_Draw_Mode_Off (Palet.OD_Orientation);

         --  Draw interpolated circles
         LR := C3GA_Utilities.Log_TR_Versor (R_Versor);
         Alpha := 0.0;
         while Alpha < 1.0 loop
             --   compute interpolated rotor
             R_Versor := To_TRversor (Multivectors.Exp (Alpha * LR));
             --  draw rotated circle (light green)
             Shader_Manager.Set_Ambient_Colour ((0.5, 1.0, 0.5, 1.0));
             Rotated_Circle := Draw_1_1.Draw_Rotated_Circle
               (Render_Graphic_Program, aCircle, R_Versor);
             --  draw reflected, rotated circle (light blue)
             Shader_Manager.Set_Ambient_Colour ((0.5, 0.5, 1.0, 1.0));
             Draw_1_1.Draw_Reflected_Circle
               (Render_Graphic_Program, Rotated_Circle, aDual_Plane);
            Alpha := Alpha + 0.1;
         end loop;

         Translation_Matrix := Maths.Translation_Matrix ((0.0, -2.0, 10.0)) *
           Translation_Matrix;
         Shader_Manager.Set_Translation_Matrix (Translation_Matrix);

         --  Draw plane (yellow)
         New_Line;
         Put_Line ("Main_Loop.Display drawing plane.");
         Shader_Manager.Set_Ambient_Colour (Yellow);
         Draw_1_1.Draw_Plane (Render_Graphic_Program, aDual_Plane);
         Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -10.0)) *
           Translation_Matrix;
         Shader_Manager.Set_Translation_Matrix (Translation_Matrix);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Render_Program : out GL.Objects.Programs.Program) is
   begin
      Shader_Manager.Init (Render_Program);
      Palet.Set_Point_Size (1.0);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup_Graphic.");
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

    use Glfw.Input;
   Render_Graphic_Program : GL.Objects.Programs.Program;
   Running : Boolean := True;
   Key_Now : Button_State;
begin
   Multivectors.Set_Geometry (Multivectors.C3_Geometry);
   Utilities.Clear_Background_Colour_And_Depth (White);
   Main_Window.Set_Input_Toggle (Sticky_Keys, True);
   Glfw.Input.Poll_Events;
   Setup_Graphic (Render_Graphic_Program);

   while Running loop
      --  Swap_Buffers first to display background colour on start up.
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Display (Main_Window, Render_Graphic_Program);
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
