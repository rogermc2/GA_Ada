
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

with GA_Draw;
with GL_Util;
with E2GA;
with E2GA_Draw;
with E3GA;
with E3GA_Utilities;
with GA_Maths;
with Multivector;

with Silo;

with Text_Management;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   subtype tVec4f is Singles.Vector4;

   Black          : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Red            : constant Colors.Color := (1.0, 0.0, 0.0, 1.0);
   Green          : constant Colors.Color := (0.0, 1.0, 0.0, 1.0);
   Blue           : constant Colors.Color := (0.0, 0.0, 1.0, 1.0);
   Yellow         : constant Colors.Color := (1.0, 1.0, 0.0, 1.0);
   Back_Colour    : constant Colors.Color := (0.7, 0.7, 0.7, 1.0);
   Key_Pressed    : boolean := False;
   Parallelogram  : boolean := True;

   procedure Draw_Parallelogram (Render_Program : GL.Objects.Programs.Program;
                                 MV_Matrix      : GL.Types.Singles.Matrix4;
                                 V2, V3, V4     : E2GA.Vector;
                                 Colour         : GL.Types.Colors.Color);
   procedure Draw_Text (Window_Width, Window_Height : Glfw.Size;
                        theText         : String;
                        Render_Program  : GL.Objects.Programs.Program;
                        Text_X, Text_Y  : GL.Types.Single;
                        Text_Scale      : GL.Types.Single);
   procedure Text_Shader_Locations (Render_Text_Program : GL.Objects.Programs.Program;
                                    Projection_Matrix_ID, Texture_ID, Text_Dimesions_ID,
                                    Colour_ID : out GL.Uniforms.Uniform);

   --  -------------------------------------------------------------------------

   procedure Display (Window                 : in out Glfw.Windows.Window;
                      Render_Graphic_Program : GL.Objects.Programs.Program;
                      Render_Text_Program    : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Types.Colors;
      use GL.Types.Singles;     --  for matrix multiplication

      use Maths.Single_Math_Functions;

      use E2GA;
      use GA_Maths;
      use GA_Maths.Float_Functions;

      Label             : Silo.Label_Data;
      Label_Position    : GL.Types.Singles.Vector2;
      --  How many bivectors? what spacing between them?:
      Entry_Width       : constant single := 2.8;
      Entry_Height      : constant single := 3.5;
      Num_Bivector_X    : constant integer := 6;
      Num_Bivector_Y    : constant integer := 4;
      Scale             : constant float := 40.0;
      Scale_S           : constant single := single (Scale);
      Text_Scale        : constant single := 0.12;
      Position_X        : integer := 0;
      Position_Y        : single := 100.0;

      A                 : float := 0.0;
      BV                : E2GA.Bivector;
      E11               : constant float := E3GA.Get_Coord_1 (E3GA.e1);
      E12               : constant float := E3GA.Get_Coord_2 (E3GA.e1);
      E21               : constant float := E3GA.Get_Coord_1 (E3GA.e2);
      E22               : constant float := E3GA.Get_Coord_2 (E3GA.e2);
      Step              : constant float :=
        GA_Maths.Two_Pi / float (Num_Bivector_X * Num_Bivector_Y);
      V1                    : E2GA.Vector; --  2D vector (0, 0), (1, 0)
      V2                    : E2GA.Vector;

      Text_Coords           : GA_Maths.Array_3D := (0.0, 0.0, 0.0);
      Window_Width          : Glfw.Size;
      Window_Height         : Glfw.Size;
      Translation_Matrix    : GL.Types.Singles.Matrix4;
      BV_Translation_Matrix : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      Model_View_Matrix     : GL.Types.Singles.Matrix4;
      Projection_Matrix     : GL.Types.Singles.Matrix4;
      Vertex_Buffer         : GL.Objects.Buffers.Buffer;
      Text                  : Ada.Strings.Unbounded.Unbounded_String;
      Text_X                : GL.Types.Single := 50.0;
      Text_Y                : GL.Types.Single := 50.0;

   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

      --  Set scale and position of first diagram
      Translation_Matrix := Maths.Translation_Matrix
        ((Entry_Width * Scale_S / 2.0,
          (Single (Num_Bivector_Y)) * Entry_Height * Scale_S / 2.0 - Position_Y, 0.0));
     Model_View_Matrix := Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S));
      GA_Draw.Set_Projection_Matrix (Projection_Matrix);
      --  The final MVP matrix is set up in the draw routines
      V1 := New_Vector (E11, E12);
      while A < Two_Pi - 0.1 loop
         --  E2GA.e2 vector (0, 0), (0, 1)
         V2 := New_Vector (Cos (A) * E11 - Sin (A) * E21,
                     Cos (A) * E21 - Sin (A) * E22);
         Model_View_Matrix := Translation_Matrix * Model_View_Matrix;
         E2GA_Draw.Draw_Vector (Render_Graphic_Program, Model_View_Matrix,
                         V1, Red, Scale);
         E2GA_Draw.Draw_Vector (Render_Graphic_Program, Model_View_Matrix,
                         V2, Green, Scale);

         BV := Multivector.Outer_Product (V1, V2);
         if Parallelogram then
            --  Draw Quad with vertices: origin -> V1 -> V1+V2 -> V2
            Draw_Parallelogram (Render_Graphic_Program, Model_View_Matrix,
                                V1, V1 + V2, V2, Blue);
         else
            BV_Translation_Matrix := Translation_Matrix * BV_Translation_Matrix;
            E2GA_Draw.Draw_Bivector (Render_Graphic_Program, BV_Translation_Matrix,
                            BV, Yellow);
         end if;

         if A < Pi - 0.1 then
            Text_Coords (2) := 0.35 * float (Entry_Height);
         else
            Text_Coords (1) := - 0.25 * float (Entry_Height);
            Text_Coords (2) := 0.4 * float (Entry_Height);
         end if;

         GL_Util.Viewport_Coordinates (Text_Coords, Model_View_Matrix,
                                       Projection_Matrix, Label_Position);
         --  store bivector label:
         E3GA_Utilities.Print_Multivector ("BV", BV);
         Put_Line ("Display bivector label: " & E2GA.Bivector_String (BV));
         Label := Silo.Set_Data (Ada.Strings.Unbounded.To_Unbounded_String
                                 (E2GA.Bivector_String
                                    (BV)), Label_Position);
         Silo.Push (Label);

         --  Set X position of next diagram
         Translation_Matrix := Maths.Translation_Matrix ((Entry_Width * Scale_S,
                                                        0.0, 0.0));
         if Position_X < Num_Bivector_X - 1 then
            Position_X := Position_X + 1;
         else
            --  Set X and Y positions of next diagram
            Position_X := 0;
            Position_Y := Position_Y + Entry_Height;
            Translation_Matrix := Maths.Translation_Matrix
              ((-Single (Num_Bivector_X) * Entry_Width * Scale_S,
                Position_Y, 0.0)) * Translation_Matrix;
         end if;
         A := A + Step;
      end loop;

      for i in 1 .. Silo.Size loop
         Silo.Get_Data (Text, Label_Position);
         Draw_Text (Window_Width, Window_Height,
                    Ada.Strings.Unbounded.To_String (Text), Render_Text_Program,
                    Label_Position (GL.X), Label_Position (GL.Y), Text_Scale);
      end loop;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Draw_Parallelogram (Render_Program  : GL.Objects.Programs.Program;
                                 MV_Matrix       : GL.Types.Singles.Matrix4;
                                 V2, V3, V4      : E2GA.Vector;
                                 Colour          : GL.Types.Colors.Color) is
      use GL.Objects.Buffers;
      use GL.Types.Colors;
      Colour_Location      : GL.Uniforms.Uniform;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Vertex_Buffer        : Buffer;
      Element_Buffer       : Buffer;
      Vertices             : Singles.Vector3_Array (1 .. 4) :=
        ((0.0, 0.0, 0.0),                       --  Bottom-left
         GL_Util.To_GL (V2),   --  Bottom-right,
         GL_Util.To_GL (V3),   --  Top-right,
         GL_Util.To_GL (V4));   --  Top-left
      Elements             : GL.Types.Int_Array (1 .. 6) := (0, 1, 2,
                                                             2, 3, 0);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;

      GA_Draw.Set_Projection_Matrix (Projection_Matrix );

      GA_Draw.Graphic_Shader_Locations (Render_Program, MV_Matrix_ID, Projection_Matrix_ID,
                                Colour_Location);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Elements, Static_Draw);

      GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
      GL.Uniforms.Set_Single (MV_Matrix_ID, MV_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Toggles.Disable (GL.Toggles.Cull_Face);
      GL.Objects.Buffers.Draw_Elements (Triangles, 6, UInt_Type);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Draw_Parallelogram.");
         raise;
   end Draw_Parallelogram;

   --  ------------------------------------------------------------------------

   procedure Draw_Text (Window_Width, Window_Height : Glfw.Size;
                        theText          : String;
                        Render_Program   : GL.Objects.Programs.Program;
                        Text_X, Text_Y   : GL.Types.Single;
                        Text_Scale       : GL.Types.Single) is
      Text_Dimesions_ID       : GL.Uniforms.Uniform;
      Text_Proj_Matrix_ID     : GL.Uniforms.Uniform;
      Text_Texture_ID         : GL.Uniforms.Uniform;
      Text_Colour_ID          : GL.Uniforms.Uniform;
      Text_Projection_Matrix  : GL.Types.Singles.Matrix4;
      Text_Colour             : constant Colors.Color := Black;
   begin
      Text_Shader_Locations (Render_Program, Text_Proj_Matrix_ID,
                             Text_Texture_ID, Text_Dimesions_ID, Text_Colour_ID);
      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                                         Single (Window_Width), 0.1, -100.0,
                                         Text_Projection_Matrix);
      Text_Management.Render_Text (Render_Program, theText, Text_X, Text_Y,
                                   Text_Scale, Text_Colour, Text_Texture_ID,
                                   Text_Proj_Matrix_ID, Text_Dimesions_ID,
                                   Text_Colour_ID, Text_Projection_Matrix);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Main_Loop.Draw_Text.");
         raise;
   end Draw_Text;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Window : in out Glfw.Windows.Window;
                            Render_Graphic_Program : out GL.Objects.Programs.Program;
                            Render_Text_Program    : out GL.Objects.Programs.Program) is
      use Glfw.Input;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;
      Font_File : string := "../fonts/Helvetica.ttc";
   begin
      Render_Graphic_Program := Program_Loader.Program_From
        ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

      Render_Text_Program := Program_Loader.Program_From
        ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));

      Text_Management.Setup (Font_File);
      GA_Draw.Set_Point_Size (0.005);
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
   Render_Text_Program : GL.Objects.Programs.Program;
   Running : Boolean := True;
   Key_Now : Button_State;
begin
   Main_Window.Set_Input_Toggle (Sticky_Keys, True);
   Glfw.Input.Poll_Events;
   GL.Toggles.Disable (GL.Toggles.Cull_Face);
   Setup_Graphic (Main_Window, Render_Graphic_Program, Render_Text_Program);
   while Running loop
      Display (Main_Window, Render_Graphic_Program, Render_Text_Program);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Key_Now := Main_Window.Key_State (Glfw.Input.Keys.Space);
      if not Key_Pressed and Key_Now = Glfw.Input.Pressed then
         Parallelogram := not Parallelogram;
         Key_Pressed := True;
      else
         Key_Pressed := Key_Now = Glfw.Input.Pressed;
      end if;
      Running := Running and then
        not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;
end Main_Loop;
