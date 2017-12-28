
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Culling;
with GL.Immediate;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Maths;
with GA_Maths;
with Utilities;

with E3GA;
with E3GA_Utilities;
with Geosphere;
with GL_Util;

package body GA_Draw is

   Palet                : Colour_Palet;
   G_Draw_State         : Draw_State;

   procedure Draw_Circle (Render_Program    : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Scale             : float);

   --  ------------------------------------------------------------------------

   procedure Draw_Base (Render_Program    : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Scale             : Gl.Types.Single) is

      use GL.Objects.Buffers;
      use GL.Types.Singles;
      use GA_Maths.Float_Functions;

      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      GL_e1           : Vector3 := GL_Util.To_GL (E3GA.e1);
      GL_e2           : Vector3 := GL_Util.To_GL (E3GA.e2);
      GL_e3           : Vector3 := GL_Util.To_GL (E3GA.e3);
      Z               : float := 0.0;
      Num_Steps       : constant int := 32;
      Rotor_Step      : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
      Vertex_Buffer   : GL.Objects.Buffers.Buffer;
      Fan             : Singles.Vector3_Array (1 .. Num_Steps + 1);
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Fan (1) := (0.0, 0.0, -0.25);
      for Count in 2 .. Num_Steps + 1 loop
         Fan (Count) := (Single (0.1 * Cos (Z)), Single (0.1 * Sin (Z)), -0.25);
         Z := Z + Rotor_Step;
      end loop;

      Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

      Set_Projection_Matrix (Projection_Matrix);
      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                            First => 0,
                                            Count => Num_Steps);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Base.");
         raise;
   end Draw_Base;

   --  ------------------------------------------------------------------------

   --  Draw_Bivector corresponds to draw.draw_Bivector of draw.cpp
   --  The parameter names correspond of those in draw.h!
   procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                            Translation_Matrix : GL.Types.Singles.Matrix4;
                            Normal, Ortho_1, Ortho_2 : E3GA.Vector;
                            Colour : GL.Types.Colors.Color; Scale  : float := 1.0;
                            Method : Bivector_Method_Type := Draw_Bivector_Circle) is
      use GA_Maths;
      use GL.Types.Singles;
      Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
      Scale_S              : GL.Types.Single := GL.Types.Single (Scale);
      Cords                : Array_3D := (0.0, 0.0, 0.0);
      Translate            : Vector3 :=  (0.0, 0.0, 0.0);
      O2                   : E3GA.Vector := Ortho_2;
      MVP_Matrix           : Matrix4 := Singles.Identity4;
      Scaled               : GL.Types.Single;
      Normed_E2            : E3GA.Scalar;
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;

      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      Set_Projection_Matrix (Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
      GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

      if  Method = Draw_Bivector_Parallelogram and then
        Method = Draw_Bivector_Parallelogram_No_Vectors then
         MVP_Matrix := Translation_Matrix * Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S));
      else
         Normed_E2 := E3GA.Norm_E2 (E3GA.Outer_Product (Ortho_1, Ortho_2));
         Scaled := GL.Types.Single (Scale * Float_Functions.Sqrt (E3GA.Get_Coord (Normed_E2)));
         MVP_Matrix := Translation_Matrix * Maths.Scaling_Matrix ((Scaled, Scaled, Scaled))
           * MVP_Matrix;
      end if;

      case Method is
         when Draw_Bivector_Circle|
              Draw_Bivector_Circle_Outline =>
            Draw_Circle (Render_Program, MVP_Matrix, Scale);
         when others => null;
      end case;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
         raise;
   end Draw_Bivector;

   --  ----------------------------------------------------------------------

   procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                            Translation_Matrix : GL.Types.Singles.Matrix4;
                            Base, Normal, Ortho_1, Ortho_2 : E3GA.Vector;
                            Colour : GL.Types.Colors.Color; Scale  : float := 1.0;
                            Method : Bivector_Method_Type := Draw_Bivector_Circle) is
      use GA_Maths;
      use GL.Types.Singles;

      Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      Rotor_Step           : float := 2.0 * Ada.Numerics.Pi / 64.0;
      Scale_S              : GL.Types.Single := GL.Types.Single (Scale);
      Cords                : Array_3D := (0.0, 0.0, 0.0);
      Translate            : Vector3 :=  (0.0, 0.0, 0.0);
      O2                   : E3GA.Vector := Ortho_2;
      Model_View_Matrix    : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      MVP_Matrix           : Matrix4 := Singles.Identity4;
      Scaled             : GL.Types.Single;
      E2_Norm              : E3GA.Scalar;
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;

      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      Set_Projection_Matrix (Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
      GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

      MVP_Matrix := Model_View_Matrix;
      Cords := E3GA.Get_Coords (Base);
      Translate := (Single (E3GA.Get_Coord_1 (Base)),
                    Single (E3GA.Get_Coord_2 (Base)),
                    Single (E3GA.Get_Coord_3 (Base)));
      E2_Norm := E3GA.Norm_E2 (Base);
      if  E3GA.Get_Coord (E2_Norm) /= 0.0  then
         MVP_Matrix := Maths.Translation_Matrix (Translate) * MVP_Matrix;
      end if;

      if  Method = Draw_Bivector_Parallelogram and then
        Method = Draw_Bivector_Parallelogram_No_Vectors then
         MVP_Matrix := Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S)) * MVP_Matrix;
      else
         E2_Norm := E3GA.Norm_E2 (E3GA.Outer_Product (Ortho_1, Ortho_2));
         Scaled := GL.Types.Single (Scale * Float_Functions.Sqrt (E3GA.Get_Coord (E2_Norm)));
         MVP_Matrix := Maths.Scaling_Matrix ((Scaled, Scaled, Scaled))
           * MVP_Matrix;
      end if;

      Case Method is
         when Draw_Bivector_Circle|
              Draw_Bivector_Circle_Outline =>
            Draw_Circle (Render_Program, Model_View_Matrix, Scale);
         when others => null;
      end case;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
         raise;
   end Draw_Bivector;

   --  ----------------------------------------------------------------------

   --      procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
   --               Model_View_Matrix : GL.Types.Singles.Matrix4;
   --               MV             : E2GA.Multivector;
   --               Colour         : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
   --               Scale          : GL.Types.Single) is
   --      --   L          : boolean;
   --          Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
   --          --   X          : float;
   --          --   Y          : float;
   --          MVP_Matrix : Singles.Matrix4 := Singles.Identity4;
   --          Rotor      : E2GA.Rotor;
   --
   --      begin
   --
   --      GL.Objects.Programs.Use_Program (Render_Program);
   --      Vertex_Array_Object.Initialize_Id;
   --      Vertex_Array_Object.Bind;
   --          --          if E3GA.Norm_E2 (MV).M_C1 = 0.0  then
   --          --              Maths.Translation_Matrix (Get_Coord_1 (Base),
   --          --                Get_Coord_2 (Base), Get_Coord_3 (Base)) * MVP_Matrix;
   --          --          end if;
   --      exception
   --          when anError :  others =>
   --              Put_Line ("An exception occurred in Draw_Object.Draw_Multivector.");
   --              raise;
   --      end Draw_Multivector;

   --  ----------------------------------------------------------------------

   procedure Draw_Circle (Render_Program    : GL.Objects.Programs.Program;
                          Model_View_Matrix : GL.Types.Singles.Matrix4;
                          Scale             : float) is
      use GA_Maths;
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      use GA_Maths.Float_Functions;

      type Circle_Part is (Back_Part, Front_Part, Outline_Part);

      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      GL_e1                : Vector3 := GL_Util.To_GL (E3GA.e1);
      GL_e2                : Vector3 := GL_Util.To_GL (E3GA.e2);
      GL_e3                : Vector3 := GL_Util.To_GL (E3GA.e3);
      Angle                : float := 0.0;
      Num_Steps            : constant int := 256;
      Rotor_Step           : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
      Vertex_Buffer        : GL.Objects.Buffers.Buffer;
      Fan                  : Singles.Vector3_Array (1 .. Num_Steps);

      procedure Draw_Part (Part : Circle_Part) is
         Normal : E3GA.Vector;
         Norm_Z : float;
      begin
         Case Part is
            when Back_Part | Outline_Part => Norm_Z := 1.0;
            when Front_Part => Norm_Z := -1.0;
         end case;
         E3GA.Set_Coords (Normal, 0.0, 0.0, Norm_Z);

         Fan (1) := (0.0, 0.0, 0.0);
         for Count in 2 .. Num_Steps loop
            Fan (Count) := (Single (Cos (Angle)), Single (Sin (Angle)), 0.0);
            Angle := Angle + Rotor_Step;
         end loop;

         Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

         Set_Projection_Matrix (Projection_Matrix);
         GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
         GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
         if Part = Back_Part or Part = Front_Part then
            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                                  First => 0,
                                                  Count => Num_Steps);
         else
            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Line_Loop,
                                                  First => 0,
                                                  Count => Num_Steps);
         end if;
      end Draw_Part;
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      Draw_Part (Back_Part);
      Draw_Part (Front_Part);
      Draw_Part (Outline_Part);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Cone.");
         raise;
   end Draw_Circle;

   --  ------------------------------------------------------------------------

   procedure Draw_Cone (Render_Program    : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Scale             : Gl.Types.Single) is

      use GL.Objects.Buffers;
      use GL.Types.Singles;
      use GA_Maths.Float_Functions;

      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      GL_e1                : Vector3 := GL_Util.To_GL (E3GA.e1);
      GL_e2                : Vector3 := GL_Util.To_GL (E3GA.e2);
      GL_e3                : Vector3 := GL_Util.To_GL (E3GA.e3);
      Z                    : float := 0.0;
      Num_Steps            : constant int := 256;
      Rotor_Step           : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
      Vertex_Buffer        : GL.Objects.Buffers.Buffer;
      Fan                  : Singles.Vector3_Array (1 .. Num_Steps);
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Fan (1) := (0.0, 0.0, 0.0);
      for Count in 2 .. Num_Steps loop
         Fan (Count) := (Single (0.1 * Cos (Z)), Single (0.1 * Sin (Z)), -0.25);
         Z := Z + Rotor_Step;
      end loop;

      Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

      Set_Projection_Matrix (Projection_Matrix);
      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                            First => 0,
                                            Count => Num_Steps);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Cone.");
         raise;
   end Draw_Cone;

   --  ------------------------------------------------------------------------

   procedure Draw_Line (Render_Program    : GL.Objects.Programs.Program;
                        Model_View_Matrix : GL.Types.Singles.Matrix4;
                        Tail, Direction   : E3GA.Vector;
                        Colour            : GL.Types.Colors.Color;
                        Scale             : float) is

      use GL.Objects.Buffers;
      use GL.Toggles;
      use GL.Types.Colors;
      use GL.Types.Singles;

      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      GL_Dir               : Vector3 := GL_Util.To_GL (Direction);
      GL_e1                : Vector3 := GL_Util.To_GL (E3GA.e1);
      GL_e2                : Vector3 := GL_Util.To_GL (E3GA.e2);
      GL_e3                : Vector3 := GL_Util.To_GL (E3GA.e3);
      Dir_e1               : Single := Single (Dot_Product (GL_Dir, GL_e1));
      Dir_e2               : Single := Single (Dot_Product (GL_Dir, GL_e2));
      Dir_e3               : Single := Single (Dot_Product (GL_Dir, GL_e3));
      Vertex_Buffer        : GL.Objects.Buffers.Buffer;
      Vertices             : Singles.Vector3_Array (1 .. 2);
   begin
      Vertices := ((0.0, 0.0, 0.0),
                   (0.98 * Dir_e1, 0.98 * Dir_e2, 0.98 * Dir_e3));

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                Projection_Matrix_ID, Colour_Location);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

      Set_Projection_Matrix (Projection_Matrix);
      GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                            First => 0,
                                            Count => 1 * 3);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
         raise;
   end Draw_Line;

   --  ------------------------------------------------------------------------

   procedure Draw_Sphere (Render_Program : GL.Objects.Programs.Program;
                          MV_Matrix : GL.Types.Singles.Matrix4;
                          Normal : GL.Types.Single; Colour : GL.Types.Colors.Color) is
      Sphere : Geosphere.Geosphere;
   begin
      Geosphere.GS_Compute (Sphere, 4);
      Geosphere.GS_Draw (Render_Program, MV_Matrix, Sphere, Normal, Colour);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Sphere.");
         raise;
   end Draw_Sphere;

   --  ------------------------------------------------------------------------

--     procedure Draw_Sphere (Render_Program : GL.Objects.Programs.Program;
--                            Translation_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
--                            Normal : E3GA.Vector; Scale : float := 1.0;
--                            Colour : GL.Types.Colors.Color) is
--     begin
--        if G_Draw_State.Max_Vertices = 0 then
--           Geosphere.GS_Compute (G_Draw_State.M_Sphere, 4);
--        end if;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
--           raise;
--     end Draw_Sphere;

   --  ------------------------------------------------------------------------
   --  Based on draw.cpp drawTriVector
   procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                             Model_View_Matrix : GL.Types.Singles.Matrix4;
                             Position : E3GA.Vector; Colour : GL.Types.Colors.Color;
                             Scale : float;
                             Method : Trivector_Method_Type := Draw_TV_Sphere) is
      use GL.Types.Singles;
      Scale_Sign          : Single := 1.0;
      Scale_S             : Single := Single (Scale);
      Z_Max               : constant Single := 4.0 * Single (GA_Maths.Pi);
      s                   : Single;
      Translation_Matrix  : Matrix4 := Identity4;
      Scaling_Matrix      : Matrix4 := Identity4;
      MV_Matrix           : Matrix4;
   begin
      if Scale_S < 0.0 then
         Scale_Sign := -1.0;
      end if;
      --  adjust scale for sphere
      Scale_S := Scale_Sign *
        Maths.Cube_Root (Scale_Sign * Scale_S / ((4.0 / 3.0) * Single (GA_Maths.Pi)));
      if Scale_S >= 0.0 then
         Scale_Sign := 1.0;
      else
         Scale_Sign := -1.0;
      end if;

      if E3GA.Get_Coord (E3GA.Norm_E2 (Position)) >= 0.0 then
         Translation_Matrix :=
          Maths.Translation_Matrix ((Single (E3GA.Get_Coord_1 (Position)),
                                      Single (E3GA.Get_Coord_2 (Position)),
                                      Single (E3GA.Get_Coord_3 (Position))));
      end if;
      Scaling_Matrix := Maths.Scaling_Matrix ((Scale_S, Scale_S, Scale_S));
      MV_Matrix := Translation_Matrix * Scaling_Matrix * Model_View_Matrix;
      case Method is
         when DRAW_TV_SPHERE =>
            if Get_Draw_Mode = OD_Orientation then
               s := 0.1;
            else
               s := 0.0;
            end if;
--              s := 1.0;  --  TEST

            Draw_Sphere (Render_Program, MV_Matrix, s, Colour);
         when others => null;
      end case;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
         raise;
   end Draw_Trivector;

   --  ------------------------------------------------------------------------

   procedure Draw_Trivector (Render_Program : GL.Objects.Programs.Program;
                             Translation_Matrix : GL.Types.Singles.Matrix4;
                             Base : E3GA.Vector; Colour : GL.Types.Colors.Color;
                             Scale : float := 1.0; V : E3GA.Vector;
                             Method : Trivector_Method_Type := Draw_TV_Sphere) is
   begin
      if Method = Draw_TV_Parellelepiped or
        Method = Draw_TV_Parellelepiped_No_Vectors then
         null;
      else
         null;
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Trivector.");
         raise;
   end Draw_Trivector;

   --  ------------------------------------------------------------------------

   procedure Draw_Vector (Render_Program  : GL.Objects.Programs.Program;
                          MV_Matrix       : GL.Types.Singles.Matrix4;
                          Tail, Direction : E3GA.Vector;
                          Colour          : GL.Types.Colors.Color;
                          Scale           : float) is
      use GL.Culling;
      use GL.Objects.Buffers;
      use GL.Toggles;
      use GL.Types.Colors;
      use GL.Types.Singles;
      use GA_Maths;
      use GA_Maths.Float_Functions;

      Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      MV_Matrix_ID         : GL.Uniforms.Uniform;
      Projection_Matrix_ID : GL.Uniforms.Uniform;
      Colour_Location      : GL.Uniforms.Uniform;
      Model_View_Matrix    : GL.Types.Singles.Matrix4;
      Projection_Matrix    : GL.Types.Singles.Matrix4;
      GL_Tail              : Vector3 := GL_Util.To_GL (Tail);
      GL_Dir               : Vector3 := GL_Util.To_GL (Direction);

      Z                    : Single := 0.0;
      Dir_e1               : Single := Single (E3GA.Dot_Product (Direction, E3GA.e1));
      Dir_e2               : Single := Single (E3GA.Dot_Product (Direction, E3GA.e2));
      Dir_e3               : Single := Single (E3GA.Dot_Product (Direction, E3GA.e3));
      Tail_e1              : Single := Single (E3GA.Dot_Product (Tail, E3GA.e1));
      Tail_e2              : Single := Single (E3GA.Dot_Product (Tail, E3GA.e2));
      Tail_e3              : Single := Single (E3GA.Dot_Product (Tail, E3GA.e3));
      Scale_Factor1        : Single := Single (1.2 / Scale);
      Scale_Factor2        : Single := 1.1 * Single (Sqrt (float (Scale)));
      Scale_Factor1_V      : Singles.Vector3 := (Scale_Factor1, Scale_Factor1, Scale_Factor1);
      Scale_Factor2_V      : Singles.Vector3 := (Scale_Factor2, Scale_Factor2, Scale_Factor2);
      aRotor               : E3GA.Rotor;
      Saved_Cull_Face      : Face_Selector := Cull_Face;
   begin
      if Scale /= 0.0 then
         GL.Objects.Programs.Use_Program (Render_Program);
         Vertex_Array_Object.Initialize_Id;
         Vertex_Array_Object.Bind;

         Set_Projection_Matrix (Projection_Matrix);
         Graphic_Shader_Locations (Render_Program, MV_Matrix_ID,
                                   Projection_Matrix_ID, Colour_Location);
         GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
         Model_View_Matrix := MV_Matrix;

         if E3GA.Get_Coord (E3GA.Norm_e2 (Tail)) /= 0.0 then
            Model_View_Matrix := Maths.Translation_Matrix
              ((Tail_e1, Tail_e2, Tail_e3)) * Model_View_Matrix;
         end if;
         Draw_Line (Render_Program, Model_View_Matrix, Tail,
                    Direction, Colour, Scale);

         --  rotate e3 to vector direction
         Model_View_Matrix := GL.Types.Singles.Identity4;
         aRotor := E3GA_Utilities.Rotor_Vector_To_Vector (E3GA.e3, E3GA.Unit_e (Direction));
         GL_Util.Rotor_GL_Multiply (aRotor, Model_View_Matrix);

         Model_View_Matrix := MV_Matrix * Model_View_Matrix;

         --  Translate to head of vector
         if E3GA.Get_Coord (E3GA.Norm_e2 (Tail)) /= 0.0 then
            Model_View_Matrix := Maths.Translation_Matrix
              ((Tail_e1, Tail_e2, Tail_e3)) * Model_View_Matrix;
         end if;
         Model_View_Matrix := Maths.Translation_Matrix (Single (Scale) * GL_Dir) * Model_View_Matrix;
         Enable (Cull_Face);
         Set_Front_Face (GL.Types.Clockwise);
         Set_Cull_Face (Front);

         Draw_Cone (Render_Program, Model_View_Matrix, Single (Scale));
         Draw_Base (Render_Program, Model_View_Matrix, Single (Scale));
         Set_Cull_Face (Saved_Cull_Face);
         Disable (Cull_Face);
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Draw.Draw_Vector.");
         raise;
   end Draw_Vector;

   --  -----------------------------------------------------------------------

   function Get_Draw_Mode return Draw_Mode is
   begin
      return G_Draw_State.M_Draw_Mode;
   end Get_Draw_Mode;

   --  ------------------------------------------------------------------------

   procedure Graphic_Shader_Locations (Render_Program : GL.Objects.Programs.Program;
                                       MV_Matrix_ID, Projection_Matrix_ID,
                                       Colour_Location : out GL.Uniforms.Uniform) is
   begin
       MV_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MV_Matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "Proj_Matrix");
      Colour_Location := GL.Objects.Programs.Uniform_Location
        (Render_Program, "vector_colour");
   end Graphic_Shader_Locations;

   --  -------------------------------------------------------------------------

   function Point_Size return Float is
   begin
      return G_Draw_State.Point_Size;
   end Point_Size;

   --  ------------------------------------------------------------------------

   procedure Set_Background_Colour (Back_Colour : Color) is
   begin
      Palet.Background_Colour := Back_Colour;
   end Set_Background_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Draw_Mode (Mode : Draw_Mode) is
   begin
      G_Draw_State.M_Draw_Mode := Mode;
   end Set_Draw_Mode;

   --  ------------------------------------------------------------------------

   procedure Set_Foreground_Colour (Fore_Colour : Color) is
   begin
      Palet.Foreground_Colour := Fore_Colour;
   end Set_Foreground_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Ol_Colour (Ol_Colour : Color) is
   begin
      Palet.Ol_Colour := Ol_Colour;
   end Set_Ol_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Point_Size (Point_Size : Float) is
   begin
      G_Draw_State.Point_Size := Point_Size;
   end Set_Point_Size;

   --  ------------------------------------------------------------------------

   procedure Set_Projection_Matrix (theMatrix : out GL.Types.Singles.Matrix4;
                                    Near : GL.Types.Single := -100.0;
                                    Far  : GL.Types.Single := 100.0) is
      use GL.Types;
      VP_X      : Int;
      VP_Y      : Int;
      VP_Height : Int;
      VP_Width  : Int;
   begin
      GL.Window.Get_Viewport (VP_X, VP_Y, VP_Width, VP_Height);
      Maths.Init_Orthographic_Transform (Single (VP_Y), Single (VP_Y + VP_Height),
                                         Single (VP_X), Single (VP_X + VP_Width),
                                         Near, Far, theMatrix);
   end Set_Projection_Matrix;

   --  ------------------------------------------------------------------------

end GA_Draw;
