
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Window;
with Utilities;

with Blade;
with Blade_Types;
with E3GA;
with E3GA_Utilities;

package body GL_Util is

   Pick_Is_Active      : Boolean := False;  --  set to true during picking
   --  set to picking window (x, y, w, h) during picking
   Pick_Window_Open    : Pick_Window := (others => 0.0);
   --  Must be set correctly by caller of pick() to get correct distances returned
   Frustum_Near_Pos    : Single := 1.0;
   --  Must be set correctly by caller of pick() to get correct distances returned
   Frustum_Far_Pos     : Single := -100.0;
   --  not required for pick(), provided for completenes
   Width_Of_Frustum    : Single := -1.0;
   --  not required for pick(), provided for completenes
   Height_Of_Frustum   : Single := -1.0;
   Size_Of_Pick_Window : Int := 4;

   --  ------------------------------------------------------------------

   function Frustum_Near return Single is
   begin
      return Frustum_Near_Pos;
   end Frustum_Near;

   --  ------------------------------------------------------------------

   function Frustum_Far return Single is
   begin
      return Frustum_Far_Pos;
   end Frustum_Far;

   --  ------------------------------------------------------------------

   function Frustum_Width return Single is
   begin
      return Width_Of_Frustum;
   end Frustum_Width;

   --  ------------------------------------------------------------------

   function Frustum_Height return Single is
   begin
      return Height_Of_Frustum;
   end Frustum_Height;

   --  ------------------------------------------------------------------

   function OpenGL_Pick return Pick_Window is
   begin
      return Pick_Window_Open;
   end OpenGL_Pick;

   --  ------------------------------------------------------------------

   procedure Load_Pick_Matrix is
      Viewport_Coords : GL.Types.Int_Array (1 .. 4);
   begin
      if Pick_Is_Active then
         GL.Window.Get_Viewport (Viewport_Coords (1),
                                 Viewport_Coords (2),
                                 Viewport_Coords (3),
                                 Viewport_Coords (4));
      end if;
   end Load_Pick_Matrix;

   --  ------------------------------------------------------------------

   function Pick_Active return Boolean is
   begin
      return Pick_Is_Active;
   end Pick_Active;

   --  ------------------------------------------------------------------

   function Pick_Window_Size return Int is
   begin
      return Size_Of_Pick_Window;
   end Pick_Window_Size;

   --  ------------------------------------------------------------------

   procedure Print_GL_Int3_Array
     (Name : String; anArray : GL.Types.Ints.Vector3_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Utilities.Print_Vector ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Int3_Array;

   --  ------------------------------------------------------------------------

   --  Rotor_GL_Multiply multiplies GL_Matrix by rotor 'R'
   procedure Rotor_GL_Multiply (R         : Multivectors.Rotor;
                                GL_Matrix : in out GL.Types.Singles.Matrix4) is
      use E3GA;
      use Multivectors;
      use GL;
      use GL.Types.Singles;
      IR        : constant Rotor := To_Rotor (General_Inverse (R));
      E_Rot     : Multivectors.Multivector;
      E1_IR     : Multivectors.Multivector;
      Image     : Vector3_Array (1 .. 4);
      VC        : Vector3;
      Matrix    : Matrix4 := Identity4;
      Image_Row : Int := 0;
   begin
      E1_IR := Geometric_Product (e1, IR);
      if not Is_Null (E1_IR ) then
         --  compute the images of all OpenGL basis vectors
         E_Rot := Geometric_Product (R, E1_IR);
         Image (1) := To_GL (E_Rot);
         E_Rot := Geometric_Product (R, Geometric_Product (e2, IR));
         Image (2) := To_GL (E_Rot);
         E_Rot := Geometric_Product (R, Geometric_Product (e3, IR));
         Image (3) := To_GL (E_Rot);
         Image (4) := (0.0, 0.0, 0.0);  -- Image of origin
         --  Transfer the coordinates to the OpenGL matrix
         for row in GL.Index_Homogeneous loop
            Image_Row := Image_Row + 1;
            VC := Image (Image_Row);
            Matrix (row, X) := VC (X);
            Matrix (row, Y) := VC (Y);
            Matrix (row, Z) := VC (Z);
         end loop;
         GL_Matrix := Matrix * GL_Matrix;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in GL_Util.Rotor_GL_Multiply.");
         raise;
   end Rotor_GL_Multiply;

   --  -------------------------------------------------------------------------
   --  Pick_Matrix defines a picking region
   procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                          Width, Height      : GL.Types.Size) is
   begin
      GL.Window.Set_Viewport (Centre_X, Centre_Y, Width, Height);
   end Pick_Matrix;

   --  ------------------------------------------------------------------

   function Rotor_To_GL_Matrix (R : Multivectors.Rotor)
                                return  GL.Types.Singles.Matrix4 is
      use GL;
      M3        : GA_Maths.GA_Matrix3;
      GL_Matrix : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      Mrow      : integer := 0;
      Mcol      : integer := 0;
   begin
      E3GA_Utilities.Rotor_To_Matrix (R, M3);
      for row in Index_Homogeneous range X .. Z loop
         Mrow := Mrow + 1;
         for col in Index_Homogeneous range X .. Z loop
            Mcol := Mcol + 1;
            GL_Matrix (col, row) := GL.Types.Single (M3 (Mrow, Mcol));
         end loop;
         Mcol := 0;
      end loop;
      return GL_Matrix;

   exception
      when others =>
         Put_Line ("An exception occurred in GL_Util.Rotor_To_GL_Matrix.");
         raise;
   end Rotor_To_GL_Matrix;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : Multivectors.Multivector) return GL.Types.Doubles.Vector3 is
      use Interfaces;
      use Blade.Blade_List_Package;
      use Blade;
      use Blade_Types;
      Blades  : constant Blade.Blade_List := Multivectors.Get_Blade_List (V3);
      Curs    : Cursor := Blades.First;
      BM      : Unsigned_32;
      Value   : Double;
      Val_X   : Double := 0.0;
      Val_Y   : Double := 0.0;
      Val_Z   : Double := 0.0;
   begin
      while Has_Element (Curs) loop
         BM := Bitmap (Element (Curs));
         Value := Double (Blade.Weight (Element (Curs)));
         if (BM and E3_Base'Enum_Rep (E3_e1)) /= 0 then
            Val_X := Val_X + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e2)) /= 0 then
            Val_Y := Val_Y + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e3)) /= 0 then
            Val_Z := Val_Z + Value;
         end if;
         Next (Curs);
      end loop;
      return (Val_X, Val_Y, Val_Z);

   exception
      when others =>
         Put_Line ("An exception occurred in GL_Util.To_GL Double.");
         raise;
   end To_GL;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : Multivectors.Multivector) return GL.Types.Singles.Vector3 is
      use Interfaces;
      use Blade.Blade_List_Package;
      use Blade;
      use Blade_Types;
      Blades  : constant Blade.Blade_List := Multivectors.Get_Blade_List (V3);
      Curs    : Cursor := Blades.First;
      BM      : Unsigned_32;
      Value   : Single;
      Val_X   : Single := 0.0;
      Val_Y   : Single := 0.0;
      Val_Z   : Single := 0.0;
   begin
      while Has_Element (Curs) loop
         BM := Bitmap (Element (Curs));
         Value := Single (Blade.Weight (Element (Curs)));
         if (BM and E3_Base'Enum_Rep (E3_e1)) /= 0 then
            Val_X := Val_X + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e2)) /= 0 then
            Val_Y := Val_Y + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e3)) /= 0 then
            Val_Z := Val_Z + Value;
         end if;
         Next (Curs);
      end loop;
      return (Val_X, Val_Y, Val_Z);

   exception
      when others =>
         Put_Line ("An exception occurred in GL_Util.To_GL Single.");
         raise;
   end To_GL;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : GA_Maths.Float_3D) return GL.Types.Singles.Vector3 is
   begin
      return (Single (V3 (1)), Single (V3 (2)), Single (V3 (3)));
   end To_GL;

   --  ---------------------------------------------------------------------

   procedure Viewport_Coordinates (Pt_World          : GA_Maths.Float_3D;
                                   Model_View_Matrix,
                                   Projection_Matrix : GL.Types.Singles.Matrix4;
                                   Coords            : out GL.Types.Singles.Vector2) is
      use GL;
      use GL.Types.Singles;
      VP_X          : Int;
      VP_Y          : Int;
      Window_Width  : Size;
      Window_Height : Size;
      PT1           : constant Vector4 := (Single (Pt_World (1)), Single (Pt_World (2)),
                                           Single (Pt_World (3)), 1.0);
      PT2           : constant Vector4 := Projection_Matrix * Model_View_Matrix * PT1;
   begin
      --   PT1 := Projection_Matrix * PT2;
      GL.Window.Get_Viewport (VP_X, VP_Y, Window_Width, Window_Height);
      Coords (X) := Single (VP_X) + (1.0 + PT2 (X) / PT2 (W)) * Single (Window_Width) / 2.0;
      Coords (Y) := Single (VP_Y) + (1.0 + PT2 (Y) / PT2 (W)) * Single (Window_Height) / 2.0;

   exception
      when others =>
         Put_Line ("An exception occurred in GL_Util.Viewport_Coordinates.");
         raise;
   end Viewport_Coordinates;

   --  -------------------------------------------------------------------------

end GL_Util;
