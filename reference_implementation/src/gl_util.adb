
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Window;
with Utilities;

with Blade;
with E3GA_Utilities;
with GA_Maths;
with GA_Utilities;

package body GL_Util is
   use GL.Types;

   procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                          Width, Height : GL.Types.Size);

   --  ------------------------------------------------------------------

   procedure GL_Color_3fm (R, G, B : GL.Types.Single) is
      A       : constant GL.Types.Single := 0.3;
      D       : constant GL.Types.Single := 0.7;
      Ambient : constant array (1 .. 4) of GL.Types.Single
        := (A * R, A * G, A * B, 1.0);
      Dif     : constant array (1 .. 4) of GL.Types.Single
        := (D * R, D * G, D * B, 1.0);
   begin
      null;
   end GL_Color_3fm;

   --  ------------------------------------------------------------------
   --  Load_Pick_Matrix
   procedure Load_Pick_Matrix is
   begin
      null;
   end Load_Pick_Matrix;

   --  ------------------------------------------------------------------
   --  Rotor_GL_Multiply multiplies GL_Matrix by rotor 'R'
   procedure Rotor_GL_Multiply (R : Multivector.Rotor; GL_Matrix : in out GL.Types.Singles.Matrix4) is
      use E3GA;
      use Multivector;
      use GL;
      use GL.Types.Singles;
      IR        : constant Rotor := General_Inverse (R);
      E_Rot     : Vector;
      Image     : Vector3_Array (1 .. 4);
      VC        : Vector3;
      Matrix    : Matrix4 := Identity4;
      Image_Row : Int := 0;
   begin
      --  compute the images of all OpenGL basis vectors
      GA_Utilities.Print_Multivector ("GL_Util.Rotor_GL_Multiply IR", IR);
      E_Rot := Geometric_Product (R, Geometric_Product (e1, IR));
      GA_Utilities.Print_Multivector ("GL_Util.Rotor_GL_Multiply e1 IR", Geometric_Product (e1, IR));
      GA_Utilities.Print_Multivector ("GL_Util.Rotor_GL_Multiply R e1 IR", E_Rot);
      Image (1) := To_GL (E_Rot);
      E_Rot := Geometric_Product (R, Geometric_Product (e2, IR));
      GA_Utilities.Print_Multivector ("GL_Util.Rotor_GL_Multiply R e2 IR", E_Rot);
      Image (2) := To_GL (E_Rot);
      E_Rot := Geometric_Product (R, Geometric_Product (e3, IR));
      GA_Utilities.Print_Multivector ("GL_Util.Rotor_GL_Multiply R e3 IR", E_Rot);
      Image (3) := To_GL (E_Rot);
      Image (4) := (0.0, 0.0, 0.0);  -- Image of origin
      Utilities.Print_GL_Array3 ("GL_Util.Rotor_GL_Multiply Image", Image);
      --  Transfer the coordinates to the OpenGL matrix
      for row in GL.Index_Homogeneous loop
         Image_Row := Image_Row + 1;
         VC := Image (Image_Row);
         Matrix (row, X) := Dot_Product (VC, To_GL (e1));
         Matrix (row, Y) := Dot_Product (VC, To_GL (e2));
         Matrix (row, Z) := Dot_Product (VC, To_GL (e3));
      end loop;
      Utilities.Print_Matrix ("GL_Util.Rotor_GL_Multiply Matrix", Matrix);
      GL_Matrix := Matrix * GL_Matrix;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GL_Util.Rotor_GL_Multiply.");
         raise;
   end Rotor_GL_Multiply;

   --  -------------------------------------------------------------------------
   --  Pick_Matrix defines a picking region
   procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                          Width, Height : GL.Types.Size) is
   begin
      GL.Window.Set_Viewport (Centre_X, Centre_Y, Width, Height);
   end Pick_Matrix;

   --  ------------------------------------------------------------------

   function Rotor_To_GL_Matrix (R : Multivector.Rotor) return  GL.Types.Singles.Matrix4 is
      use GL;
      M3        : GA_Maths.GA_Matrix3;
      GL_Matrix : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      Mrow      : integer := 0;
      Mcol      : integer := 0;
   begin
      E3GA_Utilities.Rotor_To_Matrix (R, M3);
      GA_Utilities.Print_Matrix ("Rotor_To_Matrix, M3", M3);
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
      when anError :  others =>
         Put_Line ("An exception occurred in GL_Util.Rotor_To_GL_Matrix.");
         raise;
   end Rotor_To_GL_Matrix;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : Multivector.Vector) return GL.Types.Doubles.Vector3 is
      use Interfaces;
      use GL.Types;
      use Multivector.Blade_List_Package;
      use Blade;
      use GA_Maths;
      Blades  : Multivector.Blade_List := Multivector.Get_Blade_List (V3);
      Curs    : Cursor := Blades.First;
      BM      : Unsigned_32;
      Value   : Double;
      Val_X   : Double := 0.0;
      Val_Y   : Double := 0.0;
      Val_Z   : Double := 0.0;
   begin
      while Has_Element (Curs) loop
         BM := Unsigned_32 (Bitmap (Element (Curs)));
         Value := Double (Blade.Weight (Element (Curs)));
         if (BM and E3_Base'Enum_Rep (E3_e1)) /= 0 then
            Val_X := Val_X + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e2)) /= 0 then
            Val_Y:= Val_Y + Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e3)) /= 0 then
            Val_Z := Val_Z + Value;
         end if;
         Next (Curs);
      end loop;
      return (Val_X, Val_Y, Val_Z);
   end To_GL;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : Multivector.Vector) return GL.Types.Singles.Vector3 is
      use Interfaces;
      use GL.Types;
      use Multivector.Blade_List_Package;
      use Blade;
      Blades  : Multivector.Blade_List := Multivector.Get_Blade_List (V3);
      Curs    : Cursor := Blades.First;
      BM      : Unsigned_32;
      Value   : Single;
      Val_X   : Single := 0.0;
      Val_Y   : Single := 0.0;
      Val_Z   : Single := 0.0;
   begin
      while Has_Element (Curs) loop
         BM := Unsigned_32 (Bitmap (Element (Curs)));
         Value := Single (Blade.Weight (Element (Curs)));
         if (BM and E3_Base'Enum_Rep (E3_e1)) /= 0 then
            Val_X := Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e2)) /= 0 then
            Val_Y:= Value;
         end if;
         if (BM and E3_Base'Enum_Rep (E3_e3)) /= 0 then
            Val_Z := Value;
         end if;
         Next (Curs);
      end loop;
      return (Val_X, Val_Y, Val_Z);
   end To_GL;

   --  -------------------------------------------------------------------------

   --     function To_GL (V2 : Multivector.Vector) return GL.Types.Singles.Vector3 is
   --          use E2GA;
   --     begin
   --          return (Single (Get_Coord_1 (V2)), Single (Get_Coord_2 (V2)), 0.0);
   --     end To_GL;

   --  -------------------------------------------------------------------------

   procedure Viewport_Coordinates (Pt_World : GA_Maths.Array_3D;
                                   Model_View_Matrix,
                                   Projection_Matrix : GL.Types.Singles.Matrix4;
                                   Coords : out GL.Types.Singles.Vector2) is
      use GL;
      use GL.Types;
      use GL.Types.Singles;
      VP_X          : Int;
      VP_Y          : Int;
      Window_Width  : Size;
      Window_Height : Size;
      PT1 : Vector4 := (Single (Pt_World (1)), Single (Pt_World (2)),
                        Single (Pt_World (3)), 1.0);
      PT2 : Vector4 := Projection_Matrix * Model_View_Matrix * PT1;
   begin
      --   PT1 := Projection_Matrix * PT2;
      GL.Window.Get_Viewport (VP_X, VP_Y, Window_Width, Window_Height);
      Coords (X) := Single (VP_X) + (1.0 + PT2 (X) / PT2 (W)) * Single (Window_Width) / 2.0;
      Coords (Y) := Single (VP_Y) + (1.0 + PT2 (Y) / PT2 (W)) * Single (Window_Height) / 2.0;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GL_Util.Viewport_Coordinates.");
         raise;
   end Viewport_Coordinates;

   --  -------------------------------------------------------------------------

end GL_Util;
