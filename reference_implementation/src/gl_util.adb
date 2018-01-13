
with GL;
with GL.Window;

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
    procedure Rotor_GL_Multiply (R : E3GA.Rotor; GL_Matrix : in out GL.Types.Singles.Matrix4) is
        use E3GA;
        use GL;
        use GL.Types.Singles;
        IR        : Rotor := Inverse (R);
        VGP       : E3GA.Vector;
        Image     : Vector3_Array (1 .. 4);
        Matrix    : Matrix4 := Identity4;
        Image_Row : Int := 0;
    begin
        --  compute the images of all OpenGL basis vectors
        VGP := To_Vector (Geometric_Product (R, Geometric_Product (e1, IR)));
        Image (1) := To_GL (VGP);
        VGP := To_Vector (Geometric_Product (R, Geometric_Product (e2, IR)));
        Image (2) := To_GL (VGP);
        VGP := To_Vector (Geometric_Product (R, Geometric_Product (e3, IR)));
        Image (3) := To_GL (VGP);
        Image (4) := (0.0, 0.0, 0.0);  -- Image of origin
        for row in GL.Index_Homogeneous loop
            Image_Row := Image_Row + 1;
            for col in GL.Index_Homogeneous range X .. Z loop
                    Matrix (row, col) := Image (Image_Row) (col);
            end loop;
        end loop;
        GL_Matrix := Matrix * GL_Matrix;
    end Rotor_GL_Multiply;

   --  -------------------------------------------------------------------------
   --  Pick_Matrix defines a picking region
   procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                          Width, Height : GL.Types.Size) is
   begin
      GL.Window.Set_Viewport (Centre_X, Centre_Y, Width, Height);
   end Pick_Matrix;

--  ------------------------------------------------------------------
    --  Rotor_GL_Multiply multiplies GL_Matrix by rotor 'R'
   function Rotor_To_GL_Matrix (R : E3GA.Rotor) return  GL.Types.Singles.Matrix4 is
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
   end Rotor_To_GL_Matrix;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : E3GA.Vector) return GL.Types.Doubles.Vector3 is
        use GL.Types;
        use E3GA;
   begin
        return (Double (Get_Coord_1 (V3)), Double (Get_Coord_2 (V3)),
                                      Double (Get_Coord_3 (V3)));
   end To_GL;

   --  -------------------------------------------------------------------------

   function To_GL (V3 : E3GA.Vector) return GL.Types.Singles.Vector3 is
        use E3GA;
   begin
        return (Single (Get_Coord_1 (V3)), Single (Get_Coord_2 (V3)),
                                      Single (Get_Coord_3 (V3)));
   end To_GL;

   --  -------------------------------------------------------------------------

   function To_GL (V2 : E2GA.Vector) return GL.Types.Singles.Vector3 is
        use E2GA;
   begin
        return (Single (Get_Coord_1 (V2)), Single (Get_Coord_2 (V2)), 0.0);
   end To_GL;

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
   end Viewport_Coordinates;

   --  -------------------------------------------------------------------------

end GL_Util;
