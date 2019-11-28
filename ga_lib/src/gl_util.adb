
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Window;
--  with Utilities;

with Blade;
with Blade_Types;
with E3GA_Utilities;

package body GL_Util is

    --  ------------------------------------------------------------------

--      procedure GL_Color_3fm (R, G, B : GL.Types.Single) is
--          A       : constant GL.Types.Single := 0.3;
--          D       : constant GL.Types.Single := 0.7;
--          Ambient : constant array (1 .. 4) of GL.Types.Single
--            := (A * R, A * G, A * B, 1.0);
--          Dif     : constant array (1 .. 4) of GL.Types.Single
--            := (D * R, D * G, D * B, 1.0);
--      begin
--          null;
--      end GL_Color_3fm;

    --  ------------------------------------------------------------------
    --  Load_Pick_Matrix
    procedure Load_Pick_Matrix is
    begin
        null;
    end Load_Pick_Matrix;

    --  ------------------------------------------------------------------
    --  Rotor_GL_Multiply multiplies GL_Matrix by rotor 'R'
    function Rotor_GL_Multiply (R : Multivectors.Rotor;
                                GL_Matrix : in out GL.Types.Singles.Matrix4)
                               return Boolean is
        use E3GA;
        use Multivectors;
        use GL;
        use GL.Types.Singles;
        IR        : Rotor := New_Rotor;
        E_Rot     : Multivectors.Multivector;
        Image     : Vector3_Array (1 .. 4);
        VC        : Vector3;
        Matrix    : Matrix4 := Identity4;
        Image_Row : Int := 0;
        R_Invertible : constant Boolean := Rotor_Inverse (R, IR);
    begin
        if R_Invertible then
            --  compute the images of all OpenGL basis vectors
            E_Rot := Geometric_Product (R, Geometric_Product (e1, IR));
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
        else
            Put_Line ("GL_Util.Rotor_GL_Multiply, rotor is not invertible.");
        end if;
        return R_Invertible;

    exception
        when others =>
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

    function Rotor_To_GL_Matrix (R : Multivectors.Rotor) return  GL.Types.Singles.Matrix4 is
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
        use Multivectors.Blade_List_Package;
        use Blade;
        use Blade_Types;
        use GA_Maths;
        Blades  :constant Multivectors.Blade_List := Multivectors.Get_Blade_List (V3);
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
        use Multivectors.Blade_List_Package;
        use Blade;
        use Blade_Types;
        Blades  : constant Multivectors.Blade_List := Multivectors.Get_Blade_List (V3);
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

   function To_GL (V3 : E3GA.Vector) return GL.Types.Singles.Vector3 is
   begin
        return (Single (V3 (1)), Single (V3 (2)), Single (V3 (3)));
   end To_GL;

    --  -------------------------------------------------------------------------

    procedure Viewport_Coordinates (Pt_World : GA_Maths.Array_3D;
                                    Model_View_Matrix,
                                    Projection_Matrix : GL.Types.Singles.Matrix4;
                                    Coords : out GL.Types.Singles.Vector2) is
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
