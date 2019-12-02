
with GL.Types;
with GL.Types.Colors; use GL.Types.Colors;

with Geosphere;

package Palet is

    use  GL.Types;

    function Current_Sphere return Geosphere.Geosphere;
    type Draw_Mode is (OD_Shade, OD_Wireframe, OD_Magnitude, OD_Orientation);
    type Draw_State is private;
    type Colour_Palet is private;

    function Background_Colour return Color;
    function Background_Red return Single;
    function Background_Green return Single;
    function Background_Blue return Single;
    function Colour_Null return Boolean;
    function Colour_Null (Palet_Type : Colour_Palet) return Boolean;
    function Foreground_Alpha return Single;
    function Foreground_Alpha (Palet_Type : Colour_Palet) return Single;
    function Foreground_Blue return Single;
    function Foreground_Colour return Color;
    function Foreground_Green return Single;
    function Foreground_Red return Single;
    function Get_Draw_Mode return Draw_Mode;
    function Line_Length return Float;
    function Ol_Colour return Color;
    function Point_Size return Float;

    procedure Set_Background_Alpa (Alpa : Float);
    procedure Set_Background_Colour (Back_Colour : Color);
    procedure Set_Foreground_Alpa (Alpa : Float);
    procedure Set_Foreground_Colour (Fore_Colour : Color);
   procedure Set_Foreground_Colour (Palet_Type  : in out Colour_Palet;
                                    Fore_Colour : Color);
    procedure Set_Ol_Alpa (Alpa : Float);
    procedure Set_Ol_Colour;
    procedure Set_Ol_Colour (Ol_Colour : Color);
    procedure Set_Point_Size (Point_Size : Float);

private
    type Colour_Palet is record
        Colour_Defined    : Boolean := False;
        Foreground_Colour : Color := (1.0, 0.0, 0.0, 1.0);
        Background_Colour : Color := (0.0, 1.0, 0.0, 1.0);
        Ol_Colour         : Color := (0.0, 0.0, 0.0, 1.0);
    end record;

    type Draw_State is record
        Ambient      : Float := 1.0;
        Diffuse      : Float := 0.0;
        Point_Size   : Float := 0.2;
        Line_Length  : Float := 6.0;
        Plane_Size   : Float := 6.0;
        M_Draw_Mode  : Draw_Mode := OD_Magnitude;
        M_Sphere     : Geosphere.Geosphere;
        --  M_Sphere_GL_List : GL.Types.UInt;
    end record;
end Palet;
