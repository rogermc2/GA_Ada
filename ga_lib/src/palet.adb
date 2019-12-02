
package body Palet is

    G_Draw_State   : Draw_State;
    Palet          : Colour_Palet;

    function Background_Colour return Color is
    begin
        return Palet.Background_Colour;
    end Background_Colour;

    --  ------------------------------------------------------------------------

    function Background_Red return Single is
    begin
        return Palet.Background_Colour (R);
    end Background_Red;

    --  ------------------------------------------------------------------------

    function Background_Green return Single is
    begin
        return Palet.Background_Colour (G);
    end Background_Green;

    --  ------------------------------------------------------------------------

    function Background_Blue return Single is
    begin
        return Palet.Background_Colour (B);
    end Background_Blue;

    --  ------------------------------------------------------------------------

    function Current_Sphere return Geosphere.Geosphere is
    begin
        return G_Draw_State.M_Sphere;
    end Current_Sphere;

    --  ------------------------------------------------------------------------

    function Colour_Null return Boolean is
    begin
        return Palet.Colour_Defined;
    end Colour_Null;

    --  ------------------------------------------------------------------------

    function Foreground_Colour return Color is
    begin
        return Palet.Foreground_Colour;
    end Foreground_Colour;

    --  ------------------------------------------------------------------------

    function Foreground_Red return Single is
    begin
        return Palet.Background_Colour (R);
    end Foreground_Red;

    --  ------------------------------------------------------------------------

    function Foreground_Green return Single is
    begin
        return Palet.Background_Colour (G);
    end Foreground_Green;

    --  ------------------------------------------------------------------------

    function Foreground_Blue return Single is
    begin
        return Palet.Background_Colour (B);
    end Foreground_Blue;

    --  ------------------------------------------------------------------------

    function Get_Draw_Mode return Draw_Mode is
    begin
        return G_Draw_State.M_Draw_Mode;
    end Get_Draw_Mode;

    --  ------------------------------------------------------------------------

    function Line_Length return Float is
    begin
        return G_Draw_State.Line_Length;
    end Line_Length;

    --  ------------------------------------------------------------------------

    function Ol_Colour return Color is
    begin
        return Palet.Ol_Colour;
    end Ol_Colour;

    --  ------------------------------------------------------------------------

    function Point_Size return Float is
    begin
        return G_Draw_State.Point_Size;
    end Point_Size;

    --  ------------------------------------------------------------------------

    procedure Set_Background_Alpa (Alpa : Float) is
    begin
        Palet.Background_Colour (A) := GL.Types.Single (Alpa);
    end Set_Background_Alpa;

    --  ------------------------------------------------------------------------

    procedure Set_Background_Colour (Back_Colour : Color) is
    begin
        Palet.Colour_Defined := True;
        Palet.Background_Colour := Back_Colour;
    end Set_Background_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Foreground_Alpa (Alpa : Float) is
    begin
        Palet.Foreground_Colour (A) := GL.Types.Single (Alpa);
    end Set_Foreground_Alpa;

    --  ------------------------------------------------------------------------

    procedure Set_Foreground_Colour (Fore_Colour : Color) is
    begin
        Palet.Colour_Defined := True;
        Palet.Foreground_Colour := Fore_Colour;
    end Set_Foreground_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Ol_Alpa (Alpa : Float) is
    begin
        Palet.Ol_Colour (A) := GL.Types.Single (Alpa);
    end Set_Ol_Alpa;

    --  ------------------------------------------------------------------------

    procedure Set_Ol_Colour (Ol_Colour : Color) is
    begin
        Palet.Colour_Defined := True;
        Palet.Ol_Colour := Ol_Colour;
    end Set_Ol_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Ol_Colour is
    begin
        Palet.Ol_Colour := Ol_Colour;
    end Set_Ol_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Point_Size (Point_Size : Float) is
    begin
        G_Draw_State.Point_Size := Point_Size;
    end Set_Point_Size;

    --  ------------------------------------------------------------------------

end Palet;
