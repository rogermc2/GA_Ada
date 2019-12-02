
package body Palet is

    G_Draw_State   : Draw_State;
    Palet          : Colour_Palet;

    function Background_Colour return Color is
    begin
        return Palet.Background_Colour;
    end Background_Colour;

    --  ------------------------------------------------------------------------

    function Current_Sphere return Geosphere.Geosphere is
    begin
        return G_Draw_State.M_Sphere;
    end Current_Sphere;

    --  ------------------------------------------------------------------------

    function Foreground_Colour return Color is
    begin
        return Palet.Foreground_Colour;
    end Foreground_Colour;

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

    procedure Set_Background_Colour (Back_Colour : Color) is
    begin
        Palet.Background_Colour := Back_Colour;
    end Set_Background_Colour;

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
