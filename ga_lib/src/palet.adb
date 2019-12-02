
package body Palet is
    Palet          : Colour_Palet;

    function Background_Colour return Color is
    begin
        return Palet.Background_Colour;
    end Background_Colour;

    --  ------------------------------------------------------------------------

    function Foreground_Colour return Color is
    begin
        return Palet.Foreground_Colour;
    end Foreground_Colour;

    --  ------------------------------------------------------------------------

    function Ol_Colour return Color is
    begin
        return Palet.Ol_Colour;
    end Ol_Colour;

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
end Palet;
