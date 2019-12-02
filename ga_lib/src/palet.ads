
with GL.Types;
with GL.Types.Colors; use GL.Types.Colors;

package Palet is

    use  GL.Types;
    type Colour_Palet is private;

    procedure Set_Background_Colour (Back_Colour : Color);
    procedure Set_Foreground_Colour (Fore_Colour : Color);
    procedure Set_Ol_Colour (Ol_Colour : Color);

private
    type Colour_Palet is record
        Foreground_Colour : Color := (1.0, 0.0, 0.0, 1.0);
        Background_Colour : Color := (0.0, 1.0, 0.0, 1.0);
        Ol_Colour         : Color := (0.0, 0.0, 0.0, 1.0);
    end record;

end Palet;
