
with Shader_Manager;

package body Palet is

   G_Draw_State   : Draw_State;

   function Background_Colour (Palet_Data : Colour_Palet) return Color is
   begin
      return Palet_Data.Background_Colour;
   end Background_Colour;

   --  ------------------------------------------------------------------------

   function Background_Red (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (R);
   end Background_Red;

   --  ------------------------------------------------------------------------

   function Background_Green (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (G);
   end Background_Green;

   --  ------------------------------------------------------------------------

   function Background_Blue (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (B);
   end Background_Blue;

   --  ------------------------------------------------------------------------

   function Current_Sphere return Geosphere.Geosphere is
   begin
      return G_Draw_State.M_Sphere;
   end Current_Sphere;

   --  ------------------------------------------------------------------------

   function Foreground_Colour (Palet_Data : Colour_Palet) return Color is
   begin
      return Palet_Data.Foreground_Colour;
   end Foreground_Colour;

   --  ------------------------------------------------------------------------

   function Foreground_Alpha (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Foreground_Colour (A);
   end Foreground_Alpha;

   --  ------------------------------------------------------------------------

   function Foreground_Blue (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (B);
   end Foreground_Blue;

   --  ------------------------------------------------------------------------

   function Foreground_Green (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (G);
   end Foreground_Green;

   --  ------------------------------------------------------------------------

   function Foreground_Red (Palet_Data : Colour_Palet) return Single is
   begin
      return Palet_Data.Background_Colour (R);
   end Foreground_Red;

   --  ------------------------------------------------------------------------

   function Get_Draw_Mode return Draw_Mode is
   begin
      return G_Draw_State.M_Draw_Mode;
   end Get_Draw_Mode;

   --  ------------------------------------------------------------------------

   function Get_Plane_Size return Float is
   begin
      return G_Draw_State.Plane_Size;
   end Get_Plane_Size;

   --  ------------------------------------------------------------------------

   function Is_Null return Colour_Palet is
      Null_Colour : constant Color := (0.0, 0.0, 0.0, 0.0);
   begin
      return (False, Null_Colour, Null_Colour, Null_Colour);
   end Is_Null;

   --  ------------------------------------------------------------------------

   function Line_Length return Float is
   begin
      return G_Draw_State.Line_Length;
   end Line_Length;

   --  ------------------------------------------------------------------------

   function Magnitude return Boolean is
   begin
      return G_Draw_State.M_Draw_Mode.Magnitude;
   end Magnitude;

   --  ------------------------------------------------------------------------

   function Orientation return Boolean is
   begin
      return G_Draw_State.M_Draw_Mode.Orientation;
   end Orientation;

   --  ------------------------------------------------------------------------
   function Outline_Colour (Palet_Data : Colour_Palet) return Color is
   begin
      return Palet_Data.Outline_Colour;
   end Outline_Colour;

   --  ------------------------------------------------------------------------

   function Point_Size return Float is
   begin
      return G_Draw_State.Point_Size;
   end Point_Size;

   --  ------------------------------------------------------------------------

   procedure Set_Background_Alpa (Palet_Data : in out Colour_Palet; Alpa : Float) is
   begin
      Palet_Data.Background_Colour (A) := GL.Types.Single (Alpa);
   end Set_Background_Alpa;

   --  ------------------------------------------------------------------------

   procedure Set_Background_Colour (Palet_Data  : in out Colour_Palet;
                                    Back_Colour : Color) is
   begin
      Palet_Data.Background_Colour := Back_Colour;
   end Set_Background_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Background_Colour (Palet_Data : Colour_Palet) is
      Colour         : constant Color := Background_Colour (Palet_Data);
      A_State        : constant Single := Single (G_Draw_State.Ambient);
      D_State        : constant Single := Single (G_Draw_State.Diffuse);
      Ambient_Colour : constant Color :=
                         (A_State * Colour (R), A_State * Colour (G),
                          A_State * Colour (B), A_State * Colour (A));
      Diffuse_Colour : constant Color :=
                         (D_State * Colour (R), D_State * Colour (G),
                          D_State * Colour (B), D_State * Colour (A));
   begin
      Shader_Manager.Set_Ambient_Colour
        ((Ambient_Colour (R), Ambient_Colour (G), Ambient_Colour (B),
         Ambient_Colour (A)));
      Shader_Manager.Set_Diffuse_Colour
        ((Diffuse_Colour (R), Diffuse_Colour (G),Diffuse_Colour (B),
         Diffuse_Colour (A)));
   end Set_Background_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Current_Sphere (aSphere : Geosphere.Geosphere) is
   begin
      G_Draw_State.M_Sphere := aSphere;
   end Set_Current_Sphere;

   --  ------------------------------------------------------------------------

   procedure Set_Draw_Mode_Off (Mode : Draw_Mode_Type) is
   begin
      case Mode is
         when OD_Shade => G_Draw_State.M_Draw_Mode.Shade := False;
         when OD_Wireframe => G_Draw_State.M_Draw_Mode.Wireframe := False;
         when OD_Magnitude => G_Draw_State.M_Draw_Mode.Magnitude := False;
         when OD_Orientation => G_Draw_State.M_Draw_Mode.Orientation := False;
      end case;
   end Set_Draw_Mode_Off;

      --  ------------------------------------------------------------------------

   procedure Set_Draw_Mode_On (Mode : Draw_Mode_Type) is
   begin
      case Mode is
         when OD_Shade => G_Draw_State.M_Draw_Mode.Shade := True;
         when OD_Wireframe => G_Draw_State.M_Draw_Mode.Wireframe := True;
         when OD_Magnitude => G_Draw_State.M_Draw_Mode.Magnitude := True;
         when OD_Orientation => G_Draw_State.M_Draw_Mode.Orientation := True;
      end case;
   end Set_Draw_Mode_On;

   --  ------------------------------------------------------------------------

   procedure Set_Foreground_Alpa (Palet_Data : in out Colour_Palet; Alpa : Float) is
   begin
      Palet_Data.Foreground_Colour (A) := GL.Types.Single (Alpa);
   end Set_Foreground_Alpa;

   --  ------------------------------------------------------------------------

   procedure Set_Foreground_Colour (Palet_Data : Colour_Palet) is
      Colour         : constant Color := Foreground_Colour (Palet_Data);
      A_State        : constant Single := Single (G_Draw_State.Ambient);
      D_State        : constant Single := Single (G_Draw_State.Diffuse);
      Ambient_Colour : constant Color :=
                         (A_State * Colour (R), A_State * Colour (G),
                          A_State * Colour (B), A_State * Colour (A));
      Diffuse_Colour : constant Color :=
                         (D_State * Colour (R), D_State * Colour (G),
                          D_State * Colour (B), D_State * Colour (A));
   begin
      Shader_Manager.Set_Ambient_Colour
        ((Ambient_Colour (R),Ambient_Colour (G), Ambient_Colour (B),
         Ambient_Colour (A)));
      Shader_Manager.Set_Diffuse_Colour
        ((Diffuse_Colour (R), Diffuse_Colour (G), Diffuse_Colour (B),
         Diffuse_Colour (A)));
   end Set_Foreground_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Foreground_Colour (Palet_Data  : in out Colour_Palet;
                                    Fore_Colour : Color) is
   begin
      Palet_Data.Foreground_Colour := Fore_Colour;
   end Set_Foreground_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Outline_Alpa (Palet_Data : in out Colour_Palet; Alpa : Float) is
   begin
      Palet_Data.Outline_Colour (A) := GL.Types.Single (Alpa);
   end Set_Outline_Alpa;

   --  ------------------------------------------------------------------------

   procedure Set_Outline_Colour (Palet_Data : in out Colour_Palet;
                                 Outline_Colour : Color) is
   begin
      Palet_Data.Outline_Colour := Outline_Colour;
   end Set_Outline_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Outline_Colour (Palet_Data : Colour_Palet) is
      Colour         : constant Color := Outline_Colour (Palet_Data);
      A_State        : constant Single := Single (G_Draw_State.Ambient);
      D_State        : constant Single := Single (G_Draw_State.Diffuse);
      Ambient_Colour : constant Color :=
                         (A_State * Colour (R), A_State * Colour (G), A_State * Colour (B),
                          A_State * Colour (A));
      Diffuse_Colour : constant Color :=
                         (D_State * Colour (R), D_State * Colour (G),
                          D_State * Colour (B), D_State * Colour (A));
   begin
      Shader_Manager.Set_Ambient_Colour
        ((Ambient_Colour (R), Ambient_Colour (G), Ambient_Colour (B),
         Ambient_Colour (A)));
      Shader_Manager.Set_Diffuse_Colour
        ((Diffuse_Colour (R), Diffuse_Colour (G), Diffuse_Colour (B),
         Diffuse_Colour (A)));
   end Set_Outline_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Point_Size (Point_Size : Float) is
   begin
      G_Draw_State.Point_Size := Point_Size;
   end Set_Point_Size;

   --  ------------------------------------------------------------------------

   function Shade return Boolean is
   begin
      return G_Draw_State.M_Draw_Mode.Shade;
   end Shade;

   --  ------------------------------------------------------------------------

   function Wireframe return Boolean is
   begin
      return G_Draw_State.M_Draw_Mode.Wireframe;
   end Wireframe;

   --  ------------------------------------------------------------------------

end Palet;
