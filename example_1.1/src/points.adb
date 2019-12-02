
package body Points is

   function Set_Draw_Colour (Colour : Singles.Vector4) return Colour_Data is
   begin
      return (others => Colour);
   end Set_Draw_Colour;

end Points;
