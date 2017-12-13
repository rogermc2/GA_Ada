
with E3GA;

package body Geometric_Objects is

   function Set_Line (L1, L2 : C3GA.Normalized_Point) return C3GA.Line is
   begin
      return C3GA.Unit_R (E3GA.Outer_Product (L1, L2));
   end Set_Line;

end Geometric_Objects;
