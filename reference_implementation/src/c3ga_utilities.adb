
package body C3GA_Utilities is

   Ni : float := 1.0;
   No : float := 1.0;

   function C3GA_Point (V : E3GA.Vector) return C3GA.Normalized_Point is
      thePoint : C3GA.Normalized_Point;
   begin
      thePoint.Coordinates (1) := 1.0 + no + 0.5 *
        E3GA.Get_Coord (E3GA.Norm_e2 (V)) * ni;
      return thePoint;
   end C3GA_Point;

   --  -------------------------------------------------------------------------

end C3GA_Utilities;
