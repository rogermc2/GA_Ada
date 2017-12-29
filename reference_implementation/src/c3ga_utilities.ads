
with C3GA;
with E3GA;

package C3GA_Utilities is

   function C3GA_Point (V : C3GA.Vector_E3GA) return C3GA.Normalized_Point;
   procedure Print_Vector (Name : String; aVector : C3GA.Vector_E3GA);

end C3GA_Utilities;
