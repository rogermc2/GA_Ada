
with C3GA;
with GA_Maths;
with Multivectors;

package C3GA_Utilities is

   function Factorize_Blade (MV : Multivectors.Multivector;
                             Factor : out C3GA.Dual_Sphere_Array;
                             Grade : Integer := -1) return Float;

end C3GA_Utilities;
