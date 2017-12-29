
with C3GA;
with E2GA;
with E3GA;
with GA_Maths;
with Multivector_Analyze;

package E3GA_Utilities is

   function C3GA_Point (Vec : E3GA.Vector) return C3GA.Normalized_Point;
   --  special exp() for 3D bivectors
   function exp (BV : E3GA.Bivector) return E3GA.Rotor;
   --  special log() for 3D rotors
   function log (R : E3GA.Rotor) return E3GA.Bivector;
   procedure Print_Analysis (Name : String;
                             Info : Multivector_Analyze.MV_Analysis);
   procedure Print_Matrix (Name : String; aMatrix : GA_Maths.GA_Matrix3);
   procedure Print_Multivector (Name : String; MV : E2GA.Multivector);
   procedure Print_Multivector_Info (Name : String; Info : E2GA.MV_Type);
   procedure Print_Rotor (Name : String; R : E3GA.Rotor);
   procedure Print_Vector (Name : String; aVector : E2GA.Vector);
   procedure Print_Vector (Name : String; aVector : E3GA.Vector);
   procedure Print_Vector (Name : String; aVector : C3GA.Vector_E3GA);
   procedure Rotor_To_Matrix (R : E3GA.Rotor; M : out GA_Maths.GA_Matrix3);
   function Rotor_Vector_To_Vector (V_From, V_To : E3GA.Vector)
                                     return E3GA.Rotor;
end E3GA_Utilities;
