
with E3GA;
with GA_Maths;
with Multivector;
with Multivector_Analyze;
with Multivector_Type;

package E3GA_Utilities is

   --  special exp() for 3D bivectors
   function exp (BV : E3GA.Bivector) return E3GA.Rotor;
   --  special log() for 3D rotors
   function log (R : E3GA.Rotor) return E3GA.Bivector;
    procedure Print_Rotor (Name : String; R : E3GA.Rotor);
--     procedure Print_Vector (Name : String; aVector : E2GA.Vector);
--     procedure Print_Vector (Name : String; aVector : E3GA.Vector);
   procedure Rotor_To_Matrix (R : E3GA.Rotor; M : out GA_Maths.GA_Matrix3);
   function Rotor_Vector_To_Vector (V_From, V_To : E3GA.Vector)
                                     return E3GA.Rotor;
end E3GA_Utilities;
