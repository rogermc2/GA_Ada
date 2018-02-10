
with E3GA;
with GA_Maths;
with Multivectors; use Multivectors;
with Multivector_Analyze;
with Multivector_Type;

package E3GA_Utilities is

   --  special exp() for 3D bivectors
   function exp (BV : Bivector) return Rotor;
   --  special log() for 3D rotors
   function log (R : Rotor) return Bivector;
    procedure Print_Rotor (Name : String; R : Rotor);
--     procedure Print_Vector (Name : String; aVector : E2GA.Vector);
--     procedure Print_Vector (Name : String; aVector : E3GA.Vector);
   procedure Rotor_To_Matrix (R : Rotor; M : out GA_Maths.GA_Matrix3);
   function Rotor_Vector_To_Vector (From, To : Vector)
                                     return Rotor;
end E3GA_Utilities;
