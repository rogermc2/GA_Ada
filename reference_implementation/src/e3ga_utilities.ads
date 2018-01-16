
with E3GA;
with GA_Maths;
with Multivector;
with Multivector_Analyze;
with Multivector_Type;

package E3GA_Utilities is

   --  special exp() for 3D bivectors
   function exp (BV : Multivector.Bivector) return Multivector.Rotor;
   --  special log() for 3D rotors
   function log (R : Multivector.Rotor) return Multivector.Bivector;
    procedure Print_Rotor (Name : String; R : Multivector.Rotor);
--     procedure Print_Vector (Name : String; aVector : E2GA.Vector);
--     procedure Print_Vector (Name : String; aVector : E3GA.Vector);
   procedure Rotor_To_Matrix (R : Multivector.Rotor; M : out GA_Maths.GA_Matrix3);
   function Rotor_Vector_To_Vector (V_From, V_To : Multivector.Vector)
                                     return Multivector.Rotor;
end E3GA_Utilities;
