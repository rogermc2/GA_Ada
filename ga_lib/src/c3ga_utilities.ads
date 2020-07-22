
with GA_Maths;
with Multivectors; use Multivectors;

package C3GA_Utilities is

   function exp (MV_X : Multivector; Order : Integer := 9) return Multivector;
   --  special exp() for 3D bivectors
   function exp_BV (BV : Bivector) return Rotor;
   --  special log() for 3D rotors
   function Log_Rotor (R : Rotor) return Bivector;
   function Log_TR_Versor (V : TR_Versor) return Dual_Line;
   procedure Print_Rotor (Name : String; R : Rotor);
--     procedure Print_Vector (Name : String; aVector : E2GA.Vector);
--     procedure Print_Vector (Name : String; aVector : E3GA.Vector);
   procedure Rotor_To_Matrix (R : Rotor; M : out GA_Maths.GA_Matrix3);
   function Rotor_Vector_To_Vector (From_V1, To_V2 : Multivectors.M_Vector)
                                    return Rotor;

end C3GA_Utilities;
