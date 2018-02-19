
with Interfaces;

with Ada.Containers;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with GA_Utilities;
with Multivector_Type_Base;

package body E3GA_Utilities is

   --  -------------------------------------------------------------------------

   function exp (BV : Multivectors.Bivector) return Multivectors.Rotor is
      use E3GA;
      use Multivectors;
      V          : Vector :=
        Inner_Product (BV, BV, Blade.Left_Contraction);
      X2         : float := E3GA.e1_e2 (V);
      Half_Angle : float;
      Cos_HA     : float;
      Sin_HA     : float;
      Sum        : Bivector;
      Result     : Rotor;
   begin
      if X2 > 0.0 then
         X2 := 0.0;
      end if;
      Half_Angle := GA_Maths.Float_Functions.Sqrt (-X2);
      if Half_Angle = 0.0 then
         Update_Scalar_Part (Result, 1.0);
      else
         Cos_HA := GA_Maths.Float_Functions.Cos (Half_Angle);
         Sin_HA := GA_Maths.Float_Functions.Sin (Half_Angle) / Half_Angle;
         Result := New_Rotor (0.0, Cos_HA + Sin_HA * BV);
      end if;
      return Result;
   end exp;

   --  ----------------------------------------------------------------------------

   --  special log() for 3D rotors
   function log (R : Multivectors.Rotor) return Multivectors.Bivector is
      use E3GA;
      use Multivectors;
      R2       : float;
      R1       : float;
      BV       : Bivector;
      Result   : Bivector;
   begin
      --  get the bivector 2-blade part of R
      BV := New_Bivector (e1_e2 (R), e2_e3 (R), e3_e1 (R));
      --  compute the 'reverse norm' of the bivector part of R
      R2 := Norm_R (BV);
      if R2 > 0.0 then
         --  return _bivector(B * ((float)atan2(R2, _Float(R)) / R2));
         R1 := GA_Maths.Float_Functions.Arctan (R2, Scalar_Part (R)) / R2;
         Result := R1 * BV;

         --  otherwise, avoid divide-by-zero (and below zero due to FP roundoff)
      elsif Scalar_Part (R) < 0.0 then
         --  Return a 360 degree rotation in an arbitrary plane
         Result := Ada.Numerics.Pi * Outer_Product (e1, e2);
      else
         BV := New_Bivector (0.0, 0.0, 0.0);
      end if;
      return Result;
   end log;

   --  ------------------------------------------------------------------------

   procedure Print_Rotor (Name : String; R : Multivectors.Rotor) is
      Rot : GA_Maths.Array_4D := E3GA.Get_Coords (R);
   begin
      GA_Utilities.Print_Multivector (Name, R);
   end Print_Rotor;

   --  ------------------------------------------------------------------------

   procedure Rotor_To_Matrix (R : Multivectors.Rotor;  M : out GA_Maths.GA_Matrix3) is
      Rot : GA_Maths.Array_4D := E3GA.Get_Coords (R);
   begin
      M (1, 1) := 1.0 - 2.0 * (Rot (3) * Rot (3) + Rot (4) * Rot (4));
      M (2, 1) := 2.0 * (Rot (2) * Rot (3) + Rot (4) * Rot (1));
      M (3, 1) := 2.0 * (Rot (2) * Rot (4) - Rot (3) * Rot (1));

      M (1, 2) := 2.0 * (Rot (2) * Rot (3) - Rot (4) * Rot (1));
      M (2, 2) := 1.0 - 2.0 * (Rot (2) * Rot (2) + Rot (4) * Rot (4));
      M (3, 2) := 2.0 * (Rot (3) * Rot (4) + Rot (2) * Rot (1));

      M (1, 3) := 2.0 * (Rot (2) * Rot (4) + Rot (3) * Rot (1));
      M (2, 3) := 2.0 * (Rot (3) * Rot (4) - Rot (2) * Rot (1));
      M (3, 3) := 1.0 - 2.0 * (Rot (2) * Rot (2) + Rot (3) * Rot (3));
   end Rotor_To_Matrix;

   --  ------------------------------------------------------------------------
   --  Rotor Utheta = ba = b.a + b^a = cos theta + i sin theta = e**i theta
   --                                = bx        + i by  (with respect to a)
   --  for theta = angle from a to b.
   function Rotor_Vector_To_Vector (From, To : Multivectors.Vector) return Multivectors.Rotor is
      use GA_Maths.Float_Functions;
      use Multivectors;
      S      : float;
      w0     : Vector;
      w1     : Vector;
      w2     : Vector;
      N2     : Float;
      R      : Rotor;
      Result : Rotor;
   begin
      if  Scalar_Product (From, To) < -0.9 then
         w0 := Left_Contraction (From, Outer_Product (From, To));
         N2 := Norm_E2 (w0);
         if N2 = 0.0 then
            w1 := Left_Contraction (From, Outer_Product (From, Basis_Vector (Blade_Types.E3_e1)));
            w2 := Left_Contraction (From, Outer_Product (From, Basis_Vector (Blade_Types.E3_e2)));
            if Norm_E2 (w1) > Norm_E2 (w2) then
               Result := Outer_Product (From, Unit_e (w1));
            else
               Result := Outer_Product (From, Unit_e (w2));
            end if;
         else  --  N2 /= 0.0
            --  Replace V1 with -V1 and additional 180 degree rotation.
            S := Sqrt (2.0 * (1.0 - Scalar_Part (Left_Contraction (To, From))));
            R := (1.0 - Geometric_Product (To, From)) / S;
            Result := Geometric_Product (R, Outer_Product (From, Unit_e (w0)));
         end if;
      else
         --  (1 + ba)(1 + ab) = 1 + ab + ba + baab
         --                   = 1 + a.b + a^b + b.a + b^a + 1
         --                   = 2 + 2a.b + a^b - a^b
         --                   = 2(1 + a.b)
         --  Geometric Algebra fot Computer Science, Equation (10.13)
         S := Sqrt (2.0 * (1.0 + Scalar_Part (Dot (To, From))));
         Result :=  (1.0 + Geometric_Product (To, From)) / S;
      end if;
      Simplify (Result);

      return Result;
   end Rotor_Vector_To_Vector;

   --  ----------------------------------------------------------------------------

end E3GA_Utilities;
