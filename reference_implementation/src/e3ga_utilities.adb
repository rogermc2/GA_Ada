
with Interfaces;

with Ada.Containers;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with GA_Utilities;
with Multivector_Type_Base;

package body E3GA_Utilities is

   --  -------------------------------------------------------------------------

   function exp (BV : Multivector.Bivector) return Multivector.Rotor is
      use E3GA;
      use Multivector;
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
   function log (R : Multivector.Rotor) return Multivector.Bivector is
      use E3GA;
      use Multivector;
      R2       : float;
      R1       : float;
      BV       : Multivector.Bivector;
      Result   : Multivector.Bivector;
   begin
      --  get the bivector 2-blade part of R
      BV := New_Bivector (e1_e2 (R), e2_e3 (R), e3_e1 (R));
      --  compute the 'reverse norm' of the bivector part of R
      R2 := E3GA.Norm_R (BV);
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

   procedure Print_Rotor (Name : String; R : Multivector.Rotor) is
      Rot : GA_Maths.Array_4D := E3GA.Get_Coords (R);
   begin
      GA_Utilities.Print_Multivector (Name, R);
   end Print_Rotor;

   --  ------------------------------------------------------------------------

   procedure Rotor_To_Matrix (R : Multivector.Rotor;  M : out GA_Maths.GA_Matrix3) is
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
   function Rotor_Vector_To_Vector (From, To : Multivector.Vector) return Multivector.Rotor is
      use GA_Maths.Float_Functions;
      use Multivector;
      ab     : Multivector.Multivector := Geometric_Product (From, To);
      ba     : Multivector.Multivector := Geometric_Product (To, From);
      ab1    : Multivector.Multivector := 1.0 + ab;
      ba1    : Multivector.Multivector := 1.0 + ba;
      aa     : Multivector.Multivector := Geometric_Product (From, From);
      baab   : Multivector.Multivector := Geometric_Product (To, Geometric_Product (aa, To));
      OP_Sum : Multivector.Multivector := Outer_Product (From, To) + Outer_Product (To, From);
      ba1ab1 : Multivector.Multivector;
      S      : float;
      w0     : Vector;
      w1     : Vector;
      w2     : Vector;
      N2     : Float;
      R      : Rotor;
      Result : Rotor;
   begin
      Simplify (OP_Sum);
      if  Scalar_Product (From, To) < -0.9 then
         w0 := Left_Contraction (From, Outer_Product (From, To));
         N2 := Norm_E2 (w0);
         if N2 = 0.0 then
            w1 :=  Left_Contraction (From, Outer_Product (From, Get_Basis_Vector (Blade.E3_e1)));
            w2 := Left_Contraction (From, Outer_Product (From, Get_Basis_Vector (Blade.E3_e2)));
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
         --  Geometric Algebra fot Computer Science, Equation (10.13)
         Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector, else > -0.9");
         S := Sqrt (2.0 * (1.0 + Scalar_Part (Dot (To, From))));
         Result :=  (1.0 + Geometric_Product (To, From)) / S;
      end if;
      Simplify (Result);

--        GA_Utilities.Print_Multivector("E3GA_Utilities.Rotor_Vector_To_Vector Dot product",
--                                       Dot (To, From));
--        New_Line;
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector GP (To, From)", Geometric_Product (To, From));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector GP (From, To)", Geometric_Product (From, To));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ab", ab);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ba", ba);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector 1 + ba", 1.0 + ba);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector 1 + ab", 1.0 + ab);
--        --  (1 + ba)(1 + ab) = 1 + ab + ba + baab
      --                   = 1 + a.b + a^b + b.a + b^a + 1
      --                   = 2 + 2a.b + a^b - a^b
      --                   = 2(1 + a.b)

--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector (1 + ba)(1 + ab)",
--                                     Geometric_Product (1.0 + ba, 1.0 + ab));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ba1", ba1);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ab1", ab1);
      --  ba1 ab1 = ba1.ab1 + ba1 ^ ab1
--        ba1ab1 := Dot (ba1, ab1) + Outer_Product (ba1, ab1);
--        Simplify (ba1ab1);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ba1ab1 = ba1.ab1 + ba1 ^ ab1",
--                                        ba1ab1);
--        ba1ab1 := Outer_Product (ba1, ab1);
--        Simplify (ba1ab1);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ba1 ^ ab1",
--                                        ba1ab1);
--
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ba1 ab1 should = ba1.ab1 + ba1 ^ ab1",
--                                         Geometric_Product (ba1, ab1));
--        ba1ab1 := 1.0 + ab + ba + Geometric_Product (To, Geometric_Product (From, ab));
--        Simplify (ba1ab1);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector 1 + ab + ba + baab",
--                                         ba1ab1);
--        Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector, S 1/S: " &
--                   Float'Image (S) & Float'Image (1.0 / S));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector a.b", Dot (From, To));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector b.a", Dot (To, From));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector aa", aa);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector baab", baab);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector a^b + b^a", OP_Sum);
--
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector Result", Result);
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ~Result", Reverse_MV (Result));
--        GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector R~R",
--             Geometric_Product (Result, Reverse_MV (Result)));
      return Result;
   end Rotor_Vector_To_Vector;

   --  ----------------------------------------------------------------------------

   function Rotor_Vector_To_Vector2 (V_From, V_To : Multivector.Vector) return Multivector.Rotor is
      use Multivector;
      Result : Rotor :=  Geometric_Product (V_To, V_From);
   begin
      Simplify (Result);
      GA_Utilities.Print_Multivector("E3GA_Utilities.Rotor_Vector_To_Vector V_From", V_From);
      GA_Utilities.Print_Multivector("EV_3GA_Utilities.Rotor_Vector_To_Vector V_To", V_To);
      GA_Utilities.Print_Multivector("E3GA_Utilities.Rotor_Vector_To_Vector Result", Result);
      GA_Utilities.Print_Multivector("E3GA_Utilities.Rotor_Vector_To_Vector ~Result", Multivector.Reverse_MV (Result));
      GA_Utilities.Print_Multivector("E3GA_Utilities.Rotor_Vector_To_Vector R~R",
           Multivector.Geometric_Product (Result, Multivector.Reverse_MV (Result)));
      return Result;
   end Rotor_Vector_To_Vector2;

   --  ----------------------------------------------------------------------------

   function Rotor_Vector_To_Vector1 (V_From, V_To : Multivector.Vector) return Multivector.Rotor is
      use GA_Maths.Float_Functions;
      use Multivector;
      Cos_12 : constant Float := Norm_E (Dot (V_To, V_From));
      Sin_12 : constant Float := Norm_E (Outer_Product (V_To, V_From));
      Cos_HA : constant Float := Sqrt ((1.0 + Cos_12) / 2.0);
      Sin_HA : constant Float := Sqrt ((1.0 - Cos_12) / 2.0);
      B      : constant Bivector := Outer_Product (V_To, V_From);
      Result : Rotor := Cos_HA + Sin_HA * B;
   begin
      --  R(m,n) = nm = n.m + n^m = Cos (n,m) + n^m
      --  Define B = m^n / Sin (n,m)
      --  So, R(m,n) = Cos (n,m) + B Sin (n,m)
      --  R((m,n) / 2) = Cos ((n,m) / 2) + B Sin (() / 2)
      --  Cos = Dot product
      --  Sin = Outer Product
      --  (1 - Cos(t)) / 2 = (Sin(t/2))**2
      --          Sin(t/2) = Sqrt ((1 - Cos(t)) / 2)
      --  (1 + Cos(t)) / 2 = (Cos(t/2))**2
      --          Cos(t/2) = Sqrt ((1 + Cos(t)) / 2)
--        if Scalar_Product (V_To, V_From) < -0.9 then
--           null;
--        else
--           S := Sqrt (2.0 * (1.0 + Scalar_Part (Dot (V_To, V_From))));
--           Result := (1.0 + Geometric_Product (V_To, V_From)) / S;
--        end if;
      Simplify (Result);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector V_From", V_From);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector V_To", V_To);
      Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector Cos_12" & Float'Image (Cos_12));
      Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector Sin_12" & Float'Image (Sin_12));
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector B^2", Geometric_Product (B, B));
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector Result", Result);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ~Result", Multivector.Reverse_MV (Result));
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector R~R",
           Multivector.Geometric_Product (Result, Multivector.Reverse_MV (Result)));

      return Result;
   end Rotor_Vector_To_Vector1;

   --  ----------------------------------------------------------------------------

   function Rotor_Vector_To_Vector3 (V_From, V_To : Multivector.Vector) return Multivector.Rotor is
      use GA_Maths.Float_Functions;
      use Multivector;
      Cos_12 : constant Float := Norm_E (Dot (V_To, V_From));
      Sin_12 : constant Float := Norm_E (Outer_Product (V_To, V_From));
      Cos_2A : constant Float := Cos_12 ** 2 + Sin_12 ** 2;
      Sin_2A : constant Float := 2.0 * Sin_12 * Cos_12;
      B      : constant Bivector := Outer_Product (V_To, V_From);
      Result : Rotor := Cos_2A + Sin_2A * B;
   begin
      --  R(m,n) = nm = n.m + n^m = Cos (n,m) + n^m
      --  Define B = m^n / Sin (n,m)
      --  So, R(m,n) = Cos (n,m) + B Sin (n,m)
      --  R((m,n) / 2) = Cos ((n,m) / 2) + B Sin (() / 2)
      --  Cos = Dot product
      --  Sin = Outer Product
      --  Cos(2t) = Cos(t)**2 - Sin(t))**2
      --  Sin(2t) = 2Sin(t)Cos(2t))
--        if Scalar_Product (V_To, V_From) < -0.9 then
--           null;
--        else
--           S := Sqrt (2.0 * (1.0 + Scalar_Part (Dot (V_To, V_From))));
--           Result := (1.0 + Geometric_Product (V_To, V_From)) / S;
--        end if;
      Simplify (Result);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector V_From", V_From);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector V_To", V_To);
      Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector Cos_12" & Float'Image (Cos_12));
      Put_Line ("E3GA_Utilities.Rotor_Vector_To_Vector Sin_12" & Float'Image (Sin_12));
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector Result", Result);
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector ~Result", Multivector.Reverse_MV (Result));
      GA_Utilities.Print_Multivector ("E3GA_Utilities.Rotor_Vector_To_Vector R~R",
           Multivector.Geometric_Product (Result, Multivector.Reverse_MV (Result)));

      return Result;
   end Rotor_Vector_To_Vector3;

end E3GA_Utilities;
