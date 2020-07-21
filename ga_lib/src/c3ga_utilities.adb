
--  with Interfaces;

with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with C3GA;
with E3GA;
with GA_Utilities;
with Metric;
--  with Multivector_Type;

package body C3GA_Utilities is

   epsilon : constant Float := 10.0 ** (-6);

   --  -------------------------------------------------------------------------

   function exp (BV : Multivectors.Bivector) return Multivectors.Rotor is
      V          :  constant Multivectors.M_Vector :=
                     Inner_Product (BV, BV, Blade.Left_Contraction);
      X2         : float := C3GA.e1_e2 (V);
      Half_Angle : float;
      Cos_HA     : float;
      Sin_HA     : float;
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
   function Log_Rotor (R : Multivectors.Rotor) return Multivectors.Bivector is
      use E3GA;
      R2    : float;
      R1    : float;
      BV    : Bivector;
      Log_R : Bivector;
   begin
      GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_Rotor, R", R);
      GA_Utilities.Print_Multivector_String
          ("C3GA_Utilities.Log_Rotor, R", R, Blade_Types.Basis_Names_C3GA);
      --  get the bivector 2-blade part of R
      BV := To_Bivector (R);
      GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_Rotor, BV", BV);
      --  compute the 'reverse norm' of the bivector part of R
      R2 := Norm_E (BV);
      Put_Line ("C3GA_Utilities.Log_Rotor, R2" & Float'Image (R2));
      if R2 > 0.0 then
         --  return _bivector(B * ((float)atan2(R2, _Float(R)) / R2));
         R1 := GA_Maths.Float_Functions.Arctan (R2, Scalar_Part (R)) / R2;
         Log_R := R1 * BV;

         --  otherwise, avoid divide-by-zero (and below zero due to FP roundoff)
      elsif Scalar_Part (R) < 0.0 then
         --  Return a 360 degree rotation in an arbitrary plane
         Log_R := Ada.Numerics.Pi * Outer_Product (e1, e2);
      else
         Put_Line ("C3GA_Utilities.Log_Rotor Log_R = 0.");
         Log_R := New_Bivector (0.0, 0.0, 0.0);
      end if;

      return Log_R;

   exception
      when others =>
         Put_Line ("An exception occurred in C3GA_Utilities.Log_Rotor.");
         raise;
   end Log_Rotor;

   --  ------------------------------------------------------------------------

   function Log_TR_Versor (V : TR_Versor) return Dual_Line is
      --        use Interfaces;
      use Blade_Types;
      use Metric;
      --        no       : constant Unsigned_32 := Unsigned_32 (C3_Base'Enum_Rep (C3_no));
      --        ni       : constant Unsigned_32 := Unsigned_32 (C3_Base'Enum_Rep (C3_ni));
      MV       : constant Multivector := V;
      Rot      : Rotor;
      Rot_I    : Rotor;
      --        Trans    : E3_Vector;
      Trans    : Multivector;
      I3_Blade : constant Blade.Basis_Blade := Blade.New_Basis_Blade (C3_e3, 1.0);
      I3       : constant Multivector := New_Multivector (I3_Blade);
      BV_Norm  : float;
      BV       : Bivector;
      BV_I     : Bivector;
      BV_I_Phi : Bivector;
      DL_1     : Dual_Line;
      DL_2     : Dual_Line;
      I_R2     : Rotor;
      Log_V    : Dual_Line;
      LC       : Multivector;
   begin
      --  isolate the rotation and translation parts
      GA_Utilities.Print_Multivector_String
          ("C3GA_Utilities.Log_TR_Versor MV)", MV, Blade_Types.Basis_Names_C3GA);
      GA_Utilities.Print_Multivector_String
          ("C3GA_Utilities.Log_TR_Versor C3GA.ni)", C3GA.ni, Blade_Types.Basis_Names_C3GA);
      GA_Utilities.Print_Multivector_String
          ("C3GA_Utilities.Log_TR_Versor Geometric_Product (MV, C3GA.ni, C3_Metric))",
           Geometric_Product (MV, C3GA.ni, C3_Metric), Blade_Types.Basis_Names_C3GA);
      LC := Left_Contraction
        (C3GA.no, (Geometric_Product (MV, C3GA.ni, C3_Metric)), C3_Metric);
      GA_Utilities.Print_Multivector_String
        ("C3GA_Utilities.Log_TR_Versor LC", LC, Blade_Types.Basis_Names_C3GA);
      Rot := To_Rotor (-LC);
--        GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_TR_Versor Rot", Rot);
      Rot_I := Inverse_Rotor (Rot);
      Trans := -2.0 * Geometric_Product
        (Left_Contraction (C3GA.no, MV), Rot_I, C3_Metric);
      --  get the bivector 2-blade part of R
      BV := New_Bivector (E3GA.e1_e2 (MV), E3GA.e2_e3 (MV), E3GA.e3_e1 (MV));
      --  'reverse norm' of the bivector part of R
      GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_TR_Versor Trans", Trans);
      GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_TR_Versor BV", BV);
      BV_Norm := Norm_E (BV);
      if BV_Norm > epsilon then
         --  Logarithm of rotation part
         BV_I_Phi := -2.0 * Log_Rotor (Rot);
         GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_TR_Versor Rot", Rot);
         GA_Utilities.Print_Multivector ("C3GA_Utilities.Log_TR_Versor BV_I_Phi", BV_I_Phi);
         --  Rotation plane:
         Rot_I := Multivectors.Unit_E (BV_I_Phi);
         DL_1 := Geometric_Product (Inverse_Rotor (Rot_I), C3GA.ni, C3_Metric);
         DL_1 := Geometric_Product (Outer_Product (Trans, Rot_I), DL_1, C3_Metric);
         DL_2 := Left_Contraction (Trans, BV_I_Phi, C3_Metric);
         DL_2 := Geometric_Product (DL_2, C3GA.ni, C3_Metric);
         I_R2 := Inverse_Rotor
              (1.0 - Multivectors.Geometric_Product (Rot, Rot, C3_Metric));
         DL_2 := Geometric_Product (I_R2, DL_2, C3_Metric);
         Log_V := 0.5 * (DL_1 - DL_2 - BV_I_Phi);
      else  --  BV_Norm <= epsilon
         if Scalar_Part (Rot) < 0.0 then
            --  The versor has a rotation over 360 degrees.
            if Norm_E (Trans) > epsilon then
               BV_I :=
                 Multivectors.Unit_E (Left_Contraction (Trans, I3, C3_Metric));
            else
               BV_I := Outer_Product (E3GA.e1, E3GA.e2);
            end if;
            DL_1 := Outer_Product (Trans, C3GA.ni);
            Log_V := 0.5 * (2.0 * GA_Maths.Pi * BV_I - DL_1);
         else  --  Scalar_Part (Rot) >= 0.0
            Log_V := -0.5 * Outer_Product (Trans, C3GA.ni);
         end if;
      end if;
      return Log_V;

   exception
      when others =>
         Put_Line ("An exception occurred in C3GA_Utilities.Log_TR_Versor.");
         raise;
   end Log_TR_Versor;

   --  ------------------------------------------------------------------------

   procedure Print_Rotor (Name : String; R : Multivectors.Rotor) is
   begin
      GA_Utilities.Print_Multivector (Name, R);
   end Print_Rotor;

   --  ------------------------------------------------------------------------

   procedure Rotor_To_Matrix (R : Multivectors.Rotor;
                              M : out GA_Maths.GA_Matrix3) is
      Rot : constant GA_Maths.Float_4D := E3GA.Get_Coords (R);
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
   --  Based on rotorFromVectorToVector
   function Rotor_Vector_To_Vector (From_V1, To_V2 : Multivectors.M_Vector)
                                    return Multivectors.Rotor is
      use GA_Maths.Float_Functions;
      S      : float;
      w0     : M_Vector;
      w1     : M_Vector;
      w2     : M_Vector;
      Nsq    : Float;
      R      : Rotor;
      Result : Rotor;
   begin
      if Scalar_Product (From_V1, To_V2) < -0.9 then
         --  "near" 180 degree rotation :
         --  v1 factor in returning blade regardless of any loss of precision
         --  v1 << (v1^v2) means c3ga::lcont(v1, (v1^v2)),
         --  lcont Left_Contraction
         w0 := Left_Contraction (From_V1, Outer_Product (From_V1, To_V2));
         Nsq := Norm_Esq (w0);

         if Nsq = 0.0 then
            w1 := Left_Contraction
              (From_V1, Outer_Product (From_V1, Basis_Vector (Blade_Types.E3_e1)));
            w2 := Left_Contraction
              (From_V1, Outer_Product (From_V1, Basis_Vector (Blade_Types.E3_e2)));
            if Norm_Esq (w1) > Norm_Esq (w2) then
               Result := Outer_Product (From_V1, Unit_e (w1));
            else
               Result := Outer_Product (From_V1, Unit_e (w2));
            end if;
         else  --  Nsq /= 0.0
            --  Replace V1 with -V1 and an additional 180 degree rotation.
            S := Sqrt (2.0 * (1.0 - Scalar_Part (Left_Contraction (To_V2, From_V1))));
            R := (1.0 - Geometric_Product (To_V2, From_V1)) / S;
            Result := Geometric_Product (R, Outer_Product (From_V1, Unit_e (w0)));
         end if;
      else  --  normal case, not "near" 180 degree rotation.
         --  (1 + ba)(1 + ab) = 1 + ab + ba + baab
         --                   = 1 + a.b + a^b + b.a + b^a + 1
         --                   = 2 + 2a.b + a^b - a^b
         --                   = 2(1 + a.b)
         --  Geometric Algebra for Computer Science, Equation (10.13)
         S := Sqrt (2.0 * (1.0 + Scalar_Part (Dot (To_V2, From_V1))));
         Result :=  To_Rotor ((1.0 + Geometric_Product (To_V2, From_V1)) / S);
      end if;
      Simplify (Result);

      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in C3GA_Utilities.Rotor_Vector_To_Vector.");
         raise;
   end Rotor_Vector_To_Vector;

   --  ----------------------------------------------------------------------------

end C3GA_Utilities;
