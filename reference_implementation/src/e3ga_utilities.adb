
with Interfaces;

with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with E2GA;

package body E3GA_Utilities is

   function exp (BV : E3GA.Bivector) return E3GA.Rotor is
      use E3GA;
      X2         : float := E3GA.Get_Coord (Left_Contraction (BV, BV));
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
          Set_Rotor (Result, 1.0);
      else
         Cos_HA := GA_Maths.Float_Functions.Cos (Half_Angle);
         Sin_HA := GA_Maths.Float_Functions.Sin (Half_Angle) / Half_Angle;
         E3GA.Set_Rotor (Result, Cos_HA, Sin_HA * BV);
      end if;
      return Result;
   end exp;

      --  ----------------------------------------------------------------------------

   --  special log() for 3D rotors
   function log (R : E3GA.Rotor) return E3GA.Bivector is
      use E3GA;
      R2       : float;
      R1       : float;
      BV       : Bivector;
      Result   : Bivector;
   begin
      --  get the bivector 2-blade part of R
      Set_Bivector (BV, e1e2 (R), e2e3 (R), e3e1 (R));
      --  compute the 'reverse norm' of the bivector part of R
      R2 := E3GA.Get_Coord (Norm_R (BV));
      if R2 > 0.0 then
         --  return _bivector(B * ((float)atan2(R2, _Float(R)) / R2));
         R1 := GA_Maths.Float_Functions.Arctan (R2, R_Scalar (R)) / R2;
         Result := R1 * BV;

      --  otherwise, avoid divide-by-zero (and below zero due to FP roundoff)
      elsif R_Scalar (R) < 0.0 then
         --  Return a 360 degree rotation in an arbitrary plane
         Result := Ada.Numerics.Pi * Outer_Product (e1, e2);
      else
         Set_Bivector (Result, 0.0, 0.0, 0.0);
      end if;
      return Result;
   end log;

    --  ------------------------------------------------------------------------

    procedure Print_Matrix (Name    : String; aMatrix : GA_Maths.GA_Matrix3) is
    begin
        Put_Line (Name & ":");
        for Row in 1 .. 3 loop
            for Column in  1 .. 3 loop
                Put (float'Image (aMatrix (Row, Column)) & "   ");
            end loop;
            New_Line;
        end loop;
        New_Line;
    end Print_Matrix;

    --  ------------------------------------------------------------------------

   procedure Print_Rotor (Name : String; R : E3GA.Rotor) is
         Rot : GA_Maths.Array_4D := E3GA.Get_Coords (R);
   begin
        Put_Line (Name & ": " & float'Image (Rot (1)) & ",  " & float'Image (Rot (2))
                  & ",  " & float'Image (Rot (3)) & ",  " & float'Image (Rot (4)));
   end Print_Rotor;

    --  ------------------------------------------------------------------------

    procedure Print_Vector (Name : String; aVector : E3GA.Vector) is
        Coords : GA_Maths.Array_3D := E3GA.Get_Coords (aVector);
    begin
        Put (Name & ":  ");
        for Index in Coords'Range loop
            Put (float'Image (Coords (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Vector;

    --  ------------------------------------------------------------------------

   procedure Rotor_To_Matrix (R : E3GA.Rotor;  M : out GA_Maths.GA_Matrix3) is
        Rot : GA_Maths.Array_4D := E3GA.Get_Coords (R);
   begin
        Print_Rotor ("Rotor_To_Matrix, R", R);
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
    function Rotor_Vector_To_Vector (V_From, V_To : E3GA.Vector) return E3GA.Rotor is
        use Interfaces;
        use GA_Maths;
        use GA_Maths.Float_Functions;
        use E3GA;
        V1     : Vector_Unsigned := To_Unsigned (V_From);
        V2     : Vector_Unsigned := To_Unsigned (V_To);
        C1     : float;
        S      : float;
        w0     : E3GA.Vector;
        w1     : E3GA.Vector;
        w2     : E3GA.Vector;
        N2     : Scalar;
        R      : Rotor;
        Result : Rotor;
    begin
        Set_Coords (w0, 0.0, 0.0, 0.0);
        Set_Coords (w1, 0.0, 0.0, 0.0);
        Set_Coords (w2, 0.0, 0.0, 0.0);
        if float (E3GA.Get_Coord (Scalar_Product (V_From, V_To))) < -0.9 then
            C1 := E3GA.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, V_To)));
            Set_Coords (w0, C1, 0.0, 0.0);
            N2 := Norm_E2 (w0);

            if E3GA.Get_Coord (N2) = 0.0 then
                C1 := E3GA.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, e1)));
                Set_Coords (w1, C1, 0.0, 0.0);
                C1 := E3GA.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, e2)));
                Set_Coords (w2, C1, 0.0, 0.0);
                if E3GA.Get_Coord (Norm_E2 (w1)) > E3GA.Get_Coord (Norm_E2 (w2)) then
                    Set_Rotor (Result, Outer_Product (V_From, Unit_e (w1)));
                else
                    Set_Rotor (Result, Outer_Product (V_From, Unit_e (w2)));
                end if;
            else
                --  Replace V1 with -V1 and additional 180 degree rotation.
                S := Sqrt (2.0 * float (1.0 - Dot_Product (V_To, V_From)));
                R := (1.0 - Geometric_Product (V_To, V_From)) / S;
                Result := Geometric_Product (R, Outer_Product (V_From, Unit_e (w0)));
            end if;
        else
            S := Sqrt (2.0 * float (1.0 + Dot_Product (V_To, V_From)));
            Result := (1.0 + Geometric_Product (V_To, V_From)) / S;
        end if;
        return Result;
    end Rotor_Vector_To_Vector;

end E3GA_Utilities;
