
with Interfaces;

with Blade;
with C3GA;
with GA_Maths;
with Multivector;
with Multivector_Type;

package body C3GA_Utilities is

   --  Factorize_Blade factors blades into vectors (euclidean unit length).
   function Factorize_Blade (MV : Multivector.Multivector;
                             Factor : out C3GA.Dual_Sphere_Array;
                             Grade : Integer := -1) return Float is
      use Interfaces;
      use GA_Maths;
      use Multivector;
      use Multivector_Type;
      K       : Integer := Grade;
      MV_Info : MV_Type_Record := Init (MV);
      BB      : Blade.Basis_Blade;
      aBlade  : Blade.Basis_Blade;
      BM      : Unsigned_32;
      Bc      : Multivector.Multivector;
      Eidx    : Integer := 1;
      Coords  : array (1 .. 5) of Float := (others => 0.0);
      Ei      : Multivector.Multivector;
      F_Index : Integer := 1;
      DS      : C3GA.Dual_Sphere;
      Scale   : Float;
   begin
      if K < 0 then
        K := Integer (Top_Grade (MV_Info));
      end if;
      if K = 0 then
         Scale := Scalar_Part (MV);
      else
         Scale := Norm_E (MV);
      end if;
      if Scale /= 0.0 and then K /= 0 then
         BB := Largest_Basis_Blade (MV);
         Bc := Unit_E (MV);
         BM :=Unsigned_32 (Blade.Bitmap (BB));
         while (BM and 1) /= 1 loop
            BM := Shift_Right (BM, 1);
            Eidx := Eidx + 1;
         end loop;
         Coords (Eidx) := 1.0;
         BM := BM xor 1;
         for Index in Coords'Range loop
            if Coords (Index) /= 0.0 then
               aBlade := Blade.New_Basis_Blade (C3GA.Grade_1, Coords (Index));
               Multivector.Add_Blade (Ei, aBlade);
            end if;
         end loop;
         --  Project basis vector ei and normalize projection
         DS := C3GA.Dual_Sphere (Unit_E (Left_Contraction (Left_Contraction (Ei, Bc), Bc)));
         Factor (F_Index) := DS;
      end if;
      return Scale;
   end Factorize_Blade;

   --  -------------------------------------------------------------------------

end C3GA_Utilities;
