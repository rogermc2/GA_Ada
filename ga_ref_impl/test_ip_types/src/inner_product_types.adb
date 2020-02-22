
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

with Maths;

with Bits;
with Blade;
with Blade_Types;
with GA_Maths;

package body Inner_Product_Types is

   type Scale_Array is array (Integer range <>) of Float;

   --  --------------------------------------------------------------------
   --  Factorize_Blade returns the k unit factors of the blade and
   --  the scale of the blade
   function Factorize_Blade (MV_B : Multivectors.Multivector; Scale : in out Float)
                             return Multivectors.Multivector_List is
      use Interfaces;
      use Blade;
      use Blade_Types;
      use GA_Maths;
      use Multivectors;
      K_Grade    : Integer;
      E_Largest  : Basis_Blade;
      Basis_Bit  : Unsigned_32;
      B_Current  : Multivector;
      aFactor    : Multivector;
      Factors    : Multivector_List;
   begin
      if not Grade (MV_B, K_Grade) then
         raise Inner_Product_Types_Exception with
           "Inner_Product_Types.Factorize_Blade inhomogenous multivector detected.";
      else
         --  set scale of output, no matter what
         if K_Grade = 0 then
            Scale := Scalar_Part (MV_B);
         else
            Scale := Norm_E (MV_B);
         end if;

         if K_Grade > 0 and Scale /= 0.0 then
            --  not a scalar-blade or a null-blade
            E_Largest := Largest_Basis_Blade (MV_B);
            declare
               E_Array : array (0 .. K_Grade - 1 ) of Basis_Blade;
               Idx     : Integer := 0;
            begin
               E_Array (0) := New_Basis_Blade (C3_Base'Enum_Val (K_Grade));
               for Index_G in 0 .. K_Grade - 1 loop
                  Basis_Bit := 2 ** Index_G;
                  if Bitmap (E_Largest) > 0 and Basis_Bit /= 0 then
                     E_Array (Idx) := New_Basis_Blade (C3_Base'Enum_Val (Basis_Bit), 1.0);
                     Idx := Idx + 1;
                  end if;
               end loop;

               B_Current := Geometric_Product (MV_B, 1.0 / Scale);
               --  for all but one of the E_Array basis vectors:
               for index in 0 .. K_Grade - 2 loop
                  --  Project basis vector E_Array (index) onto B_Current
                  --  (E(i) lc B_Current) inv(B_Current) but
                  --  inv(B_Current) not required because Bc is a unit vector
                  aFactor := New_Multivector (E_Array (index));
                  aFactor := Inner_Product
                    (Inner_Product (aFactor, B_Current, Left_Contraction),
                     B_Current, Left_Contraction);
                  --  Normalize aFactor
                  aFactor := Unit_E (aFactor);
                  Add_Multivector (Factors, aFactor);
                  --  Remove aFactor from B_Current
                  B_Current := Inner_Product (aFactor, B_Current, Left_Contraction);
               end loop;
            end;  --  declare block
            --  last factor = what is left of the input blade
            --  B_Current is already normalized but
            --  renormalize to remove any FP round-off error
            Add_Multivector (Factors, Unit_E (B_Current));
         end if;
      end if;
      return  Factors;

   exception
      when others =>
         Put_Line ("An exception occurred in Inner_Product_Types.Factorize_Blade");
         raise;
   end Factorize_Blade;

   --  --------------------------------------------------------------------

   function Factorize_Blade_Fast (MV_B  : Multivectors.Multivector;
                                  Scale : in out Float)
                                  return Multivectors.Multivector_List is
      use Interfaces;
      use Blade;
      use Blade_Types;
      use GA_Maths;
      use Multivectors;
      Grade_K       : Unsigned_32;
      Sc            : Float;
      Blade_E       : Basis_Blade;
      Lowest_Bit    : Integer;
      Highest_Bit   : Integer;
      Blades_B      : Blade_List;
      Basis_Bit     : Unsigned_32;
      Basis_Bitmap  : Unsigned_32;
      Vec_Bitmap    : Unsigned_32;
      Blades_Bj     : Basis_Blade;
      Factors       : Multivector_List;  --  F
      L_List        : Blade_List;
   begin
      if not Grade (MV_B, Integer (Grade_K)) then
         raise Inner_Product_Types_Exception with
           "Inner_Product_Types.Factorize_Blade inhomogenous multivector detected.";
      else
         if Grade_K = 0 then
            Scale := Scalar_Part (MV_B);
         else
            Scale := Norm_E (MV_B);
         end if;

         if Grade_K > 0 and Scale /= 0.0 then
            Blade_E := Largest_Basis_Blade (MV_B);
            Lowest_Bit := Bits.Lowest_One_Bit (Bitmap (Blade_E));
            Highest_Bit := Bits.Highest_One_Bit (Bitmap (Blade_E));

            if Grade_K = 1 then
               Add_Multivector (Factors, Unit_E (MV_B));
            else
               if Weight (Blade_E) < 0.0 then
                  --  positive scale for blade needed
                  Scale := - Scale;

                  --  take care of orientation of blade:
                  if (Grade_K and 1) = 1 then
                     Scale := - Scale;
                  end if;
               end if;

               --  fix sign issues
               if (Grade_K mod 4) = 2 then
                  Scale := - Scale;
               end if;

               Blades_B := Blades (MV_B);
               for index in Lowest_Bit .. Highest_Bit loop
                  Basis_Bit := 2 ** Integer (index);
                  if (Unsigned_32 (Bitmap (Blade_E)) and Basis_Bit) /= 0 then
                     Basis_Bitmap := Unsigned_32 (Bitmap (Blade_E)) xor Basis_Bit;
                     New_Line;
                     for index_j in 1 .. List_Length (Blades_B) loop
                        Blades_Bj := BB_Item (Blades_B, index_j);
                        if (Unsigned_32 (Bitmap (Blades_Bj)) and Basis_Bitmap) =
                          Basis_Bitmap then
                           Vec_Bitmap := Unsigned_32 (Bitmap (Blades_Bj)) xor
                             Basis_Bitmap;
                           Sc := Weight (Blades_Bj) *
                             Canonical_Reordering_Sign
                               (Unsigned_32 (Basis_Bitmap), Bitmap (Blades_Bj));
                           Blade.Add_Blade
                             (L_List, New_Basis_Blade (C3_Base'Enum_Val (Vec_Bitmap), Sc));
                        end if;
                     end loop;
                  end if;
                  Add_Multivector (Factors, New_Multivector (L_List));
               end loop;
            end if;
         else
            Add_Multivector (Factors, New_Multivector (0.0));
         end if;
      end if;
      return  Factors;

   exception
      when others =>
         Put_Line ("An exception occurred in Inner_Product_Types.Factorize_Blade_Fast");
         raise;
   end Factorize_Blade_Fast;

   --  --------------------------------------------------------------------

end Inner_Product_Types;
