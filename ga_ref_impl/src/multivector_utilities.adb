
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

with Bits;
with Blade;
with Blade_Types;
--  with GA_Utilities;
with Metric;
with Multivector_Type;

package body Multivector_Utilities is

    --  Factorize_Blades returns the k unit factors of the blade and
    --  the scale of the blade
    function Factorize_Blades (MV_B : Multivectors.Multivector; Scale : out Float)
                               return Multivectors.Multivector_List is
        use Interfaces;
        use Blade;
        use Multivectors;
        MV_Type_Rec : Multivector_Type.MV_Type_Record;
        K_Grade     : Integer := 0;
        Grade_Valid : Grade_Status;
        E_Largest   : Basis_Blade;
        Basis_Bit   : Unsigned_32;
        B_Current   : Multivector;
        aFactor     : Multivector;
        Factors     : Multivector_List;
        E_Array     : array (1 .. Space_Dimension) of Basis_Blade;
        Idx         : Integer := 0;
    begin
        if Space_Dimension < 1 then
            raise MV_Utilities_Exception with
              "Multivector_Utilities.Factorize_Blades Geometry type has not been set.";
        end if;
        Grade_Valid := Grade (MV_B, K_Grade);
        if Grade_Valid = Grade_Inhomogeneous then
            MV_Type_Rec := Multivector_Type.Init (MV_B);
            K_Grade := Multivector_Type.MV_Grade (MV_Type_Rec);
        elsif Grade_Valid = Grade_Null then
            raise MV_Utilities_Exception with
              "Multivector_Utilities.Factorize_Blades null grade multivector detected.";
        end if;

        --  set scale of output
        if K_Grade = 0 then
            Scale := Scalar_Part (MV_B);
        else
            Scale := Norm_E (MV_B);
        end if;

        if K_Grade > 0 and Scale /= 0.0 then
            --  not a scalar-blade or a null-blade
            --  get largest basis blade
            E_Largest := Largest_Basis_Blade (MV_B);
            --  get basis vectors
            for Index_G in 0 .. Space_Dimension - 1 loop
                --  Shift 1 left by Index_G bits
                Basis_Bit := Shift_Left (1, Index_G);
                if (Bitmap (E_Largest) and Basis_Bit) /= 0 then
                    Idx := Idx + 1;
                    E_Array (Idx) :=
                      New_Basis_Blade (Basis_Bit);
                end if;
            end loop;

            --  setup the 'current input blade'
            B_Current := Geometric_Product (MV_B, 1.0 / Scale);

            --  for all but one of the E_Array basis vectors:
            for index in 1 .. Space_Dimension - 1 loop
                --  Project basis vector E_Array (index) onto B_Current
                --  (E(i) lc B_Current) inv(B_Current) but
                --  inv(B_Current) not required because Bc is a unit vector
                aFactor := New_Multivector (E_Array (index));
                aFactor := Inner_Product
                  (Inner_Product (aFactor, B_Current, Left_Contraction),
                   B_Current, Left_Contraction);
                if not Is_Null (aFactor) then
                    --  Normalize aFactor
                    aFactor := Unit_E (aFactor);
                    Add_Multivector (Factors, aFactor);
                    --  Remove aFactor from B_Current
                    B_Current :=
                      Inner_Product (aFactor, B_Current, Left_Contraction);
                end if;
            end loop;
            --  last factor = what is left of the input blade
            --  B_Current is already normalized but
            --  renormalize to remove any floating point round-off error
            Add_Multivector (Factors, Unit_E (B_Current));
        end if;
        return  Factors;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Utilities.Factorize_Blades");
            raise;
    end Factorize_Blades;

    --  ------------------------------------------------------------------------

    function Factorize_Blade_Fast (MV_B  : Multivectors.Multivector;
                                   Scale : out Float)
                                   return Multivectors.Multivector_List is
        use Interfaces;
        use Blade;
        use Blade_Types;
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
        if Space_Dimension < 1 then
            raise MV_Utilities_Exception with
              "Multivector_Utilities.Factorize_Blades Geometry type has not been set.";
        end if;

        if Grade (MV_B, Integer (Grade_K)) /= Grade_OK then
            raise MV_Utilities_Exception with
              "Multivector_Utilities.Factorize_Blade inhomogenous multivector detected.";
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
                        Basis_Bit := Shift_Left (1, Integer (index));
                        if (Bitmap (Blade_E) and Basis_Bit) /= 0 then
                            Basis_Bitmap := Bitmap (Blade_E) xor Basis_Bit;
                            New_Line;
                            for index_j in 1 .. List_Length (Blades_B) loop
                                Blades_Bj := BB_Item (Blades_B, index_j);
                                if (Bitmap (Blades_Bj) and Basis_Bitmap) =
                                  Basis_Bitmap then
                                    Vec_Bitmap := Bitmap (Blades_Bj) xor
                                      Basis_Bitmap;
                                    Sc := Weight (Blades_Bj) *
                                      Canonical_Reordering_Sign
                                        (Basis_Bitmap, Bitmap (Blades_Bj));
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
            Put_Line ("An exception occurred in Multivector_Utilities.Factorize_Blade_Fast");
            raise;
    end Factorize_Blade_Fast;

    --  --------------------------------------------------------------------

   function Reflect (MV : Multivectors.Multivector;
                     DP: Multivectors.Dual_Plane)
                     return Multivectors.Multivector is
        use Metric;
        use Multivectors;
        IDP : constant Multivector := General_Inverse (DP, C3_Metric);
   begin
        return Geometric_Product
          (-DP, Geometric_Product (MV, IDP, C3_Metric), C3_Metric);
   end Reflect;

   --  ------------------------------------------------------------------------

   function Rotate (MV : Multivectors.Multivector;
                    aVersor : Multivectors.TR_Versor)
                     return Multivectors.Multivector is
        use Metric;
        use Multivectors;
        IV : constant Multivector := General_Inverse (aVersor, C3_Metric);
   begin
        return Geometric_Product
          (aVersor, Geometric_Product (MV, IV, C3_Metric), C3_Metric);
   end Rotate;

   --  ------------------------------------------------------------------------

end Multivector_Utilities;
