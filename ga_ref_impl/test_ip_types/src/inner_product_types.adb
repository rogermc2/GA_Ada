
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

with Maths;

with Bits;
with Blade;
with Blade_Types;
with C3GA;
with GA_Maths;
with GA_Utilities;
with Metric;
with Multivectors;

package body Inner_Product_Types is

    type Scale_Array is array (Integer range <>) of Float;

    function Factorize_Blade (MV_B : Multivectors.Multivector;
                              Scale : in out Scale_Array)
                              return Multivectors.Multivector_List;
    function Factorize_Blade_Fast (MV_B : Multivectors.Multivector; Scale : in out Scale_Array)
                                   return Multivectors.Multivector_List;

    --  --------------------------------------------------------------------

    procedure Test is
        use Blade;
        use Multivectors;
        C3_M : constant Metric.Metric_Matrix (1 .. 5, 1 .. 5) :=
                 ((0.0, 0.0, 0.0, 0.0, -1.0),
                  (0.0, 1.0, 0.0, 0.0, 0.0),
                  (0.0, 0.0, 1.0, 0.0, 0.0),
                  (0.0, 0.0, 0.0, 1.0, 0.0),
                  (-1.0, 0.0, 0.0, 0.0, 0.0));
        Dim    : constant Integer := 8;
        --          no     : Multivector := New_Multivector (New_Basis_Blade (0));
        --          e1     : Multivector := New_Multivector (New_Basis_Blade (0));
        --          e2     : Multivector := New_Multivector (New_Basis_Blade (0));
        --          e3     : Multivector := New_Multivector (New_Basis_Blade (0));
        --          ni     : Multivector := New_Multivector (New_Basis_Blade (0));
        --          Scale  : Float;
        Scales       : Scale_Array (1 .. 1);
        Sss          : array (1 .. Dim + 1) of String (1 .. 1);
        Blades       : Blade_List;
        MV_B         : Multivector;
        MV_R         : Multivector;
        MV_Fast      : Multivector;
        Factors      : Multivector_List;
        Fast_Factors : Multivector_List;
        K            : Integer;
        OK           : Boolean;
        Check_Scale  : Float;
    begin
        Scales (1) := 0.0;
        for index in 1 .. dim loop
            MV_B := Random_Blade
              (Dim, Integer (Float (Maths.Random_Float) * (float (Dim) + 0.49)), 1.0);
            Blades.Append (New_Basis_Blade (30, -0.662244));
            Blades.Append (New_Basis_Blade (29, -0.391495));
            Blades.Append (New_Basis_Blade (27, -0.430912));
            Blades.Append (New_Basis_Blade (23,  0.218277));
            Blades.Append (New_Basis_Blade (15, -0.213881));
            Update (MV_B, Blades);

            Factors := Factorize_Blade (MV_B, Scales);
            Put_Line ("Inner_Product_Types.Test Factors size: " &
                        Integer'Image (List_Length (Factors)));
            MV_R := New_Multivector (1.0);
            for index in 1 .. List_Length (Factors) loop
                MV_R := Outer_Product (MV_R, MV_Item (Factors, index));
            end loop;
--              GA_Utilities.Print_Multivector ("Inner_Product_Types.Test MV_R", MV_R);

            Fast_Factors := Factorize_Blade_Fast (MV_B, Scales);
            Put_Line ("Inner_Product_Types.Test Fast_Factors size: " &
                        Integer'Image (List_Length (Fast_Factors)));
            MV_Fast := New_Multivector (1.0);
            for index in 1 .. List_Length (Fast_Factors) loop
                MV_Fast := Outer_Product (MV_Fast, MV_Item (Fast_Factors, index));
            end loop;

            MV_B := Unit_E (MV_B);
            MV_R := Unit_E (MV_R);
            MV_Fast := Unit_E (MV_Fast);

            OK := Grade (MV_B, K);
            if not OK then
                raise Inner_Product_Types_Exception with
                  "Inner_Product_Types.Test, empty multivector detected.";
            else
                Check_Scale := Scalar_Part (Geometric_Product (MV_R, Versor_Inverse (MV_Fast)));
                if Check_Scale < 0.0 then
                    Put_Line ("Whaaaaa! Scalar_Part < 0.");
                    Sss (K) := "-";
                else
                    Sss (K) := "+";
                end if;
                Put_Line ("B = " & C3GA.Multivector_String (MV_B));
            end if;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Inner_Product_Types.Test");
            raise;
    end Test;

    --  --------------------------------------------------------------------

    function Factorize_Blade (MV_B : Multivectors.Multivector; Scale : in out Scale_Array)
                              return Multivectors.Multivector_List is
        use Interfaces;
        use Blade;
        use Blade_Types;
        use GA_Maths;
        use Multivectors;
        K         : Integer;
        S         : Float;
        E         : Basis_Blade;
        E_Array   : array (0 .. Space_Dimension (MV_B)) of Basis_Blade;
        Idx       : Integer := 0;
        Basis_Bit : Unsigned_32;
        B_Current : Multivector;
        aFactor   : Multivector;
        Factors   : Multivector_List;
        OK        : constant Boolean := Grade (MV_B, K);
    begin
        if not OK then
            raise Inner_Product_Types_Exception with
              "Inner_Product_Types.Factorize_Blade Empty multivector detected.";
        else
            if K = 0 then
                S := Scalar_Part (MV_B);
            else
                S := Norm_E (MV_B);
            end if;

            if Scale'length > 0 then
                Scale (1) := S;
            end if;

            if K > 0 and S /= 0.0 then
                E := Largest_Basis_Blade (MV_B);
                E_Array (0) := New_Basis_Blade (C3_Base'Enum_Val (K));

                for Index_G in Unsigned_32 range 0 .. Unsigned_32 (Space_Dimension (MV_B) - 1) loop
                    Basis_Bit := Shift_Left (1, Integer (Index_G));
                    if Bitmap (E) > 0 and Basis_Bit /= 0 then
                        Idx := Idx + 1;
                        E_Array (Idx) := New_Basis_Blade (C3_Base'Enum_Val (Basis_Bit), 1.0);
                    end if;
                end loop;

                B_Current := Geometric_Product (MV_B, 1.0 / S);
                for index in 0 .. K - 2 loop
                    aFactor := New_Multivector (E_Array (index));
                    aFactor := Inner_Product
                      (Inner_Product (aFactor, B_Current, Left_Contraction),
                       B_Current, Left_Contraction);
                    aFactor := Unit_E (aFactor);
                    Add_Multivector (Factors, aFactor);
                    --  Remove aFactor from B_Current
                    B_Current := Inner_Product (aFactor, B_Current, Left_Contraction);
                end loop;
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

    function Factorize_Blade_Fast (MV_B : Multivectors.Multivector; Scale : in out Scale_Array)
                                   return Multivectors.Multivector_List is
        use Interfaces;
        use Blade;
        use Blade_Types;
        use GA_Maths;
        use Multivectors;
        K            : Integer;
        S            : Float;
        Sc           : Float;
        E            : Basis_Blade;
        Lowest_Bit   : Integer;
        Highest_Bit  : Integer;
        Blades_B     : Blade_List;
        Basis_Bit    : Unsigned_32;
        Basis_Bitmap : Unsigned_32;
        Vec_Bitmap   : Unsigned_32;
        Blades_Bj    : Basis_Blade;
        Factors      : Multivector_List;  --  F
        L_List       : Blade_List;
        OK           : constant Boolean := Grade (MV_B, K);
    begin
        if not OK then
            raise Inner_Product_Types_Exception with
              "Inner_Product_Types.Factorize_Blade Empty multivector detected.";
        else
            if K = 0 then
                S := Scalar_Part (MV_B);
            else
                S := Norm_E (MV_B);
            end if;
            Scale (1) := S;

            if K > 0 and S /= 0.0 then
                E := Largest_Basis_Blade (MV_B);
                Lowest_Bit := Bits.Lowest_One_Bit (Bitmap (E));
                Highest_Bit := Bits.Highest_One_Bit (Bitmap (E));

                if K = 1 then
                    Add_Multivector (Factors, Unit_E (MV_B));
                else
                    if Weight (E) < 0.0 then
                        Scale (1) := - Scale (1);
                        --  take care of orientation of blade:

                        if (Unsigned_Integer (K) and 1) = 1 then
                            Scale (1) := - Scale (1);
                        end if;
                    end if;

                    --  fix sign issues
                    if (K mod 4) = 2 then
                        Scale (1) := - Scale (1);
                    end if;

                    Blades_B := Blades (MV_B);

                    for index in Lowest_Bit .. Highest_Bit loop
                        Basis_Bit := Shift_Left (1, Integer (index));
                        if Bitmap (E) > 0 and Basis_Bit /= 0 then
                            Basis_Bitmap := Unsigned_32 (Bitmap (E)) xor Basis_Bit;
                            for index_j in 1 .. List_Length (Blades_B) loop
                                Blades_Bj := BB_Item (Blades_B, index_j);
                                if (Unsigned_32 (Bitmap (Blades_Bj)) and Basis_Bitmap) =
                                  Basis_Bitmap then
                                    Vec_Bitmap := Unsigned_32 (Bitmap (Blades_Bj)) xor
                                      Basis_Bitmap;
                                    Sc := Weight (Blades_Bj) *
                                      Canonical_Reordering_Sign
                                        (Unsigned_Integer (Basis_Bitmap), Bitmap (Blades_Bj));
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