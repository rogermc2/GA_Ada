
with Interfaces;

with Maths;

with Blade;
with Blade_Types;
with GA_Maths;
with Multivectors;

package body Inner_Product_Types is

    type Scale_Array is array (Integer range <>) of Float;

    function Factorize_Blade (B : Multivectors.Multivector;
                              Scale : in out Scale_Array)
                              return Multivectors.Multivector;
    --  --------------------------------------------------------------------

    function Init_Conformal return Metric.Metric_Matrix is
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
        Scales : Scale_Array (1 .. 1);
--          Sss    : Blade_List;
        B      : Multivector;
        F      : Multivector;
        Blades : Blade_List;
    begin
        for index in 1 .. dim loop
            B := Random_Blade
              (Dim, Integer (Float (Maths.Random_Float) * (float (Dim) + 0.49)), 1.0);
            Blades.Append (New_Basis_Blade (30, -0.662244));
            Blades.Append (New_Basis_Blade (29, -0.391495));
            Blades.Append (New_Basis_Blade (27, -0.430912));
            Blades.Append (New_Basis_Blade (23,  0.218277));
            Blades.Append (New_Basis_Blade (15, -0.213881));
            Update (B, Blades);
            F := Factorize_Blade (B, Scales);
        end loop;
        return  C3_M;
    end Init_Conformal;

    --  --------------------------------------------------------------------

    function Factorize_Blade (B : Multivectors.Multivector; Scale : in out Scale_Array)
                             return Multivectors.Multivector is
                             use Interfaces;
        use Blade;
        use Blade_Types;
        use GA_Maths;
        use Multivectors;
        K        : Integer;
        S        : Float;
        E        : Basis_Blade;
        E_Array  : array (0 .. 100) of Basis_Blade;
        Idx      : Integer := 0;
        Test_Bit : Unsigned_32;
        Result   : Multivector := New_Multivector (0.0);
        OK       : constant Boolean := Grade (B, K);
    begin
        if not OK then
            raise Inner_Product_Types_Exception with
              "Inner_Product_Types.Factorize_Blade Empty multivector detected.";
        else
            if K = 0 then
                S := Scalar_Part (B);
            else
                S := Norm_E (B);
            end if;

            if Scale'length > 0 then
                Scale (1) := S;
            end if;

            if K > 0 and S /= 0.0 then
                E := Largest_Basis_Blade (B);
                E_Array (0) := New_Basis_Blade (C3_Base'Enum_Val (K));

                for Index_G in Unsigned_32 range 0 .. Unsigned_32 (Space_Dimension (B) - 1) loop
                    Test_Bit := Shift_Left (1, Integer (Index_G));
                    if Bitmap (E) > 0 and Test_Bit /= 0 then
                        Idx := Idx + 1;
                        E_Array (Idx) := New_Basis_Blade (C3_Base'Enum_Val (Test_Bit), 1.0);
                    end if;
                end loop;
            end if;
        end if;
        return  Result;
    end Factorize_Blade;

    --  --------------------------------------------------------------------

end Inner_Product_Types;
