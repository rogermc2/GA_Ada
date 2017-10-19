
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

package body GA_Maths is

    function Bit_Count (Value : Interfaces.Unsigned_32) return Natural is
    begin
        return 0;
    end Bit_Count;

    --  ------------------------------------------------------------------------

    function Canonical_Reordering_Sign (Map_A, Map_B : integer) return float is
        use Interfaces;
        A     : Unsigned_32 := Shift_Right (Unsigned_32 (Map_A), 1);
        B     : Unsigned_32 := Unsigned_32 (Map_B);
        Swaps : Natural := 0;
    begin
        while A /= 0 loop
            Swaps := Swaps + Bit_Count (A and B);
            A := Shift_Right (Unsigned_32 (A), 1);
        end loop;

        if Swaps mod 2 = 0 then  -- an even number of swaps
            return 1.0;
        else  -- an odd number of swaps
            return -1.0;
        end if;
    end Canonical_Reordering_Sign;

    --  ------------------------------------------------------------------------

end GA_Maths;
