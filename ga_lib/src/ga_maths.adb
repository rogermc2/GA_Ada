
package body GA_Maths is

    function Bit_Count (Bitmap : Unsigned_Integer) return Natural is
        use Interfaces;
        Count   : Unsigned_32 := Interfaces.Unsigned_32 (Bitmap);
    begin
        Count := Count - ((Shift_Right (Count, 1)) and 16#55555555#);
        Count := (Count and 16#33333333#) + (Shift_Right (Count, 2) and 16#33333333#);
        Count := (Count + Shift_Right (Count, 4)) and 16#0F0F0F0F#;
        Count := Count + (Shift_Right (Count, 8));
        Count := Count + (Shift_Right (Count, 16));
        --  Return count in range 0 to 31.
        return Natural (Count and 16#3F#);
    end Bit_Count;

    --  ------------------------------------------------------------------------

    function Highest_One_Bit (Bitmap : Unsigned_Integer) return Natural is
    begin
        return 32 - Number_Of_Leading_Zero_Bits (Bitmap);
    end Highest_One_Bit;

    --  ------------------------------------------------------------------------

    function Is_Diagonal (aMatrix : Float_Matrix) return Boolean is
        epsilon : constant float := 10.0 ** (-9);
        row     : Integer := aMatrix'First;
        col     : Integer := aMatrix'First (2);
        OK      : Boolean := True;
    begin
        while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
                if row /= col then
                    OK := Abs (aMatrix (row, col)) <= epsilon;
                end if;
                col := col + 1;
            end loop;
            row := row + 1;
        end loop;
        return OK;
    end Is_Diagonal;

    --  ------------------------------------------------------------------------

    function Maximum (I1, I2 : Integer) return Integer is
        Max : Integer;
    begin
        if I1 > I2 then
            Max := I1;
        else
            Max := I2;
        end if;
        return Max;
    end Maximum;

    --  ------------------------------------------------------------------------

    function Maximum (I1, I2 : Float) return Float is
        Max : Float;
    begin
        if I1 > I2 then
            Max := I1;
        else
            Max := I2;
        end if;
        return Max;
    end Maximum;

    --  ------------------------------------------------------------------------

    function Minimum (I1, I2 : Integer) return Integer is
        Min : Integer;
    begin
        if I1 < I2 then
            Min := I1;
        else
            Min := I2;
        end if;
        return Min;
    end Minimum;

    --  ------------------------------------------------------------------------

    function Minimum (I1, I2 : Float) return Float is
        Min : Float;
    begin
        if I1 < I2 then
            Min := I1;
        else
            Min := I2;
        end if;
        return Min;
    end Minimum;

    --  ------------------------------------------------------------------------

    function Number_Of_Leading_Zero_Bits (Bitmap : Unsigned_Integer) return Natural is
        use Interfaces;
        Num : Unsigned_32 := Unsigned_32 (Bitmap);
    begin
        Num := Num or Shift_Right (Num, 1);
        Num := Num or Shift_Right (Num, 2);
        Num := Num or Shift_Right (Num, 4);
        Num := Num or Shift_Right (Num, 8);
        Num := Num or Shift_Right (Num, 16);
        return Bit_Count (Unsigned_Integer (Not Num));
    end Number_Of_Leading_Zero_Bits;

    --  ------------------------------------------------------------------------

end GA_Maths;
