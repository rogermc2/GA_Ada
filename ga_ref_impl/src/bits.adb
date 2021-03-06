
package body Bits is

    function Bit_Count (Bitmap : Unsigned_32) return Natural is
        Count   : Unsigned_32 := Bitmap;
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

    function Highest_One_Bit (Bitmap : Unsigned_32) return Natural is
    begin
        return 32 - Number_Of_Leading_Zero_Bits (Bitmap);
    end Highest_One_Bit;

    --  ------------------------------------------------------------------------

    function Lowest_One_Bit (Bitmap : Unsigned_32) return Natural is
    begin
        return Number_Of_Trailing_Zero_Bits (Bitmap);
    end Lowest_One_Bit;

    --  -----------------------------------------------------------------------

    function Number_Of_Leading_Zero_Bits (Bitmap : Unsigned_32) return Natural is
        Num : Unsigned_32 := Bitmap;
    begin
        Num := Num or Shift_Right (Num, 1);
        Num := Num or Shift_Right (Num, 2);
        Num := Num or Shift_Right (Num, 4);
        Num := Num or Shift_Right (Num, 8);
        Num := Num or Shift_Right (Num, 16);
        return Bit_Count (Not Num);
    end Number_Of_Leading_Zero_Bits;

    --  ------------------------------------------------------------------------

    function Number_Of_Trailing_Zero_Bits (Bitmap : Unsigned_32) return Natural is
    begin
        return Bit_Count ((not Bitmap) and (Bitmap - 1));
    end Number_Of_Trailing_Zero_Bits;

    --  ------------------------------------------------------------------------

end Bits;
