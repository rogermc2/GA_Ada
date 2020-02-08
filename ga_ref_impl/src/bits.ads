
with GA_Maths;

package Bits is

    function Bit_Count (Bitmap : GA_Maths.Unsigned_Integer) return Natural;
    function Highest_One_Bit (Bitmap : GA_Maths.Unsigned_Integer) return Natural;
    function Lowest_One_Bit (Bitmap : GA_Maths.Unsigned_Integer) return Natural;
    function Number_Of_Leading_Zero_Bits (Bitmap : GA_Maths.Unsigned_Integer) return Natural;

end Bits;
