with Interfaces; use Interfaces;

package Bits is

   function Bit_Count (Bitmap : Unsigned_32) return Natural;
   function Highest_One_Bit (Bitmap : Unsigned_32; Bit : out Natural) return Boolean;
   function Lowest_One_Bit (Bitmap : Unsigned_32) return Natural;
   function Number_Of_Leading_Zero_Bits (Bitmap : Unsigned_32) return Natural;
   function Number_Of_Trailing_Zero_Bits (Bitmap : Unsigned_32) return Natural;

end Bits;
