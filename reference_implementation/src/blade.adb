
with Ada.Text_IO; use Ada.Text_IO;

package body Blade is

   type Metric_Array is array (Integer range <>) of float;

   function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade;
   function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                  BB : Basis_Blade; Cont : Contraction_Type)
                                  return Basis_Blade;

   --  ------------------------------------------------------------------------

   function Blade_String (aBlade : Basis_Blade; BV_Names : Basis_Vector_Names)
                          return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use Names_Package;
      BM        : Unsigned_Integer := aBlade.Bitmap;
      Index     : Natural := 1;
      Scale     : GA_Maths.float_3 := GA_Maths.float_3 (Weight (aBlade));
      Name      : Unbounded_String;
      Val       : Unbounded_String;
      theString : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
   begin
      if BM = 0 then
         theString := To_Unbounded_String (GA_Maths.float_3'Image (Scale));
      else
         while BM /= 0 loop
            --           Put_Line ("Blade, BM: " & Unsigned_Integer'Image (BM));
            if (BM and 1) /= 0 then
               if Length (theString) > 0 then
                  theString := theString & "^";
               end if;

               if Is_Empty (Vector (BV_Names)) or
                 (Index > Natural (Length (Vector (BV_Names))) or
                      (Index - 1) < 1) then
                  theString := theString & "e";
                  Val := To_Unbounded_String (Natural'Image (Index));
                  Val := Trim (Val, Ada.Strings.Left);
                  theString := theString & Val;
               else
                  Name := Element (BV_Names, Index - 1);
                  theString := theString & Name;
               end if;
               --              Put_Line ("Blade theString:  " & To_String (theString));
            end if;
            BM := BM / 2;  --  BM >>= 1;
            Index := Index + 1;
         end loop;

         if Length (theString) > 0 then
            theString := GA_Maths.float_3'Image (Scale) & " * " & theString;
         end if;
      end if;
      return theString;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Blade.Blade_String.");
         raise;
   end Blade_String;

   --  -------------------------------------------------------------------------

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer is
   begin
      return BB.Bitmap;
   end Bitmap;

   --  ------------------------------------------------------------------------

   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float is
      use GA_Maths;
      A     : Unsigned_Integer := Map_A / 2;
      Swaps : Natural := 0;
   begin
      while A /= 0 loop
         Swaps := Swaps + Bit_Count (A and Map_B);
         A := A / 2;
      end loop;

      if Swaps mod 2 = 0 then  -- an even number of swaps
         return 1.0;
      else  -- an odd number of swaps
         return -1.0;
      end if;
   end Canonical_Reordering_Sign;

   --  ------------------------------------------------------------------------

   function Geometric_Product (BB : Basis_Blade; Sc : Float) return Basis_Blade is
      S_Blade : Basis_Blade := New_Scalar_Blade (Sc);
   begin
      return GP_OP (BB, S_Blade, False);
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   function Geometric_Product (BA, BB : Basis_Blade) return Basis_Blade is
   begin
      return GP_OP (BA, BB, False);
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   function Geometric_Product (BA, BB : Basis_Blade;
                               Met : Metric.Metric_Record) return Basis_Blade is
      Result     : Basis_Blade := Geometric_Product (BA, BB);
      BM         : Unsigned_Integer := Bitmap (BA) and Bitmap (BB);
      Row        : Integer range 1 .. 6 := 1;
      Col        : Integer range 1 .. 5 := 1;
      Met_Matrix : GA_Maths.Float_Matrix := Metric.Matrix (Met);
   begin
      while BM /= 0 loop
         if (BM and 1) /= 0 then
            Result.Weight := Result.Weight * Met_Matrix (Row, Col);
         end if;
         if Col = 5 then
            Row := Row + 1;
            Col := 1;
         else
            Col := Col + 1;
         end if;
         BM := BM / 2;
      end loop;
      return Result;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Blade.Geometric_Product with Metric.");
         raise;
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade is
      OP_Blade : Basis_Blade;
      Sign     : Float;
   begin
      if Outer and then (BA.Bitmap and BB.Bitmap) /= 0 then
         null;  --  return zero blade
      else
         --  if BA.Bitmap = BB.Bitmap, xor = 0, so Dot product part of MV
         --  else xor > 0 so Outer product part of MV
         OP_Blade.Bitmap := BA.Bitmap xor BB.Bitmap;
         Sign := Canonical_Reordering_Sign (BA.Bitmap, BB.Bitmap);
         OP_Blade.Weight := Sign * BA.Weight * BB.Weight;
      end if;

      return OP_Blade;
   end GP_OP;

   --  ------------------------------------------------------------------------

   function Grade (BB : Basis_Blade) return Integer is
   begin
      return  GA_Maths.Bit_Count (BB.Bitmap);
   end Grade;

   --  ------------------------------------------------------------------------

   function Grade_Inversion (B : Basis_Blade) return Basis_Blade is
      W : constant float
        := Float (Minus_1_Power (Integer (Grade (B))) * Integer (B.Weight));
   begin
      return New_Basis_Blade (B.Bitmap, W);
   end Grade_Inversion;

   --  ------------------------------------------------------------------------

   function Inner_Product (BA, BB : Basis_Blade; Cont : Contraction_Type)
                           return Basis_Blade is
   begin
      return Inner_Product_Filter (Grade (BA), Grade (BB),
                                   Geometric_Product (BA, BB), Cont);
   end Inner_Product;

   --  ------------------------------------------------------------------------

   function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                  BB : Basis_Blade; Cont : Contraction_Type)
                                  return Basis_Blade is
      IP_Blade : Basis_Blade;
   begin
      case Cont is
         when Left_Contraction =>
            if (Grade_1 > Grade_2) or (Grade (BB) /= (Grade_2 - Grade_1)) then
               null;
            else  --  Grade_1 <= Grade_2 and Grade (BB) = Grade_2 - Grade_1
               IP_Blade := BB;
--                 Print_Blade ("Inner_Product_Filter LC result", IP_Blade);
            end if;
         when Right_Contraction =>
            if (Grade_1 < Grade_2) or (Grade (BB) /= Grade_1 - Grade_2) then
               null;
            else
                IP_Blade := BB;
            end if;
         when Hestenes_Inner_Product =>
            if (Grade_1 = 0) or (Grade_2 = 0) then
               null;
            elsif Abs (Grade_1 - Grade_2) = Grade (BB) then
               IP_Blade := BB;
            end if;
         when Modified_Hestenes_Inner_Product =>
            if Abs (Grade_1 - Grade_2) = Grade (BB) then
               IP_Blade := BB;
            end if;
      end case;
      return IP_Blade;
   end Inner_Product_Filter;

   --  ------------------------------------------------------------------------

   function Minus_1_Power (Power : Integer) return Integer is
   begin
      return (-1) ** Power;
   end Minus_1_Power;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Bitmap : Unsigned_Integer; Weight : Float := 1.0)
                             return Basis_Blade is
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := Bitmap;
      Blade.Weight := Weight;
      return Blade;
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : BV_Base; Weight : Float := 1.0) return Basis_Blade is
   begin
      return (Index'Enum_Rep, Weight);
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : E2_Base; Weight : Float := 1.0) return Basis_Blade is
   begin
      return (Index'Enum_Rep, Weight);
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : E3_Base; Weight : Float := 1.0) return Basis_Blade is
   begin
      return (Index'Enum_Rep, Weight);
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Index : C3_Base; Weight : Float := 1.0) return Basis_Blade is
   begin
      return (Index'Enum_Rep, Weight);
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Scalar_Blade (Weight : Float := 1.0) return Basis_Blade is
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := 0;
      Blade.Weight := Weight;
      return Blade;
   end New_Scalar_Blade;

   --  ------------------------------------------------------------------------

   function New_Zero_Blade return Basis_Blade is
      Blade : Basis_Blade;
   begin
      return Blade;
   end New_Zero_Blade;

   --  ------------------------------------------------------------------------

   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade is
   begin
      return GP_OP (BA, BB, True);
   end Outer_Product;

   --  ------------------------------------------------------------------------

   procedure Print_Blade (Name : String; B : Basis_Blade) is
   begin
      New_Line;
      Put_Line (Name & " Bitmap and Weight");
         Put_Line (GA_Maths.Unsigned_Integer'Image (Bitmap (B)) &
                   "  " & float'Image (Weight (B)));
   end Print_Blade;

   --  ------------------------------------------------------------------------

   function Reverse_Blade (B : Basis_Blade) return Basis_Blade is
      G   : constant Integer := Grade (B); -- Bit_Count (B.Bitmap)
      W   : constant float
        := Float (Minus_1_Power (G * (G - 1) / 2)) * B.Weight;
      Rev : constant Basis_Blade := (B.Bitmap, W);
   begin
      return Rev;
   end Reverse_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Weight : Float) is
   begin
      BB.Weight := Weight;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Blade.Update_Blade 1");
         raise;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer) is
   begin
      BB.Bitmap := Bitmap;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Blade.Update_Blade 2");
         raise;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Weight : Float) is
   begin
      BB.Bitmap := Bitmap;
      BB.Weight := Weight;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Blade.Update_Blade 3");
         raise;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   function Weight (BB : Basis_Blade) return Float is
   begin
      return BB.Weight;
   end Weight;

   --  ------------------------------------------------------------------------

end Blade;
