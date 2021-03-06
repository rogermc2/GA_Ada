
with Ada.Text_IO; use Ada.Text_IO;

with Bits;

package body Blade is

   epsilon : constant Float := 10.0 ** (-6);

   function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade;
   function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                  Blades           : Blade_List; Cont : Contraction_Type)
                                   return Blade_List;
   function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                  BB               : Basis_Blade; Cont : Contraction_Type)
                                   return Basis_Blade;

   function New_Blade (Weight : Float := 1.0) return Basis_Blade;
   function To_Eigen_Basis (BB : Basis_Blade; Met : Metric.Metric_Record)
                             return Blade_List;
   function To_Metric_Basis (BL : Blade_List; Met : Metric.Metric_Record)
                              return Blade_List;
   function Transform_Basis (BA : Blade.Basis_Blade; Met : GA_Maths.Float_Matrix)
                              return Blade_List;

   --  -------------------------------------------------------------------------

   function "<" (Left, Right : Blade.Basis_Blade) return Boolean is
   begin
      return Bitmap (Left) < Bitmap (Right);
   end "<";

   --  ------------------------------------------------------------------------

   function "*" (S : Float; BB : Basis_Blade) return Basis_Blade is

   begin
      return (BB.Bitmap, S * BB.Weight);
   end "*";

   --  ------------------------------------------------------------------------

   function "*" (BB : Basis_Blade; S : Float) return Basis_Blade is
   begin
      return S * BB;
   end "*";

   --  ------------------------------------------------------------------------

   procedure Add_Blade (Blades : in out Blade_List; BB : Basis_Blade)is
   begin
      Blades.Append (BB);
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (Blades : in out Blade_Vector;
                        Index  : Natural; BB : Basis_Blade)is
   begin
      Blades.Replace_Element (Index, BB);
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blades (Blades : in out Blade_List; More_Blades : Blade_List) is
      use Blade_List_Package;
      Curs   : Cursor := More_Blades.First;
      aBlade : Basis_Blade;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         if Weight (aBlade) /= 0.0 then
            Blades.Append (aBlade);
         end if;
         Next (Curs);
      end loop;
   end Add_Blades;

   --  -------------------------------------------------------------------------

   function BB_First (BB_List : Blade_List) return Basis_Blade is
   begin
      return BB_List.First_Element;
   end BB_First;

   --  -------------------------------------------------------------------------

   function BB_Item (BB_List : Blade_List; Index : Integer) return Basis_Blade is
      use Blade_List_Package;
      Curs : Cursor := BB_List.First;
   begin
      if Index > 1 then
         for count in 2 .. Index loop
            Next (Curs);
         end loop;
      end if;
      return Element (Curs);
   end BB_Item;

   --  -------------------------------------------------------------------------

   function Blade_String (aBlade : Basis_Blade; BV_Names : Basis_Vector_Names)
                           return Ada.Strings.Unbounded.Unbounded_String is
      use Names_Package;
      BM        : Unsigned_32 := aBlade.Bitmap;
      Bit_Num   : Natural := 1;
      Scale     : constant GA_Maths.float_3 := GA_Maths.float_3 (Weight (aBlade));
      Name      : Unbounded_String;
      Val       : Unbounded_String;
      theString : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
   begin
      --        Put_Line ("Blade.Blade_String, initial BM: " & Unsigned_32'Image (BM));
      if BM = 0 then
         theString := To_Unbounded_String (GA_Maths.float_3'Image (Scale));
      else
         while BM /= 0 loop
            --              Put_Line ("Blade.Blade_String, BM, Bit_Num: " & Unsigned_32'Image (BM) &
            --                          ",   " & Natural'Image (Bit_Num));
            if (BM and 1) /= 0 then
               --                 Put_Line ("Blade.Blade_String, BM bit detected: " & Unsigned_32'Image (BM));
               if Length (theString) > 0 then
                  theString := theString & "^";
               end if;

               if Is_Empty (Vector (BV_Names)) or
                 (Bit_Num > Natural (Length (Vector (BV_Names))) or
                      (Bit_Num) < 1) then
                  theString := theString & "e";
                  Val := To_Unbounded_String (Natural'Image (Bit_Num));
                  Val := Trim (Val, Ada.Strings.Left);
                  theString := theString & Val;
               else
                  Name := Element (BV_Names, Bit_Num);
                  theString := theString & Name;
               end if;
               --                 Put_Line ("Blade.Blade_String, theString:  " & To_String (theString));
            end if;
            BM := BM / 2;  --  BM >>= 1;
            Bit_Num := Bit_Num + 1;
         end loop;

         if Length (theString) > 0 then
            theString := GA_Maths.float_3'Image (Scale) & " * " & theString;
         end if;
      end if;
      --        Put_Line ("Blade.Blade_String, final theString:  " & To_String (theString));
      return theString;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Blade_String.");
         raise;
   end Blade_String;

   --  -------------------------------------------------------------------------

   function Bitmap (BB : Basis_Blade) return Unsigned_32 is
   begin
      return BB.Bitmap;
   end Bitmap;

   --  ------------------------------------------------------------------------

   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_32) return float is
      A     : Unsigned_32 := Map_A / 2;
      Swaps : Natural := 0;
   begin
      while A /= 0 loop
         Swaps := Swaps + Bits.Bit_Count (A and Map_B);
         A := A / 2;
      end loop;

      if Swaps mod 2 = 0 then  -- an even number of swaps
         return 1.0;
      else  -- an odd number of swaps
         return -1.0;
      end if;
   end Canonical_Reordering_Sign;

   --  ------------------------------------------------------------------------
   --  Based on BasisBlade.java geometricProduct(BasisBlade a, BasisBlade b, double[] m)
   --  wher m is an array of doubles giving the metric for each basis vector.
   function Geometric_Product (BB : Basis_Blade; Sc : Float) return Basis_Blade is
      S_Blade : constant Basis_Blade := New_Scalar_Blade (Sc);
   begin
      return GP_OP (BB, S_Blade, False);
   end Geometric_Product;

   --  ------------------------------------------------------------------------
   --  Geometric_Product computes the geometric product of two basis blades.
   function Geometric_Product (BA, BB : Basis_Blade) return Basis_Blade is
   begin
      return GP_OP (BA, BB, False);
   end Geometric_Product;

   --  ------------------------------------------------------------------------
   --  This Geometric_Product returns an ArrayList because
   --  the result does not have to be a single BasisBlade.
   function Geometric_Product (BA, BB : Basis_Blade;
                               Met    : Metric.Metric_Record) return Blade_List is
      use Blade_List_Package;
      use GA_Maths.Float_Array_Package;
      List_A     : constant Blade_List := To_Eigen_Basis (BA, Met);
      List_B     : constant Blade_List := To_Eigen_Basis (BB, Met);
      LA_Cursor  : Cursor := List_A.First;
      LB_Cursor  : Cursor;
      Eigen_Vals : constant Real_Vector := Metric.Eigen_Values (Met);  --  M.getEigenMetric
      GP         : Basis_Blade;
      Result     : Blade_List;
   begin
      --  List_A and List_B needed because To_Eigen_Basis returns an ArrayList
      --  because its result does not have to be a single BasisBlade.
      while Has_Element (LA_Cursor) loop
         LB_Cursor := List_B.First;
         while Has_Element (LB_Cursor) loop
            GP := Geometric_Product
              (Element (LA_Cursor), Element (LB_Cursor), Eigen_Vals);
            Add_Blade (Result, (GP));
            Next (LB_Cursor);
         end loop;
         Next (LA_Cursor);
      end loop;

      Simplify (Result);
      Result := To_Metric_Basis (Result, Met);
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Geometric_Product with Metric.");
         raise;
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   function Geometric_Product (BA, BB     : Basis_Blade;
                               Eigen_Vals : GA_Maths.Float_Array_Package.Real_Vector)
                                return Basis_Blade is
      --  Eigen_Vals gives the metric for each basis vector
      --  BM is the meet (bitmap of annihilated vectors)
      --  Only retain vectors common to both blades
      BM          : Unsigned_32 := Bitmap (BA) and Bitmap (BB); --  M.getEigenMetric
      Index       : Integer := 1;
      New_Blade   : Basis_Blade := Geometric_Product (BA, BB); --  Euclidean metric
   begin
      while BM /= 0 loop
         if (BM and 1) /= 0 then
            --  This basis vector is non-zero
            New_Blade.Weight := New_Blade.Weight * Eigen_Vals (Index);
         end if;
         --  Move right to next basis vector indicator
         --           BM := BM / 2;
         BM := Shift_Right (BM, 1);
         Index := Index + 1;
      end loop;

      if New_Blade.Weight = 0.0 then
         New_Blade := New_Basis_Blade;
      end if;
      return New_Blade;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Geometric_Product with Metric.");
         raise;
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade is
      OP_Blade : Basis_Blade;
      Sign     : Float;
   begin
      if Outer and then (BA.Bitmap and BB.Bitmap) /= 0 then
         --  BA and BB are parallel; so their volume is zero
         OP_Blade  := New_Basis_Blade (0, 0.0);  --  return zero blade
      else  --  compute geometric product
         --  if BA.Bitmap = BB.Bitmap, xor = 0, so Dot product part of MV
         --  else xor > 0 so Outer product part of MV
         Sign := Canonical_Reordering_Sign (BA.Bitmap, BB.Bitmap);
         OP_Blade := New_Blade
           (BA.Bitmap xor BB.Bitmap, Sign * BA.Weight * BB.Weight);
      end if;
      return OP_Blade;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.GP_OP");
         raise;
   end GP_OP;

   --  ------------------------------------------------------------------------

   function Grade (BB : Basis_Blade) return Integer is
   begin
      return  Bits.Bit_Count (BB.Bitmap);
   end Grade;

   --  ------------------------------------------------------------------------

   function Grade_Inversion (B : Basis_Blade) return Basis_Blade is
      W : constant Float
        := Float (Minus_1_Power (Grade (B)) * Integer (B.Weight));
   begin
      return New_Blade (B.Bitmap, W);
   end Grade_Inversion;

   --  ------------------------------------------------------------------------

   function Inner_Product (BA, BB : Basis_Blade; Cont : Contraction_Type)
                            return Basis_Blade is
   begin
      return Inner_Product_Filter (Grade (BA), Grade (BB),
                                   Geometric_Product (BA, BB), Cont);
   end Inner_Product;

   --  ------------------------------------------------------------------------

   function Inner_Product (BA, BB : Basis_Blade; Met : Metric.Metric_Record;
                           Cont   : Contraction_Type) return Blade_List is
      GP : constant Blade_List := Geometric_Product (BA, BB, Met);
   begin
      return Inner_Product_Filter (Grade (BA), Grade (BB), GP, Cont);
   end Inner_Product;

   --  ------------------------------------------------------------------------

   function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                  BB               : Basis_Blade; Cont : Contraction_Type)
                                   return Basis_Blade is
      IP_Blade : Basis_Blade;
   begin
      case Cont is
         when Left_Contraction =>
            if (Grade_1 > Grade_2) or (Grade (BB) /= (Grade_2 - Grade_1)) then
               IP_Blade := New_Basis_Blade;
            else  --  Grade_1 <= Grade_2 and Grade (BB) = Grade_2 - Grade_1
               IP_Blade := BB;
            end if;

         when Right_Contraction =>
            if (Grade_1 < Grade_2) or (Grade (BB) /= Grade_1 - Grade_2) then
               IP_Blade := New_Basis_Blade;
            else
               IP_Blade := BB;
            end if;

         when Hestenes_Inner_Product =>
            if (Grade_1 = 0) or (Grade_2 = 0) then
               IP_Blade := New_Basis_Blade;
            elsif Abs (Grade_1 - Grade_2) = Grade (BB) then
               IP_Blade := BB;
            end if;

         when Modified_Hestenes_Inner_Product =>
            if Abs (Grade_1 - Grade_2) = Grade (BB) then
               IP_Blade := BB;
            else
               IP_Blade := New_Basis_Blade;
            end if;
      end case;

      return IP_Blade;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Inner_Product_Filter");
         raise;
   end Inner_Product_Filter;

   --  ------------------------------------------------------------------------

   function Inner_Product_Filter (Grade_1, Grade_2  : Integer;
                                  Blades            : Blade_List;
                                  Cont              : Contraction_Type)
                                   return Blade_List is
      use Blade_List_Package;
      Blade_Cursor : Cursor := Blades.First;
      aBlade       : Basis_Blade;
      New_Blades   : Blade_List;
   begin
      while Has_Element (Blade_Cursor) loop
         --  Inner_Product_Filter returns either a null blade or Element (Blade_Cursor)
         aBlade := Inner_Product_Filter (Grade_1, Grade_2,
                                         Element (Blade_Cursor), Cont);
         if Weight (aBlade) /= 0.0 then
            Add_Blade (New_Blades, (aBlade));
         end if;
         Next (Blade_Cursor);
      end loop;

      return New_Blades;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Inner_Product_Filter");
         raise;
   end Inner_Product_Filter;

   --  ------------------------------------------------------------------------

   function List_Length (Blades : Blade_List) return Integer is
   begin
      return Integer (Blades.Length);
   end List_Length;

   --  ------------------------------------------------------------------------

   function Minus_1_Power (Power : Integer) return Integer is
   begin
      return (-1) ** Power;
   end Minus_1_Power;

   --  ------------------------------------------------------------------------

   function New_Blade (Weight : Float := 1.0) return Basis_Blade is
      Blade : Basis_Blade;
   begin
      Blade.Weight := Weight;
      return Blade;
   end New_Blade;

   --  ------------------------------------------------------------------------

   function New_Blade (Bitmap : Unsigned_32; Weight : Float := 1.0)
                        return Basis_Blade is
      Blade : Basis_Blade;
   begin
      if Bitmap < 32 then
         Blade.Bitmap := Bitmap;
         Blade.Weight := Weight;
      else
         raise Blade_Exception with
           "Blade.New_Blade, invalid Bitmap" & Unsigned_32'Image (Bitmap);
      end if;
      return Blade;
   end New_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Bitmap : Unsigned_32; Weight : Float := 1.0)
                              return Basis_Blade is
      Blade : Basis_Blade;
   begin
      if Bitmap < 32 then
         Blade.Bitmap := Bitmap;
         Blade.Weight := Weight;
      else
         raise Blade_Exception with
           "Blade.New_Basis_Blade, invalid Bitmap" & Unsigned_32'Image (Bitmap);
      end if;
      return Blade;
   end New_Basis_Blade;

   --  ------------------------------------------------------------------------

   function New_Basis_Blade (Weight : Float := 0.0) return Basis_Blade is
      Blade : Basis_Blade;
   begin
      Blade.Bitmap := 0;
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

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.New_Basis_Blade");
         raise;
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

   function New_Complex_Basis_Blade (Index  : C3_Base;
                                     Weight : GA_Maths.Complex_Types.Complex := (0.0, 1.0))
                                      return Complex_Basis_Blade is
   begin
      return (Index'Enum_Rep, Weight);
   end New_Complex_Basis_Blade;

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

   function Reverse_Blade (B : Basis_Blade) return Basis_Blade is
      G   : constant Integer := Grade (B); -- Bit_Count (B.Bitmap)
      W   : constant Float
        := Float (Minus_1_Power ((G * (G - 1)) / 2)) * B.Weight;
   begin
      return (B.Bitmap, W);
   end Reverse_Blade;

   --  ------------------------------------------------------------------------

   procedure Simplify (Blades : in out Blade_List) is
      use Blade_List_Package;
      Current_Blade  : Blade.Basis_Blade;
      Blade_B        : Blade.Basis_Blade;
      Blade_Cursor   : Cursor;
      Result         : Blade_List;
   begin
      if List (Blades) /= Empty_List then
         Blade_Cursor := Blades.First;
         while Has_Element (Blade_Cursor) loop
            Blade_B := Element (Blade_Cursor);
            if Weight (Blade_B) = 0.0 then
               Delete (Blades, Blade_Cursor);
            else
               Next (Blade_Cursor);
            end if;
         end loop;

         if List (Blades) /= Empty_List then
            Blade_Sort_Package.Sort (List (Blades));
            Blade_Cursor := Blades.First;
            Current_Blade := Element (Blade_Cursor);
            Next (Blade_Cursor);

            while Has_Element (Blade_Cursor) loop
               Blade_B := Element (Blade_Cursor);
               if Bitmap (Blade_B) = Bitmap (Current_Blade) then
                  Current_Blade :=
                    New_Blade (Bitmap (Current_Blade),
                               Weight (Current_Blade) + Weight (Blade_B));
               else
                  if Abs (Weight (Current_Blade)) > epsilon then
                     Result.Append (Current_Blade);
                  end if;
                  Current_Blade := Blade_B;
               end if;
               Next (Blade_Cursor);
            end loop;

            if Abs (Weight (Current_Blade)) > epsilon then
               Result.Append (Current_Blade);
            end if;
            Blades := Result;
         end if;
      end if;
   end Simplify;

   --  -------------------------------------------------------------------------

   function To_Eigen_Basis (BB : Basis_Blade; Met : Metric.Metric_Record)
                             return Blade_List is
   begin
      return Transform_Basis (BB, GA_Maths.Float_Matrix (Metric.Eigen_Vectors (Met)));
   end To_Eigen_Basis;

   --  ------------------------------------------------------------------------

   function To_Metric_Basis (BB : Basis_Blade; Met : Metric.Metric_Record)
                              return Blade_List is
   begin
      return Transform_Basis (BB, GA_Maths.Float_Matrix (Metric.Inv_Eigen_Matrix (Met)));
   end To_Metric_Basis;

   --  ------------------------------------------------------------------------

   function To_Metric_Basis (BL : Blade_List; Met : Metric.Metric_Record)
                              return Blade_List is
      use Blade_List_Package;
      BL_Cursor      : Cursor := BL.First;
      Tmp_List       : Blade_List;
      TL_Cursor      : Cursor;
      Result         : Blade_List;
   begin
      --        GA_Utilities.Print_Blade_List ("Blade.To_Metric_Basis BL", BL);
      while Has_Element (BL_Cursor) loop
         Tmp_List.Clear;
         --              GA_Utilities.Print_Blade ("Blade.To_Metric_Basis blade", Element (BL_Cursor));
         Tmp_List := To_Metric_Basis (Element (BL_Cursor), Met);
         --              GA_Utilities.Print_Blade_List ("Blade.To_Metric_Basis Tmp_List", Tmp_List);
         TL_Cursor := Tmp_List.First;
         while Has_Element (TL_Cursor) loop
            Result.Append (Element (TL_Cursor));
            Next (TL_Cursor);
         end loop;
         Next (BL_Cursor);
      end loop;
      Simplify (Result);
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.To_Metric_Basis");
         raise;

   end To_Metric_Basis;

   --  ------------------------------------------------------------------------
   --  Transform_Basis transforms a Basis_Blade to a new basis
   --  Based on Metric.java ArrayList transform(BasisBlade a, DoubleMatrix2D M)
   function Transform_Basis (BA  : Blade.Basis_Blade;
                             Met : GA_Maths.Float_Matrix)
                              return Blade_List is
      use Blade_List_Package;
      List_A : Blade_List;
      BM     : Unsigned_32 := Bitmap (BA);
      Curs   : Cursor;
      OP     : Basis_Blade;
      Temp   : Blade_List;
      I_Col  : Integer := 1;
      Value  : Float;
   begin
      --        New_Line;
      --        GA_Utilities.Print_Matrix ("Blade.Transform_Basis Met", Real_Matrix ((Met)));
      --        GA_Utilities.Print_Blade ("Blade.Transform_Basis BA", BA);
      --        Put_Line ("Blade.Transform_Basis Bitmap (BA)" & Unsigned_32'Image (BM));
      --  start with just a scalar
      List_A.Append (New_Blade (Weight (BA)));
      --  convert each 1 bit to a list of blades
      --          Put_Line ("Blade.Transform_Basis 1st blade added");
      while BM /= 0 loop
         --              Put_Line ("Blade.Transform_Basis BM" & Unsigned_32'Image (BM));
         if (BM and 1) /= 0 then
            Temp.Clear;
            for Row in 1 .. Met'Length (1) loop
               --                      Put_Line ("Blade.Transform_Basis Row, I_Col" &
               --                                 Integer'Image (Row) & Integer'Image (I_Col));
               Value := Met (Row, I_Col);
               if Value /= 0.0 then
                  --  Wedge column Col of the matrix with List_A
                  Curs := List_A.First;
                  while Has_Element (Curs) loop
                     --                       GA_Utilities.Print_Blade ("Blade.Transform_Basis Element (Curs)", Element (Curs));
                     OP := Outer_Product (Element (Curs),
                                          New_Basis_Blade (2 ** (Row - 1), Value));
                     Temp.Append (OP);
                     Next (Curs);
                  end loop;
               end if;
            end loop;  --  Row
            List_A := Temp;
         end if;  --  (BM and 1) /= 0
         BM := Shift_Right (BM, 1);
         I_Col := I_Col + 1;
      end loop;  --  BM /= 0
      return List_A;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Transform_Basis");
         raise;
   end Transform_Basis;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Weight : Float) is
   begin
      BB.Weight := Weight;

   exception
      when others =>
         Put_Line ("An exception occurred in Blade.Update_Blade 1");
         raise;
   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_32) is
   begin
      if Bitmap < 32 then
         BB.Bitmap := Bitmap;
      else
         raise Blade_Exception with "Blade.Update_Blade invalid Bitmap.";
      end if;

   end Update_Blade;

   --  ------------------------------------------------------------------------

   procedure Update_Blade (BB     : in out Basis_Blade; Bitmap : Unsigned_32;
                           Weight : Float) is
   begin
      if Bitmap < 32 then
         BB.Bitmap := Bitmap;
         BB.Weight := Weight;
      else
         raise Blade_Exception with "Blade.Update_Blade invalid Bitmap.";
      end if;

   end Update_Blade;

   --  ------------------------------------------------------------------------

   function Weight (BB : Basis_Blade) return Float is
   begin
      return BB.Weight;
   end Weight;

   --  ------------------------------------------------------------------------

end Blade;
