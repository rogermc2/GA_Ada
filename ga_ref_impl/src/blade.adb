
with Ada.Text_IO; use Ada.Text_IO;

with Bits;

package body Blade is

    function GP_OP (BA, BB : Basis_Blade; Outer : Boolean) return Basis_Blade;
    function Inner_Product_Filter (Grade_1, Grade_2 : Integer;
                                   BB : Basis_Blade; Cont : Contraction_Type)
                                   return Basis_Blade;

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
                         Index : Natural; BB : Basis_Blade)is
    begin
        Blades.Replace_Element (Index, BB);
    end Add_Blade;

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
        Index     : Natural := 1;
        Scale     : constant GA_Maths.float_3 := GA_Maths.float_3 (Weight (aBlade));
        Name      : Unbounded_String;
        Val       : Unbounded_String;
        theString : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
    begin
        if BM = 0 then
            theString := To_Unbounded_String (GA_Maths.float_3'Image (Scale));
        else
            while BM /= 0 loop
                --           Put_Line ("Blade, BM: " & Unsigned_32'Image (BM));
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
    --  Based on BasisBlade.java geometricProduct(BasisBlade a, BasisBlade b, double[] m)
    --  wher m is an array of doubles giving the metric for each basis vector.
    function Geometric_Product (BA, BB : Basis_Blade;
                                Met : Metric.Metric_Matrix) return Basis_Blade is
        Result : Basis_Blade := Geometric_Product (BA, BB); --  Euclidean metric
        --  BM is the meet (bitmap of annihilated vectors)
        --  Only retain vectors commomt to both blades
        BM     : Unsigned_32 := Bitmap (BA) and Bitmap (BB);
        Row    : Integer range 1 .. Met'Length (1) := 1;
        Col    : Integer range 1 .. Met'Length (2) := 1;
    begin
        while BM /= 0 loop
            if (BM and 1) /= 0 then
                --  This basis vector is non-zero
                Result.Weight := Result.Weight * Met (Row, Col);
            end if;
            if Col = 5 then
                Row := Row + 1;
                Col := 1;
            else
                Col := Col + 1;
            end if;
            --  Move rigth to next basis vector indicator
            BM := BM / 2;  --  shift right
        end loop;
        return Result;

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
            OP_Blade  := New_Basis_Blade (0, 0.0);  --  return zero blade
        else  --  compute geometric product
            --  if BA.Bitmap = BB.Bitmap, xor = 0, so Dot product part of MV
            --  else xor > 0 so Outer product part of MV
            Sign := Canonical_Reordering_Sign (BA.Bitmap, BB.Bitmap);
            OP_Blade := New_Basis_Blade
              (BA.Bitmap xor BB.Bitmap, Sign * BA.Weight * BB.Weight);
        end if;

        return OP_Blade;
    end GP_OP;

    --  ------------------------------------------------------------------------

    function Grade (BB : Basis_Blade) return Integer is
    begin
        return  Bits.Bit_Count (BB.Bitmap);
    end Grade;

    --  ------------------------------------------------------------------------

    function Grade_Inversion (B : Basis_Blade) return Basis_Blade is
        W : constant float
          := Float (Minus_1_Power (Grade (B)) * Integer (B.Weight));
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

    function Inner_Product (BA, BB : Basis_Blade; Met : Metric.Metric_Matrix;
                            Cont : Contraction_Type) return Basis_Blade is
        GM : constant Basis_Blade := Geometric_Product (BA, BB, Met);
        IP : constant Basis_Blade := Inner_Product_Filter (Grade (BA), Grade (BB),
                                                           Geometric_Product (BA, BB, Met), Cont);
    begin
        if Weight (GM) /= 0.0 then
            Put_Line ("Blade.Inner_Product Geometric_Product " &
                        Unsigned_32'Image (Bitmap (GM)) & "  " &
                        Float'Image (Weight (GM)));
            Put_Line ("Blade.Inner_Product filtered Inner_Product " &
                        Unsigned_32'Image (Bitmap (IP)) & "  " &
                        Float'Image (Weight (IP)));
        end if;
        return Inner_Product_Filter (Grade (BA), Grade (BB),
                                     Geometric_Product (BA, BB, Met), Cont);
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

    function New_Basis_Blade (Bitmap : Unsigned_32; Weight : Float := 1.0)
                              return Basis_Blade is
        Blade : Basis_Blade;
    begin
        Blade.Bitmap := Bitmap;
        Blade.Weight := Weight;
        return Blade;
    end New_Basis_Blade;

    --  ------------------------------------------------------------------------

    function New_Basis_Blade (Weight : Float) return Basis_Blade is
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

    procedure Print_Blade (Name : String; B : Basis_Blade) is
    begin
        New_Line;
        Put_Line (Name & " Bitmap and Weight");
        Put_Line (Interfaces.Unsigned_32'Image (Bitmap (B)) &
                    "  " & float'Image (Weight (B)));
    end Print_Blade;

    --  ------------------------------------------------------------------------

    function Reverse_Blade (B : Basis_Blade) return Basis_Blade is
        G   : constant Integer := Grade (B); -- Bit_Count (B.Bitmap)
        W   : constant float
          := Float (Minus_1_Power (G * (G - 1) / 2)) * B.Weight;
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
            Blade_Sort_Package.Sort (List (Blades));
            Blade_Cursor := Blades.First;
            Current_Blade := Element (Blade_Cursor);
            Next (Blade_Cursor);

            while Has_Element (Blade_Cursor) loop
                Blade_B := Element (Blade_Cursor);
                if Bitmap (Blade_B) = Bitmap (Current_Blade) then
                    Current_Blade := New_Basis_Blade (Bitmap (Current_Blade),
                                                      Weight (Current_Blade) + Weight (Blade_B));
                else
                    if Weight (Current_Blade) /= 0.0 then
                        Result.Append (Current_Blade);
                    end if;
                    Current_Blade := Blade_B;
                end if;
                Next (Blade_Cursor);
            end loop;

            if Weight (Current_Blade) /= 0.0 then
                Result.Append (Current_Blade);
            end if;
            Blades := Result;
        end if;
    end Simplify;

   --  -------------------------------------------------------------------------

   function Transform_Basis (BB      : Blade.Basis_Blade;
                             aMatrix : GA_Maths.Float_Matrix)
                             return Blade.Blade_List is
      use Blade_List_Package;
      BM     : Unsigned_32 := Bitmap (BB);
      Curs   : Cursor;
      Temp   : Blade.Blade_List;
      Col    : Integer := 1;
      Value  : Float;
      Result : Blade.Blade_List;
   begin
      --  start with just scalar
      Result.Append (New_Basis_Blade (Weight (BB)));
      --  for each 1 bit: convert to list of blades
      while (BM and 1) /= 0 loop
         for Row in aMatrix'Range(1) loop
            Value := aMatrix (Row, Col);
            if Value /= 0.0 then
               Curs := Result.First;
               while Has_Element (Curs) loop
                  Temp.Append (Outer_Product (Element (Curs),
                               New_Basis_Blade (Shift_Left (1, Row), Value)));
                  Next (Curs);
               end loop;
            end if;
         end loop;
         BM := BM / 2;  --  Shift BM right by one bit
         Col := Col + 1;
      end loop;

      return Result;
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
        BB.Bitmap := Bitmap;
    exception
        when others =>
            Put_Line ("An exception occurred in Blade.Update_Blade 2");
            raise;
    end Update_Blade;

    --  ------------------------------------------------------------------------

    procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_32;
                            Weight : Float) is
    begin
        BB.Bitmap := Bitmap;
        BB.Weight := Weight;
    exception
        when others =>
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
