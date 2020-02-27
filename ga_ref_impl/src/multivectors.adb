--  Derived from ga_ref_impl Multivector.java

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Bits;
with GA_Utilities;

package body Multivectors is

   type Basis_Blade_Array is array (integer range <>) of Blade.Basis_Blade;

   MV_Basis_Vector_Names : Blade.Basis_Vector_Names;
   --  This array can be used to lookup the number of coordinates for
   --  the grade part of a general multivector

   procedure Compress (MV : in out Multivector);
   function Cosine_Series (MV : Multivector; Order : Integer) return Multivector;
   --      function Matrix_To_MV_Invert (Mat    : GA_Maths.Float_Matrix;
   --                                    BBs    : in out Basis_Blade_Array;
   --                                    Inv_MV : out Multivector) return Boolean;
   function Random_Vector (Dim : Integer; Scale : Float) return Multivector;
   function Sine_Series (MV : Multivector; Order : Integer) return Multivector;

   --  -------------------------------------------------------------------------

   function "+" (MV : Multivector; S : Float) return Multivector is
      MV1 : Multivector := MV;
   begin
      MV1.Blades.Append (Blade.New_Scalar_Blade (S));
      Simplify (MV1);
      return MV1;
   end  "+";

   --  ------------------------------------------------------------------------

   function  "+" (S : Float; MV : Multivector) return Multivector is
   begin
      return  MV + S;
   end  "+";

   --  ------------------------------------------------------------------------

   function "+" (MV1, MV2 : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Blades_3  : Blade_List;
      Curs      : Cursor := Blades_1.First;
      MV3       : Multivector := MV1;
   begin
      --  result.addAll(blades);
      while Has_Element (Curs) loop
         Blades_3.Append (Element (Curs));
         Next (Curs);
      end loop;
      Curs := Blades_2.First;
      --  result.addAll(x.blades);
      while Has_Element (Curs) loop
         Blades_3.Append (Element (Curs));
         Next (Curs);
      end loop;

      MV3.Blades := Blades_3;
      --  Simplify does the adding
      Simplify (MV3);
      return MV3;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors +.");
         raise;
   end "+";

   --  -------------------------------------------------------------------------

   function "-" (MV : Multivector; S : Float) return Multivector is
      MV1 : Multivector := MV;
   begin
      MV1.Blades.Append (Blade.New_Scalar_Blade (-S));
      Simplify (MV1);
      return MV1;
   end  "-";

   --  ------------------------------------------------------------------------

   function  "-" (S : Float; MV : Multivector) return Multivector is
   begin
      return  MV - S;
   end  "-";

   --  ------------------------------------------------------------------------

   --  Return negative value of a  multivector
   function "-" (MV : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Neg_MV     : Multivector := MV;
      Neg_Blades : Blade_List := Neg_MV.Blades;
      Neg_Curs   : Cursor := Neg_Blades.First;
      Blade_Neg  : Blade.Basis_Blade;
   begin
      while Has_Element (Neg_Curs) loop
         Blade_Neg := Element (Neg_Curs);
         Blade_Neg := New_Basis_Blade (Bitmap (Blade_Neg), - Weight (Blade_Neg)) ;
         Neg_Blades.Replace_Element (Neg_Curs, Blade_Neg);
         Next (Neg_Curs);
      end loop;
      Neg_MV.Blades := Neg_Blades;
      return Neg_MV;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector - 1");
         raise;
   end "-";

   --  -------------------------------------------------------------------------

   function "-" (MV1, MV2 : Multivector) return Multivector is
      Neg_MV2   : constant Multivector := -MV2;
   begin
      return MV1 + Neg_MV2;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector - 2");
         raise;
   end "-";

   --  -------------------------------------------------------------------------

   function "*" (Scale : float; MV : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades   : constant Blade_List := Get_Blade_List (MV);
      Curs     : Cursor := Blades.First;
      aBlade   : Basis_Blade;
      New_MV   : Multivector := MV;
      N_Blades : Blade_List := Get_Blade_List (New_MV);
      N_Curs   : Cursor := N_Blades.First;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Blade.Update_Blade (aBlade, Scale * Weight (aBlade));
         N_Blades.Replace_Element (N_Curs, aBlade);
         Next (Curs);
         Next (N_Curs);
      end loop;
      New_MV.Blades := N_Blades;
      return New_MV;
   end "*";

   --  ------------------------------------------------------------------------

   function "*" (MV : Multivector; Scale : float) return Multivector is
   begin
      return Scale * MV;
   end "*";

   --  ------------------------------------------------------------------------

   function "/" (MV : Multivector; Scale : float) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades   : constant Blade_List := Get_Blade_List (MV);
      Curs     : Cursor := Blades.First;
      aBlade   : Basis_Blade;
      New_MV   : Multivector := MV;
      N_Blades : Blade_List := Get_Blade_List (New_MV);
      N_Curs   : Cursor := N_Blades.First;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Blade.Update_Blade (aBlade, Weight (aBlade) / Scale);
         N_Blades.Replace_Element (N_Curs, aBlade);
         Next (Curs);
         Next (N_Curs);
      end loop;
      New_MV.Blades := N_Blades;
      return New_MV;
   end "/";

   --  ------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; aBlade : Blade.Basis_Blade) is
   begin
      MV.Blades.Append (aBlade);
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; Index : E2_Base; Value : Float) is
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index, Value));

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_Blade 1");
         raise;
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; Index : E3_Base; Value : Float) is
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index, Value));

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_Blade 2");
         raise;
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; Index : C3_Base; Value : Float) is
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index, Value));

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_Blade 2");
         raise;
   end Add_Blade;

   --  -------------------------------------------------------------------------

   --     procedure Add_Complex_Blade (MV     : in out Multivector; Index : C3_Base;
   --                                  Value : GA_Maths.Complex_Types.Complex) is
   --     begin
   --        MV.Blades.Append (Blade.New_Complex_Basis_Blade (Index, Value));
   --
   --     exception
   --        when others =>
   --           Put_Line ("An exception occurred in Multivector.Add_Blade 2");
   --           raise;
   --     end Add_Complex_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_To_Matrix (M      : in out GA_Maths.Float_Matrix;
                            BB, BG : Blade.Basis_Blade) is
      use Blade;
      Row     : Integer;
      Col     : Integer;
   begin
      Row := Bits.Highest_One_Bit (Bitmap (BG));
      Col := Bits.Highest_One_Bit (Bitmap (BB));

      --                GA_Utilities.Print_Matrix ("Multivector.Add_To_Matrix M", M);
      --                GA_Utilities.Print_Blade ("Multivector.Add_To_Matrix BB", BB);
      --                GA_Utilities.Print_Blade ("Multivector.Add_To_Matrix BG", BG);
--       Put_Line ("Multivector.Add_To_Matrix 1 Row, Col: " &
--                  Integer'Image (Row) & Integer'Image (Col));
      M (Row, Col) := M (Row, Col) + Weight (BG);

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_To_Matrix 1.");
         raise;
   end Add_To_Matrix;

   --  -------------------------------------------------------------------------

   procedure Add_To_Matrix (M  : in out GA_Maths.Float_Matrix;
                            BB : Blade.Basis_Blade; BL : Blade.Blade_List) is
      use Blade;
      use Blade_List_Package;
      Curs  : Cursor := BL.First;
   begin
      while Has_Element (Curs) loop
         Add_To_Matrix (M, BB, Element (Curs));
         Next (Curs);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_To_Matrix 2.");
         raise;
   end Add_To_Matrix;

   --  -------------------------------------------------------------------------

   procedure Add_Multivector (MV_List : in out Multivector_List; MV : Multivector)is
   begin
      MV_List.Append (MV);
   end Add_Multivector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : BV_Base) return Vector is
      MV : Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : E2_Base) return Vector is
      MV : Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : E3_Base) return Vector is
      MV : Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Basis_Vector.");
         raise;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : C3_Base) return Vector is
      MV : Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Blades (MV : Multivector) return Blade.Blade_List is
   begin
      return MV.Blades;
   end Blades;

   --  -------------------------------------------------------------------------

   function Component (MV : Multivector; BM : Interfaces.Unsigned_32) return Float is
      use Interfaces;
      use Blade;
      use Blade_List_Package;
      Blades  : constant Blade_List := Get_Blade_List (MV);
      Curs    : Cursor := Blades.First;
      Found   : Boolean := False;
      Value   : Float := 0.0;
   begin
      while Has_Element (Curs) and not Found loop
         Found := Blade.Bitmap (Element (Curs)) = BM;
         if Found then
            Value :=  Blade.Weight (Element (Curs));
         else
            Next (Curs);
         end if;
      end loop;
      return Value;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Component");
         raise;
   end Component;

   --  -------------------------------------------------------------------------

   procedure Compress (MV : in out Multivector; Epsilon : Float) is
      use Blade;
      use Blade_List_Package;
      use GA_Maths;
      Blades    : Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Max_Mag   : Float := 0.0;
   begin
      Simplify (MV);
      while Has_Element (Curs) loop
         thisBlade := Element (Curs);
         Max_Mag := Maximum (Weight (thisBlade), Max_Mag);
         Next (Curs);
      end loop;

      if Max_Mag = 0.0 then
         MV.Blades.Clear;
      else
         --  remove basis blades with too low scale
         while Has_Element (Curs) loop
            thisBlade := Element (Curs);
            if Abs (Weight (thisBlade)) < Epsilon then
               Blades.Delete (Curs);
            end if ;
            Next (Curs);
         end loop;
         MV.Blades := Blades;
      end if;

   end Compress;

   --  -------------------------------------------------------------------------

   procedure Compress (MV : in out Multivector) is
   begin
      Compress (MV, 10.0 ** (-13));
   end Compress;

   --  -------------------------------------------------------------------------

   function Cosine (MV : Multivector) return Multivector is
   begin
      return Cosine (MV, 12);
   end Cosine;

   --  -------------------------------------------------------------------------

   function Cosine (MV : Multivector; Order : Integer) return Multivector is
      use GA_Maths.Float_Functions;
      A2        : Multivector := Geometric_Product (MV, MV);
      A2_Scalar : Float;
      Alpha     : Float;
      Result    : Multivector;
   begin
      Compress (A2);
      if Is_Null (A2, 10.0 ** (-8)) then
         Result := New_Multivector (1.0);
      elsif Is_Scalar (A2) then
         A2_Scalar := Scalar_Part (A2);
         if A2_Scalar < 0.0 then
            Alpha := Sqrt (-A2_Scalar);
            Result := New_Multivector (Cosh (Alpha));
         else
            Alpha := Sqrt (A2_Scalar);
            Result := New_Multivector (Cos (Alpha));
         end if;
      else  --  Not null and not scalar
         Result :=  Cosine_Series (MV, Order);
      end if;
      return Result;
   end Cosine;

   --  -------------------------------------------------------------------------

   function Cosine_Series (MV : Multivector; Order : Integer)
                           return Multivector is
      use Interfaces;
      Scaled    : constant Multivector := MV;
      Scaled_GP : Multivector;
      Temp      : Multivector := Scaled;
      Sign      : Integer := -1;
      Result    : Multivector := New_Multivector (1.0);
   begin
      --  Taylor approximation
      for Count in 2 .. Order loop
         Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
         Temp := Geometric_Product (Temp, Scaled_GP);
         if (Unsigned_32 (Count) and 1) = 0 then
            Result := Result + Geometric_Product (Temp, Float (Sign));
            Sign := -Sign;
         end if;
      end loop;
      return Result;
   end Cosine_Series;

   --  -------------------------------------------------------------------------

   function Dot (MV1, MV2 : Multivector) return Multivector is
   begin
      return Inner_Product (MV1, MV2, Blade.Hestenes_Inner_Product);
   end Dot;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector) return Multivector is
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Space_Dimension (MV) - 1);
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (Index));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Blade.Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector; Met : Metric.Metric_Record) return Multivector is
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Space_Dimension (MV) - 1);
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (Index));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Met, Blade.Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector; Dim : Integer) return Multivector is
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Dim - 1);
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (Index));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Blade.Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------
   --  Possibly imprecise
   --     function Exp_Series (MV : Multivector; Order : Integer)
   --                           return Multivector is
   --        Scaled    : Multivector;
   --        Scaled_GP : Multivector;
   --        Temp      : Multivector := New_Multivector (1.0);
   --        Sign      : constant Integer := -1;
   --        Scale     : Integer := 1;
   --        Max       : Float := Norm_E (MV);
   --        Result    : Multivector := Temp;
   --     begin
   --        if Max > 1.0 then
   --           Scale := 2;
   --        end if;
   --        while Max > 1.0 loop
   --           Max := Max / 2.0;
   --           Scale := 2 * Scale;
   --        end loop;
   --        Scaled := Geometric_Product (MV, 1.0 / Float (Scale));
   --
   --        --  Taylor approximation
   --        for Count in 1 .. Order loop
   --           Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
   --           Temp := Geometric_Product (Temp, Scaled_GP);
   --           Result := Result + Temp;
   --        end loop;
   --
   --        --  Undo scaling
   --        while Scale > 1 loop
   --           Result := Geometric_Product (Result, Result);
   --           Scale := Scale / 2;
   --        end loop;
   --        return Result;
   --     end Exp_Series;

   --  -------------------------------------------------------------------------

   function Extract_Grade (MV : Multivector; Index : integer) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades    : constant Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Max_Grade : Integer := 0;
      Gr        : array (1 .. Index) of Integer;
      New_List  : Blade_List;
      aGrade    : Integer;
      MV_E      : Multivector;
   begin
      for k in Gr'Range loop
         Gr (k) := k;
         if Gr (k) > Max_Grade then
            Max_Grade := Gr (k);
         end if;
      end loop;

      declare
         Keep : constant array (0 .. Max_Grade + 1) of Boolean
           := (others => True);
      begin
         while Has_Element (Curs) loop
            thisBlade := Element (Curs);
            aGrade := Blade.Grade (thisBlade);
            if aGrade <= Max_Grade and then Keep (aGrade) then
               New_List.Append (thisBlade);
            end if;
            Next (Curs);
         end loop;
      end;
      MV_E.Blades := New_List;
      Simplify (MV_E);
      return MV_E;
   end Extract_Grade;

   --  -------------------------------------------------------------------------

   function From_Vector (V : Vector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades : constant Blade_List := V.Blades;
      Curs   : Cursor := Blades.First;
      MV     : Multivector;
   begin
      while Has_Element (Curs) loop
         Add_Blade (MV, Element (Curs));
         Next (Curs);
      end loop;
      return MV;
   end From_Vector;

   --  -------------------------------------------------------------------------
   --  Implements metric == null case of Multivector.java generalInverse
   function General_Inverse (MV : Multivector) return Multivector is
      use Interfaces;
      use GA_Maths.Float_Array_Package;
      use Blade;
      use Blade_List_Package;
      use GA_Maths;
      Dim        : constant Integer :=  Space_Dimension (MV);
      US_Size    : constant Unsigned_32 := Shift_Left (1, Dim);
      BB_Size    : constant Integer := Integer (US_Size);
      Max_Index  : constant Integer := BB_Size - 1;
      Mat        : Float_Matrix (0 .. Max_Index, 0 .. Max_Index) := (others => (others => 0.0));
      Mat_Inv    : Float_Matrix (0 .. Max_Index, 0 .. Max_Index) := (others => (others => 0.0));
      BBs        : Basis_Blade_Array (0 .. Max_Index);
      Curs       : Cursor := MV.Blades.First;
      aBlade     : Basis_Blade;
      Value      : Float;
      Result     : Blade_List;
   begin
      --        Put_Line ("Multivector.General_Inverse Dim" & Integer'Image (Dim));
      --        Put_Line ("Multivector.General_Inverse BB_Size" & Integer'Image (BB_Size));
      --        GA_Utilities.Print_Multivector ("Multivector.General_Inverse MV", MV);
      --  create all unit basis blades for 'Dim'
      for index in BBs'Range loop
         BBs (index) := New_Basis_Blade (Interfaces.Unsigned_32 (index));
      end loop;

      --  Construct a matrix 'Mat' such that matrix multiplication of 'Mat' with
      --  the coordinates of another multivector 'x' (stored in a vector)
      --  would result in the geometric product of 'Mat' and 'x'
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         for index in BBs'Range loop
            --              GA_Utilities.Print_Blade ("Multivector.General_Inverse GP",
--              Geometric_Product (aBlade, BBs (index)));
            Add_To_Matrix (Mat, BBs (index),
                           Geometric_Product (aBlade, BBs (index)));
         end loop;
         Next (Curs);
      end loop;

      --        GA_Utilities.Print_Matrix ("Multivector.General_Inverse Mat", Mat);
      Mat_Inv := Inverse (Mat);
      --        GA_Utilities.Print_Matrix ("Multivector.General_Inverse Mat_Inv", Mat_Inv);
      --  reconstruct multivector from first column of matrix
      --          for Row in BBs'Range loop
      for Row in Mat_Inv'Range (1) loop
         Value := Mat_Inv (Row, 1);
         if Value /= 0.0 then
            Update_Blade (BBs (Row), Value);
            Result.Append (BBs (Row));
         end if;
      end loop;
      --        GA_Utilities.Print_Multivector ("Multivector.General_Inverse Result",
      --                                        New_Multivector (Result));
      return New_Multivector (Result);

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.General_Inverse.");
         raise;
   end General_Inverse;

   --  -------------------------------------------------------------------------

   function General_Inverse (MV : Multivector;  Met : Metric.Metric_Record)
                             return Multivector is
      use Interfaces;
      use GA_Maths.Float_Array_Package;
      use Blade;
      use Blade_List_Package;
      use GA_Maths;
      Dim          : Integer;
      US_Size      : Unsigned_32;
      BB_Max_Index : Integer;
      Value        : Float;
      Blades       : Blade_List;
      Result       : Multivector;
   begin
      GA_Utilities.Print_Multivector ("Multivector.General_Inverse MV", MV);
      if Is_Null (MV) then
         Result := MV;
      else
         Dim := Space_Dimension (MV);
         US_Size := Shift_Left (1, Dim - 1);
         BB_Max_Index := Integer (US_Size) - 1;

         declare
            Mat        : Float_Matrix (0 .. BB_Max_Index, 0 .. BB_Max_Index) := (others => (others => 0.0));
            Mat_Inv    : Float_Matrix (0 .. BB_Max_Index, 0 .. BB_Max_Index) := (others => (others => 0.0));
            BBs        : Basis_Blade_Array (0 .. BB_Max_Index);
            BL_Curs    : Cursor := MV.Blades.First;
            aBlade     : Basis_Blade;
            GP_List    : Blade_List;
         begin
            --  The BBs array contains incremental bitmaps in the range 0 - 31
            --  of weight 1.0
            for index in BBs'Range loop
               BBs (index) := New_Basis_Blade (Interfaces.Unsigned_32 (index));
--                 GA_Utilities.Print_Blade ("Multivector.General_Inverse BBs (index)",
--                                           BBs (index));
            end loop;

            --  Construct a matrix 'Mat' such that matrix multiplication of 'Mat' with
            --  the coordinates of another multivector 'x' (stored in a vector)
            --  would result in the geometric product of 'Mat' and 'x'
            while Has_Element (BL_Curs) loop
               --  for each blade of BL
               aBlade := Element (BL_Curs);
               GA_Utilities.Print_Blade ("Multivector.General_Inverse aBlade", aBlade);
               GA_Utilities.Print_Bitmap ("Multivector.General_Inverse aBlade",
                                          Bitmap (aBlade));
               --  for each bitmap
               for index in BBs'Range loop
                  --                 Put_Line ("Multivector.General_Inverse Metric index: " &
                  --                             Integer'Image (index));
--                    GA_Utilities.Print_Blade ("Multivector.General_Inverse BBs (index)", BBs (index));
                  --  gp(aBlade, BBs (index), Met) corresponds to L_k L_j of equation (20.1)
                  GP_List := Geometric_Product (aBlade, BBs (index), Met);
                  if not Is_Empty (GP_List) then
--                       GA_Utilities.Print_Blade ("Multivector.General_Inverse Metric BBs (index)",
--                                                      BBs (index));
--                       GA_Utilities.Print_Blade_List ("Multivector.General_Inverse Metric GP_List",
--                                                      GP_List);
                     Add_To_Matrix (Mat, BBs (index), GP_List);
                  end if;
               end loop;
               Next (BL_Curs);
            end loop;
--              GA_Utilities.Print_Matrix ("Multivector.General_Inverse Metric Mat",
--                                         Mat, (1, 1), (6, 6));
            Mat_Inv := Inverse (Mat);
            for Row in BBs'Range loop
               Value := Mat_Inv (Row, 1);
               if Value /= 0.0 then
                  Update_Blade (BBs (Row), Value);
                  Blades.Append (BBs (Row));
               end if;
            end loop;
            Result := New_Multivector (Blades);
         end; --  declare block
      end if;
      Simplify (Result);
      GA_Utilities.Print_Multivector ("Multivector.General_Inverse Metric Result",
                                       Result);
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.General_Inverse Metric");
         raise;
   end General_Inverse;

   --  -------------------------------------------------------------------------

   function Init_Geometric_Matrix (MV : Multivector)
                                   return GA_Maths.Float_Matrix is
      use Blade;
      use GA_Maths;
      use Blade_List_Package;
      MV_List   : constant Blade_List := MV.Blades;
      MV_Curs   : Cursor;
      Dim       : constant Natural:= Space_Dimension (MV);
      Max_Index : constant Natural := Dim - 1;
      L         : Basis_Blade_Array (0 .. Max_Index);
      L_Prod    : Basis_Blade;  --  L_i
      G_Product : array (0 .. Max_Index, 0 .. Max_Index) of Basis_Blade;
      Matrix_G  : Float_Matrix (0 .. Max_Index, 0 .. Max_Index) :=
                    (others => (others => 0.0));
   begin
      for index in L'Range loop
         L (index) := New_Basis_Blade (Interfaces.Unsigned_32 (index));
      end loop;

      for k in Matrix_G'Range(1) loop
         for j in Matrix_G'Range(2) loop
            L_Prod := Geometric_Product (L(k), L(j));
            G_Product (k, j) := L_Prod;  --  L_i
         end loop;
      end loop;

      for i in Matrix_G'Range(1) loop
         for j in Matrix_G'Range(2) loop
            MV_Curs := MV_List.First;
            while Has_Element (MV_Curs) loop
               Matrix_G (i, j) := Matrix_G (i, j) + Weight (Element (MV_Curs));
               Next (MV_Curs);
            end loop;
         end loop;
      end loop;
      return Matrix_G;
   end Init_Geometric_Matrix;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades    : constant Blade_List := MV.Blades;
      Curs      : Cursor := Blades.First;
      New_MV    : Multivector;
   begin
      if Is_Empty (Blades) then
         raise MV_Exception with "Geometric_Product, MV * scalar, MV is null.";
      end if;
      if Sc /= 0.0 then
         while Has_Element (Curs) loop
            New_MV.Blades.Append (New_Basis_Blade (Bitmap (Element (Curs)),
                                  Sc * Weight (Element (Curs))));
            Next (Curs);
         end loop;
         Simplify (New_MV);
         if Is_Empty (New_MV.Blades) then
            raise MV_Exception with "Geometric_Product, scalar product MV is null.";
         end if;
      end if;
      return New_MV;
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector is
   begin
      return Geometric_Product (MV, Sc);
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Curs_1    : Cursor := Blades_1.First;
      Curs_2    : Cursor;
      Blade_1   : Blade.Basis_Blade;
      Blade_2   : Blade.Basis_Blade;
      GP        : Multivector;
   begin
      if Is_Empty (List (Blades_1)) then
         raise MV_Exception with "Multivector.Geometric_Product, MV1 is null.";
      end if;
      if Is_Empty (List (Blades_2)) then
         raise MV_Exception with "Multivector.Geometric_Product, MV2 is null.";
      end if;

      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         Curs_2 := Blades_2.First;
         while Has_Element (Curs_2) loop
            Blade_2 := Element (Curs_2);
            GP.Blades.Append (Blade.Geometric_Product (Blade_1, Blade_2));
            Next (Curs_2);
         end loop;
         Next (Curs_1);
      end loop;
      Simplify (GP);

      if Is_Empty (GP.Blades) then
         Put_Line ("Multivector.Geometric_Product, product MV is null.");
      end if;
      return GP;

   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                               return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades_1  : Blade_List;
      Blades_2  : Blade_List;
      Blades_GP : Blade_List;
      Curs_1    : Cursor;
      Curs_2    : Cursor;
      Blade_1   : Blade.Basis_Blade;
      Blade_2   : Blade.Basis_Blade;
      GP        : Multivector;
   begin
      if Is_Empty (List (MV1.Blades)) then
         raise MV_Exception with "Multivector.Geometric_Product, MV1 is null.";
      end if;
      if Is_Empty (List (MV2.Blades)) then
         raise MV_Exception with "Multivector.Geometric_Product, MV2 is null.";
      end if;

--        GA_Utilities.Print_Multivector ("Multivector.Geometric_Product with Metric, MV1",
--                                        MV1);
--        GA_Utilities.Print_Multivector ("Multivector.Geometric_Product with Metric, MV2",
--                                        MV2);

      Blades_1 := MV1.Blades;
      Blades_2 := MV2.Blades;
      Curs_1 := Blades_1.First;

      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         Curs_2 := Blades_2.First;
--           GA_Utilities.Print_Blade ("Multivector.Geometric_Product with Metric, Blade_1",
--                                      Blade_1);
         while Has_Element (Curs_2) loop
            Blade_2 := Element (Curs_2);
--              GA_Utilities.Print_Blade ("Multivector.Geometric_Product with Metric, Blade_2",
--                                        Blade_2);
            Blades_GP := Blade.Geometric_Product (Blade_1, Blade_2, Met);
--              GA_Utilities.Print_Blade_List ("Multivector.Geometric_Product with Metric, Blades_GP",
--                                              Blades_GP);
            Add_Blades (GP.Blades, Blades_GP);
            Next (Curs_2);
         end loop;
         Next (Curs_1);
      end loop;
      --        GA_Utilities.Print_Blade_List ("Multivector.Geometric_Product with Metric, GP.Blades", GP.Blades);
      Simplify (GP);

      if Is_Empty (GP.Blades) then
         Put_Line ("Multivector.Geometric_Product with Metric, product MV is null.");
      end if;
      return GP;

   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Get_Blade_List (MV : Multivector) return Blade.Blade_List is
   begin
      return MV.Blades;
   end Get_Blade_List;

   --  -------------------------------------------------------------------------

   function Get_Blade (MV : Multivector; Index : Interfaces.Unsigned_32)
                       return Blade.Basis_Blade is
      use Blade;
      use Blade_List_Package;
      Blades    : constant Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Found     : Boolean := False;
   begin
      while Has_Element (Curs) and not Found loop
         thisBlade := Element (Curs);
         Found := Blade.Grade (thisBlade) = Integer (Index);
         Next (Curs);
      end loop;
      return thisBlade;
   end Get_Blade;

   --  -------------------------------------------------------------------------

   function Get_Blade (MV    : Multivector; theBlade : out Multivector;
                       Index : Interfaces.Unsigned_32) return Boolean is
      use Interfaces;
      use Blade;
      use Blade_List_Package;
      Blades : constant Blade_List := MV.Blades;
      Curs   : Cursor := Blades.First;
      Found  : Boolean := False;
   begin
      while Has_Element (Curs) and not Found loop
         Found := Bitmap (Element (Curs)) = Index;
         if Found then
            theBlade.Blades.Append (Element (Curs));
         else
            Next (Curs);
         end if;
      end loop;

      return Found;
   end Get_Blade;

   --  -------------------------------------------------------------------------
   --  Grade returns the grade (bit count) of an homogeneous Multivector.
   --  0 is returned for null Multivectors.
   --  False is returned if the Multivector is not homogeneous.
   function Grade (MV : Multivector; theGrade : out Integer)
                   return Boolean is
      use Blade;
      use Blade_List_Package;
      Blades    : constant Blade_List := MV.Blades;
      Cursor_B  : Cursor := Blades.First;
      thisBlade : Blade.Basis_Blade;
      OK        : Boolean := not Blades.Is_Empty;
   begin
      if OK then
         thisBlade := Element (Cursor_B);   -- Blades.First
         theGrade := Blade.Grade (thisBlade);
         Next (Cursor_B);
         while OK and Has_Element (Cursor_B) loop
            thisBlade := Element (Cursor_B);
            OK := Blade.Grade (thisBlade) = theGrade;
            Next (Cursor_B);
         end loop;
      end if;

      if not OK then
         theGrade := 0;
      end if;

      return OK;
   end Grade;

   --  -------------------------------------------------------------------------

   function Grade_Inversion (MV : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Inversion     : Blade_List;
      thisBlade     : Blade.Basis_Blade;
      Cursor_B      : Cursor := Blades.First;
      Result        : Multivector;
   begin
      while Has_Element (Cursor_B) loop
         thisBlade := Element (Cursor_B);
         Inversion.Append (Blade.Grade_Inversion (thisBlade));
         Next (Cursor_B);
      end loop;
      Result := (MV.Type_Of_MV, Inversion, False);
      Simplify (Result);
      return Result;
   end Grade_Inversion;

   --  -------------------------------------------------------------------------
   --  Grade_Use returns a bitmap of grades that are in use in MV
   --  Bit 1: Scalar, Bit 2 etc non-scalar grades
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage is
      use Interfaces;
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      BB         : Blade.Basis_Blade;
      Cursor_B   : Cursor := Blades.First;
      GU_Bitmap  : Unsigned_32 := 0;
   begin
      while Has_Element (Cursor_B) loop
         BB := Element (Cursor_B);
         GU_Bitmap := GU_Bitmap or
           Shift_Left (1, Blade.Grade (BB));
         Next (Cursor_B);
      end loop;
      return GU_Bitmap;
   end Grade_Use;

   --  -------------------------------------------------------------------------

   function Inner_Product (MV1, MV2 : Multivector; Cont : Blade.Contraction_Type)
                           return Multivector is
      use Blade;
      use Blade_List_Package;
      B1       : Basis_Blade;
      B2       : Basis_Blade;
      List_1   : constant Blade_List := MV1.Blades;
      List_2   : constant Blade_List := MV2.Blades;
      Cursor_1 : Cursor := List_1.First;
      Cursor_2 : Cursor;
      IP       : Blade.Basis_Blade;
      MV       : Multivector;
   begin
      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         Cursor_2 := List_2.First;
         while Has_Element (Cursor_2) loop
            B2 := Element (Cursor_2);
            IP := Blade.Inner_Product (B1, B2, Cont);
            if Blade.Weight (IP) /= 0.0 then
               MV.Blades.Append (IP);
            end if;
            Next (Cursor_2);
         end loop;
         Next (Cursor_1);
      end loop;

      Simplify (MV);
      return MV;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Inner_Product");
         raise;
   end Inner_Product;

   --  -------------------------------------------------------------------------

   function Inner_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record;
                           Cont     : Blade.Contraction_Type) return Multivector is
      use Blade;
      use Blade_List_Package;
      B1        : Blade.Basis_Blade;
      B2        : Blade.Basis_Blade;
      List_1    : Blade_List;
      List_2    : Blade_List;
      Cursor_1  : Cursor;
      Cursor_2  : Cursor;
      Blades_IP : Blade.Basis_Blade;
      MV        : Multivector;
   begin
      List_1 := MV1.Blades;
      List_2 := MV2.Blades;
      Cursor_1 := List_1.First;

      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         Cursor_2 := List_2.First;
         while Has_Element (Cursor_2) loop
            B2 := Element (Cursor_2);
            Blades_IP := Blade.Inner_Product (B1, B2, Metric.Eigen_Values (Met), Cont);
            if Blade.Weight (Blades_IP) /= 0.0 then
               MV.Blades.Append (Blades_IP);
            end if;
            Next (Cursor_2);
         end loop;
         Next (Cursor_1);
      end loop;

      Simplify (MV);
      return MV;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors.Inner_Product metric");
         raise;
   end Inner_Product;

   --  -------------------------------------------------------------------------

   --      function Matrix_To_MV_Invert (Mat    : GA_Maths.Float_Matrix;
   --                                    BBs    : in out Basis_Blade_Array;
   --                                    Inv_MV : out Multivector) return Boolean is
   --          use GA_Maths.Float_Array_Package;
   --          Dim           : constant Integer := Mat'Last - Mat'First + 1;
   --          Inv_Mat       : GA_Maths.Float_Matrix (1 .. Dim, 1 .. Dim) := (others => (others => 0.0));
   --          Value         : Float;
   --          Blades        : Blade.Blade_List;
   --          Invertable    : constant Boolean := Determinant (Mat) /= 0.0;
   --      begin
   --          if Invertable then
   --              Inv_Mat := Inverse (Mat);
   --              for Index in Inv_Mat'Range loop
   --                  Value := Inv_Mat (Index, 1);
   --                  if Value /= 0.0 then
   --                      Blade.Update_Blade (BBs (Index), Value);
   --                      Blades.Append (BBs (Index));
   --                  end if;
   --              end loop;
   --              Inv_MV.Blades := Blades;
   --          end if;
   --          return Invertable;
   --
   --      exception
   --          when others =>
   --              Put_Line ("An exception occurred in Multivectors.Matrix_To_MV_Invert");
   --              raise;
   --      end Matrix_To_MV_Invert;

   --  -------------------------------------------------------------------------

   function Is_Null (MV : Multivector) return Boolean is
      M : Multivector := MV;
   begin
      Simplify (M);
      return M.Blades.Is_Empty;
   end Is_Null;

   --  -------------------------------------------------------------------------

   function Is_Null (MV : Multivector; Epsilon : Float) return Boolean is
      S : constant Float := Norm_Esq (MV);
   begin
      return S < Epsilon * Epsilon;
   end Is_Null;

   --  -------------------------------------------------------------------------

   function Is_Scalar (MV : Multivector) return Boolean is
      use Interfaces;
      use Ada.Containers;
      use Blade;
      use Blade_List_Package;
      Blades : constant Blade_List := MV.Blades;
      Result : Boolean := Is_Null (MV);
   begin
      if not Result and then Blades.Length = 1 then
         Result := Bitmap (Element (Blades.First)) = 0;
      end if;
      return Result;
   end Is_Scalar;

   --  -------------------------------------------------------------------------

   function Largest_Basis_Blade (MV : Multivector) return Blade.Basis_Blade is
      use Blade;
      use Blade_List_Package;
      theMV          : Multivector := MV;
      Blades         : constant Blade_List := theMV.Blades;
      Cursor_B       : Cursor := Blades.First;
      Largest_Blade  : Blade.Basis_Blade;
      Best_Scale     : Float := 0.0;
   begin
      Simplify (theMV);
      while Has_Element (Cursor_B) loop
         if Abs (Blade.Weight (Element (Cursor_B))) > Best_Scale then
            Best_Scale := Abs (Blade.Weight (Element (Cursor_B)));
            Largest_Blade := Element (Cursor_B);
         end if;
         Next (Cursor_B);
      end loop;
      return  Largest_Blade;
   end Largest_Basis_Blade;

   --  -------------------------------------------------------------------------

   function Left_Contraction (MV1, MV2 : Multivector) return Multivector is
   begin
      return  Inner_Product (MV1, MV2, Blade.Left_Contraction);
   end Left_Contraction;

   --  -------------------------------------------------------------------------

   function Left_Contraction (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                              return Multivector is
   begin
      return  Inner_Product (MV1, MV2, Met, Blade.Left_Contraction);
   end Left_Contraction;

   --  -------------------------------------------------------------------------

   function List_Length (MV_List : Multivector_List) return Integer is
   begin
      return  Integer (MV_List.Length);
   end List_Length;

   --  -------------------------------------------------------------------------

   function Multivector_String (MV : Multivector; BV_Names : Blade.Basis_Vector_Names)
                                return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use Blade;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Blade_Cursor  : Cursor := Blades.First;
      thisBlade     : Blade.Basis_Blade;
      Blade_UBS     : Ada.Strings.Unbounded.Unbounded_String;
      theString     : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("0.0");
   begin
      while Has_Element (Blade_Cursor) loop
         thisBlade := Element (Blade_Cursor);
         Blade_UBS := Blade.Blade_String (thisBlade, BV_Names);
         if Length (Blade_UBS) > 0 then
            declare
               Blade_String : constant String := To_String (Blade_UBS);
            begin
--                 New_Line;
               if Blade_Cursor = Blades.First then
                  theString := To_Unbounded_String (Blade_String);
               else
                  if Blade_String (1) = '-' then
                     theString := theString & " - ";
                  else
                     theString := theString & " + ";
                  end if;
                  theString := theString & Blade_String (2 .. Blade_String'Length);
               end if;
            end;
         end if;
         Next (Blade_Cursor);
      end loop;

      return theString;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Multivectors.Multivector_String.");
         Put_Line (Exception_Information (anError));
         raise;
   end Multivector_String;

   --  -------------------------------------------------------------------------

   function MV_Kind (MV : Multivector) return MV_Type is
   begin
      return MV.Type_Of_MV;
   end MV_Kind;

   --  -------------------------------------------------------------------------

   function MV_First (MV_List : Multivector_List) return Multivector is
   begin
      return MV_List.First_Element;
   end MV_First;

   --  -------------------------------------------------------------------------

   function MV_Item (MV_List : Multivector_List; Index : Integer) return Multivector is
      use MV_List_Package;
      Curs : Cursor := MV_List.First;
   begin
      if Index > 1 then
         for count in 2 .. Index loop
            Next (Curs);
         end loop;
      end if;
      return Element (Curs);
   end MV_Item;

   --  -------------------------------------------------------------------------

   function Negate (MV : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      Curs       : Cursor := Blades.First;
      aBlade     : Blade.Basis_Blade;
      Negated_MV : Multivector;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Blade.Update_Blade (aBlade, -Weight (aBlade));
         Negated_MV.Blades.Append (aBlade);
         Next (Curs);
      end loop;
      return Negated_MV;
   end Negate;

   --  -----------------------------------------------------------

   function New_Bivector (V1, V2 : Vector) return Bivector is
   begin
      return Bivector (Outer_Product (V1, V2));
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Bivector (e1e2, e2e3, e3e1 : Float) return Bivector is
      use Blade;
      BV : Bivector;
   begin
      BV.Type_Of_MV := MV_Bivector;
      BV.Blades.Append (New_Basis_Blade (BV_e1e2, e1e2));
      BV.Blades.Append (New_Basis_Blade (BV_e2e3, e2e3));
      BV.Blades.Append (New_Basis_Blade (BV_e3e1, e3e1));
      Simplify (BV);
      return BV;
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Circle return Circle is
      C : Circle;
   begin
      C.Type_Of_MV := MV_Circle;
      return C;
   end New_Circle;

   --  -------------------------------------------------------------------------

   function New_Dual_Plane return Dual_Plane is
      DP : Dual_Plane;
   begin
      DP.Type_Of_MV := MV_Dual_Plane;
      return DP;
   end New_Dual_Plane;

   --  -------------------------------------------------------------------------

   function New_MV_Line return Line is
      L : Line;
   begin
      L.Type_Of_MV := MV_Line;
      return L;
   end New_MV_Line;

   --  -------------------------------------------------------------------------

   function New_Multivector (Scalar_Weight : Float) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (Blade.New_Scalar_Blade (Scalar_Weight));
      return  MV;
   end New_Multivector;

   --  -------------------------------------------------------------------------

   function New_Multivector (aBlade : Blade.Basis_Blade) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (aBlade);
      return  MV;
   end New_Multivector;

   --  -------------------------------------------------------------------------

   function New_Multivector (Blades : Blade.Blade_List) return Multivector is
      MV : Multivector;
   begin
      MV.Blades := Blades;
      return  MV;
   end New_Multivector;

   --  -------------------------------------------------------------------------

   function New_Normalized_Point return Normalized_Point is
      NP : Normalized_Point;
   begin
      NP.Type_Of_MV := MV_Normalized_Point;
      return NP;
   end New_Normalized_Point;

   --  -------------------------------------------------------------------------

   function New_Rotor return Rotor is
   begin
      return  New_Rotor (1.0);
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float) return Rotor is
      use Blade;
      R : Rotor;
   begin
      R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (Blade.New_Scalar_Blade (Scalar_Weight));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float; BV : Bivector) return Rotor is
      use Blade;
      R : Rotor;
   begin
      R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e1e2)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e2e3)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e3e1)));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight, e1, e2, e3 : Float) return Rotor is
      use Blade;
      R : Rotor;
   begin
      R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (New_Basis_Blade (E3_e1, e1));
      R.Blades.Append (New_Basis_Blade (E3_e2, e2));
      R.Blades.Append (New_Basis_Blade (E3_e3, e3));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Vector return Vector is
      V : Vector;
   begin
      V.Type_Of_MV := MV_Vector;
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function New_Vector (e1, e2 : Float) return Vector is
      use Blade;
      V : Vector;
   begin
      V.Type_Of_MV := MV_Vector;
      Add_Blade (V, New_Basis_Blade (E2_e1, e1));
      Add_Blade (V, New_Basis_Blade (E2_e2, e2));
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function New_Vector (e1, e2, e3 : Float) return Vector is
      use Blade;
      V : Vector;
   begin
      V.Type_Of_MV := MV_Vector;
      Add_Blade (V, New_Basis_Blade (E3_e1, e1));
      Add_Blade (V, New_Basis_Blade (E3_e2, e2));
      Add_Blade (V, New_Basis_Blade (E3_e3, e3));
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function Norm_E (MV : Multivector) return Float is
      use GA_Maths.Float_Functions;
   begin
      return Sqrt (Norm_Esq (MV));
   end Norm_E;

   --  -------------------------------------------------------------------------

   function Norm_E (MV : Multivector; Met : Metric.Metric_Record) return Float is
      use GA_Maths.Float_Functions;
   begin
      return Sqrt (Norm_Esq (MV, Met));
   end Norm_E;

   --  -------------------------------------------------------------------------
   --  Based on Geometric Algebra for Computer Science section 3.1.3, eq (3.4)
   --  and ga_ref_impl.Multivector.java norm_e2()
   function Norm_Esq (MV : Multivector) return Float is
      S : Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if S < 0.0 then
         S := 0.0;
      end if;
      return S;
   end Norm_Esq;

   --  -------------------------------------------------------------------------

   function Norm_Esq (MV : Multivector; Met : Metric.Metric_Record) return Float is
      S : Float := Scalar_Product (MV, Reverse_MV (MV), Met);
   begin
      if S < 0.0 then
         S := 0.0;
      end if;
      return S;
   end Norm_Esq;

   --  -------------------------------------------------------------------------

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;

      function Product (List_1, List_2 : Blade_List) return Blade_List is
         Cursor_1  : Cursor := List_1.First;
         Cursor_2  : Cursor;
         Blade_OP  : Blade.Basis_Blade;
         B1        : Blade.Basis_Blade;
         B2        : Blade.Basis_Blade;
         Result    : Blade_List;
      begin
         --  For each element of List_1
         --     For  each element of List_1
         --        Calculate the outer product of these two elements
         --        Append the resultant element to the Result list.
         while Has_Element (Cursor_1) loop
            B1 := Element (Cursor_1);
            Cursor_2 := List_2.First;
            while Has_Element (Cursor_2) loop
               B2 := Element (Cursor_2);
               Blade_OP := Outer_Product (B1, B2);
               Result.Append (Blade_OP);
               Next (Cursor_2);
            end loop;
            Next (Cursor_1);
         end loop;

         return Result;
      end Product;

   begin
      if MV1.Type_Of_MV = MV_Vector and then MV1.Type_Of_MV = MV_Vector then
         declare
            OP : Bivector;
         begin
            OP.Blades := Product (MV1.Blades, MV2.Blades);
            Simplify (OP);
            return OP;
         end;
      else
         declare
            OP : Multivector;
         begin
            OP.Blades := Product (MV1.Blades, MV2.Blades);
            Simplify (OP);
            return OP;
         end;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors.Outer_Product.");
         raise;
   end Outer_Product;

   --  -------------------------------------------------------------------------

   function Random_Blade (Dim, Grade : Integer; Scale : Float) return Multivector is
      Result : Multivector :=
                 New_Multivector (2.0 * Scale * Float (Maths.Random_Float) - 0.5);
   begin
      For index in 1 .. Grade loop
         Result := Outer_Product (Result, Random_Vector (Dim, Scale));
      end loop;
      return  Result;
   end Random_Blade;

   --  -------------------------------------------------------------------------

   function Random_Vector (Dim : Integer; Scale : Float) return Multivector is
      Result : Multivector :=
                 New_Multivector (2.0 * Scale * Float (Maths.Random_Float) - 0.5);
      use Interfaces;
      use Blade;
      Bases  : Blade_List;
      aBlade : Basis_Blade;
      Bit    : Unsigned_32;
   begin
      For index in Unsigned_32 range  0 .. Unsigned_32 (Dim - 1) loop
         Bit := Shift_Left (1, Natural (index));
         aBlade := New_Basis_Blade (C3_Base'Enum_Val (Integer (Bit)),
                                    2.0 * Scale * Float (Maths.Random_Float) - 0.5);
         Bases.Append (aBlade);
      end loop;
      Result.Blades := Bases;
      return  Result;
   end Random_Vector;

   --  -------------------------------------------------------------------------
   function Reverse_MV (MV : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      B_Cursor   : Cursor := Blades.First;
      Rev_Blades : Blade_List;
      Rev_MV     : Multivector;
   begin
      while Has_Element (B_Cursor) loop
         Rev_Blades.Append (Blade.Reverse_Blade (Element (B_Cursor)));
         Next (B_Cursor);
      end loop;
      Rev_MV.Blades := Rev_Blades;
      return Rev_MV;
   end Reverse_MV;

   --  -------------------------------------------------------------------------

   function Right_Contraction (MV1, MV2 : Multivector) return Multivector is
   begin
      return  Inner_Product (MV1, MV2, Blade.Right_Contraction);
   end Right_Contraction;

   --  -------------------------------------------------------------------------

   --      function Rotor_Inverse (R : Rotor; IR : out Rotor) return Boolean is
   --          use Blade;
   --          use Blade_List_Package;
   --          MV         : Multivector;
   --          Blades     : constant Blade_List := MV.Blades;
   --          Curs       : Cursor := Blades.First;
   --          Invertible : constant Boolean := General_Inverse (R, MV);
   --      begin
   --          if Invertible then
   --              while Has_Element (Curs) loop
   --                  Add_Blade (IR, Element (Curs));
   --                  Next (Curs);
   --              end loop;
   --          end if;
   --          return Invertible;
   --      end Rotor_Inverse;

   --  -------------------------------------------------------------------------

   function Scalar_Part (MV : Multivector) return Float is
      use Interfaces;
      use Blade;
      use Blade_List_Package;
      BB       : Blade.Basis_Blade;
      Blades   : constant Blade_List := MV.Blades;
      B_Cursor : Cursor := Blades.First;
      Sum      : Float := 0.0;
   begin
      while Has_Element (B_Cursor) loop
         BB := Element (B_Cursor);
         if Bitmap (BB) = 0 then
            Sum := Sum + Weight (BB);
         end if;
         Next (B_Cursor);
      end loop;
      return Sum;
   end Scalar_Part;

   --  -------------------------------------------------------------------------

   function Scalar_Product (MV1, MV2 : Multivector) return float is
   begin
      return Scalar_Part (Inner_Product (MV1, MV2, Blade.Left_Contraction));
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   function Scalar_Product (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                            return float is
   begin
      return Scalar_Part (Inner_Product (MV1, MV2, Met, Blade.Left_Contraction));
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   procedure Simplify (MV : in out Multivector) is
      Blades : Blade.Blade_List := MV.Blades;
   begin
      Blade.Simplify (Blades);
      MV.Blades := Blades;
   end Simplify;

   --  -------------------------------------------------------------------------

   function Sine (MV : Multivector) return Multivector is
   begin
      return Sine (MV, 12);
   end Sine;

   --  -------------------------------------------------------------------------

   function Sine (MV : Multivector; Order : Integer) return Multivector is
      use GA_Maths.Float_Functions;
      A2        : Multivector := Geometric_Product (MV, MV);
      A2_Scalar : Float;
      Alpha     : Float;
      Result    : Multivector;
   begin
      Compress (A2);
      if Is_Null (A2, 10.0 ** (-8)) then
         Result := MV;
      elsif Is_Scalar (A2) then
         A2_Scalar := Scalar_Part (A2);
         if A2_Scalar < 0.0 then
            Alpha := Sqrt (-A2_Scalar);
            Result := Geometric_Product (MV, Sinh (Alpha) / Alpha);
         else
            Alpha := Sqrt (A2_Scalar);
            Result := Geometric_Product (MV, Sin (Alpha) / Alpha);
         end if;
      else  --  Not null and not scalar
         Result :=  Sine_Series (MV, Order);
      end if;
      return Result;
   end Sine;

   --  -------------------------------------------------------------------------

   function Sine_Series (MV : Multivector; Order : Integer) return Multivector is
      use Interfaces;
      Scaled    : constant Multivector := MV;
      Scaled_GP : Multivector;
      Temp      : Multivector := Scaled;
      Sign      : Integer := -1;
      Result    : Multivector := Scaled;
   begin
      --  Taylor approximation
      for Count in 2 .. Order loop
         Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
         Temp := Geometric_Product (Temp, Scaled_GP);
         if (Unsigned_32 (Count) and 1) /= 0 then
            --  use only the odd part of the series
            Result := Result + Geometric_Product (Temp, Float (Sign));
            Sign := -Sign;
         end if;
      end loop;
      return Result;
   end Sine_Series;

   --  -------------------------------------------------------------------------

   function MV_Size (MV : Multivector) return Natural is
      Blades  : constant Blade.Blade_List := MV.Blades;
   begin
      return Natural (Blades.Length);
   end MV_Size;

   --  -------------------------------------------------------------------------

   function Space_Dimension (MV : Multivector) return Natural is
      use GA_Maths;
      use Blade;
      use Blade_List_Package;
      Blades       : constant Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      BM           : Interfaces.Unsigned_32;
      Max_Dim      : Natural := 0;
   begin
      while Has_Element (Blade_Cursor) loop
         BM := Bitmap (Element (Blade_Cursor));
         Max_Dim := Maximum (Max_Dim, (Bits.Highest_One_Bit (BM)));
         Next (Blade_Cursor);
      end loop;
      return Max_Dim + 1;
   end Space_Dimension;

   --  -------------------------------------------------------------------------

   function Top_Grade_Index (MV : Multivector) return Interfaces.Unsigned_32 is
      use Blade;
      use Blade_List_Package;
      use GA_Maths;
      Max_Grade_Count : Integer := 0;
      Grade_Count     : Integer := 0;
      Blades          : constant Blade_List := MV.Blades;
      Blade_Cursor    : Cursor := Blades.First;
   begin
      while Has_Element (Blade_Cursor) loop
         --  Grade_Count = bit count
         Grade_Count := Blade.Grade (Element (Blade_Cursor));
         Max_Grade_Count := Maximum (Max_Grade_Count, Grade_Count);
         Next (Blade_Cursor);
      end loop;
      --          Put_Line ("Multivectors.Top_Grade_Index Max Grade Count:" & Integer'Image (Max_Grade_Count));
      return Interfaces.Unsigned_32 (Grade_Count);
   end Top_Grade_Index;

   --  -------------------------------------------------------------------------

   procedure To_Circle (MV : in out Multivector) is
   begin
      MV.Type_Of_MV := MV_Circle;
   end To_Circle;

   --  -------------------------------------------------------------------------

   procedure To_Dual_Plane (MV : in out Multivector) is
   begin
      MV.Type_Of_MV := MV_Dual_Plane;
   end To_Dual_Plane;

   --  -------------------------------------------------------------------------

   procedure To_Line (MV : in out Multivector) is
   begin
      MV.Type_Of_MV := MV_Line;
   end To_Line;

   --  -------------------------------------------------------------------------

   function To_Rotor (MV : Multivector) return Rotor is
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      Curs       : Cursor := Blades.First;
      Rot        : Rotor;
   begin
      while Has_Element (Curs) loop
         Add_Blade (Rot, Element (Curs));
         Next (Curs);
      end loop;
      return Rot;
   end To_Rotor;

   --  -------------------------------------------------------------------------

   function To_Vector (MV : Multivector) return Vector is
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      Curs       : Cursor := Blades.First;
      Vec        : Vector;
   begin
      while Has_Element (Curs) loop
         Add_Blade (Vec, Element (Curs));
         Next (Curs);
      end loop;
      return Vec;
   end To_Vector;

   --  -------------------------------------------------------------------------

   function Unit_E (MV : Multivector) return Multivector is
   begin
      return Unit_R (MV);

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors.Unit_E.");
         raise;
   end Unit_E;

   --  -------------------------------------------------------------------------

   function Unit_R (MV : Multivector) return Multivector is
      use GA_Maths.Float_Functions;
      theNorm  : constant Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if theNorm = 0.0 then
         raise MV_Exception with "Multivectors.Unit_R encountered a null multivector";
      end if;

      return Geometric_Product (MV, 1.0 / Sqrt (Abs (theNorm)));

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors.Unit_R.");
         raise;
   end Unit_R;

   --  -------------------------------------------------------------------------

   function Unit_R (MV : Multivector; Met : Metric.Metric_Record) return Multivector is
      use GA_Maths.Float_Functions;
      theNorm  : constant Float := Scalar_Product (MV, Reverse_MV (MV), Met);
   begin
      GA_Utilities.Print_Multivector ("Multivectors.Unit_R Metric MV", MV);
      if theNorm = 0.0 then
         raise MV_Exception with "Multivectors.Unit_R Metric encountered a null multivector";
      end if;

      return Geometric_Product (MV, 1.0 / Sqrt (Abs (theNorm)));

   exception
      when others =>
         Put_Line ("An exception occurred in Multivectors.Unit_R Metric.");
         raise;
   end Unit_R;

   --  -------------------------------------------------------------------------

   procedure Update (MV     : in out Multivector; Blades : Blade.Blade_List;
                     Sorted : Boolean := False) is
   begin
      MV.Blades := Blades;
      MV.Sorted := Sorted;
   end Update;

   --  -------------------------------------------------------------------------

   procedure Update_Scalar_Part (MV : in out Multivector; Value : Float) is
      Blades    : constant Blade.Blade_List := Get_Blade_List (MV);
   begin
      MV.Blades.Replace_Element (Blades.First, Blade.New_Scalar_Blade (Value));
   end Update_Scalar_Part;

   --  -------------------------------------------------------------------------
   --  Geometric ALgebra for Computer Scientists, Section 21.1
   function Versor_Inverse (MV : Multivector) return Multivector is
      Rev          : constant Multivector := Reverse_MV (MV);
      S_Product    : constant Float := Scalar_Product (MV, Rev);
   begin
      if S_Product = 0.0 then
         Put_Line ("Multivector.Versor_Inverse encountered a non-invertible multivector");
         raise MV_Exception;
      end if;

      return Rev / S_Product;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Versor_Inverse.");
         raise;
   end Versor_Inverse;

   --  -------------------------------------------------------------------------

   function Versor_Inverse (MV : Multivector; Met : Metric.Metric_Record)
                            return Multivector is
      Rev          : constant Multivector := Reverse_MV (MV);
      S_Product    : constant Float := Scalar_Product (MV, Rev, Met);
   begin
      if S_Product = 0.0 then
         Put_Line ("Multivector.Versor_Inverse encountered a non-invertible multivector");
         raise MV_Exception;
      end if;

      return Rev / S_Product;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Versor_Inverse.");
         raise;
   end Versor_Inverse;

   --  -------------------------------------------------------------------------
begin
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

end Multivectors;
