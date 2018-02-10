--  Derived from ga_ref_impl Multivector.java

with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with GA_Utilities;

package body Multivector is

   type Basis_Blade_Array is array (integer range <>) of Blade.Basis_Blade;

   function "<" (Left, Right : Blade.Basis_Blade) return Boolean;

   package Blade_Sort_Package is new
     Blade_List_Package.Generic_Sorting ("<");

   --     Basis : array (1 .. 5, 1 ..5) of float :=
   --       ((0.0, 0.0, 0.0, 0.0, -1.0),
   --        (0.0, 1.0, 0.0, 0.0, 0.0),
   --        (0.0, 0.0, 1.0, 0.0, 0.0),
   --        (0.0, 0.0, 0.0 ,1.0, 0.0),
   --        (-1.0, 0.0, 0.0 , 0.0, 0.0));

   MV_Basis_Vector_Names : Blade.Basis_Vector_Names;
   --  This array can be used to lookup the number of coordinates for
   --  the grade part of a general multivector

   procedure Compress (MV : in out Multivector);
   procedure Compress (MV : in out Multivector; Epsilon : Float);
   function Cosine_Series (MV : Multivector; Order : Integer) return Multivector;
   function Matrix_To_MV_Invert (Mat : GA_Maths.Float_Matrix;
                                 BBs : in out Basis_Blade_Array) return Multivector;
   procedure Simplify (Blades : in out Blade_List; Sorted : out Boolean);
   function Sine_Series (MV : Multivector; Order : Integer) return Multivector;
   function Space_Dimension (MV : Multivector) return Integer;

   --  -------------------------------------------------------------------------

   function "<" (Left, Right : Blade.Basis_Blade) return Boolean is
      use GA_Maths;
   begin
      return Bitmap (Left) < Bitmap (Right);
   end "<";

   --  -------------------------------------------------------------------------

   function "+" (MV : Multivector; S : Float) return Multivector is
      MV1 : Multivector := MV;
   begin
      MV1.Blades.Append (New_Scalar_Blade (S));
      Simplify (MV1);
      return MV1;
   end  "+";

   --  ------------------------------------------------------------------------

   function  "+" (S :Float; MV : Multivector) return Multivector is
   begin
      return  MV + S;
   end  "+";

   --  ------------------------------------------------------------------------

   function "-" (MV : Multivector; S : Float) return Multivector is
      MV1 : Multivector := MV;
   begin
      MV1.Blades.Append (New_Scalar_Blade (-S));
      Simplify (MV1);
      return MV1;
   end  "-";

   --  ------------------------------------------------------------------------

   function  "-" (S :Float; MV : Multivector) return Multivector is
   begin
      return  MV - S;
   end  "-";

   --  ------------------------------------------------------------------------

   function "+" (MV1, MV2 : Multivector) return Multivector is
      use Blade_List_Package;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Blades_3  : Blade_List;
      Curs      : Cursor := Blades_1.First;
      MV3       : Multivector;
   begin
      while Has_Element (Curs) loop
         Blades_3.Append (Element (Curs));
         Next (Curs);
      end loop;
      Curs := Blades_2.First;
      while Has_Element (Curs) loop
         Blades_3.Append (Element (Curs));
         Next (Curs);
      end loop;
      MV3.Blades := Blades_3;
      --  Simplify should do the adding?
      Simplify (MV3);
      return MV3;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.+");
         raise;
   end "+";

   --  -------------------------------------------------------------------------
   --  Return negative value of a  multivector
   function "-" (MV : Multivector) return Multivector is
      use Blade_List_Package;
      MV_Blades  : constant Blade_List := MV.Blades;
      MV_Curs    : Cursor := MV_Blades.First;
      Neg_MV     : Multivector := MV;
      Neg_Blades : Blade_List := Neg_MV.Blades;
      Neg_Curs   : Cursor := Neg_Blades.First;
      Blade_MV   : Blade.Basis_Blade;
      Blade_Neg  : Blade.Basis_Blade;
   begin
      while Has_Element (MV_Curs) and Has_Element (Neg_Curs) loop
         Blade_MV := Element (MV_Curs);
         Blade_Neg := Blade_MV;
         Blade.Update_Blade (Blade_Neg, - Weight (Blade_MV));
         Neg_Blades.Replace_Element (Neg_Curs, Blade_Neg);
         Next (MV_Curs);
         Next (Neg_Curs);
      end loop;
      Neg_MV.Blades := Neg_Blades;
      return Neg_MV;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.-");
         raise;
   end "-";

   --  -------------------------------------------------------------------------

   function "-" (MV1, MV2 : Multivector) return Multivector is
      Neg_MV2   : Multivector := -MV2;
   begin
      return MV1 + Neg_MV2;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.- 2");
         raise;
   end "-";

   --  -------------------------------------------------------------------------

   function "*" (Scale : float; MV : Multivector) return Multivector is
      use Blade_List_Package;
      Blades   : Blade_List := Get_Blade_List (MV);
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
      use Blade_List_Package;
      Blades   : Blade_List := Get_Blade_List (MV);
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
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; Index : E3_Base; Value : Float) is
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index, Value));
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; Index : C3_Base; Value : Float) is
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index, Value));
   end Add_Blade;

   --  -------------------------------------------------------------------------

   procedure Add_To_Matrix (M : in out GA_Maths.Float_Matrix;
                            BB, GP : Blade.Basis_Blade) is
      use Blade;
      BMB  : constant Integer := Integer (Bitmap (BB)) + 1;
      BMGP : constant Integer := Integer (Bitmap (GP)) + 1;
   begin
      M (BMGP, BMB) :=  M (BMGP, BMB) + Weight (GP);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Add_To_Matrix 1");
         Put_Line ("BMB, BMGP: " & Integer'Image (BMB) &  Integer'Image (BMGP));
         raise;
   end Add_To_Matrix;

   --  -------------------------------------------------------------------------

   procedure Add_To_Matrix (M : in out GA_Maths.Float_Matrix; BB : Blade.Basis_Blade;
                            Blades  : Blade_List) is
      use Blade_List_Package;
      Curs    : Cursor := Blades.First;
   begin
      while Has_Element (Curs) loop
         Add_To_Matrix (M, BB, Element (Curs));
         Next (Curs);
      end loop;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Add_To_Matrix 2");
         raise;
   end Add_To_Matrix;

   --  -------------------------------------------------------------------------

   function Blades (MV : Multivector) return Blade_List is
   begin
      return MV.Blades;
   end Blades;

   --  -------------------------------------------------------------------------

   function Component (MV : Multivector; BM : GA_Maths.Unsigned_Integer;
                       Value : out Float) return Boolean is
      use Blade_List_Package;
      use GA_Maths;
      Blades  : constant Blade_List := Get_Blade_List (MV);
      Curs    : Cursor := Blades.First;
      Found   : Boolean := False;
   begin
      Value := 0.0;
      while Has_Element (Curs) and not Found loop
         Found := Blade.Bitmap (Element (Curs)) = BM;
         if found then
            Value :=  Blade.Weight (Element (Curs));
         else
            Next (Curs);
         end if;
      end loop;
      return Found;
   end Component;

   --  -------------------------------------------------------------------------

   procedure Compress (MV : in out Multivector; Epsilon : Float) is
      use Blade_List_Package;
      use GA_Maths;
      Blades    : Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Max_Mag   : Float := 0.0;
   begin
      while Has_Element (Curs) loop
         thisBlade := Element (Curs);
         Max_Mag := Maximum (Weight (thisBlade), Max_Mag);
         Next (Curs);
      end loop;

      if Max_Mag = 0.0 then
         MV.Blades.Clear;
      else
         Max_Mag := Epsilon;
         while Has_Element (Curs) loop
            thisBlade := Element (Curs);
            if Abs (Weight (thisBlade)) < Max_Mag then
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
      use GA_Maths;
      Scaled    : Multivector := MV;
      Scaled_GP : Multivector;
      Temp      : Multivector := Scaled;
      Sign      : Integer := -1;
      Result    : Multivector := New_Multivector (1.0);
   begin
      --  Taylor approximation
      for Count in 2 .. Order loop
         Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
         Temp := Geometric_Product (Temp, Scaled_GP);
         if (GA_Maths.Unsigned_Integer (Count) and 1) = 0 then
            Result := Result + Geometric_Product (Temp, Float (Sign));
            Sign := -Sign;
         end if;
      end loop;
      return Result;
   end Cosine_Series;

   --  -------------------------------------------------------------------------

   function Dot (MV1, MV2 : Multivector) return Multivector is
   begin
      return Inner_Product (MV1, MV2, Hestenes_Inner_Product);
   end Dot;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Size (MV)) - 1;
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (C3_Base'Enum_Val (Index)));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector; Dim : Integer) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, dim) - 1;
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (C3_Base'Enum_Val (Index)));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------
   --  Possibly imprecise
   function Exp_Series (MV : Multivector; Order : Integer)
                        return Multivector is
      Scaled    : Multivector;
      Scaled_GP : Multivector;
      Temp      : Multivector := New_Multivector (1.0);
      Sign      : Integer := -1;
      Scale     : Integer := 1;
      Max       : Float := Norm_E (MV);
      Result    : Multivector := Temp;
   begin
      if Max > 1.0 then
         Scale := 2;
      end if;
      while Max > 1.0 loop
         Max := Max / 2.0;
         Scale := 2 * Scale;
      end loop;
      Scaled := Geometric_Product (MV, 1.0 / Float (Scale));

      --  Taylor approximation
      for Count in 1 .. Order loop
         Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
         Temp := Geometric_Product (Temp, Scaled_GP);
         Result := Result + Temp;
      end loop;

      --  Undo scaling
      while Scale > 1 loop
         Result := Geometric_Product (Result, Result);
         Scale := Scale / 2;
      end loop;
      return Result;
   end Exp_Series;

   --  -------------------------------------------------------------------------

   function Extract_Grade (MV : Multivector; Index : integer) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
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
         Keep : array (0 .. Max_Grade + 1) of Boolean
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
      return MV_E;
   end Extract_Grade;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector is
      use Blade_List_Package;
      use Blade;
      use GA_Maths;
      Blades    : constant Blade_List := MV.Blades;
      Curs      : Cursor := Blades.First;
      New_MV    : Multivector;
   begin
      if Sc /= 0.0 then
         while Has_Element (Curs) loop
            New_MV.Blades.Append (New_Basis_Blade (Bitmap (Element (Curs)),
                                  Sc * Weight (Element (Curs))));
            Next (Curs);
         end loop;
         Simplify (New_MV);
         if Is_Empty (New_MV.Blades) then
            Put_Line ("Geometric_Product, scalar product MV is null.");
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
      use Blade_List_Package;
      use GA_Maths;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Curs_1    : Cursor := Blades_1.First;
      Curs_2    : Cursor;
      Blade_1   : Blade.Basis_Blade;
      Blade_2   : Blade.Basis_Blade;
      GP        : Multivector;
   begin
      if Is_Empty (List (Blades_1)) then
         Put_Line ("Multivector.Geometric_Product, MV1 is null.");
      end if;
      if Is_Empty (List (Blades_2)) then
         Put_Line ("Multivector.Geometric_Product, MV2 is null.");
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

   function General_Inverse (MV : Multivector) return Multivector is
      use Interfaces;
      use Blade_List_Package;
      use Blade;
      use GA_Maths;
      use GA_Maths.Float_Array_Package;
      use GA_Maths.Float_Functions;
      use Metric;
      Dim        : constant Integer :=  Space_Dimension (MV);
      Max_G      : constant Integer := 2 ** (Dim + 1);
      Blades     : constant Blade_List := MV.Blades;
      aBlade     : Basis_Blade;
      Curs       : Cursor := Blades.First;
      Mat        : Float_Matrix (1 .. Max_G, 1 .. Max_G) := (others => (others => 0.0));
      BBs        : Basis_Blade_Array (1 .. Max_G);
   begin
      for index in BBs'Range loop
         BBs (index) := New_Basis_Blade (Unsigned_Integer (index - 1));
      end loop;
      --  Construct a matrix 'Mat' such that matrix multiplication of 'Mat' with
      --  the coordinates of another multivector 'x' (stored in a vector)
      --  would result in the geometric product of 'Mat' and 'x'

      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         for index in BBs'Range loop
            Add_To_Matrix (Mat, BBs (index),
                           Geometric_Product (aBlade, BBs (index)));
         end loop;
         Next (Curs);
      end loop;

      return Matrix_To_MV_Invert (Mat, BBs);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.General_Inverse");
         raise;
   end General_Inverse;

   --  -------------------------------------------------------------------------

   function General_Inverse (MV : Multivector;
                             Met : Metric.Metric_Record) return Multivector is
      use Interfaces;
      use Blade_List_Package;
      use Blade;
      use GA_Maths;
      use GA_Maths.Float_Array_Package;
      use Metric;
      Dim        : constant Integer :=  Space_Dimension (MV);
      Max_G      : constant Integer := 2 ** (Dim + 1);
      Blades     : constant Blade_List := MV.Blades;
      aBlade     : Basis_Blade;
      Curs       : Cursor := Blades.First;
      Mat        : Float_Matrix (1 .. Max_G, 1 .. Max_G) := (others => (others => 0.0));
      BBs        : Basis_Blade_Array (1 .. Max_G);
   begin
      for index in BBs'Range loop
         BBs (index) := New_Basis_Blade (Unsigned_Integer (index - 1));
      end loop;
      --  Construct a matrix 'Mat' such that matrix multiplication of 'Mat' with
      --  the coordinates of another multivector 'x' (stored in a vector)
      --  would result in the geometric product of 'Mat' and 'x'

      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         for index in BBs'Range loop
            Add_To_Matrix (Mat, BBs (index),
                           Geometric_Product (aBlade, BBs (index), Met));
         end loop;
         Next (Curs);
      end loop;

      return Matrix_To_MV_Invert (Mat, BBs);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.General_Inverse Metric");
         raise;
   end General_Inverse;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : BV_Base) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Basis_Blade (Index));
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : E2_Base) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Basis_Blade (Index));
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : E3_Base) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Basis_Blade (Index));
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : C3_Base) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Basis_Blade (Index));
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Blade_List (MV : Multivector) return Blade_List is
   begin
      return MV.Blades;
   end Get_Blade_List;

   --  -------------------------------------------------------------------------

   function Get_Blade (MV : Multivector; Index : GA_Maths.Unsigned_Integer) return Blade.Basis_Blade is
      use Blade_List_Package;
      use GA_Maths;
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

   function Get_Blade (MV : Multivector; MV1 : out Multivector;
                       Index : GA_Maths.Unsigned_Integer) return Boolean is
      use Blade_List_Package;
      use Blade;
      use GA_Maths;
      Blades   : Blade_List := MV.Blades;
      Curs     : Cursor := Blades.First;
      Found    : Boolean := False;

   begin
      while Has_Element (Curs) and not Found loop
         Found := Bitmap (Element (Curs)) = Index;
         if Found then
            MV1.Blades.Append (Element (Curs));
         else
            Next (Curs);
         end if;
      end loop;

      return Found;
   end Get_Blade;

   --  -------------------------------------------------------------------------
   --  Grade returns the grade of a Multivector if homogeneous, -1 otherwise.
   --  0 is return for null Multivectors.
   --     function Grade (Blades : Blade_List) return Unsigned_Integer is
   --        use Blade_List_Package;
   --        thisBlade : GA_Maths.Basis_Blade;
   --        Cursor_B  : Cursor := Blades.First;
   --        theGrade  : Unsigned_Integer := 0; -- 0 .. 32
   --        OK        : Boolean := True;
   --     begin
   --        while OK and Has_Element (Cursor_B) loop
   --           thisBlade := Element (Cursor_B);
   --           if Cursor_B = Blades.First then
   --              theGrade := GA_Maths.Grade (GA_Maths.Bitmap (thisBlade));
   --           elsif theGrade /= GA_Maths.Grade (GA_Maths.Bitmap (thisBlade)) then
   --              OK := False;
   --           end if;
   --           Next (Cursor_B);
   --        end loop;
   --
   --        return theGrade;
   --     end Grade;

   --  -------------------------------------------------------------------------

   function Grade_Inversion (MV : Multivector) return Multivector is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Inversion     : Blade_List;
      thisBlade     : Blade.Basis_Blade;
      Cursor_B      : Cursor := Blades.First;
      GU            : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
      Largest_Part  : Multivector;
      Max_Norm      : Float := 0.0;
   begin
      while Has_Element (Cursor_B) loop
         thisBlade := Element (Cursor_B);
         Inversion.Append (Blade.Grade_Inversion (thisBlade));
         Next (Cursor_B);
      end loop;
      return  (Inversion, False);
   end Grade_Inversion;

   --  -------------------------------------------------------------------------
   --  Grade_Use returns a bitmap of grades that are in use in MV
   --  Bit 1: Scalar, Bit 2 etc non-scalar grades
   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      BB         : Blade.Basis_Blade;
      Cursor_B   : Cursor := Blades.First;
      GU_Bitmap  : Unsigned_32 := 0;
   begin
      while Has_Element (Cursor_B) loop
         BB := Element (Cursor_B);
         GU_Bitmap := GU_Bitmap or
           Shift_Left (1, Integer (Blade.Grade (BB)));
         Next (Cursor_B);
      end loop;
      return Unsigned_Integer (GU_Bitmap);
   end Grade_Use;

   --  -------------------------------------------------------------------------

   function Inner_Product (MV1, MV2 : Multivector; Cont : Contraction_Type)
                           return Multivector is
      use Ada.Containers;
      use Blade_List_Package;
      B1       : Blade.Basis_Blade;
      B2       : Blade.Basis_Blade;
      List_1   : Blade_List := MV1.Blades;
      List_2   : Blade_List := MV2.Blades;
      Cursor_1 : Cursor := List_1.First;
      Cursor_2 : Cursor;
      IP       : Blade.Basis_Blade;
      MV       : Multivector;
   begin
      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         Cursor_2:= List_2.First;
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
   end Inner_Product;

   --  -------------------------------------------------------------------------

   function Matrix_To_MV_Invert (Mat : GA_Maths.Float_Matrix;
                                 BBs : in out Basis_Blade_Array) return Multivector is
      use GA_Maths.Float_Array_Package;
      Dim        : Integer := Mat'Last - Mat'First + 1;
      Inv_Mat    : GA_Maths.Float_Matrix (1 .. Dim, 1 .. Dim) := (others => (others => 0.0));
      Value      : Float;
      Blades     : Blade_List;
      Inv_MV     : Multivector;
   begin
      Inv_Mat := Inverse (Mat);
      for Index in Inv_Mat'Range loop
         Value := Inv_Mat (Index, 1);
         if Value /= 0.0 then
            Update_Blade (BBs (Index), Value);
            Blades.Append (BBs (Index));
         end if;
      end loop;
      Inv_MV.Blades := Blades;
      return Inv_MV;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Matrix_To_MV_Invert");
         raise;
   end Matrix_To_MV_Invert;

   --  -------------------------------------------------------------------------

   function Is_Null (MV : Multivector) return Boolean is
      M : Multivector := MV;
   begin
      Simplify (M);
      return M.Blades.Is_Empty;
   end Is_Null;

   --  -------------------------------------------------------------------------

   function Is_Null (MV : Multivector; Epsilon : Float) return Boolean is
      S : Float := Norm_E2 (MV);
   begin
      return S < Epsilon * Epsilon;
   end Is_Null;

   --  -------------------------------------------------------------------------

   function Is_Scalar (MV : Multivector) return Boolean is
      use Ada.Containers;
      use Blade_List_Package;
      use GA_Maths;
      Blades : Blade_List := MV.Blades;
      Result : Boolean := Is_Null (MV);
   begin
      if not Result and then Blades.Length = 1 then
         Result := Bitmap (Element (Blades.First)) = 0;
      end if;
      return Result;
   end Is_Scalar;

   --  -------------------------------------------------------------------------

   function Largest_Basis_Blade (MV : Multivector) return Blade.Basis_Blade is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      theMV          : Multivector := MV;
      Blades         : constant Blade_List := theMV.Blades;
      Cursor_B       : Cursor := Blades.First;
      Largest_Blade  : Blade.Basis_Blade;
      Best_Scale     : Float := -1.0;
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
      return  Inner_Product (MV1, MV2, Left_Contraction);
   end Left_Contraction;

   --  -------------------------------------------------------------------------

   function Multivector_String (MV : Multivector; BV_Names : Blade.Basis_Vector_Names)
                                return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use Blade_List_Package;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      thisBlade    : Blade.Basis_Blade;
      Blade_UBS     : Ada.Strings.Unbounded.Unbounded_String;
      theString    : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("0.0");
   begin
      while Has_Element (Blade_Cursor) loop
         thisBlade := Element (Blade_Cursor);
         Blade_UBS := Blade.Blade_String (thisBlade, BV_Names);
         if Length (Blade_UBS) > 0 then
            declare
               Blade_String : String := To_String (Blade_UBS);
            begin
               New_Line;
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
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Multivector_String.");
         Put_Line (Exception_Information (anError));
         raise;
   end Multivector_String;

   --  -------------------------------------------------------------------------

   function New_Bivector (V1, V2 : Vector) return Bivector is
      BV : Bivector;
   begin
      return Bivector (Outer_Product (V1, V2));
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Bivector (e1e2, e2e3, e3e1 : Float) return Bivector is
      BV : Bivector;
   begin
      BV.Blades.Append (New_Basis_Blade (BV_e1e2, e1e2));
      BV.Blades.Append (New_Basis_Blade (BV_e2e3, e2e3));
      BV.Blades.Append (New_Basis_Blade (BV_e3e1, e3e1));
      Simplify (BV);
      return BV;
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Multivector (Scalar_Weight : Float) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      return  MV;
   end New_Multivector;

   --  -------------------------------------------------------------------------

   function New_Rotor return Rotor is
   begin
      return  New_Rotor (1.0);
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float) return Rotor is
      R : Rotor;
   begin
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float; BV : Bivector) return Rotor is
      R : Rotor;
   begin
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e1e2)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e2e3)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e3e1)));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight, e1, e2, e3 : Float) return Rotor is
      R : Rotor;
   begin
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (New_Basis_Blade (E3_e1, e1));
      R.Blades.Append (New_Basis_Blade (E3_e2, e2));
      R.Blades.Append (New_Basis_Blade (E3_e3, e3));
      return  R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Vector (e1, e2 : Float) return Vector is
      use Blade;
      V       : Vector;
      Blades  : Blade_List := Get_Blade_List (V);
   begin
      Add_Blade (V, New_Basis_Blade (E2_e1, e1));
      Add_Blade (V, New_Basis_Blade (E2_e2, e2));
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function New_Vector (e1, e2, e3 : Float) return Vector is
      use Blade;
      V       : Vector;
      Blades  : Blade_List := Get_Blade_List (V);
   begin
      Add_Blade (V, New_Basis_Blade (E3_e1, e1));
      Add_Blade (V, New_Basis_Blade (E3_e2, e2));
      Add_Blade (V, New_Basis_Blade (E3_e3, e3));
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function Norm_E (MV : Multivector) return Float is
      use GA_Maths.Float_Functions;
      S : Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if S < 0.0 then
         return 0.0;
      else
         return Sqrt (S);
      end if;
   end Norm_E;

   --  -------------------------------------------------------------------------

   function Norm_E2 (MV : Multivector) return Float is
      use GA_Maths.Float_Functions;
      S : Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if S < 0.0 then
         return 0.0;
      else
         return S;
      end if;
   end Norm_E2;

   --  -------------------------------------------------------------------------

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use Ada.Containers;
      use Blade_List_Package;
      B1        : Blade.Basis_Blade;
      B2        : Blade.Basis_Blade;
      List_1    : Blade_List := MV1.Blades;
      Cursor_1  : Cursor := List_1.First;
      OP_Blades : Blade_List;
      Blade_OP  : Blade.Basis_Blade;
      OP        : Multivector;
      Sorted    : Boolean;
   begin
      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         declare
            List_2   : Blade_List := MV2.Blades;
            Cursor_2 : Cursor := List_2.First;
         begin
            while Has_Element (Cursor_2) loop
               B2 := Element (Cursor_2);
               Blade_OP := Outer_Product (B1, B2);
               if Weight (Blade_OP) /= 0.0 then
                  OP_Blades.Append (Blade_OP);
               end if;
               Next (Cursor_2);
            end loop;
         end;
         Next (Cursor_1);
      end loop;

      Simplify (OP_Blades, Sorted);
      OP.Blades := OP_Blades;
      OP.Sorted := Sorted;
      return OP;
   end Outer_Product;

   --  -------------------------------------------------------------------------

   function Reverse_MV (MV : Multivector) return Multivector is
      use Blade_List_Package;
      Blades     : Blade_List := MV.Blades;
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
      return  Inner_Product (MV1, MV2, Right_Contraction);
   end Right_Contraction;

   --  -------------------------------------------------------------------------

   function Scalar_Part (MV : Multivector) return Float is
      use Blade_List_Package;
      use GA_Maths;
      BB       : Blade.Basis_Blade;
      Blades   : Blade_List := MV.Blades;
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
      return Scalar_Part (Inner_Product (MV1, MV2, Left_Contraction));
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   procedure Simplify (MV : in out Multivector) is
      use Blade_List_Package;
      Blades : Blade_List := MV.Blades;
      Sorted : Boolean;
   begin
      Simplify (Blades, Sorted);
      MV.Blades := Blades;
      MV.Sorted := Sorted;
   end Simplify;

   --  -------------------------------------------------------------------------

   procedure Simplify (Blades : in out Blade_List; Sorted : out Boolean) is
      use Blade_List_Package;
      use GA_Maths;
      Current_Blade  : Blade.Basis_Blade;
      Previous_Blade : Blade.Basis_Blade;
      Blade_Cursor   : Cursor;
      Prev_Curs      : Cursor;
      Has_Previous   : Boolean := False;
   begin
      if List (Blades) /= Empty_List then
         Blade_Sort_Package.Sort (List (Blades));
         Reverse_Elements (Blades);
         Blade_Cursor := Blades.First;
         Prev_Curs := No_Element;
         while Has_Element (Blade_Cursor) loop
            Current_Blade := Element (Blade_Cursor);
            if Weight (Current_Blade) = 0.0 then
               Blades.Delete (Blade_Cursor);
               Has_Previous := False;
               --  Delete sets Blade_Cursor to No_Element
               Blade_Cursor := Prev_Curs;
            elsif Has_Previous and then
              Bitmap (Previous_Blade) = Bitmap (Current_Blade) then
               Update_Blade (Previous_Blade,
                             Weight (Previous_Blade) + Weight (Current_Blade));
               Blades.Replace_Element (Prev_Curs, Previous_Blade);
               Blades.Delete (Blade_Cursor);
               Blade_Cursor := Prev_Curs;
            else
               Previous_Blade := Current_Blade;
               Has_Previous := True;
               Prev_Curs := Blade_Cursor;
            end if;
            Next (Blade_Cursor);
         end loop;

         Blade_Cursor := Blades.First;
         while Has_Element (Blade_Cursor) loop
            if Weight (Element (Blade_Cursor)) = 0.0 then
               Blades.Delete (Blade_Cursor);
            else
               Next (Blade_Cursor);
            end if;
         end loop;
      end if;
      Sorted := Blade_Sort_Package.Is_Sorted (List (Blades));
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
      use GA_Maths;
      Scaled    : Multivector := MV;
      Scaled_GP : Multivector;
      Temp      : Multivector := Scaled;
      Sign      : Integer := -1;
      Result    : Multivector := Scaled;
   begin
      --  Taylor approximation
      for Count in 2 .. Order loop
         Scaled_GP := Geometric_Product (Scaled, 1.0 / Float (Count));
         Temp := Geometric_Product (Temp, Scaled_GP);
         if (GA_Maths.Unsigned_Integer (Count) and 1) /= 0 then
            --  use only the odd part of the series
            Result := Result + Geometric_Product (Temp, Float (Sign));
            Sign := -Sign;
         end if;
      end loop;
      return Result;
   end Sine_Series;

   --  -------------------------------------------------------------------------

   function Size (MV : Multivector) return Natural is
      use Blade_List_Package;
      Blades  : Blade_List := MV.Blades;
   begin
      return Natural (Blades.Length);
   end Size;

   --  -------------------------------------------------------------------------

   function Space_Dimension (MV : Multivector) return Integer is
      use Blade_List_Package;
      use GA_Maths;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      High_Bit     : Integer;
      Max_Dim      : Integer := 0;
   begin
      while Has_Element (Blade_Cursor) loop
         High_Bit := Highest_One_Bit (Bitmap (Element (Blade_Cursor)));
         Max_Dim := Maximum (Max_Dim, High_Bit);
         Next (Blade_Cursor);
      end loop;
      return Max_Dim + 1;
   end Space_Dimension;

   --  -------------------------------------------------------------------------

   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer is
      use Blade_List_Package;
      use GA_Maths;
      Max_G        : Integer := 0;
      G            : Integer := 0;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      ThisBlade    : Blade.Basis_Blade;
      Index        : Integer := 0;
   begin
      while Has_Element (Blade_Cursor) loop
         Index := Index +1;
         ThisBlade := Element (Blade_Cursor);
         G := Integer (Blade.Grade (ThisBlade));
         Max_G := Maximum (Max_G, G);
         --           Put_Line ("Top_Grade_Index Index:" & Integer'Image (Index));
         --           Put_Line ("Top_Grade_Index Grade_Use:" & Integer'Image (G));
         --           Put_Line ("Top_Grade_Index Max_G:" & Integer'Image (Max_G));
         Next (Blade_Cursor);
      end loop;
      --         Put_Line ("Top_Grade_Index Max_G:" & Integer'Image (Max_G));
      return Unsigned_Integer (Max_G);
   end Top_Grade_Index;

   --  -------------------------------------------------------------------------

   function Unit_E (MV : Multivector) return Multivector is
   begin
      return Unit_R (MV);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Unit_E.");
         raise;
   end Unit_E;

   --  -------------------------------------------------------------------------

   function Unit_R (MV : Multivector) return Multivector is
      use GA_Maths.Float_Functions;
      theNorm  : Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if theNorm = 0.0 then
         Put_Line ("Multivector.Unit_R encountered a null multivector");
         raise MV_Exception;
      end if;

      return Geometric_Product (MV, 1.0 / Sqrt (Abs (theNorm)));

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Unit_R.");
         raise;
   end Unit_R;

   --  -------------------------------------------------------------------------

   procedure Update (MV : in out Multivector; Blades : Blade_List;
                     Sorted : Boolean := False) is
   begin
      MV.Blades := Blades;
      MV.Sorted := Sorted;
   end Update;

   --  -------------------------------------------------------------------------

   procedure Update_Scalar_Part (MV : in out Multivector; Value : Float) is
      use Blade_List_Package;
      use Blade;
      Blades    : Blade_List := Get_Blade_List (MV);
   begin
      MV.Blades.Replace_Element (Blades.First, New_Scalar_Blade (Value));
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
      when anError :  others =>
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

end Multivector;
