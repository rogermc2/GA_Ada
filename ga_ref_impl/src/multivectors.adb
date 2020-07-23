--  Derived from ga_ref_impl Multivector.java

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with SVD;

package body Multivectors is

   type Basis_Blade_Array is array (integer range <>) of Blade.Basis_Blade;

   Spatial_Dimension     : Natural := 0;
   MV_Basis_Vector_Names : Blade_Types.Basis_Vector_Names;
   --  This array can be used to lookup the number of coordinates for
   --  the grade part of a general multivector

   procedure Compress (MV : in out Multivector);
   function Cosine_Series (MV : Multivector; Order : Integer) return Multivector;
   function Exp_Series (MV    : Multivector; Met : Metric.Metric_Record;
                        Order : Integer)  return Multivector;
   function Random_Vector (Dim : Integer; Scale : Float) return Multivector;
   function Sine_Series (MV : Multivector; Order : Integer) return Multivector;
   function To_Geometric_Matrix (MV  : Multivector; BBs_L : Basis_Blade_Array;
                                 Met : Metric.Metric_Record)
                                  return GA_Maths.Float_Matrix;

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
         Blade_Neg := New_Blade (Bitmap (Blade_Neg), - Weight (Blade_Neg)) ;
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

   procedure Add_To_Matrix (M           : in out GA_Maths.Float_Matrix;
                            Beta, Gamma : Blade.Basis_Blade) is
      use Blade;
      --  Matrix indices are 1 greater than bitmap values
      Row  : constant Integer := Integer (Bitmap (Gamma)) + 1;
      Col  : constant Integer := Integer (Bitmap (Beta)) + 1;
   begin
      M (Row, Col) := M (Row, Col) + Weight (Gamma);

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Add_To_Matrix 1.");
         raise;
   end Add_To_Matrix;

   --  -------------------------------------------------------------------------

   procedure Add_To_Matrix (M    : in out GA_Maths.Float_Matrix;
                            Beta : Blade.Basis_Blade; Gamma : Blade.Blade_List) is
      use Blade;
      use Blade_List_Package;
      Curs  : Cursor := Gamma.First;
   begin
      while Has_Element (Curs) loop
         Add_To_Matrix (M, Beta, Element (Curs));
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

   function Basis_Vector (Index : BV_Base) return M_Vector is
      MV : M_Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : E2_Base) return M_Vector is
      MV : M_Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : E3_Base) return M_Vector is
      MV : M_Vector;
   begin
      MV.Blades.Append (Blade.New_Basis_Blade (Index));
      return MV;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.Basis_Vector.");
         raise;
   end Basis_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector (Index : C3_Base) return M_Vector is
      MV : M_Vector;
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

   function Dual (MV : Multivector; Met : Metric_Record := C3_Metric)
                   return Multivector is
      use Interfaces;
      Index  : constant Unsigned_32 :=
                 Shift_Left (1, Metric.Eigen_Values (Met)'Length) - 1;
      MV_I   : Multivector;
   begin
      --          case MV.Type_Of_MV is
      --          when MV_Line => MV_I.Type_Of_MV := MV_Dual_Line;
      --          when MV_Sphere => MV_I.Type_Of_MV := MV_Dual_Sphere;
      --          when others => raise MV_Exception with
      --                "Multivectors.DualMet called with invalid Type_Of_MV: " &
      --                MV_Type'Image (MV.Type_Of_MV);
      --          end case;

      MV_I.Blades.Append (Blade.New_Basis_Blade (Index));
      MV_I := Versor_Inverse (MV_I);
      return Inner_Product (MV, MV_I, Met, Blade.Left_Contraction);
   end Dual;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector; Dim : Integer) return Multivector is
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Dim) - 1;
      MV_I    : Multivector;
   begin
      MV_I.Blades.Append (Blade.New_Basis_Blade (Index));
      MV_I := Versor_Inverse (MV_I);
      return Inner_Product (MV, MV_I, Blade.Left_Contraction);
   end Dual;

   --  -------------------------------------------------------------------------

   function Exp (MV    : Multivector; Met : Metric_Record := C3_Metric;
                 Order : Integer := 12) return Multivector is
      use GA_Maths.Float_Functions;
      A2     : constant Multivector := Geometric_Product (MV, MV, Met);
      Result : Multivector;
   begin
      if Is_Null (A2, 10.0 ** (-8)) then
         Result := New_Multivector (1.0);
      elsif Is_Scalar (A2) then
         Result := New_Multivector (Exp (Scalar_Part (A2)));
      else
         Result := Exp_Series (MV, Met, Order);
      end if;
      return Result;
   end Exp;

   --  -------------------------------------------------------------------------

   --      function Exp_Dual_Line (DL : Dual_Line; Met : Metric.Metric_Record)
   --                             return Dual_Line is
   --
   --          use Blade;
   --          use Blade_List_Package;
   --          Blades     : constant Blade_List := DL.Blades;
   --          Curs       : Cursor := Blades.First;
   --          MV        : Multivector;
   --      begin
   --          while Has_Element (Curs) loop
   --              Add_Blade (MV, Element (Curs));
   --              Next (Curs);
   --          end loop;
   --          MV := Exp (MV, Met);
   --          return To_Dual_Line (MV);
   --      end Exp_Dual_Line;

   --  -------------------------------------------------------------------------
   --  Possibly imprecise
   function Exp_Series (MV    : Multivector;  Met : Metric.Metric_Record;
                        Order : Integer) return Multivector is
      Scaled    : Multivector;
      Scaled_GP : Multivector;
      Temp      : Multivector := New_Multivector (1.0);
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
         Result := Geometric_Product (Result, Result, Met);
         Scale := Scale / 2;
      end loop;
      return Result;
   end Exp_Series;

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

   function From_Vector (V : M_Vector) return Multivector is
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
--     function General_Inverse (MV : Multivector) return Multivector is
--        use Interfaces;
--        use GA_Maths.Float_Array_Package;
--        use Blade;
--        use Blade_List_Package;
--        use GA_Maths;
--        --        Dim          : constant Natural := Space_Dimension (MV);
--        Max_Index    : constant Natural := 2 ** Spatial_Dimension;
--        Mat          : Float_Matrix (1 .. Max_Index, 1 .. Max_Index) :=
--                         (others => (others => 0.0));
--        Mat_Inv      : Float_Matrix (1 .. Max_Index, 1 .. Max_Index) :=
--                         (others => (others => 0.0));
--        BBs_L        : Basis_Blade_Array (1 .. Max_Index);
--        Curs         : Cursor := MV.Blades.First;
--        Cond         : Float;
--        aBlade       : Basis_Blade;
--        GP           : Basis_Blade;
--        Value        : Float;
--        Blades       : Blade_List;
--        Result       : Multivector;
--     begin
--        if Is_Scalar (MV) or Is_Null (MV) then
--           Result := MV;
--        else
--           for index in BBs_L'Range loop
--              BBs_L (index) := New_Basis_Blade (Interfaces.Unsigned_32 (index - 1));
--           end loop;
--
--           --  Construct a matrix 'Mat' such that matrix multiplication of 'Mat' with
--           --  the coordinates of another multivector 'x' (stored in a M_Vector)
--           --  would result in the geometric product of 'Mat' and 'x'
--           while Has_Element (Curs) loop
--              aBlade := Element (Curs);
--              for index in BBs_L'Range loop
--                 GP := Geometric_Product (aBlade, BBs_L (index));
--                 if Weight (GP) /= 0.0 then
--                    Add_To_Matrix (Mat, BBs_L (index), GP);
--                 end if;
--              end loop;
--              Next (Curs);
--           end loop;
--
--           Cond := SVD.Condition_Number (Mat);
--           if Cond < 1.0 / Float'Model_Epsilon then
--              Mat_Inv := Inverse (Mat);
--           else
--              raise MV_Exception with
--                "Multivectors.Geometric_Product, Mat is not invertible";
--           end if;
--           --  reconstruct multivector from first column of matrix
--           --  for Row in BBs'Range loop
--           for Row in Mat_Inv'Range (1) loop
--              Value := Mat_Inv (Row, 1);
--              if Value /= 0.0 then
--                 Update_Blade (BBs_L (Row), Value);
--                 Blades.Append (BBs_L (Row));
--              end if;
--           end loop;
--           Result := New_Multivector (Blades);
--        end if;
--
--        return Result;
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Multivector.General_Inverse.");
--           raise;
--     end General_Inverse;

   --  -------------------------------------------------------------------------

   function General_Inverse (MV : Multivector;  Met : Metric_Record := C3_Metric)
                              return Multivector is
      use Interfaces;
      use Blade;
      use GA_Maths;
      use Float_Array_Package;
      --        Dim          : Natural;
      Cond         : Float;
      Value        : Float;
      Blades       : Blade_List;
      Result       : Multivector;
   begin
      if Is_Scalar (MV) or Is_Null (MV) then
         Result := MV;
      else
         --           Dim := Space_Dimension (MV) ;
         declare
            --  cern.colt.matrix.DoubleFactory2D.dense.make(1 << dim, 1 << dim);
            --              L_Max    : constant integer := 2 ** (Dim - 1);  --  1 << dim
            L_Max    : constant integer := 2 ** (Spatial_Dimension);
            Mat      : Float_Matrix (1 .. L_Max, 1 .. L_Max) :=
                         (others => (others => 0.0));
            Mat_Inv  : Float_Matrix (1 .. L_Max, 1 .. L_Max) :=
                         (others => (others => 0.0));
            BBs_L    : Basis_Blade_Array (1 .. L_Max);
            --                  Det      : Float;
         begin
            --  Create all unit basis blades for Dim L
            --  Array of basis bitmaps (0 - 31)
            for index in BBs_L'Range loop
               BBs_L (index) := New_Basis_Blade (Interfaces.Unsigned_32 (index - 1));
            end loop;

            Mat := To_Geometric_Matrix (MV, BBs_L, Met);
            Cond := SVD.Condition_Number (Mat);
            if Cond < 1.0 / Float'Model_Epsilon then
               Mat_Inv := Inverse (Mat);
            else
               raise MV_Exception with
                 "Multivectors.General_Inverse metric cannot invert matrix as it is singular.";
            end if;

            for Row in BBs_L'Range loop
               Value := Mat_Inv (Row, 1);
               if Value /= 0.0 then
                  Update_Blade (BBs_L (Row), Value);
                  Blades.Append (BBs_L (Row));
               end if;
            end loop;
            Result := New_Multivector (Blades);
         end; --  declare block
      end if;
      Simplify (Result);
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector.General_Inverse Metric");
         raise;
   end General_Inverse;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades    : constant Blade_List := MV.Blades;
      Curs      : Cursor := Blades.First;
      New_MV    : Multivector;
   begin
      if Is_Empty (Blades) then
         New_MV := MV;
      elsif Sc /= 0.0 then
         while Has_Element (Curs) loop
            New_MV.Blades.Append (New_Blade (Bitmap (Element (Curs)),
                                  Sc * Weight (Element (Curs))));
            Next (Curs);
         end loop;
         Simplify (New_MV);
      end if;
      return New_MV;
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector is
   begin
      return Geometric_Product (MV, Sc);
   end Geometric_Product;

   --  -------------------------------------------------------------------------

--     function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
--        use Blade;
--        use Blade_List_Package;
--        Blades_1  : constant Blade_List := MV1.Blades;
--        Blades_2  : constant Blade_List := MV2.Blades;
--        Curs_1    : Cursor := Blades_1.First;
--        Curs_2    : Cursor;
--        Blade_1   : Blade.Basis_Blade;
--        Blade_2   : Blade.Basis_Blade;
--        GP        : Multivector;
--     begin
--        if Is_Empty (List (Blades_1)) then
--           raise MV_Exception with "Multivectors.Geometric_Product, MV1 is null.";
--        end if;
--        if Is_Empty (List (Blades_2)) then
--           raise MV_Exception with "Multivectors.Geometric_Product, MV2 is null.";
--        end if;
--
--        while Has_Element (Curs_1) loop
--           Blade_1 := Element (Curs_1);
--           Curs_2 := Blades_2.First;
--           while Has_Element (Curs_2) loop
--              Blade_2 := Element (Curs_2);
--              GP.Blades.Append (Blade.Geometric_Product (Blade_1, Blade_2));
--              Next (Curs_2);
--           end loop;
--           Next (Curs_1);
--        end loop;
--        Simplify (GP);
--
--        return GP;
--
--     end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV1, MV2 : Multivector; Met : Metric_Record := C3_Metric)
                               return Multivector is
      use Blade;
      use Blade_List_Package;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Blades_GP : Blade_List;
      Curs_1    : Cursor := Blades_1.First;
      Curs_2    : Cursor;
      Blade_1   : Blade.Basis_Blade;
      Blade_2   : Blade.Basis_Blade;
      GP        : Multivector;
   begin
      if Is_Empty (List (MV1.Blades)) then
         raise MV_Exception with "Multivectors.Geometric_Product with Metric, MV1 is null.";
      end if;
      if Is_Empty (List (MV2.Blades)) then
         raise MV_Exception with "Multivectors.Geometric_Product with Metric, MV2 is null.";
      end if;

      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         Curs_2 := Blades_2.First;
--           GA_Utilities.Print_Blade_String
--             ("Multivectors.Geometric_Product with Metric, Blade_1",
--              Blade_1, Blade_Types.Basis_Names_C3GA);
         while Has_Element (Curs_2) loop
            Blade_2 := Element (Curs_2);
--              GA_Utilities.Print_Blade_String
--                ("Multivectors.Geometric_Product with Metric, Blade_2", Blade_2,
--                 Blade_Types.Basis_Names_C3GA);
            Blades_GP := Blade.Geometric_Product (Blade_1, Blade_2, Met);
--              GA_Utilities.Print_Blade_List
--                ("Multivectors.Geometric_Product with Metric, Blades_GP", Blades_GP);
            Add_Blades (GP.Blades, Blades_GP);
            Next (Curs_2);
         end loop;
         Next (Curs_1);
      end loop;
      Simplify (GP);
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

   function Get_Random_Blade (Dim, Grade : Integer; Scale : Float)
                               return Multivector is
      Result : Multivector :=
                 New_Multivector (Scale * Float (Maths.Random_Float));
   begin
      for  index in 1 .. Grade loop
         Result := Outer_Product (Result, Get_Random_Vector (Dim, Scale));
      end loop;
      return Result;
   end Get_Random_Blade;

   --  -------------------------------------------------------------------------

   function Get_Random_Vector (Dim : Integer; Scale : Float)
                                return Multivector is
      use Blade;
      Blades     : Blade_List;
      Base_Index : C3_Base;
   begin
      for index in 1 .. Dim loop
         Base_Index := C3_Base'Enum_Val (2 * (index - 1));
         Blades.Add_Blade (New_Basis_Blade (Base_Index,
                           Scale * Float (Maths.Random_Float)));
      end loop;
      return New_Multivector (Blades);
   end Get_Random_Vector;

   --  -------------------------------------------------------------------------
   --  Grade returns the grade (bit count) of an homogeneous Multivector.
   --  0 is returned for null Multivectors.
   --  Is_Homogeneous False is returned if the Multivector is not homogeneous.
   function Grade (MV : Multivector; theGrade : out Integer)
                    return Grade_Status is
      use Blade;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Cursor_B      : Cursor := Blades.First;
      thisBlade     : Blade.Basis_Blade;
      OK            : Boolean := not Blades.Is_Empty;
      Status        : Grade_Status := Grade_OK;
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
         if not OK then
            Status := Grade_Inhomogeneous;
         end if;
      else
         Status := Grade_Null;
      end if;

      return Status;
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

   function Highest_Grade (MV : Multivector) return Integer is

      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := Multivectors.Get_Blade_List (MV);
      Curs       : Cursor := Blades.First;
      aGrade     : Integer;
      High_Grade : Integer := 0;
   begin
      while Has_Element (Curs) loop
         aGrade := Grade (Element (Curs));
         if aGrade > High_Grade then
            High_Grade := aGrade;
         end if;
         Next (Curs);
      end loop;
      return High_Grade;

   end Highest_Grade;

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
            Blades_IP := Blade.Inner_Product (B1, B2, Met, Cont);
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

   function Inverse_Rotor (R : Rotor) return Rotor is
   begin
      return To_Rotor (General_Inverse (R));
   end Inverse_Rotor;

   --  -------------------------------------------------------------------------
   --  Inverse based on c3ga.h inline scalar inverse
   function Inverse_Scalar (theScalar : Scalar) return Scalar is
      use Blade;
      S_Weight    : constant float := Weight (theScalar.Blades.First_Element);
      Inv_Weight  : float;
   begin
      if S_Weight = 0.0 then
         raise MV_Exception with
           "Multivectors.Inverse_Scalar called with zero weight blade";
      end if;
      Inv_Weight := 1.0 / S_Weight ** 2;
      return New_Scalar (S_Weight * Inv_Weight);
   end Inverse_Scalar;

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

   function Largest_Coordinate (MV : Multivector) return Float is
      use Blade;
      use Blade_List_Package;
      theMV    : Multivector := MV;
      Blades   : constant Blade_List := theMV.Blades;
      Cursor_B : Cursor := Blades.First;
      aBlade   : Blade.Basis_Blade;
      Largest  : Float := 0.0;
   begin
      Simplify (theMV);
      while Has_Element (Cursor_B) loop
         aBlade := Element (Cursor_B);
         Largest := GA_Maths.Maximum (Largest, Abs (Blade.Weight (aBlade)));
         Next (Cursor_B);
      end loop;
      return Largest;
   end Largest_Coordinate;

   --  -------------------------------------------------------------------------

   function Left_Contraction (MV1, MV2 : Multivector) return Multivector is
   begin
      return Inner_Product (MV1, MV2, Blade.Left_Contraction);
   end Left_Contraction;

   --  -------------------------------------------------------------------------

   function Left_Contraction (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                               return Multivector is
   begin
      return Inner_Product (MV1, MV2, Met, Blade.Left_Contraction);
   end Left_Contraction;

   --  -------------------------------------------------------------------------

   function List_Length (MV_List : Multivector_List) return Integer is
   begin
      return  Integer (MV_List.Length);
   end List_Length;

   --  -------------------------------------------------------------------------

   function Multivector_String (MV       : Multivector;
                                BV_Names : Blade_Types.Basis_Vector_Names)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use Blade;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Blade_Cursor  : Cursor := Blades.First;
      thisBlade     : Blade.Basis_Blade;
      Blade_UBS     : Ada.Strings.Unbounded.Unbounded_String;
      theString     : Ada.Strings.Unbounded.Unbounded_String :=
                        To_Unbounded_String ("0.0");
      String_Length : Integer := 0;
   begin
      while Has_Element (Blade_Cursor) loop
         thisBlade := Element (Blade_Cursor);
         --           GA_Utilities.Print_Blade ("Multivectors.Multivector_String thisBlade", thisBlade);
         Blade_UBS := Blade.Blade_String (thisBlade, BV_Names);
         --           Put_Line ("Multivectors.Multivector_String, " & To_String (Blade_UBS));
         if Length (Blade_UBS) > 0 then
            declare
               Blade_String : constant String := To_String (Blade_UBS);
            begin
               if Blade_Cursor = Blades.First then
                  theString := To_Unbounded_String (Blade_String);
               else
                  if Blade_String (1) = '-' then
                     theString := theString & " - ";
                  else
                     theString := theString & " + ";
                  end if;
                  theString :=
                    theString & Blade_String (2 .. Blade_String'Length);
               end if;

               if Length (theString) - String_Length > 50 then
                  theString := theString & ASCII.LF;
                  String_Length := Length (theString);
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

   function MV_Item (MV_List : Multivector_List; Index : Integer)
                      return Multivector is
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

   function MV_Size (MV : Multivector) return Natural is
      Blades  : constant Blade.Blade_List := MV.Blades;
   begin
      return Natural (Blades.Length);
   end MV_Size;

   --  -------------------------------------------------------------------------

   function Space_Dimension return Natural is
   begin
      return Spatial_Dimension;
   end Space_Dimension;

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

   --  ------------------------------------------------------------------------

   function New_Bivector (V1, V2 : M_Vector) return Bivector is
   begin
      return Bivector (Outer_Product (V1, V2));
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Bivector (e1e2, e2e3, e3e1 : Float) return Bivector is
      use Blade;
      BV : Bivector;
   begin
      --          BV.Type_Of_MV := MV_Bivector;
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

   --      function New_Normalized_Point return Normalized_Point is
   --          NP : Normalized_Point;
   --      begin
   --          NP.Type_Of_MV := MV_Normalized_Point;
   --          return NP;
   --      end New_Normalized_Point;

   --  -------------------------------------------------------------------------

   function New_Normalized_Point (e1, e2, e3 : Float) return Normalized_Point is
      use Blade;
      NP     : Normalized_Point;
   begin
      --          NP.Type_Of_MV := MV_Normalized_Point;
      NP.Blades.Append (New_Basis_Blade (C3_no, 1.0));
      NP.Blades.Append (New_Basis_Blade (C3_e1, e1));
      NP.Blades.Append (New_Basis_Blade (C3_e2, e2));
      NP.Blades.Append (New_Basis_Blade (C3_e3, e3));
      NP.Blades.Append
        (New_Basis_Blade (C3_ni, 0.5 * (e1 * e1 + e2 * e2 + e3 * e3)));
      return NP;
   end New_Normalized_Point;

   --  -------------------------------------------------------------------------

   --      function New_Rotor return Rotor is
   --      begin
   --          return  New_Rotor (1.0);
   --      end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float) return Rotor is
      use Blade;
      R : Rotor;
   begin
      --          R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (Blade.New_Scalar_Blade (Scalar_Weight));
      return R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight : Float; BV : Bivector) return Rotor is
      use Blade;
      R : Rotor;
   begin
      --          R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e1e2)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e2e3)));
      R.Blades.Append (Get_Blade (BV, BV_Base'Enum_Rep (BV_e3e1)));
      return R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Rotor (Scalar_Weight, e1, e2, e3 : Float) return Rotor is
      use Blade;
      R : Rotor;
   begin
      --          R.Type_Of_MV := MV_Rotor;
      R.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      R.Blades.Append (New_Basis_Blade (E3_e1, e1));
      R.Blades.Append (New_Basis_Blade (E3_e2, e2));
      R.Blades.Append (New_Basis_Blade (E3_e3, e3));
      return R;
   end New_Rotor;

   --  -------------------------------------------------------------------------

   function New_Scalar  (Scalar_Weight : Float := 0.0) return Scalar is
      S : Scalar;
   begin
      --          S.Type_Of_MV := MV_Scalar;
      S.Blades.Append (Blade.New_Scalar_Blade (Scalar_Weight));
      return S;
   end New_Scalar;

   --  ------------------------------------------------------------------------

   function New_TR_Versor (Scalar_Weight : Float := 0.0) return TR_Versor is
      V : TR_Versor;
   begin
      --          V.Type_Of_MV := MV_TR_Versor;
      if Scalar_Weight /= 0.0 then
         V.Blades.Append (Blade.New_Scalar_Blade (Scalar_Weight));
      end if;
      return V;
   end New_TR_Versor;

   --  ------------------------------------------------------------------------

   --      function New_Vector return M_Vector is
   --          V : M_Vector;
   --      begin
   --          V.Type_Of_MV := MV_Vector;
   --          return V;
   --      end New_Vector;

   --  ------------------------------------------------------------------------

   function New_Vector (e1, e2 : Float) return M_Vector is
      use Blade;
      V : M_Vector;
   begin
      --          V.Type_Of_MV := MV_Vector;
      Add_Blade (V, New_Basis_Blade (E2_e1, e1));
      Add_Blade (V, New_Basis_Blade (E2_e2, e2));
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function New_Vector (e1, e2, e3 : Float) return M_Vector is
      use Blade;
      V : M_Vector;
   begin
      --          V.Type_Of_MV := MV_Vector;
      Add_Blade (V, New_Basis_Blade (E3_e1, e1));
      Add_Blade (V, New_Basis_Blade (E3_e2, e2));
      Add_Blade (V, New_Basis_Blade (E3_e3, e3));
      Simplify (V);
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

   function Norm_E (MV : Multivector) return Float is
      use GA_Maths.Float_Functions;
   begin
      return Sqrt (Norm_Esq (MV));
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

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use Blade;
      use Blade_List_Package;
      List_1     : constant Blade_List := MV1.Blades;
      List_2     : constant Blade_List := MV2.Blades;
      Cursor_1   : Cursor := List_1.First;
      Cursor_2   : Cursor;
      Blade_OP   : Blade.Basis_Blade;
      B1         : Blade.Basis_Blade;
      B2         : Blade.Basis_Blade;
      OP         : Multivector;
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
            OP.Blades.Append (Blade_OP);
            Next (Cursor_2);
         end loop;
         Next (Cursor_1);
      end loop;

      Simplify (OP);
      return OP;
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
   begin
      --  loop over all basis lades, reverse each one and add to result
      while Has_Element (B_Cursor) loop
         Rev_Blades.Append (Reverse_Blade (Element (B_Cursor)));
         Next (B_Cursor);
      end loop;
      return New_Multivector (Rev_Blades);
   end Reverse_MV;

   --  -------------------------------------------------------------------------

   function Right_Contraction (MV1, MV2 : Multivector) return Multivector is
   begin
      return Inner_Product (MV1, MV2, Blade.Right_Contraction);
   end Right_Contraction;

   --  -------------------------------------------------------------------------

   function Right_Contraction (MV1, MV2 : Multivector; Met : Metric.Metric_Record)
                                return Multivector is
   begin
      return Inner_Product (MV1, MV2, Met, Blade.Right_Contraction);
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

   procedure Set_Geometry (theGeometry : Geometry_Type) is
   begin
      case theGeometry is
         when E1_Geometry => Spatial_Dimension := 1;
         when E2_Geometry => Spatial_Dimension := 2;
         when E3_Geometry => Spatial_Dimension := 3;
         when H3_Geometry => Spatial_Dimension := 4;
         when C3_Geometry => Spatial_Dimension := 5;
         when others =>
            raise MV_Exception with
              "Multivectors.Set_Geometry, invalid geometry.";
      end case;
   end Set_Geometry;

   --  -------------------------------------------------------------------------

   procedure Simplify (MV : in out Multivector) is
      Blades : Blade.Blade_List := MV.Blades;
   begin
      if Blade.List_Length (Blades) > 0 then
         Blade.Simplify (Blades);
         MV.Blades := Blades;
      end if;
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

   --  Space_Dimension returns the dimension of the space that this blade
   --  (apparently) lives in.
   --     function Space_Dimension (MV : Multivector) return Natural is
   --        use Blade;
   --        use Blade_List_Package;
   --        Max_D        : Natural := 0;
   --        Blade_Cursor : Cursor := MV.Blades.First;
   --     begin
   --        while Has_Element (Blade_Cursor) loop
   --           Max_D := GA_Maths.Maximum (Max_D,
   --                                      Bits.Highest_One_Bit (Bitmap (Element (Blade_Cursor))));
   --           Next (Blade_Cursor);
   --        end loop;
   --
   --        return Max_D + 1;
   --     end Space_Dimension;

   --  -------------------------------------------------------------------------
   --  To_Geometric_Matrix constructs a matrix 'Matrix_AG' such that
   --  matrix multiplication of 'Matrix_AG' with the coordinates of another
   --  multivector 'x' (stored in a M_Vector) would result in the
   --  geometric product of 'Matrix_AG' and 'x'
   --  Metric version:
   function To_Geometric_Matrix (MV  : Multivector; BBs_L : Basis_Blade_Array;
                                 Met : Metric.Metric_Record)
                                  return GA_Maths.Float_Matrix is
      use Blade;
      use GA_Maths;
      use Blade_List_Package;
      L_Max      : constant integer := BBs_L'Length;
      Matrix_AG  : Float_Matrix (1 .. L_Max, 1 .. L_Max) := (others => (others => 0.0));
      BL_Curs_i  : Cursor := MV.Blades.First;
      Blade_b    : Basis_Blade;  --  b
      GP_List    : Blade_List;

   begin
      while Has_Element (BL_Curs_i) loop
         --  for each blade of BL
         Blade_B := Element (BL_Curs_i);
         --  for each bitmap
         --  geometric multiply the multivector blade by each unit basis blade
         --  and add each result to matrix M at rows corresponding to the
         --  the product and the column corresponding to the unit basis blade.
         for index_j in BBs_L'Range loop
            --  metric instance of Metric
            --  gp(aBlade, BBs (index_j), Met) corresponds to L_k L_j of equation (20.1)
            GP_List := Geometric_Product (Blade_b, BBs_L (index_j), Met);
            if not Is_Empty (GP_List) then
               Add_To_Matrix (Matrix_AG, BBs_L (index_j), GP_List);
            end if;
         end loop;
         Next (BL_Curs_i);
      end loop;

      return Matrix_AG;
   end To_Geometric_Matrix;

   --  -------------------------------------------------------------------------

   function Top_Grade_Index (MV : Multivector) return Integer is
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
      return Grade_Count;
   end Top_Grade_Index;

   --  -------------------------------------------------------------------------

   function To_Bivector (R : Rotor) return Bivector is
      use Interfaces;
      use Blade;
      use Blade_List_Package;
      Blades : constant Blade_List := R.Blades;
      Curs   : Cursor := Blades.First;
      aBlade : Basis_Blade;
      BM     : Unsigned_32;
      BV     : Bivector;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         BM := aBlade.Bitmap;
         if BM = C3_Base'Enum_Rep (C3_e1_e2) or
           BM = C3_Base'Enum_Rep (C3_e1_e3) or
           BM = C3_Base'Enum_Rep (C3_e2_e3) then
            Add_Blade (BV, aBlade);
         end if;
         Next (Curs);
      end loop;
      return BV;
   end To_Bivector;

   --  -------------------------------------------------------------------------

   function To_Circle (MV : Multivector) return Circle is
      theCircle : Circle;
   begin
      theCircle.Blades := MV.Blades;
      theCircle.Sorted := MV.Sorted;
      return theCircle;
   end To_Circle;

   --  -------------------------------------------------------------------------

   function To_Dual_Line (MV : Multivector) return Dual_Line is
      DL : Dual_Line;
   begin
      DL.Blades := MV.Blades;
      DL.Sorted := MV.Sorted;
      return DL;
   end To_Dual_Line;

   --  -------------------------------------------------------------------------

   function To_Dual_Plane (MV : Multivector) return Dual_Plane is
      theDual_Plane : Dual_Plane;
   begin
      theDual_Plane.Blades := MV.Blades;
      theDual_Plane.Sorted := MV.Sorted;
      return theDual_Plane;
   end To_Dual_Plane;

   --  -------------------------------------------------------------------------

   function To_Line (MV : Multivector) return Line is
      theLine : Line;
   begin
      theLine.Blades := MV.Blades;
      theLine.Sorted := MV.Sorted;
      return theLine;
   end To_Line;

   --  -------------------------------------------------------------------------

   function To_Normalized_Point (MV :  Multivector) return Normalized_Point is
      thePoint : Normalized_Point;
   begin
      thePoint.Blades := MV.Blades;
      thePoint.Sorted := MV.Sorted;
      return thePoint;
   end To_Normalized_Point;

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

   function To_TRversor (MV : Multivector) return TR_Versor is
      use Blade;
      use Blade_List_Package;
      Blades : constant Blade_List := MV.Blades;
      Curs   : Cursor := Blades.First;
      TRV    : TR_Versor;
   begin
      while Has_Element (Curs) loop
         Add_Blade (TRV, Element (Curs));
         Next (Curs);
      end loop;
      return TRV;
   end To_TRversor;

   --  -------------------------------------------------------------------------
   function To_Vector (MV : Multivector) return M_Vector is
      use Blade;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      Curs       : Cursor := Blades.First;
      Vec        : M_Vector;
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

--     function Unit_R (MV : Multivector) return Multivector is
--        use GA_Maths.Float_Functions;
--        Norm_Sq  : constant Float := Scalar_Product (MV, Reverse_MV (MV));
--     begin
--        if Norm_Sq = 0.0 then
--           raise MV_Exception with
--             "Multivectors.Unit_R encountered a null multivector";
--        end if;
--
--        return Geometric_Product (MV, 1.0 / Sqrt (Abs (Norm_Sq)));
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Multivectors.Unit_R.");
--           raise;
--     end Unit_R;

   --  -------------------------------------------------------------------------

   function Unit_R (MV : Multivector; Met : Metric_Record := C3_Metric)
                    return Multivector is
      use GA_Maths.Float_Functions;
      Norm_Sq  : constant Float := Scalar_Product (MV, Reverse_MV (MV), Met);
   begin
      if Norm_Sq = 0.0 then
         raise MV_Exception with
           "Multivectors.Unit_R Metric encountered a null multivector";
      end if;

      return Geometric_Product (MV, 1.0 / Sqrt (Abs (Norm_Sq)));

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
--     function Versor_Inverse (MV : Multivector) return Multivector is
--        Rev    : constant Multivector := Reverse_MV (MV);
--        Weight : constant Float := Scalar_Product (MV, Rev);
--     begin
--        if Weight = 0.0 then
--           raise MV_Exception with
--             "Multivector.Versor_Inverse encountered a non-invertible multivector" ;
--        end if;
--
--        return Rev / Weight;
--
--     end Versor_Inverse;

   --  -------------------------------------------------------------------------

   function Versor_Inverse (MV : Multivector; Met : Metric_Record := C3_Metric)
                             return Multivector is
      Rev    : constant Multivector := Reverse_MV (MV);
      Weight : constant Float := Scalar_Product (MV, Rev, Met);
   begin
      if Weight = 0.0 then
         Put_Line
           ("Multivector.Versor_Inverse encountered a non-invertible multivector");
         raise MV_Exception;
      end if;

      return Rev / Weight;

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
