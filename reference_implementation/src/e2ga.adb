
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Multivector_Analyze;
with Multivector_Type_Base;

package body E2GA is

   type Array_BM4 is array (Bit_Map range 1 .. 4) of integer;
   type Array_F4 is array (1 .. 4) of float;
   type float_3 is digits 3;

   MV_Space_Dimension                : constant integer := 2;
   MV_Metric_Euclidean               : constant boolean := True;
   --  MV_Grade_Size is a lookup table for the number of coordinates
   --  in the grade part of a general multivector
   MV_Grade_Size                     : constant GA_Maths.Grade_Array := (1, 2, 1);
   --  MV_Size is a lookup table for the number of coordinates based on a grade usage bitmap
   MV_Size                           : constant GA_Maths.Array_I8 := (0, 1, 2, 3, 1, 2, 3, 4);
   MV_Basis_Elements                 : array (0 .. 3, 1 .. 3) of integer :=
     ((-1, -1, -1), (0, -1, -1), (1, -1, -1), (0, 1, -1));
   MV_Basis_Element_Sign_By_Index    : constant Array_F4 := (1.0, 1.0, 1.0, 1.0);
   MV_Basis_Element_Sign_By_Bit_Map  : constant Array_BM4 := (1, 1, 1, 1);
   --  MV_Basis_Element_Index_By_Bit_Map contains the order of basis elements in the general multivector
   --  Use it to answer: 'at what index do I find basis element [x] (x = basis vector bitmap)?'
   MV_Basis_Element_Index_By_Bit_Map : constant Array_BM4 := (0, 1, 2, 3);
   --  MV_Basis_Element_Bit_Map_By_Index contains the indices of basis elements in the general multivector
   --  Use it to answer: 'what basis element do I find at index [x]'?
   MV_Basis_Element_Bit_Map_By_Index : constant GA_Maths.Array_I4 := (0, 1, 2, 3);
   MV_Basis_Element_Grade_By_Bit_Map : constant Array_BM4 := (0, 1, 1, 2);
   MV_Basis_Vector_Names             : constant array (1 .. 2) of string (1 .. 2) := ("e1", "e2");

   e1_basis : Vector_2D := (1.0, 0.0);
   e2_basis : Vector_2D := (0.0, 1.0);

   --  -------------------------------------------------------------------------

   function Inverse (MV : Multivector) return Multivector;
   function Scalar_Product (MV1, MV2 : Multivector) return Scalar_MV;
   function Scalar_Product (MV1, MV2 : Multivector) return Scalar;
   function Reverse_Multivector (MV : Multivector) return Multivector;

   --  -------------------------------------------------------------------------

   function "+" (V1, V2 : Vector_2D) return Vector_2D is
   begin
      return (V1 (1) + V2 (1), V1 (2) + V2 (2));
   end "+";

   --  ------------------------------------------------------------------------

   function "-" (V1, V2 : Vector_2D) return Vector_2D is
   begin
      return (V1 (1) - V2 (1), V1 (2) - V2 (2));
   end "-";

   --  ------------------------------------------------------------------------

   function "*" (Weight : float; V : Vector_2D) return Vector_2D is
   begin
      return (Weight * V (1), Weight * V (1));
   end "*";

   --  ------------------------------------------------------------------------

   function "*" (V1, V2 : Vector_2D) return Vector_2D is
   begin
      return (V1 (1) * V2 (1), V1 (2) * V2 (2));
   end "*";

   --  ------------------------------------------------------------------------

   function "+" (MV1, MV2 : Multivector) return Multivector is
      Sum : Multivector (MV1.MV_Size, MV1.Grade_Use) := MV1;
   begin
      Sum.Coordinates (1) := Sum.Coordinates (1) + MV2.Coordinates (1);
      Sum.Coordinates (2) := Sum.Coordinates (2) + MV2.Coordinates (2);
      return Sum;
   end "+";

   --  -------------------------------------------------------------------------

   function "-" (MV1, MV2 : Multivector) return Multivector is
      Difference : Multivector := MV1;
   begin
      Difference.Coordinates (1) := MV1.Coordinates (1) + MV2.Coordinates (1);
      Difference.Coordinates (2) := MV1.Coordinates (2) + MV2.Coordinates (2);
      return Difference;
   end "-";

   --  -------------------------------------------------------------------------

   function Bivector_String (BV : Bivector; Text : String := "") return String is
      --          num : GA_Maths.Fixed_4 := GA_Maths.Fixed_4 (BV.e1e2_Coord);
      --        BV_Coords : constant Coords_Continuous_Array (1 .. 1) := BV.e1e2_Coord;   --  m_c[4]
      --        BV_Size   : constant Integer := MV_Size (Integer (BV.Grade_Usage));
      --        MV        : Multivector (BV_Size);
      MV : Bivector_MV;
   begin
      --        MV.Grade_Use := BV.Grade_Usage;
      --        MV.Coordinates (1) := BV_Coords (1);
      MV.Coordinates := BV.Coordinates;
      return Multivector_String (MV, Text);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA.Bivector_String.");
         Put_Line (Exception_Information (anError));
         raise;
   end Bivector_String;

   --  -------------------------------------------------------------------------
   --  Construct_Vector corresponds to e2ga._vector
   function Construct_Vector (Coord : float) return Vector_MV is
      theVector : Vector_MV;
   begin
      theVector.Coordinates := (0.0, Coord);
      return theVector;
   end Construct_Vector;

   --  -------------------------------------------------------------------------

   function Construct_Vector (Coord_1, Coord_2 : float) return Vector_MV is
      theVector : Vector_MV;
   begin
      theVector.Coordinates := (Coord_1, Coord_2);
      return theVector;
   end Construct_Vector;

   --  -------------------------------------------------------------------------

   function Basis_Vector_Name (Index : GA_Maths.Basis_Index) return String is
   begin
      case Index is
         when 1 => return "e1";
         when 2 => return "e2";
      end case;
   end Basis_Vector_Name;

   --  -------------------------------------------------------------------------

   function Dot_Product (R1, R2 : Rotor) return float is
   begin
      return R1.Coordinates (1) * R2.Coordinates (1) +
        R1.Coordinates (2) * R2.Coordinates (2);
   end Dot_Product;

   --  ------------------------------------------------------------------------

   function Dual (MV : Multivector) return Multivector is
      Coords : Coords_Continuous_Array (1 .. 4) := (0.0, 0.0, 0.0, 0.0);
      use GA_Maths;
   begin
      if (MV.Grade_Use and 1) /= 0 then
         Coords (4) := -MV.Coordinates (1);
      end if;
      if (MV.Grade_Use and 2) /= 0 then
         Coords (2) := MV.Coordinates (3);
         Coords (3) := -MV.Coordinates (2);
      end if;
      if (MV.Grade_Use and 4) /= 0 then
         Coords (1) := MV.Coordinates (4);
      end if;
      return (MV.MV_Size, MV.Grade_Use, Coords);
   end Dual;

   --  -------------------------------------------------------------------------

   function e1 return Vector_2D is
   begin
      return e1_basis;
   end e1;

   --  -------------------------------------------------------------------------

   function e2 return Vector_2D is
   begin
      return e2_basis;
   end e2;

   --  -------------------------------------------------------------------------

   function E1_E2 (BV : Bivector_MV) return float is
   begin
      return BV.Coordinates (1);
   end E1_E2;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
      use Interfaces;
      use GA_Maths;
      Product : Multivector := MV1;
   begin
      --  m_Grade_Usage: Grade_Usage
      --  m_c[4]: Coords
      if (MV1.Grade_Use and Unsigned_Integer (1)) /= 0 then
         Product.Coordinates (1) := MV1.Coordinates (1) * MV2.Coordinates (1);
      end if;
      if (MV1.Grade_Use and Unsigned_Integer (2)) /= 0 then
         Product.Coordinates (2) := MV1.Coordinates (2) * MV2.Coordinates (1);
         Product.Coordinates (3) := MV1.Coordinates (3) * MV2.Coordinates (1);
      end if;
      if (MV1.Grade_Use and Unsigned_Integer (4)) /= 0 then
         Product.Coordinates (4) := MV1.Coordinates (3) * MV2.Coordinates (1);
      end if;
      return Product;
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV : Multivector; V : Vector_2D) return Multivector is
      use Interfaces;
      use GA_Maths;
      Product : Multivector := MV;
   begin
      --  m_Grade_Usage: Grade_Usage
      --  m_c[4]: Coords
      if (MV.Grade_Use and Unsigned_Integer (1)) /= 0 then
         Product.Coordinates (1) := MV.Coordinates (1) * V (1);
      end if;
      if (MV.Grade_Use and Unsigned_Integer (2)) /= 0 then
         Product.Coordinates (2) := MV.Coordinates (2) * V (1);
         Product.Coordinates (3) := MV.Coordinates (3) * V (1);
      end if;
      if (MV.Grade_Use and Unsigned_Integer (4)) /= 0 then
         Product.Coordinates (4) := MV.Coordinates (3) * V (1);
      end if;
      return Product;
   end Geometric_Product;

   --  ------------------------------------------------------------------------

   --      function Geometric_Product (BV1, BV2 : Bivector) return Multivector is
   --          MV2 : Multivector;
   --      begin
   --
   --          return MV2;
   --      end Geometric_Product;

   --  ------------------------------------------------------------------------

   --      function Geometric_Product (V1, V2 : Vector) return Multivector is
   --          MV2 : Multivector;
   --      begin
   --
   --          return MV2;
   --      end Geometric_Product;

   --  ------------------------------------------------------------------------

   function Get_Coords (BV : Bivector_MV) return Bivector_Coords is
   begin
      return BV.Coordinates;
   end Get_Coords;

   --  ----------------------------------------------------------------------------

   function Grade_Involution (MV : Multivector) return Multivector is
      use GA_Maths;
      MV1    : Multivector := MV;
   begin
      if (MV.Grade_Use and 2) /= 0 then
         MV1.Coordinates (2) := -MV.Coordinates (2);
         MV1.Coordinates (3) := -MV.Coordinates (3);
      end if;
      return MV1;
   end Grade_Involution;

   --  -------------------------------------------------------------------------

   function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer  is
   begin
      return MV.Grade_Use;
   end Grade_Use;

   --  ----------------------------------------------------------------------------

   function Init (MV : Multivector; Epsilon : float; Use_Algebra_Metric : Boolean;
                  GU_Count : Integer) return MV_Type is
      use GA_Maths;
      M_Type_Record      : MV_Type;  -- initialized to default values
      M_Type             : Multivector_Type_Base.Object_Type;
      GU                 : GA_Maths.Grade_Usage := MV.Grade_Use; --  Bit map indicating which grades are present
      MV_Reverse         : constant Multivector := Reverse_Multivector (MV);
      VI                 : constant Multivector := Inverse (MV);
      GI                 : constant Multivector := Grade_Involution (MV);
      Sq                 : Scalar_MV;
      --  V: versor; VI: versor inverse
      --  GI: Grade involution
      GI_VI              : constant Multivector := Geometric_Product (GI, VI);
      VI_GI              : constant Multivector := Geometric_Product (VI, GI);
      VI_E1              : constant Multivector := Geometric_Product (VI, e1);
      VI_E2              : constant Multivector := Geometric_Product (VI, e2);
      GI_VI_E1           : constant Multivector := Geometric_Product (GI_VI, e1);
      VI_GI_E1           : constant Multivector := Geometric_Product (VI_GI, e1);
      VI_GI_GI_VI        : constant Multivector := VI_GI - GI_VI;
      VI_E1_GI           : constant Multivector := Geometric_Product (VI_E1, GI);
      VI_E2_GI           : constant Multivector := Geometric_Product (VI_E2, GI);

   begin
      Sq := Scalar_Product (MV, MV_Reverse);
      if Sq.Coordinates (1) /= 0.0 and then
         GI.Grade_Use = Grade_0 and then
         VI_GI_GI_VI.Grade_Use = Grade_0 and then
         VI_E1_GI.Grade_Use = Grade_1 and then
         VI_E2_GI.Grade_Use = Grade_1 and then
         GU_Count = 1 then
         M_Type := Multivector_Type_Base.Blade_Object;
      else
         M_Type := Multivector_Type_Base.Versor_Object;
      end if;
      --  Set_M_Type (Base : in out Type_Base; theType : Object_Type);
      M_Type_Record.M_Type := M_Type;
      return M_Type_Record;
   end Init;

   --  -------------------------------------------------------------------------
   --  Initialize MV_Type corresponds to e2ga void mvtype::init
   --  which is called by e2ga::mvType constructor
   --  M_Zero        : boolean := False; -- True if multivector is zero
   --  M_Type        : Object_Type := Multivector_Object;
   --  M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
   --  M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
   --  M_Parity      : Parity
   function Init (MV : Multivector; Epsilon : float) return MV_Type is
      use Interfaces;
      use GA_Maths;
      use  Multivector_Type_Base;
      Base               : Type_Base;
      GU                 : GA_Maths.Grade_Usage := MV.Grade_Use;
      Count              : array (1 .. 2) of Integer := (0, 0);
      Count_Index        : Integer := 0;
      Index              : Integer;
      Type_Record        : MV_Type;
      US_1               : constant Unsigned_32 := Unsigned_32 (1);
      Done               : Boolean := False;

   begin
--        Set_Grade_Usage (Base, GU);
      Base.M_GU := GU;
      --  count grade part usage
      while GU /= 0 loop
         if Shift_Left (US_1, Natural (GU)) /= 0 then
            Index := Integer (Shift_Left (US_1, Count_Index));
            Count (Index) := Count (Index) + 1;
         end if;
         GU := Unsigned_Integer (Shift_Right (Unsigned_32 (GU), 1));
--           Set_Top_Grade (Base, Count_Index);
         Base.M_Top_Grade := Count_Index;
         Count_Index := Count_Index + 1;
      end loop;
      --  if no grade part in use: zero blade
      if Count (1) = 0 and then Count (2) = 0  then  --  this is a zero blade
         Set_Type_Base (Base, True, Blade_Object, 0, GU, Even_Parity);
         Done := True;
      else
         --  Base.M_Zero = False by default
         if Count (1) /= 0 and then Count (2) /= 0  then
            --  Base.M_Parity = No_Parity by default
            Done := True;
         else
            if Count (1) = 0 then
--                 Set_Parity (Base, Even_Parity);
              Base.M_Parity := Even_Parity;
            else
--                 Set_Parity (Base, Odd_Parity);
               Base.M_Parity := Odd_Parity;
            end if;
            Type_Record := Init (MV, Epsilon, True, Count (1) + Count (2)) ;
         end if;
      end if;

      return Type_Record;
   end Init;

   --  -------------------------------------------------------------------------

   function Inverse (MV : Multivector) return Multivector is
      use GA_Maths;
      N : Scalar_MV;
   begin
      if (MV.Grade_Use and 1) /= 0 then
         N.Coordinates (1) := MV.Coordinates (1) * MV.Coordinates (1);
      end if;
      if (MV.Grade_Use and 2) /= 0 then
         N.Coordinates (1) := N.Coordinates (1) + MV.Coordinates (2) * MV.Coordinates (2) +
           MV.Coordinates (3) * MV.Coordinates (3);
      end if;
      if (MV.Grade_Use and 4) /= 0 then
         N.Coordinates (1) := N.Coordinates (1) + MV.Coordinates (4) * MV.Coordinates (4);
      end if;
      return N;
   end Inverse;

   --  -------------------------------------------------------------------------

   --     function  Get_MV_Type (X : Multivector; Epsilon : float)
   --                            return Multivector_Type_Base.M_Type_Type is
   --     begin
   --        return Multivector_Analyze.Get_Multivector_Type (X, Epsilon);
   --     end Get_MV_Type;

   --  -------------------------------------------------------------------------
   --  Implements e2ga.cpp (line 1524)  char *string(const mv & obj, char *str,
   --  int maxLength, const char *fp /* = NULL */)
   --  goal: print [+|-] MV.Coord (k) (* Basis_Vector_1 ^ ... ^ Basis_Vector_N)
   --  theString := theString & float'Image (Abs (Coordinate));
   function Multivector_String (MV : Multivector; Text : String := "") return String is
      use Interfaces;
      use Ada.Strings.Unbounded;
      String_Start     : Unbounded_String := To_Unbounded_String ("");
      String_End       : Unbounded_String := To_Unbounded_String ("");
      Value            : float_3;
      theString        : Unbounded_String := String_Start;
      Basis_Index      : Integer range 1 .. 4 := 1;  --  ia
      Coord_Index      : Integer := 1;  --  k
      Basis_Elem_Index : Integer;       --  bei
      Name_Index       : Integer range 1 .. 2;
      Coordinate       : float;
   begin
      theString := theString & Text;
      --  Print all coordinates (x, y, z)

      --  Loop on coordinate
      --    Loop on grade
      --      Loop on  number of coordinates in the grade part
      --        Print sign and coordinate value
      --        Loop on basis symbols
      --          Print symbol
      --          If not last symbol print ^
      for Grade in GA_Maths.Grade_Index'Range loop  --  i
         if  Unsigned_32 (MV.Grade_Use) /=
           Shift_Left (Unsigned_32 (Grade), 1) then
            if Basis_Index - MV_Grade_Size (Grade) > 0 then
               Basis_Index := Basis_Index + MV_Grade_Size (Grade);
            end if;
         else
            --  int mv_gradeSize[3] = {1, 2, 1 };
            --  Bivector grade size is 2?
            --  Loop on grade
            for Grade_Index in Integer range 1 .. MV_Grade_Size (Grade) loop  --  j
               --  Print sign and coordinate value
               Coordinate := MV_Basis_Element_Sign_By_Index (Basis_Index) *
               Abs (MV.Coordinates (Coord_Index));
               Value := float_3 (Coordinate);
               theString := theString & float_3'Image (Value);
               if Grade /= 1 then  --  Not grade 0
                  --  print [* basisVector1 ^ ... ^ basisVectorN]
                  theString := theString & " * ";
                  --  Loop on the basis vector symbols
                  --  MV_Basis_Elements : array (1 .. 4, 1 .. 3) of integer :=
                  --        ((-1, 0, 0), (0, -1, 0), (1, -1, 0), (0, 1, -1));

                  Basis_Elem_Index := 1;  --  bei
                  while MV_Basis_Elements (Basis_Index, Basis_Elem_Index) >= 0 loop
                     if Basis_Elem_Index /= 1 then
                        theString := theString & "^";
                     end if;
                     Name_Index := MV_Basis_Elements (Basis_Index, Basis_Elem_Index) + 1;
                     theString := theString & MV_Basis_Vector_Names (Name_Index);
                     Basis_Elem_Index := Basis_Elem_Index + 1;
                  end loop;  --  MV_Basis_Elements
                  Basis_Index := Basis_Index + 1;
               end if;  --  Grade
            end loop;  --  Grade_Index
         end if;  --  MV.Grade_Use
         if Grade >= 2 then
            Coord_Index := Coord_Index + 1;
            Basis_Index := Basis_Index + 1;
         end if;
      end loop;   --  Grade

      return To_String (theString & String_End);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA.Multivector_String.");
         raise;
   end Multivector_String;

   --  -------------------------------------------------------------------------

   function Norm_E2 (V2 : Vector_2D) return Scalar_MV is
      Norm : Scalar_MV;
   begin
      Norm.Coordinates (1) := (V2 (1) * V2 (1) + V2 (2) * V2 (2));
      return Norm ;
   end Norm_E2;

   --  ----------------------------------------------------------------------------

   function Norm_E (MV : Multivector) return Scalar_MV is
      use GA_Maths;
      Value : float := 0.0;
      Norm  : Scalar_MV;
   begin
      if (MV.Grade_Use and 1) /= 0 then
         Value := MV.Coordinates (1) * MV.Coordinates (1);
      end if;
      if (MV.Grade_Use and 2) /= 0 then
         Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
           MV.Coordinates (3) * MV.Coordinates (3);
      end if;
      if (MV.Grade_Use and 4) /= 0 then
         Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
      end if;
      Norm.Coordinates (1) := Value;
      return Norm;
   end Norm_E;

   --  ----------------------------------------------------------------------------

   function Norm_E2 (MV : Multivector) return Scalar_MV is
      use GA_Maths;
      Value : float := 0.0;
      Norm  : Scalar_MV;
   begin
      if (MV.Grade_Use and 1) /= 0 then
         Value := MV.Coordinates (1) * MV.Coordinates (1);
      end if;
      if (MV.Grade_Use and 2) /= 0 then
         Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
           MV.Coordinates (3) * MV.Coordinates (3);
      end if;
      if (MV.Grade_Use and 4) /= 0 then
         Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
      end if;

      Norm.Coordinates (1) := Value;
      return Norm;
   end Norm_E2;

   --  ----------------------------------------------------------------------------

   function Largest_Basis_Blade (Map : Bit_Map) return float is
   begin
      return 0.0;
   end Largest_Basis_Blade;

   --  ----------------------------------------------------------------------------

   function Largest_Coordinate return float is
   begin
      return 0.0;
   end Largest_Coordinate;

   --  ------------------------------------------------------------------------

   function Left_Contraction (V1, V2 : Vector_2D) return Scalar_MV is
      LC  : Scalar_MV;
   begin
      LC.Coordinates (1) := Dot_Product (V1, V2);
      return LC;
   end Left_Contraction;

   --  ------------------------------------------------------------------------

   function Left_Contraction (V : Vector_MV; BV : Bivector_MV) return Vector_MV is
      use GA_Maths;
      Vout        : Vector_MV;
   begin
      if (BV.Grade_Use and 1) /= 0 then
         Vout.Coordinates (1) := V.Coordinates (1) * BV.Coordinates (1);
      end if;
      if (BV.Grade_Use and 2) /= 0 then
         Vout.Coordinates (2) :=  V.Coordinates (2) * BV.Coordinates (1);
      end if;
      return Vout;
   end Left_Contraction;

   --  ------------------------------------------------------------------------

   function Outer_Product (V1, V2 : Vector_2D) return Bivector is
      --  The outer product basis in 2D is the coordinate of e1^e2.
      use E2GA;
      BV        : Bivector;
   begin
      BV.Coordinates (1) :=
        GA_Maths.Float_Array_Package.Determinant (((V1 (1), V2 (1)), (V1 (2), V2 (2))));
      return BV;
   end Outer_Product;

   --  ------------------------------------------------------------------------

   function Reverse_Multivector (MV : Multivector) return Multivector is
      use GA_Maths;
      MV_R : Multivector := MV;
      Coords_8 : Coords_Continuous_Array (1 .. 4) := (others => 0.0);
   begin
      if (MV.Grade_Use and 1) /= 0 then
         Coords_8 (1) := MV.Coordinates (1);
      end if;
      if (MV.Grade_Use and 2) /= 0 then
         Coords_8 (2) := MV.Coordinates (2);
         Coords_8 (3) := MV.Coordinates (3);
      end if;
      if (MV.Grade_Use and 4) /= 0 then
         Coords_8 (4) := - MV.Coordinates (4);
      end if;
      MV_R.Coordinates := Coords_8;
      return MV_R;
   end Reverse_Multivector;

   --  ------------------------------------------------------------------------

   function Scalar_Product (V1, V2 : Vector_2D) return Scalar_MV is
      Product : Scalar_MV;
   begin
      Product.Coordinates (1) := Dot_Product (V1, V2);
      return  Product;
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   function Scalar_Product (MV1, MV2 : Multivector) return Scalar is
      use GA_Maths;
      Product : float := 0.0;
   begin
      if (MV2.Grade_Use and 1) /= 0 and (MV1.Grade_Use and 1) /= 0 then
         Product :=  MV1.Coordinates (1) * MV2.Coordinates (1);
      end if;

      if (MV2.Grade_Use and 2) /= 0 and (MV1.Grade_Use and 2) /= 0 then
         Product :=  Product + MV1.Coordinates (2) * MV2.Coordinates (2);
      end if;

      if (MV2.Grade_Use and 4) /= 0 and (MV1.Grade_Use and 4) /= 0 then
         Product :=  Product - MV1.Coordinates (3) * MV2.Coordinates (3);
      end if;
      return  Scalar (Product);
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   function Scalar_Product (MV1, MV2 : Multivector) return Scalar_MV is
      use GA_Maths;
      Product : float := 0.0;
      SMV     : Scalar_MV;
   begin
      if (MV2.Grade_Use and 1) /= 0 and (MV1.Grade_Use and 1) /= 0 then
         Product :=  MV1.Coordinates (1) * MV2.Coordinates (1);
      end if;

      if (MV2.Grade_Use and 2) /= 0 and (MV1.Grade_Use and 2) /= 0 then
         Product :=  Product + MV1.Coordinates (2) * MV2.Coordinates (2);
      end if;

      if (MV2.Grade_Use and 4) /= 0 and (MV1.Grade_Use and 4) /= 0 then
         Product :=  Product - MV1.Coordinates (3) * MV2.Coordinates (3);
      end if;
      SMV.Coordinates (1) := Product;
      return  SMV;
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   function Set_Bivector (V1, V2 : Vector_2D) return Bivector is
   begin
      return  Outer_Product (V1, V2);
   end Set_Bivector;

   --  -------------------------------------------------------------------------

   function Set_Rotor (E1_E2 : float) return Rotor is
      theRotor : Rotor;
   begin
      theRotor.Coordinates := (E1_E2, 0.0);
      return theRotor;
   end Set_Rotor;

   --  -------------------------------------------------------------------------

   function Set_Vector (V1 : Vector_2D) return Vector_MV is
      theVector : Vector_MV;
   begin
      theVector.Coordinates (1) := V1 (1);
      theVector.Coordinates (2) := V1 (2);
      return theVector;
   end Set_Vector;

   --  -------------------------------------------------------------------------

   function Unit_E (V : Vector_MV) return Vector_MV is
      E2_Value   : float;
      IE_Value   : float;
      V1         : float := V.Coordinates (1);
      V2         : float := V.Coordinates (2);
      e21        : float;
      Result     : Vector_MV := V;
   begin
      e21 := e2 (1);
      E2_Value := V1 * V1 + V2 * V2;
      IE_Value := 1.0 / GA_Maths.Float_Functions.Sqrt (e21);
      Result.Coordinates (1) := V1 * IE_Value;
      Result.Coordinates (2) := V2 * IE_Value;
      return  Result;
   end Unit_E;

   --  ----------------------------------------------------------------------------

   function Dot_Product (V1, V2 : Vector_2D) return float is
   begin
      return V1 (1) * V2 (1) + V1 (2) * V2 (2);
   end Dot_Product;

   --  ------------------------------------------------------------------------

   function Get_Coords (V : Vector_2D) return GA_Maths.Array_2D is
   begin
      return (V (1), V (2));
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coord_1 (V : Vector_2D) return float is
   begin
      return V (1);
   end Get_Coord_1;

   --  ------------------------------------------------------------------------

   function Get_Coord_2 (V : Vector_2D) return float is
   begin
      return V (2);
   end Get_Coord_2;

   --  ------------------------------------------------------------------------

   function Magnitude (V : Vector_2D) return float is
   begin
      return GA_Maths.Float_Functions.Sqrt (V (1) * V (1) + V (2) * V (2));
   end Magnitude;

   --  ------------------------------------------------------------------------

   procedure Set_Coords (V : out Vector_2D; C1, C2 : float) is
   begin
      V := (C1, C2);
   end Set_Coords;

   --  ------------------------------------------------------------------------

end E2GA;
