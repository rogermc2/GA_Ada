
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Multivector_Analysis;
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

   e1_basis : GA_Maths.Array_2D := (1.0, 0.0);
   e2_basis : GA_Maths.Array_2D := (0.0, 1.0);

   --  -------------------------------------------------------------------------

   function "+" (MV1, MV2 : Multivector) return Multivector is
      Sum : Multivector (MV1.MV_Size) := MV1;
   begin
      Sum.Coordinates (1) := Sum.Coordinates (1) + MV2.Coordinates (1);
      Sum.Coordinates (2) := Sum.Coordinates (2) + MV2.Coordinates (2);
      return Sum;
   end "+";

   --  -------------------------------------------------------------------------

   function Bivector_String (BV : Bivector; Text : String := "") return String is
      --          num : GA_Maths.Fixed_4 := GA_Maths.Fixed_4 (BV.e1e2_Coord);
      --        BV_Coords : constant Coords_Continuous_Array (1 .. 1) := BV.e1e2_Coord;   --  m_c[4]
      --        BV_Size   : constant Integer := MV_Size (Integer (BV.Grade_Usage));
      --        MV        : Multivector (BV_Size);
   begin
      --        MV.Grade_Use := BV.Grade_Usage;
      --        MV.Coordinates (1) := BV_Coords (1);
      return Multivector_String (BV, Text);
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA.Bivector_String.");
         Put_Line (Exception_Information (anError));
         raise;
   end Bivector_String;

   --  -------------------------------------------------------------------------
   --  Construct_Vector corresponds to e2ga._vector
   function Construct_Vector (Coord : float) return Vector is
      theVector : Vector;
   begin
      theVector.Grade_Use := 0;
      theVector.Coordinates := (0.0, Coord);
      return theVector;
   end Construct_Vector;

   --  ----------------------------------------------------------------------------

   function Construct_Vector (Coord_1, Coord_2 : float) return Vector is
      theVector : Vector;
   begin
      theVector.Grade_Use := 0;
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

   --  ----------------------------------------------------------------------------

   function Dot_Product (R1, R2 : Rotor) return float is
   begin
      return R1.M_C1 * R2.M_C1 + R1.M_C2 * R2.M_C2;
   end Dot_Product;

   --  ------------------------------------------------------------------------

   function Dot_Product (V1, V2 : Vector) return float is
   begin
      return V1.Coordinates (1) * V2.Coordinates (1) +
        V1.Coordinates (2) * V2.Coordinates (2);
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

   --  ----------------------------------------------------------------------------

   function e1 return GA_Maths.Vector_2D is
      theVector : GA_Maths.Vector_2D;
   begin
      GA_Maths.Set_Coords (theVector, e1_basis (1), e1_basis (2));
      return theVector;
   end e1;

   --  ----------------------------------------------------------------------------

   function e2 return GA_Maths.Vector_2D is
      theVector : GA_Maths.Vector_2D;
   begin
      GA_Maths.Set_Coords (theVector, e2_basis (1), e2_basis (2));
      return theVector;
   end e2;

   --  ----------------------------------------------------------------------------

   --     function  Get_MV_Type (X : Multivector; Epsilon : float)
   --                            return Multivector_Type_Base.M_Type_Type is
   --     begin
   --        return Multivector_Analysis.Get_Multivector_Type (X, Epsilon);
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
      New_Line;
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

   --     function Norm_E2 (BV : Bivector) return Scalar is
   --     begin
   --        return Scalar (BV.e1e2_Coord (1) * BV.e1e2_Coord (1));
   --     end Norm_E2;

   --  ----------------------------------------------------------------------------

   function Norm_E2 (V2 : GA_Maths.Vector_2D) return GA_Maths.Scalar is
      V21 : float := GA_Maths.Get_Coord_1 (V2);
      V22 : float := GA_Maths.Get_Coord_2 (V2);
   begin
      return GA_Maths.Scalar (V21 * V21 + V22 * V22);
   end Norm_E2;

   --  ----------------------------------------------------------------------------

   function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer  is
   begin
      return MV.Grade_Use;
   end Grade_Use;

   --  ----------------------------------------------------------------------------

   function Norm_E (MV : Multivector) return GA_Maths.Scalar is
      use GA_Maths;
      Value : float := 0.0;
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

      return Scalar (Value);
   end Norm_E;

   --  ----------------------------------------------------------------------------

   function Norm_E2 (MV : Multivector) return GA_Maths.Scalar is
      use GA_Maths;
      Value : float := 0.0;
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

      return Scalar (Value);
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
   --  Expand is not for external use. It decompresses the coordinates stored in this
   function  Expand (MV : Multivector; Nulls : boolean := True)
                     return GA_Maths.Array_3D is
      --          C_M_C : float;
      --          Null_Float : float;
   begin
      return (MV.Coordinates (1), MV.Coordinates (2), MV.Coordinates (3));
   end Expand;

   --  ------------------------------------------------------------------------

   function Left_Contraction (V1, V2 : Vector) return GA_Maths.Scalar is
   begin
      return GA_Maths.Scalar (Dot_Product (V1, V2));
   end Left_Contraction;

   --  ------------------------------------------------------------------------

   function Left_Contraction (V : Vector; BV : Bivector) return Vector is
      use GA_Maths;
      Vout        : Vector;
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

   function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
      use Interfaces;
      use GA_Maths;
      Coords     : Coord4_Array := (0.0, 0.0, 0.0, 0.0);
      MV1_Expand : Array_3D := Expand (MV1, True);
      MV2_Expand : Array_3D := Expand (MV2, True);
   begin
      --  m_Grade_Usage: Grade_Usage
      --  m_c[4]: Coords
      --  Expand decompresses the coordinates stored in a multivector
      if (MV1.Grade_Use and Unsigned_Integer (1)) /= 0 then
         Coords (1) := Coords (1) + MV1_Expand (1) * MV2_Expand (1);
      end if;
      if (MV1.Grade_Use and Unsigned_Integer (2)) /= 0 then
         Coords (2) := Coords (2) + MV1_Expand (2) * MV2_Expand (1);
         Coords (3) := Coords (3) + MV1_Expand (2) * MV2_Expand (1);
      end if;

      return MV2;
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

   function Outer_Product (V1, V2 : Vector) return Bivector is
      --  The outer product basis in 2D is the coordinate of e1^e2.
      use GA_Maths;
      BV        : Bivector;
      V11       : constant float := V1.Coordinates (1);
      V12       : constant float := V1.Coordinates (2);
      V21       : constant float := V2.Coordinates (1);
      V22       : constant float := V2.Coordinates (2);
   begin
      BV.Coordinates (1) :=
        Float_Array_Package.Determinant (((V11, V21), (V12, V22)));
      return BV;
   end Outer_Product;

   --  ------------------------------------------------------------------------

   function Scalar_Product (V1, V2 : Vector) return GA_Maths.Scalar is
   begin
      return  GA_Maths.Scalar (Dot_Product (V1, V2));
   end Scalar_Product;

   --  ----------------------------------------------------------------------------

   function Set_Bivector (V1, V2 : Vector) return Bivector is
   begin
      return  Outer_Product (V1, V2);
   end Set_Bivector;

   --  ----------------------------------------------------------------------------

   function Set_Rotor (E1_E2 : float) return Rotor is
   begin
      return  (E1_E2, 0.0);
   end Set_Rotor;

   --  ----------------------------------------------------------------------------

   function Set_Vector (V1 : GA_Maths.Vector_2D) return Vector is
      theVector : Vector;
   begin
      theVector.Grade_Use := 1;
      theVector.Coordinates (1) := GA_Maths.Get_Coord_1 (V1);
      theVector.Coordinates (2) := GA_Maths.Get_Coord_2 (V1);
      return theVector;
   end Set_Vector;

   --  ----------------------------------------------------------------------------
   function Unit_E (V : Vector) return Vector is
      E2_Value   : float;
      IE_Value   : float;
      V1         : float := V.Coordinates (1);
      V2         : float := V.Coordinates (2);
      e21        : float;
      Result     : Vector := V;
   begin
      e21 := GA_Maths.Get_Coord_1 (e2);
      E2_Value := V1 * V1 + V2 * V2;
      IE_Value := 1.0 / GA_Maths.Float_Functions.Sqrt (e21);
      Result.Coordinates (1) := V1 * IE_Value;
      Result.Coordinates (2) := V2 * IE_Value;
      return  Result;
   end Unit_E;

   --  ----------------------------------------------------------------------------

end E2GA;
