
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Multivector_Analyze;
with Multivector_Type_Base;
with E3GA_Utilities;

package body E2GA is

   type Basis_Name is (E1b, E2b);
   type E2_Base is (e1_b, e2_b);

   type Array_BM4 is array (Bit_Map range 1 .. 4) of integer;
   type Array_F4 is array (1 .. 4) of float;
   type float_3 is digits 3;

   MV_Space_Dimension                : constant integer := 2;
   MV_Metric_Euclidean               : constant boolean := True;
   --  MV_Size is a lookup table for the number of coordinates based on a grade usage bitmap
   MV_Size                           : constant array (0 .. 7) of integer := (0, 1, 2, 3, 1, 2, 3, 4);
   --  MV_Grade_Size is a lookup table for the number of coordinates
   --  in the grade part of a general multivector
   MV_Grade_Size                     : constant GA_Maths.Grade_Array :=
     (1, 2, 1);
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
--     MV_Basis_Vector_Names             : constant array (1 .. 2) of string (1 .. 2) := ("e1", "e2");
   MV_Basis_Vector_Names             : Blade.Basis_Vector_Names;

--     e1_basis : Vector_Coords := (1.0, 0.0);
--     e2_basis : Vector_Coords := (0.0, 1.0);
   e1_basis : Bivector;
   e2_basis : Bivector;

   --  -------------------------------------------------------------------------

--     function Init (MV : Multivector; Epsilon : float;
--                    Use_Algebra_Metric : Boolean; GU_Count : Integer) return MV_Type;
--     function Inverse (MV : Multivector) return Multivector;
--     function Scalar_Product (MV1, MV2 : Multivector) return Scalar;
--     function Reverse_Multivector (MV : Multivector) return Multivector;

   --  -------------------------------------------------------------------------

   function "+" (V1, V2 : Vector) return Vector is
      use Multivector.Blade_List_Package;
      use Blade;
      Blades_1  : Multivector.Blade_List := Multivector.Get_Blade_List (V1);
      Blades_2  : Multivector.Blade_List := Multivector.Get_Blade_List (V2);
      C11       : constant float := Weight (Blades_1.First_Element);
      C12       : constant float := Weight (Blades_1.Last_Element);
      C21       : constant float := Weight (Blades_2.First_Element);
      C22       : constant float := Weight (Blades_2.Last_Element);
      Sum : Vector;
   begin
      Multivector.Add_Blade (Sum, Blade.New_Basis_Blade (Blade.E2_e1, C11 + C21));
      Multivector.Add_Blade (Sum, Blade.New_Basis_Blade (Blade.E2_e2, C12 + C22));
      return Sum;
   end "+";

   --  ------------------------------------------------------------------------

--     function "-" (V1, V2 : Vector) return Vector is
--        Diff : Vector;
--     begin
--        Diff.Coordinates (1) := V1.Coordinates (1) - V2.Coordinates (1);
--        Diff.Coordinates (2) := V1.Coordinates (2) - V2.Coordinates (2);
--        return Diff;
--     end "-";

   --  ------------------------------------------------------------------------

--     function "*" (Weight : float; V : Vector) return Vector is
--        Product : Vector;
--     begin
--        Product.Coordinates (1) := Weight * V.Coordinates (1);
--        Product.Coordinates (2) := Weight * V.Coordinates (2);
--        return Product;
--     end "*";

   --  ------------------------------------------------------------------------

--     function "*" (V1, V2 : Vector) return Vector is
--        Product : Vector;
--     begin
--        Product.Coordinates (1) := V1.Coordinates (1) * V2.Coordinates (1);
--        Product.Coordinates (2) := V1.Coordinates (2) * V2.Coordinates (2);
--        return Product;
--     end "*";

   --  ------------------------------------------------------------------------

--     function "+" (MV1, MV2 : Multivector) return Multivector is
--        Sum : Multivector (MV1.Grade_Use) := MV1;
--     begin
--        Sum.Coordinates (1) := Sum.Coordinates (1) + MV2.Coordinates (1);
--        Sum.Coordinates (2) := Sum.Coordinates (2) + MV2.Coordinates (2);
--        return Sum;
--     end "+";

   --  -------------------------------------------------------------------------

--     function "-" (MV1, MV2 : Multivector) return Multivector is
--        Difference : Multivector := MV1;
--     begin
--        Difference.Coordinates (1) := MV1.Coordinates (1) + MV2.Coordinates (1);
--        Difference.Coordinates (2) := MV1.Coordinates (2) + MV2.Coordinates (2);
--        return Difference;
--     end "-";

   --  -------------------------------------------------------------------------

--     function Bivector_String (BV : Bivector; Text : String := "") return String is
   function Bivector_String (BV : Bivector) return String is
      use Ada.Strings.Unbounded;
      MV : Multivector.Multivector := BV;
--        MV : Multivector := Set_Multivector (BV);
   begin
      return To_String (Multivector.Multivector_String
                        (MV, MV_Basis_Vector_Names));
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in E2GA.Bivector_String.");
         Put_Line (Exception_Information (anError));
         raise;
   end Bivector_String;

   --  -------------------------------------------------------------------------

--     function Construct_Vector (Coord_1, Coord_2 : float) return Vector is
--        theVector : Vector;
--     begin
--        theVector.Coordinates (1) := Coord_1;
--        theVector.Coordinates (2) := Coord_2;
--        return theVector;
--     end Construct_Vector;

   --  -------------------------------------------------------------------------

--     function Basis_Vector_Name (Index : GA_Maths.Basis_Index) return String is
--     begin
--        case Index is
--           when 1 => return "e1";
--           when 2 => return "e2";
--        end case;
--     end Basis_Vector_Name;

   --  -------------------------------------------------------------------------

--     function Dot_Product (V1, V2 : Vector) return float is
--     begin
--        return V1.Coordinates (1) * V2.Coordinates (1) +
--          V1.Coordinates (2) * V2.Coordinates (2);
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Dot_Product.");
--           raise;
--     end Dot_Product;

   --  ------------------------------------------------------------------------

   function Dot_Product (R1, R2 : Rotor) return float is
   begin
      return R1.Coordinates (1) * R2.Coordinates (1) +
        R1.Coordinates (2) * R2.Coordinates (2);
   end Dot_Product;

   --  ------------------------------------------------------------------------

--     function Dual (MV : Multivector) return Multivector is
--        use GA_Maths;
--        Coords : Coords_Continuous_Array (1 .. 4) := (0.0, 0.0, 0.0, 0.0);
--        Info   : MV_Type;
--     begin
--        if (MV.Grade_Use and 1) /= 0 then
--           Coords (4) := -MV.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and 2) /= 0 then
--           Coords (2) := MV.Coordinates (3);
--           Coords (3) := -MV.Coordinates (2);
--        end if;
--        if (MV.Grade_Use and 4) /= 0 then
--           Coords (1) := MV.Coordinates (4);
--        end if;
--        return (MV.Grade_Use, Coords);
--     end Dual;

   --  -------------------------------------------------------------------------

   function e1 return Vector is
      use Multivector.Blade_List_Package;
      use Blade;
      Basis   : Vector;
   begin
      Multivector.Add_Blade (Basis, New_Basis_Blade (E2_e1));
      return Basis;
   end e1;

   --  -------------------------------------------------------------------------

   function e2 return Vector is
      use Multivector.Blade_List_Package;
      use Blade;
      Basis : Vector;
   begin
      Multivector.Add_Blade (Basis, New_Basis_Blade (E2_e2));
      return Basis;
   end e2;

   --  -------------------------------------------------------------------------

   function E1_E2 (BV : Bivector) return float is
      use Multivector.Blade_List_Package;
      Blades  : constant Multivector.Blade_List := Multivector.Get_Blade_List (BV);
   begin
      return Blade.Weight (Element (Blades.First));
   end E1_E2;

   --  -------------------------------------------------------------------------

--     function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
--        use Interfaces;
--        use GA_Maths;
--        Product : Multivector := MV1;
--     begin
--        --  m_Grade_Usage: Grade_Usage
--        --  m_c[4]: Coords
--        if (MV1.Grade_Use and Unsigned_Integer (1)) /= 0 then
--           Product.Coordinates (1) := MV1.Coordinates (1) * MV2.Coordinates (1);
--        end if;
--        if (MV1.Grade_Use and Unsigned_Integer (2)) /= 0 then
--           Product.Coordinates (2) := MV1.Coordinates (2) * MV2.Coordinates (1);
--           Product.Coordinates (3) := MV1.Coordinates (3) * MV2.Coordinates (1);
--        end if;
--        if (MV1.Grade_Use and Unsigned_Integer (4)) /= 0 then
--           Product.Coordinates (4) := MV1.Coordinates (3) * MV2.Coordinates (1);
--        end if;
--        return Product;
--     end Geometric_Product;

   --  -------------------------------------------------------------------------

--     function Geometric_Product (MV : Multivector; V : Vector) return Multivector is
--        use Interfaces;
--        use GA_Maths;
--        Product : Multivector := MV;
--     begin
--        --  m_Grade_Usage: Grade_Usage
--        --  m_c[4]: Coords
--        if (MV.Grade_Use and Unsigned_Integer (1)) /= 0 then
--           Product.Coordinates (1) := MV.Coordinates (1) * V.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and Unsigned_Integer (2)) /= 0 then
--           Product.Coordinates (2) := MV.Coordinates (2) * V.Coordinates (1);
--           Product.Coordinates (3) := MV.Coordinates (3) * V.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and Unsigned_Integer (4)) /= 0 then
--           Product.Coordinates (4) := MV.Coordinates (3) * V.Coordinates (1);
--        end if;
--        return Product;
--     end Geometric_Product;

   --  ------------------------------------------------------------------------

--     function Get_Basis_Vector_Names return Blade.Basis_Vector_Names is
--     begin
--        return MV_Basis_Vector_Names;
--     end Get_Basis_Vector_Names;

   --  ------------------------------------------------------------------------
   function Get_Coord (BV : Bivector) return float is
   begin
      return E1_E2 (BV);
   end Get_Coord;

   --  ------------------------------------------------------------------------

   function Get_Coord (S : Scalar) return float is
   begin
      return S.Coordinates (1);
   end Get_Coord;

   --  ------------------------------------------------------------------------

--     function Get_Coords (BV : Bivector) return GA_Maths.Bivector_Coords is
--     begin
--        return BV.Coordinates;
--     end Get_Coords;

   --  -------------------------------------------------------------------------

--     function Get_Coords (V : Vector) return GA_Maths.Array_2D is
--     begin
--        return (V.Coordinates (1), V.Coordinates (2));
--     end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coord_1 (R : Rotor) return float is
   begin
      return R.Coordinates (1);
   end Get_Coord_1;

   --  ------------------------------------------------------------------------

   function Get_Coord_2 (R : Rotor) return float is
   begin
      return R.Coordinates (2);
   end Get_Coord_2;

   --  ------------------------------------------------------------------------

   function Get_Coord_1 (V : Vector) return float is
      use Multivector.Blade_List_Package;
      Blades : Multivector.Blade_List := Multivector.Get_Blade_List (V);
   begin
      return Blade.Weight (Blades.First_Element);
   end Get_Coord_1;

   --  ------------------------------------------------------------------------

   function Get_Coord_2 (V : Vector) return float is
      use Multivector.Blade_List_Package;
      Blades : Multivector.Blade_List := Multivector.Get_Blade_List (V);
   begin
      return Blade.Weight (Blades.Last_Element);
   end Get_Coord_2;

   --  ------------------------------------------------------------------------

--     function Get_Coords (MV : Multivector) return GA_Maths.Coords_Continuous_Array is
--     begin
--        return MV.Coordinates;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Get_Coords MV.");
--           raise;
--     end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coords (V : Vector) return GA_Maths.Array_F2 is
      use Multivector.Blade_List_Package;
      use Blade;
      Blades   : Multivector.Blade_List := Multivector.Get_Blade_List (V);
   begin
      return (Weight (Blades.First_Element), Weight (Blades.Last_Element));
   end Get_Coords;

   --  ------------------------------------------------------------------------

--     function Get_Size (MV : Multivector) return Integer is
--     begin
--        return  MV_Size (integer (MV.Grade_Use));
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Get_Size.");
--           raise;
--     end Get_Size;

   --  ------------------------------------------------------------------------

--     function Grade_Involution (MV : Multivector) return Multivector is
--        use GA_Maths;
--        MV1    : Multivector := MV;
--     begin
--        if (MV.Grade_Use and 2) /= 0 then
--           MV1.Coordinates (2) := -MV.Coordinates (2);
--           MV1.Coordinates (3) := -MV.Coordinates (3);
--        end if;
--        return MV1;
--     end Grade_Involution;

   --  -------------------------------------------------------------------------

--     function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer  is
--     begin
--        return MV.Grade_Use;
--     end Grade_Use;

   --  ----------------------------------------------------------------------------

--     function Init (MV : Multivector; Epsilon : float := 0.0) return MV_Type is
--        use Interfaces;
--        use GA_Maths;
--        use  Multivector_Type_Base;
--        MV_Info            : MV_Type;
--        GU                 : GA_Maths.Grade_Usage := MV.Grade_Use;
--        GU_1               : constant GA_Maths.Grade_Usage := 1;
--        Count              : array (Unsigned_Integer range 1 .. 2) of Integer := (0, 0);
--        Count_Index        : Unsigned_Integer := 0;
--        Done               : Boolean := False;
--     begin
--        --  e2ga.cpp line 1631
--        MV_Info.M_Type := Multivector_Object;
--        MV_Info.M_Grade_Use := GU;
--        --  count grade part usage
--        while GU /= 0 loop
--           if (GU and GU_1) /= 0 then  --  e2ga.cpp line 1678
--              Count (Count_Index + 1 and US_1) := Count (Count_Index + 1 and US_1) + 1;
--           end if;
--           GU := Unsigned_Integer (Shift_Right (Unsigned_32 (GU), 1));
--           MV_Info.M_Grade := Integer (Count_Index);
--           Count_Index := Count_Index + 1;
--        end loop;
--
--        --  if no grade part in use: zero blade
--        if Count (1) = 0 and then Count (2) = 0  then  --  this is a zero blade
--           Put_Line ("E2GA.Init 2 Setting zero blade.");
--           Set_Type_Base (MV_Info, True, Blade_MV, 0, GU, Even_Parity);
--           Done := True;
--        else
--           --  Base.M_Zero = False by default
--           if Count (1) /= 0 and then Count (2) /= 0  then
--              --  Base.M_Parity = No_Parity by default
--              Done := True;
--           else
--              if Count (1) = 0 then
--                 Put_Line ("E2GA.Init 1 Setting even parity.");
--                 MV_Info.M_Parity := Even_Parity;
--              else
--                 --                 Put_Line ("E2GA.Init 1 Setting odd parity.");
--                 MV_Info.M_Parity := Odd_Parity;
--              end if;
--           end if;
--        end if;
--        if not Done then
--           MV_Info := Init (MV, Epsilon, True, Count (1) + Count (2));
--        end if;
--        return MV_Info;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Init 1.");
--           raise;
--     end Init;

   --  -------------------------------------------------------------------------

--     function Init (MV : Multivector; Epsilon : float;
--                    Use_Algebra_Metric : Boolean;
--                    GU_Count : Integer) return MV_Type is
--        use GA_Maths;
--        MV_Info        : MV_Type;
--        GU             : GA_Maths.Grade_Usage := MV.Grade_Use; --  Bit map indicating which grades are present
--        MV_Reverse     : constant Multivector := Reverse_Multivector (MV);
--        VI             : constant Multivector := Inverse (MV);
--        GI             : constant Multivector := Grade_Involution (MV);
--        Sq             : Scalar;
      --  V: versor; VI: versor inverse
      --  GI: Grade involution
--        GI_VI          : constant Multivector := Geometric_Product (GI, VI);
--        VI_GI          : constant Multivector := Geometric_Product (VI, GI);
--        VI_E1          : constant Multivector := Geometric_Product (VI, e1);
--        VI_E2          : constant Multivector := Geometric_Product (VI, e2);
--        GI_VI_E1       : constant Multivector := Geometric_Product (GI_VI, e1);
--        VI_GI_E1       : constant Multivector := Geometric_Product (VI_GI, e1);
--        VI_GI_GI_VI    : constant Multivector := VI_GI - GI_VI;
--        VI_E1_GI       : constant Multivector := Geometric_Product (VI_E1, GI);
--        VI_E2_GI       : constant Multivector := Geometric_Product (VI_E2, GI);
--     begin
      --  MV_Info is initialized to default values
      --        E3GA_Utilities.Print_Multivector ("E2GA.Init MV", MV);
      --        E3GA_Utilities.Print_Multivector ("E2GA.Init MV_Reverse", MV_Reverse);
      --        Put_Line ("E2GA.Init MV.GU: " & Unsigned_Integer'Image (MV.Grade_Use));
--        Sq := Scalar_Product (MV, MV_Reverse);
      --        Put_Line ("E2GA.Init Sq: " & Float'Image (Sq.Coordinates (1)));
      --        Put_Line ("E2GA.Init GI_VI: " & Unsigned_Integer'Image (GI_VI.Grade_Use));
      --        Put_Line ("E2GA.Init VI_GI_GI_VI: " & Unsigned_Integer'Image (VI_GI_GI_VI.Grade_Use));
      --        Put_Line ("E2GA.Init VI_E1_GI: " & Unsigned_Integer'Image (VI_E1_GI.Grade_Use));
      --        Put_Line ("E2GA.Init VI_E2_GI: " & Unsigned_Integer'Image (VI_E2_GI.Grade_Use));
--        if Sq.Coordinates (1) = 0.0 then  --  or
--           --           GI_VI.Grade_Use /= Grade_0 or
--           --           VI_GI_GI_VI.Grade_Use /= Grade_0 or
--           --           VI_E1_GI.Grade_Use /= Grade_1 or
--           --           VI_E2_GI.Grade_Use /= Grade_1 then
--           MV_Info.M_Type := Multivector_Type_Base.Multivector_Object;
--           --           Put_Line ("E2GA.Init MV_Info.M_Type Multivector_Object set");
--        else
--           if GU_Count = 1 then
--              MV_Info.M_Type := Multivector_Type_Base.Blade_MV;
--              --              Put_Line ("E2GA.Init MV_Info.M_Type Blade_Object set");
--           else
--              MV_Info.M_Type := Multivector_Type_Base.Versor_MV;
--              --              Put_Line ("E2GA.Init MV_Info.M_Type Versor_Object set");
--           end if;
--        end if;
--        return MV_Info;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Init 2.");
--           raise;
--     end Init;

   --  -------------------------------------------------------------------------

--     function Inverse (MV : Multivector) return Multivector is
--        use GA_Maths;
--        N : Multivector := MV;
--     begin
--        if (MV.Grade_Use and 1) /= 0 then
--           N.Coordinates (1) := MV.Coordinates (1) * MV.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and 2) /= 0 then
--           N.Coordinates (1) := N.Coordinates (1) + MV.Coordinates (2) * MV.Coordinates (2) +
--             MV.Coordinates (3) * MV.Coordinates (3);
--        end if;
--        if (MV.Grade_Use and 4) /= 0 then
--           N.Coordinates (1) := N.Coordinates (1) + MV.Coordinates (4) * MV.Coordinates (4);
--        end if;
--        return N;
--     end Inverse;

   --  -------------------------------------------------------------------------

--     function Largest_Basis_Blade (Map : Bit_Map) return float is
--     begin
--        return 0.0;
--     end Largest_Basis_Blade;

   --  -------------------------------------------------------------------------

   function Largest_Coordinate return float is
   begin
      return 0.0;
   end Largest_Coordinate;

   --  ------------------------------------------------------------------------

--     function Left_Contraction (V1, V2 : Vector) return Scalar is
--        LC  : Scalar;
--     begin
--        LC.Coordinates (1) := Dot_Product (V1, V2);
--        return LC;
--     end Left_Contraction;

   --  ------------------------------------------------------------------------

--     function Left_Contraction (V : Vector; BV : Bivector) return Vector is
--        use GA_Maths;
--        BMV   : Multivector := Set_Multivector (BV);
--        Vout  : Vector;
--     begin
--        if (BMV.Grade_Use and 1) /= 0 then
--           Vout.Coordinates (1) := V.Coordinates (1) * BV.Coordinates (1);
--        end if;
--        if (BMV.Grade_Use and 2) /= 0 then
--           Vout.Coordinates (2) :=  V.Coordinates (2) * BV.Coordinates (1);
--        end if;
--        return Vout;
--     end Left_Contraction;

   --  ------------------------------------------------------------------------

--     function Magnitude (V : Vector) return float is
--     begin
--        return GA_Maths.Float_Functions.Sqrt (V.Coordinates (1) * V.Coordinates (1) +
--                                                V.Coordinates (2) * V.Coordinates (2));
--     end Magnitude;

   --  ------------------------------------------------------------------------

   --  Implements e2ga.cpp (line 1524)  char *string(const mv & obj, char *str,
   --  int maxLength, const char *fp /* = NULL */)
   --  goal: print [+|-] MV.Coord (k) (* Basis_Vector_1 ^ ... ^ Basis_Vector_N)
   --  theString := theString & float'Image (Abs (Coordinate));
--     function Multivector_String (MV : Multivector.Multivector;
--                                  Text : String := "") return String is
--        use Interfaces;
--        use Ada.Strings.Unbounded;
--        use Multivector.Blade_List_Package;
--        Blades : Multivector.Blade_List := Multivector.Get_Blade_List (MV);
--        Curs   : Multivector.Blade_List_Package.Cursor := Blades.First;
--        aBlade : Blade.Basis_Blade;
--
--        String_Start     : Unbounded_String := To_Unbounded_String ("");
--        String_End       : Unbounded_String := To_Unbounded_String ("");
--        Value            : float_3;
--        theString        : Unbounded_String := String_Start;
--        Basis_Index      : Integer range 1 .. 4 := 1;  --  ia
--        Coord_Index      : Integer := 1;  --  k
--        Basis_Elem_Index : Integer;       --  bei
--        Name_Index       : Integer range 1 .. 2;
--        Coordinate       : float;
--     begin
--        theString := theString & Text;
--        --  Print all coordinates (x, y, z)
--        --  Loop on coordinate  i
--        --    Loop on grade     j
--        --      Loop on  number of coordinates in the grade part
--        --        Print sign and coordinate value
--        --        Loop on basis symbols
--        --          Print symbol
--        --          If not last symbol print ^
--        --        for Coord_Index in 1 .. 3 loop
--        for Grade in GA_Maths.Grade_Index'Range loop
--           if  Unsigned_32 (Multivector.Grade_Use (MV)) /=
--             Shift_Left (Unsigned_32 (Grade), 1) then
--              if Basis_Index - MV_Grade_Size (Grade) + 1 > 0 then
--                 Basis_Index := Basis_Index + MV_Grade_Size (Grade);
--              end if;
--           else
--              --  int mv_gradeSize[3] = {1, 2, 1 };
--              --  Vector grade size is 2?
--              --  Loop on grade
--              for Grade_Index in Integer range 1 .. MV_Grade_Size (Grade) loop  --  j
--                 Coord_Index := Basis_Index;
--                 aBlade := Element (Curs);
--                 Coordinate := MV_Basis_Element_Sign_By_Index (Basis_Index) *
--                 Abs (Blade.Weight (aBlade));
--  --                 Abs (MV.Coordinates (Coord_Index));
--                 Value := float_3 (Coordinate);
--                 theString := theString & float_3'Image (Value);
--                 if Grade /= 1 then  --  Not grade 0
--                    --  print [* basisVector1 ^ ... ^ basisVectorN]
--                    theString := theString & " ";
--                    --  Loop on the basis vector symbols
--                    --  MV_Basis_Elements : array (1 .. 4, 1 .. 3) of integer :=
--                    --        ((-1, 0, 0), (0, -1, 0), (1, -1, 0), (0, 1, -1));
--
--                    Basis_Elem_Index := 1;  --  bei
--                    while MV_Basis_Elements (Basis_Index - 1, Basis_Elem_Index) >= 0 loop
--                       if Basis_Elem_Index /= 1 then
--                          theString := theString & "^";
--                       end if;
--                       Name_Index := MV_Basis_Elements (Basis_Index - 1, Basis_Elem_Index) + 1;
--                       theString := theString & MV_Basis_Vector_Names (Name_Index);
--                       Basis_Elem_Index := Basis_Elem_Index + 1;
--                    end loop;  --  MV_Basis_Elements
--                 end if;  --  Grade
--                 Next (Curs);
--              end loop;  --  Grade_Index
--           end if;  --  MV.Grade_Use
--           if Grade >= 2 then
--              Coord_Index := Coord_Index + 1;
--           end if;
--        end loop;   --  Grade
--
--        return To_String (theString & String_End);
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Multivector_String.");
--           raise;
--     end Multivector_String;

   --  -------------------------------------------------------------------------

   function New_Bivector (V1, V2 : Vector) return Bivector is
      BV : Bivector;
   begin
      return Multivector.Outer_Product (V1, V2);
   end New_Bivector;

   --  -------------------------------------------------------------------------

   function New_Vector (e1, e2 : Float) return Vector is
      use Blade;
      V       : Vector;
      Blades  : Multivector.Blade_List := Multivector.Get_Blade_List (V);
      aBlade  : Basis_Blade := New_Basis_Blade (E2_e1, e1);
   begin
      Multivector.Add_Blade (V, aBlade);
      aBlade :=  New_Basis_Blade (E2_e2, e2);
      Multivector.Add_Blade (V, aBlade);
      return V;
   end New_Vector;

   --  ------------------------------------------------------------------------

--     function Norm_E2 (V2 : Vector) return Scalar is
--        Norm : Scalar;
--     begin
--        Norm.Coordinates (1) := (V2.Coordinates (1) * V2.Coordinates (1) +
--                                   V2.Coordinates (2) * V2.Coordinates (2));
--        return Norm ;
--     end Norm_E2;

   --  ----------------------------------------------------------------------------

--     function Norm_E (MV : Multivector) return Scalar is
--        use GA_Maths;
--        Value : float := 0.0;
--        Norm  : Scalar;
--     begin
--        if (MV.Grade_Use and 1) /= 0 then
--           Value := MV.Coordinates (1) * MV.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and 2) /= 0 then
--           Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
--             MV.Coordinates (3) * MV.Coordinates (3);
--        end if;
--        if (MV.Grade_Use and 4) /= 0 then
--           Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
--        end if;
--        Norm.Coordinates (1) := Value;
--        return Norm;
--     end Norm_E;

   --  ----------------------------------------------------------------------------

--     function Norm_E2 (MV : Multivector) return Scalar is
--        use GA_Maths;
--        Value : float := 0.0;
--        Norm  : Scalar;
--     begin
--        if (MV.Grade_Use and 1) /= 0 then
--           Value := MV.Coordinates (1) * MV.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and 2) /= 0 then
--           Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
--             MV.Coordinates (3) * MV.Coordinates (3);
--        end if;
--        if (MV.Grade_Use and 4) /= 0 then
--           Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
--        end if;
--
--        Norm.Coordinates (1) := Value;
--        return Norm;
--     end Norm_E2;

   --  -------------------------------------------------------------------------

--     function Outer_Product (V1, V2 : Vector) return Bivector is
--        --  The outer product basis in 2D is the coordinate of e1^e2.
--        use E2GA;
--        BV        : Bivector;
--     begin
--        BV.Coordinates (1) :=
--          GA_Maths.Float_Array_Package.Determinant
--            (((V1.Coordinates (1), V2.Coordinates (1)),
--             (V1.Coordinates (2), V2.Coordinates (2))));
--        return BV;
--     end Outer_Product;

   --  ------------------------------------------------------------------------

--     function Reverse_Multivector (MV : Multivector) return Multivector is
--        use GA_Maths;
--        MV_R     : Multivector := MV;
--        Coords_8 : Coords_Continuous_Array (1 .. 4) := (others => 0.0);
--     begin
--        if (MV.Grade_Use and 1) /= 0 then
--           Coords_8 (1) := MV.Coordinates (1);
--        end if;
--        if (MV.Grade_Use and 2) /= 0 then
--           Coords_8 (2) := MV.Coordinates (2);
--           Coords_8 (3) := MV.Coordinates (3);
--        end if;
--        if (MV.Grade_Use and 4) /= 0 then
--           Coords_8 (4) := - MV.Coordinates (4);
--        end if;
--        MV_R.Coordinates := Coords_8;
--        return MV_R;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Reverse_Multivector");
--           raise;
--     end Reverse_Multivector;

   --  ------------------------------------------------------------------------

--     function Scalar_Product (V1, V2 : Vector) return Scalar is
--        Product : Scalar;
--     begin
--        Product.Coordinates (1) := Dot_Product (V1, V2);
--        return  Product;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Scalar_Product V.");
--           raise;
--     end Scalar_Product;

   --  -------------------------------------------------------------------------

--     function Scalar_Product (MV1, MV2 : Multivector) return Scalar is
--        use GA_Maths;
--        Product   : float := 0.0;
--        theScalar : Scalar;
--     begin
--        if (MV2.Grade_Use and 1) /= 0 and (MV1.Grade_Use and 1) /= 0 then
--           Product :=  MV1.Coordinates (1) * MV2.Coordinates (1);
--        end if;
--
--        if (MV2.Grade_Use and 2) /= 0 and (MV1.Grade_Use and 2) /= 0 then
--           Product :=  Product + MV1.Coordinates (2) * MV2.Coordinates (2)
--             + MV1.Coordinates (3) * MV2.Coordinates (3);
--        end if;
--
--        if (MV2.Grade_Use and 4) /= 0 and (MV1.Grade_Use and 4) /= 0 then
--           Product :=  Product - MV1.Coordinates (4) * MV2.Coordinates (4);
--        end if;
--        theScalar.Coordinates (1) := Product;
--        return  theScalar;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Scalar_Product MV.");
--           raise;
--     end Scalar_Product;

   --  -------------------------------------------------------------------------

--     function Set_Bivector (V1, V2 : Vector) return Bivector is
--     begin
--        return  Multivector.Outer_Product (V1, V2);
--     end Set_Bivector;

   --  -------------------------------------------------------------------------

--     function Set_Bivector (MV : Multivector) return Bivector is
--        BV : Bivector;
--     begin
--        BV.Coordinates (1) := MV.Coordinates (4);
--        return  BV;
--     end Set_Bivector;

   --  -------------------------------------------------------------------------

   procedure Set_Coords (V : out Vector; C1, C2 : float) is
   begin
      V := New_Vector (C1, C2);
   end Set_Coords;

   --  ------------------------------------------------------------------------

--     function Set_Multivector (S1 : Scalar) return Multivector is
--        MV : Multivector (1);
--     begin
--        MV.Coordinates (1) := S1.Coordinates (1);
--        return  MV;
--     end Set_Multivector;

   --  -------------------------------------------------------------------------

--     function Set_Multivector (V : Vector) return Multivector is
--        MV : Multivector (2);
--     begin
--        MV.Coordinates (1) := V.Coordinates (1);
--        MV.Coordinates (2) := V.Coordinates (2);
--        return  MV;
--     end Set_Multivector;

   --  -------------------------------------------------------------------------

--     function Set_Multivector (BV : Bivector) return Multivector is
--        use GA_Maths.Float_Functions;
--        MV   : Multivector (4);
--     begin
--        MV.Coordinates (4) := BV.Coordinates (1);
--        return  MV;
--     exception
--        when anError :  others =>
--           Put_Line ("An exception occurred in E2GA.Set_Multivector BV.");
--           raise;
--     end Set_Multivector;

   --  -------------------------------------------------------------------------

--     function Set_Multivector (R : Rotor) return Multivector is
--        MV : Multivector (5);
--     begin
--        MV.Coordinates (1) := R.Coordinates (1);
--        MV.Coordinates (2) := R.Coordinates (2);
--        return  MV;
--     end Set_Multivector;

   --  -------------------------------------------------------------------------

--     function Set_Scalar (MV : Multivector) return Scalar is
--        use GA_Maths;
--        theScalar : Scalar;
--     begin
--        if (Unsigned_Integer (MV.Grade_Use) and US_1) /= 0 then
--           theScalar.Coordinates (1) := MV.Coordinates (1);
--        else
--           theScalar.Coordinates (1) := 0.0;
--        end if;
--        return theScalar;
--     end Set_Scalar;

   --  -------------------------------------------------------------------------

   function Set_Rotor (E1_E2 : float) return Rotor is
      theRotor : Rotor;
   begin
      theRotor.Coordinates := (E1_E2, 0.0);
      return theRotor;
   end Set_Rotor;

   --  ------------------------------------------------------------------------

    procedure Set_Scalar (S : out Scalar; Value : float) is
    begin
        S.Coordinates (1) := Value;
    end Set_Scalar;

   --  ------------------------------------------------------------------------

--     function Unit_E (MV : Multivector) return Vector is
--        use GA_Maths;
--        E2         : constant Scalar := Scalar_Product (MV, MV);
--        E2_Value   : constant float := E2.Coordinates (1);
--        IE_Value   : constant float := 1.0 / GA_Maths.Float_Functions.Sqrt (E2_Value);
--        theVector  : Vector;
--     begin
--        theVector.Coordinates (1) :=  MV.Coordinates (1) * MV.Coordinates (1);
--        theVector.Coordinates (2) :=  MV.Coordinates (2) * IE_Value;
--        return  theVector;
--     end Unit_E;

   --  -------------------------------------------------------------------------

   function Unit_E (V : Vector) return Vector is
      use Multivector.Blade_List_Package;
      use Blade;
      Blades  : Multivector.Blade_List := Multivector.Get_Blade_List (V);
      C1       : constant float := Weight (Blades.First_Element);
      C2       : constant float := Weight (Blades.Last_Element);
      e2s      : constant float :=  C1 * C1 + C2 * C2;
      IE       : constant float := 1.0 / GA_Maths.Float_Functions.Sqrt (e2s);
      Result   : Vector;
   begin
      Multivector.Add_Blade (Result, New_Basis_Blade (E2_e1, C1 * IE));
      Multivector.Add_Blade (Result, New_Basis_Blade (E2_e2, C2 * IE));
      return  Result;
   end Unit_E;

   --  -------------------------------------------------------------------------

begin
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   MV_Basis_Vector_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));

end E2GA;
