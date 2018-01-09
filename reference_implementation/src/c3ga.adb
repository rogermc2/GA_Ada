
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Multivector;
with Multivector_Type_Base;

package body C3GA is

   MV_Space_Dimension  : constant Integer := 5;
   MV_Metric_Euclidean : constant Boolean := False;

   type Basis_Name is (NOb, E1b, E2b, E3b, NIb);
   type Grade_Name is (G0, G1, G2, G3, G4, G5);

   --  This array can be used to lookup the number of coordinates for
   --  a grade part of a general multivector
   MV_Grade_Size : constant array (0 .. 5) of Integer := (1, 5, 10, 10, 5, 1 );
   --  This array can be used to lookup the number of coordinates
   --  based on a grade usage bitmap
   MV_Size       : constant array (1 .. 64) of Integer :=
     (0, 1, 5, 6, 10, 11, 15, 16, 10, 11, 15, 16, 20, 21, 25, 26,
      5, 6, 10, 11, 15, 16, 20, 21, 15, 16, 20, 21, 25, 26, 30, 31,
      1, 2, 6, 7, 11, 12, 16, 17, 11, 12, 16, 17, 21, 22, 26, 27,
      6, 7, 11, 12, 16, 17, 21, 22, 16, 17, 21, 22, 26, 27, 31, 32);
   --  This array contains the order of basis elements in the general multivector
   --  Use it to answer : 'at what index do I find basis element (x)
   --  (x = basis vector bitmap)?
   MV_Basis_Element_Index_By_Bitmap : constant array (1 .. 32) of Integer :=
     (0, 1, 2, 6, 3, 7, 9, 24, 4, 8, 11, 23, 10, 22, 25, 30,
      5, 15, 12, 20, 13, 21, 18, 29, 14, 19, 17, 28, 16, 27, 26, 31);

      no_basis : constant Vector := (0.0, 0.0, 0.0, 0.0, 1.0);
      e1_basis : constant Vector := (0.0, 1.0, 0.0, 0.0, 0.0);
      e2_basis : constant Vector := (0.0, 0.0, 1.0, 0.0, 0.0);
      e3_basis : constant Vector := (0.0, 0.0, 0.0, 1.0, 0.0);
      ni_basis : constant Vector := (1.0, 0.0, 0.0, 0.0, 0.0);

--     no_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.no);
--     e1_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e1);
--     e2_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e2);
--     e3_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e3);
--     ni_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.ni);

   function Init (MV : C3GA.Multivector; Epsilon : float;
                  Use_Algebra_Metric : Boolean;
                  GU_Count : Integer) return MV_Type;

   --  -------------------------------------------------------------------------

   function "+" (V1 : Vector; V2 : Vector) return Vector is
   begin
      return (V1 (1) + V2 (1), V1 (2) + V2 (2), V1 (3) + V2 (3),
              V1 (4) + V2 (4), V1 (5) + V2 (5));
   end  "+";

   --  -------------------------------------------------------------------------

   function "*" (F : Float; V : Vector) return Vector is
   begin
      return (F * V (1), F * V (2), F * V (3), F * V (4), F * V (5));
   end  "*";

   --  -------------------------------------------------------------------------

   function "*" (L : Line; S : Float) return Line is
   begin
      return (L.E1_E2_NI * S, L.E1_E3_NI * S, L.E2_E3_NI * S,
              L.E1_NO_NI * S, L.E2_NO_NI * S, L.E3_NO_NI * S);
   end  "*";

   --  -------------------------------------------------------------------------

   function "*" (S : Float; L : Line) return Line is
   begin
      return L * S;
   end  "*";

   --  -------------------------------------------------------------------------

   function C3GA_Point (V : Vector_E3GA) return Normalized_Point is
      thePoint : Normalized_Point;
   begin
      --  thePoint.Origin of a Normalized_Point is a constant 1.0
      thePoint.E1 := V.Coordinates (1);
      thePoint.E2 := V.Coordinates (2);
      thePoint.E3 := V.Coordinates (3);
      thePoint.Inf := 0.5 * Norm_E2(V).Coordinates (1) * GA_Base_Types.NI;
      return thePoint;
   end C3GA_Point;

   --  ------------------------------------------------------------------------

   function Coord (S : Scalar) return float is
   begin
      return S.Coordinates (1);
   end Coord;

   --  -------------------------------------------------------------------------

   function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_Integer  is
   begin
      return MV.Grade_Use;
   end Grade_Use;

    --  ------------------------------------------------------------------------

   function Init (MV : Multivector; Epsilon : float := 0.0) return MV_Type is
      use Interfaces;
      use GA_Maths;
      use  Multivector_Type_Base;
      MV_Info            : MV_Type;
      GU                 : GA_Maths.Grade_Usage := Grade_Use (MV);
      Count              : array (Unsigned_Integer range 1 .. 2) of Integer := (0, 0);
      Count_Index        : Unsigned_Integer := 0;
      Index              : Unsigned_Integer := 0;
      Done               : Boolean := False;
   begin
      MV_Info.M_Type := Multivector_Object;
      MV_Info.M_Grade_Use := GU;
      --  count grade part usage
      while GU /= 0 loop
         if (GU and GU_1) /= 0 then  --  c3ga.cpp line 21731
            Index := Count_Index and US_1;
            Count (Index) := Count (Index) + 1;
         end if;
         GU := Unsigned_Integer (Shift_Right (Unsigned_32 (GU), 1));
         MV_Info.M_Grade := Integer (Count_Index);
         Count_Index := Count_Index + 1;
      end loop;

      --  if no grade part in use: zero blade
      if Count (1) = 0 and then Count (2) = 0  then  --  this is a zero blade
         Put_Line ("C3GA.Init 1 Setting zero blade.");
         Set_Type_Base (MV_Info, True, Blade_MV, 0, GU, Even_Parity);
         Done := True;
      else
         --  Base.M_Zero = False by default
         if Count (1) /= 0 and then Count (2) /= 0  then
            --  Base.M_Parity = No_Parity by default
            Done := True;
         else
            if Count (1) = 0 then
               Put_Line ("C3GA.Init 1 Setting even parity.");
               MV_Info.M_Parity := Even_Parity;
            else
               --                 Put_Line ("C3GA.Init 1 Setting odd parity.");
               MV_Info.M_Parity := Odd_Parity;
            end if;
         end if;
      end if;
      if not Done then
         MV_Info := Init (MV, Epsilon, True, Count (1) + Count (2));
      end if;
      return MV_Info;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in C3GA.Init 1.");
         raise;
   end Init;

   -------------------------------------------------------------------------

   function Init (MV : C3GA.Multivector; Epsilon : float;
                  Use_Algebra_Metric : Boolean;
                  GU_Count : Integer) return MV_Type is
      MV_Info : MV_Type;
   begin
      --  To be completed.
      return MV_Info;
   end Init;

   --  -------------------------------------------------------------------------

   function E1_E2_NI (C : Circle) return float is
   begin
      return C.E1_E2_NI;
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E3_E1_NI (C : Circle) return float is
   begin
      return C.E3_E1_NI;
   end E3_E1_NI;

   --  -------------------------------------------------------------------------

   function E1_E2_E3 (C : Circle) return float is
   begin
      return C.E1_E2_E3;
   end E1_E2_E3;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (C : Circle) return float is
   begin
      return C.E2_E3_NI;
   end E2_E3_NI;

   --  -------------------------------------------------------------------------

   function Get_Coord_1 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (1);
   end Get_Coord_1;

   --  ------------------------------------------------------------------------

   function Get_Coord_2 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (2);
   end Get_Coord_2;

   --  ------------------------------------------------------------------------

   function Get_Coord_3 (V : Vector_E3GA) return float is
   begin
      return V.Coordinates (3);
   end Get_Coord_3;

   --  ------------------------------------------------------------------------

   function Get_Coords (V : Vector_E3GA) return GA_Maths.Array_3D is
   begin
      return (V.Coordinates (1), V.Coordinates (2), V.Coordinates (3));
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coords (NP : Normalized_Point) return Vector is
   begin
      return (1.0, NP.E1, NP.E2, NP.E3, NP.Inf);
   end Get_Coords;

   ------------------------------------------------------------------------

   function Get_Coords (NP : Normalized_Point)
                        return GA_Maths.Coords_Continuous_Array is
      Coords : GA_Maths.Coords_Continuous_Array (1 .. 4)
        :=  (NP.E1, NP.E2, NP.E3, NP.Inf);
   begin
      return Coords;
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function NO_E1_E2 (C : Circle) return float is
   begin
      return C.NO_E1_E2;
   end NO_E1_E2;

   --  -------------------------------------------------------------------------

   function NO_E1_E3 (C : Circle) return float is
   begin
      return C.NO_E1_E3;
   end NO_E1_E3;

   --  -------------------------------------------------------------------------

   function NO_E1_NI (C : Circle) return float is
   begin
      return C.NO_E1_NI;
   end NO_E1_NI;

   --  -------------------------------------------------------------------------

   function NO_E2_E3 (C : Circle) return float is
   begin
      return C.NO_E2_E3;
   end NO_E2_E3;

   --  -------------------------------------------------------------------------

   function NO_E2_NI (C : Circle) return float is
   begin
      return C.NO_E2_NI;
   end NO_E2_NI;

   --  -------------------------------------------------------------------------

   function NO_E3_NI (C : Circle) return float is
   begin
      return C.NO_E3_NI;
   end NO_E3_NI;

   --  -------------------------------------------------------------------------

   function E1b (DP : Dual_Plane) return float is
   begin
      return DP.E1;
   end E1b;

   --  -------------------------------------------------------------------------

   function E2b (DP : Dual_Plane) return float is
   begin
      return DP.E2;
   end E2b;

   --  -------------------------------------------------------------------------

   function E3b (DP : Dual_Plane) return float is
   begin
      return DP.E3;
   end E3b;

   --  -------------------------------------------------------------------------

   function NIb (DP : Dual_Plane) return GA_Base_Types.NI_T is
   begin
      return DP.Inf;
   end NIb;

   --  -------------------------------------------------------------------------

   function E1_E2_NI (L : Line) return float is
   begin
      return L.E1_E2_NI;
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E1_E3_NI (L : Line) return float is
   begin
      return L.E1_E3_NI;
   end E1_E3_NI;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (L : Line) return float is
   begin
      return L.E2_E3_NI;
   end E2_E3_NI;

   --  -------------------------------------------------------------------------

   function E1_NO_NI (L : Line) return float is
   begin
      return L.E1_NO_NI;
   end E1_NO_NI;

   --  -------------------------------------------------------------------------

   function E2_NO_NI (L : Line) return float is
   begin
      return L.E2_NO_NI;
   end E2_NO_NI;

   --  -------------------------------------------------------------------------

   function E3_NO_NI (L : Line) return float is
   begin
      return L.E3_NO_NI;
   end E3_NO_NI;

   --  -------------------------------------------------------------------------

   function NO_E1_E2_E3_NI (MV : C3GA.Multivector) return float is
      use GA_Maths;
--        use Multivector.Blade_List_Package;
--        Blades     : constant Multivector.Blade_List
--          := Multivector.Get_Blade_List (MV);
--        thisBlade  : Blade.Basis_Blade;
      GU         : Grade_Usage := Grade_Use (MV);
      GU_32      : constant Grade_Usage := 32;
      Grade_Size : Integer;
      MV2        : Multivector (GU);
   begin
         if (GU and GU_32) = 0 then
            return 0.0;
         else
            Grade_Size := MV_Grade_Size (Integer(GU and GU_32));
--              MV2 := Multivector.Get_Basis_Vector (Blade.Base'Enum_Val (Grade_Size));
            return 0.0;
         end if;
   end NO_E1_E2_E3_NI;

      --  ---------------------------------------------------------

      function E1b (NP : Normalized_Point) return float is
      begin
         return NP.E1;
      end E1b;

      --  -------------------------------------------------------------------------

      function E2b (NP : Normalized_Point) return float is
      begin
         return NP.E2;
      end E2b;

      --  -------------------------------------------------------------------------

      function E3b (NP : Normalized_Point) return float is
      begin
         return NP.E3;
      end E3b;

      --  -------------------------------------------------------------------------

      function NIb (NP : Normalized_Point) return Float is
      begin
         return NP.Inf;
      end NIb;

      --  -------------------------------------------------------------------------

      function NOb (NP : Normalized_Point) return Float is
      begin
         return 1.0;
      end NOb;

      --  -------------------------------------------------------------------------

      function E1_E2_E3_NI (S : Sphere) return float is
      begin
         return S.E1_E2_E3_NI;
      end E1_E2_E3_NI;

      --  -------------------------------------------------------------------------

      function E1_E2_NO_NI (S : Sphere) return float is
      begin
         return S.E1_E2_NO_NI;
      end E1_E2_NO_NI;

      --  -------------------------------------------------------------------------

      function E1_E3_NO_NI (S : Sphere) return float is
      begin
         return S.E1_E3_NO_NI;
      end E1_E3_NO_NI;

      --  -------------------------------------------------------------------------

      function E2_E3_NO_NI (S : Sphere) return float is
      begin
         return S.E2_E3_NO_NI;
      end E2_E3_NO_NI;

      --  -------------------------------------------------------------------------

      function E1_E2_E3_NO (S : Sphere) return float is
      begin
         return S.E1_E2_E3_NO;
      end E1_E2_E3_NO;

      --  -------------------------------------------------------------------------

      function Norm_E (MV : Multivector) return Scalar is
         use GA_Maths;
         GU  : Grade_Usage :=  Grade_Use (MV);
         Sum : Float := 0.0;
         E2  : Scalar;
      begin
         if (GU and GU_0) /= 0 then
            Sum := MV.Coordinates (1) * MV.Coordinates (1);
         end if;
         if (GU and GU_1) /= 0 then
            For index in 2 .. 6 loop
               Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
            end loop;
         end if;
         if (GU and GU_2) /= 0 then
            For index in 7 .. 16 loop
               Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
            end loop;
         end if;
         if (GU and GU_4) /= 0 then
            For index in 17 .. 26 loop
               Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
            end loop;
         end if;
         if (GU and GU_8) /= 0 then
            For index in 27 .. 31 loop
               Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
            end loop;
         end if;
         if (GU and GU_16) /= 0 then
            Sum := Sum + MV.Coordinates (32) * MV.Coordinates (32);
         end if;
         E2.Coordinates (1) := Sum;
         return E2;
      end Norm_E;

      --  -------------------------------------------------------------------------

      function Norm_E2 (V : Vector_E3GA) return Scalar is
         theNorm : Scalar;
      begin
         theNorm.Coordinates (1) := V.Coordinates (1) * V.Coordinates (1) +
           V.Coordinates (2) * V.Coordinates (2) +
           V.Coordinates (3) * V.Coordinates (3);
         return theNorm;
      end Norm_E2;

      --  -------------------------------------------------------------------------

      procedure Set_Coords (V : out Vector_E3GA; C1, C2, C3 : float) is
      begin
         V.Coordinates (1) := C1;
         V.Coordinates (2) := C2;
         V.Coordinates (3) := C3;
      end Set_Coords;

      --  -------------------------------------------------------------------------

      function Set_Coords (C1, C2, C3 : float) return Vector_E3GA is
         Vec : Vector_E3GA;
      begin
         Vec.Coordinates :=  (C1, C2, C3);
         return Vec;
      end Set_Coords;

      --  -------------------------------------------------------------------------

      procedure Set_Coords (P : out Point; Origin, C1, C2, C3, Inf : float) is
         use GA_Base_Types;
      begin
         Set_NO (P.Origin, Origin);
         P.E1 := C1;
         P.E2 := C2;
         P.E3 := C3;
         P.Inf := Inf;
      end Set_Coords;

      --  -------------------------------------------------------------------------

      procedure Set_Multivector (MV : out Multivector; NP : Normalized_Point) is
      begin
         MV.Coordinates (1) := 1.0;
         MV.Coordinates (2) := NP.E1;
         MV.Coordinates (3) := NP.E2;
         MV.Coordinates (4) := NP.E3;
         MV.Coordinates (5) := NP.Inf;
      end Set_Multivector;

      --  -------------------------------------------------------------------------

      procedure Set_Multivector (MV : out Multivector; N : GA_Base_Types.NI_T) is
      begin
         MV.Coordinates (1) := 0.0;
         MV.Coordinates (2) := 0.0;
         MV.Coordinates (3) := 0.0;
         MV.Coordinates (4) := 0.0;
         MV.Coordinates (5) := GA_Base_Types.NI (N);
      end Set_Multivector;

      --  -------------------------------------------------------------------------

      procedure Set_Multivector (MV : out Multivector; N : GA_Base_Types.NO_T) is
      begin
         MV.Coordinates (1) := GA_Base_Types.NO (N);
         MV.Coordinates (2) := 0.0;
         MV.Coordinates (3) := 0.0;
         MV.Coordinates (4) := 0.0;
         MV.Coordinates (5) := 0.0;
      end Set_Multivector;

      --  -------------------------------------------------------------------------

      function Set_Normalized_Point (E1, E2, E3 : float; Inf : float := 1.0)
                                  return Normalized_Point is
         Point : Normalized_Point;
      begin
         Point.E1 := E1;
         Point.E2 := E2;
         Point.E3 := E3;
         Point.Inf := Inf;
         return Point;
      end Set_Normalized_Point;

      --  -------------------------------------------------------------------------

      function Set_Normalized_Point (Point : GA_Maths.Array_3D;
                                     Inf : float := 1.0)
                                  return Normalized_Point is
         NP : Normalized_Point;
      begin
         NP.E1 := Point (1);
         NP.E2 := Point (2);
         NP.E3 := Point (3);
         NP.Inf := Inf;
         return NP;
      end Set_Normalized_Point;

      --  -------------------------------------------------------------------------

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use GA_Maths;
      Coords  : GA_Maths.Coords_Continuous_Array (1 .. 32);
      GU1     : Grade_Usage := MV1.Grade_Use;
      GU2     : Grade_Usage := MV2.Grade_Use;
      Size_1  : integer := MV_Size (Integer (MV1.Grade_Use));
      Size_2  : integer := MV_Size (Integer (MV2.Grade_Use));
      MV_GU   : Grade_Usage := GU1 or GU2;
      Sum     : Float := 0.0;
      Product : Multivector (MV_GU);

      function Grade_Used (MV : Multivector; Index : Integer) return Boolean is
         GU     : Grade_Usage := MV1.Grade_Use;
         Result : Boolean := False;
      begin
         case Index is
               when 0 => Result := (GU and GU_0) /= 0;
               when 1 => Result := (GU and GU_1) /= 0;
               when 2 => Result := (GU and GU_2) /= 0;
               when 3 => Result := (GU and GU_4) /= 0;
               when 4 => Result := (GU and GU_8) /= 0;
               when 5 => Result := (GU and GU_16) /= 0;
               when others =>
                   Put_Line ("C3GA.Outer_Product Invalid Index");
         end case;
         return Result;
      end Grade_Used;

   begin
         for Index2 in 1 ..32 loop
            if Grade_Used (MV2, Integer (GU1)) then
               for Index1 in 1 .. 32 loop
                  null;
               end loop;
            end if;
         end loop;

         if (GU2 and GU_0) /= 0 then
            if (GU1 and GU_1) /= 0 then
               Coords (1) := MV1.Coordinates (1) * MV2.Coordinates (1);
            end if;
            if (GU1 and GU_1) /= 0 then
               For index in 2 .. 6 loop
                  Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
               end loop;
            end if;
            if (GU1 and GU_2) /= 0 then
               For index in 7 .. 16 loop
                  Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
               end loop;
            end if;
            if (GU1 and GU_4) /= 0 then
               For index in 17 .. 26 loop
                  Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
               end loop;
            end if;
            if (GU1 and GU_8) /= 0 then
               For index in 27 .. 31 loop
                  Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
               end loop;
            end if;
            if (GU1 and GU_16) /= 0 then
               Coords (32) := MV1.Coordinates (32) * MV2.Coordinates (1);
            end if;
         end if;

         if (GU2 and GU_1) /= 0 then
            if (GU1 and GU_1) /= 0 then
               For index in 2 .. 6 loop
                  Coords (index) := Coords (index) +
                    MV1.Coordinates (1) * MV2.Coordinates (index);
               end loop;
            end if;

            if (GU1 and GU_2) /= 0 then
               Coords (7) := Coords (7) +
                 MV1.Coordinates (2) * MV2.Coordinates (3) -
                 MV1.Coordinates (3) * MV2.Coordinates (2);
               Coords (8) := Coords (8) +
                 MV1.Coordinates (2) * MV2.Coordinates (4) -
                 MV1.Coordinates (4) * MV2.Coordinates (2);
               Coords (9) := Coords (9) +
                 MV1.Coordinates (2) * MV2.Coordinates (5) -
                 MV1.Coordinates (5) * MV2.Coordinates (2);
               Coords (10) := Coords (10) +
                 MV1.Coordinates (3) * MV2.Coordinates (4) -
                 MV1.Coordinates (4) * MV2.Coordinates (3);
               Coords (11) := Coords (11) +
                 MV1.Coordinates (4) * MV2.Coordinates (5) -
                 MV1.Coordinates (5) * MV2.Coordinates (4);
               Coords (12) := Coords (12) +
                 MV1.Coordinates (5) * MV2.Coordinates (3) -
                 MV1.Coordinates (3) * MV2.Coordinates (5);
               Coords (13) := Coords (13) +
                 MV1.Coordinates (3) * MV2.Coordinates (6) -
                 MV1.Coordinates (6) * MV2.Coordinates (3);
            end if;
         end if;
         return Product;
   end Outer_Product;

      --  -------------------------------------------------------------------------

      function Unit_R (L : Line) return Line is
         use GA_Maths.Float_Functions;
         R_Sq : constant float := -(L.E1_NO_NI * L.E1_NO_NI +
                                      L.E2_NO_NI * L.E2_NO_NI + L.E3_NO_NI * L.E3_NO_NI);
         Inv  : constant float := 1.0 / Sqrt (Abs (R_Sq));
      begin
         return L * Inv;
      end Unit_R;

      --  -------------------------------------------------------------------------

      --     function US_Normalized_Point (N : Normalized_Point) return Normalized_Point is
      --        thePoint : Normalized_Point := N;
      --     begin
      --        thePoint.Inf := 0.0;
      --        return thePoint;
      --     end US_Normalized_Point;
      --
      --     --  -------------------------------------------------------------------------
      --
      --     function US_Set_Normalized_Point (E1, E2, E3 : Float) return Normalized_Point is
      --        NP : Normalized_Point;
      --     begin
      --        NP.E1 := E1;
      --        NP.E2 := E2;
      --        NP.E3 := E3;
      --        NP.Inf := 0.0;
      --        return NP;
      --     end US_Set_Normalized_Point;
      --
      --     --  -------------------------------------------------------------------------
      --
      --     function US_Set_Normalized_Point (Point : Vector_E3GA) return Normalized_Point is
      --        NP : Normalized_Point;
      --     begin
      --        NP.E1 := Point.Coordinates (1);
      --        NP.E2 := Point.Coordinates (2);
      --        NP.E3 := Point.Coordinates (3);
      --        NP.Inf := 0.0;
      --        return NP;
      --     end US_Set_Normalized_Point;

      --  -------------------------------------------------------------------------

end C3GA;
