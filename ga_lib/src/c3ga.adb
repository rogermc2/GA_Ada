
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Blade;
with Blade_Types; use Blade_Types;
with GA_Utilities;
with Metric;

package body C3GA is

   --     MV_Space_Dimension  : constant Integer := 5;
   --     MV_Metric_Euclidean : constant Boolean := False;
   --
   --     type Basis_Name is (NOb, E1b, E2b, E3b, NIb);
   --     type Grade_Name is (G0, G1, G2, G3, G4, G5);

   --  This array can be used to lookup the number of coordinates for
   --  a grade part of a general multivector
   MV_Grade_Size : constant array (0 .. 5) of Integer := (1, 5, 10, 10, 5, 1 );
   --  This array can be used to lookup the number of coordinates
   --  based on a grade usage bitmap
   --     MV_Size       : constant array (1 .. 64) of Integer :=
   --       (0, 1, 5, 6, 10, 11, 15, 16, 10, 11, 15, 16, 20, 21, 25, 26,
   --        5, 6, 10, 11, 15, 16, 20, 21, 15, 16, 20, 21, 25, 26, 30, 31,
   --        1, 2, 6, 7, 11, 12, 16, 17, 11, 12, 16, 17, 21, 22, 26, 27,
   --        6, 7, 11, 12, 16, 17, 21, 22, 16, 17, 21, 22, 26, 27, 31, 32);
   --  This array contains the order of basis elements in the general multivector
   --  Use it to answer : 'at what index do I find basis element (x)
   --  (x = basis vector bitmap)?
   --     MV_Basis_Element_Index_By_Bitmap : constant array (1 .. 32) of Integer :=
   --       (0, 1, 2, 6, 3, 7, 9, 24, 4, 8, 11, 23, 10, 22, 25, 30,
   --        5, 15, 12, 20, 13, 21, 18, 29, 14, 19, 17, 28, 16, 27, 26, 31);

   --     no_basis : constant Vector_C3GA := (0.0, 0.0, 0.0, 0.0, 1.0);
   --     e1_basis : constant Vector_C3GA := (0.0, 1.0, 0.0, 0.0, 0.0);
   --     e2_basis : constant Vector_C3GA := (0.0, 0.0, 1.0, 0.0, 0.0);
   --     e3_basis : constant Vector_C3GA := (0.0, 0.0, 0.0, 1.0, 0.0);
   --     ni_basis : constant Vector_C3GA := (1.0, 0.0, 0.0, 0.0, 0.0);

   --     no_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.no);
   --     e1_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e1);
   --     e2_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e2);
   --     e3_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.e3);
   --     ni_BV : constant C3GA.Multivector := Get_Basis_Vector (Blade.ni);

   function "*" (L : Float; R : Vector_E3) return Vector_E3 is
      use GL.Types;
   begin
      return (Single (L) * R (GL.X), Single (L) * R (GL.Y), Single (L) * R (GL.Z));
   end  "*";

   --  -------------------------------------------------------------------------    --          R_Coords : constant E3GA.Vector := R.Coordinates;

   function e1 return Multivectors.Multivector is
      use Multivectors;
      Basis   : Multivectors.M_Vector;
   begin
      Add_Blade (Basis, C3_e1, 1.0);
      return Basis;
   end e1;

   --  -------------------------------------------------------------------------

   function e1 (MV : Multivectors.Multivector) return float is
   begin
      return Multivectors.Component (MV, C3_Base'Enum_Rep (C3_e1));
   end e1;

   --  -------------------------------------------------------------------------

   function e2 return Multivectors.Multivector is
      use Multivectors;
      Basis   : Multivectors.M_Vector;
   begin
      Add_Blade (Basis, C3_e2, 1.0);
      return Basis;
   end e2;

   --  -------------------------------------------------------------------------

   function e2 (MV : Multivectors.Multivector) return float is
   begin
      return Multivectors.Component (MV, C3_Base'Enum_Rep (C3_e2));
   end e2;

   --  -------------------------------------------------------------------------

   function e3 return Multivectors.Multivector is
      use Multivectors;
      Basis   : Multivectors.M_Vector;
   begin
      Add_Blade (Basis, C3_e3, 1.0);
      return Basis;
   end e3;

   --  -------------------------------------------------------------------------

   function e3 (MV : Multivectors.Multivector) return float is
   begin
      return Multivectors.Component (MV, C3_Base'Enum_Rep (C3_e3));
   end e3;

   --  -------------------------------------------------------------------------

   function e1_e2 (MV : Multivectors.Multivector) return float is
      use Interfaces;
      BM_E12   : constant Unsigned_32 := Unsigned_32 (C3_Base'Enum_Rep (C3_e1))
        or Unsigned_32 (C3_Base'Enum_Rep (C3_e2));
   begin
      return  Multivectors.Component (MV, BM_E12);
   end e1_e2;

   --  -------------------------------------------------------------------------

   function e1_e3 (MV : Multivectors.Multivector) return float is
      use Interfaces;
      BM_E13   : constant Unsigned_32 :=
                   Unsigned_32 (E3_Base'Enum_Rep (E3_e1)) or
        Unsigned_32 (E3_Base'Enum_Rep (E3_e3));
   begin
      return  Multivectors.Component (MV, BM_E13);
   end e1_e3;

   --  -------------------------------------------------------------------------

   function e2_e3 (MV : Multivectors.Multivector) return float is
      use Interfaces;
      BM_E23   : constant Unsigned_32 :=
                   Unsigned_32 (E3_Base'Enum_Rep (E3_e2)) or Unsigned_32 (E3_Base'Enum_Rep (E3_e3));
   begin
      return Multivectors.Component (MV, BM_E23);
   end e2_e3;

   --  -------------------------------------------------------------------------

   function E1_E2_E3 (MV : Multivectors.Multivector) return float is
      use Interfaces;
      BM   : constant Unsigned_32 :=
               Unsigned_32 (E3_Base'Enum_Rep (E3_e1)) or
        Unsigned_32 (E3_Base'Enum_Rep (E3_e2)) or Unsigned_32 (E3_Base'Enum_Rep (E3_e3));
   begin
      return Multivectors.Component (MV, BM);
   end E1_E2_E3;

   --  -------------------------------------------------------------------------

   function Element (C :  Multivectors.Circle; E : C3_Base) return float is
      use Multivectors;
      theBlade : constant Blade.Basis_Blade  :=
                   Get_Blade (Multivector (C), C3_Base'Enum_Rep (E));
   begin
      return Blade.Weight (theBlade);
   end Element;

   --  -------------------------------------------------------------------------

   function E1_E2_NI (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e1_e2_ni);
   end E1_E2_NI;

   --  -------------------------------------------------------------------------

   function E1_E3_NI (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e1_e3_ni);
   end E1_E3_NI;

   --  -------------------------------------------------------------------------

   function E2_E3_NI (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e2_e3_ni);
   end E2_E3_NI;

   function Get_Coords (V : Vector_E3) return GA_Maths.Float_3D is
   begin
      return (Float (V (GL.X)), Float (V (GL.Y)), Float (V (GL.Z)));
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function Get_Coords (NP : Multivectors.Normalized_Point)
                         return GA_Maths.Coords_Continuous_Array is
      use Blade.Blade_List_Package;
      Blades : constant Blade.Blade_List := Multivectors.Get_Blade_List (NP);
      Curs   : Cursor := Blades.First;
      Coords : GA_Maths.Coords_Continuous_Array (1 .. 4);
      Index  : Integer := 0;
   begin
      while Has_Element (Curs) and Index < 4 loop
         Index := Index + 1;
         Coords (Index) := Blade.Weight (Element (Curs));
         Next (Curs);
      end loop;
      return Coords;
   end Get_Coords;

   --  ------------------------------------------------------------------------

   function ni return Multivectors.Multivector is
      use Multivectors;
      Basis : Multivectors.M_Vector;
   begin
      Add_Blade (Basis, C3_ni, 1.0);
      return Basis;
   end ni;

   --  -------------------------------------------------------------------------
   --  no is based on C3GA set to no_t
   function no return Multivectors.Multivector is
      use Multivectors;
      Basis  : Multivectors.Normalized_Point;
   begin
      Add_Blade (Basis, C3_no, 1.0);
      return Basis;
   end no;

   --  -------------------------------------------------------------------------

   function NO_E1_E2 (C :  Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e1_e2_no);
   end NO_E1_E2;

   --  -------------------------------------------------------------------------

   function NO_E1_E3 (C :  Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e1_e3_no);
   end NO_E1_E3;

   --  -------------------------------------------------------------------------

   function NO_E1_NI (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e1_no_ni);
   end NO_E1_NI;

   --  -------------------------------------------------------------------------

   function NO_E2_E3 (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e2_e3_no);
   end NO_E2_E3;

   --  -------------------------------------------------------------------------

   function NO_E2_NI (C : Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e2_no_ni);
   end NO_E2_NI;

   --  -------------------------------------------------------------------------

   function NO_E3_NI (C :  Multivectors.Multivector) return float is
   begin
      return Element (C, C3_e3_no_ni);
   end NO_E3_NI;

   --  -------------------------------------------------------------------------

   function NO_E1_E2_E3_NI (MV : Multivectors.Multivector) return float is
   begin
      return Element (MV, C3_e1_e2_e3_no_ni);
   end NO_E1_E2_E3_NI;

   --  -------------------------------------------------------------------------

   function NP_Element (NP : Multivectors.Normalized_Point; E : C3_Base) return float is
      use Multivectors;
      theBlade : constant Blade.Basis_Blade  :=
                   Get_Blade (Multivector (NP), C3_Base'Enum_Rep (E));
   begin
      return Blade.Weight (theBlade);
   end NP_Element;

   --  -------------------------------------------------------------------------

   function E1b (NP : Multivectors.Normalized_Point) return float is
   begin
      return NP_Element (NP, C3_e1);
   end E1b;

   --  -------------------------------------------------------------------------

   function E2b (NP : Multivectors.Normalized_Point) return float is
   begin
      return NP_Element (NP, C3_e2);
   end E2b;

   --  -------------------------------------------------------------------------

   function E3b (NP : Multivectors.Normalized_Point) return float is
   begin
      return NP_Element (NP, C3_e3);
   end E3b;

   --  -------------------------------------------------------------------------

   function NIb (NP : Multivectors.Normalized_Point) return Float is
   begin
      return NP_Element (NP, C3_ni);
   end NIb;

   --  -------------------------------------------------------------------------

   function NOb (NP : Multivectors.Normalized_Point) return Float is
      pragma Unreferenced (NP);
   begin
      return 1.0;
   end NOb;

   --  -------------------------------------------------------------------------

   function C3GA_Point (V : Vector_E3) return C3GA_Normalized_Point is
      Result : C3GA_Normalized_Point := (1.0, 0.0, 0.0, 0.0, 0.0);
   begin
      Result (2) := Float (V (GL.X));
      Result (3) := Float (V (GL.Y));
      Result (4) := Float (V (GL.Z));
      Result (5) := -0.5 * Norm_E2 (V);
      return Result;
   end C3GA_Point;

   --  -------------------------------------------------------------------------
   --  Based on C3GA char *string
   function Multivector_String (MV : Multivectors.Multivector) return String is
      use Ada.Strings.Unbounded;
      use Blade.Blade_List_Package;
      use Interfaces;
      Blades            : constant Blade.Blade_List := Multivectors.Blades (MV);
      Curs              : Cursor := Blades.First;   --  k
      Char_Buff         : Unbounded_String := To_Unbounded_String ("");
      Float_Buff        : Unbounded_String;
      Result            : Unbounded_String;
      Grade_Usage       : constant Unsigned_32 := Unsigned_32 (Multivectors.Grade_Use (MV));
      Coord             : Float;
      Grade_Size        : Integer;
      Index_A           : Integer := 1;
      Index_BE          : Integer;
      Shift_Bit         : Unsigned_32;
   begin
      --  print all coordinates
      for Coord_Index in 0 .. 5 loop   --  i
         Shift_Bit := Shift_Left (1, Coord_Index);
         if (Grade_Usage and Shift_Bit) > 0 then
            --          Grade_Usage : constant array (0 .. 5) of Integer := (1, 5, 10, 10, 5, 1);
            --          Thus, in C3, grade 0 blades have 1 coordinate; grade 1 blades have 5 coordinats;
            Grade_Size := MV_Grade_Size (Coord_Index);
            Curs := Blades.First;   --  k
            for index_j in 0 .. Grade_Size - 1 loop
               Coord := MV_Basis_Element_Sign_By_Index (Index_A) *
                 Blade.Weight (Element (Curs));

               if Coord /= 0.0 then
                  Char_Buff := To_Unbounded_String ("");
                  Float_Buff := To_Unbounded_String (Float'Image (Abs (Coord)));
                  if Coord < 0.0 then
                     Char_Buff := Char_Buff & "-";
                  elsif index_j > 0 then
                     Char_Buff := Char_Buff & "+";
                  end if;

                  Char_Buff := Char_Buff & Float_Buff;
                  if Coord_Index > 0 then
                     Char_Buff :=  Char_Buff & " * ";
                     Index_BE := 1;
                     while MV_Basis_Elements (Index_A, Index_BE) >= 0 loop
                        if Index_BE /= 1 then
                           Char_Buff := Char_Buff & " ^ ";
                        end if;
                        Char_Buff := Char_Buff &
                          MV_Basis_Vector_Names (MV_Basis_Elements (Index_A, Index_BE));
                        Index_BE := Index_BE + 1;
                     end loop;
                     Char_Buff := Char_Buff & ASCII.LF & "    ";
                  end if;
                  Result := Result & Char_Buff;
               end if;

               Index_A := Index_A + 1;
               Next (Curs);
            end loop;  --  end index_j
         else
            Index_A := Index_A + MV_Grade_Size (Coord_Index);
         end if;
      end loop;  --  Grade_Index

      return To_String (Result);

   exception
      when others =>
         Put_Line ("An exception occurred in C3GA.Multivector_String.");
         raise;
   end Multivector_String;

   --  -------------------------------------------------------------------------

   function Norm_E2 (V : Vector_E3) return Float is
      use GL.Types;
   begin
      return Float (V (GL.X) * V (GL.X) + V (GL.Y) * V (GL.Y) + V (GL.Z) * V (GL.Z));
   end Norm_E2;

   --  -------------------------------------------------------------------------

   function NP_To_VectorE3GA (NP : Multivectors.Normalized_Point) return Vector_E3 is
      use GL.Types;
      theVector : Vector_E3;
   begin
      theVector (GL.X) := Single (Multivectors.Component (NP, 2));
      theVector (GL.Y) := Single (Multivectors.Component (NP, 3));
      theVector (GL.Z) := Single (Multivectors.Component (NP, 4));
      return theVector;
   end NP_To_VectorE3GA;

   --  -------------------------------------------------------------------------

   function Norm_R (V : Vector_E3) return Float is
      use GL.Types;
      use Maths.Single_Math_Functions;
   begin
      return Float (Sqrt (Single (Norm_R2 (V))));
   end Norm_R;

   --  -------------------------------------------------------------------------

   function Norm_R2 (V : Vector_E3) return Float is
      use GL.Types;
      theNorm : Single;
   begin
      theNorm := V (GL.X) * V (GL.Z) +  V (GL.Y) * V (GL.Y) +  V (GL.Z) * V (GL.X);
      theNorm := Abs (theNorm);
      return Float (theNorm);
   end Norm_R2;

   --  -------------------------------------------------------------------------

   function Probe (Pr : C3_Base) return Multivectors.Normalized_Point is
      NP  : Multivectors.Normalized_Point;
   begin
      --  thePoint.Origin of a Normalized_Point is a constant 1.0
      Multivectors.Add_Blade (NP, Blade.New_Basis_Blade (Pr, 1.0));
      return NP;
   end Probe;

   --  ------------------------------------------------------------------------

   function Set_Circle (P1, P2, P3 : Multivectors.Normalized_Point) return  Multivectors.Circle is
      use Multivectors;
      OP        : Multivector;
      theCircle :  Multivectors.Circle;
   begin
      OP := Outer_Product (Multivector (P1), Outer_Product (Multivector (P2),
                           Multivector (P3)));
      Multivectors.Update (theCircle, Get_Blade_List (OP));
      return theCircle;
   end Set_Circle;

   --  ------------------------------------------------------------------------

   procedure Set_Coords (V : out Vector_E3; C1, C2, C3 : float) is
      use GL.Types;
   begin
      V := (Single (C1), Single (C2), Single (C3));
   end Set_Coords;

   --  -------------------------------------------------------------------------

   function Set_Dual_Plane (P1 : Multivectors.Normalized_Point; Dir : Multivectors.M_Vector)
                             return Multivectors.Dual_Plane is
      use Multivectors;
      LC       : constant Multivector := Left_Contraction (P1, Outer_Product (Dir, ni));
      D_Plane  : Multivectors.Dual_Plane;
   begin
      Multivectors.Update (D_Plane, Get_Blade_List (LC));
      return D_Plane;
   end Set_Dual_Plane;

   --  ------------------------------------------------------------------------

   function Set_Line (P1, P2 : Multivectors.Normalized_Point)
                       return  Multivectors.Line is
      use Multivectors;
      OP   : constant Multivector :=
               Outer_Product (P1, Outer_Product (P2, ni));
      MV_X : constant Multivector := Unit_R (OP, Metric.C3_Metric);
   begin
      return To_Line (MV_X);
   end Set_Line;

   --  ------------------------------------------------------------------------
   --  From c3ga_util.h
   --  c3gaPoint is afunction that returns a normalizedPoint
   --  inline normalizedPoint c3gaPoint (const vectorE3GA &vl)
   --  return _normalizedPoint (vl + no + 0.5f * norm_e2 (vl) * ni);  no = ni = 1.0

   function Set_Normalized_Point (V : Vector_E3) return Multivectors.Normalized_Point is
      --        use GA_Maths.Complex_Types;
      use Multivectors;
      use Blade;
      NP  : Multivectors.Normalized_Point;
      --        NI  : constant Complex := (0.0, 1.0);
   begin
      --  thePoint.Origin of a Normalized_Point is a constant 1.0
      Add_Blade (NP, New_Basis_Blade (C3_no, 1.0));
      Add_Blade (NP, New_Basis_Blade (C3_e1, Float (V (GL.X))));
      Add_Blade (NP, New_Basis_Blade (C3_e2, Float (V (GL.Y))));
      Add_Blade (NP, New_Basis_Blade (C3_e3, Float (V (GL.Z))));
      Add_Blade (NP, New_Basis_Blade (C3_ni, 0.5 * (Norm_E2 (V))));
      return NP;
   end Set_Normalized_Point;

   --  ------------------------------------------------------------------------

   function Set_Normalized_Point (E1, E2, E3 : float)
                                   return Multivectors.Normalized_Point is
      use GL.Types;
      use Blade;
      use Multivectors;
      NP  : Multivectors.Normalized_Point;
   begin
      --  thePoint.Origin of a Normalized_Point is a constant 1.0
      Add_Blade (NP, Blade.New_Basis_Blade (C3_no, 1.0));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e1, E1));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e2, E2));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e3, E3));
      Add_Blade (NP, Blade.New_Basis_Blade
                 (C3_ni,  -0.5 * Norm_R ((Single (E1), Single (E2), Single (E3)))));
      return NP;
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function Set_Normalized_Point (Point : GA_Maths.Float_3D)
                                   return Multivectors.Normalized_Point is
      use GL.Types;
      use Blade;
      use Multivectors;
      NP : Normalized_Point;
   begin
      Add_Blade (NP, Blade.New_Basis_Blade (C3_no, 1.0));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e1, Point (1)));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e2, Point (2)));
      Add_Blade (NP, Blade.New_Basis_Blade (C3_e3, Point (3)));
      Add_Blade (NP, Blade.New_Basis_Blade
                 (C3_ni, -0.5 * Norm_R ((Single (Point (1)),
                    Single (Point (2)), Single (Point (3))))));
      return NP;
   end Set_Normalized_Point;

   --  -------------------------------------------------------------------------

   function To_VectorE3GA (MV : Multivectors.Multivector) return Vector_E3 is
      use Interfaces;
      use GL.Types;
--        use GA_Maths.Float_Functions;
      theVector : Vector_E3;
   begin
      GA_Utilities.Print_Multivector_String ("C3GA.To_VectorE3GA MV", MV,
                                             Blade_Types.Basis_Names_C3GA);
      GA_Utilities.Print_Multivector ("C3GA.To_VectorE3GA MV", MV);
      Put_Line ("C3GA.To_VectorE3GA C3_e3'Enum_Rep " &
               Unsigned_32'Image (C3_e3'Enum_Rep));
      theVector (GL.X) := Single (Multivectors.Component (MV, C3_e1'Enum_Rep) +
                                 Multivectors.Component (MV, C3_e1_no_ni'Enum_Rep));
      theVector (GL.Y) := Single (Multivectors.Component (MV, C3_e2'Enum_Rep) +
                                  Multivectors.Component (MV, C3_e2_no_ni'Enum_Rep));
      theVector (GL.Z) := Single (Multivectors.Component (MV, C3_e3'Enum_Rep) +
                                  Multivectors.Component (MV, C3_e3_no_ni'Enum_Rep));
      GA_Utilities.Print_E3_Vector
        ("C3GA.To_VectorE3GA theVector", theVector);
      return theVector;
   end To_VectorE3GA;

   --  -------------------------------------------------------------------------

   function To_VectorE3GA (Vec : E3GA.E3_Vector) return Vector_E3 is
   begin
      return Vec;
   end To_VectorE3GA;

   --  -------------------------------------------------------------------------

   function Unit_E (X : C3GA.Vector_E3) return GL.Types.Singles.Vector3 is
   begin
      return E3GA.Unit_E (X);
   end Unit_E;

   --  ------------------------------------------------------------------------

end C3GA;
