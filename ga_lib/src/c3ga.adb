
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Blade;
with Blade_Types; use Blade_Types;
--  with GA_Utilities;
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

    --     function Init (MV : Multivector.Multivector; Epsilon : float;
    --                    Use_Algebra_Metric : Boolean;
    --                    GU_Count : Integer) return MV_Type;

    --  -------------------------------------------------------------------------

    --     function "+" (V1 : Vector_C3GA; V2 : Vector_C3GA) return Vector_C3GA is
    --     begin
    --        return (V1 (1) + V2 (1), V1 (2) + V2 (2), V1 (3) + V2 (3),
    --                V1 (4) + V2 (4), V1 (5) + V2 (5));
    --     end  "+";

    --  -------------------------------------------------------------------------

    function "*" (L : Float; R : Vector_E3) return Vector_E3 is
        use GL.Types;
    begin
        return (Single (L) * R (GL.X), Single (L) * R (GL.Y), Single (L) * R (GL.Z));
    end  "*";

    --  -------------------------------------------------------------------------

    --     function "*" (L : Line; S : Float) return Line is
    --        use Multivectors;
    --        use Multivectors.Blade_List_Package;
    --        Blades   : constant Blade_List := Get_Blade_List (L);
    --        Curs     : Cursor := Blades.First;
    --        aBlade   : Blade.Basis_Blade;
    --        New_List : Blade_List;
    --        theLine  : Multivector;
    --     begin
    --        while Has_Element (Curs) loop
    --           aBlade := Element (Curs);
    --           Blade.Update_Blade (aBlade, S * Blade.Weight (aBlade));
    --           Add_Blade (theLine, aBlade);
    --           Next (Curs);
    --        end loop;
    --        Update (theLine, New_List);
    --        return Line (theLine);
    --     end  "*";

    --  -------------------------------------------------------------------------

    --     function "*" (S : Float; L : Line) return Line is
    --     begin
    --        return L * S;
    --     end  "*";

    --  -------------------------------------------------------------------------

    --     function C3GA_Point (V : Vector_E3) return Normalized_Point is
    --        thePoint : Normalized_Point := Multivectors.New_Normalized_Point;
    --     begin
    --        --  thePoint.Origin of a Normalized_Point is a constant 1.0
    --        thePoint.E1 := V.Coordinates (1);
    --        thePoint.E2 := V.Coordinates (2);
    --        thePoint.E3 := V.Coordinates (3);
    --        thePoint.Inf := 0.5 * Norm_E2(V).Coordinates (1) * GA_Base_Types.NI;
    --        return thePoint;
    --     end C3GA_Point;

    --  ------------------------------------------------------------------------
    --
    --     function "+" (L, R : Vector_E3) return Vector_E3 is
    --          L_Coords : constant E3GA.Vector := L.Coordinates;
    --          R_Coords : constant E3GA.Vector := R.Coordinates;
    --          Result   : Vector_E3;
    --     begin
    --        Result.Coordinates (1) := L_Coords (1) + R_Coords (1);
    --        Result.Coordinates (2) := L_Coords (2) + R_Coords (2);
    --        Result.Coordinates (3) := L_Coords (3) + R_Coords (3);
    --        return Result;
    --     end "+";

    --  -------------------------------------------------------------------------

    --     function "-" (L, R : Vector_E3) return Vector_E3 is
    --          L_Coords : constant E3GA.Vector := L.Coordinates;
    --          R_Coords : constant E3GA.Vector := R.Coordinates;
    --          Result   : Vector_E3;
    --     begin
    --        Result.Coordinates (1) := L_Coords (1) - R_Coords (1);
    --        Result.Coordinates (2) := L_Coords (2) - R_Coords (2);
    --        Result.Coordinates (3) := L_Coords (3) - R_Coords (3);
    --        return Result;
    --     end "-";

    --  -------------------------------------------------------------------------

    --     function "*" (L : float; R : Vector_E3) return Vector_E3 is
    --          R_Coords : constant E3GA.Vector := R.Coordinates;
    --          Result   : Vector_E3;
    --     begin
    --        Result.Coordinates (1) := L * R_Coords (1);
    --        Result.Coordinates (2) := L * R_Coords (2);
    --        Result.Coordinates (3) := L * R_Coords (3);
    --        return Result;
    --     end "*";

    --  -------------------------------------------------------------------------

    --     function "*" (L : Vector_E3; R : float) return Vector_E3 is
    --          L_Coords : constant E3GA.Vector := L.Coordinates;
    --          Result   : Vector_E3;
    --     begin
    --        Result.Coordinates (1) := R * L_Coords (1);
    --        Result.Coordinates (2) := R * L_Coords (2);
    --        Result.Coordinates (3) := R * L_Coords (3);
    --        return Result;
    --     end "*";

    --  -------------------------------------------------------------------------

    --     function Coord (S : Multivectors.Scalar) return float is
    --     begin
    --        return Multivectors.Scalar_Part (S);
    --     end Coord;

    --  -------------------------------------------------------------------------

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

    function e1_e2_e3 (MV : Multivectors.Multivector) return float is
        use Interfaces;
        BM   : constant Unsigned_32 :=
                 Unsigned_32 (E3_Base'Enum_Rep (E3_e1)) or
          Unsigned_32 (E3_Base'Enum_Rep (E3_e2)) or Unsigned_32 (E3_Base'Enum_Rep (E3_e3));
    begin
        return Multivectors.Component (MV, BM);
    end e1_e2_e3;

    --  -------------------------------------------------------------------------

    --     function Grade_Use (MV : Multivector) return GA_Maths.Unsigned_32  is
    --     begin
    --        return MV.Grade_Use;
    --     end Grade_Use;

    --  ------------------------------------------------------------------------

    --     function Init (MV : Multivector; Epsilon : float := 0.0) return MV_Type is
    --        use Interfaces;
    --        use GA_Maths;
    --        use  Multivector_Type_Base;
    --        MV_Info            : MV_Type;
    --        GU                 : GA_Maths.Grade_Usage := Grade_Use (MV);
    --        Count              : array (Unsigned_32 range 1 .. 2) of Integer := (0, 0);
    --        Count_Index        : Unsigned_32 := 0;
    --        Index              : Unsigned_32 := 0;
    --        Done               : Boolean := False;
    --     begin
    --        MV_Info.M_Type := Multivector_Object;
    --        MV_Info.M_Grade_Use := GU;
    --        --  count grade part usage
    --        while GU /= 0 loop
    --           if (GU and GU_1) /= 0 then  --  c3ga.cpp line 21731
    --              Index := Count_Index and US_1;
    --              Count (Index) := Count (Index) + 1;
    --           end if;
    --           GU := Unsigned_32 (Shift_Right (Unsigned_32 (GU), 1));
    --           MV_Info.M_Grade := Integer (Count_Index);
    --           Count_Index := Count_Index + 1;
    --        end loop;
    --
    --        --  if no grade part in use: zero blade
    --        if Count (1) = 0 and then Count (2) = 0  then  --  this is a zero blade
    --           Put_Line ("C3GA.Init 1 Setting zero blade.");
    --           Set_Type_Base (MV_Info, True, Blade_MV, 0, GU, Even_Parity);
    --           Done := True;
    --        else
    --           --  Base.M_Zero = False by default
    --           if Count (1) /= 0 and then Count (2) /= 0  then
    --              --  Base.M_Parity = No_Parity by default
    --              Done := True;
    --           else
    --              if Count (1) = 0 then
    --                 Put_Line ("C3GA.Init 1 Setting even parity.");
    --                 MV_Info.M_Parity := Even_Parity;
    --              else
    --                 --                 Put_Line ("C3GA.Init 1 Setting odd parity.");
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
    --           Put_Line ("An exception occurred in C3GA.Init 1.");
    --           raise;
    --     end Init;

    -------------------------------------------------------------------------

    --     function Init (MV : C3GA.Multivector; Epsilon : float;
    --                    Use_Algebra_Metric : Boolean;
    --                    GU_Count : Integer) return MV_Type is
    --        MV_Info : MV_Type;
    --     begin
    --        --  To be completed.
    --        return MV_Info;
    --     end Init;

    --  -------------------------------------------------------------------------

    function Element (C :  Multivectors.Circle; E : C3_Base) return float is
        use Multivectors;
        theBlade : constant Blade.Basis_Blade  :=
                     Get_Blade (Multivector (C), C3_Base'Enum_Rep (E));
    begin
        return Blade.Weight (theBlade);
    end Element;

    --  -------------------------------------------------------------------------

    function Line_Element (L :  Multivectors.Line; E : C3_Base) return float is
        use Multivectors;
        theBlade : constant Blade.Basis_Blade  :=
                     Get_Blade (Multivector (L), C3_Base'Enum_Rep (E));
    begin
        return Blade.Weight (theBlade);
    end Line_Element;

    --  -------------------------------------------------------------------------

    function E1_E2_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e1_e2_ni);
    end E1_E2_NI;

    --  -------------------------------------------------------------------------

    function E1_E3_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e1_e3_ni);
    end E1_E3_NI;

    --  -------------------------------------------------------------------------

    --     function E1_E2_E3 (C : Circle) return float is
    --     begin
    --        return Element (C, C3_e1_e2_e3);
    --     end E1_E2_E3;

    --  -------------------------------------------------------------------------

    function E2_E3_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e2_e3_ni);
    end E2_E3_NI;

    function Get_Coords (V : Vector_E3) return GA_Maths.Float_3D is
    begin
        return (Float (V (GL.X)), Float (V (GL.Y)), Float (V (GL.Z)));
    end Get_Coords;

    --  ------------------------------------------------------------------------

    --     function Get_Coords (NP : Normalized_Point) return M_Vector is
    --     begin
    --        return (1.0, NP.E1, NP.E2, NP.E3, NP.Inf);
    --     end Get_Coords;

    ------------------------------------------------------------------------

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

    function NO_E1_E2 (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e1_e2_no);
    end NO_E1_E2;

    --  -------------------------------------------------------------------------

    function NO_E1_E3 (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e1_e3_no);
    end NO_E1_E3;

    --  -------------------------------------------------------------------------

    function NO_E1_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e1_no_ni);
    end NO_E1_NI;

    --  -------------------------------------------------------------------------

    function NO_E2_E3 (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e2_e3_no);
    end NO_E2_E3;

    --  -------------------------------------------------------------------------

    function NO_E2_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e2_no_ni);
    end NO_E2_NI;

    --  -------------------------------------------------------------------------

    function NO_E3_NI (C :  Multivectors.Circle) return float is
    begin
        return Element (C, C3_e3_no_ni);
    end NO_E3_NI;

    --  -------------------------------------------------------------------------

    --     function E1b (DP : Dual_Plane) return float is
    --     begin
    --        return DP.E1;
    --     end E1b;

    --  -------------------------------------------------------------------------

    --     function E2b (DP : Dual_Plane) return float is
    --     begin
    --        return DP.E2;
    --     end E2b;

    --  -------------------------------------------------------------------------

    --     function E3b (DP : Dual_Plane) return float is
    --     begin
    --        return DP.E3;
    --     end E3b;

    --  -------------------------------------------------------------------------

    --     function NIb (DP : Dual_Plane) return GA_Base_Types.NI_T is
    --     begin
    --        return DP.Inf;
    --     end NIb;

    --  -------------------------------------------------------------------------

    --     function Element (L : Line; E : C3_Base) return float is
    --        use Blade_Types;
    --        use Multivectors;
    --        theBlade : constant Blade.Basis_Blade  :=
    --          Get_Blade (Multivector (L), C3_Base'Enum_Rep (E));
    --     begin
    --        return Blade.Weight (theBlade);
    --     end Element;

    --  -------------------------------------------------------------------------

    --     function E1_E2_NI (L :  Multivectors.Line) return float is
    --     begin
    --        return Element (L, C3_e1_e2_ni);
    --     end E1_E2_NI;

    --  -------------------------------------------------------------------------

    --     function E1_E3_NI (L :  Multivectors.Line) return float is
    --     begin
    --        return Element (L, C3_e1_e3_ni);
    --     end E1_E3_NI;

    --  -------------------------------------------------------------------------

    --     function E2_E3_NI (L :  Multivectors.Line) return float is
    --     begin
    --        return Element (L, C3_e2_e3_ni);
    --     end E2_E3_NI;

    --  -------------------------------------------------------------------------

    function E1_NO_NI (L :  Multivectors.Line) return float is
    begin
        return Line_Element (L, C3_e1_no_ni);
    end E1_NO_NI;

    --  -------------------------------------------------------------------------

    function E2_NO_NI (L :  Multivectors.Line) return float is
    begin
        return Line_Element (L, C3_e2_no_ni);
    end E2_NO_NI;

    --  -------------------------------------------------------------------------

    function E3_NO_NI (L :  Multivectors.Line) return float is
    begin
        return Line_Element (L, C3_e3_no_ni);
    end E3_NO_NI;

    --  -------------------------------------------------------------------------

    --     function Element (MV : Multivectors.Multivector; E : C3_Base) return float is
    --        use Blade_Types;
    --        use Multivectors;
    --        theBlade : constant Blade.Basis_Blade  :=
    --          Get_Blade (Multivector (MV), C3_Base'Enum_Rep (E));
    --     begin
    --        return Blade.Weight (theBlade);
    --     end Element;

    --  -------------------------------------------------------------------------

    function NO_E1_E2_E3_NI (MV : Multivectors.Multivector) return float is
    begin
        return Element (MV, C3_e1_e2_e3_no_ni);
    end NO_E1_E2_E3_NI;

    --  -------------------------------------------------------------------------

    --     function NO_E1_E2_E3_NI (MV : Multivectors.Multivector) return float is
    --        use GA_Maths;
    --        GU         : Grade_Usage := Multivectors.Grade_Use (MV);
    --        GU_32      : constant Grade_Usage := 32;
    --        Grade_Size : Integer;
    --        MV2        : Multivectors.Multivector;
    --     begin
    --        if (GU and GU_32) = 0 then
    --           return 0.0;
    --        else
    --           Grade_Size := MV_Grade_Size (Integer(GU and GU_32));
    --           --              MV2 := Multivectors.Get_Basis_Vector (Blade.Base'Enum_Val (Grade_Size));
    --           return 0.0;
    --        end if;
    --     end NO_E1_E2_E3_NI;

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

    function Sphere_Element (S : Sphere; E : C3_Base) return float is
        use Multivectors;
        theBlade : constant Blade.Basis_Blade  :=
                     Get_Blade (Multivector (S), C3_Base'Enum_Rep (E));
    begin
        return Blade.Weight (theBlade);
    end Sphere_Element;

    --  -------------------------------------------------------------------------

    function E1_E2_E3_NI (S : Sphere) return float is
    begin
        return Sphere_Element (S, C3_e1_e2_e3_ni);
    end E1_E2_E3_NI;

    --  -------------------------------------------------------------------------

    function E1_E2_NO_NI (S : Sphere) return float is
    begin
        return Sphere_Element (S, C3_e1_e2_no_ni);
    end E1_E2_NO_NI;

    --  -------------------------------------------------------------------------

    function E1_E3_NO_NI (S : Sphere) return float is
    begin
        return Sphere_Element (S, C3_e1_e3_no_ni);
    end E1_E3_NO_NI;

    --  -------------------------------------------------------------------------

    function E2_E3_NO_NI (S : Sphere) return float is
    begin
        return Sphere_Element (S, C3_e2_e3_no_ni);
    end E2_E3_NO_NI;

    --  -------------------------------------------------------------------------

    function E1_E2_E3_NO (S : Sphere) return float is
    begin
        return Sphere_Element (S, C3_e1_e2_e3_no);
    end E1_E2_E3_NO;

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

    --        function Norm_E (MV : Multivector) return Scalar is
    --           use GA_Maths;
    --           GU  : Grade_Usage :=  Grade_Use (MV);
    --           Sum : Float := 0.0;
    --           E2  : Scalar;
    --        begin
    --           if (GU and GU_0) /= 0 then
    --              Sum := MV.Coordinates (1) * MV.Coordinates (1);
    --           end if;
    --           if (GU and GU_1) /= 0 then
    --              For index in 2 .. 6 loop
    --                 Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
    --              end loop;
    --           end if;
    --           if (GU and GU_2) /= 0 then
    --              For index in 7 .. 16 loop
    --                 Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
    --              end loop;
    --           end if;
    --           if (GU and GU_4) /= 0 then
    --              For index in 17 .. 26 loop
    --                 Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
    --              end loop;
    --           end if;
    --           if (GU and GU_8) /= 0 then
    --              For index in 27 .. 31 loop
    --                 Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
    --              end loop;
    --           end if;
    --           if (GU and GU_16) /= 0 then
    --              Sum := Sum + MV.Coordinates (32) * MV.Coordinates (32);
    --           end if;
    --           E2.Coordinates (1) := Sum;
    --           return E2;
    --        end Norm_E;

    --  -------------------------------------------------------------------------

    function Norm_E2 (V : Vector_E3) return Float is
        use GL.Types;
    begin
        return Float (V (GL.X) * V (GL.X) + V (GL.Y) * V (GL.Y) + V (GL.Z) * V (GL.Z));
    end Norm_E2;

    --  -------------------------------------------------------------------------

    function Norm_R (V : Vector_E3) return Float is
        use GL.Types;
        use Maths.Single_Math_Functions;
    begin
        return Float (Sqrt (Single (Norm_R2 (V))));
    end Norm_R;

    --  -------------------------------------------------------------------------

    --      function Norm_R (MV : Multivectors.Multivector) return Float is
    --          use GA_Maths.Float_Functions;
    --          use Multivectors;
    --          DP     : constant float := Scalar_Part (Dot (MV, MV));
    --          Result : Float := 0.0;
    --      begin
    --          if DP /= 0.0 then
    --              if DP < 0.0 then
    --                  Result := -Sqrt (-DP);
    --              else
    --                  Result := Sqrt (DP);
    --              end if;
    --          end if;
    --          return Result;
    --      end Norm_R;

    --  ------------------------------------------------------------------------

    function Norm_R2 (V : Vector_E3) return Float is
        use GL.Types;
        theNorm : Single;
    begin
        theNorm := V (GL.X) * V (GL.Z) +  V (GL.Y) * V (GL.Y) +  V (GL.Z) * V (GL.X);
        theNorm := Abs (theNorm);
        return Float (theNorm);
    end Norm_R2;

    --  -------------------------------------------------------------------------

    --      function Norm_Rsq (MV : Multivectors.Multivector) return Float is
    --          use GA_Maths;
    --          use Multivectors;
    --          GU  : constant Grade_Usage :=  Grade_Use (MV);
    --          Sum : Float := 0.0;
    --      begin
    --          if (GU and GU_0) /= 0 then
    --              Sum := Component (MV, 1) * Component (MV, 1);
    --          end if;
    --          if (GU and GU_1) /= 0 then
    --  --              For index in 2 .. 6 loop
    --               Sum := Sum - 2.0 * Component (MV, 2) * Component (MV, 6) +
    --               Component (MV, 3) * Component (MV, 3) +
    --               Component (MV, 4) * Component (MV, 4) +
    --               Component (MV, 5) * Component (MV, 5);
    --  --              end loop;
    --          end if;
    --          if (GU and GU_2) /= 0 then
    --  --              For index in 7 .. 16 loop
    --               Sum := Sum + 2.0 + Component (MV, 7) * Component (MV, 13) +
    --               2.0 * Component (MV, 8) * Component (MV, 14) +
    --               Component (MV, 9) * Component (MV, 15) +
    --               Component (MV, 10) * Component (MV, 10) +
    --               Component (MV, 11) * Component (MV, 11) +
    --               Component (MV, 12) * Component (MV, 12) +
    --               Component (MV, 16) * Component (MV, 16);
    --  --              end loop;
    --          end if;
    --          if (GU and GU_4) /= 0 then
    --  --              For index in 17 .. 26 loop
    --               Sum := Sum +
    --               2.0 * Component (MV, 17) * Component (MV, 23) +
    --               2.0 * Component (MV, 18) * Component (MV, 24) +
    --               2.0 * Component (MV, 19) * Component (MV, 25) +
    --               Component (MV, 20) * Component (MV, 20) +
    --               Component (MV, 21) * Component (MV, 21) +
    --               Component (MV, 22) * Component (MV, 22) -
    --               Component (MV, 26) * Component (MV, 26);
    --  --              end loop;
    --          end if;
    --  --          if (GU and GU_8) /= 0 then
    --  --              For index in 27 .. 31 loop
    --  --                  Sum := Sum + MV.Coordinates (index) * MV.Coordinates (index);
    --  --              end loop;
    --  --          end if;
    --  --          if (GU and GU_16) /= 0 then
    --  --              Sum := Sum + MV.Coordinates (32) * MV.Coordinates (32);
    --  --          end if;
    --  --          E2.Coordinates (1) := Sum;
    --          return Sum;
    --
    --      end Norm_Rsq;

    --  ------------------------------------------------------------------------

    --     function Outer_Product (NP1, NP2 : Normalized_Point) return Line is
    --        use Multivectors;
    --     begin
    --        return Line (Outer_Product (Multivector (NP1), Multivector (NP2)));
    --     end Outer_Product;

    --  ------------------------------------------------------------------------

    --     function Outer_Product (L1, L2 : Line) return Line is
    --        use Multivectors;
    --     begin
    --        return Line (Outer_Product (Multivector (L1), Multivector (L2)));
    --     end Outer_Product;

    --  ------------------------------------------------------------------------

    --     function Outer_Product (NP : Normalized_Point; MV : Multivectors.Multivector)
    --                             return Normalized_Point is
    --        use Multivectors;
    --     begin
    --        return Normalized_Point (Outer_Product (Multivector (NP), MV));
    --     end Outer_Product;

    --  ------------------------------------------------------------------------

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

    --     function Set_Coords (C1, C2, C3 : float) return Vector_E3 is
    --        use GL.Types;
    --     begin
    --        return (Single (C1), Single (C2), Single (C3));
    --     end Set_Coords;

    --  -------------------------------------------------------------------------

    --     procedure Set_Coords (P : out Point; Origin, C1, C2, C3, Inf : float) is
    --        use GA_Base_Types;
    --     begin
    --        Set_NO (P.Origin, Origin);
    --        P.E1 := C1;
    --        P.E2 := C2;
    --        P.E3 := C3;
    --        P.Inf := Inf;
    --     end Set_Coords;

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

    --     function Set_Line (E1_E2_NI, E1_E3_NI, E2_E3_NI,
    --                        E1_NO_NI, E2_NO_NI, E3_NO_NI : Float) return Line is
    --        use Blade;
    --        theLine : Line;
    --     begin
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e1_e2_ni, E1_E2_NI));
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e1_e3_ni, E1_E3_NI));
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e2_e3_ni, E2_E3_NI));
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e1_no_ni, E1_NO_NI));
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e2_no_ni, E2_NO_NI));
    --        Add_Blade (theLine, Blade.New_Basis_Blade (C3_e3_no_ni, E3_NO_NI));
    --        return theLine;
    --     end Set_Line;

    --  -------------------------------------------------------------------------

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
        use GL.Types;
        theVector : Vector_E3;
    begin
        theVector (GL.X) := Single (Multivectors.Component (MV, C3_e1'Enum_Rep));
        theVector (GL.Y) := Single (Multivectors.Component (MV, C3_e2'Enum_Rep));
        theVector (GL.Z) := Single (Multivectors.Component (MV, C3_e3'Enum_Rep));
        return theVector;
    end To_VectorE3GA;

    --  -------------------------------------------------------------------------

    function To_VectorE3GA (Vec : E3GA.E3_Vector) return Vector_E3 is
    begin
        return Vec;
    end To_VectorE3GA;

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

    --     function Outer_Product (MV1, MV2 : Multivector) return Multivector is
    --        use GA_Maths;
    --        Coords  : GA_Maths.Coords_Continuous_Array (1 .. 32);
    --        GU1     : Grade_Usage := MV1.Grade_Use;
    --        GU2     : Grade_Usage := MV2.Grade_Use;
    --        Size_1  : integer := MV_Size (Integer (MV1.Grade_Use));
    --        Size_2  : integer := MV_Size (Integer (MV2.Grade_Use));
    --        MV_GU   : Grade_Usage := GU1 or GU2;
    --        Sum     : Float := 0.0;
    --        Product : Multivector (MV_GU);
    --
    --        function Grade_Used (MV : Multivector; Index : Integer) return Boolean is
    --           GU     : Grade_Usage := MV1.Grade_Use;
    --           Result : Boolean := False;
    --        begin
    --           case Index is
    --                 when 0 => Result := (GU and GU_0) /= 0;
    --                 when 1 => Result := (GU and GU_1) /= 0;
    --                 when 2 => Result := (GU and GU_2) /= 0;
    --                 when 3 => Result := (GU and GU_4) /= 0;
    --                 when 4 => Result := (GU and GU_8) /= 0;
    --                 when 5 => Result := (GU and GU_16) /= 0;
    --                 when others =>
    --                     Put_Line ("C3GA.Outer_Product Invalid Index");
    --           end case;
    --           return Result;
    --        end Grade_Used;
    --
    --     begin
    --           for Index2 in 1 ..32 loop
    --              if Grade_Used (MV2, Integer (GU1)) then
    --                 for Index1 in 1 .. 32 loop
    --                    null;
    --                 end loop;
    --              end if;
    --           end loop;
    --
    --           if (GU2 and GU_0) /= 0 then
    --              if (GU1 and GU_1) /= 0 then
    --                 Coords (1) := MV1.Coordinates (1) * MV2.Coordinates (1);
    --              end if;
    --              if (GU1 and GU_1) /= 0 then
    --                 For index in 2 .. 6 loop
    --                    Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
    --                 end loop;
    --              end if;
    --              if (GU1 and GU_2) /= 0 then
    --                 For index in 7 .. 16 loop
    --                    Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
    --                 end loop;
    --              end if;
    --              if (GU1 and GU_4) /= 0 then
    --                 For index in 17 .. 26 loop
    --                    Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
    --                 end loop;
    --              end if;
    --              if (GU1 and GU_8) /= 0 then
    --                 For index in 27 .. 31 loop
    --                    Coords (index) := MV1.Coordinates (index) * MV2.Coordinates (1);
    --                 end loop;
    --              end if;
    --              if (GU1 and GU_16) /= 0 then
    --                 Coords (32) := MV1.Coordinates (32) * MV2.Coordinates (1);
    --              end if;
    --           end if;
    --
    --           if (GU2 and GU_1) /= 0 then
    --              if (GU1 and GU_1) /= 0 then
    --                 For index in 2 .. 6 loop
    --                    Coords (index) := Coords (index) +
    --                      MV1.Coordinates (1) * MV2.Coordinates (index);
    --                 end loop;
    --              end if;
    --
    --              if (GU1 and GU_2) /= 0 then
    --                 Coords (7) := Coords (7) +
    --                   MV1.Coordinates (2) * MV2.Coordinates (3) -
    --                   MV1.Coordinates (3) * MV2.Coordinates (2);
    --                 Coords (8) := Coords (8) +
    --                   MV1.Coordinates (2) * MV2.Coordinates (4) -
    --                   MV1.Coordinates (4) * MV2.Coordinates (2);
    --                 Coords (9) := Coords (9) +
    --                   MV1.Coordinates (2) * MV2.Coordinates (5) -
    --                   MV1.Coordinates (5) * MV2.Coordinates (2);
    --                 Coords (10) := Coords (10) +
    --                   MV1.Coordinates (3) * MV2.Coordinates (4) -
    --                   MV1.Coordinates (4) * MV2.Coordinates (3);
    --                 Coords (11) := Coords (11) +
    --                   MV1.Coordinates (4) * MV2.Coordinates (5) -
    --                   MV1.Coordinates (5) * MV2.Coordinates (4);
    --                 Coords (12) := Coords (12) +
    --                   MV1.Coordinates (5) * MV2.Coordinates (3) -
    --                   MV1.Coordinates (3) * MV2.Coordinates (5);
    --                 Coords (13) := Coords (13) +
    --                   MV1.Coordinates (3) * MV2.Coordinates (6) -
    --                   MV1.Coordinates (6) * MV2.Coordinates (3);
    --              end if;
    --           end if;
    --           return Product;
    --     end Outer_Product;

    --  -------------------------------------------------------------------------

    --     function Unit_R (L : Line) return Line is
    --        use Blade;
    --        use Blade_Types;
    --        use GA_Maths.Float_Functions;
    --        use Multivectors;
    --        Blade_1 : constant Basis_Blade :=
    --          Get_Blade (Multivector (L), C3_Base'Enum_Rep (C3_e1_no_ni));
    --        Blade_2 : constant Basis_Blade :=
    --          Get_Blade (Multivector (L), C3_Base'Enum_Rep (C3_e2_no_ni));
    --        Blade_3 : constant Basis_Blade :=
    --          Get_Blade (Multivector (L), C3_Base'Enum_Rep (C3_e3_no_ni));
    --        R_Sq : constant float := -(Weight (Blade_1) * Weight (Blade_1) +
    --                                   Weight (Blade_2) * Weight (Blade_2) +
    --                                   Weight (Blade_3) * Weight (Blade_3));
    --        Inv  : constant float := 1.0 / Sqrt (Abs (R_Sq));
    --     begin
    --
    --        return L * Inv;
    --     end Unit_R;

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
    --     function US_Set_Normalized_Point (Point : Vector_E3) return Normalized_Point is
    --        NP : Normalized_Point;
    --     begin
    --        NP.E1 := Point.Coordinates (1);
    --        NP.E2 := Point.Coordinates (2);
    --        NP.E3 := Point.Coordinates (3);
    --        NP.Inf := 0.0;
    --        return NP;
    --     end US_Set_Normalized_Point;

    --  -------------------------------------------------------------------------

    function Unit_E (X : C3GA.Vector_E3) return GL.Types.Singles.Vector3 is
    begin
        return E3GA.Unit_E (X);
    end Unit_E;

    --  ------------------------------------------------------------------------

end C3GA;
