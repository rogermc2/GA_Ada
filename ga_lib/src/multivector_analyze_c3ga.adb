
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with E3GA;
with GA_Maths;
with GA_Utilities;
with Multivector_Type;

package body Multivector_Analyze_C3GA is

   procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                           MV : Multivectors.Multivector;
                           Probe : C3GA.Normalized_Point);
   procedure Analyze_Free (theAnalysis : in out MV_Analysis;
                           MV : Multivectors.Multivector);
   procedure Analyze_Round (theAnalysis : in out MV_Analysis;
                            MV : Multivectors.Multivector);
   procedure Analyze_Tangent (theAnalysis : in out MV_Analysis;
                              MV : Multivectors.Multivector);

   --  -------------------------------------------------------------------------

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Probe : C3GA.Normalized_Point;
                      Flags : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon) is
      use Multivector_Analyze;
      use Multivector_Type;
      use GA_Maths;

      MV_X      : Multivectors.Multivector := MV;
      MV_Info   : Multivector_Type.MV_Type_Record;
      Analysis  : MV_Analysis;

      procedure Classify is
         use Multivectors;
         OP_Nix_Val : constant Float := Norm_E (Outer_Product (C3GA.ni, MV_X));
         IP_Nix_Val : constant Float := Norm_E (Left_Contraction (C3GA.ni, MV_X));
         X2_Val     : constant Float := E3GA.Norm_R2 (MV_X);
         OP_Nix     : constant Boolean := Abs (OP_Nix_Val) >= Epsilon;
         IP_Nix     : constant Boolean := Abs (IP_Nix_Val) >= Epsilon;
         X2         : constant Boolean := Abs (X2_Val) >= Epsilon;
      begin
         if not OP_Nix and not IP_Nix then
            Analyze_Free (theAnalysis, MV);
         elsif not OP_Nix and IP_Nix then
            Analyze_Flat (theAnalysis, MV, Probe);
         elsif OP_Nix and not IP_Nix then
            Analysis.M_Flags.Dual := not Analysis.M_Flags.Dual;
            Analyze_Flat (theAnalysis, Dual (MV), Probe);
         elsif OP_Nix and IP_Nix and not X2 then
            Analyze_Tangent (theAnalysis, MV);
         elsif OP_Nix and IP_Nix and X2 then
            Analyze_Round (theAnalysis, MV);
         end if;
      end Classify;

   begin
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Conformal_Model;

      if Flags.Dual then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Is Dual.");
         Analysis.M_Flags.Dual := True;
         --           MV_X := C3GA.Dual (MV_X);
      end if;

      --        MV_Info := C3GA.Init (MV_X, Epsilon);
      MV_Info := Init (MV_X);
      Analysis.M_MV_Type := MV_Info;
      Print_Multivector_Info ("Multivector_Analyze_C3GA.Analyze MV_Info", MV_Info);
      New_Line;
      --        Analysis.M_Type.Multivector_Kind := MV_Info.M_Type;

      --  Check for zero blade
      --        if Analysis.M_MV_Type.M_Zero then
      if Zero (Analysis.M_MV_Type) then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Zero_Blade.");
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.M_Scalors (1) := 0.0;

      elsif MV_Kind (Analysis.M_MV_Type) = Versor_MV then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
         Analysis.M_Type.Versor_Subclass := Even_Versor;
         Analysis.M_Vectors (1) := E3GA.e1;

      elsif Grade_Use (Analysis.M_MV_Type) = 1 then  --  Grade 0
         Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 1.");
         Analysis.M_Type.Blade_Class := Scalar_Blade;
         Analysis.M_Type.M_Grade := 1;
         --           Analysis.M_Scalors (1) := MV_X.Coordinates (1);

      elsif Grade_Use (Analysis.M_MV_Type) = 6 then  --  Grade 5
         Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 6.");
         Analysis.M_Type.Blade_Class := Scalar_Blade;
         Analysis.M_Type.M_Grade := 6;
         Analysis.M_Scalors (1) := C3GA.NO_E1_E2_E3_NI (MV);
      else
         Classify;
         --  TO BE COMPLETED
         Put_Line ("Multivector_Analyze_C3GA.Analyze Multivector Type.");
      end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze.");
         raise;
   end Analyze;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                           MV : Multivectors.Multivector;
                           Probe : C3GA.Normalized_Point) is
      use GA_Maths;
      use Multivectors;
      Grade         : Unsigned_Integer :=
        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Attitude      : Multivector := Negate (Left_Contraction (C3GA.ni, MV));
      Location      : Multivector;
      Blade_Factors : Multivectors.Multivector_List;
      Scale         : Float;
      Weight        : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Flat_Blade;
      if theAnalysis.M_Flags.Dual then
         Grade := 5 - Grade;
      end if;
      Location := Left_Contraction (Left_Contraction (Multivector (Probe), MV),
                                    General_Inverse (MV));
      Location := Geometric_Product (Location,
                                     -1.0 / Scalar_Product (C3GA.ni, Location));
      if Grade = 1 then
         Weight := Scalar_Product (MV, C3GA.no);
      else
         Weight := Abs (E3GA.Norm_R (MV));
      end if;
      theAnalysis.M_Points (1) := Location;
      theAnalysis.M_Scalors (1) := Weight;
      case Grade is
         when 1 => theAnalysis.M_Type.MV_Subtype := Scalar_Type;
         when 2 => theAnalysis.M_Type.MV_Subtype := Point_Type;
         when 3 =>  --  Line
            theAnalysis.M_Type.MV_Subtype := Line_Type;
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, MV));
         when 4 =>  --  Plane
            theAnalysis.M_Type.MV_Subtype := Plane_Type;
            Blade_Factors := GA_Utilities.Factorize_Blade
              (Reverse_MV (Left_Contraction (C3GA.no, Reverse_MV (Attitude))), Scale);

            theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
            theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
            theAnalysis.M_Vectors (3) :=
              -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                     theAnalysis.M_Vectors (2)));
         when others => null;
      end case;
   end Analyze_Flat;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Free (theAnalysis : in out MV_Analysis;
                           MV : Multivectors.Multivector) is
      use Multivectors;
      Grade         : constant GA_Maths.Unsigned_Integer :=
        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Weight        : constant Float := Norm_E (MV);
      Attitude      : Multivector := MV;
      Blade_Factors : Multivectors.Multivector_List;
      Scale         : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Free_Blade;
      theAnalysis.M_Points (1) := Basis_Vector (Blade_Types.C3_no);
      theAnalysis.M_Scalors (1) := Weight;
      case Grade is
         when 1 => theAnalysis.M_Type.MV_Subtype := Scalar_Type;
         when 2 =>  --  F Vector
            theAnalysis.M_Type.MV_Subtype := Vector_Type;
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, MV));
         when 3 =>  --  F Bivector
            theAnalysis.M_Type.MV_Subtype := Bivector_Type;
            Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
            theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
            theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
            theAnalysis.M_Vectors (3) :=
              -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                     theAnalysis.M_Vectors (2)));
         when 4 =>  --  F Trivector
            theAnalysis.M_Type.MV_Subtype := Trivector_Type;
            theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
            theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
            theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
         when others => null;
      end case;
   end Analyze_Free;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Round (theAnalysis : in out MV_Analysis;
                            MV : Multivectors.Multivector) is
      use GA_Maths;
      use Multivectors;
      MV_X          : Multivector := MV;
      Grade         : constant Unsigned_Integer :=
        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Blade_Factors : Multivectors.Multivector_List;
      LC_NI_MV      : Multivector;
      Attitude      : Multivector;
      Location      : Multivector;
      NI_X2         : Float;
      Radius_Sq     : Float;
      Weight        : Float;
      Scale         : Float;
   begin
      if Grade = 0 then
         theAnalysis.M_Type.Blade_Class := Scalar_Blade;
         theAnalysis.M_Type.MV_Subtype := Scalar_Type;
         theAnalysis.M_Scalors (1) := Scalar_Part (MV);
      else
         theAnalysis.M_Type.Blade_Class := Round_Blade;
         if Grade = 1 then
            MV_X := Dual (MV_X);
            theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
         end if;

         LC_NI_MV := Left_Contraction (C3GA.ni, MV_X);
         Attitude :=
           Negate (Outer_Product (LC_NI_MV, C3GA.ni));
         Location :=
           Geometric_Product (MV_X, General_Inverse (LC_NI_MV));
         Location := Geometric_Product (Location, -1.0 / Scalar_Product (C3GA.ni, Location));

         NI_X2 := Scalar_Product (LC_NI_MV, LC_NI_MV);
         Radius_Sq := Scalar_Part (Geometric_Product (MV_X, 1.0 / NI_X2 * Grade_Inversion (MV_X)));
         Weight := E3GA.Norm_R (Left_Contraction (C3GA.no, Attitude));

         theAnalysis.M_Points (1) := Location;
         theAnalysis.M_Scalors (1) := Weight;
         case Grade is
         when 1 =>
            theAnalysis.M_Type.MV_Subtype := Sphere_Type;
            theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
            theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
            theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
            theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
         when 2 =>
            theAnalysis.M_Type.MV_Subtype := Point_Pair_Type;
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, Attitude));
         when 3 =>
            theAnalysis.M_Type.MV_Subtype := Circle_Type;
            Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
            theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
            theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
            theAnalysis.M_Vectors (3) :=
              -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                     theAnalysis.M_Vectors (2)));
         when 4 =>
            theAnalysis.M_Type.MV_Subtype := Sphere_Type;
            theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
            theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
            theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
         when others => null;
         end case;
      end if;
   end Analyze_Round;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Tangent (theAnalysis : in out MV_Analysis;
                              MV : Multivectors.Multivector) is
      use GA_Maths;
      use Multivectors;
      Grade         : constant Unsigned_Integer :=
        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      LC_NI_MV      : Multivector;
      Attitude      : Multivector:=
        Negate (Outer_Product (LC_NI_MV, C3GA.ni));
      Blade_Factors : Multivectors.Multivector_List;
      Location      : Multivector;
      Scale         : Float;
      Weight        : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Tangent_Blade;

      LC_NI_MV := Left_Contraction (C3GA.ni, MV);
      Location :=
        Geometric_Product (MV, General_Inverse (LC_NI_MV));
      Location := Geometric_Product (Location, -1.0 / Scalar_Product (C3GA.ni, Location));
      Weight := E3GA.Norm_R (Left_Contraction (C3GA.no, Attitude));

      theAnalysis.M_Points (1) := Location;
      theAnalysis.M_Scalors (1) := Weight;
      case Grade is
         when 1 =>
            theAnalysis.M_Type.MV_Subtype := Scalar_Type;
            theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
         when 2 =>
            theAnalysis.M_Type.MV_Subtype := Vector_Type;
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, Attitude));
         when 3 =>
            theAnalysis.M_Type.MV_Subtype := Bivector_Type;
            Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
            theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
            theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
            theAnalysis.M_Vectors (3) :=
              -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                     theAnalysis.M_Vectors (2)));
         when 4 =>
            theAnalysis.M_Type.MV_Subtype := Trivector_Type;
            theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
            theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
            theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
         when others => null;
      end case;
   end Analyze_Tangent;

   --  ----------------------------------------------------------------------------

end Multivector_Analyze_C3GA;
