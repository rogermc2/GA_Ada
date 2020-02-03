
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;
--  with Utilities;

with Blade_Types;
with E3GA;
with GA_Maths;
with GA_Utilities;
with Metric;
with Multivector_Type;

package body Multivector_Analyze_C3GA is

   procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                           MV          : Multivectors.Multivector;
                           Met         : Metric.Metric_Matrix;
                           Probe       : Multivectors.Normalized_Point);
   procedure Analyze_Free (theAnalysis : in out MV_Analysis;
                           MV          : Multivectors.Multivector);
   procedure Analyze_Round (theAnalysis : in out MV_Analysis;
                            MV          : Multivectors.Multivector);
   procedure Analyze_Tangent (theAnalysis : in out MV_Analysis;
                              MV          : Multivectors.Multivector);

   --  -------------------------------------------------------------------------

   procedure Analyze (Analysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Probe    : Multivectors.Normalized_Point := C3GA.no;
                      Flags    : Flag_Type := (Flag_Invalid, false);
                      Epsilon  : float := Default_Epsilon) is
      use Multivector_Type;
      use GA_Maths;

      MV_X      : Multivectors.Multivector := MV;
      MV_Info   : Multivector_Type.MV_Type_Record;

      procedure Classify is
         use Multivectors;
         --  C3GA.ni weight = 1.0
         OP_NiX_Val : constant Float := Norm_E (Outer_Product (C3GA.ni, MV_X), Metric.Metric_C3);
         IP_NiX_Val : constant Float := Norm_E (Left_Contraction (C3GA.ni, MV_X), Metric.Metric_C3);
         Xsq_Val    : constant Float := Norm_Esq (MV_X, Metric.Metric_C3);
         --              Xsq_Val    : constant Float := Norm_Esq_NP (MV_X);
         OP_NiX     : constant Boolean := Abs (OP_Nix_Val) > Epsilon;
         IP_NiX     : constant Boolean := Abs (IP_Nix_Val) > Epsilon;
         Xsq        : constant Boolean := Abs (Xsq_Val) > Epsilon;
      begin
         Put_Line ("Multivector_Analyze_C3GA.Classify, OP_NiX_Val, IP_NiX_Val, Xsq_Val" &
                     Float'Image (OP_NiX_Val) & Float'Image (IP_NiX_Val) & Float'Image (Xsq_Val));
         --           GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Classify MV_X", MV_X);
         --              GA_Utilities.Print_Multivector ("OP (C3GA.ni, MV_X)", Outer_Product (C3GA.ni, MV_X));
         --              Put_Line ("Multivector_Analyze_C3GA.Classify, Norm_E (OP (C3GA.ni, MV_X))" &
         --                        Float'Image (Norm_E (Outer_Product (C3GA.ni, MV_X))));
         if (not OP_NiX) and (not IP_NiX) then  --  OP_NiX and IP_NiX approx 0.0
            Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Free.");
            Analyze_Free (Analysis, MV_X);
         elsif (not OP_NiX) and IP_NiX then  --  OP_NiX approx 0.0
            Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Flat.");
            Analyze_Flat (Analysis, MV_X, Metric.Metric_C3, Probe);
         elsif OP_Nix and (not IP_NiX) then  --  IP_NiX approx 0.0
            Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Dual.");
            Analysis.M_Flags.Dual := not Analysis.M_Flags.Dual;
            Analyze_Flat (Analysis, Dual (MV_X), Metric.Metric_C3, Probe);
         elsif OP_NiX and IP_NiX then
            if not Xsq then
               Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Tangent.");
               Analyze_Tangent (Analysis, MV_X);
            else
               Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Round.");
               Analyze_Round (Analysis, MV_X);
            end if;
         end if;
      end Classify;

   begin
      GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Analyze MV_X:", MV_X);
      New_Line;
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Conformal_Model;

      if Flags.Dual then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Is Dual.");
         Analysis.M_Flags.Dual := True;
         MV_X := Multivectors.Dual (MV_X);
      end if;

      MV_Info := Init (MV_X);
      Analysis.M_MV_Type := MV_Info;
      Print_Multivector_Info ("Multivector_Analyze_C3GA.Analyze MV_Info:", MV_Info);
      New_Line;

      --  Check for zero blade
      if Zero (Analysis.M_MV_Type) then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Zero_Blade.");
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.Scalors (1) := 0.0;

      elsif MV_Kind (Analysis.M_MV_Type) = Versor_MV then
         Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
         Analysis.M_Type.Versor_Subclass := Even_Versor;
         Analysis.M_Vectors (1) := E3GA.e1;

      else
         case Grade_Use (Analysis.M_MV_Type) is
            when 0 =>  --  Grade 0 Scalar
               Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 1.");
               Analysis.M_Type.Blade_Class := Scalar_Blade;
               Analysis.M_Type.Blade_Subclass := Scalar_Subclass;
               Analysis.M_Type.M_Grade := 1;
               Analysis.Scalors (1) := Multivectors.Scalar_Part (MV_X);

            when 5 =>  --  Grade 5 Pseudo scalar
               Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 6.");
               Analysis.M_Type.Blade_Class := Pseudo_Scalar_Blade;
               Analysis.M_Type.Blade_Subclass := Pseudo_Scalar_Subclass;
               Analysis.M_Type.M_Grade := 6;
               Analysis.Scalors (1) := C3GA.NO_E1_E2_E3_NI (MV_X);
            when others => Classify;
         end case;
      end if;
      New_Line;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze.");
         raise;
   end Analyze;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                           MV          : Multivectors.Multivector;
                           Met         : Metric.Metric_Matrix;
                           Probe       : Multivectors.Normalized_Point) is
      use GA_Maths;
      use Multivectors;
      Grade         : Unsigned_Integer :=
                        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      --  Attitude is a free N-vector
      Attitude      : constant Multivector := Negate (Left_Contraction (C3GA.ni, MV, Met));
      MV_Inverse    : Multivector;
      MV_Invertible : Boolean;
      MV_Location   : Multivector;
      Location      : Multivectors.Normalized_Point;
      Blade_Factors : Multivectors.Multivector_List;
      Scale         : Float;
      Weight        : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Flat_Blade;
      if theAnalysis.M_Flags.Dual then
         Grade := 5 - Grade;
      end if;
      MV_Invertible := General_Inverse (MV, Met, MV_Inverse);
      --  MV_Location is a normalized dual sphere
      if MV_Invertible then
         GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Analyze_Flat MV_Inverse",
                                         MV_Inverse);
         GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Analyze_Flat Left_Contraction (Probe, MV)",
                                         Left_Contraction (Probe, MV));
         MV_Location := Left_Contraction (Left_Contraction (Probe, MV), MV_Inverse, Met);
         GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Analyze_Flat MV_Location 2",
                                         MV_Location);
         MV_Location := Geometric_Product (MV_Location,
                                           -1.0 / Scalar_Product (C3GA.ni, MV_Location, Met));
         if Grade = 0 then
            Weight := Scalar_Product (MV, C3GA.no, Met);
         else
            Weight := Abs (Norm_Esq (MV, Met));
         end if;

         Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (MV_Location));
         theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Location);
         theAnalysis.Scalors (1) := Weight;
         --  Grade indications are taken from Geometric Algebra and its Application to Computer Graphics
         --  by Hildenbrand, Fontijne, Perwass and Dorst, Eurographics 2004.
         --  "the representation of a point is simply a sphere of radius zero".
         case Grade is
            when 0 => theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
               Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Scalar_Subclass.");
            when 1 => theAnalysis.M_Type.Blade_Subclass := Point_Subclass;
               Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Point_Subclass.");
            when 3 =>  --  Line
               theAnalysis.M_Type.Blade_Subclass := Line_Subclass;
               theAnalysis.M_Vectors (1) :=
                 To_Vector (Unit_E (Left_Contraction (C3GA.no, MV)));
               Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Line_Subclass.");
            when 4 =>  --  Plane
               theAnalysis.M_Type.Blade_Subclass := Plane_Subclass;
               Blade_Factors := GA_Utilities.Factorize_Blade
                 (Reverse_MV (Left_Contraction (C3GA.no, Reverse_MV (Attitude))), Scale);
               Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Plane_Subclass.");

               theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
               theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
               theAnalysis.M_Vectors (3) :=
                 -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                        theAnalysis.M_Vectors (2)));
            when others => null;
         end case;
         Print_Analysis ("Multivector_Analyze_C3GA.Analyze_Flat", theAnalysis);
      else
         Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, multivector MV is not invertible.");
      end if;
   exception
      when others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Flat.");
         raise;
   end Analyze_Flat;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Free (theAnalysis : in out MV_Analysis;
                           MV          : Multivectors.Multivector) is
      use Multivectors;
      Grade         : constant GA_Maths.Unsigned_Integer :=
                        Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Weight        : constant Float := Norm_E (MV);
      --        Attitude      : constant Multivector := MV;
      Blade_Factors : Multivectors.Multivector_List;
      Scale         : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Free_Blade;
      theAnalysis.Points (1) := (0.0, 0.0, 0.0);
      theAnalysis.Scalors (1) := Weight;
      case Grade is
         when 1 => theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 1.");
         when 2 =>  --  F Vector
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 2.");
            theAnalysis.M_Type.Blade_Subclass := Vector_Subclass;
            theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, MV));
         when 3 =>  --  F Bivector
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 3.");
            theAnalysis.M_Type.Blade_Subclass := Bivector_Subclass;
            Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
            theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
            theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
            theAnalysis.M_Vectors (3) :=
              -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                     theAnalysis.M_Vectors (2)));
         when 4 =>  --  F Trivector
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 4.");
            theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
            theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
            theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
            theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
         when others => null;
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade others.");
      end case;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Free.");
         raise;
   end Analyze_Free;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Round (theAnalysis : in out MV_Analysis;
                            MV          : Multivectors.Multivector) is
      use Maths.Single_Math_Functions;
      use GA_Maths;
      use Multivectors;
      MV_X             : Multivector := MV;
      Grade            : constant Unsigned_Integer :=
                           Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      Blade_Factors    : Multivector_List;
      LC               : Multivector;
      LC_NI_MV         : Multivector;
      LC_NI_MV_Inverse : Multivector;
      Invertible       : Boolean;
      Attitude         : Multivector;
      Location         : Multivector;
      Point_Location   : Multivectors.Normalized_Point;
      NI_Xsq           : Float;
      Radius           : Float;
      Radius_Sq        : Float;
      Weight           : Float;
      Scale            : Float;
   begin
      --          Put_Line ("Multivector_Analyze_C3GA.Analyze_Round, Grade:" &
      --                      Unsigned_Integer'Image (Grade));
      if Grade = 0 then
         theAnalysis.M_Type.Blade_Class := Scalar_Blade;
         theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
         theAnalysis.Scalors (1) := Scalar_Part (MV_X);  --  signed weight
      else
         theAnalysis.M_Type.Blade_Class := Round_Blade;
         if Grade = 1 then
            MV_X := Dual (MV_X);
            --  m_flags = m_flags xor FLAG_DUAL?
            theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
         end if;

         LC_NI_MV := Left_Contraction (C3GA.ni, MV_X);
         --              GA_Utilities.Print_Multivector
         --                ("Multivector_Analyze_C3GA.Analyze_Round LC_NI_MV", LC_NI_MV);
         Attitude :=
           Negate (Outer_Product (LC_NI_MV, C3GA.ni));
         --              GA_Utilities.Print_Multivector
         --                ("Multivector_Analyze_C3GA.Analyze_Round Attitude", Attitude);

         Invertible := General_Inverse (LC_NI_MV, LC_NI_MV_Inverse);
         if Invertible then
            --  location is normalized dual sphere
            Location := Geometric_Product (MV_X, LC_NI_MV_Inverse);  --  _location
            Location := Geometric_Product
              (Location, -1.0 / Scalar_Product (C3GA.ni, Location));
            --  normalizedPoint location = c3gaPoint(_vectorE3GA(_location));
            Point_Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (Location));
            --                  GA_Utilities.Print_Multivector
            --                    ("Multivector_Analyze_C3GA.Analyze_Round Point_Location", Point_Location);

            NI_Xsq := Scalar_Product (LC_NI_MV, LC_NI_MV);
            Radius_Sq := Scalar_Part
              (Geometric_Product (MV_X, Grade_Inversion (MV_X))) / NI_Xsq;
            if Radius_Sq < 0.0 then
               Radius_Sq := -Radius_Sq;
            end if;

            Radius := Float (Sqrt (GL.Types.Single (Abs (Radius_Sq))));
            Weight := Norm_E (Left_Contraction (C3GA.no, Attitude));

            --  Format of round
            --  m_pt[0] = location
            --  m_sc[0] = signed radius
            --  m_sc[1] = signed weight
            --  m_vc[0] .. m_vc[2] = unit 3D vector basis for attitude
            theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
            theAnalysis.Scalors (1) := Radius;
            theAnalysis.Scalors (2) := Weight;
            --                  New_Line;
            --                  Utilities.Print_Vector ("Multivector_Analyze_C3GA.Analyze_Round, Point vector",
            --                                          theAnalysis.Points (1));
            --                  Put_Line ("Multivector_Analyze_C3GA.Analyze_Round radius and weight:" &
            --                              Float'Image (theAnalysis.Scalors (1)) &
            --                              Float'Image (theAnalysis.Scalors (2)));
            --                  New_Line;

            case Grade is
               when 1 =>
                  theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                  theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
                  theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
                  theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
                  theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
               when 2 =>
                  theAnalysis.M_Type.Blade_Subclass := Point_Pair_Subclass;
                  --                      GA_Utilities.Print_Multivector
                  --                        ("Multivector_Analyze_C3GA.Analyze_Round C3GA.no", C3GA.no);
                  --                      GA_Utilities.Print_Multivector
                  --                        ("Multivector_Analyze_C3GA.Analyze_Round Attitude", Attitude);
                  --                      GA_Utilities.Print_Multivector
                  --                        ("Multivector_Analyze_C3GA.Analyze_Round Left_Contraction",
                  --                         Left_Contraction (C3GA.no, Attitude));
                  LC := (Left_Contraction (C3GA.no, Attitude));
                  if GA_Utilities.Multivector_Size (LC) > 0 then
                     theAnalysis.M_Vectors (1) := Unit_E (LC);
                  else
                     raise MVA_Exception with
                       "Analyze_Round, Grade 2 Left Contraction is zero.";
                  end if;
               when 3 =>
                  theAnalysis.M_Type.Blade_Subclass := Circle_Subclass;
                  Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
                  theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
                  theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
                  theAnalysis.M_Vectors (3) :=
                    -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                           theAnalysis.M_Vectors (2)));
               when 4 =>
                  theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                  theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
                  theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
                  theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
               when others => null;
            end case;
            --                  Print_Analysis ("Multivector_Analyze_C3GA.Analyze_Round,",
            --                                  theAnalysis);
            --                  GA_Utilities.Print_Multivector
            --                    ("theAnalysis.M_Vectors (1)", theAnalysis.M_Vectors (1));
            --                  GA_Utilities.Print_Multivector
            --                    ("theAnalysis.M_Vectors (2)", theAnalysis.M_Vectors (2));
            --                  GA_Utilities.Print_Multivector
            --                    ("theAnalysis.M_Vectors (3)", theAnalysis.M_Vectors (3));
         else
            Put_Line ("Multivector_Analyze_C3GA.Analyze_Round, LC_NI_MV is not inverttble.");
         end if;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Round.");
         raise;
   end Analyze_Round;

   --  ----------------------------------------------------------------------------

   procedure Analyze_Tangent (theAnalysis : in out MV_Analysis;
                              MV          : Multivectors.Multivector) is
      use GA_Maths;
      use Multivectors;
      Grade          : constant Unsigned_Integer :=
                         Multivector_Type.Top_Grade (theAnalysis.M_MV_Type);
      LC_NI_MV       : Multivector;
      LC_NI_MV_Inv   : Multivector;
      Attitude       : constant Multivector :=
                         Negate (Outer_Product (LC_NI_MV, C3GA.ni));
      Blade_Factors  : Multivectors.Multivector_List;
      Location       : Multivector;
      Point_Location : Multivectors.Normalized_Point;
      Scale          : Float;
      Weight         : Float;
   begin
      theAnalysis.M_Type.Blade_Class := Tangent_Blade;

      LC_NI_MV := Left_Contraction (C3GA.ni, MV);
      if General_Inverse (LC_NI_MV, LC_NI_MV_Inv) then
         Location :=
           Geometric_Product (MV, LC_NI_MV_Inv);
         Location := Geometric_Product (Location, -1.0 / Scalar_Product (C3GA.ni, Location));
         Point_Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (Location));
         Weight := Norm_E (Left_Contraction (C3GA.no, Attitude));

         theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
         theAnalysis.Scalors (1) := Weight;

         case Grade is
            when 1 =>
               theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
               theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
            when 2 =>
               theAnalysis.M_Type.Blade_Subclass := Vector_Subclass;
               theAnalysis.M_Vectors (1) := Unit_E (Left_Contraction (C3GA.no, Attitude));
            when 3 =>
               theAnalysis.M_Type.Blade_Subclass := Bivector_Subclass;
               Blade_Factors := GA_Utilities.Factorize_Blade (MV, Scale);
               theAnalysis.M_Vectors (1) := MV_First (Blade_Factors);
               theAnalysis.M_Vectors (2) := MV_Item (Blade_Factors, 2);
               theAnalysis.M_Vectors (3) :=
                 -Dual (Outer_Product (theAnalysis.M_Vectors (1),
                        theAnalysis.M_Vectors (2)));
            when 4 =>
               theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
               theAnalysis.M_Vectors (1) := Basis_Vector (Blade_Types.E3_e1);
               theAnalysis.M_Vectors (2) := Basis_Vector (Blade_Types.E3_e2);
               theAnalysis.M_Vectors (3) := Basis_Vector (Blade_Types.E3_e3);
            when others => null;
         end case;
      else
         Put_Line ("Multivector_Analyze_C3GA,Analyze_Tangent, LC_NI_MV is not invertible");
      end if;
   end Analyze_Tangent;

   --  ----------------------------------------------------------------------------

end Multivector_Analyze_C3GA;
