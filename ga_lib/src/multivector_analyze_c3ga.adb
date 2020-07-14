
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;
--  with Utilities;

with Blade_Types;
with E3GA;
with GA_Maths;
with GA_Utilities;
with Multivector_Utilities;
with Metric;
with Multivector_Type;

package body Multivector_Analyze_C3GA is

    procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                            MV_X        : Multivectors.Multivector;
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
        MV_X      : Multivectors.Multivector := MV;
        MV_Info   : Multivector_Type.MV_Type_Record;

        procedure Classify is
            use Multivectors;
            --  C3GA.ni weight = 1.0
            --  OP_NiX_Val : constant Float := Norm_E (Outer_Product (C3GA.ni, MV_X), Met);
            --  IP_NiX_Val : constant Float := Norm_E (Left_Contraction (C3GA.ni, MV_X), Met);
            --  Xsq_Val    : constant Float := Norm_Esq (MV_X, Metric.C3_Metric);
            OP_NiX_Val : Float := 0.0;
            IP_NiX_Val : Float := 0.0;
            --           Xsq_Val    : Float := 0.0;
            Product    : Multivectors.Multivector;
            Xsq_Val    : constant Float := Norm_Esq (MV_X);
            OP_NiX     : Boolean;
            IP_NiX     : Boolean;
            Xsq        : constant Boolean := Abs (Xsq_Val) > Epsilon;
        begin
            Product := Outer_Product (C3GA.ni, MV_X);
            OP_NiX := not Multivectors.Is_Null (Product);
            if OP_NiX then
                OP_NiX_Val := Norm_E (Product);
                OP_NiX := Abs (OP_Nix_Val) > Epsilon;
            end if;

            Product := Left_Contraction (C3GA.ni, MV_X, Metric.C3_Metric);
            IP_NiX := not Multivectors.Is_Null (Product);
            if IP_NiX then
                IP_NiX_Val := Norm_E (Product);
                IP_NiX := Abs (IP_Nix_Val) > Epsilon;
            end if;
            --           Xsq_Val := Norm_Esq (MV_X);  --  norm_r

            if (not OP_NiX) and (not IP_NiX) then  --  OP_NiX and IP_NiX approx 0.0
                Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Free.");
                Analyze_Free (Analysis, MV_X);
            elsif (not OP_NiX) and IP_NiX then  --  OP_NiX approx 0.0
                Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Flat.");
                Analyze_Flat (Analysis, MV_X, Probe);
            elsif OP_Nix and (not IP_NiX) then  --  IP_NiX approx 0.0
                Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Dual.");
                Analysis.M_Flags.Dual := not Analysis.M_Flags.Dual;
                Analyze_Flat (Analysis, Dual (MV_X, Metric.C3_Metric), Probe);
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
            MV_X := Multivectors.Dual (MV_X, Metric.C3_Metric);
        end if;

        MV_Info := Init (MV_X, Metric.C3_Metric);
        Analysis.M_MV_Type := MV_Info;

        --  Check for zero blade
        if Zero (Analysis.M_MV_Type) then
            Put_Line ("Multivector_Analyze_C3GA.Analyze Zero_Blade.");
            Analysis.M_Type.Blade_Class := Zero_Blade;
            Analysis.Scalars (1) := 0.0;

        elsif MV_Kind (Analysis.M_MV_Type) = Versor_MV then
            Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
            Analysis.M_Type.Versor_Subclass := Even_Versor;
            Analysis.M_Vectors (1) := C3GA.To_VectorE3GA (E3GA.e1);

        else
            Put_Line ("Multivector_Analyze_C3GA.Analyze case Grade_Use: " &
                        GA_Maths.Grade_Usage'Image (Grade_Use (Analysis.M_MV_Type)));
            case Grade_Use (Analysis.M_MV_Type) is
            when 0 =>  --  Grade 0 Scalar
                Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 1.");
                Analysis.M_Type.Blade_Class := Scalar_Blade;
                Analysis.M_Type.Blade_Subclass := Scalar_Subclass;
                Analysis.M_Type.M_Grade := 1;
                Analysis.Scalars (1) := Multivectors.Scalar_Part (MV_X);

            when 5 =>  --  Grade 5 Pseudo scalar
                Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 6.");
                Analysis.M_Type.Blade_Class := Pseudo_Scalar_Blade;
                Analysis.M_Type.Blade_Subclass := Pseudo_Scalar_Subclass;
                Analysis.M_Type.M_Grade := 6;
                Analysis.Scalars (1) := C3GA.NO_E1_E2_E3_NI (MV_X);
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
    --  format of flat
    --  theAnalysis.M_Vectors (1)  m_pt[0] = location
    --  theAnalysis.M_Vectors (2)  m_vc[0] .. m_vc[1]
    --                             unit 3D vector basis for attitude (direction)
    --  theAnalysis.M_Vectors (3)  m_sc[0] = weight
    procedure Analyze_Flat (theAnalysis : in out MV_Analysis;
                            MV_X        : Multivectors.Multivector;
                            Probe       : Multivectors.Normalized_Point) is
        use Multivectors;
        Met           : constant Metric.Metric_Record := Metric.C3_Metric;
        Grade         : Integer :=
                          Multivector_Type.MV_Grade (theAnalysis.M_MV_Type);
        --  Attitude is a free N-vector
        Attitude      : constant Multivector :=
                          Negate (Left_Contraction (C3GA.ni, MV_X, Metric.C3_Metric));
        MV_Inverse    : Multivector;
        MV_Location   : Multivector;
        Location      : Multivectors.Normalized_Point;
        Blade_Factors : Multivectors.Multivector_List;
        SP            : Float;
        Scale         : Float;
        Weight        : Float;
    begin
        theAnalysis.M_Type.Blade_Class := Flat_Blade;
        if theAnalysis.M_Flags.Dual then
            Grade := 5 - Grade;
        end if;

        MV_Inverse := General_Inverse (MV_X, Met);
        --  MV_Location is a normalized dual sphere
        MV_Location := Left_Contraction (Probe, MV_X, Met);
        MV_Location := Left_Contraction (MV_Location, MV_Inverse, Met);
        SP := Scalar_Product (C3GA.ni, MV_Location, Met);
        if SP = 0.0 then
            Location := C3GA.Set_Normalized_Point (E1 => 0.0, E2 => 0.0, E3 => 0.0);
        else
            MV_Location := Geometric_Product (MV_Location, -New_Scalar (1.0 / SP), Met);
            Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (MV_Location));
        end if;

        if Grade = 1 then
            Weight := -Scalar_Product (MV_X, C3GA.no, Met);
        else
            Weight := Abs (Norm_Esq (MV_X));   --  Norm_Esq is norm_r
        end if;

        --  ************* format of flat ***************
        --  theAnalysis.Points    m_pt[0] = location
        --  theAnalysis.M_Vectors m_vc[0] .. m_vc[1] = unit 3D vector basis for attitude
        --  theAnalysis.Scalars   m_sc[0] = weight
        --  ************* END format of flat ***************
        theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Location);
        theAnalysis.Scalars (1) := Weight;
        --  Grade indications are taken from Geometric Algebra and its
        --  Application to Computer Graphics by Hildenbrand, Fontijne, Perwass and
        --  Dorst, Eurographics 2004.
        --  "the representation of a point is simply a sphere of radius zero".
        --  factor attitude:
        case Grade is
            when 1 => theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Scalar_Subclass.");
            when 2 => theAnalysis.M_Type.Blade_Subclass := Point_Subclass;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Point_Subclass.");
            when 3 =>  --  Line
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Line_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Line_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Unit_E (Left_Contraction (C3GA.no, Attitude, Met)));
            when 4 =>  --  Plane
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Plane_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Plane_Subclass;
                Blade_Factors := Multivector_Utilities.Factorize_Blades
                  (Reverse_MV (Left_Contraction (C3GA.no, Reverse_MV (Attitude), Met)),
                   Scale);

                theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (MV_First (Blade_Factors));
                theAnalysis.M_Vectors (2) := C3GA.To_VectorE3GA (MV_Item (Blade_Factors, 2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (MV_First (Blade_Factors),
                                      MV_Item (Blade_Factors, 2)), Metric.C3_Metric));
            when others => null;
        end case;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Flat.");
            raise;
    end Analyze_Flat;

    --  ----------------------------------------------------------------------------
    --  format of free
    --  m_pt[0] = no (or probe?)
    --  m_vc[0] .. m_vc[2] = unit 3D vector basis for attitude (direction)
    --   m_sc[0] = weight
    procedure Analyze_Free (theAnalysis : in out MV_Analysis;
                            MV          : Multivectors.Multivector) is
        use Metric;
        use Multivectors;
        Grade         : constant Integer :=
                          Multivector_Type.MV_Grade (theAnalysis.M_MV_Type);
        Weight        : constant Float := Norm_E (MV);
        --        Attitude      : constant Multivector := MV;
        Blade_Factors : Multivectors.Multivector_List;
        Scale         : Float := 1.0;
    begin
        theAnalysis.M_Type.Blade_Class := Free_Blade;
        theAnalysis.Points (1) := (0.0, 0.0, 0.0);
        theAnalysis.Scalars (1) := Weight;
        case Grade is
            when 1 => theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 1.");
            when 2 =>  --  F Vector
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 2.");
                theAnalysis.M_Type.Blade_Subclass := Vector_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Unit_E (Left_Contraction
                                      (C3GA.no, MV, C3_Metric)));
            when 3 =>  --  F Bivector
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 3.");
                theAnalysis.M_Type.Blade_Subclass := Bivector_Subclass;
                Blade_Factors :=
                  Multivector_Utilities.Factorize_Blades (MV, Scale);
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (MV_First (Blade_Factors));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (MV_Item (Blade_Factors, 2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (MV_First (Blade_Factors),
                                      MV_Item (Blade_Factors, 2)), C3_Metric));
            when 4 =>  --  F Trivector
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 4.");
                theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others => null;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade others.");
        end case;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Free.");
            raise;
    end Analyze_Free;

    --  ----------------------------------------------------------------------------
    --  Format of round
    --  m_pt[0] = location
    --  m_sc[0] = signed radius
    --  m_sc[1] = signed weight
    --  m_vc[0] .. m_vc[2] = unit 3D vector basis for attitude (direction)
    procedure Analyze_Round (theAnalysis : in out MV_Analysis;
                             MV          : Multivectors.Multivector) is
        use Maths.Single_Math_Functions;
        use Multivectors;
        Met              : constant Metric.Metric_Record :=  Metric.C3_Metric;
        MV_X             : Multivector := MV;
        Grade            : constant Integer :=
                             Multivector_Type.MV_Grade (theAnalysis.M_MV_Type);
        MV_Factors       : Multivector_List;
        LC               : Multivector;
        LC_NI_MV         : Multivector;
        LC_NI_MV_Inverse : Multivector;
        Attitude         : Multivector;
        Location         : Multivector;
        Point_Location   : Multivectors.Normalized_Point;
        NI_Xsq           : Float;
        Radius           : Float;
        Radius_Sq        : Float;
        Weight           : Float;
        Scale            : Float := 1.0;
    begin
        Put_Line ("Multivector_Analyze_C3GA.Analyze_Round, Grade:" &
                    Integer'Image (Grade));
        if Grade = 0 then
            theAnalysis.M_Type.Blade_Class := Scalar_Blade;
            theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
            theAnalysis.Scalars (1) := Scalar_Part (MV_X);  --  signed weight
        else
            theAnalysis.M_Type.Blade_Class := Round_Blade;
            if Grade = 1 then
                MV_X := Dual (MV_X, Metric.C3_Metric);
                --  m_flags = m_flags xor FLAG_DUAL?
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
            end if;

            LC_NI_MV := Left_Contraction (C3GA.ni, MV_X,  Metric.C3_Metric);
            --              GA_Utilities.Print_Multivector
            --                ("Multivector_Analyze_C3GA.Analyze_Round LC_NI_MV", LC_NI_MV);
            Attitude :=
              Negate (Outer_Product (LC_NI_MV, C3GA.ni));
            --              GA_Utilities.Print_Multivector
            --                ("Multivector_Analyze_C3GA.Analyze_Round Attitude", Attitude);

            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round calling General_Inverse");
            LC_NI_MV_Inverse := General_Inverse (LC_NI_MV, Met);
            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round calling Geometric_Product 1");
            --  location is normalized dual sphere
            Location := Geometric_Product (MV_X, LC_NI_MV_Inverse, Met);  --  _location
            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round calling Geometric_Product 2");
            Location := Geometric_Product
              (Location, -1.0 / Scalar_Product (C3GA.ni, Location, Met));
            --  normalizedPoint location = c3gaPoint(_vectorE3GA(_location));
            Point_Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (Location));
            --                  GA_Utilities.Print_Multivector
            --                    ("Multivector_Analyze_C3GA.Analyze_Round Point_Location", Point_Location);

            NI_Xsq := Scalar_Product (LC_NI_MV, LC_NI_MV, Met);
            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round calling Geometric_Product 3");
            Radius_Sq := Scalar_Part
              (Geometric_Product (MV_X, Grade_Inversion (MV_X), Met)) / NI_Xsq;
            if Radius_Sq < 0.0 then
                Radius_Sq := -Radius_Sq;
            end if;

            Radius := Float (Sqrt (GL.Types.Single (Abs (Radius_Sq))));
            Weight := Norm_E (Left_Contraction (C3GA.no, Attitude, Met));

            theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
            theAnalysis.Scalars (1) := Radius;
            theAnalysis.Scalars (2) := Weight;
            --           New_Line;
            --           Utilities.Print_Vector ("Multivector_Analyze_C3GA.Analyze_Round, Point vector",
            --                                   theAnalysis.Points (1));
            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round radius and weight:" &
            --                       Float'Image (theAnalysis.Scalars (1)) &
            --                       Float'Image (theAnalysis.Scalars (2)));
            --           New_Line;
            --  Factor attitude:
            case Grade is
            when 1 =>
                theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
                --  direction
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when 2 =>
                theAnalysis.M_Type.Blade_Subclass := Point_Pair_Subclass;
                --                      GA_Utilities.Print_Multivector
                --                        ("Multivector_Analyze_C3GA.Analyze_Round C3GA.no", C3GA.no);
                --                      GA_Utilities.Print_Multivector
                --                        ("Multivector_Analyze_C3GA.Analyze_Round Attitude", Attitude);
                --                      GA_Utilities.Print_Multivector
                --                        ("Multivector_Analyze_C3GA.Analyze_Round Left_Contraction",
                --                         Left_Contraction (C3GA.no, Attitude, Met));
                LC := Left_Contraction (C3GA.no, Attitude, Met);
                if GA_Utilities.Multivector_Size (LC) > 0 then
                    theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (Unit_E (LC));
                else
                    raise Analyze_C3GA_Exception with
                      "Multivector_Analyze_C3GA.Analyze_Round, " &
                      "Grade 2 Left Contraction is zero.";
                end if;
            when 3 | 5 =>
                --  circle explicit factorization required:
                theAnalysis.M_Type.Blade_Subclass := Circle_Subclass;
--                  MV_X := Reverse_MV (Left_Contraction (C3GA.no,
--                                      Reverse_MV (Attitude), Met));
                MV_X := MV;
                GA_Utilities.Print_Multivector
                  ("Multivector_Analyze_C3GA.Analyze_Round MV_X", MV_X);
                --  Returned scale not used
                MV_Factors :=
                  Multivector_Utilities.Factorize_Blades (MV_X, Scale);
                GA_Utilities.Print_Multivector_List
                  ("Multivector_Analyze_C3GA.Analyze_Round MV Factors", MV_Factors);
                --  direction
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (MV_First (MV_Factors));
                --                 Utilities.Print_Vector
                --                   ("Multivector_Analyze_C3GA.Analyze_Round Grade 3 X direction M_Vectors (1)",
                --                 theAnalysis.M_Vectors (1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (MV_Item (MV_Factors, 2));
                --                 Utilities.Print_Vector
                --                   ("Multivector_Analyze_C3GA.Analyze_Round Grade 3 Y direction M_Vectors (2)",
                --                    theAnalysis.M_Vectors (2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (MV_First (MV_Factors),
                                      MV_Item (MV_Factors, 2)), Met));
            when 4 =>
                theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others => null;
            end case;
            --                  Print_Analysis ("Multivector_Analyze_C3GA.Analyze_Round,",
            --                                  theAnalysis);
            Print_E3_Vector_Array
              ("Multivector_Analyze_C3GA.Analyze_Round direction M_Vectors",
               theAnalysis.M_Vectors);
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Analyze_C3GA.Analyze_Round.");
            raise;
    end Analyze_Round;

    --  ----------------------------------------------------------------------------
    --  format of tangent
    --  m_pt[0] = location
    --  m_vc[0] .. m_vc[2] = unit 3D vector basis for attitude
    --  m_sc[0] = weight
    procedure Analyze_Tangent (theAnalysis : in out MV_Analysis;
                               MV          : Multivectors.Multivector) is
        use Multivectors;
        Met            : constant Metric.Metric_Record :=  Metric.C3_Metric;
        Grade          : constant Integer :=
                           Multivector_Type.MV_Grade (theAnalysis.M_MV_Type);
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

        LC_NI_MV := Left_Contraction (C3GA.ni, MV, Met);
        Put_Line ("Multivector_Analyze_C3GA.Analyze_Tangent calling General_Inverse");
        LC_NI_MV_Inv := General_Inverse (LC_NI_MV, Met);
        Put_Line ("Multivector_Analyze_C3GA.Analyze_Tangent calling Geometric_Product 1");
        Location :=
          Geometric_Product (MV, LC_NI_MV_Inv, Met);
        Put_Line ("Multivector_Analyze_C3GA.Analyze_Tangent calling Geometric_Product 2");
        Location := Geometric_Product (Location, -1.0 / Scalar_Product (C3GA.ni, Location, Met));
        Point_Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (Location));
        Weight := Norm_E (Left_Contraction (C3GA.no, Attitude, Met));

        theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
        theAnalysis.Scalars (1) := Weight;

        case Grade is
            when 1 =>
                theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
            when 2 =>
                theAnalysis.M_Type.Blade_Subclass := Vector_Subclass;
                theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA
                  (Unit_E (Left_Contraction (C3GA.no, Attitude, Met)));
            when 3 =>
                theAnalysis.M_Type.Blade_Subclass := Bivector_Subclass;
                Blade_Factors :=
                  Multivector_Utilities.Factorize_Blades (MV, Scale);
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (MV_First (Blade_Factors));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (MV_Item (Blade_Factors, 2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (MV_First (Blade_Factors),
                                      MV_Item (Blade_Factors, 2)), Metric.C3_Metric));
            when 4 =>
                theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
                theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others => null;
        end case;

    end Analyze_Tangent;

    --  ----------------------------------------------------------------------------

end Multivector_Analyze_C3GA;
