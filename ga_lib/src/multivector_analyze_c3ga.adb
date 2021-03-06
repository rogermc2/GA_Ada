
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

    function Analyze_Flat (Analysis_In : MV_Analysis;
                           MV_X        : Multivectors.Multivector;
                           Probe       : Multivectors.Normalized_Point)
                           return MV_Analysis;
    function Analyze_Free (Analysis_In : MV_Analysis;
                           MV          : Multivectors.Multivector)
                           return MV_Analysis;
    function Analyze_Round (Analysis_In : MV_Analysis;
                            MV          : Multivectors.Multivector)
                           return MV_Analysis;
    function Analyze_Tangent (Analysis_In : MV_Analysis;
                              MV          : Multivectors.Multivector)
                           return MV_Analysis;

    procedure Init_Analysis_Type (Analysis_In : MV_Analysis;
                                  theAnalysis : in out MV_Analysis);

    --  -------------------------------------------------------------------------

    function Analyze (MV      : Multivectors.Multivector;
                      Probe   : Multivectors.Normalized_Point := C3GA.no;
                      Flags   : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon)
                      return MV_Analysis is
        use Multivector_Type;
        MV_X        : Multivectors.Multivector := MV;
        MV_Info     : Multivector_Type.MV_Type_Record;
        theAnalysis : MV_Analysis;

        procedure Classify is
            use Multivectors;
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
                theAnalysis := Analyze_Free (theAnalysis, MV_X);
            elsif (not OP_NiX) and IP_NiX then  --  OP_NiX approx 0.0
                Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Flat.");
                theAnalysis := Analyze_Flat (theAnalysis, MV_X, Probe);
            elsif OP_Nix and (not IP_NiX) then  --  IP_NiX approx 0.0
                Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Dual.");
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
                theAnalysis := Analyze_Flat (theAnalysis, Dual (MV_X, Metric.C3_Metric), Probe);

            elsif OP_NiX and IP_NiX then
                if not Xsq then
                    Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Tangent.");
                    theAnalysis := Analyze_Tangent (theAnalysis, MV_X);
                else
                    Put_Line ("Multivector_Analyze_C3GA.Classify, classification: Round.");
                    theAnalysis := Analyze_Round (theAnalysis, MV_X);
                end if;
            end if;
        end Classify;

    begin
--          GA_Utilities.Print_Multivector ("Multivector_Analyze_C3GA.Analyze MV_X:", MV_X);
--          New_Line;
        theAnalysis.M_Flags.Valid := True;
        theAnalysis.Epsilon := Epsilon;
        theAnalysis.M_Type.Model_Kind := Multivector_Analyze.Conformal_Model;

        if Flags.Dual then
            Put_Line ("Multivector_Analyze_C3GA.Analyze Is Dual.");
            theAnalysis.M_Flags.Dual := True;
            MV_X := Multivectors.Dual (MV_X, Metric.C3_Metric);
            GA_Utilities.Print_Multivector_String
              ("Multivector_Analyze_C3GA.Analyze Dual MV_X", MV_X,
              Blade_Types.Basis_Names_C3GA);
        end if;

        MV_Info := Init (MV_X, Metric.C3_Metric);
        theAnalysis.M_MV_Type := MV_Info;

        --  Check for zero blade
        if Zero (theAnalysis.M_MV_Type) then
            Put_Line ("Multivector_Analyze_C3GA.Analyze Zero_Blade.");
            theAnalysis.M_Type.Blade_Class := Zero_Blade;
            theAnalysis.Weight := 0.0;

        elsif MV_Kind (theAnalysis.M_MV_Type) = Versor_MV then
            Put_Line ("Multivector_Analyze_C3GA.Analyze Versor_Object 2.");
            theAnalysis.M_Type.Versor_Subclass := Even_Versor;
            theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (E3GA.e1);

        else
            Put_Line ("Multivector_Analyze_C3GA.Analyze case Grade_Use: " &
                        GA_Maths.Grade_Usage'Image (Grade_Use (theAnalysis.M_MV_Type)));
            case Grade_Use (theAnalysis.M_MV_Type) is
            when 0 =>  --  Grade 0 Scalar
                Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 1.");
                theAnalysis.M_Type.Blade_Class := Scalar_Blade;
                theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
                theAnalysis.M_Type.M_Grade := 1;
                theAnalysis.Weight := Multivectors.Scalar_Part (MV_X);

            when 5 =>  --  Grade 5 Pseudo scalar
                Put_Line ("Multivector_Analyze_C3GA.Analyze Grade_Use = 6.");
                theAnalysis.M_Type.Blade_Class := Pseudo_Scalar_Blade;
                theAnalysis.M_Type.Blade_Subclass := Pseudo_Scalar_Subclass;
                theAnalysis.M_Type.M_Grade := 6;
                theAnalysis.Weight := C3GA.NO_E1_E2_E3_NI (MV_X);
            when others => Classify;
            end case;
        end if;
        New_Line;
        return theAnalysis;

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
    function Analyze_Flat (Analysis_In : MV_Analysis;
                            MV_X        : Multivectors.Multivector;
                            Probe       : Multivectors.Normalized_Point)
                            return MV_Analysis is
        use Multivectors;
        Met           : constant Metric.Metric_Record := Metric.C3_Metric;
        Grade         : Integer :=
                          Multivector_Type.MV_Grade (Analysis_In.M_MV_Type);
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
        theAnalysis   : MV_Analysis (Flat_Analysis);
    begin
        Init_Analysis_Type (Analysis_In, theAnalysis);
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
        --  theAnalysis.M_Vectors m_vc[0] .. m_vc[1] = unit 3D vector basis
        --  for attitude (direction)
        --  theAnalysis.Scalars   m_sc[0] = weight
        --  ************* END format of flat ***************
        theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Location);
        theAnalysis.Weight := Weight;
        --  Grade indications are taken from Geometric Algebra and its
        --  Application to Computer Graphics by Hildenbrand, Fontijne,
        --  Perwass and Dorst, Eurographics 2004.
        --  "the representation of a point is simply a sphere of radius zero".
        --  factor attitude:
        case Grade is
            when 1 => theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Scalar_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
            when 2 => theAnalysis.M_Type.Blade_Subclass := Point_Subclass;
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Point_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Point_Subclass;
            when 3 =>  --  Line
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Line_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Line_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Unit_E (Left_Contraction (C3GA.no, Attitude, Met)));
            when 4 =>  --  Plane
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Flat, Plane_Subclass.");
                theAnalysis.M_Type.Blade_Subclass := Plane_Subclass;
--                   GA_Utilities.Print_Multivector_String
--                    ("Multivector_Analyze_C3GA.Analyze_Flat, " &
--                      "Plane Subclass Reverse_MV (LC (C3GA.no, Reverse_MV (Attitude)",
--                     Reverse_MV (Left_Contraction (C3GA.no, Reverse_MV (Attitude), Met)),
--                       Blade_Types.Basis_Names_C3GA);
                Blade_Factors := Multivector_Utilities.Factorize_Blades
                  (Reverse_MV (Left_Contraction (C3GA.no, Reverse_MV (Attitude), Met)),
                   Scale);
                --  Blade_Factors agrees with C++: -1.00E+00 * e1 -1.00E+00 * e3
--                  GA_Utilities.Print_Multivector_List_String
--                    ("Multivector_Analyze_C3GA.Analyze_Flat, " &
--                      "Plane Subclass Blade_Factors", Blade_Factors, Blade_Types.Basis_Names_C3GA);
            theAnalysis.M_Vectors (1) :=
              C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 1));
            theAnalysis.M_Vectors (2) :=
              C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 2));
            theAnalysis.M_Vectors (3) :=
              C3GA.To_VectorE3GA (-Dual (Outer_Product (Get_Multivector (Blade_Factors, 1),
                                  Get_Multivector (Blade_Factors, 2)), Metric.C3_Metric));
                --  M_Vectors agrees with C++: -1.00E+00 * e1 -1.00E+00 * e2 1.00E+00 * e3
            when others => null;
        end case;
       return theAnalysis;

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
    function Analyze_Free (Analysis_In : MV_Analysis;
                           MV          : Multivectors.Multivector)
                           return MV_Analysis is
        use Metric;
        use Multivectors;
        Grade         : constant Integer :=
                          Multivector_Type.MV_Grade (Analysis_In.M_MV_Type);
        Weight        : constant Float := Norm_E (MV);
        --        Attitude      : constant Multivector := MV;
        Blade_Factors : Multivectors.Multivector_List;
        Scale         : Float := 1.0;
        theAnalysis   : MV_Analysis (Free_Analysis);
    begin
        Init_Analysis_Type (Analysis_In, theAnalysis);
        theAnalysis.M_Type.Blade_Class := Free_Blade;
        theAnalysis.Points (1) := (0.0, 0.0, 0.0);
        theAnalysis.Weight := Weight;
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
                  C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (Get_Multivector (Blade_Factors, 1),
                                      Get_Multivector (Blade_Factors, 2)), C3_Metric));
            when 4 =>  --  F Trivector
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade 4.");
                theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others =>
                Put_Line ("Multivector_Analyze_C3GA.Analyze_Free Grade others.");
        end case;
        return theAnalysis;

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
    function Analyze_Round (Analysis_In : MV_Analysis;
                            MV          : Multivectors.Multivector)
                            return MV_Analysis is
        use Maths.Single_Math_Functions;
        use Multivectors;
        Met              : constant Metric.Metric_Record :=  Metric.C3_Metric;
        MV_X             : Multivector := MV;
        Grade            : constant Integer :=
                             Multivector_Type.MV_Grade (Analysis_In.M_MV_Type);
        MV_Factors       : Multivector_List;
        LC               : Multivector;
        LC_NI_MV         : Multivector;
        LC_NI_MV_Inverse : Multivector;
        SCP_Inverse      : Float;
        Attitude         : Multivector;
        Location         : Multivector;
        Point_Location   : Multivectors.Normalized_Point;
        NI_Xsq           : Float;
        Radius           : Float;
        Radius_Sq        : Float;
        Weight           : Float;
        Scale            : Float := 1.0;
        theAnalysis      : MV_Analysis (Round_Analysis);
    begin
--          Put_Line ("Multivector_Analyze_C3GA.Analyze_Round, Grade:" &
--                      Integer'Image (Grade));
        Init_Analysis_Type (Analysis_In, theAnalysis);
        if Grade = 0 then
            theAnalysis.M_Type.Blade_Class := Scalar_Blade;
            theAnalysis.M_Type.Blade_Subclass := Scalar_Subclass;
            theAnalysis.Weight := Scalar_Part (MV_X);  --  signed weight
        else
            theAnalysis.M_Type.Blade_Class := Round_Blade;
            if Grade = 1 then
                MV_X := Dual (MV_X, Metric.C3_Metric);
                --  m_flags = m_flags xor FLAG_DUAL?
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
            end if;

            --  MV_X checked OK against C++ version
            LC_NI_MV := Left_Contraction (C3GA.ni, MV_X,  Metric.C3_Metric);
--              GA_Utilities.Print_Multivector_String
--                   ("Multivector_Analyze_C3GA.Analyze_Round LC_NI_MV", LC_NI_MV,
--                    Blade_Types.Basis_Names_C3GA);

            Attitude :=
              Negate (Outer_Product (LC_NI_MV, C3GA.ni));

            LC_NI_MV_Inverse := General_Inverse (LC_NI_MV, Met);
             --  location is normalized dual sphere
            Location := Geometric_Product (MV_X, LC_NI_MV_Inverse, Met);  --  _location
            --           Put_Line ("Multivector_Analyze_C3GA.Analyze_Round calling Geometric_Product 2");
            SCP_Inverse := -1.0 / Scalar_Product (C3GA.ni, Location, Met);
            Location := Geometric_Product (Location, SCP_Inverse);
            --  normalizedPoint location = c3gaPoint(_vectorE3GA(_location));
            Point_Location := To_Normalized_Point (Location);

            NI_Xsq := Scalar_Product (LC_NI_MV, LC_NI_MV, Met);
            --  Grade_Inversion is eqivalent to Grade_Involution
            Radius_Sq := Scalar_Part
              (Geometric_Product (MV_X, Grade_Inversion (MV_X), Met)) / NI_Xsq;
            if Radius_Sq < 0.0 then
                Radius_Sq := -Radius_Sq;
            end if;

            Radius := Float (Sqrt (GL.Types.Single (Abs (Radius_Sq))));
            Weight := Norm_E (Left_Contraction (C3GA.no, Attitude, Met));

            theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
            theAnalysis.Radius := Radius;
            theAnalysis.Weight := Weight;
             --  Factor attitude (direction):
            case Grade is
            when 1 =>  --  (dual) sphere
                theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                theAnalysis.M_Flags.Dual := not theAnalysis.M_Flags.Dual;
                --  direction
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when 2 =>  --  point pair
                theAnalysis.M_Type.Blade_Subclass := Point_Pair_Subclass;
                 LC := Left_Contraction (C3GA.no, Attitude, Met);
                if GA_Utilities.Multivector_Size (LC) > 0 then
                    theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (Unit_E (LC));
                else
                    raise Analyze_C3GA_Exception with
                      "Multivector_Analyze_C3GA.Analyze_Round, " &
                      "Grade 2 Left Contraction is zero.";
                end if;
            when 3 =>
                --  circle explicit factorization required:
                theAnalysis.M_Type.Blade_Subclass := Circle_Subclass;
                MV_X := Reverse_MV (Left_Contraction (C3GA.no,
                                    Reverse_MV (Attitude), Met));
--                  GA_Utilities.Print_Multivector_String
--                    ("Multivector_Analyze_C3GA.Analyze_Round grade 3 MV_X",
--                     MV_X, Blade_Types.Basis_Names_C3GA);
                --  Returned scale not used
                MV_Factors :=
                  Multivector_Utilities.Factorize_Blades (MV_X, Scale);
--                  GA_Utilities.Print_Multivector_List_String
--                    ("Multivector_Analyze_C3GA.Analyze_Round MV_Factors",
--                     MV_Factors, Blade_Types.Basis_Names_C3GA);
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Get_Multivector (MV_Factors, 1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Get_Multivector (MV_Factors, 2));
                if E3GA.Is_Zero (theAnalysis.M_Vectors (1)) or
                 E3GA.Is_Zero (theAnalysis.M_Vectors (2)) then
                    theAnalysis.M_Vectors (3) := (0.0, 0.0, 1.0);
                else
                   theAnalysis.M_Vectors (3) :=
                      C3GA.To_VectorE3GA
                        (Dual (Outer_Product (Get_Multivector (MV_Factors, 1),
                                Get_Multivector (MV_Factors, 2)), Met));
                end if;
            when 4 =>
                theAnalysis.M_Type.Blade_Subclass := Sphere_Subclass;
                theAnalysis.M_Vectors (1) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others =>
               Put_Line
                 ("Multivector_Analyze_C3GA.Analyze_Round, unprocessed Grade: " &
                  Integer'Image (Grade));
            end case;
            --                  Print_Analysis ("Multivector_Analyze_C3GA.Analyze_Round,",
            --                                  theAnalysis);
--              Print_E3_Vector_Array
--                ("Multivector_Analyze_C3GA.Analyze_Round direction M_Vectors",
--                 theAnalysis.M_Vectors);
        end if;
        return theAnalysis;

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
    function Analyze_Tangent (Analysis_In : MV_Analysis;
                              MV          : Multivectors.Multivector)
                              return MV_Analysis is
        use Multivectors;
        Met            : constant Metric.Metric_Record :=  Metric.C3_Metric;
        Grade          : constant Integer :=
                           Multivector_Type.MV_Grade (Analysis_In.M_MV_Type);
        LC_NI_MV       : Multivector;
        LC_NI_MV_Inv   : Multivector;
        Attitude       : constant Multivector :=
                           Negate (Outer_Product (LC_NI_MV, C3GA.ni));
        Blade_Factors  : Multivectors.Multivector_List;
        Location       : Multivector;
        Point_Location : Multivectors.Normalized_Point;
        Scale          : Float;
        Weight         : Float;
        theAnalysis    : MV_Analysis (Tangent_Analysis);
    begin
        Init_Analysis_Type (Analysis_In, theAnalysis);
        theAnalysis.M_Type.Blade_Class := Tangent_Blade;

        LC_NI_MV := Left_Contraction (C3GA.ni, MV, Met);
        LC_NI_MV_Inv := General_Inverse (LC_NI_MV, Met);
        Location :=
          Geometric_Product (MV, LC_NI_MV_Inv, Met);
        Location := Geometric_Product
          (Location, -1.0 / Scalar_Product (C3GA.ni, Location, Met));
        Point_Location := C3GA.Set_Normalized_Point (C3GA.To_VectorE3GA (Location));
        Weight := Norm_E (Left_Contraction (C3GA.no, Attitude, Met));

        theAnalysis.Points (1) := C3GA.NP_To_VectorE3GA (Point_Location);
        theAnalysis.Weight := Weight;

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
                  C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 1));
                theAnalysis.M_Vectors (2) :=
                  C3GA.To_VectorE3GA (Get_Multivector (Blade_Factors, 2));
                theAnalysis.M_Vectors (3) :=
                  C3GA.To_VectorE3GA (-Dual (Outer_Product (Get_Multivector (Blade_Factors, 1),
                                      Get_Multivector (Blade_Factors, 2)), Metric.C3_Metric));
            when 4 =>
                theAnalysis.M_Type.Blade_Subclass := Trivector_Subclass;
                theAnalysis.M_Vectors (1) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e1));
                theAnalysis.M_Vectors (2) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e2));
                theAnalysis.M_Vectors (3) := C3GA.To_VectorE3GA (Basis_Vector (Blade_Types.E3_e3));
            when others => null;
        end case;
      return theAnalysis;

    end Analyze_Tangent;

    --  ----------------------------------------------------------------------------

    procedure Init_Analysis_Type (Analysis_In : MV_Analysis;
                                  theAnalysis : in out MV_Analysis) is
    begin
        theAnalysis.Epsilon := Analysis_In.Epsilon;
        theAnalysis.M_Type := Analysis_In.M_Type;
        theAnalysis.M_Flags := Analysis_In.M_Flags;
    end Init_Analysis_Type;

    --  ----------------------------------------------------------------------------

end Multivector_Analyze_C3GA;
