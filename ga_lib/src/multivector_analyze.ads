
with Blade_Types;
with C3GA;
with E3GA;
with GA_Maths;
with Multivectors;
with Multivector_Type;
with Multivector_Type_Base;

package Multivector_Analyze is

   Flag_Invalid    : constant boolean := false;
   Flag_Valid      : constant boolean := true;
   Max_Points      : constant integer := 3;
   Max_Scalars     : constant integer := 3;
   Max_Vectors     : constant integer := 3;
   Default_Epsilon : constant float := 10.0 ** (-5);

   type Flag_Type is record
      Valid : boolean := Flag_Invalid;
      Dual  : boolean := false;
   end record;

   type Analysis_Type is (Unspecified_Analysis, Free_Analysis, Flat_Analysis,
                          Round_Analysis, Scalar_Analysis, Tangent_Analysis);
   type Blade_Type is (Non_Blade, Scalar_Blade, Flat_Blade, Round_Blade,
                       Tangent_Blade, Free_Blade, Pseudo_Scalar_Blade, Zero_Blade);
   type Blade_Subclass_Type is (Unspecified_Subclass, Point_Subclass,
                                Point_Pair_Subclass, Line_Subclass,
                                Circle_Subclass, Plane_Subclass, Scalar_Subclass,
                                Pseudo_Scalar_Subclass, Vector_Subclass,
                                Sphere_Subclass, Bivector_Subclass, Trivector_Subclass);
   type Conformal_Type is (Not_Conformal, Conformal_Multivector, Conformal_Versor,
                           Conformal_Blade);
   type Conformal_Blade_Class is (Blade_Scalar, Blade_Flat, Blade_Round,
                                  Blade_Tangent, Blade_Free, Blade_Zero);
   type Flat_Type is (Not_A_Flat, Flat_Point, Flat_Line, Flat_Plane);
   type Model_Type is (Vector_Space, Homogenous_Model, Conformal_Model);

   type M_Type_Type is (Unspecified_Type, Scalar_Type, Vector_Type,
                        Bivector_Type, Trivector_Type, Point_Type, Point_Pair_Type,
                        Line_Type, Circle_Type, Plane_Type, Sphere_Type);

   type N_Vector_Type is (N_Vector, N_Bivector, N_Trivector);
   type Round_Type is (Not_A_Round, Round_Point_Pair, Round_Circle, Round_Sphere);
   type Versor_Subclass_Type is (Not_A_Versor, Even_Versor, Odd_Versor, Rotor_Versor);

--  mv_analysis
   type M_Type_Record is record
      --  mv_analyze.h data format:
      --  m_pt   points
      --  m_vc   vectors
      --  m_sc   scalars
      --  m_type(0 .. 3), Intended use:
	  --  m_type[0] = model
	  --  m_type[1] = multivector type (c3ga_type::BLADE, c3ga_type::VERSOR, c3ga_type::MULTIVECTOR,)
	  --  m_type[2] = class (round, flat, free, etc)
	  --  m_type[3] = grade / class dependent
      --  m_flags
      --  m_epsilon
      --  m_mvType

      Model_Kind       : Model_Type := Vector_Space;           --  m_type[0]
      Multivector_Kind : Multivector_Type_Base.Object_Type :=  --  m_type[1]
                           Multivector_Type_Base.MV_Object;
      Blade_Class      : Blade_Type := Non_Blade;              --  m_type[2]
                                     --  m_type[3] = grade / class dependent
      M_Grade          : GA_Maths.Grade_Usage;
--        MV_Subtype       : M_Type_Type := Unspecified_Type;
      Blade_Subclass   : Blade_Subclass_Type := Unspecified_Subclass;  --  m_type[3]
      Versor_Subclass  : Versor_Subclass_Type := Not_A_Versor;       --  m_type[3]
--        Round_Kind       : Round_Type := Round_Invalid;
   end record;

   type Point_Array is array (1 .. Max_Points) of E3GA.E3_Vector;
   type Scalar_Array is array (1 .. Max_Scalars) of float;
   type E3_Vector_Array is array (1 .. Max_Vectors) of E3GA.E3_Vector;

   type MV_Analysis (Analyze : Analysis_Type := Unspecified_Analysis) is record
      M_Flags          : Flag_Type := (Flag_Valid, False);
      --  MV_Type is Multivector_Type_Base.Type_Base; --  m_mvType
      M_MV_Type        : Multivector_Type.MV_Type_Record;
      Conformal_Kind   : Conformal_Type := Not_Conformal;
      Epsilon          : Float;
      M_Type           : M_Type_Record;
      Pseudo_Scalar    : Boolean := False;
      Versor_Kind      : Versor_Subclass_Type := Not_A_Versor;
      --  Each analyzed multivector is decomposed into
      --  (analysis dependent) points, scalars and vectors.
      Points           : Point_Array := (others => (0.0, 0.0, 0.0));  --  m_pt
      M_Vectors        : E3_Vector_Array := (others => (others => 0.0));
      --  m_sc  Float array
      Weight : Float := 0.0;
      case Analyze is
         when Round_Analysis =>
            Radius : Float := 0.0;
         when others =>
              null;
      end case;
   end record;

   MVA_Exception : Exception;

--     procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
--                       Flags : Flag_Type := (Flag_Invalid, False);
--                       Epsilon : float := Default_Epsilon);
--     procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
   function Analyze (MV : Multivectors.Multivector;
                     Probe : Multivectors.Normalized_Point := C3GA.Probe (Blade_Types.C3_no);
                     Flags : Flag_Type := (Flag_Invalid, False);
                     Epsilon : float := Default_Epsilon) return MV_Analysis;
   function Blade_Subclass (A : MV_Analysis) return Blade_Subclass_Type;
   function isValid (A : MV_Analysis) return Boolean;
   function isDual (A : MV_Analysis) return Boolean;

   function isBlade (A : MV_Analysis) return Boolean;
   function isVersor (A : MV_Analysis) return Boolean;
   function isNull (A : MV_Analysis) return Boolean;
   function isZero (A : MV_Analysis)  return Boolean;
   function Num_Points return integer;
   function Num_Vectors return integer;
   function Num_Scalars return integer;
   procedure Print_Analysis (Name : String;
                             Analysis : MV_Analysis);
   procedure Print_Analysis_M_Vectors (Name : String; Analysis : MV_Analysis);
   procedure Print_Analysis_Points (Name : String; Analysis : MV_Analysis);
   procedure Print_E3_Vector_Array (Name : String; anArray : E3_Vector_Array);
   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type;

end Multivector_Analyze;
