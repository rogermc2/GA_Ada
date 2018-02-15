
with C3GA;
with GA_Maths;
with Multivectors;
with Multivector_Type;
with Multivector_Type_Base;

package Multivector_Analyze is

   Flag_Invalid          : constant boolean := false;
   Flag_Valid            : constant boolean := true;
   Number_Of_Points      : integer := 3;
   Number_Of_Scalars     : integer := 3;
   Number_Of_Vectors     : integer := 3;
--   Number_Of_Type_Levels : integer := 4;   Not required as M_Type is a record

   --  subtype MV_Type is declared in E2GA to prevent circular dependency
   --  subtype MV_Type is Multivector_Type_Base.M_Type_Type; --  m_mvType

   type Flag_Type is record
      Valid : boolean := Flag_Invalid;
      Dual  : boolean := false;
   end record;

   type Blade_Type is (Non_Blade, Scalar_Blade, Flat_Blade, Round_Blade,
                       Tangent_Blade, Free_Blade, Pseudo_Scalar_Blade, Zero_Blade);
   type Blade_Subclass_Type is (Unspecified_Subclass, Point_Subclass, Line_Subclass,
                           Plane_Subclass, Vector_Subclass, Bivector_Subclass);
--     type Blade_Subclass_Type is (Unspecified_Blade_Subclass,
--                                  Even_Versor_Subclass);
   type Conformal_Type is (Not_Conformal, Conformal_Multivector, Conformal_Versor,
                           Conformal_Blade);
   type Conformal_Blade_Class is (Blade_Scalar, Blade_Flat, Blade_Round,
                                  Blade_Tangent, Blade_Free, Blade_Zero);
   type Flat_Type is (Flat_Invalid, Flat_Point, Flat_Line, Flat_Plane);
   type Model_Type is (Vector_Space, Homogenous_Model, Conformal_Model);

   type M_Type_Type is (Unspecified_Type, Scalar_Type, Vector_Type,
                        Bivector_Type, Trivector_Type, Point_Type, Point_Pair_Type,
                        Line_Type, Circle_Type, Plane_Type, Sphere_Type);

   type N_Vector_Type is (N_Vector, N_Bivector, N_Trivector);
   type Round_Type is (Round_Invalid, Round_Point_Pair, Round_Circle, Round_Sphere);
   type Versor_Subclass_Type is (Invalid_Versor, Even_Versor, Odd_Versor, Rotor_Versor);

--  mv_analysis
   type M_Type_Record is record
      Model_Kind       : Model_Type := Vector_Space;  --  m_type[0] = model
       --  m_type[1] = multivector type (c3ga_type::BLADE, c3ga_type::VERSOR,
       --              c3ga_type::MULTIVECTOR,)
      Multivector_Kind : Multivector_Type_Base.Object_Type :=
                           Multivector_Type_Base.Multivector_Object;
      Blade_Class      : Blade_Type := Non_Blade;  --  m_type[2] = class (round, flat, free, etc)
      --  m_type[3] = grade / class dependent
      M_Grade          : GA_Maths.Grade_Usage;
      MV_Subtype       : M_Type_Type := Unspecified_Type;
      Blade_Subclass   : Blade_Subclass_Type := Unspecified_Subclass;
      Versor_Subclass  : Versor_Subclass_Type := Invalid_Versor;
      Round_Kind       : Round_Type := Round_Invalid;
   end record;

   type Point_Array is array (1 .. Number_Of_Points) of Multivectors.Vector;
   type Scalar_Array is array (1 .. Number_Of_Scalars) of float;
   type Vector_Array is array (1 .. Number_Of_Vectors) of Multivectors.Vector;

   type MV_Analysis is record
      M_Flags          : Flag_Type := (Flag_Valid, False);
      --  MV_Type is Multivector_Type_Base.Type_Base; --  m_mvType
      M_MV_Type        : Multivector_Type.MV_Type_Record;
      Conformal_Kind   : Conformal_Type := Not_Conformal;
      Epsilon          : Float;
      M_Type           : M_Type_Record;
      Pseudo_Scalar    : Boolean := False;
      Versor_Kind      : Versor_Subclass_Type := Invalid_Versor;
      --  Each analyzed multivector is decomposed into
      --  (analysis dependent) points, vectors and scalars.
      M_Points         : Point_Array;
      M_Scalors        : Scalar_Array;
      M_Vectors        : Vector_Array;
   end record;

   function Default_Epsilon return float;  --  Must precede Analyze
   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                     Flags : Flag_Type := (Flag_Invalid, False);
                     Epsilon : float := Default_Epsilon);
   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Probe : C3GA.Normalized_Point;
--                        Probe : C3GA.Normalized_Point := C3GA.Probe (Blade.C3_no));
                      Flags : Flag_Type := (Flag_Invalid, False);
                      Epsilon : float := Default_Epsilon);
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
                             Info : MV_Analysis);
   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type;

end Multivector_Analyze;
