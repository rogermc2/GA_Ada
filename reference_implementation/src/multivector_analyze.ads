
with E2GA;
with E3GA;
with GA_Maths;
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
   type Blade_Type is (Invalid_Blade, Scalar_Blade, Flat_Blade, Round_Blade,
                       Tangent_Blade, Free_Blade, Zero_Blade);
   type Blade_Subclass_Type is (Vector_Subclass, Bivector_Subclass,
                                Even_Versor_Subclass);
   type Conformal_Type is (Invalid_Model, Multivector_Model, Versor_Model,
                           Blade_Model);
   type Flat_Type is (Flat_Invalid, Flat_Point, Flat_Line, Flat_Plane);
   type Model_Type is (Vector_Space, Homogenous_Model, Conformal_Model);

   type Round_Type is (Round_Invalid, Round_Point_Pair, Round_Circle, Round_Sphere);
   type Versor_Type is (Invalid_Versor, Even_Versor, Odd_Versor, Rotor_Versor);

   type M_Type is record
      Model_Kind       : Model_Type := Vector_Space;
      Multivector_Kind : Multivector_Type_Base.M_Type_Type :=
                           Multivector_Type_Base.Invalid_Base_Type;
      Blade_Class      : Blade_Type;
      Blade_Subclass   : Blade_Subclass_Type;
      Versor_Subclass  : Versor_Type;
      Round_Kind       : Round_Type := Round_Invalid;
   end record;

   type Point_Array is array (1 .. Number_Of_Points) of E3GA.Vector_3D;
   type Scalar_Array is array (1 .. Number_Of_Scalars) of float;
   type Vector_Array is array (1 .. Number_Of_Vectors) of E3GA.Vector_3D;

   type MV_Analysis is record
      M_Flags          : Flag_Type := (Flag_Valid, False);
      M_MV_Type          : Multivector_Type_Base.M_Type_Type;
      Conformal_Kind   : Conformal_Type := Invalid_Model;
      Epsilon          : Float;
      Analysis_Type    : M_Type;
      Pseudo_Scalar    : Boolean := False;
      Versor_Kind      : Versor_Type := Invalid_Versor;
      --  Each analyzed multivector is decomposed into
      --  (analysis dependent) points, vectors and scalars.
      M_Points         : Point_Array;
      M_Scalors        : Scalar_Array;
      M_Vectors        : Vector_Array;
   end record;

   function Default_Epsilon return float;  --  Must precede Analyze
   procedure Analyze (MV : in out E2GA.Multivector;
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
   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type;

end Multivector_Analyze;
