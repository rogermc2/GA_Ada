with E2GA;
with GA_Maths;
with Multivector_Type_Base;

package Multivector_Analysis is
   Default_Epsilon_Value : float := 10.0 ** (-5);
   Flag_Invalid          : constant boolean := false;
   Flag_Valid            : constant boolean := true;
   Number_Of_Points      : integer := 3;
   Number_Of_Scalars     : integer := 3;
   Number_Of_Vectors     : integer := 3;

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

   subtype Multivector_Type is Multivector_Type_Base.M_Type_Type; --  m_mvType

   type Round_Type is (Round_Invalid, Round_Point_Pair, Round_Circle, Round_Sphere);
   type Versor_Type is (Invalid_Versor, Even_Versor, Odd_Versor, Rotor_Versor);

   --   Intended use of M_Type:
   --   m_type[0] = model
   --   m_type[1] = multivector type (c3ga_type : BLADE, c3ga_type : VERSOR,
   --               c3ga_type :  : MULTIVECTOR, )
   --   m_type[2] = class (round, flat, free, etc)
   --   m_type[3] = grade / class dependent
   type M_Type is record
      Model_Kind       : Model_Type := Vector_Space;
      Multivector_Kind : Multivector_Type := Multivector_Type_Base.Invalid_Base_Type;
      Blade_Class      : Blade_Type;
      Blade_Subclass   : Blade_Subclass_Type;
      Round_Kind       : Round_Type := Round_Invalid;
   end record;

   type Point_Array is array (1 .. Number_Of_Points) of GA_Maths.Vector;
   type Scalar_Array is array (1 .. Number_Of_Scalars) of integer;
   type Vector_Array is array (1 .. Number_Of_Vectors) of GA_Maths.Vector;

   type MV_Analysis is record
      Flag             : Flag_Type := (Flag_Valid, False);
      MV_Kind          : Multivector_Type_Base.M_Type_Type;
      Conformal_Kind   : Conformal_Type := Invalid_Model;
      Epsilon          : Float := Default_Epsilon_Value;
      Analysis_Type    : M_Type;
      Pseudo_Scalar    : Boolean := False;
      Versor_Kind      : Versor_Type := Invalid_Versor;
      M_Points         : Point_Array;
      M_Scalors        : Scalar_Array;
      M_Vectors        : Vector_Array;
   end record;

   function Default_Epsilon return float;
   function Analyze (MV : in out E2GA.Multivector;
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
   function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type;

end Multivector_Analysis;
