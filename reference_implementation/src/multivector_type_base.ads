
with GA_Maths;

package Multivector_Type_Base is

   type Model_Type is (Vector_Space_Model, Homogenous_Model, Conformal_Model);
   type M_Type_Type is (Invalid_Base_Type, Unused_Type, Zero,
                        Vector_Space_Model, Blade, Bivector, Trivector,
                        Even_Versor);
    type Blade_Type is (Scalar_Blade, Flat_Blade, Round_Blade, Tangent_Blade,
                        Free_Blade, Zero_Blade);
    type Object_Type is (Multivector_Object, Versor_Object, Blade_Object);
    type Parity is (No_Parity, Even_Parity, Odd_Parity);
    type Round_Type is (Point_Pair_Round, Circle_Round, Sphere_Round);
    type N_Vector_Type is (N_Vector, N_Bivector, N_Trivector);
    type Versor_Type is (Even_Versor, Odd_Versor, Rotor_Versor);

    type Type_Base is record
        M_Zero        : boolean := False; -- True if multivector is zero
        M_Type        : Object_Type := Multivector_Object;
        M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
        M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
        M_Parity      : Parity := No_Parity;
    end record;

   type Type_Model is record
      Model  : Model_Type := Vector_Space_Model;
      M_Type : M_Type_Type := Invalid_Base_Type;
      Blade  : Blade_Type;
   end record;

    function Get_Current_Type_Base return Type_Base;
    procedure Set_Current_Type_Base (Zero : boolean; Object : Object_Type;
                                     Grade : integer; GU : GA_Maths.Grade_Usage;
                                     Par : Parity := No_Parity);
--      procedure Set_Grade_Usage (Base : in out Type_Base; GU : GA_Maths.Grade_Usage);
    procedure Set_M_Type (M_Type : Object_Type);
--      procedure Set_M_Type (Base : in out Type_Base; theType : Object_Type);
--      procedure Set_Parity (Base : in out Type_Base; Par : Parity);
--      procedure Set_Top_Grade (Base : in out Type_Base; Grade : Integer);
    procedure Set_Type_Base (Base : in out Type_Base; Zero : boolean;
                             Object : Object_Type; Grade : integer;
                             GU : GA_Maths.Grade_Usage; Par : Parity := No_Parity);
private

    type Flag is (Valid);

end Multivector_Type_Base;
