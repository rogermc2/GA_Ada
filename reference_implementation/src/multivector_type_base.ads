
with GA_Maths;

package Multivector_Type_Base is

   type M_Type_Type is (Invalid_Base_Type, Unused_Type, Zero,
                        Vector_Space_Model, Blade, Bivector, Trivector,
                        Even_Versor);
    type Object_Type is (Multivector_Object, Versor_Object, Blade_Object);
    type Parity is (No_Parity, Even_Parity, Odd_Parity);
    type Type_Base is private;

    function Get_Current_Type_Base return Type_Base;
    procedure Set_Current_Type_Base (Zero : boolean; Object : Object_Type;
                                     Grade : integer; GU : GA_Maths.Grade_Usage;
                                     Par : Parity := No_Parity);
    procedure Set_Grade_Usage (Base : in out Type_Base; GU : GA_Maths.Grade_Usage);
    procedure Set_M_Type (M_Type : Object_Type);
    procedure Set_M_Type (Base : in out Type_Base; theType : Object_Type);
    procedure Set_Parity (Base : in out Type_Base; Par : Parity);
    procedure Set_Top_Grade (Base : in out Type_Base; Grade : Integer);
    procedure Set_Type_Base (Base : in out Type_Base; Zero : boolean;
                             Object : Object_Type; Grade : integer;
                             GU : GA_Maths.Grade_Usage; Par : Parity := No_Parity);
private
    type Type_Base is record
        M_Zero        : boolean := False; -- True if multivector is zero
        M_Type        : Object_Type := Multivector_Object;
        M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
        M_GU          : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
        M_Parity      : Parity := No_Parity;
    end record;

    type Flag is (Valid);

end Multivector_Type_Base;
