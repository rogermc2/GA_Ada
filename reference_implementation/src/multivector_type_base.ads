
with GA_Maths;

package Multivector_Type_Base is

    --  Object_Type mvtypebase.h lines 8 - 13 and 36
    type Object_Type is (Multivector_Object, Versor_Object, Blade_Object);
    type Parity is (No_Parity, Even_Parity, Odd_Parity);  --  line 43

   --  mvtypebase.h lines 33 - 43
    type MV_Typebase is record
        M_Zero        : boolean := False; -- True if multivector is zero
        M_Type        : Object_Type := Multivector_Object;
        M_Top_Grade   : integer := -1;    --  Top grade occupied by the multivector
        M_Grade       : GA_Maths.Grade_Usage := 0; --  Bit map indicating which grades are present
        M_Parity      : Parity := No_Parity;
    end record;

--      procedure Set_Grade_Usage (Base : in out Type_Base; GU : GA_Maths.Grade_Usage);
--      procedure Set_M_Type (Base : in out Type_Base; theType : Object_Type);
--      procedure Set_Parity (Base : in out Type_Base; Par : Parity);
--      procedure Set_Top_Grade (Base : in out Type_Base; Grade : Integer);
    procedure Set_Type_Base (Base : in out MV_Typebase; Zero : boolean;
                             Object : Object_Type; Grade : integer;
                             GU : GA_Maths.Grade_Usage; Par : Parity := No_Parity);
private

    type Flag is (Valid);

end Multivector_Type_Base;
