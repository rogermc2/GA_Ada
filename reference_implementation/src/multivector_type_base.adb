
package body Multivector_Type_Base is

    Current_Type_Base : Type_Base;

--  ----------------------------------------------------------------------------

    procedure Set_Grade_Usage (Base : in out Type_Base; GU : GA_Maths.Grade_Usage) is
      theBase : Type_Base;
    begin
      theBase.M_GU := GU;
    end Set_Grade_Usage;

--  ----------------------------------------------------------------------------

    function Get_Current_Type_Base return Type_Base is
    begin
        return Current_Type_Base;
    end Get_Current_Type_Base;

--  ----------------------------------------------------------------------------

    procedure Set_Current_Type_Base (Zero : boolean; Object : Object_Type;
                             Grade : integer; GU : GA_Maths.Grade_Usage;
                             Par : Parity := No_Parity) is
    begin
       Current_Type_Base.M_Zero := Zero;
       Current_Type_Base.M_Type := Object;
       Current_Type_Base.M_Top_Grade := Grade;
       Current_Type_Base.M_GU := GU;
       Current_Type_Base.M_Parity := Par;
    end Set_Current_Type_Base;

--  ----------------------------------------------------------------------------

    procedure Set_M_Type (M_Type : Object_Type) is
    begin
       Current_Type_Base.M_Type := M_Type;
    end Set_M_Type;

--  ----------------------------------------------------------------------------

    procedure Set_Parity (Base : in out Type_Base; Par : Parity) is
    begin
       Base.M_Parity := Par;
    end Set_Parity;

--  ----------------------------------------------------------------------------

    procedure Set_Top_Grade (Base : in out Type_Base; Grade :Integer) is
    begin
       Base.M_Top_Grade := Grade;
    end Set_Top_Grade;

--  ----------------------------------------------------------------------------

    procedure Set_Type_Base (Base : in out Type_Base; Zero : boolean;
                             Object : Object_Type; Grade : integer;
                             GU : GA_Maths.Grade_Usage; Par : Parity := No_Parity) is
    begin
       Base.M_Zero := Zero;
       Base.M_Type := Object;
       Base.M_Top_Grade := Grade;
       Base.M_GU := GU;
       Base.M_Parity := Par;
    end Set_Type_Base;

--  ----------------------------------------------------------------------------

end Multivector_Type_Base;
