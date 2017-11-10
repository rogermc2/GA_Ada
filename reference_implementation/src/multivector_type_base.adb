
package body Multivector_Type_Base is

--  ----------------------------------------------------------------------------

--      procedure Set_Grade_Usage (Base : in out Type_Base; GU : GA_Maths.Grade_Usage) is
--        theBase : Type_Base;
--      begin
--        theBase.M_GU := GU;
--      end Set_Grade_Usage;

--  ----------------------------------------------------------------------------

--      procedure Set_M_Type (Base : in out Type_Base; theType : Object_Type) is
--      begin
--         Base.M_Type := theType;
--      end Set_M_Type;

--  ----------------------------------------------------------------------------

--      procedure Set_Parity (Base : in out Type_Base; Par : Parity) is
--      begin
--         Base.M_Parity := Par;
--      end Set_Parity;

--  ----------------------------------------------------------------------------

--      procedure Set_Top_Grade (Base : in out Type_Base; Grade :Integer) is
--      begin
--         Base.M_Top_Grade := Grade;
--      end Set_Top_Grade;

--  ----------------------------------------------------------------------------

    procedure Set_Type_Base (Base : in out MV_Typebase; Zero : boolean;
                             Object : Object_Type; Grade : integer;
                             GU : GA_Maths.Grade_Usage; Par : Parity := No_Parity) is
    begin
       Base.M_Zero := Zero;
       Base.M_Type := Object;
       Base.M_Top_Grade := Grade;
       Base.M_Grade := GU;
       Base.M_Parity := Par;
    end Set_Type_Base;

--  ----------------------------------------------------------------------------

end Multivector_Type_Base;
