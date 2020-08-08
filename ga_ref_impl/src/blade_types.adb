
package body Blade_Types is

    BV_Names : Basis_Vector_Names;

    --  -------------------------------------------------------------------------

    function Basis_Names_C3GA return Basis_Vector_Names is
    begin
        return BV_Names;
    end Basis_Names_C3GA;

    --  -------------------------------------------------------------------------

begin
    BV_Names.Append (To_Unbounded_String ("no"));
    BV_Names.Append (To_Unbounded_String ("e1"));
    BV_Names.Append (To_Unbounded_String ("e2"));
    BV_Names.Append (To_Unbounded_String ("e3"));
    BV_Names.Append (To_Unbounded_String ("ni"));
end Blade_Types;
