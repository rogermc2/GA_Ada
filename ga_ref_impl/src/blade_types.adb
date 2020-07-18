
package body Blade_Types is

    function Basis_Names_C3GA return Basis_Vector_Names is
        BV_Names : Basis_Vector_Names;
    begin
        BV_Names.Append (To_Unbounded_String ("no"));
        BV_Names.Append (To_Unbounded_String ("e1"));
        BV_Names.Append (To_Unbounded_String ("e2"));
        BV_Names.Append (To_Unbounded_String ("e3"));
        BV_Names.Append (To_Unbounded_String ("ni"));
        return BV_Names;
    end Basis_Names_C3GA;

    --  -------------------------------------------------------------------------

end Blade_Types;
