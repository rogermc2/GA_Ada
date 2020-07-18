
package body Blade_Types is

    function Basis_Names_C3GA return Basis_Vector_Names is
        BV_Names : Basis_Vector_Names;
    begin
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_no)),
                                  To_Unbounded_String ("no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1)),
                                  To_Unbounded_String ("e1"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e2)),
                                  To_Unbounded_String ("e2"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e3)),
                                  To_Unbounded_String ("e3"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_ni)),
                                  To_Unbounded_String ("ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2)),
                                  To_Unbounded_String ("e1^e2"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e3)),
                                  To_Unbounded_String ("e1^e3"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e2_e3)),
                                  To_Unbounded_String ("e2^e3"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_no)),
                                  To_Unbounded_String ("e1^no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e2_no)),
                                  To_Unbounded_String ("e2^no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e3_no)),
                                  To_Unbounded_String ("e3^no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_ni)),
                                  To_Unbounded_String ("e1^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e2_ni)),
                                  To_Unbounded_String ("e2^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e3_ni)),
                                  To_Unbounded_String ("e3^ni"));

        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_e3)),
                                  To_Unbounded_String ("e1^e2^e3"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_no)),
                                  To_Unbounded_String ("e1^e2^no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_ni)),
                                  To_Unbounded_String ("e1^e2^ni"));

        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_e3_no)),
                                  To_Unbounded_String ("e1^e2^e3^no"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_e3_ni)),
                                  To_Unbounded_String ("e1^e2^e3^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_no_ni)),
                                  To_Unbounded_String ("no^e1^e2^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e3_no_ni)),
                                  To_Unbounded_String ("no^e1^e3^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e2_e3_no_ni)),
                                  To_Unbounded_String ("no^e2^e3^ni"));
        BV_Names.Replace_Element (Natural (C3_Base'Enum_Rep (C3_e1_e2_e3_ni)),
                                  To_Unbounded_String ("no^e1^e2^e3^ni"));
        return BV_Names;
    end Basis_Names_C3GA;

    --  -------------------------------------------------------------------------

end Blade_Types;
