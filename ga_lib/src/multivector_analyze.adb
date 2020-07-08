
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

--  with Multivector_Analyze_E2GA;
with Multivector_Analyze_C3GA;

with Utilities;

package body Multivector_Analyze is

    --  --------------------------------------------------------------------------

    --     procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
    --                        Flags : Flag_Type := (Flag_Invalid, False);
    --                        Epsilon : float := Default_Epsilon) is
    --     begin
    --        Multivector_Analyze_E2GA.Analyze (theAnalysis, MV, Flags, Epsilon);
    --     end Analyze;

    --  --------------------------------------------------------------------------

    procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                       Probe : Multivectors.Normalized_Point := C3GA.Probe (Blade_Types.C3_no);
                       Flags : Flag_Type := (Flag_Invalid, False);
                       Epsilon : float := Default_Epsilon) is
    begin
        Multivector_Analyze_C3GA.Analyze (theAnalysis, MV, Probe, Flags, Epsilon);
    end Analyze;

    --  -------------------------------------------------------------------------

    function Blade_Subclass (A : MV_Analysis) return Blade_Subclass_Type is
    begin
        return  A.M_Type.Blade_Subclass;
    end Blade_Subclass;

    --  --------------------------------------------------------------------------

    function isValid (A : MV_Analysis) return Boolean is
    begin
        return A.M_Flags.Valid = Flag_Valid;
    end isValid;

    --  --------------------------------------------------------------------------

    function isDual (A : MV_Analysis) return Boolean is
    begin
        return A.M_Flags.Dual;
    end isDual;

    --  --------------------------------------------------------------------------

    function isBlade (A : MV_Analysis) return Boolean is
    begin
        --        return A.M_Type.Multivector_Kind
        return A.M_Type.Blade_Class /= Non_Blade;
    end isBlade;

    --  --------------------------------------------------------------------------

    function isVersor (A : MV_Analysis) return Boolean is
    begin
        return A.Versor_Kind /= Not_A_Versor;
    end isVersor;

    --  --------------------------------------------------------------------------

    function isNull (A : MV_Analysis) return Boolean is
    --  {return ((type() == BLADE) && (bladeClass() == ZERO));}
    begin
        return isBlade (A) and A.M_Type.Blade_Class = Zero_Blade;
    end isNull;

    --  --------------------------------------------------------------------------

    function isZero (A : MV_Analysis)  return Boolean is
    --  {return ((type() == BLADE) && (bladeClass() == ZERO));}
    begin
        return isBlade (A) and A.M_Type.Blade_Class = Zero_Blade;
    end isZero;

    --  --------------------------------------------------------------------------

    function Num_Points return integer is
    begin
        return Max_Points;
    end Num_Points;

    --  --------------------------------------------------------------------------

    function Num_Vectors return integer is
    begin
        return Max_Vectors;
    end Num_Vectors;

    --  --------------------------------------------------------------------------

    function Num_Scalars return integer is
    begin
        return Max_Scalars;
    end Num_Scalars;

    --  --------------------------------------------------------------------------

    procedure Print_E3_Vector_Array (Name : String; anArray : E3_Vector_Array) is
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Utilities.Print_Vector ("", anArray (Index));
        end loop;
        New_Line;
    end Print_E3_Vector_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Analysis (Name : String; Analysis : MV_Analysis) is
        use GL.Types;
        use Multivector_Type;
    begin
        Put_Line (Name & " Analysis");
        Put_Line ("Valid Flag    " & boolean'Image (Analysis.M_Flags.Valid));
        Put_Line ("Dual Flag     " & boolean'Image (Analysis.M_Flags.Dual));
        Print_Multivector_Info (Name & " M_MV_Type data", Analysis.M_MV_Type);
        Put_Line ("Model Type    " &
                    Model_Type'Image (Analysis.M_Type.Model_Kind));
        Put_Line ("Multivector_Kind " &
                    Multivector_Type_Base.Object_Type'Image (Analysis.M_Type.Multivector_Kind));
        Put_Line ("Epsilon      " & Float'Image (Analysis.Epsilon));
        Put_Line ("Pseudo_Scalar " & boolean'Image (Analysis.Pseudo_Scalar));
        Put_Line ("Versor_Kind   " & Versor_Subclass_Type'Image (Analysis.Versor_Kind));
        Put_Line ("Blade_Subclass " & Blade_Subclass_Type'Image (Analysis.M_Type.Blade_Subclass));
        Put_Line ("Points  array:");
        for index in Analysis.Points'Range loop
            Put_Line (Single'Image (Analysis.Points (index) (GL.X)) & " " &
                        Single'Image (Analysis.Points (index) (GL.Y)) & " " &
                        Single'Image (Analysis.Points (index) (GL.Z)));
        end loop;

    exception
        when  others =>
            Put_Line ("An exception occurred in Multivector_Analyze.Print_Analysis.");
            raise;
    end Print_Analysis;

    --  ------------------------------------------------------------------------

    function Versor_Subclass (A : MV_Analysis) return Blade_Subclass_Type is
    begin
        return Blade_Subclass (A);
    end Versor_Subclass;

    --  --------------------------------------------------------------------------

end Multivector_Analyze;
