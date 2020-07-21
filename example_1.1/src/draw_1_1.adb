
with C3GA;
with C3GA_Draw;
--  with GA_Utilities;
with Multivector_Utilities;

package body Draw_1_1 is

    --  ---------------------------------------------------------------------

    function Draw_Circle (Render_Program : GL.Objects.Programs.Program;
                          C1, C2, C3     : Normalized_Point) return Circle is
        OP      : Multivector;
        aCircle : Circle;
    begin
        OP := Outer_Product (C1, Outer_Product (C2, C3));
        aCircle := To_Circle (OP);
        C3GA_Draw.Draw (Render_Program, aCircle);
        return aCircle;
    end Draw_Circle;

    --  ---------------------------------------------------------------------

    function Draw_Line (Render_Program : GL.Objects.Programs.Program;
                        P1, P2          : Normalized_Point) return Line is
        aLine : constant Line := C3GA.Set_Line (P1, P2);
    begin
        C3GA_Draw.Draw (Render_Program, aLine);
        return aLine;
    end Draw_Line;

    --  ---------------------------------------------------------------------

    procedure Draw_Reflected_Circle (Render_Program : GL.Objects.Programs.Program;
                                     C: Circle; DP : Dual_Plane)  is
    begin
        C3GA_Draw.Draw (Render_Program, Multivector_Utilities.Reflect (C, DP));
    end Draw_Reflected_Circle;

    --  ---------------------------------------------------------------------

    procedure Draw_Reflected_Line (Render_Program : GL.Objects.Programs.Program;
                                  L: Line; DP : Dual_Plane)  is
    begin
        C3GA_Draw.Draw (Render_Program, Multivector_Utilities.Reflect (L, DP));
    end Draw_Reflected_Line;

    --  ---------------------------------------------------------------------

    function New_Dual_Plane (P1 : Normalized_Point; Normal : E3GA.E3_Vector)
                             return Dual_Plane is
        OP : Multivector := Outer_Product (E3GA.To_MV_Vector (Normal), C3GA.ni);
    begin
        OP := Left_Contraction (P1, OP);
        return To_Dual_Plane (OP);
    end New_Dual_Plane;

    --  ---------------------------------------------------------------------

end Draw_1_1;
