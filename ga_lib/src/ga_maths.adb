
package body GA_Maths is

    function Is_Diagonal (aMatrix : Float_Matrix) return Boolean is
        epsilon : constant float := 10.0 ** (-9);
        row     : Integer := aMatrix'First;
        col     : Integer := aMatrix'First (2);
        OK      : Boolean := True;
    begin
        while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
                if row /= col then
                    OK := Abs (aMatrix (row, col)) <= epsilon;
                end if;
                col := col + 1;
            end loop;
            row := row + 1;
        end loop;
        return OK;
    end Is_Diagonal;

    --  ------------------------------------------------------------------------

    function Maximum (I1, I2 : Integer) return Integer is
        Max : Integer;
    begin
        if I1 > I2 then
            Max := I1;
        else
            Max := I2;
        end if;
        return Max;
    end Maximum;

    --  ------------------------------------------------------------------------

    function Maximum (I1, I2 : Float) return Float is
        Max : Float;
    begin
        if I1 > I2 then
            Max := I1;
        else
            Max := I2;
        end if;
        return Max;
    end Maximum;

    --  ------------------------------------------------------------------------

    function Minimum (I1, I2 : Integer) return Integer is
        Min : Integer;
    begin
        if I1 < I2 then
            Min := I1;
        else
            Min := I2;
        end if;
        return Min;
    end Minimum;

    --  ------------------------------------------------------------------------

    function Minimum (I1, I2 : Float) return Float is
        Min : Float;
    begin
        if I1 < I2 then
            Min := I1;
        else
            Min := I2;
        end if;
        return Min;
    end Minimum;

    --  ------------------------------------------------------------------------

end GA_Maths;
