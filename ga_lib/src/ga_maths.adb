
package body GA_Maths is

   function Is_Anti_Euclidean (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := aMatrix (row, col) + 1.0 <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) ** 2 -
                               aMatrix (col, row) ** 2) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Anti_Euclidean;

   --  ------------------------------------------------------------------------

   function Is_Diagonal (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row /= col then
                  OK := Abs (aMatrix (row, col)) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Diagonal;

   --  ------------------------------------------------------------------------

   function Is_Symetric (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := aMatrix (row, col) - 1.0 <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) - aMatrix (col, row)) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Symetric;

   --  ------------------------------------------------------------------------

   function Is_Euclidean (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := Abs (aMatrix (row, col)) <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) ** 2 -
                               aMatrix (col, row) ** 2) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Euclidean;

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

   function Maximum (F1, F2 : Float) return Float is
      Max : Float;
   begin
      if F1 > F2 then
         Max := F1;
      else
         Max := F2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Maximum (F1, F2 : Long_Float) return Long_Float is
      Max : Long_Float;
   begin
      if F1 > F2 then
         Max := F1;
      else
         Max := F2;
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

   function Minimum (F1, F2 : Float) return Float is
      Min : Float;
   begin
      if F1 < F2 then
         Min := F1;
      else
         Min := F2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

   function Minimum (F1, F2 : Long_Float) return Long_Float is
      Min : Long_Float;
   begin
      if F1 < F2 then
         Min := F1;
      else
         Min := F2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

    function Singular_Value_List (aMatrix : Float_Matrix)
                                  return Float_List is
        use Float_Array_Package;
        use Float_Functions;
        use Float_Sort_Package;
        Values          : constant Real_Vector (aMatrix'Range) :=
                            Eigenvalues (aMatrix * Transpose (aMatrix));
        Abs_Value       : Float;
        Singular_Values : Float_List;
    begin
        for index in aMatrix'Range loop
            Abs_Value := Abs (Values (index));
            if Abs_Value > 10.0 ** (-8) then
               Append (Singular_Values, Abs_Value);
            end if;
        end loop;
        Sort (Float_List_Package.List (Singular_Values));
        return Singular_Values;
    end Singular_Value_List;

   --  ------------------------------------------------------------------------

    function Condition_Number (aMatrix : Float_Matrix) return Float is
        use Float_Array_Package;
        use Float_Functions;
        use Float_List_Package;
        Singular_Values : constant Float_List := Singular_Value_List (aMatrix);
        Max             : Float;
        Min             : Float;
        Result          : Float := 0.0;
    begin
        if List (Singular_Values) /= Empty_List then
            Max := Singular_Values.First_Element;
            Min := Singular_Values.Last_Element;
            if Min > 0.0 then
                Result := Max / Min;
            else
                Result := Max / 10.0 ** (-8);
            end if;
        end if;
        return Result;
    end Condition_Number;

   --  ------------------------------------------------------------------------

end GA_Maths;
