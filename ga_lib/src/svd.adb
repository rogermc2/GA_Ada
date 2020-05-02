
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Generic_Complex_Types;

with Ada_Lapack;

package body SVD is
    package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
    package Complex_Arrays is new
      Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
    package Lapack is new Ada_Lapack (Real, Complex_Types, Real_Arrays, Complex_Arrays);

    --  ------------------------------------------------------------------------

    function Singular_Value_Decomposition (aMatrix : GA_Maths.Float_Matrix)
                                           return SVD is
        Converted_Matrix : Real_Arrays.Real_Matrix (aMatrix'Range (1), aMatrix'Range (2)) :=
                             (others => (others => 0.0));
    begin
        for row in aMatrix'Range (1) loop
            for col in aMatrix'Range (2) loop
                Converted_Matrix (row, col) := (Real (aMatrix (row, col)));
            end loop;
        end loop;
        return Singular_Value_Decomposition (Converted_Matrix);
    end Singular_Value_Decomposition;

    --  ------------------------------------------------------------------------

    function Singular_Value_Decomposition (aMatrix : Real_Arrays.Real_Matrix)
                                           return SVD is
        use Lapack;
        use Real_Arrays;

        Num_Rows        : constant Integer := aMatrix'Length (1);
        Num_Cols        : constant Integer := aMatrix'Length (2);
        Matrix_A        : Real_Matrix := aMatrix;
        Matrix_U        : Real_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
        V_Transpose     : Real_Matrix (1 .. Num_Cols, 1 .. Num_Cols);
        Short_Vector    : Real_Vector (1 .. 1);
        Num_Singular    : constant Integer := GA_Maths.Minimum (Num_Rows, Num_Cols);
        Singular_Values : Real_Vector (1 .. Num_Singular);  --  sorted: S(i) >= S(i+1).
        Return_Code     : Integer := 0;
        --  Return_Code  = 0:  successful exit.
        --            < 0:  if INFO = -i, the i-th argument had an illegal value.
        --            > 0:  if DBDSQR did not converge, INFO specifies how many
        --                  superdiagonals of an intermediate bidiagonal form B
        --                  did not converge to zero.
        --  Refer http://www.netlib.org/lapack/explore-html/index.html for
        --  additional GESVD information
    begin
        GESVD (JOBU   => 'S', JOBVT  => 'S',
               M      => Num_Rows, N      => Num_Cols,
               A      => Matrix_A, LDA    => Num_Rows,
               S      => Singular_Values,
               U      => Matrix_U, LDU    => Num_Rows,
               VT     => V_Transpose, LDVT   => Num_Cols,
               WORK   => Short_Vector, LWORK  => -1,
               INFO   => Return_code);
        declare
            Work_Vector_Rows : constant  Integer := Integer (Short_Vector (Short_Vector'First));
            Work_Vector      : Real_Vector (1 .. Work_Vector_Rows);
            theSVD           : SVD (Num_Rows, Num_Cols, Num_Singular, Work_Vector_Rows);
        begin
            GESVD (JOBU   => 'S', JOBVT  => 'S',
                   M      => Num_Rows, N      => Num_Cols,
                   A      => Matrix_A, LDA    => Num_Rows,
                   S      => Singular_Values,
                   U      => Matrix_U, LDU    => Num_Rows,
                   VT     => V_Transpose, LDVT   => Num_Cols,
                   WORK   => Work_Vector, LWORK  => Work_Vector_Rows,
                   INFO   => Return_Code);
            if Return_Code = 0 then
                theSVD.Matrix_U := Matrix_U;
                theSVD.Matrix_V := Transpose (V_Transpose);
                theSVD.Sorted_Singular_Values := Singular_Values;
                theSVD.Matrix_W := Work_Vector;
                return theSVD;
            else
                raise SVD_Exception with
                  "Singular_Value_Decomposition failed with return code" & Integer'Image (Return_Code);
            end if;
        end;
    end Singular_Value_Decomposition;

    --  ------------------------------------------------------------------------

    function Condition_Number (aMatrix : GA_Maths.Float_Matrix) return Float is
        anSVD     : constant SVD := Singular_Value_Decomposition (aMatrix);
        Max       : constant Float :=
                      Float (anSVD.Sorted_Singular_Values (anSVD.Sorted_Singular_Values'First));
        Min       : constant Float :=
                      Float (anSVD.Sorted_Singular_Values (anSVD.Sorted_Singular_Values'Last));
        Result    : Float := 0.0;
    begin
        if Min > 0.0 then
            Result := Max / Min;
        else
            Result := Max / 10.0 ** (-8);
        end if;
        return Result;
    end Condition_Number;

    --  ------------------------------------------------------------------------

end SVD;
