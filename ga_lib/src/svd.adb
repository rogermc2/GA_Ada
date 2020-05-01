
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with Ada_Lapack;

package body SVD is

    package Complex_Maths   is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
    package Lapack is new Ada_Lapack (Real, Complex_Types,
                                      Real_Arrays, Complex_Arrays);

    --  ------------------------------------------------------------------------

    function Singular_Value_Decomposition (aMatrix : GA_Maths.Float_Matrix)
                                           return SVD is
        use Complex_Arrays;
        use Real_Arrays;
        Converted_Matrix : Complex_Arrays.Complex_Matrix (aMatrix'Range (1), aMatrix'Range (2)) :=
                             (others => (others => (0.0, 0.0)));
    begin
        for row in aMatrix'Range (1) loop
            for col in aMatrix'Range (2) loop
                Converted_Matrix (row, col) := (Real (aMatrix (row, col)), 0.0);
            end loop;
        end loop;
        return Singular_Value_Decomposition (Converted_Matrix);
    end Singular_Value_Decomposition;

    --  ------------------------------------------------------------------------

    function Singular_Value_Decomposition (aMatrix : Real_Arrays.Real_Matrix)
                                           return SVD is
        use Complex_Arrays;
        Converted_Matrix : Complex_Arrays.Complex_Matrix (aMatrix'Range (1), aMatrix'Range (2)) :=
                             (others => (others => (0.0, 0.0)));
    begin
        Set_Re (Converted_Matrix, aMatrix);
        return Singular_Value_Decomposition (Converted_Matrix);
    end Singular_Value_Decomposition;

    --  ------------------------------------------------------------------------

    function Singular_Value_Decomposition (aMatrix : Complex_Arrays.Complex_Matrix)
                                           return SVD is
        use Lapack;
        --          use Float_Array_Package;
        --          use Float_Functions;
        --          use Float_Sort_Package;
        use Real_Arrays;
        use Complex_Types;
        use Complex_Arrays;

        One          : constant Real := 1.0e0;
        Zero         : constant Real := 0.0e0;

        Num_Rows        : constant Natural := aMatrix'Length (1);
        Num_Cols        : constant Natural := aMatrix'Length (2);

        Matrix_A        : Complex_Matrix := aMatrix;
        Matrix_U        : Complex_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
        V_Transpose     : Complex_Matrix (1 .. Num_Cols, 1 .. Num_Cols);
        Vector_R        : Real_Vector (1 .. Num_Rows);
        Short_Vector    : Complex_Vector (1 .. 1);
        Num_Singular    : Integer := GA_Maths.Minimum (Num_Rows, Num_Cols);
        Singular_Values : Real_Vector (1 .. Num_Singular);  --  sorted: S(i) >= S(i+1).
        Return_Code     : Integer := 0;
        theSVD          : SVD (Num_Rows, Num_Cols, Num_Singular);
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
               RWORK  => Vector_R,
               INFO   => Return_code);
        declare
            Work_Vector_Rows : Integer := Integer (Re (Short_Vector (1)));
            Work_Vector      : Complex_Vector (1 .. Work_Vector_Rows);
        begin
            GESVD (JOBU   => 'S', JOBVT  => 'S',
                   M      => Num_Rows, N      => Num_Cols,
                   A      => Matrix_A, LDA    => Num_Rows,
                   S      => Singular_Values,
                   U      => Matrix_U, LDU    => Num_Rows,
                   VT     => V_Transpose, LDVT   => Num_Cols,
                   WORK   => Work_Vector, LWORK  => Work_Vector_Rows,
                   RWORK  => Vector_R,
                   INFO   => Return_Code);
            if Return_Code = 0 then
                theSVD.Matrix_W := Work_Vector;
            end if;
        end;

        if Return_Code > 0 then
            raise SVD_Exception with
              "Singular_Value_Decomposition failed with return code" & Integer'Image (Return_Code);
        else
            theSVD.Matrix_U := Matrix_U;
            theSVD.Matrix_V := Transpose (V_Transpose);
            theSVD.Sorted_Singular_Values := Singular_Values;
        end if;
        return theSVD;
    end Singular_Value_Decomposition;

    --  ------------------------------------------------------------------------

    function Condition_Number (aMatrix : GA_Maths.Float_Matrix) return Float is
        anSVD     : SVD := Singular_Value_Decomposition (aMatrix);
        Max       : Float :=
                      Float (anSVD.Sorted_Singular_Values (anSVD.Sorted_Singular_Values'First));
        Min       : Float :=
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
