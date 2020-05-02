
with Ada.Numerics.Generic_Real_Arrays;

with GA_Maths;

package SVD is

    type Real is digits 18;
    package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);

    type SVD (Num_Rows, Num_Cols, Num_Singular, Work_Vector_Rows : Natural) is private;

    SVD_Exception : Exception;

    function Condition_Number (aMatrix : GA_Maths.Float_Matrix) return Float;
    function Singular_Value_Decomposition (aMatrix : Real_Arrays.Real_Matrix)
                                           return SVD;
    function Singular_Value_Decomposition (aMatrix : GA_Maths.Float_Matrix)
                                           return SVD;
private

    type SVD (Num_Rows, Num_Cols, Num_Singular, Work_Vector_Rows : Natural) is record
        Matrix_U              : Real_Arrays.Real_Matrix
          (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 0.0));
        Matrix_V               : Real_Arrays.Real_Matrix
          (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 0.0));
        Matrix_W               : Real_Arrays.Real_Vector
          (1 .. Work_Vector_Rows) :=(others => 0.0);
        Sorted_Singular_Values : Real_Arrays.Real_Vector (1 .. Num_Singular);
    end record;

end SVD;
