
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Real_Arrays;

package SVD is

    type Real is digits 18;
    package Real_Arrays     is new Ada.Numerics.Generic_Real_Arrays (Real);
    package Complex_Types   is new Ada.Numerics.Generic_Complex_Types (Real);
    package Complex_Arrays  is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
    type SVD (Num_Rows, Num_Cols : Natural) is private;

    SVD_Exception : Exception;

    function Condition_Number (aMatrix : Real_Arrays.Real_Matrix) return Float;
    function Singular_Value_Decomposition (aMatrix : Complex_Arrays.Complex_Matrix)
                                           return SVD;
private

    type SVD (Num_Rows, Num_Cols : Natural) is record
        Matrix_U : Real_Arrays.Real_Matrix (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 0.0));
        Matrix_V : Real_Arrays.Real_Matrix (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 0.0));
        Matrix_W : Real_Arrays.Real_Vector (1 .. Num_Rows) := (others => 0.0);
    end record;

end SVD;
