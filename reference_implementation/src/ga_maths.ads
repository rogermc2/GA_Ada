
with Interfaces;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;

with Ada.Numerics;

package GA_Maths is
    package Float_Array_Package is new Ada.Numerics.Generic_Real_Arrays (float);
    package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
    subtype Float_Matrix is Float_Array_Package.Real_Matrix;

    type Fixed_4 is delta 0.01 range -1.0 .. 1.0;
    for Fixed_4'Small use 0.01;

    type Unsigned_Integer is mod 2 ** 32;
    type Vector_Unsigned_3D is array (1 .. 3) of Interfaces.Unsigned_32;

    subtype Basis_Index is Integer range 1 .. 2;
    subtype Grade_Index is Integer range 0 .. 2;
    subtype Grade_Usage is GA_Maths.Unsigned_Integer;

    type Basis_Array is array (Basis_Index) of integer;
    type Grade_Array is array (Grade_Index) of integer;
    type Array_I2 is array (1 .. 2) of integer;
    type Array_I3 is array (1 .. 3) of integer;
    type Array_I4 is array (1 .. 4) of integer;
    type Array_I8 is array (1 .. 8) of integer;
    type Array_UI2 is array (1 .. 2) of Unsigned_Integer;
    type Array_UI3 is array (1 .. 3) of Unsigned_Integer;
    type Array_UI4 is array (1 .. 4) of Unsigned_Integer;
    type Array_UI8 is array (1 .. 8) of Unsigned_Integer;
    type Array_F8 is array (1 .. 8) of float;

    type GA_Matrix3 is new Float_Matrix (1 .. 3, 1 .. 3);

--      type Scalar_Coords is (Scalar_Scalar);
--      type Scalar is record
--      --    Coord_Type : Scalar_Coords := Scalar_Scalar;
--          M_C1       : float;
--      end record;
    type Vector_Unsigned is record
        C1_e1   : Interfaces.Unsigned_64;
        C2_e2   : Interfaces.Unsigned_64;
        C3_e3   : Interfaces.Unsigned_64;
    end record;

    type Array_2D is array (1 .. 2) of float;
    type Array_3D is array (1 .. 3) of float;
    type Array_4D is array (1 .. 4) of float;
    type Coord4_Array is  Array (1 .. 4) of float;

    Pi      : constant float := Ada.Numerics.Pi;
    Two_Pi  : constant float := 2.0 * Ada.Numerics.Pi;

   function Canonical_Reordering_Sign (Map_A, Map_B : integer) return float;

end GA_Maths;
