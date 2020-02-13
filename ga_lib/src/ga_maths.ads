
with Interfaces;

with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;

with Ada.Numerics;

package GA_Maths is
   package Float_Array_Package is new Ada.Numerics.Generic_Real_Arrays (float);
   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Float);
   package Complex_Functions is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
   subtype Float_Matrix is Float_Array_Package.Real_Matrix;
   subtype Float_Vector is Float_Array_Package.Real_Vector;

   type float_3 is digits 3;
   type Bit_Map is new integer range 0 .. 2 ** 30;
   type Bit_Map_Array is array (integer range <>) of Bit_Map;

   type Coords_Continuous_Array is array (integer range <>) of float;
   subtype Scalar_Coords is Coords_Continuous_Array (1 .. 1);
   subtype Bivector_Coords is Coords_Continuous_Array (1 .. 1);
   subtype MV_Coordinate_Array is Coords_Continuous_Array (1 .. 32);

   subtype Safe_Float is Float range Float'Range;

   type Fixed_4 is delta 0.01 range -1.0 .. 1.0;
   for Fixed_4'Small use 0.01;

   US_1 : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (1);
   type Vector_Unsigned_3D is array (1 .. 3) of Interfaces.Unsigned_32;

   subtype Basis_Index is Integer range 1 .. 2;  --  e1, e2
   subtype Grade_Index is Integer range 0 .. 2;  --  Scalar, Vector, Bivector
   subtype Grade_Usage is Interfaces.Unsigned_32;

   type Basis_Array is array (Basis_Index) of integer;
   type Grade_Array is array (Grade_Index) of integer;
   type Integer_Array is array (Integer range <>) of Integer;
   type Array_I2 is array (1 .. 2) of integer;
   type Array_I3 is array (1 .. 3) of integer;
   type Array_I4 is array (1 .. 4) of integer;
   type Array_I8 is array (1 .. 8) of integer;
   type Array_UI2 is array (1 .. 2) of Interfaces.Unsigned_32;
   type Array_UI3 is array (1 .. 3) of Interfaces.Unsigned_32;
   type Array_UI4 is array (1 .. 4) of Interfaces.Unsigned_32;
   type Array_UI8 is array (1 .. 8) of Interfaces.Unsigned_32;
   type Array_F1 is array (1 .. 1) of float;
   type Array_F2 is array (1 .. 2) of float;
   type Array_F4 is array (1 .. 4) of float;
   type Array_F8 is array (1 .. 8) of float;

   type GA_Matrix3 is new Float_Matrix (1 .. 3, 1 .. 3);

   type Vector_Unsigned is record
      C1_e1   : Interfaces.Unsigned_64;
      C2_e2   : Interfaces.Unsigned_64;
      C3_e3   : Interfaces.Unsigned_64;
   end record;

   type Array_2D is array (1 .. 2) of float;
   type Array_3D is array (1 .. 3) of float;
   type Array_4D is array (1 .. 4) of float;
   type Coord4_Array is  Array (1 .. 4) of float;

   Infinity : constant Safe_Float := Safe_Float'Last;
   Pi       : constant float := Ada.Numerics.Pi;
   Two_Pi   : constant float := 2.0 * Ada.Numerics.Pi;

   GU_0     : constant Grade_Usage := 1;
   GU_1     : constant Grade_Usage := 2;
   GU_2     : constant Grade_Usage := 4;
   GU_4     : constant Grade_Usage := 8;
   GU_8     : constant Grade_Usage := 16;
   GU_16    : constant Grade_Usage := 32;

   function Is_Diagonal (aMatrix : Float_Matrix) return Boolean;
   function Maximum (I1, I2 : Integer) return Integer;
   function Maximum (I1, I2 : Float) return Float;
   function Minimum (I1, I2 : Integer) return Integer;
   function Minimum (I1, I2 : Float) return Float;

end GA_Maths;
