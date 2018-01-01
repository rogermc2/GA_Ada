
with Interfaces;

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;

with Ada.Numerics;

package GA_Maths is
   package Float_Array_Package is new Ada.Numerics.Generic_Real_Arrays (float);
   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   subtype Float_Matrix is Float_Array_Package.Real_Matrix;

   type Coords_Continuous_Array is array (integer range <>) of float;
   subtype Scalar_Coords is Coords_Continuous_Array (1 .. 1);
   subtype Bivector_Coords is Coords_Continuous_Array (1 .. 1);
   subtype MV_Coordinate_Array is Coords_Continuous_Array (1 .. 32);

   subtype Safe_Float is Float range Float'Range;

   type Fixed_4 is delta 0.01 range -1.0 .. 1.0;
   for Fixed_4'Small use 0.01;

   type Basis_Blade is private;

   type Unsigned_Integer is mod 2 ** 32;
   US_1 : constant Unsigned_Integer := Unsigned_Integer (1);
   type Vector_Unsigned_3D is array (1 .. 3) of Interfaces.Unsigned_32;

   subtype Basis_Index is Integer range 1 .. 2;  --  e1, e2
   subtype Grade_Index is Integer range 0 .. 2;  --  Scalar, Vector, Bivector
   subtype Grade_Usage is Unsigned_Integer;

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

   function Bitmap (BB : Basis_Blade) return Unsigned_Integer;
   function Blade_Scale (BB : Basis_Blade) return Float;
   function Canonical_Reordering_Sign (Map_A, Map_B : Unsigned_Integer) return float;
   function Outer_Product (BA, BB : Basis_Blade) return Basis_Blade;
   function New_Basis_Blade (Index : Integer; Scale : Float := 1.0) return Basis_Blade;

   procedure Update_Blade (BB : in out Basis_Blade; Scale : Float);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer);
   procedure Update_Blade (BB : in out Basis_Blade; Bitmap : Unsigned_Integer;
                           Scale : Float);

private
   type Basis_Blade is record
      Bitmap : Unsigned_Integer := 0;
      Scale  : Float := 1.0;
   end record;

end GA_Maths;
