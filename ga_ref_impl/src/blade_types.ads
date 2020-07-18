
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Blade_Types is

    package Names_Package is new
      Ada.Containers.Vectors (Natural, Unbounded_String);
   type Basis_Vector_Names is new Names_Package.Vector with null record;

   type BV_Base is (BV_e1e2, BV_e2e3, BV_e3e1);
   type E2_Base is (E2_1, E2_e1, E2_e2, E2_e1_e2);
   type E3_Base is (E3_1, E3_e1, E3_e2, E3_e3, E3_e1_e2, E3_e1_e3, E3_e2_e3,
                    E3_e1_e2_e3);
   type C3_Base is (C3_no, C3_e1, C3_e2, C3_e3, C3_ni,
                    C3_e1_e2, C3_e1_e3, C3_e2_e3,
                    C3_e1_no, C3_e2_no, C3_e3_no, C3_e1_ni, C3_e2_ni, C3_e3_ni,
                    C3_e1_e2_e3, C3_e1_e2_no, C3_e1_e2_ni,
                    C3_e1_e3_no, C3_e1_e3_ni, C3_e2_e3_no, C3_e2_e3_ni,
                    C3_e1_no_ni, C3_e2_no_ni, C3_e3_no_ni, C3_e1_e2_e3_ni,
                    C3_e1_e2_e3_no, C3_e1_e2_no_ni,C3_e1_e3_no_ni,
                    C3_e2_e3_no_ni, C3_e1_e2_e3_no_ni);

    function Basis_Names_C3GA return Basis_Vector_Names;

private
   for BV_Base use (BV_e1e2 => 1, BV_e2e3 => 2, BV_e3e1 => 4);
   for E2_Base use (E2_1 => 0, E2_e1 => 1, E2_e2 => 2, E2_e1_e2 => 4);

   for E3_Base use (E3_1 => 0, E3_e1 => 1, E3_e2 => 2, E3_e3 => 4,
                    E3_e1_e2    => 8, E3_e1_e3 => 16, E3_e2_e3 => 32,
                    E3_e1_e2_e3 => 64);

   for C3_Base use
     (C3_no => 1, C3_e1 => 2, C3_e2 => 4, C3_e3 => 8, C3_ni => 16,
      C3_e1_e2 => 2 ** 5, C3_e1_e3 => 2 ** 6, C3_e2_e3 => 2 ** 7,
      C3_e1_no => 2 ** 8, C3_e2_no => 2 ** 9, C3_e3_no => 2 ** 10,
      C3_e1_ni => 2 ** 11, C3_e2_ni => 2 ** 12, C3_e3_ni => 2 ** 13,
      C3_e1_e2_e3 => 2 ** 14, C3_e1_e2_no => 2 ** 15, C3_e1_e2_ni => 2 ** 16,
      C3_e1_e3_no => 2 ** 17, C3_e1_e3_ni => 2 ** 18,
      C3_e2_e3_no => 2 ** 19, C3_e2_e3_ni => 2 ** 20,
      C3_e1_no_ni => 2 ** 21, C3_e2_no_ni => 2 ** 22, C3_e3_no_ni => 2 ** 23,
      C3_e1_e2_e3_ni => 2 ** 24, C3_e1_e2_e3_no => 2 ** 25,
      C3_e1_e2_no_ni => 2 ** 26, C3_e1_e3_no_ni => 2 ** 27,
      C3_e2_e3_no_ni => 2 ** 28, C3_e1_e2_e3_no_ni => 2 ** 29);

end Blade_Types;
