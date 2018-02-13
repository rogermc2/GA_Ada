
package Blade_Types is

   type BV_Base is (BV_e1e2, BV_e2e3, BV_e3e1);
   for BV_Base use (BV_e1e2 => 1, BV_e2e3 => 2, BV_e3e1 => 4);

   type E2_Base is (E2_e1, E2_e2, E2_e1_e2);
   for E2_Base use (E2_e1 => 1, E2_e2 => 2, E2_e1_e2 => 4);

   type E3_Base is (E3_e1, E3_e2, E3_e3, E3_e1_e2, E3_e1_e3, E3_e2_e3, E3_e1_e2_e3);
   for E3_Base use (E3_e1 => 1, E3_e2 => 2, E3_e3 => 4,
                    E3_e1_e2 => 8, E3_e1_e3 => 16, E3_e2_e3 => 32, E3_e1_e2_e3 => 64);

   type C3_Base is (C3_no, C3_e1, C3_e2, C3_e3, C3_ni,
                    C3_e1_e2, C3_e1_e3, C3_e2_e3,
                    C3_e1_no, C3_e2_no, C3_e3_no, C3_e1_ni, C3_e2_ni, C3_e3_ni,
                    C3_e1_e2_e3, C3_e1_e2_no, C3_e1_e2_ni,
                    C3_e2_e3_no, C3_e2_e3_ni);
   for C3_Base use
     (C3_no => 1, C3_e1 => 2, C3_e2 => 4, C3_e3 => 8, C3_ni => 16,
      C3_e1_e2 => 2 ** 5, C3_e1_e3 => 2 ** 6, C3_e2_e3 => 2 ** 7,
      C3_e1_no => 2 ** 8, C3_e2_no => 2 ** 9, C3_e3_no => 2 ** 10,
      C3_e1_ni => 2 ** 11, C3_e2_ni => 2 ** 12, C3_e3_ni => 2 ** 13,
      C3_e1_e2_e3 => 2 ** 14, C3_e1_e2_no => 2 ** 15, C3_e1_e2_ni => 2 ** 16,
      C3_e2_e3_no => 2 ** 17, C3_e2_e3_ni => 2 ** 18);

end Blade_Types;
