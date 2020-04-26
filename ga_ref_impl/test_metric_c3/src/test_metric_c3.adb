
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with GA_Utilities;
with Metric;
with Multivectors; use Multivectors;

--  --------------------------------------------------------------------

procedure Test_Metric_C3 is
    C3_Met_Matrix  : constant Metric.Metric_Matrix (1 .. 5, 1 .. 5) :=
                    ((0.0, 0.0, 0.0, 0.0, -1.0),
                     (0.0, 1.0, 0.0, 0.0, 0.0),
                     (0.0, 0.0, 1.0, 0.0, 0.0),
                     (0.0, 0.0, 0.0 ,1.0, 0.0),
                     (-1.0, 0.0, 0.0 , 0.0, 0.0));
   C3_Met          : constant Metric.Metric_Record := Metric.New_Metric (C3_Met_Matrix);
   NP_1            : Multivector;
   NP_1_GP         : Multivector;

   Blades_GP       : Blade.Blade_List;
   Blade_1         : Blade.Basis_Blade;
   Blade_2         : Blade.Basis_Blade;
begin
   GA_Utilities.Print_Matrix ("Test_Metric_C3, C3_Met_Matrix", C3_Met_Matrix);
   GA_Utilities.Print_Metric ("Test_Metric_C3 C3_Met", C3_Met);
   GA_Utilities.Print_Matrix ("Eigen_Vectors (C3_Met)", Metric.Eigen_Vectors (C3_Met));
   GA_Utilities.Print_Float_Array ("Eigen_Values (C3_Met)", Metric.Eigen_Values (C3_Met));

   NP_1 := New_Normalized_Point (1.0, 1.0, 1.0);
   GA_Utilities.Print_Multivector ("Test_Metric_C3 NP_1", NP_1);

--     NP_1_GP := Geometric_Product (NP_1, NP_1, C3_Met);
--     GA_Utilities.Print_Multivector ("Test_Metric_C3 NP_1_GP", NP_1_GP);

   Blade_1 := Blade.New_Basis_Blade (1,1.0);
   Blade_2 := Blade.New_Basis_Blade (2,1.0);
   Blades_GP := Blade.Geometric_Product (Blade_1, Blade_2, C3_Met);
   GA_Utilities.Print_Blade_List ("Test_Metric_C3 Blades_GP", Blades_GP);

exception
   when others =>
      Put_Line ("An exception occurred in Test_Metric_C3.");
      raise;

end Test_Metric_C3;
