
with Ada.Text_IO; use Ada.Text_IO;

with GA_Utilities;
with Metric;

--  --------------------------------------------------------------------
--  Based on Metric.java main
procedure Test_Metric is
   M_Matrix : constant Metric.Metric_Matrix (1 .. 3, 1 .. 3) :=
                ((1.0, 0.0, 0.0),
                 (0.0, 0.0, -1.0),
                 (0.0, -1.0, 0.0));
   E3_Matrix : constant Metric.Metric_Matrix (1 .. 3, 1 .. 3) :=
                ((1.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0),
                 (0.0, 0.0, 1.0));
   Met       : constant Metric.Metric_Record := Metric.New_Metric (M_Matrix);
   E3_Met    : constant Metric.Metric_Record := Metric.New_Metric (E3_Matrix);
begin
   GA_Utilities.Print_Metric ("Test_Metric", Met);
   GA_Utilities.Print_Metric ("Test_Metric E3", E3_Met);
   GA_Utilities.Print_Float_Array ("Eigen_Values (Met)", Metric.Eigen_Values (Met));
   GA_Utilities.Print_Matrix ("Eigen_Vectors (Met)", Metric.Eigen_Vectors (Met));
   GA_Utilities.Print_Float_Array ("Eigen_Values (E3_Met)", Metric.Eigen_Values (E3_Met));
   GA_Utilities.Print_Matrix ("Eigen_Vectors (E3_Met)", Metric.Eigen_Vectors (E3_Met));
exception
   when others =>
      Put_Line ("An exception occurred in Test_Metric.");
      raise;

end Test_Metric;
