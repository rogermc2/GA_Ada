
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade_Types;
with C3GA;
with GA_Maths;
with GA_Utilities;
with Metric;
with Multivectors; use Multivectors;
with Multivector_Type;
with C3GA;

procedure Test_Matrix is
   use GA_Maths.Float_Array_Package;
--     no_bv   : Multivector := Basis_Vector (Blade_Types.C3_no);
--     e1_bv   : Multivector := Basis_Vector (Blade_Types.C3_e1);
--     e2_bv   : Multivector := Basis_Vector (Blade_Types.C3_e2);
--     e3_bv   : Multivector := Basis_Vector (Blade_Types.C3_e3);
--     ni_bv   : Multivector := Basis_Vector (Blade_Types.C3_ni);

   MV        : Multivectors.Normalized_Point :=
                 C3GA.Set_Normalized_Point (1.0, -0.5, 0.3);
   Dim       : constant Natural:= Space_Dimension (MV);
   Met       : Metric.Metric_Record (5) := Metric.C3_Metric;
   Matrix_AG : GA_Maths.Float_Matrix (0 .. 31, 0 .. 31);
   Mat_Inv   : GA_Maths.Float_Matrix (0 .. 31, 0 .. 31);
begin
--     Matrix_AG := Multivectors.To_Geometric_Matrix (MV, Met);
   Matrix_AG := Multivectors.Init_Geometric_Matrix (MV, Met);
   New_Line;
   GA_Utilities.Print_Matrix ("Test_Matrix Matrix_AG",
                              Matrix_AG, (0, 0), (6, 6));
   Mat_Inv := Inverse (Matrix_AG);
   GA_Utilities.Print_Matrix ("Test_Matrix Mat_Inv",
                              Mat_Inv, (0, 0), (6, 6));
--     Matrix_AG := Multivectors.Init_Geometric_Matrix (MV, Met);
--     New_Line;
--     GA_Utilities.Print_Matrix ("Test_Matrix Matrix_AG", Matrix_AG);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_Matrix.");
      raise;

end Test_Matrix;