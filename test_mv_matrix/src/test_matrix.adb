
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade_Types;
with GA_Maths;
with GA_Utilities;
with Metric;
with Multivectors; use Multivectors;
with Multivector_Type;

procedure Test_Matrix is
   no_bv   : Multivector := Basis_Vector (Blade_Types.C3_no);
   e1_bv   : Multivector := Basis_Vector (Blade_Types.C3_e1);
   e2_bv   : Multivector := Basis_Vector (Blade_Types.C3_e2);
   e3_bv   : Multivector := Basis_Vector (Blade_Types.C3_e3);
   ni_bv   : Multivector := Basis_Vector (Blade_Types.C3_ni);

   MV        : Multivector := e1_bv + e2_bv;
   Dim       : constant Natural:= Space_Dimension (MV);
   Met       : Metric.Metric_Record (5) := Metric.C3_Metric;
   Max_Index : constant Natural := Dim - 1;
   Matrix_AG : GA_Maths.Float_Matrix (0 .. Max_Index, 0 .. Max_Index);
begin
   Matrix_AG := Multivectors.Init_Geometric_Matrix (MV, Met);
   GA_Utilities.Print_Matrix ("", Matrix_AG);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_Matrix.");
      raise;

end Test_Matrix;
