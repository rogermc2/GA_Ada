
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with GA_Maths;
with GA_Utilities;
with Metric;

with Multivectors; use Multivectors;
with Multivector_Type;

procedure General_Inverse_Test is
   use Blade.Names_Package;
   BV_Names     : Blade.Basis_Vector_Names;

   Met_Matrix   : constant Metric.Metric_Matrix (1 .. 5, 1 .. 5) :=
                    ((0.0, 0.0, 0.0, 0.0, -1.0),
                     (0.0, 1.0, 0.0, 0.0, 0.0),
                     (0.0, 0.0, 1.0, 0.0, 0.0),
                     (0.0, 0.0, 0.0 ,1.0, 0.0),
                     (-1.0, 0.0, 0.0 , 0.0, 0.0));
   Met          : constant Metric.Metric_Record := Metric.New_Metric (Met_Matrix);

   no_bv        : Multivector := Basis_Vector (Blade_Types.C3_no);
   e1_bv        : Multivector := Basis_Vector (Blade_Types.C3_e1);
   e2_bv        : Multivector := Basis_Vector (Blade_Types.C3_e2);
   e3_bv        : Multivector := Basis_Vector (Blade_Types.C3_e3);
   ni_bv        : Multivector := Basis_Vector (Blade_Types.C3_ni);

   MV_A         : Multivector;
   MV_A_Inv      : Multivector;
   MV_Info      : Multivector_Type.MV_Type_Record;

begin
   GA_Utilities.Print_Metric ("General_Inverse_Test Metric", Met);
   GA_Utilities.Print_Float_Array ("General_Inverse_Test Metric Eigen_Values",
                                   Metric.Eigen_Values (Met));
   New_Line;
   GA_Utilities.Print_Matrix ("General_Inverse_Test Metric Eigen_Vectors",
                                   Metric.Eigen_Vectors (Met));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

   MV_A := New_Normalized_Point (1.0, 2.5, 3.0);
   GA_Utilities.Print_Multivector ("MV_A", MV_A);

   MV_Info := Multivector_Type.Init (MV_A);

   MV_A_Inv := General_Inverse (MV_A, Met);
   GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A_Inv", MV_A_Inv);
   MV_Info := Multivector_Type.Init (MV_A_Inv);
   GA_Utilities.Print_Multivector_Info ("General_Inverse_Test MV_A_Inv Info", MV_Info);

exception
   when anError :  others =>
      Put_Line ("An exception occurred in General_Inverse_Test.");
      raise;
end General_Inverse_Test;
