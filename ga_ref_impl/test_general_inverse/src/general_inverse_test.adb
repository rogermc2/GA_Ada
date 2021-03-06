
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
   BV_Names      : Blade.Basis_Vector_Names;
   C3_Met_Matrix : constant Metric.Metric_Matrix (1 .. 5, 1 .. 5) :=
                    ((0.0, 0.0, 0.0, 0.0, -1.0),
                     (0.0, 1.0, 0.0, 0.0, 0.0),
                     (0.0, 0.0, 1.0, 0.0, 0.0),
                     (0.0, 0.0, 0.0 ,1.0, 0.0),
                     (-1.0, 0.0, 0.0 , 0.0, 0.0));
   C3_Metric      : constant Metric.Metric_Record := Metric.New_Metric (C3_Met_Matrix);

   no_bv        : Multivector := Basis_Vector (Blade_Types.C3_no);
   e1_bv        : Multivector := Basis_Vector (Blade_Types.C3_e1);
   e2_bv        : Multivector := Basis_Vector (Blade_Types.C3_e2);
   e3_bv        : Multivector := Basis_Vector (Blade_Types.C3_e3);
   ni_bv        : Multivector := Basis_Vector (Blade_Types.C3_ni);

   MV_A1        : Multivector;
   MV_A2        : Multivector;
   MV_A         : Multivector;
   MV_A_Inv    : Multivector;

   NP_1         : constant Multivector := New_Normalized_Point (-0.356756, -0.881980, 0.0);
   NP_1_OP      : Multivector;
   NP_1_IP      : Multivector;
   NP_1_GP_Met  : Multivector;
   NP_1_IP_Met  : Multivector;
   NP_1_Inv     : Multivector;
   NP_1_GP      : Multivector;

begin
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

   MV_A1 := Outer_Product (e2_bv, e3_bv);
   MV_A2 := e1_bv + Outer_Product (e2_bv, e3_bv);
   MV_A := e1_bv + Outer_Product (Outer_Product (e2_bv, e3_bv), e1_bv);
   GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A1", MV_A1);
   GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A2", MV_A2);
   GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A", MV_A);

   MV_A_Inv := General_Inverse (MV_A);
   GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A_Inv", MV_A_Inv);

   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1", NP_1);
   NP_1_OP := Outer_Product (NP_1, NP_1);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_OP", NP_1_OP);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A", MV_A);
--     MV_A_GP := Geometric_Product (MV_A, MV_A);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A_GP", MV_A_GP);
--     MV_A_GP_Met := Geometric_Product (MV_A, MV_A, C3_Metric);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test MV_A_GP_Met", MV_A_GP_Met);
--     MV_A_Inv := General_Inverse (MV_A, C3_Metric);

   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1", NP_1);
   NP_1_OP := Outer_Product (NP_1, NP_1);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_OP", NP_1_OP);
   NP_1_GP := Geometric_Product (NP_1, NP_1);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_GP", NP_1_GP);

   NP_1_GP_Met := Geometric_Product (NP_1, NP_1, C3_Metric);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_GP_Met", NP_1_GP_Met);

   NP_1_IP := Inner_Product (NP_1, NP_1,Blade.Left_Contraction);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_IP", NP_1_IP);
   NP_1_IP_Met := Geometric_Product (NP_1, NP_1, C3_Metric);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_IP_Met", NP_1_IP_Met);

   NP_1_Inv := General_Inverse (NP_1, C3_Metric);
   GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_Inv", NP_1_Inv);

exception
   when anError :  others =>
      Put_Line ("An exception occurred in General_Inverse_Test.");
      raise;
end General_Inverse_Test;
