
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with GA_Maths;
with GA_Utilities;
with Metric; use Metric;

with Multivectors; use Multivectors;
with Multivector_Type;

procedure LC_Test is
   use Blade_Types;
   BV_Names      : Basis_Vector_Names;

   no_bv        : Multivector := Basis_Vector (Blade_Types.C3_no);
   e1_bv        : Multivector := Basis_Vector (Blade_Types.C3_e1);
   e2_bv        : Multivector := Basis_Vector (Blade_Types.C3_e2);
   e3_bv        : Multivector := Basis_Vector (Blade_Types.C3_e3);
   ni_bv        : Multivector := Basis_Vector (Blade_Types.C3_ni);

   Met          : Metric_Record := C3_Metric;

   MV_A1        : Multivector;
   MV_A2        : Multivector;
   MV_A         : Multivector;
   MV_A_Inv    : Multivector;

   NP_1         : constant Multivector :=
                    New_Normalized_Point (-0.356756, -0.881980, 0.0);
   NP_1_OP      : Multivector;
   NP_1_IP      : Multivector;
   NP_1_GP_Met  : Multivector;
   NP_1_IP_Met  : Multivector;
   NP_1_Inv     : Multivector;
   NP_1_GP      : Multivector;

begin
   Multivectors.Set_Geometry (C3_Geometry);
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

   MV_A1 := Left_Contraction (e2_bv, e3_bv, Met);
   MV_A2 := Left_Contraction (e2_bv, ni_bv, Met);
--     MV_A := e1_bv + Outer_Product (Outer_Product (e2_bv, e3_bv), e1_bv);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e2, e3)", MV_A1, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e2, ni)", MV_A2, Basis_Names_C3GA);
--     GA_Utilities.Print_Multivector_String ("LC_Test MV_A", MV_A, Basis_Names_C3GA);

   MV_A1 := Left_Contraction (e3_bv, e2_bv, Met);
   MV_A2 := Left_Contraction (ni_bv, e2_bv, Met);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e3, e2)", MV_A1, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (n1, e2)", MV_A2, Basis_Names_C3GA);

   MV_A1 := Left_Contraction (e3_bv, Geometric_Product (e2_bv, e3_bv, Met), Met);
   MV_A2 := Left_Contraction (ni_bv, Geometric_Product (e2_bv, ni_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e3, GP (e2, e3))", MV_A1, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("LC_Test (ni, GP (e2, ni))", MV_A2, Basis_Names_C3GA);

   MV_A1 := Left_Contraction (e3_bv, Geometric_Product (e3_bv, e2_bv, Met), Met);
   MV_A2 := Left_Contraction (ni_bv, Geometric_Product (ni_bv, e2_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test (e3, GP (e3, e2))", MV_A1, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("LC_Test (ni, GP (ni, e2))", MV_A2, Basis_Names_C3GA);
--     MV_A_Inv := General_Inverse (MV_A);
--     GA_Utilities.Print_Multivector ("LC_Test MV_A_Inv", MV_A_Inv);
--
--     GA_Utilities.Print_Multivector ("LC_Test", NP_1);
--     NP_1_OP := Outer_Product (NP_1, NP_1);
--     GA_Utilities.Print_Multivector ("LC_Test NP_1_OP", NP_1_OP);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1", NP_1);
--     NP_1_OP := Outer_Product (NP_1, NP_1);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_OP", NP_1_OP);
--     NP_1_GP := Geometric_Product (NP_1, NP_1);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_GP", NP_1_GP);
--
--     NP_1_GP_Met := Geometric_Product (NP_1, NP_1, C3_Metric);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_GP_Met", NP_1_GP_Met);
--
--     NP_1_IP := Inner_Product (NP_1, NP_1,Blade.Left_Contraction);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_IP", NP_1_IP);
--     NP_1_IP_Met := Geometric_Product (NP_1, NP_1, C3_Metric);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_IP_Met", NP_1_IP_Met);
--
--     NP_1_Inv := General_Inverse (NP_1, C3_Metric);
--     GA_Utilities.Print_Multivector ("General_Inverse_Test NP_1_Inv", NP_1_Inv);

exception
   when anError :  others =>
      Put_Line ("An exception occurred in LC_Test.");
      raise;
end LC_Test;
