
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
   MV_A_Inv     : Multivector;

   NP_1         : constant Normalized_Point :=
                    New_Normalized_Point (1.0, 0.0, 0.0);
   NP_2         : constant Normalized_Point :=
                    New_Normalized_Point (0.0, 1.0, 0.0);
   NP_123       : constant Normalized_Point :=
                    New_Normalized_Point (-1.0, 2.0, -2.0);
   C1           : constant Normalized_Point :=
                    New_Normalized_Point (0.707, 0.707, 0.0);
   C2           : constant Normalized_Point :=
                    New_Normalized_Point (0.0, 1.0, 0.0);
   C3           : constant Normalized_Point :=
                    New_Normalized_Point (-0.356756, -0.881980, 0.0);
   Circle       : constant Multivector := Outer_Product (C1, Outer_Product (C2, C3));
   NP_1_OP      : Multivector;
   NP_1_IP      : Multivector;
   NP_1_LC      : Multivector;
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
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e3, e2)", MV_A1, Basis_Names_C3GA);
   New_Line;
   MV_A2 := Left_Contraction (ni_bv, e2_bv, Met);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (ni, e2)", MV_A2, Basis_Names_C3GA);

   MV_A1 := Left_Contraction (e3_bv, Geometric_Product (e2_bv, e3_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test LC (e3, GP (e2, e3))", MV_A1, Basis_Names_C3GA);
   MV_A2 := Left_Contraction (ni_bv, Geometric_Product (e2_bv, ni_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test (ni, GP (e2, ni))", MV_A2, Basis_Names_C3GA);

   MV_A1 := Left_Contraction (e3_bv, Geometric_Product (e3_bv, e2_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test (e3, GP (e3, e2))", MV_A1, Basis_Names_C3GA);
   MV_A2 := Left_Contraction (ni_bv, Geometric_Product (ni_bv, e2_bv, Met), Met);
   GA_Utilities.Print_Multivector_String ("LC_Test (ni, GP (ni, e2))", MV_A2, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("LC_Test Circle", Circle, Basis_Names_C3GA);

   GA_Utilities.Print_Multivector_String ("LC_Tests NP_1", NP_1, Basis_Names_C3GA);
   NP_1_IP := Inner_Product (NP_1, NP_1, Met);
   NP_1_OP := Outer_Product (NP_1, NP_1);
   NP_1_GP := Geometric_Product (NP_1, NP_1, Met);
   GA_Utilities.Print_Multivector_String ("Inner Product (NP_1, NP_1)", NP_1_IP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Outer Product (NP_1, NP_1)", NP_1_OP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Geometric Product (NP_1, NP_1)", NP_1_GP, Basis_Names_C3GA);
   New_Line;

   NP_1_IP := Inner_Product (NP_123, NP_123, Met);
   NP_1_OP := Outer_Product (NP_123, NP_123);
   NP_1_GP := Geometric_Product (NP_123, NP_123, Met);
   GA_Utilities.Print_Multivector_String ("Inner Product (NP_123, NP_123)", NP_1_IP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Outer Product (NP_123, NP_123)", NP_1_OP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Geometric Product (NP_123, NP_123)", NP_1_GP, Basis_Names_C3GA);

   New_Line;
   NP_1_IP := Inner_Product (NP_1, NP_2, Met);
   NP_1_OP := Outer_Product (NP_1, NP_2);
   NP_1_GP := Geometric_Product (NP_1, NP_2, Met);
   GA_Utilities.Print_Multivector_String ("Inner Product (NP_1, NP_2)", NP_1_IP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Outer Product (NP_1, NP_2)", NP_1_OP, Basis_Names_C3GA);
   GA_Utilities.Print_Multivector_String ("Geometric Product (NP_1, NP_2)", NP_1_GP, Basis_Names_C3GA);

   New_Line;
   NP_1_IP := Left_Contraction (NP_1, NP_2, Met);
   GA_Utilities.Print_Multivector_String ("Left Contraction (NP_1, NP_2)", NP_1_IP, Basis_Names_C3GA);
   NP_1_IP := Left_Contraction (NP_1, NP_123, Met);
   GA_Utilities.Print_Multivector_String ("Left Contraction (NP_1, NP_123)", NP_1_IP, Basis_Names_C3GA);

exception
   when anError :  others =>
      Put_Line ("An exception occurred in LC_Test.");
      raise;
end LC_Test;
