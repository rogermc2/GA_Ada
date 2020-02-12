
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Blade_Types;
with GA_Maths;
with GA_Utilities;
with Metric;

with Multivectors; use Multivectors;
with Multivector_Type;

procedure Test_MV is
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
   MV_AI_1      : Multivector;
   MV_Info      : Multivector_Type.MV_Type_Record;

begin
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

   MV_A := e1_bv + Outer_Product (e2_bv, (Outer_Product (e3_bv, e1_bv)));
   GA_Utilities.Print_Multivector ("MV_A", MV_A);
   New_Line;
   MV_Info := Multivector_Type.Init (MV_A);
   GA_Utilities.Print_Multivector_Info ("MV A Info", MV_Info);
   New_Line;

   MV_A := e1_bv + Outer_Product (e2_bv, e3_bv);
   GA_Utilities.Print_Multivector ("MV_A", MV_A);

   if General_Inverse (MV_A, Metric.Matrix (Met), MV_AI_1) then
        GA_Utilities.Print_Multivector ("MV_AI_1", MV_AI_1);
        MV_Info := Multivector_Type.Init (MV_AI_1);
        GA_Utilities.Print_Multivector_Info ("MV AI 1 Info", MV_Info);
   else
        Put_Line ("Multivector MV_A is not invertable.");
   end if;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_MV.");
      raise;
end Test_MV;
