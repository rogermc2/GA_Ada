
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with C3GA_Utilities;
with Multivector; use Multivector;
with Multivector_Type;

procedure Test_Mv is
   no_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.no);
   e1_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.e1);
   e2_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.e2);
   e3_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.e3);
   ni_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.ni);

   MV      : Multivector.Multivector;
   MV_Info : Multivector_Type.MV_Type_Record;
begin
   MV_Info := Multivector_Type.Init (e1_bv);
   C3GA_Utilities.Print_Multivector ("e1", e1_bv);
   Multivector_Type.Print_Multivector_Info ("e1", MV_Info);

   MV_Info := Multivector_Type.Init (e2_bv);
   C3GA_Utilities.Print_Multivector ("e2", e2_bv);
   Multivector_Type.Print_Multivector_Info ("e2", MV_Info);

   MV_Info := Multivector_Type.Init (MV);
   C3GA_Utilities.Print_Multivector ("New", MV);
   Multivector_Type.Print_Multivector_Info ("Null MV", MV_Info);

   MV := e1_bv;
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("e1", MV_Info);
   C3GA_Utilities.Print_Multivector ("e1", MV);

   MV := Outer_Product (MV, e2_bv);
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("e1 ^ e2", MV_Info);
   C3GA_Utilities.Print_Multivector ("e1 ^ e2", MV);
end Test_Mv;
