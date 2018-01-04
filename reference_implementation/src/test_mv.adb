
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
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
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("", MV_Info);
   MV := Outer_Product (MV, e1_bv);
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("", MV_Info);
end Test_Mv;
