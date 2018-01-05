
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with C3GA_Utilities;
with GA_Maths;

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
   C3GA_Utilities.Print_Multivector ("New", MV);
   Multivector_Type.Print_Multivector_Info ("Null MV", MV_Info);
   New_Line;

   C3GA_Utilities.Print_Multivector ("no", no_bv);
   MV_Info := Multivector_Type.Init (no_bv);
   Put_Line ("Bit count: " & GA_Maths.Unsigned_Integer
                  'Image (1) &
               Natural'Image (GA_Maths.Bit_Count (1)));
   Multivector_Type.Print_Multivector_Info ("no", MV_Info);

   MV_Info := Multivector_Type.Init (e1_bv);
   C3GA_Utilities.Print_Multivector ("e1", e1_bv);
   Multivector_Type.Print_Multivector_Info ("e1", MV_Info);

   C3GA_Utilities.Print_Multivector ("e2", e2_bv);
   MV_Info := Multivector_Type.Init (e2_bv);
   Multivector_Type.Print_Multivector_Info ("e2", MV_Info);

   MV_Info := Multivector_Type.Init (e3_bv);
   C3GA_Utilities.Print_Multivector ("e3", e3_bv);
   Multivector_Type.Print_Multivector_Info ("e3", MV_Info);

   C3GA_Utilities.Print_Multivector ("ni", ni_bv);
   MV_Info := Multivector_Type.Init (ni_bv);
   Multivector_Type.Print_Multivector_Info ("ni", MV_Info);

   MV := e1_bv;
   C3GA_Utilities.Print_Multivector ("MV = e1", MV);
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("MV = e1", MV_Info);

   MV := Outer_Product (MV, e2_bv);
   C3GA_Utilities.Print_Multivector ("e1 ^ e2", MV);
   MV_Info := Multivector_Type.Init (MV);
   Multivector_Type.Print_Multivector_Info ("e1 ^ e2", MV_Info);

--     for i in GA_Maths.Unsigned_Integer range 0 .. 20 loop
--        Put_Line ("Bit count: " & GA_Maths.Unsigned_Integer
--                    'Image (i) &
--                  Natural'Image (GA_Maths.Bit_Count (i)));
--     end loop;

--     Put_Line ("Bit count: " & GA_Maths.Unsigned_Integer
--                    'Image (1027) &
--                 Natural'Image (GA_Maths.Bit_Count (1027)));
end Test_Mv;
