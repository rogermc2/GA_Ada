
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with C3GA_Utilities;
with GA_Maths;

with Multivector; use Multivector;
with Multivector_Type;

procedure Test_MV is
   use Blade.Names_Package;
   no_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.C3_no);
   e1_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.C3_e1);
   e2_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.C3_e2);
   e3_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.C3_e3);
   ni_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.C3_ni);
   BV_Names     : Blade.Basis_Vector_Names;

   MV           : Multivector.Multivector;
   MV1          : Multivector.Multivector;
   MV12         : Multivector.Multivector;
   MV1p2        : Multivector.Multivector;
   MV13         : Multivector.Multivector;
   Op23         : Multivector.Multivector;
   Op23_1       : Multivector.Multivector;
   Add_1_Op23_1 : Multivector.Multivector;
   MV_Info      : Multivector_Type.MV_Type_Record;

begin
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

--     MV_Info := Multivector_Type.Init (MV);
--     C3GA_Utilities.Print_Multivector ("New", MV);
--     Multivector_Type.Print_Multivector_Info ("Null MV", MV_Info);
--     New_Line;
--
--     C3GA_Utilities.Print_Multivector ("no", no_bv);
--     MV_Info := Multivector_Type.Init (no_bv);
--     Put_Line ("Bit count: " & GA_Maths.Unsigned_Integer
--                    'Image (1) &
--                 Natural'Image (GA_Maths.Bit_Count (1)));
--     Multivector_Type.Print_Multivector_Info ("no", MV_Info);
--
--     MV_Info := Multivector_Type.Init (e1_bv);
--     C3GA_Utilities.Print_Multivector ("e1", e1_bv);
--     Multivector_Type.Print_Multivector_Info ("e1", MV_Info);
--
--     C3GA_Utilities.Print_Multivector ("e2", e2_bv);
--     MV_Info := Multivector_Type.Init (e2_bv);
--     Multivector_Type.Print_Multivector_Info ("e2", MV_Info);
--
--     MV_Info := Multivector_Type.Init (e3_bv);
--     C3GA_Utilities.Print_Multivector ("e3", e3_bv);
--     Multivector_Type.Print_Multivector_Info ("e3", MV_Info);
--
--     C3GA_Utilities.Print_Multivector ("ni", ni_bv);
--     MV_Info := Multivector_Type.Init (ni_bv);
--     Multivector_Type.Print_Multivector_Info ("ni", MV_Info);
--
   MV1 := e1_bv;
   C3GA_Utilities.Print_Multivector ("MV = e1", MV1);
   MV_Info := Multivector_Type.Init (MV1);
   Multivector_Type.Print_Multivector_Info ("MV = e1", MV_Info);
   New_Line;
   Put_Line ("Multivector_String:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV1, BV_Names)));

   MV1p2 := e1_bv + e2_bv;
   C3GA_Utilities.Print_Multivector ("e1 + e2", MV1p2);
   MV_Info := Multivector_Type.Init (MV1p2);
   Multivector_Type.Print_Multivector_Info ("e1 + e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV1p2, BV_Names)));

   MV12 := Outer_Product (e1_bv, e2_bv);
   C3GA_Utilities.Print_Multivector ("e1 ^ e2", MV12);
   MV_Info := Multivector_Type.Init (MV12);
   Multivector_Type.Print_Multivector_Info ("e1 ^ e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV12, BV_Names)));

   MV13 := Outer_Product (e1_bv, e3_bv);
   C3GA_Utilities.Print_Multivector ("e1 ^ e3", MV13);
   MV_Info := Multivector_Type.Init (MV12);
   Multivector_Type.Print_Multivector_Info ("e1 ^ e3", MV_Info);
   New_Line;
   Put_Line ("Multivector_String:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV13, BV_Names)));

   --  Multivector A = e1.add(e2.op(e3).op(e1));
   --                = e1 + (e2^e3)^e1)
   Op23 := Outer_Product (e2_bv, e3_bv);
   Op23_1 := Outer_Product (Op23, e1_bv);
   Add_1_Op23_1 := e1_bv + Op23_1;
   C3GA_Utilities.Print_Multivector ("e1 + ((e2 ^ e3) ^ e1", Add_1_Op23_1);
   MV_Info := Multivector_Type.Init (Add_1_Op23_1);
   Multivector_Type.Print_Multivector_Info ("e1 + ((e2 ^ e3) ^ e1", MV_Info);
   New_Line;
   Put_Line ("Multivector_String:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (Add_1_Op23_1, BV_Names)));

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_MV.");
      raise;

end Test_MV;
