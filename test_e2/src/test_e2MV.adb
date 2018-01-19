
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with GA_Utilities;
with GA_Maths;

with Multivector; use Multivector;
with Multivector_Type;

procedure Test_E2MV is
   use Blade.Names_Package;
   e1_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.E2_e1);
   e2_bv   : Multivector.Multivector := Get_Basis_Vector (Blade.E2_e2);
   BV_Names     : Blade.Basis_Vector_Names;

   MV           : Multivector.Multivector;
   MV1          : Multivector.Multivector;
   MV11         : Multivector.Multivector;
   MV12         : Multivector.Multivector;
   MV1p2        : Multivector.Multivector;
   MV_Info      : Multivector_Type.MV_Type_Record;

begin
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));

   MV_Info := Multivector_Type.Init (MV);
   GA_Utilities.Print_Multivector ("New MV", MV);
--     Multivector_Type.Print_Multivector_Info ("Null MV", MV_Info);
   New_Line;

   MV_Info := Multivector_Type.Init (e1_bv);
   GA_Utilities.Print_Multivector ("e1", e1_bv);
--     Multivector_Type.Print_Multivector_Info ("e1", MV_Info);

   GA_Utilities.Print_Multivector ("e2", e2_bv);
   MV_Info := Multivector_Type.Init (e2_bv);
--     Multivector_Type.Print_Multivector_Info ("e2", MV_Info);

   MV1 := e1_bv;
   GA_Utilities.Print_Multivector ("MV = e1", MV1);
   MV_Info := Multivector_Type.Init (MV1);
--     Multivector_Type.Print_Multivector_Info ("MV = e1", MV_Info);
   New_Line;
   Put_Line ("Multivector_String, MV = e1:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV1, BV_Names)));
   New_Line;

   MV1p2 := e1_bv + e2_bv;
   GA_Utilities.Print_Multivector ("e1 + e2", MV1p2);
   MV_Info := Multivector_Type.Init (MV1p2);
--     Multivector_Type.Print_Multivector_Info ("e1 + e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String, e1 + e2:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV1p2, BV_Names)));
   New_Line;

   MV12 := Outer_Product (e1_bv, e2_bv);
   GA_Utilities.Print_Multivector ("e1 ^ e2", MV12);
   MV_Info := Multivector_Type.Init (MV12);
--     Multivector_Type.Print_Multivector_Info ("e1 ^ e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String e1 ^ e2:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV12, BV_Names)));

   MV11 := Dot (e1_bv, e1_bv);
   GA_Utilities.Print_Multivector ("e1 . e1", MV11);
   MV_Info := Multivector_Type.Init (MV11);
--     Multivector_Type.Print_Multivector_Info ("e1 . e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String e1 . e1:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV11, BV_Names)));

   MV12 := Dot (e1_bv, e2_bv);
   GA_Utilities.Print_Multivector ("e1 . e2", MV12);
   MV_Info := Multivector_Type.Init (MV12);
--     Multivector_Type.Print_Multivector_Info ("e1 ^ e2", MV_Info);
   New_Line;
   Put_Line ("Multivector_String e1 . e2:");
   Put_Line (Ada.Strings.Unbounded.To_String (Multivector_String (MV12, BV_Names)));

    exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_E2MV.");
      raise;

end Test_E2MV;
