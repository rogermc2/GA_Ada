
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;
with Maths;

with Blade;
with Blade_Types;
with GA_Maths;
with GA_Utilities;

with Multivectors; use Multivectors;
with Multivector_Type;
with Multivector_Utilities;

procedure Test_MV_Factor is
    use GL.Types;
    use Blade;
--     use Blade.Names_Package;
   no_bv   : Multivector := Basis_Vector (Blade_Types.C3_no);
   e1_bv   : Multivector := Basis_Vector (Blade_Types.C3_e1);
   e2_bv   : Multivector := Basis_Vector (Blade_Types.C3_e2);
   e3_bv   : Multivector := Basis_Vector (Blade_Types.C3_e3);
   ni_bv   : Multivector := Basis_Vector (Blade_Types.C3_ni);
   BV_Names     : Blade.Basis_Vector_Names;
   Dim          : constant Integer := 8;
   Scale        : Float;
   BL           : Blade.Blade_List;
   B_MV         : Multivector;
   R_MV         : Multivector;
   Factors      : Multivector_List;

begin
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("no"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e1"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e2"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("e3"));
   BV_Names.Append (Ada.Strings.Unbounded.To_Unbounded_String ("ni"));

   B_MV := Get_Random_Blade
      (Dim, Integer (Maths.Random_Float * (Single (Dim) + 0.49)), 1.0);
   BL.Add_Blade (New_Basis_Blade (30, -0.662244));
   BL.Add_Blade (New_Basis_Blade (29, -0.391495));
   BL.Add_Blade (New_Basis_Blade (27, -0.430912));
   BL.Add_Blade (New_Basis_Blade (23, 0.218277));
   BL.Add_Blade (New_Basis_Blade (15, -0.213881));
   B_MV := New_Multivector (BL);
   GA_Utilities.Print_Multivector ("B_MV", B_MV);
   Factors := Multivector_Utilities.Factorize_Blades (B_MV, Scale);
   GA_Utilities.Print_Multivector_List("Factors", Factors);

   R_MV := New_Multivector (Scale);
   for index in 1 .. List_Length (Factors) loop
        R_MV := Outer_Product (R_MV, MV_Item (Factors, index));
   end loop;

   GA_Utilities.Print_Multivector ("R_MV", R_MV);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_MV_Factor.");
      raise;

end Test_MV_Factor;
