
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Blade;
with C3GA;
with GA_Utilities;
with Inner_Product_Types;
with Metric;
with Multivectors;

--  Based on Util.java main
procedure Test_Inner_Product_Types is
   use Blade;
   use Inner_Product_Types;
   use Multivectors;
   Dim          : constant Integer := 8;
   --          no     : Multivector := New_Multivector (New_Basis_Blade (0));
   --          e1     : Multivector := New_Multivector (New_Basis_Blade (0));
   --          e2     : Multivector := New_Multivector (New_Basis_Blade (0));
   --          e3     : Multivector := New_Multivector (New_Basis_Blade (0));
   --          ni     : Multivector := New_Multivector (New_Basis_Blade (0));
   Scale        : Float := 0.0;
   Scale_Signs  : array (1 .. Dim + 1) of String (1 .. 1);
   Blades       : Blade_List;
   MV_B         : Multivector;
   MV_R         : Multivector;
   MV_Fast      : Multivector;
   Factors_F    : Multivector_List;
   Fast_Factors : Multivector_List;
   K_Grade      : Integer;
   OK           : Boolean;
   Check_Scale  : Float;
begin
   MV_B := Random_Blade
     (Dim, Integer (Float (Maths.Random_Float) * (float (Dim) + 0.49)), 1.0);
   Blades.Append (New_Basis_Blade (30, -0.662244));
   Blades.Append (New_Basis_Blade (29, -0.391495));
   Blades.Append (New_Basis_Blade (27, -0.430912));
   Blades.Append (New_Basis_Blade (23,  0.218277));
   Blades.Append (New_Basis_Blade (15, -0.213881));
   Update (MV_B, Blades);

   GA_Utilities.Print_Multivector ("MV_B before factorization", MV_B);
   Factors_F := Factorize_Blade (MV_B, Scale);

   MV_R := New_Multivector (1.0);
   for index in 1 .. List_Length (Factors_F) loop
      GA_Utilities.Print_Multivector ("Factorized Blade ",
                                      MV_Item (Factors_F, index));
      MV_R := Outer_Product (MV_R, MV_Item (Factors_F, index));
   end loop;
   GA_Utilities.Print_Multivector ("MV_R", MV_R);

   Fast_Factors := Factorize_Blade_Fast (MV_B, Scale);
   MV_Fast := New_Multivector (1.0);
   for index in 1 .. List_Length (Fast_Factors) loop
      GA_Utilities.Print_Multivector ("Factorized Blade ", MV_Item (Fast_Factors, index));
      MV_Fast := Outer_Product (MV_Fast, MV_Item (Fast_Factors, index));
   end loop;

   MV_B := Unit_E (MV_B);
   MV_R := Unit_E (MV_R);
   MV_Fast := Unit_E (MV_Fast);

   OK := Grade (MV_B, K_Grade);
   if not OK then
      raise Inner_Product_Types_Exception with
        "Factorization_Test, inhomogeneous multivector detected.";
   else
      Check_Scale := Scalar_Part (Geometric_Product (MV_R, Versor_Inverse (MV_Fast)));
      if Check_Scale < 0.0 then
         Put_Line ("Whaaaaa! Scalar_Part < 0: " & Float'Image (Check_Scale));
         Scale_Signs (K_Grade) := "-";
      else
         Scale_Signs (K_Grade) := "+";
      end if;
      GA_Utilities.Print_Multivector ("Factorization_Test MV_B", MV_B);
      GA_Utilities.Print_Multivector ("Factorization_Test MV_R", MV_R);
      GA_Utilities.Print_Multivector ("Factorization_Test MV_Fast", MV_Fast);
      Put_Line ("B = " & C3GA.Multivector_String (MV_B) & ", ");
      Put_Line ("R = " & C3GA.Multivector_String (MV_R) & ", ");
      Put_Line ("Ra = " & C3GA.Multivector_String (MV_Fast) & ", ");
   end if;

exception
   when others =>
      Put_Line ("An exception occurred in Inner_Product_Types.Factorization_Test");
      raise;
end Test_Inner_Product_Types;
