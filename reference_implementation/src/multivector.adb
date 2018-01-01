--  Derived from ga_ref_impl Multivector.java

package body Multivector is

   function Get_Basis_Vector (Index : Integer) return Multivector is
      BB : constant GA_Maths.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use Ada.Containers;
      use Blade_List_Package;
      B1     : GA_Maths.Basis_Blade;
      B2     : GA_Maths.Basis_Blade;
      Size_1 : constant Count_Type := Length (MV1.Blades);
      Size_2 : constant Count_Type := Length (MV2.Blades);
      Size   : constant Integer := Integer (Size_1 * Size_2);
         List_1   : Blade_List := MV1.Blades;
         Cursor_1 : Cursor;
      MV     : Multivector;
   begin
      begin
         while Has_Element (Cursor_1) loop
            declare
               List_2   : Blade_List := MV1.Blades;
               Cursor_2 : Cursor;
            begin
               while Has_Element (Cursor_2) loop
                  MV.Blades.Append (Outer_Product (B1, B2));
               end loop;
            end;
         end loop;
      end;
      return MV;
   end Outer_Product;

   --  -------------------------------------------------------------------------

end Multivector;
