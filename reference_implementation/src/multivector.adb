--  Derived from ga_ref_impl Multivector.java

package body Multivector is

   --  setup conformal algebra:
   type Basis_Vector_ID is (BV_no, BV_e1, BV_e2, BV_e3, BV_ni);
   Basis_Vector_Names : constant Array (1 .. 5) of String (1 .. 2) :=
     ("no", "e1", "e2", "e3", "ni");
   Basis : array (1 .. 5, 1 ..5) of float :=
	   ((0.0, 0.0, 0.0, 0.0, -1.0),
	    (0.0, 1.0, 0.0, 0.0, 0.0),
	    (0.0, 0.0, 1.0, 0.0, 0.0),
	    (0.0, 0.0, 0.0 ,1.0, 0.0),
	    (-1.0, 0.0, 0.0 , 0.0, 0.0));

   procedure Simplify (MV : in out Multivector);

     --  -------------------------------------------------------------------------

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
      B1       : GA_Maths.Basis_Blade;
      B2       : GA_Maths.Basis_Blade;
      List_1   : Blade_List := MV1.Blades;
      Cursor_1 : Cursor := List_1.First;
      MV       : Multivector;
   begin
      begin
         while Has_Element (Cursor_1) loop
            B1 := Element (Cursor_1);
            declare
               List_2   : Blade_List := MV1.Blades;
               Cursor_2 : Cursor := List_2.First;
            begin
               while Has_Element (Cursor_2) loop
                  B2 := Element (Cursor_2);
                  MV.Blades.Append (Outer_Product (B1, B2));
               end loop;
            end;
         end loop;
      end;
      Simplify (MV);
      return MV;
   end Outer_Product;

   --  -------------------------------------------------------------------------

   procedure Simplify (MV : in out Multivector) is
      use Blade_List_Package;
      Current      : GA_Maths.Basis_Blade;
      Previous     : GA_Maths.Basis_Blade;
      Has_Previous : Boolean := False;
      New_MV       : Multivector;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      Remove_Nulls :  Boolean := False;

   begin
      while Has_Element (Blade_Cursor) loop
         Current := Element (Blade_Cursor);
         if Blade_Scale (Current) = 0.0 then
            Blades.Delete (Blade_Cursor);
            Has_Previous := False;
         elsif Has_Previous and then
           Bitmap (Previous) = Bitmap (Current) then
            Update_Blade (Previous, Blade_Scale (Previous) + Blade_Scale (Current));
            Blades.Delete (Blade_Cursor);
         else
            if Has_Previous and then Blade_Scale (Previous) = 0.0 then
               Remove_Nulls := True;
            end if;
            Previous := Current;
         end if;
      end loop;

      Blade_Cursor := Blades.First;
      if Remove_Nulls then
         while Has_Element (Blade_Cursor) loop
            Current := Element (Blade_Cursor);
            if Blade_Scale (Current) = 0.0 then
               Blades.Delete (Blade_Cursor);
            end if;
         end loop;
      end if;
      MV.Blades := Blades;
   end Simplify;

   --  -------------------------------------------------------------------------

end Multivector;
