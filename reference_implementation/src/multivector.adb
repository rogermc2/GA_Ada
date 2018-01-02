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

   C3_Blade_List : Blade_List;

   no : Multivector := Get_Basis_Vector (0);
   e1 : Multivector := Get_Basis_Vector (1);
   e2 : Multivector := Get_Basis_Vector (2);
   e3 : Multivector := Get_Basis_Vector (3);
   ni : Multivector := Get_Basis_Vector (4);

    procedure Simplify (MV : in out Multivector);

     --  -------------------------------------------------------------------------

   function C3_Multivector return Multivector is
      MV : Multivector;
   begin
      MV.Blades := C3_Blade_List;
      return MV;
   end C3_Multivector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : Integer) return Multivector is
      BB : constant GA_Maths.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------
   --  Grade returns the grade of a Multivector if homogeneous, -1 otherwise.
   --  0 is return for null Multivectors.
--     function Grade (Blades : Blade_List) return Unsigned_Integer is
--        use Blade_List_Package;
--        thisBlade : GA_Maths.Basis_Blade;
--        Cursor_B  : Cursor := Blades.First;
--        theGrade  : Unsigned_Integer := 0; -- 0 .. 32
--        OK        : Boolean := True;
--     begin
--        while OK and Has_Element (Cursor_B) loop
--           thisBlade := Element (Cursor_B);
--           if Cursor_B = Blades.First then
--              theGrade := GA_Maths.Grade (GA_Maths.Bitmap (thisBlade));
--           elsif theGrade /= GA_Maths.Grade (GA_Maths.Bitmap (thisBlade)) then
--              OK := False;
--           end if;
--           Next (Cursor_B);
--        end loop;
--
--        return theGrade;
--     end Grade;

   --  -------------------------------------------------------------------------

   function Grade_Use (MV : Multivector) return Grade_Usage is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      thisBlade  : Basis_Blade;
      Cursor_B   : Cursor := Blades.First;
      GU         : Unsigned_32 := 0;
   begin
      while Has_Element (Cursor_B) loop
         thisBlade := Element (Cursor_B);
         GU :=  GU or
            Shift_Left (1, Integer (Grade (Bitmap (thisBlade))));
         Next (Cursor_B);
      end loop;
      return Unsigned_Integer (GU);
   end Grade_Use;

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
                  Next (Cursor_2);
               end loop;
            end;
            Next (Cursor_1);
         end loop;
      end;
      Simplify (MV);
      return MV;
   end Outer_Product;

   --  -------------------------------------------------------------------------

   function Scalar_Part (MV : Multivector) return Float is
      use Blade_List_Package;
      BB       : GA_Maths.Basis_Blade;
      Blades   : Blade_List := MV.Blades;
      B_Cursor : Cursor := Blades.First;
      Sum      : Float := 0.0;
   begin
      while Has_Element (B_Cursor) loop
         BB := Element (B_Cursor);
         if Bitmap (BB) = 0 then
            Sum := Sum + Blade_Scale (BB);
         end if;
         Next (B_Cursor);
      end loop;
      return Sum;
   end Scalar_Part;

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
         Next (Blade_Cursor);
      end loop;

      Blade_Cursor := Blades.First;
      if Remove_Nulls then
         while Has_Element (Blade_Cursor) loop
            Current := Element (Blade_Cursor);
            if Blade_Scale (Current) = 0.0 then
               Blades.Delete (Blade_Cursor);
            end if;
            Next (Blade_Cursor);
         end loop;
      end if;
      MV.Blades := Blades;
   end Simplify;

   --  -------------------------------------------------------------------------

begin
   C3_Blade_List.Append (New_Basis_Blade (0));
   C3_Blade_List.Append (New_Basis_Blade (1));
   C3_Blade_List.Append (New_Basis_Blade (2));
   C3_Blade_List.Append (New_Basis_Blade (3));
   C3_Blade_List.Append (New_Basis_Blade (4));

end Multivector;
