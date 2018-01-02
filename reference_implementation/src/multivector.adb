--  Derived from ga_ref_impl Multivector.java

with Maths;

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

   function Extract_Grade (MV : Multivector; Index : integer) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      Blades    : constant Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Max_Grade : Unsigned_Integer := 0;
      Gr        : array (1 .. Unsigned_Integer (Index)) of Unsigned_Integer;
      New_List  : Blade_List;
      aGrade    : Unsigned_Integer;
      MV_E      : Multivector;
   begin
      for k in Gr'Range loop
         Gr (k) := k;
         if Gr (k) > Max_Grade then
            Max_Grade := Gr (k);
         end if;
      end loop;

      declare
         Keep : array (0 .. Max_Grade + 1) of Boolean
           := (others => True);
      begin

         while Has_Element (Curs) loop
            thisBlade := Element (Curs);
            aGrade := GA_Maths.Grade (Bitmap (thisBlade));
            if aGrade <= Max_Grade and then Keep (aGrade) then
               New_List.Append (thisBlade);
            end if;
            Next (Curs);
         end loop;
      end;
      return MV_E;
   end Extract_Grade;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : Integer) return Multivector is
      BB : constant Blade.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Blade_List (MV : Multivector) return Blade_List is
   begin
      return MV.Blades;
   end Get_Blade_List;

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

   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      thisBlade  : Blade.Basis_Blade;
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

   function Largest_Grade_Part (MV : Multivector) return Multivector is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      thisBlade     : Blade.Basis_Blade;
      Cursor_B      : Cursor := Blades.First;
      GU            : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
      Largest_Part  : Multivector;
      Max_Norm      : Float := 0.0;
   begin
      for Count in 0 .. Top_Grade_Index (MV) loop
         if (GU and Shift_Left (1, Integer (Grade (Bitmap (thisBlade))))) /= 0 then
            null;
         end if;
      end loop;
      return  Largest_Part;
   end Largest_Grade_Part;

   --  -------------------------------------------------------------------------

   function Outer_Product (MV1, MV2 : Multivector) return Multivector is
      use Ada.Containers;
      use Blade_List_Package;
      B1       : Blade.Basis_Blade;
      B2       : Blade.Basis_Blade;
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
      use GA_Maths;
      BB       : Blade.Basis_Blade;
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
      use GA_Maths;
      Current      : Blade.Basis_Blade;
      Previous     : Blade.Basis_Blade;
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

   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer is
      use Blade_List_Package;
      use GA_Maths;
      Max_G        : Integer := 0;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      Current      : Blade.Basis_Blade;
   begin
      while Has_Element (Blade_Cursor) loop
         Current := Element (Blade_Cursor);
         Max_G := Maths.Maximum (Max_G, Integer (GA_Maths.Grade (Bitmap (Current))));
         Next (Blade_Cursor);
      end loop;
      return Unsigned_Integer (Max_G);
   end Top_Grade_Index;

   --  -------------------------------------------------------------------------

begin
   C3_Blade_List.Append (New_Basis_Blade (0));
   C3_Blade_List.Append (New_Basis_Blade (1));
   C3_Blade_List.Append (New_Basis_Blade (2));
   C3_Blade_List.Append (New_Basis_Blade (3));
   C3_Blade_List.Append (New_Basis_Blade (4));

end Multivector;
