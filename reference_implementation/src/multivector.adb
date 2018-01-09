--  Derived from ga_ref_impl Multivector.java

with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Multivector is

   --  setup conformal algebra:
   Basis_Vector_Names : constant Array (1 .. 5) of String (1 .. 2) :=
     ("no", "e1", "e2", "e3", "ni");
   Basis : array (1 .. 5, 1 ..5) of float :=
     ((0.0, 0.0, 0.0, 0.0, -1.0),
      (0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0 ,1.0, 0.0),
      (-1.0, 0.0, 0.0 , 0.0, 0.0));

   C3_Blade_List : Blade_List;

--     no_bv : Multivector := Get_Basis_Vector (no);
--     e1_bv : Multivector := Get_Basis_Vector (e1);
--     e2_bv : Multivector := Get_Basis_Vector (e2);
--     e3_bv : Multivector := Get_Basis_Vector (e3);
--     ni_bv : Multivector := Get_Basis_Vector (ni);

   procedure Simplify (Blades : in out Blade_List);
   procedure Simplify (MV : in out Multivector);

   --  -------------------------------------------------------------------------

   function Add (MV : Multivector; S :Float) return Multivector is
      use Blade_List_Package;
      Blades  : constant Blade_List := MV.Blades;
      aBlade  : Blade.Basis_Blade;
      Curs    : Cursor := Blades.First;
      MV1     : Multivector;
   begin
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         MV1.Blades.Append (aBlade);
         Next (Curs);
      end loop;
      MV1.Blades.Append (New_Scalar_Blade (S));
      Simplify (MV1);
      return MV1;
   end Add;

   --  ------------------------------------------------------------------------

   function "+" (MV1, MV2 : Multivector) return Multivector is
      use Blade_List_Package;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV1.Blades;
      Blade_1    : Blade.Basis_Blade;
      Blade_2    : Blade.Basis_Blade;
      Curs_1    : Cursor := Blades_1.First;
      Curs_2    : Cursor := Blades_2.First;
      Sum       : Float := 0.0;
      MV        : Multivector := MV1;
      Blades_3  : constant Blade_List := MV1.Blades;
      Blade_3   : Blade.Basis_Blade;
      Curs_3    : Cursor := Blades_2.First;
   begin
      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         Blade_2 := Element (Curs_2);
         Blade_3 := Element (Curs_3);
         Sum := Weight (Blade_1) + Weight (Blade_2);
         Blade.Update_Blade (Blade_3, Sum);
         MV.Blades.Replace_Element (Curs_3, Blade_3);
         Next (Curs_1);
         Next (Curs_2);
         Next (Curs_3);
      end loop;
      return MV;
   end "+";

   --  -------------------------------------------------------------------------

   function "-" (MV : Multivector) return Multivector is
      use Blade_List_Package;
      Blades_1   : constant Blade_List := MV.Blades;
      Blade_1    : Blade.Basis_Blade;
      Curs_1     : Cursor := Blades_1.First;
      Neg        : Float := 0.0;
      MV_Neg     : Multivector := MV;
      Blades_2   : constant Blade_List := MV_Neg.Blades;
      Blade_2    : Blade.Basis_Blade;
      Curs_2     : Cursor := Blades_2.First;
   begin
      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         Neg := - Weight (Blade_1);
         Blade.Update_Blade (Blade_2, Neg);
         MV_Neg.Blades.Replace_Element (Curs_2, Blade_2);
         Next (Curs_1);
         Next (Curs_2);
      end loop;
      return MV_Neg;
   end "-";

   --  -------------------------------------------------------------------------

   function "-" (MV1, MV2 : Multivector) return Multivector is
      MV        : Multivector := -MV2;
   begin
      return MV1 + MV;
   end "-";

   --  -------------------------------------------------------------------------

   procedure Add_Blade (MV : in out Multivector; aBlade : Blade.Basis_Blade) is
   begin
      MV.Blades.Append (aBlade);
   end Add_Blade;

   --  -------------------------------------------------------------------------

   function Blades (MV : Multivector) return Blade_List is
   begin
      return MV.Blades;
   end Blades;

   --  -------------------------------------------------------------------------

   function C3_Multivector return Multivector is
      MV : Multivector;
   begin
      MV.Blades := C3_Blade_List;
      return MV;
   end C3_Multivector;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, Size (MV)) - 1;
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (C3_Base'Enum_Val (Index)));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------

   function Dual (MV : Multivector; Dim : Integer) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      use Interfaces;
      Index   : constant Unsigned_32 := Shift_Left (1, dim) - 1;
      Dual_MV : Multivector;
   begin
      Dual_MV.Blades.Append (Blade.New_Basis_Blade (C3_Base'Enum_Val (Index)));
      Dual_MV := Versor_Inverse (Dual_MV);
      Dual_MV := Inner_Product (MV, Dual_MV, Left_Contraction);
      return Dual_MV;
   end Dual;

   --  -------------------------------------------------------------------------

   function Extract_Grade (MV : Multivector; Index : integer) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      Blades    : constant Blade_List := MV.Blades;
      thisBlade : Blade.Basis_Blade;
      Curs      : Cursor := Blades.First;
      Max_Grade : Natural := 0;
      Gr        : array (1 .. Index) of Natural;
      New_List  : Blade_List;
      aGrade    : Natural;
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

   function Geometric_Product (MV : Multivector; Sc : Float) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      Blades    : constant Blade_List := MV.Blades;
      Curs      : Cursor := Blades.First;
      BB        : Blade.Basis_Blade;
      New_MV    : Multivector;
   begin
      while Has_Element (Curs) loop
         BB := Element (Curs);
         New_MV.Blades.Append (Blade.Geometric_Product (BB, Sc));
         Next (Curs);
      end loop;
      return New_MV;
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (Sc : Float; MV : Multivector) return Multivector is
   begin
      return Geometric_Product (MV, Sc);
   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      Blades_1  : constant Blade_List := MV1.Blades;
      Blades_2  : constant Blade_List := MV2.Blades;
      Curs_1    : Cursor := Blades_1.First;
      Curs_2    : Cursor := Blades_2.First;
      Blade_1   : Blade.Basis_Blade;
      Blade_2   : Blade.Basis_Blade;
      MV        : Multivector;
   begin
      if Is_Empty (List (Blades_1)) then
         Put_Line ("Geometric_Product, MV1 is null.");
      end if;
      if Is_Empty (List (Blades_2)) then
         Put_Line ("Geometric_Product, MV2 is null.");
      end if;
      while Has_Element (Curs_1) loop
         Blade_1 := Element (Curs_1);
         while Has_Element (Curs_2) loop
            Blade_2 := Element (Curs_2);
            MV.Blades.Append (Blade.Geometric_Product (Blade_1, Blade_2));
            Next (Curs_2);
         end loop;
         Next (Curs_1);
      end loop;

      if Is_Empty (MV.Blades) then
         Put_Line ("Geometric_Product, product MV is null.");
      end if;
      Put_Line ("Leaving Geometric_Product.");
      return MV;

   end Geometric_Product;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : E2_Base) return Multivector is
      BB : constant Blade.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : E3_Base) return Multivector is
      BB : constant Blade.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

   function Get_Basis_Vector (Index : C3_Base) return Multivector is
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

   function Grade_Inversion (MV : Multivector) return Multivector is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades        : constant Blade_List := MV.Blades;
      Inversion     : Blade_List;
      thisBlade     : Blade.Basis_Blade;
      Cursor_B      : Cursor := Blades.First;
      GU            : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
      Largest_Part  : Multivector;
      Max_Norm      : Float := 0.0;
   begin
      while Has_Element (Cursor_B) loop
         thisBlade := Element (Cursor_B);
         Inversion.Append (Blade.Grade_Inversion (thisBlade));
         Next (Cursor_B);
      end loop;
      return  (Inversion, False);
   end Grade_Inversion;

   --  -------------------------------------------------------------------------

   function Grade_Use (MV : Multivector) return GA_Maths.Grade_Usage is
      use GA_Maths;
      use Interfaces;
      use Blade_List_Package;
      Blades     : constant Blade_List := MV.Blades;
      BB         : Blade.Basis_Blade;
      Cursor_B   : Cursor := Blades.First;
      GU_Bitmap  : Unsigned_32 := 0;
      Index      : Integer := 0;
   begin
--        New_Line;
      while Has_Element (Cursor_B) loop
         Index := Index + 1;
         BB := Element (Cursor_B);
--           Put_Line ("Grade_Use Index:" & Integer'Image (Index));
--           Put_Line ("Grade_Use, Bitmap" & Unsigned_Integer'Image (Bitmap (BB)));
         GU_Bitmap := GU_Bitmap or
              Shift_Left (1, Integer (GA_Maths.Grade (Bitmap (BB))));
--           Put_Line ("Grade_Use, GU Bitmap" & Unsigned_32'Image (GU_Bitmap));
         Next (Cursor_B);
      end loop;
      return Unsigned_Integer (GU_Bitmap);
   end Grade_Use;

   --  -------------------------------------------------------------------------

   function Inner_Product (MV1, MV2 : Multivector; Cont : Contraction_Type)
                           return Multivector is
      use Ada.Containers;
      use Blade_List_Package;
      B1       : Blade.Basis_Blade;
      B2       : Blade.Basis_Blade;
      List_1   : Blade_List := MV1.Blades;
      Cursor_1 : Cursor := List_1.First;
      MV       : Multivector;
   begin
      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         declare
            List_2   : Blade_List := MV1.Blades;
            Cursor_2 : Cursor := List_2.First;
         begin
            while Has_Element (Cursor_2) loop
               B2 := Element (Cursor_2);
               MV.Blades.Append (Blade.Inner_Product (B1, B2, Cont));
               Next (Cursor_2);
            end loop;
         end;
         Next (Cursor_1);
      end loop;

      Simplify (MV);
      return MV;
   end Inner_Product;

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

   function New_Multivector (Scalar_Weight : Float) return Multivector is
      MV : Multivector;
   begin
      MV.Blades.Append (New_Scalar_Blade (Scalar_Weight));
      return  MV;
   end New_Multivector;

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
      while Has_Element (Cursor_1) loop
         B1 := Element (Cursor_1);
         declare
            List_2   : Blade_List := MV2.Blades;
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

      Simplify (MV);
      return MV;
   end Outer_Product;

   --  -------------------------------------------------------------------------

   function Reverse_MV (MV : Multivector) return Multivector is
      use Blade_List_Package;
      use GA_Maths;
      Blades   : Blade_List := MV.Blades;
      B_Cursor : Cursor := Blades.First;
      Rev_MV   : Multivector;
   begin
      while Has_Element (B_Cursor) loop
         Rev_MV.Blades.Append (Blade.Reverse_Blade (Element (B_Cursor)));
         Next (B_Cursor);
      end loop;

      return Rev_MV;
   end Reverse_MV;

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
            Sum := Sum + Weight (BB);
         end if;
         Next (B_Cursor);
      end loop;
      return Sum;
   end Scalar_Part;

   --  -------------------------------------------------------------------------

   function Scalar_Product (MV1, MV2 : Multivector) return float is
   begin
      return Scalar_Part (Inner_Product (MV1, MV2, Left_Contraction));
   end Scalar_Product;

   --  -------------------------------------------------------------------------

   procedure Simplify (MV : in out Multivector) is
   begin
     Simplify (MV.Blades);
   end Simplify;

   --  -------------------------------------------------------------------------

   procedure Simplify (Blades : in out Blade_List) is
      use Blade_List_Package;
      use GA_Maths;
      Current      : Blade.Basis_Blade;
      Previous     : Blade.Basis_Blade;
      Has_Previous : Boolean := False;
      Blade_Cursor : Cursor := Blades.First;
      Remove_Nulls : Boolean := False;

   begin
      while Has_Element (Blade_Cursor) loop
         Current := Element (Blade_Cursor);
         if Weight (Current) = 0.0 then
            Blades.Delete (Blade_Cursor);
            Has_Previous := False;
         elsif Has_Previous and then
           Bitmap (Previous) = Bitmap (Current) then
            Update_Blade (Previous, Weight (Previous) + Weight (Current));
            Blades.Delete (Blade_Cursor);
         else
            if Has_Previous and then Weight (Previous) = 0.0 then
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
            if Weight (Current) = 0.0 then
               Blades.Delete (Blade_Cursor);
            end if;
            Next (Blade_Cursor);
         end loop;
      end if;
   end Simplify;

   --  -------------------------------------------------------------------------

   function Size (MV : Multivector) return Natural is
      use Blade_List_Package;
      Blades  : Blade_List := MV.Blades;
   begin
      return Natural (Blades.Length);
   end Size;

   --  -------------------------------------------------------------------------

   function Top_Grade_Index (MV : Multivector) return GA_Maths.Unsigned_Integer is
      use Blade_List_Package;
      use GA_Maths;
      Max_G        : Integer := 0;
      G            : Integer := 0;
      Blades       : Blade_List := MV.Blades;
      Blade_Cursor : Cursor := Blades.First;
      ThisBlade    : Blade.Basis_Blade;
      Index        : Integer := 0;
   begin
      while Has_Element (Blade_Cursor) loop
         Index := Index +1;
         ThisBlade := Element (Blade_Cursor);
         G := Integer (GA_Maths.Grade (Bitmap (ThisBlade)));
         Max_G := Maximum (Max_G, G);
--           Put_Line ("Top_Grade_Index Index:" & Integer'Image (Index));
--           Put_Line ("Top_Grade_Index Grade_Use:" & Integer'Image (G));
--           Put_Line ("Top_Grade_Index Max_G:" & Integer'Image (Max_G));
         Next (Blade_Cursor);
      end loop;
--         Put_Line ("Top_Grade_Index Max_G:" & Integer'Image (Max_G));
      return Unsigned_Integer (Max_G);
   end Top_Grade_Index;

   --  -------------------------------------------------------------------------

   function Unit_E (MV : Multivector) return Multivector is
   begin
      return Unit_R (MV);
   end Unit_E;

   --  -------------------------------------------------------------------------

   function Unit_R (MV : Multivector) return Multivector is
      use GA_Maths.Float_Functions;
      theNorm  : Float := Scalar_Product (MV, Reverse_MV (MV));
   begin
      if theNorm = 0.0 then
         Put_Line ("Multivector.Unit_R encountered a null multivector");
         raise MV_Exception;
      end if;

      return Geometric_Product (MV, 1.0 / Sqrt (Abs (theNorm)));

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Versor_Inverse.");
         raise;
   end Unit_R;

   --  -------------------------------------------------------------------------

   function Versor_Inverse (MV : Multivector) return Multivector is
      Rev          : Multivector := Reverse_MV (MV);
      S_Product    : Float := 0.0;
   begin
      S_Product := Scalar_Product (MV, Rev);
      if S_Product = 0.0 then
         Put_Line ("Multivector.Versor_Inverse encountered a non-invertible multivector");
         raise MV_Exception;
      end if;

      return Geometric_Product (Rev, 1.0 / S_Product);

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Multivector.Versor_Inverse.");
         raise;
   end Versor_Inverse;

   --  -------------------------------------------------------------------------
begin
   C3_Blade_List.Append (New_Basis_Blade (C3_no));
   C3_Blade_List.Append (New_Basis_Blade (C3_e1));
   C3_Blade_List.Append (New_Basis_Blade (C3_e2));
   C3_Blade_List.Append (New_Basis_Blade (C3_e3));
   C3_Blade_List.Append (New_Basis_Blade (C3_ni));

end Multivector;
