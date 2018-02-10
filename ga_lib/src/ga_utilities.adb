
with Interfaces;

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Multivectors;
with Multivector_Type_Base;

package body GA_Utilities is

   --  ------------------------------------------------------------------------

   function Factorize_Blade (MV : Multivectors.Multivector; Scale : out Scale_Array)
                             return Multivectors.Multivector_List is
      use GA_Maths;
      use Multivectors;
      use Blade_List_Package;
      MV_Info  : Multivector_Type.MV_Type_Record := Multivector_Type.Init (MV);
      k        : Unsigned_Integer := Multivector_Type.Top_Grade (MV_Info);
      E        : Blade.Basis_Blade := Largest_Basis_Blade (MV);
      Blades   : Blade_List;
      B_Cursor : Cursor;
      Index    : Integer := 0;
      Dim      : Integer := Space_Dimension (MV);
      Current  : Multivector;
      MV_2     : Multivector;
      Result   : Multivectors.Multivector_List;
   begin
      if k = 0 then
         Scale (1) := Scalar_Part (MV);
      else
         Scale (1) := Norm_E (MV);
      end if;
      if Scale (1) /= 0.0 and k /= 0 then
         Blades.Append (Blade.New_Basis_Blade (k));
         for g in 0 .. Dim - 1 loop
            if (Blade.Bitmap (E) and 2 ** g) /= 0 then
                Blades.Append (Blade.New_Basis_Blade (2 ** g, 1.0));
            end if;
         end loop;
         Current := Geometric_Product (MV, 1.0 / Scale (1));
         Add_Multivector (Result, New_Multivector (Float (K)));
         B_Cursor := Blades.First;
         while Has_Element (B_Cursor) loop
            MV_2 := New_Multivector (Blade.Weight (Element (B_Cursor)));
            MV_2 := Inner_Product (MV_2, Current, Blade.Left_Contraction);
            MV_2 := Inner_Product (MV_2, Current, Blade.Left_Contraction);
            MV_2 := Unit_E (MV_2);
            Add_Multivector (Result, MV_2);
            Current := Left_Contraction (MV_2, Current);
            Next (B_Cursor);
         end loop;
         Add_Multivector (Result, Unit_E (Current));
      end if;
      return Result;
   end Factorize_Blade;

   --  -------------------------------------------------------------------------

   procedure Print_Matrix (Name : String; aMatrix : GA_Maths.GA_Matrix3) is
   begin
      Put_Line (Name & ":");
      for Row in 1 .. 3 loop
         for Column in  1 .. 3 loop
            Put (float'Image (aMatrix (Row, Column)) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix (Name : String; aMatrix : Real_Matrix) is
      use GA_Maths;
   begin
      Put_Line (Name & ":");
      Put_Line ("Size:" & Integer'Image (aMatrix'Length) & " X"
                  & Integer'Image (aMatrix'Length(2)));
      for Row in aMatrix'Range(1) loop
         for Column in aMatrix'Range(2) loop
            Put (Float_3'Image (Float_3 (aMatrix (Row, Column))) & "   ");
         end loop;
         New_Line;
         New_Line;
      end loop;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Multivector (Name : String; MV : Multivectors.Multivector) is
      use Multivectors;
      use Blade_List_Package;
      theBlades : constant Blade_List := Blades (MV);
      aBlade    : Blade.Basis_Blade;
      Curs      : Cursor := theBlades.First;
   begin
      New_Line;
      Put_Line (Name);
      Put_Line ("MV Size: " & Ada.Containers.Count_Type'Image (theBlades.Length));
      Put_Line ("Grade Use Bitmap: " & GA_Maths.Grade_Usage'Image (Grade_Use (MV)));
      Put_Line ("Multivector Blades, Bitmap and Weight:");
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Put_Line (GA_Maths.Unsigned_Integer'Image (Blade.Bitmap (aBlade)) &
                   "  " & float'Image (Blade.Weight (aBlade)));
         Next (Curs);
      end loop;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Multivector.");
         raise;
   end Print_Multivector;

   --  ------------------------------------------------------------------------

   procedure Print_Multivector_Info (Name : String; Info : Multivector_Type.MV_Type_Record) is
      use Multivector_Type;
   begin
      Put_Line (Name);
      Put_Line ("Zero        " & boolean'Image (Zero (Info)));
      Put_Line ("MV Type     " & MV_Type'Image (MV_Kind (Info)));
      Put_Line ("Top_Grade   " & GA_Maths.Unsigned_Integer'Image (Top_Grade (Info)));
      Put_Line ("Grade use   " & GA_Maths.Unsigned_Integer'Image (Grade_Use (Info)));
      Put_Line ("Parity      " & Parity_Type'Image (Parity (Info)));
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Multivector_Info.");
         raise;
   end Print_Multivector_Info;

   --  ------------------------------------------------------------------------

end GA_Utilities;
