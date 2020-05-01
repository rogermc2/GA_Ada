
with Ada.Text_IO; use Ada.Text_IO;

package body GA_Utilities is

   function Multivector_Size (MV : Multivectors.Multivector) return Integer is
      theBlades : constant Blade.Blade_List := Multivectors.Blades (MV);
   begin
      return Integer (theBlades.Length);
   end Multivector_Size;

   --  ------------------------------------------------------------------------

   procedure Print_Bitmap (Name : String; Bitmap : Interfaces.Unsigned_32) is
      use Interfaces;
      BM         : Unsigned_32 := Bitmap;
      Bit_String : String (1 .. 32) := (others => '0');
   begin
      New_Line;
      Put_Line (Name & " Bitmap:");
      for index in 1 .. 32 loop
         if (BM and 1) /= 0 then
            Bit_String (33 - index) := '1';
         end if;
         BM := BM / 2;
      end loop;

      Put_Line (Bit_String);
   end Print_Bitmap;

   --  ------------------------------------------------------------------------

   procedure Print_Blade (Name : String; B : Blade.Basis_Blade) is
   begin
      New_Line;
      Put_Line (Name);
      Put_Line (" Bitmap and Weight:");
      Put_Line (Interfaces.Unsigned_32'Image (Blade.Bitmap (B)) &
                  "  " & Float'Image (Blade.Weight (B)));
      New_Line;
   end Print_Blade;

   --  ------------------------------------------------------------------------

   procedure Print_Blade_List (Name : String; BL : Blade.Blade_List) is
      use Blade;
      use Blade_List_Package;
      aBlade    : Basis_Blade;
      Curs      : Cursor := BL.First;
   begin
      New_Line;
      Put_Line (Name);
      Put_Line ("Blades, Bitmap and Weight:");
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Put_Line (Interfaces.Unsigned_32'Image (Bitmap (aBlade)) &
                     "  " & Float'Image (Weight (aBlade)));
         Next (Curs);
      end loop;
      New_Line;

   exception
      when others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Blade_List.");
         raise;
   end Print_Blade_List;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Array (Name : String; anArray : GA_Maths.Float_Vector) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put (Float'Image (anArray (Index)) & " ");
         if Index mod 6 = 0 then
            New_Line;
         end if;
      end loop;
      New_Line;
   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array (Name : String; anArray : GA_Maths.Integer_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put (Integer'Image (anArray (Index)) & " ");
         if Index mod 3 = 0 then
            New_Line;
         end if;
      end loop;
      New_Line;
   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

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
      if Name /= "" then
         Put_Line (Name & ":");
      end if;
      Put_Line ("Size:" & Integer'Image (aMatrix'Length) & " X"
                & Integer'Image (aMatrix'Length (2)));
      for Row in aMatrix'Range (1) loop
         for Column in aMatrix'Range (2) loop
            Put (Float_3'Image (Float_3 (aMatrix (Row, Column))) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix (Name        : String; aMatrix : Real_Matrix;
                           Start, Last : GA_Maths.Array_I2) is
      use GA_Maths;
      L_Row : constant Integer := Minimum (aMatrix'Length(1), Last (1));
      L_Col : constant Integer := Minimum (aMatrix'Length(2), Last (2));
   begin
      if Name /= "" then
         Put_Line (Name & ":");
      end if;
      Put_Line ("Size:" & Integer'Image (aMatrix'Length (1)) & " X"
                & Integer'Image (aMatrix'Length (2)));
      Put ("Rows:" & Integer'Image (Start (1)) & " .."
                & Integer'Image (L_Row));
      Put_Line ("    Columns:" & Integer'Image (Start (2)) & " .."
                & Integer'Image (L_Col));
      for Row in Start (1) .. L_Row loop
         for Column in Start (2) .. L_Col loop
            Put (Float_3'Image (Float_3 (aMatrix (Row, Column))) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Metric (Name : String; aMetric : Metric.Metric_Record) is
      use Metric;
      Dim : constant Integer := aMetric.Dim;
   begin
      New_Line;
      Put_Line (Name);
      Put_Line ("Dimension: " & Integer'Image (Dim));
      Print_Matrix ("", Real_Matrix ((Matrix (aMetric))));
      Put_Line ("Is_Diagonal: " & Boolean'Image (Is_Diagonal (aMetric)));
      Put_Line ("Is_Euclidean: " & Boolean'Image (Is_Euclidean (aMetric)));
      Put_Line ("Is_Anti_Euclidean: " & Boolean'Image (Is_Anti_Euclidean (aMetric)));
      New_Line;
   end Print_Metric;

   --  ------------------------------------------------------------------------

   procedure Print_Multivector (Name : String; MV : Multivectors.Multivector) is
      use Blade;
      use Multivectors;
      use Blade_List_Package;
      theBlades : constant Blade_List := Blades (MV);
      aBlade    : Blade.Basis_Blade;
      Curs      : Cursor := theBlades.First;
   begin
      New_Line;
      Put_Line (Name);
      Put_Line ("MV Type: " & MV_Type'Image (MV_Kind (MV)));
      Put_Line ("MV Size: " & Integer'Image (Multivector_Size (MV)));
      Put_Line ("Grade Use Bitmap: " & GA_Maths.Grade_Usage'Image (Grade_Use (MV)));
      Put_Line ("Multivector Blades, Bitmap and Weight:");
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Put_Line (Interfaces.Unsigned_32'Image (Blade.Bitmap (aBlade)) &
                     "  " & Float'Image (Blade.Weight (aBlade)));
         Next (Curs);
      end loop;
      New_Line;

   exception
      when others =>
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
      Put_Line ("Top_Grade   " & Interfaces.Unsigned_32'Image (Top_Grade (Info)));
      Put_Line ("Grade use   " & Interfaces.Unsigned_32'Image (Grade_Use (Info)));
      Put_Line ("Parity      " & Parity_Type'Image (Parity (Info)));
   exception
      when others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Multivector_Info.");
         raise;
   end Print_Multivector_Info;

   --  ------------------------------------------------------------------------

   procedure Print_Vertex (Name : String; Vertex : Multivectors.Vector) is
      use Blade;
      use Multivectors;
      use Blade_List_Package;
      theBlades : constant Blade_List := Blades (Vertex);
      aBlade    : Blade.Basis_Blade;
      Curs      : Cursor := theBlades.First;
   begin
      Put (Name & ":  ");
      while Has_Element (Curs) loop
         aBlade := Element (Curs);
         Put (Float'Image (Blade.Weight (aBlade)) & " ");
         Next (Curs);
      end loop;
      New_Line;

   exception
      when others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Vertex.");
         raise;
   end Print_Vertex;

   --  ------------------------------------------------------------------------

end GA_Utilities;
