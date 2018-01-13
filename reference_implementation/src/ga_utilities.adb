
with Interfaces;

with Ada.Containers;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with Multivector_Type_Base;

package body GA_Utilities is

   --  -------------------------------------------------------------------------

   procedure Print_Analysis (Name : String;
                             Info : Multivector_Analyze.MV_Analysis) is
      use Multivector_Analyze;
      use Multivector_Type_Base;
   begin
      Put_Line (Name);
      Put_Line ("Valid Flag    " & boolean'Image (Info.M_Flags.Valid));
      Put_Line ("Dual Flag     " & boolean'Image (Info.M_Flags.Dual));
      Print_Multivector_Info (Name & " M_MV_Type data", Info.M_MV_Type);
      Put_Line ("Conformal Type     " & Conformal_Type'Image (Info.Conformal_Kind));
      Put_Line ("Epsilon    " & Float'Image (Info.Epsilon));
      Put_Line ("Pseudo_Scalar    " & boolean'Image (Info.Pseudo_Scalar));
      Put_Line ("Versor_Kind    " & Versor_Type'Image (Info.Versor_Kind));
      Put_Line ("Pseudo_Scalar    " & boolean'Image (Info.Pseudo_Scalar));
      Put_Line ("Points array length    " & integer'Image (Info.M_Points'Length));
      Put_Line ("Scalars array length    " & integer'Image (Info.M_Scalors'Length));
      Put_Line ("Vectors array length    " & integer'Image (Info.M_Vectors'Length));
      New_Line;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in GA_Utilities.Print_Analysis.");
         raise;
   end Print_Analysis;

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

   procedure Print_Multivector (Name : String; MV : Multivector.Multivector) is
      use Multivector;
      use Blade_List_Package;
      theBlades : constant Blade_List := Blades (MV);
      aBlade    : Blade.Basis_Blade;
      Curs      : Cursor := theBlades.First;
   begin
      New_Line;
      Put_Line (Name);
      Put_Line ("MV Size: " & Ada.Containers.Count_Type'Image (theBlades.Length));
      Put_Line ("Grade Use: " & GA_Maths.Grade_Usage'Image (Grade_Use (MV)));
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
