
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with GA_Maths;

package body C3GA_Utilities is
   NI : constant float := 1.0;

--     function C3GA_Point (V : C3GA.Vector_E3GA) return C3GA.Normalized_Point is
--        Norm   : constant C3GA.Scalar := C3GA.Norm_E2 (V);
--        Offset : constant float := 0.5 * C3GA.Coord (Norm) * NI;
--        thePoint : C3GA.Normalized_Point;
--     begin
--        thePoint := C3GA.Set_Normalized_Point (C3GA.Get_Coord_1 (V) + Offset,
--                         C3GA.Get_Coord_2 (V) + Offset,
--                         C3GA.Get_Coord_3 (V) + Offset, NI);
--        return thePoint;
--     end C3GA_Point;

   --  -------------------------------------------------------------------------

   procedure Print_Multivector (Name : String; MV : Multivector.Multivector) is
      use Multivector.Blade_List_Package;
      use GA_Maths;
      Blades    : constant Multivector.Blade_List := Multivector.Blades (MV);
      Curs      : Cursor := Blades.First;
      BB        : Blade.Basis_Blade;
      Index     : Integer := 0;
      New_MV    : Multivector.Multivector;
   begin
      New_Line;

      Put ("Multivector " & Name);
      If Is_Empty (List (Blades)) then
         Put (" is a null multivector.");
      end if;
      New_Line;

      while Has_Element (Curs) loop
         Index := Index + 1;
         BB := Element (Curs);
         Put_Line ("Blade" & Integer'Image (Index) &  ":  Bitmap, Weight" &
                   Unsigned_Integer'Image (Blade.Bitmap (BB)) &
                   ", " & Float'Image (Blade.Weight (BB)));
         Next (Curs);
      end loop;
   end Print_Multivector;

   --  -------------------------------------------------------------------------

--     procedure Print_Vector (Name : String; NP : C3GA.Normalized_Point) is
--        Coords : GA_Maths.Coords_Continuous_Array (1 .. 4) := C3GA.Get_Coords (NP);
--     begin
--        Put (Name & ":  ");
--        for Index in Coords'Range loop
--           Put (float'Image (Coords (Index)) & "   ");
--        end loop;
--        New_Line;
--     end Print_Vector;

   --  -------------------------------------------------------------------------

--     procedure Print_Vector (Name : String; aVector : C3GA.Vector_E3GA) is
--        Coords : GA_Maths.Array_3D := C3GA.Get_Coords (aVector);
--     begin
--        Put (Name & ":  ");
--        for Index in Coords'Range loop
--           Put (float'Image (Coords (Index)) & "   ");
--        end loop;
--        New_Line;
--     end Print_Vector;
--
   --  ------------------------------------------------------------------------

end C3GA_Utilities;
