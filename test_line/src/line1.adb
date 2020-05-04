
with Ada.Text_IO; use Ada.Text_IO;

with C3GA;
with GA_Utilities;
with Multivectors; use Multivectors;

procedure Line1 is
    Point1 : constant Normalized_Point := C3GA.Set_Normalized_Point (0.0, 1.0, 0.0);
    Point2 : constant Normalized_Point := C3GA.Set_Normalized_Point (1.0, 0.0, 0.0);
    Point3 : constant Normalized_Point := C3GA.Set_Normalized_Point (-2.0, 0.0, 0.0);
    Point4 : constant Normalized_Point := C3GA.Set_Normalized_Point (0.0, 0.0, 987.789);
    Point5 : constant Normalized_Point := C3GA.Set_Normalized_Point (0.068, 1190.0, -10.567);

    Line1 : constant Multivectors.Line := C3GA.Set_Line (Point1, Point2);
    Line2 : constant Multivectors.Line := C3GA.Set_Line (Point2, Point3);
    Line3 : constant Multivectors.Line := C3GA.Set_Line (Point4, Point5);
begin
    GA_Utilities.Print_Multivector ("line 1", Line1);
    GA_Utilities.Print_Multivector ("line 2", Line2);
   GA_Utilities.Print_Multivector ("line 3", Line3);
   Put_Line (Float'Image (5.77350E-01**2 + 5.77350E-01**2 + 5.77350E-01**2));
end Line1;
