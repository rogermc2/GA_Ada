
with C3GA;
with GA_Utilities;
with Multivectors; use Multivectors;

procedure Line1 is
    Point1 : constant Normalized_Point := C3GA.Set_Normalized_Point (0.0, 0.0, 0.0);
    Point2 : constant Normalized_Point := C3GA.Set_Normalized_Point (1.0, 0.0, 0.0);

    theLine : constant Line := C3GA.Set_Line (Point1, Point2);
begin
    GA_Utilities.Print_Multivector ("the line", theLine);
    null;
end Line1;
