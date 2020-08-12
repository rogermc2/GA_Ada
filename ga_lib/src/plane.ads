
with GL.Objects.Programs;

with C3GA;

package Plane is

    procedure Draw_Plane (Render_Program : GL.Objects.Programs.Program;
                          Point, X_Dir, Y_Dir, Normal : C3GA.Vector_E3;
                          Weight : Float := 1.0);
end Plane;
