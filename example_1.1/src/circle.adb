--  Program Circle
--  Author Roger Mc Murtrie
--  Created 16 November 2017

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;

with Main_Loop;

procedure Circle is
    Main_Window : Glfw.Windows.Window;
    Window_Title : String := "GA for Computer Scientists Example 1.1 An Example in Geometric Algebra";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Circle returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Circle.");
        Put_Line (Exception_Information (anError));

end Circle;
