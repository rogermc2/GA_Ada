--  Program Circle
--  Author Roger Mc Murtrie
--  Created 21 February 2018

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;

with Main_Loop;

procedure Removal is
    Main_Window : Glfw.Windows.Window;
    Window_Title : String := "GA for Computer Scientists Chapter 2, Example 2: Hidden Surface Removal";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Removal returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Removal.");
        Put_Line (Exception_Information (anError));

end Removal;
