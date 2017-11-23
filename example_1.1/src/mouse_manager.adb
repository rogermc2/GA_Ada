
with E2GA;

package body Mouse_Manager is

   --  mouse position on last call to MouseButton() / MouseMotion()
    Prev_Mouse_Pos : E2GA.Vector;

   --  when true, MouseMotion() will rotate the model
   Rotate_Model              : Boolean := False;
   Rotate_Model_Out_OfPPlane : Boolean := False;
   --  rotation of the model
   M_Model_Rotor             : E2GA.Rotor; --  (_rotor(1.0f));

   --  -------------------------------------------------------------------------

   procedure Mouse_Button (Button : Integer; State : Integer; X, Y : Integer) is

   begin
      null;
   end Mouse_Button;

   --  -------------------------------------------------------------------------

end Mouse_Manager;
