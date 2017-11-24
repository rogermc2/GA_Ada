
with Ada.Containers.Doubly_Linked_Lists;

package body Silo is
   use Ada.Containers;

   package Label_Container is new Doubly_Linked_Lists (Label_Data);
   type Label_List is new Label_Container.List with null record;

   theStack : Label_List;

   procedure Push (Data : Label_Data) is
   begin
      Append (theStack, Data);
   end Push;

   --  -----------------------------------------------------------------

   function Pull return Label_Data is
      Data : Label_Data;
   begin
      if Is_Empty (theStack) then
         raise Empty_Stack;
      end if;

      Data := First_Element (theStack);
      Delete_First (theStack);
      return Data;
   end Pull;

   --  -----------------------------------------------------------------

   function Set_Data (Label_String : Unbounded_String;
                      Label_Position : GL.Types.Singles.Vector2)
                      return Label_Data is
   begin
       return  (Label_String, Label_Position);
   end Set_Data;

   --  -----------------------------------------------------------------

   procedure Get_Data (Label_String  : out Unbounded_String;
                       Label_Position : out GL.Types.Singles.Vector2) is
      theData : Label_Data := Pull;
   begin
       Label_String:= theData.Label_String;
       Label_Position := theData.Label_Position;
   end Get_Data;

   --  -----------------------------------------------------------------
   function Size return Integer is
   begin
      return Integer (Length (theStack));
   end Size;

   --  -----------------------------------------------------------------

end Silo;
