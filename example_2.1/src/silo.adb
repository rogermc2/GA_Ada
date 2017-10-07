
with Ada.Containers.Doubly_Linked_Lists;

package body Silo is
   use Ada.Containers;

   package Label_Container is new Doubly_Linked_Lists (Label_Data);
   use Label_Container;

   theStack : List;

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

   function Size return Integer is
   begin
      return Integer (Length (theStack));
   end Size;

   --  -----------------------------------------------------------------

end Silo;
