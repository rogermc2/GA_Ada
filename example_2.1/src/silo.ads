
with Ada.Strings.Unbounded;

with GL.Types;

package Silo is

   type Label_Data is record
      Label_String      : Ada.Strings.Unbounded.Unbounded_String;
      Label_Position    : GL.Types.Singles.Vector2;
   end record;

   Empty_Stack : EXception;

   procedure Push (Data : Label_Data);
   function Pull return Label_Data;
   function Size return Integer;

end Silo;
