
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types;

package Silo is

   type Label_Data is private;

   Empty_Stack : Exception;

   procedure Get_Data (Label_String   : out Unbounded_String;
                       Label_Position : out GL.Types.Singles.Vector2);
   procedure Push (Data : Label_Data);
   function Pull return Label_Data;
   function Set_Data (Label_String : Unbounded_String;
                       Label_Position : GL.Types.Singles.Vector2)
                       return Label_Data;
   function Size return Integer;

private
   type Label_Data is record
      Label_String      : Unbounded_String;
      Label_Position    : GL.Types.Singles.Vector2;
   end record;

end Silo;
