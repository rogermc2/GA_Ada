
with Ada.Text_IO; use Ada.Text_IO;

with Metric;

with Inner_Product_Types;

procedure Test_Inner_Product_Types is
begin

   Inner_Product_Types.Test;

   exception
      when anError :  others =>
         Put_Line ("An exception occurred in Test_Inner_Product_Types.");
      raise;
end Test_Inner_Product_Types;
