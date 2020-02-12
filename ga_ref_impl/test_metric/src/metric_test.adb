
with Ada.Text_IO; use Ada.Text_IO;

with GA_Utilities;
with Metric;

package body Metric_Test is

   --  --------------------------------------------------------------------
   --  Based on Metric.java main
   procedure Test is
      M_Matrix : constant Metric.Metric_Matrix (1 .. 3, 1 .. 3) :=
                       ((1.0, 0.0, 0.0),
                        (0.0, 0.0, -1.0),
                        (0.0, -1.0, 0.0));
      Met      : Metric.Metric_Record := Metric.New_Metric (M_Matrix);
   begin
    GA_Utilities.Print_Metric ("Metric_Test.Test", Met);

   exception
      when others =>
         Put_Line ("An exception occurred in Metric_Test.Test.");
         raise;

   end Test;

   --  --------------------------------------------------------------------

end Metric_Test;
