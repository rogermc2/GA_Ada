
with Ada.Text_IO; use Ada.Text_IO;

package body Metric is

   type Metric_Array is array (Integer range <>) of float;

   function New_Metric (Dimension : Integer) return Metric is
      theMetric : Metric (1 .. Dimension, 1 .. Dimension) :=
      (others => (others => 0.0));
   begin
      return theMetric;
   end New_Metric;

   --  ------------------------------------------------------------------------

   function New_Metric (Dimension : Integer; Data : Metric_Data) return Metric is
      theMetric : Metric (1 .. Dimension, 1 .. Dimension) :=
      (others => (others => 0.0));
   begin
      for Index in 1 .. Dimension loop
         theMetric (Index, Index) := Data (Index);
      end loop;
      return theMetric;
   end New_Metric;

   --  ------------------------------------------------------------------------

end Metric;
