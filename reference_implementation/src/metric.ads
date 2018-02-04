
package Metric is

   type Metric is array (Integer range <>, Integer range <>) of float;
   type Metric_Data is array (Integer range <>) of float;

   Null_Metric : constant Metric (1 .. 0, 1 .. 0) := (others => (others => 0.0));

   function New_Metric (Dimension : Integer) return Metric;
   function New_Metric (Dimension : Integer; Data : Metric_Data) return Metric;

end Metric;
