
with Ada.Text_IO; use Ada.Text_IO;

package body Metric is

   function Is_Anti_Euclidean (Met : Metric_Record) return Boolean is
   begin
      return Met.Anti_Euclidean;
   end Is_Anti_Euclidean;

   --  --------------------------------------------------------------------

   function Is_Diagonal (Met : Metric_Record) return Boolean is
   begin
      return Met.Diagonal;
   end Is_Diagonal;

   --  --------------------------------------------------------------------

   function Is_Euclidean (Met : Metric_Record) return Boolean is
   begin
      return Met.Euclidean;
   end Is_Euclidean;

   --  --------------------------------------------------------------------
   function Matrix (Met : Metric_Record) return Metric_Matrix is
   begin
      return Met.Matrix;
   end Matrix;

   --  --------------------------------------------------------------------

   function Metric_C3 return Metric_Matrix is
      C3_M : constant Metric_Matrix (1 .. 5, 1 .. 5) :=
               ((0.0, 0.0, 0.0, 0.0, -1.0),
                (0.0, 1.0, 0.0, 0.0, 0.0),
                (0.0, 0.0, 1.0, 0.0, 0.0),
                (0.0, 0.0, 0.0, 1.0, 0.0),
                (-1.0, 0.0, 0.0, 0.0, 0.0));
   begin
      return  C3_M;
   end Metric_C3;

   --  --------------------------------------------------------------------

   function New_Metric (Dimension : Integer) return Metric_Matrix is
      theMetric : constant Metric_Matrix (1 .. Dimension, 1 .. Dimension) :=
                    (others => (others => 0.0));
   begin
      return theMetric;
   end New_Metric;

   --  ------------------------------------------------------------------------

   function New_Metric (Dimension : Integer; Data : Metric_Data)
                        return Metric_Matrix is
      theMetric : Metric_Matrix (1 .. Dimension, 1 .. Dimension) :=
                    (others => (others => 0.0));
   begin
      for Index in 1 .. Dimension loop
         theMetric (Index, Index) := Data (Index);
      end loop;
      return theMetric;
   end New_Metric;

   --  ------------------------------------------------------------------------

   function New_Metric (Met : Metric_Matrix) return Metric_Record is
      use GA_Maths;
      use Float_Array_Package;
      Eigen_Values  : Real_Vector (Met'Range);
      Eigin_Vectors : Float_Matrix (Met'Range, Met'Range);
      theMetric     : Metric_Record (Met'Last - Met'First + 1);
   begin
      for row in Met'Range loop
         for col in Met'Range (2) loop
            theMetric.Matrix (row, col) := Met (row, col);
         end loop;
      end loop;

      Eigensystem (Real_Matrix (theMetric.Matrix), Eigen_Values, Eigin_Vectors);
      theMetric.Eigen_Metric := Eigen_Values;
      theMetric.Anti_Euclidean := Is_Anti_Euclidean (Real_Matrix (theMetric.Matrix));
      theMetric.Diagonal := Is_Diagonal (Real_Matrix (theMetric.Matrix));
      theMetric.Euclidean := Is_Euclidean (Real_Matrix (theMetric.Matrix));

      return theMetric;

   exception
      when others =>
         Put_Line ("An exception occurred in Metric.New_Metric 3");
         raise;

   end New_Metric;

   --  ------------------------------------------------------------------------

end Metric;
