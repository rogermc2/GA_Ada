
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

   function New_Metric (Met   : GA_Maths.Float_Matrix;
                        State : out Metric_Record) return Metric_Matrix is
      use GA_Maths;
      Eigen_Values    : Real_Vector (Met'Range);
      Eigen_Vectors   : Float_Matrix (Met'Range, Met'Range);
      theMetric       : Metric_Matrix (Met'Range (1), Met'Range (2)) :=
                          (others => (others => 0.0));
   begin
      if not Is_Symetric (Met) then
         raise Metric_Exception with
           "Matric.New_Metric cannot process non-symmetric matrices.";
      else
         State.Matrix := Metric_Matrix (Met);
         Eigensystem (Real_Matrix (Met), Eigen_Values, Eigen_Vectors);
         State.Inverse_Vectors := Transpose (Eigen_Vectors);
         State.Eigen_Values := Eigen_Values;
         State.Eigen_Vectors := Eigen_Vectors;
         For row in Eigen_Values'Range loop
            theMetric (row, row) := Eigen_Values (row);
         end loop;
         State.Diagonal := Is_Diagonal (Met);
         if State.Diagonal then
            State.Anti_Euclidean := True;
            For col in Met'Range (2) loop
               if Met (col, col) /= 1.0 then
                  State.Euclidean := False;
               end if;
            end loop;
         end if;
      end if;
      State.Eigen_Metric := theMetric;
      return theMetric;

   exception
      when others =>
         Put_Line ("An exception occurred in Metric.New_Metric 4");
         raise;
   end New_Metric;

end Metric;
