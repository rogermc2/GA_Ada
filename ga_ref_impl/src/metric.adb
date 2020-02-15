
with Ada.Text_IO; use Ada.Text_IO;

package body Metric is

   C3_M : constant GA_Maths.Float_Matrix (1 .. 5, 1 .. 5) :=
            ((0.0, 0.0, 0.0, 0.0, -1.0),
             (0.0, 1.0, 0.0, 0.0, 0.0),
             (0.0, 0.0, 1.0, 0.0, 0.0),
             (0.0, 0.0, 0.0, 1.0, 0.0),
             (-1.0, 0.0, 0.0, 0.0, 0.0));

   Metric_C3 : Metric_Record (5);

   --  --------------------------------------------------------------------

   function C3_Eigen_Matrix return Metric_Matrix is
   begin
      return Metric_C3.Matrix;
   end C3_Eigen_Matrix;

   --  --------------------------------------------------------------------

   function Inverse_Eigen_Matrix (Met : Metric_Record) return Metric_Matrix is
   begin
      return Met.Inverse_Eigen_Matrix;
   end Inverse_Eigen_Matrix;

   --  --------------------------------------------------------------------

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

   function C3_Metric return Metric_Record is
   begin
      return  Metric_C3;
   end C3_Metric;

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

   function New_Metric (Met : GA_Maths.Float_Matrix) return Metric_Record is
      use GA_Maths;
      Eigen_Values    : Real_Vector (Met'Range);  --  m_eigenMetric
      Eigen_Vectors   : Float_Matrix (Met'Range, Met'Range);
      State           : Metric_Record (Met'Length (1));
   begin
      if not Is_Symetric (Met) then
         raise Metric_Exception with
           "Matric.New_Metric cannot process non-symmetric matrices.";
      else
         Eigensystem (Real_Matrix (Met), Eigen_Values, Eigen_Vectors); --  m_eig
         State.Eigen_Metric := Eigen_Values;
         State.Eigen_Vectors := Metric_Matrix (Eigen_Vectors);
         State.Inverse_Eigen_Matrix := Metric_Matrix (Transpose (Eigen_Vectors));
         For row in Eigen_Values'Range loop
            State.Matrix (row, row) := Eigen_Values (row);
         end loop;

         State.Diagonal := Is_Diagonal (Met);
         if not State.Diagonal then
            State.Euclidean := False;
            State.Anti_Euclidean := False;
         else
            State.Euclidean := True;
            State.Anti_Euclidean := True;
            For col in Met'Range (2) loop
               if Met (col, col) /= 1.0 then
                  State.Euclidean := False;
               end if;
               if Met (col, col) /= -1.0 then
                  State.Anti_Euclidean := False;
               end if;
            end loop;
         end if;
      end if;

      return State;

   exception
      when others =>
         Put_Line ("An exception occurred in Metric.New_Metric 4");
         raise;
   end New_Metric;

begin

   Metric_C3 := New_Metric (C3_M);

end Metric;
