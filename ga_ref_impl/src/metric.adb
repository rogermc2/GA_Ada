
with Ada.Text_IO; use Ada.Text_IO;

package body Metric is

   C3_M : constant GA_Maths.Float_Matrix (1 .. 5, 1 .. 5) :=
            ((0.0, 0.0, 0.0, 0.0, -1.0),
             (0.0, 1.0, 0.0, 0.0, 0.0),
             (0.0, 0.0, 1.0, 0.0, 0.0),
             (0.0, 0.0, 0.0, 1.0, 0.0),
             (-1.0, 0.0, 0.0, 0.0, 0.0));

   E3_M : constant GA_Maths.Float_Matrix (1 .. 3, 1 .. 3) :=
            ((1.0, 0.0, 0.0),
             (0.0, 1.0, 0.0),
             (0.0, 0.0, 1.0));

   Metric_C3 : Metric_Record (C3_M'Length(1));
   Metric_E3 : Metric_Record (E3_M'Length(1));

   --  --------------------------------------------------------------------

   function C3_Eigen_Matrix return Metric_Matrix is
   begin
      return Metric_C3.Matrix;
   end C3_Eigen_Matrix;

   --  --------------------------------------------------------------------

   function C3_Inverse_Eigen_Matrix return Metric_Matrix is
   begin
      return Metric_C3.Inverse_Eigen_Matrix;
   end C3_Inverse_Eigen_Matrix;

   --  --------------------------------------------------------------------

   function E3_Eigen_Matrix return Metric_Matrix is
   begin
      return Metric_E3.Matrix;
   end E3_Eigen_Matrix;

   --  --------------------------------------------------------------------

   function E3_Inverse_Eigen_Matrix return Metric_Matrix is
   begin
      return Metric_E3.Inverse_Eigen_Matrix;
   end E3_Inverse_Eigen_Matrix;

   --  --------------------------------------------------------------------

   function Eigen_Values (Met : Metric_Record) return Real_Vector is
   begin
      return Met.Eigen_Values;
   end Eigen_Values;

   --  --------------------------------------------------------------------

   function Eigen_Vectors (Met : Metric_Record) return Metric_Matrix is
   begin
      return Met.Eigen_Vectors;
   end Eigen_Vectors;

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

   function E3_Metric return Metric_Record is
   begin
      return  Metric_E3;
   end E3_Metric;

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
      Values    : Real_Vector (Met'Range);  --  m_eigenMetric
      Vectors   : Float_Matrix (Met'Range, Met'Range);
      State     : Metric_Record (Met'Length (1));
   begin
      if not Is_Symetric (Met) then
         raise Metric_Exception with
           "Metric.New_Metric cannot process non-symmetric matrices.";
      else
         Eigensystem (Real_Matrix (Met), Values, Vectors); --  m_eig
         State.Eigen_Values := Values;
         State.Eigen_Vectors := Metric_Matrix (Vectors);
         State.Inverse_Eigen_Matrix := Metric_Matrix (Transpose (Vectors));
--           For row in Values'Range loop
--              State.Matrix (row, row) := Values (row);
--           end loop;
         State.Matrix := Met;

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
   Metric_E3 := New_Metric (E3_M);

end Metric;
