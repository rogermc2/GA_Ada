
with GA_Maths;

package Metric is
   use GA_Maths.Float_Array_Package;

   type Metric_Record (Dim : Integer) is private;
   subtype Metric_Matrix is GA_Maths.Float_Matrix;
   type Metric_Data is array (Integer range <>) of float;

   Null_Metric      : constant Metric_Matrix (1 .. 0, 1 .. 0) := (others => (others => 0.0));
   Metric_Exception : Exception;

   function C3_Eigen_Matrix return Metric_Matrix;
   function C3_Inverse_Eigen_Matrix return Metric_Matrix;
   function E3_Eigen_Matrix return Metric_Matrix;
   function E3_Inverse_Eigen_Matrix return Metric_Matrix;
   function Inverse_Eigen_Matrix (Met : Metric_Record) return Metric_Matrix;
   function Eigen_Values (Met : Metric_Record) return Real_Vector;
   function Is_Anti_Euclidean (Met : Metric_Record) return Boolean;
   function Is_Diagonal (Met : Metric_Record) return Boolean;
   function Is_Euclidean (Met : Metric_Record) return Boolean;
   function Matrix (Met : Metric_Record) return Metric_Matrix;
   function C3_Metric return Metric_Record;
   function E3_Metric return Metric_Record;
   function New_Metric (Dimension : Integer) return Metric_Matrix;
   function New_Metric (Dimension : Integer; Data : Metric_Data) return Metric_Matrix;
   function New_Metric (Met : Metric_Matrix) return Metric_Record;

private
   type Metric_Record (Dim : Integer) is record
      Matrix               : Metric_Matrix (1 .. Dim, 1 .. Dim) :=
                               (others => (others => 0.0));
      Diagonal             : Boolean := False;
      Euclidean            : Boolean := False;
      Anti_Euclidean       : Boolean := False;
      Eigen_Values         : Real_Vector (1 .. Dim);  --  m_eigenMetric
      Eigen_Vectors        : Metric_Matrix (1 .. Dim, 1 .. Dim);  --  m_eig.getV()
      Inverse_Eigen_Matrix : Metric_Matrix (1 .. Dim, 1 .. Dim);  --  m_invEigMatrix
   end record;

end Metric;
