
with Ada.Exceptions;

with GA_Maths;

package Metric is

   type Metric_Record (Dim : Integer) is private;
   type Metric_Matrix is array (Integer range <>, Integer range <>) of float;
   type Metric_Data is array (Integer range <>) of float;

   Null_Metric      : constant Metric_Matrix (1 .. 0, 1 .. 0) := (others => (others => 0.0));
   Metric_Exception : Ada.Exceptions.Exception_Id;
   Metric_C3        : constant Metric_Matrix (1 .. 5, 1 .. 5) :=
                         (1 => (5  => -1.0, others => 0.0),
                          2 => (2  => 1.0, others => 0.0),
                          3 => (3  => 1.0, others => 0.0),
                          4 => (4  => 1.0, others => 0.0),
                          5 => (1  => -1.0, others => 0.0));

   function Matrix (Met : Metric_Record) return GA_Maths.Float_Matrix;
   function New_Metric (Dimension : Integer) return Metric_Matrix;
   function New_Metric (Dimension : Integer; Data : Metric_Data) return Metric_Matrix;
   function New_Metric (Met : Metric_Matrix) return Metric_Record;

private
   type Metric_Record (Dim : Integer) is record
      Matrix          : GA_Maths.Float_Matrix (1 .. Dim, 1 .. Dim);
      Eigen_Metric    : GA_Maths.Float_Array_Package.Real_Vector (1 .. Dim);
      Diagonal        : Boolean;
      Euclidean       : Boolean := False;
      Anti_Euclidean  : Boolean := False;
   end record;

end Metric;
