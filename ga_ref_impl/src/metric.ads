
with GA_Maths;

package Metric is

   type Metric_Record (Dim : Integer) is private;
   type Metric_Matrix is array (Integer range <>, Integer range <>) of float;
   type Metric_Data is array (Integer range <>) of float;

   Null_Metric      : constant Metric_Matrix (1 .. 0, 1 .. 0) := (others => (others => 0.0));
   Metric_Exception : Exception;

   function Is_Anti_Euclidean (Met : Metric_Record) return Boolean;
   function Is_Diagonal (Met : Metric_Record) return Boolean;
   function Is_Euclidean (Met : Metric_Record) return Boolean;
   function Matrix (Met : Metric_Record) return Metric_Matrix;
   function Metric_C3 return Metric_Matrix;
   function New_Metric (Dimension : Integer) return Metric_Matrix;
   function New_Metric (Dimension : Integer; Data : Metric_Data) return Metric_Matrix;
   function New_Metric (Met : GA_Maths.Float_Matrix;
                        State : out Metric_Record) return Metric_Matrix;

private
   use GA_Maths.Float_Array_Package;
   type Metric_Record (Dim : Integer) is record
      Matrix          : Metric_Matrix (1 .. Dim, 1 .. Dim);
      Eigen_Metric    : Metric_Matrix (1 .. Dim, 1 .. Dim);
      Diagonal        : Boolean;
      Euclidean       : Boolean := False;
      Anti_Euclidean  : Boolean := False;
      Eigen_Values    : Real_Vector (1 .. Dim);
      Eigen_Vectors   : GA_Maths.Float_Matrix (1 .. Dim, 1 .. Dim);
      Inverse_Vectors : GA_Maths.Float_Matrix (1 .. Dim, 1 .. Dim);
   end record;

end Metric;
