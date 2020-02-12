
with Blade;
with GA_Maths;
with Metric;
with Multivectors;
with Multivector_Type;

package GA_Utilities is
   use GA_Maths.Float_Array_Package;

   function Factorize_Blade (MV : Multivectors.Multivector; Scale : out Float)
                             return Multivectors.Multivector_List;
   function Multivector_Size (MV : Multivectors.Multivector) return Integer;
   procedure Print_Blade_List (Name : String; BL : Blade.Blade_List);
   procedure Print_Integer_Array (Name : String; anArray : GA_Maths.Integer_Array);
   procedure Print_Matrix (Name : String; aMatrix : GA_Maths.GA_Matrix3);
   procedure Print_Matrix (Name : String; aMatrix : Real_Matrix);
   procedure Print_Metric (Name : String; aMetric : Metric.Metric_Record);
   procedure Print_Multivector (Name : String; MV : Multivectors.Multivector);
   procedure Print_Multivector_Info (Name : String; Info : Multivector_Type.MV_Type_Record);
   procedure Print_Vertex (Name : String; Vertex : Multivectors.Vector);

end GA_Utilities;
