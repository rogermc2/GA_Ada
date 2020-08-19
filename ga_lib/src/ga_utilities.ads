
with Interfaces;

with GL.Types;

with Blade;
with Blade_Types;
with E3GA;
with GA_Maths;
with Metric;
with Multivectors;
with Multivector_Type;

package GA_Utilities is
   use GA_Maths.Float_Array_Package;

   function Multivector_Size (MV : Multivectors.Multivector) return Integer;
   procedure Print_Bitmap (Name : String; Bitmap : Interfaces.Unsigned_32);
   procedure Print_Blade (Name : String; B : Blade.Basis_Blade);
   procedure Print_Blade_List (Name : String; BL : Blade.Blade_List);
   procedure Print_Blade_String (Name : String; B : Blade.Basis_Blade;
                                 MV_Names : Blade_Types.Basis_Vector_Names);

    procedure Print_Blade_String_Array (Name : String;
                                        BB_Array : Blade.Basis_Blade_Array;
                                        MV_Names : Blade_Types.Basis_Vector_Names);
   procedure Print_E3_Vector (Name : String; aVector : E3GA.E3_Vector);
   procedure Print_E3_Vector_Array  (Name : String;
                                     anArray : GL.Types.Singles.Vector3_Array);
   procedure Print_Float_3D (Name : String; aVector : GA_Maths.Float_3D);
   procedure Print_Float_Array (Name : String; anArray : GA_Maths.Float_Vector);
   procedure Print_Integer_Array (Name : String; anArray : GA_Maths.Integer_Array);
   procedure Print_Matrix (Name : String; aMatrix : GA_Maths.GA_Matrix3);
   procedure Print_Matrix (Name : String; aMatrix : Real_Matrix);
   procedure Print_Matrix (Name  : String; aMatrix : Real_Matrix;
                           Start, Last : GA_Maths.Array_I2);
   procedure Print_Metric (Name : String; aMetric : Metric.Metric_Record);
   procedure Print_Multivector (Name : String; MV : Multivectors.Multivector);
   procedure Print_Multivector_Info (Name : String;
                                     Info : Multivector_Type.MV_Type_Record);
   procedure Print_Multivector_List (Name : String;
                                     MV_List : Multivectors.Multivector_List);
   procedure Print_Multivector_List_String
      (Name : String; MV_List : Multivectors.Multivector_List;
       MV_Names : Blade_Types.Basis_Vector_Names);
   procedure Print_Multivector_String (Name : String; MV : Multivectors.Multivector;
                                       MV_Names : Blade_Types.Basis_Vector_Names);
   procedure Print_Vertex (Name : String; Vertex : Multivectors.M_Vector);

end GA_Utilities;
