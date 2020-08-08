
with GA_Maths;
with Metric;
with Multivectors;

package Multivector_Type is

   type MV_Type is (Unspecified_MV_Type, Multivector_Type, Versor_MV, Blade_MV);
   type Parity_Type is (Odd_Parity, Even_Parity, No_Parity);
   type MV_Type_Record is private;

   function Grade_Use (MV : MV_Type_Record) return GA_Maths.Grade_Usage;
   function Init (MV : Multivectors.Multivector) return MV_Type_Record;
   function Init (MV : Multivectors.Multivector; aMetric : Metric.Metric_Record;
                 Epsilon : Float := 0.0) return MV_Type_Record;
   function MV_Grade (MV : MV_Type_Record) return Integer;
   function MV_Kind (MV : MV_Type_Record) return MV_Type;
   function Parity (MV : MV_Type_Record) return Parity_Type;
   procedure Print_Multivector_Info (Name : String; Info : MV_Type_Record);
   function Zero (MV : MV_Type_Record) return Boolean;

private

   type MV_Type_Record is record
      MV_Kind   : MV_Type := Unspecified_MV_Type;  -- type MV, Versor or Blade
      Zero      : Boolean := True;
      Grade     : Integer := 0;  --  Top_Grade
      Grade_Use : GA_Maths.Grade_Usage := 0;
      Parity    : Parity_Type := No_Parity;
   end record;

end Multivector_Type;
