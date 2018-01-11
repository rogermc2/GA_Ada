

package GA_Base_Types is

   type E1_T is private;
   type E2_T is private;
   type E3_T is private;
   type NI_T is private;
   type NI_Const is private;
   type NO_T is private;
   type NO_Const is private;

   function "*" (I1, I2 : NI_T) return float;
   function "*" (I : NI_T; O : NO_T) return float;
   function "*" (O : NO_T; I : NI_T) return float;
   function "*" (O1, O2 : NO_T) return float;

   function NI return float;
   function NI (N : NI_T) return float;
   function NO return float;
   function NO (N : NO_T) return float;

   procedure Set_NI  (N : out NI_T; Inf : float);
   procedure Set_NO  (N : out NO_T; Origin : float);

private
   type E1_T is record
      E1 : float := 0.0;
   end record;

   type E2_T is record
      E2 : float := 0.0;
   end record;

   type E3_T is record
      E3 : float := 0.0;
   end record;

   type NI_T is record
      Inf : float := 1.0;
   end record;

   type NI_Const is new float;

   type NO_T is record
      Origin : float := 0.0;
   end record;

   type NO_Const is new float;

end GA_Base_Types;
