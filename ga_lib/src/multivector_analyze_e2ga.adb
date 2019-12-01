
with Ada.Text_IO; use Ada.Text_IO;

with Blade;
with E2GA;
with GA_Maths;
with Multivector_Type;
with Multivector_Type_Base;

package body Multivector_Analyze_E2GA is

   procedure Analyze (theAnalysis : in out MV_Analysis; MV : Multivectors.Multivector;
                      Flags : Flag_Type := (Flag_Invalid, false);
                      Epsilon : float := Default_Epsilon) is
      use Multivectors;
      use Multivector_Type;
      use Multivector_Type_Base;
      use Blade_List_Package;

      MV_X      : Multivectors.Multivector := MV;
      Norm_MV_X : Float;
      Blades    : constant Blade_List := Get_Blade_List (MV_X);
--        aBlade    : Blade.Basis_Blade;
      Curs      : constant Cursor := Blades.First;
      MV_Info   : Multivector_Type.MV_Type_Record;
      Analysis  : MV_Analysis;
   begin
      Analysis.M_Flags.Valid := True;
      Analysis.Epsilon := Epsilon;
      Analysis.M_Type.Model_Kind := Multivector_Analyze.Vector_Space;

      if Flags.Dual then
         Put_Line ("Multivector_Analyze_E2GA.Analyze Is Dual.");
         Analysis.M_Flags.Dual := True;
         MV_X := Multivectors.Dual (MV_X);
      end if;
      Norm_MV_X := Norm_E2 (MV_X);

      MV_Info:= Multivector_Type.Init (MV_X);
      Analysis.M_MV_Type := MV_Info;
      Analysis.M_Type.Multivector_Kind := Multivector_Type_Base.MV_Object;

      --  Check for zero blade
      if Zero (MV_Info) then
         Put_Line ("Multivector_Analyze_E2GA.Analyze Zero_Blade.");
         Analysis.M_Type.Blade_Class := Zero_Blade;
         Analysis.Scalors (1) := 0.0;

      --  Check for Versor
      elsif MV_Kind (MV_Info) = Versor_MV then
--        elsif Analysis.M_MV_Type.M_Type = Versor_MV then
         Put_Line ("Multivector_Analyze_E2GA.Analyze Versor_Object 2.");
         Analysis.M_Type.Versor_Subclass := Even_Versor;
         Analysis.M_Vectors (1) := E2GA.e1;

--           if MV_X.Coordinates (1) < 0.0 then
         if Blade.Weight (Element (Curs)) < 0.0 then
            Analysis.M_Vectors (2) := E2GA.e2;
         else
            declare
            begin
               Analysis.M_Vectors (2) := -E2GA.e2;
            end;
         end if;

         Analysis.M_Vectors (2) := E2GA.e1;
         --           Analysis.M_Scalors (1) := E2GA.Get_Coord (Norm_E2 (MV_X));
         Analysis.Scalors (1) := Norm_MV_X;
         Analysis.Scalors (2) := 2.0 * GA_Maths.Float_Functions.Arctan
             (Norm_E2 (MV_X), Blade.Weight (Element (Curs)));
--               (E2GA.Get_Coord (E2GA.Norm_E2 (MV_X)), MV_X.Coordinates (1));

      --  Check for Blade
--        elsif Analysis.M_MV_Type.M_Type = Blade_MV then
      elsif MV_Kind (MV_Info) = Blade_MV then
         Put_Line ("Multivector_Analyze_E2GA.Analyze Blade_Object.");
         Analysis.M_Type.M_Grade := Multivector_Type.Grade_Use (Analysis.M_MV_Type);
         case Analysis.M_Type.M_Grade is
            when 1 =>
               Analysis.M_Type.Blade_Subclass := Point_Subclass;
            when 2=>
               Analysis.M_Type.Blade_Subclass := Line_Subclass;
            when 3 =>
               Analysis.M_Type.Blade_Subclass := Plane_Subclass;
            when others =>
               Analysis.M_Type.Blade_Subclass := Unspecified_Subclass;
         end case;
         Analysis.Scalors (1) := Norm_MV_X;

         if Analysis.M_Type.Blade_Subclass = Vector_Subclass then
            Put_Line ("Multivector_Analyze_E2GA.Analyze Vector_Type.");
            declare
               Xn  : Vector;
            begin
               Xn := E2GA.Unit_E (MV_X);
               Analysis.M_Vectors (1) := E2GA.Get_Coord_1 (Xn) * E2GA.e1 +
                                         E2GA.Get_Coord_2 (Xn) * E2GA.e2 ;
            end;

         elsif Analysis.M_Type.Blade_Subclass = Bivector_Subclass then
            Put_Line ("Multivector_Analyze_E2GA.Analyze Bivector_Type.");
            Analysis.M_Vectors (1) := E2GA.e1;
            declare
            begin
--        BV.Coordinates (1) := MV.Coordinates (4);
               if Blade.Weight (Element (Curs)) < 0.0 then
                  Analysis.M_Vectors (2) := -E2GA.e2;
               else
                  Analysis.M_Vectors (2) := E2GA.e2;
               end if;
            end;
         end if;
      else
         Put_Line ("Multivector_Analyze_E2GA.Analyze Multivector Type.");
      end if;
      theAnalysis := Analysis;

   exception
      when others =>
         Put_Line ("An exception occurred in Multivector_Analyze_E2GA.Analyze.");
         raise;
   end Analyze;

end Multivector_Analyze_E2GA;
