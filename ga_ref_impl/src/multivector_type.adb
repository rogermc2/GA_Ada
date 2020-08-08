
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with Bits;
--  with GA_Utilities;

package body Multivector_Type is

    --  Init is based on MultivectorType.java Init
    function Init (MV : Multivectors.Multivector) return MV_Type_Record is
        use Interfaces;
        use Multivectors;
        Rec        : MV_Type_Record;
        Count      : array (1 .. 2) of Unsigned_32 := (0, 0);
        Index      : constant Integer := 1;
        GU_Bitmap  : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
        Versor_Inv : Multivector;
        Grade_Inv  : Multivector;
    begin
        Rec.Grade := Top_Grade_Index (MV);
        Rec.Grade_Use := Grade_Use (MV);
        while GU_Bitmap /= 0 loop
            if (GU_Bitmap and 1) /= 0 then
                Count (Index) := Count (Index) + 1;
            end if;
            Count (Index) := Count (Index) xor 1;
            GU_Bitmap := Shift_Right (GU_Bitmap, 1);
        end loop;

        Rec.Zero := Count (1) = 0 and Count (2) = 0;
        if Rec.Zero then
            --  multivector = zero blade
            Rec.MV_Kind := Blade_MV;
            Rec.Parity := Even_Parity;
        elsif (Count (1) /= 0) and (Count (2) /= 0) then
            Rec.MV_Kind := Multivector_Type;
            Rec.Parity := No_Parity;
        else
            if Count (2) /= 0 then
                Rec.Parity := Odd_Parity;
            else
                Rec.Parity := Even_Parity;
            end if;

            Versor_Inv := Versor_Inverse (MV);
            Grade_Inv := Grade_Inversion (MV);
            if Geometric_Product (Versor_Inv, Grade_Inv) =
              Geometric_Product (Grade_Inv, Versor_Inv) then
                Rec.MV_Kind := Multivector_Type;
            elsif Bits.Bit_Count (Grade_Use (MV)) = 1 then
                Rec.MV_Kind := Blade_MV;
            else
                Rec.MV_Kind := Versor_MV;
            end if;
        end if;
        return Rec;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Type.Init.");
            raise;

    end Init;

    --  -------------------------------------------------------------------------

    function Init (MV : Multivectors.Multivector; aMetric : Metric.Metric_Record;
                   Epsilon : Float := 0.0) return MV_Type_Record is
        use Interfaces;
        use Multivectors;
        Ca         : Multivector := MV;
        Rec        : MV_Type_Record;
        Count      : array (1 .. 2) of Unsigned_32 := (0, 0);
        Index      : constant Integer := 1;
        GU_Bitmap  : Unsigned_32 := Unsigned_32 (Grade_Use (MV));
        Versor_Inv : Multivector;
        Grade_Inv  : Multivector;
        VG_Inv     : Multivector;
        GV_Inv     : Multivector;
    begin
        if Epsilon /= 0.0 then
            Multivectors.Compress (Ca, Epsilon);
        end if;

        Rec.Grade := Top_Grade_Index (Ca);
        Rec.Grade_Use := Grade_Use (Ca);
        while GU_Bitmap /= 0 loop
            if (GU_Bitmap and 1) /= 0 then
                Count (Index) := Count (Index) + 1;
            end if;
            Count (Index) := Count (Index) xor 1;
            GU_Bitmap := Shift_Right (GU_Bitmap, 1);
        end loop;

        Rec.Zero := Count (1) = 0 and Count (2) = 0;
        if Rec.Zero then
            --  multivector = zero blade
            Rec.MV_Kind := Blade_MV;
            Rec.Parity := Even_Parity;
        elsif (Count (1) /= 0) and (Count (2) /= 0) then
            Rec.MV_Kind := Multivector_Type;
            Rec.Parity := No_Parity;
        else
            if Count (2) /= 0 then
                Rec.Parity := Odd_Parity;
            else
                Rec.Parity := Even_Parity;
            end if;

            Versor_Inv := Versor_Inverse (MV, aMetric);
            Grade_Inv := Grade_Inversion (MV);
--              GA_Utilities.Print_Multivector ("Multivector_Type.Init Versor_Inv", Versor_Inv);
--              GA_Utilities.Print_Multivector ("Multivector_Type.Init Grade_Inv", Grade_Inv);
            VG_Inv := Geometric_Product (Versor_Inv, Grade_Inv, aMetric);
--              GA_Utilities.Print_Multivector ("Multivector_Type.Init VG_Inv", VG_Inv);
            Multivectors.Compress (VG_Inv, Epsilon);
--                                     _  GA_Utilities.Print_Multivector ("Multivector_Type.Init Compressed VG_Inv", VG_Inv);
            GV_Inv := Geometric_Product (Grade_Inv, Versor_Inv,aMetric);
--              GA_Utilities.Print_Multivector ("Multivector_Type.Init GV_Inv", GV_Inv);
            Multivectors.Compress (GV_Inv, Epsilon);
--              GA_Utilities.Print_Multivector ("Multivector_Type.Init Compressed GV_Inv", GV_Inv);
            if Grade_Use (GV_Inv) /= 1 or GV_Inv = VG_Inv then
                Rec.MV_Kind := Multivector_Type;
            elsif Bits.Bit_Count (Grade_Use (MV)) = 1 then
                Rec.MV_Kind := Blade_MV;
            else
                Rec.MV_Kind := Versor_MV;
            end if;

        end if;
        return Rec;

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Type.Init.");
            raise;

    end Init;

    --  -------------------------------------------------------------------------

    function MV_Kind (MV : MV_Type_Record) return MV_Type is
    begin
        return MV.MV_Kind;
    end MV_Kind;

    --  -------------------------------------------------------------------------

    function MV_Grade (MV : MV_Type_Record) return Integer is
    begin
        return MV.Grade;
    end MV_Grade;

    --  -------------------------------------------------------------------------

    function Grade_Use (MV : MV_Type_Record) return GA_Maths.Grade_Usage is
    begin
        return MV.Grade_Use;

    end Grade_Use;

    --  -------------------------------------------------------------------------

    function Parity (MV : MV_Type_Record) return Parity_Type is
    begin
        return MV.Parity;
    end ;

    --  -------------------------------------------------------------------------

    procedure Print_Multivector_Info (Name : String; Info : MV_Type_Record) is
    begin
        New_Line;
        Put_Line (Name & " multivector information:");
        Put_Line ("Type       " & MV_Type'Image (Info.MV_Kind));
        Put_Line ("Zero       " & boolean'Image (Info.Zero));
        Put_Line ("Parity     " & Parity_Type'Image (Info.Parity));
      Put_Line ("Grade Usage Bitmap " &
                 GA_Maths.Grade_Usage'Image (Info.Grade_Use));
        Put_Line ("Grade      " & Integer'Image (Info.Grade));

    exception
        when others =>
            Put_Line ("An exception occurred in Multivector_Type.Print_Multivector_Info.");
            raise;

    end Print_Multivector_Info;

    --  ------------------------------------------------------------------------

    function Zero (MV : MV_Type_Record) return Boolean is
    begin
        return MV.Zero;
    end ;

    --  -------------------------------------------------------------------------
end Multivector_Type;
