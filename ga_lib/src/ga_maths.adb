
with Maths;

package body GA_Maths is

   function Is_Anti_Euclidean (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := aMatrix (row, col) + 1.0 <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) ** 2 -
                               aMatrix (col, row) ** 2) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Anti_Euclidean;

   --  ------------------------------------------------------------------------

   function Is_Diagonal (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row /= col then
                  OK := Abs (aMatrix (row, col)) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Diagonal;

   --  ------------------------------------------------------------------------

   function Is_Symetric (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := aMatrix (row, col) - 1.0 <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) - aMatrix (col, row)) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Symetric;

   --  ------------------------------------------------------------------------

   function Is_Euclidean (aMatrix : Float_Matrix) return Boolean is
      epsilon : constant float := 10.0 ** (-9);
      row     : Integer := aMatrix'First;
      col     : Integer := aMatrix'First (2);
      OK      : Boolean := aMatrix'Length (1) = aMatrix'Length (2);
   begin
      if OK then
         while row <= aMatrix'Last and OK loop
            while col <= aMatrix'Last (2) and OK loop
               if row = col then
                  OK := Abs (aMatrix (row, col)) <= epsilon;
               else
                  OK := Abs (aMatrix (row, col) ** 2 -
                               aMatrix (col, row) ** 2) <= epsilon;
               end if;
               col := col + 1;
            end loop;
            row := row + 1;
         end loop;
      end if;
      return OK;
   end Is_Euclidean;

   --  ------------------------------------------------------------------------

   function Maximum (I1, I2 : Integer) return Integer is
      Max : Integer;
   begin
      if I1 > I2 then
         Max := I1;
      else
         Max := I2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Maximum (F1, F2 : Float) return Float is
      Max : Float;
   begin
      if F1 > F2 then
         Max := F1;
      else
         Max := F2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Maximum (F1, F2 : Long_Float) return Long_Float is
      Max : Long_Float;
   begin
      if F1 > F2 then
         Max := F1;
      else
         Max := F2;
      end if;
      return Max;
   end Maximum;

   --  ------------------------------------------------------------------------

   function Minimum (I1, I2 : Integer) return Integer is
      Min : Integer;
   begin
      if I1 < I2 then
         Min := I1;
      else
         Min := I2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

   function Minimum (F1, F2 : Float) return Float is
      Min : Float;
   begin
      if F1 < F2 then
         Min := F1;
      else
         Min := F2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

   function Minimum (F1, F2 : Long_Float) return Long_Float is
      Min : Long_Float;
   begin
      if F1 < F2 then
         Min := F1;
      else
         Min := F2;
      end if;
      return Min;
   end Minimum;

   --  ------------------------------------------------------------------------

   function Vector_Rotation_Matrix (From, To : GL.Types.Singles.Vector3)
                                    return GL.Types.Singles.Matrix4 is
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
--        use Maths.Single_Math_Functions;
      Norm_From : constant Vector3 := Normalized (From);
      Norm_To   : constant Vector3 := Normalized (To);
      Cross     : constant Vector3 := Cross_Product (Norm_From, Norm_To);
--        Sin       : constant Single := Norm (Cross);
      Cos       : Single := Dot_Product (Norm_From, Norm_To);
      Vx        : constant Matrix3 := ((-Cross (GL.Z), 0.0, Cross (GL.Y)),
                                       (Cross (GL.Z), 0.0, -Cross (GL.X)),
                                       (-Cross (GL.Y), 0.0, Cross (GL.X)));
      Vx_Sq     : constant Matrix3 := Vx * Vx;
      Rot_3     : Matrix3;
      Rot_4     : Matrix4 := Identity4;
   begin
      if Cos = -1.0 then
         Cos := -0.999999;
      end if;
      Rot_3 := Identity3 + Vx + 1.0 / (1.0 + Cos) * Vx_Sq;
      for row in Rot_3'Range loop
            for col in Rot_3'Range (2) loop
                Rot_4 (row, col) := Rot_3 (row, col);
            end loop;
      end loop;
      return Rot_4;
   end Vector_Rotation_Matrix;

   --  ------------------------------------------------------------------------

end GA_Maths;
