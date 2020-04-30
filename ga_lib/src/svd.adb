
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with Ada_Lapack;

with GA_Maths;

package body SVD is

    package Complex_Maths   is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
    package Lapack is new Ada_Lapack (Real, Complex_Types,
                                      Real_Arrays, Complex_Arrays);

    function Singular_Value_Decomposition (aMatrix : Complex_Arrays.Complex_Matrix)
                                           return SVD is
        use Lapack;
        --          use Float_Array_Package;
        --          use Float_Functions;
        --          use Float_Sort_Package;
        use Real_Arrays;
        use Complex_Types;
        use Complex_Arrays;

        One          : constant Real := 1.0e0;
        Zero         : constant Real := 0.0e0;

        Num_Rows        : constant Natural := aMatrix'Length (1);
        Num_Cols        : constant Natural := aMatrix'Length (2);

        Matrix_A        : Complex_Matrix := aMatrix;
        Matrix_U        : Complex_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
        V_Transpose     : Complex_Matrix (1 .. Num_Cols, 1 .. Num_Cols);
        Vector_R        : Real_Vector (1 .. Num_Rows);
        Vector_S        : Real_Vector (1 .. Num_Rows);
        Short_Vector    : Complex_Vector (1 .. 1);
        Num_Singular    : Integer := GA_Maths.Minimum (Num_Rows, Num_Cols);
        Singular_Values : Real_Vector (1 .. Num_Singular);
        Return_Code     : Integer;
        theSVD          : SVD (Num_Rows, Num_Cols);
    begin
        GESVD (JOBU   => 'S', JOBVT  => 'S',
                M      => Num_Rows, N      => Num_Cols,
                A      => Matrix_A, LDA    => Num_Rows,
                S      => Singular_Values,
                U      => Matrix_U, LDU    => Num_Rows,
                VT     => V_Transpose, LDVT   => Num_Cols,
                WORK   => Short_Vector, LWORK  => -1,
                RWORK  => Vector_R,
                INFO   => Return_code);
        declare
            Work_Vector_Rows : Integer := Integer (Re (Short_Vector (1)));
            Work_Vector      : Complex_Vector (1 .. Work_Vector_Rows);
        begin
            GESVD (JOBU   => 'S', JOBVT  => 'S',
                M      => Num_Rows, N      => Num_Cols,
                A      => Matrix_A, LDA    => Num_Rows,
                S      => Singular_Values,
                U      => Matrix_U, LDU    => Num_Rows,
                VT     => V_Transpose, LDVT   => Num_Cols,
                WORK   => Work_Vector, LWORK  => Work_Vector_Rows,
                RWORK  => Vector_R,
                INFO   => Return_Code);
        end;
        if Return_Code > 0 then
            raise SVD_Exception with
             "Singular_Value_Decomposition failed with return code" & Integer'Image (Return_Code);
        else
            theSVD.Matrix_U := Matrix_U;
        end if;
        return theSVD;
    end Singular_Value_Decomposition;

    --     --  ------------------------------------------------------------------------
    --    use Float_Functions;
    --
    --    type SVD (Num_Rows, Num_Cols : Natural) is record
    --          Matrix_U : Float_Matrix (1..Num_Rows, 1..Num_Cols) := (others => (others => 0.0));
    --          Matrix_V : Float_Matrix (1..Num_Rows, 1..Num_Cols) := (others => (others => 0.0));
    --          Matrix_W : Float_Vector (1..Num_Rows) := (others => 0.0);
    --          eps      : Float := Float'Model_Epsilon;
    --          tsh      : Float;
    --    end record;
    --
    --     function Sign (A, B : Float) return Float;
    --
    --     --  ------------------------------------------------------------------------
    --
    --      function Decompose (aMatrix : Float_Matrix) return SVD is
    --          use Float_Array_Package;
    --          use Float_Sort_Package;
    --          Rows    : constant Natural := aMatrix'Length(1);
    --          Cols    : constant Natural := aMatrix'Length(2);
    --          theSVD  : SVD (Rows, Cols);
    --          F       : Float;
    --          G       : Float := 0.0;
    --          H       : Float;
    --          L       : Natural;
    --          S       : Float;
    --          Scale   : Float := 0.0;
    --          Anorm   : Float := 0.0;
    --          Flag    : Boolean := False;
    --          RV1     : Float_Vector (1 .. Cols);
    --      begin
    --          theSVD.tsh := 0.5 * Sqrt (Float (Rows + Cols + 1)) * theSVD.eps;
    --          for col in 1 .. Cols loop
    --              L := col + 2;
    --              RV1 (col) := Scale * G;
    --              G := 0.0;
    --              S := 0.0;
    --              Scale := 0.0;
    --              if col < Rows then
    --                  for row in col .. Rows loop
    --                      Scale := Scale + Abs(theSVD.Matrix_U (col, row));
    --                  end loop;
    --                  if Scale /= 0.0 then
    --                      for row in col .. Rows loop
    --                          theSVD.Matrix_U (col, row) := theSVD.Matrix_U (col, row) / Scale;
    --                          S := S + theSVD.Matrix_U (col, row) ** 2;
    --                      end loop;
    --                      F := theSVD.Matrix_U (col, col);
    --                      G := -Sign (Sqrt (S), F);
    --                      H := F * G - S;
    --                      theSVD.Matrix_U (col, col) := F - G;
    --                      for index_j in L - 1 .. Cols loop
    --                          S := 0.0;
    --                          for index_k in col .. Rows loop
    --                                  S := S + theSVD.Matrix_U (index_k, col) * theSVD.Matrix_U (index_k, index_j);
    --                          end loop;
    --                          F := S / H;
    --                          for index_k in col .. Rows loop
    --                                  theSVD.Matrix_U (index_k, index_j) := theSVD.Matrix_U (index_k, index_j) + theSVD.Matrix_U (index_k, col);
    --                          end loop;
    --                      end loop;
    --                  end if;
    --
    --                  for index_k in col .. Rows loop
    --                       theSVD.Matrix_U (index_k, col) := Scale * theSVD.Matrix_U (index_k, col);
    --                  end loop;
    --              end if;
    --              theSVD.Matrix_W (col) := Scale * G;
    --              G := 0.0;
    --              S := 0.0;
    --              Scale := 0.0;
    --              if col <= Rows and col /= Cols then
    --                  for index_k in L - 1 .. Rows loop
    --                       Scale := Scale + Abs (theSVD.Matrix_U (col, index_k));
    --                  end loop;
    --                  if Scale /= 0.0 then
    --                      for index_k in L - 1 .. Rows loop
    --                          theSVD.Matrix_U (col, index_k) := theSVD.Matrix_U (col, index_k) / Scale;
    --                          S := S + theSVD.Matrix_U (col, index_k) ** 2;
    --                      end loop;
    --                      F := theSVD.Matrix_U (col, L - 1);
    --                      G := -Sign (Sqrt (S), F);
    --                      H := F * G - S;
    --                      for index_k in L - 1 .. Cols loop
    --                          RV1 (index_k) := theSVD.Matrix_U (col, index_k) / H;
    --                      end loop;
    --                      for index_j in L - 1 .. Rows loop
    --                          S := 0.0;
    --                          for index_k in L - 1 .. Cols loop
    --                              S := S + theSVD.Matrix_U (index_j, index_k) * theSVD.Matrix_U (col, index_k);
    --                          end loop;
    --                          for index_k in L - 1 .. Rows loop
    --                              theSVD.Matrix_U (index_j, index_k) := theSVD.Matrix_U (index_j, index_k) +
    --                              S * RV1 (index_k);
    --                          end loop;
    --                      end loop;
    --                      for index_k in L - 1 .. Rows loop
    --                          theSVD.Matrix_U (col, index_k) := Scale * theSVD.Matrix_U (col, index_k);
    --                      end loop;
    --                  end if;
    --              end if;
    --              Anorm := Maximum (Anorm, Abs(theSVD.Matrix_W (col) + RV1 (col)));
    --          end loop;
    --          --  Accumulation of right-hand transformations.
    --
    --          for col in reverse 1 .. Cols loop
    --              if col < Cols then
    --                if G /= 0.0 then
    --                      for index_j in L .. Rows loop
    --                          --  Double division to avoid possible underflow.
    --                          theSVD.Matrix_V (index_j,col) :=
    --                            theSVD.Matrix_U (col, index_j) / theSVD.Matrix_U (col, L) / G;
    --                      end loop;
    --                      for index_j in L .. Rows loop
    --                          S := 0.0;
    --                          for index_k in L .. Rows loop
    --                              S := S + theSVD.Matrix_V (col, index_k) * theSVD.Matrix_V (index_k, index_j);
    --                          end loop;
    --                          for index_k in L .. Rows loop
    --                              theSVD.Matrix_V (index_k, index_j) :=
    --                                theSVD.Matrix_V (index_k, index_j)  + S * theSVD.Matrix_V (index_k, col);
    --                          end loop;
    --                      end loop;
    --                end if;
    --              end if;
    --          end loop;
    --  --          Sort (Float_List_Package.List (Singular_Values));
    --          return theSVD;
    --      end Decompose;
    --
    --     --  ------------------------------------------------------------------------
    --
    --      function Init_Singular_Value_Decomposition (aMatrix : Float_Matrix)
    --                                    return SVD is
    --          use Float_Array_Package;
    --          use Float_Functions;
    --          use Float_Sort_Package;
    --          Rows   : constant Natural := aMatrix'Length(1);
    --          Cols   : constant Natural := aMatrix'Length(2);
    --          theSVD : SVD (Rows, Cols);
    --          Sq_Singular_Values : constant Real_Vector (aMatrix'Range) :=
    --                                 Eigenvalues (aMatrix * Transpose (aMatrix));
    --          Singular_Values    : Float_List;
    --      begin
    --          theSVD.tsh := 0.5 * Sqrt (Float (Rows + Cols + 1)) * theSVD.eps;
    --          Sort (Float_List_Package.List (Singular_Values));
    --          return theSVD;
    --      end Init_Singular_Value_Decomposition;
    --
    --     --  ------------------------------------------------------------------------
    --
    --      function Singular_Value_List (aMatrix : Float_Matrix)
    --                                    return Float_List is
    --          use Float_Array_Package;
    --          use Float_Functions;
    --          use Float_Sort_Package;
    --          Sq_Singular_Values : constant Real_Vector (aMatrix'Range) :=
    --                                 Eigenvalues (aMatrix * Transpose (aMatrix));
    --          SV                 : Float;
    --          Singular_Values    : Float_List;
    --      begin
    --          for index in aMatrix'Range loop
    --              SV := Sqrt (Abs (Sq_Singular_Values (index)));
    --              if SV > 10.0 ** (-8) then
    --                 Append (Singular_Values, SV);
    --              end if;
    --          end loop;
    --          Sort (Float_List_Package.List (Singular_Values));
    --          return Singular_Values;
    --      end Singular_Value_List;
    --
    --     --  ------------------------------------------------------------------------

    function Condition_Number (aMatrix : Real_Arrays.Real_Matrix) return Float is
--          use Float_Array_Package;
--          use Float_Functions;
--          use Float_List_Package;
        --          Singular_Values : constant Float_List := Singular_Value_List (aMatrix);
        --          Max             : Float;
        --          Min             : Float;
        Result          : Float := 0.0;
    begin
        --          if List (Singular_Values) /= Empty_List then
        --              Max := Singular_Values.First_Element;
        --              Min := Singular_Values.Last_Element;
        --              if Min > 0.0 then
        --                  Result := Max / Min;
        --              else
        --                  Result := Max / 10.0 ** (-8);
        --              end if;
        --          end if;
        return Result;
    end Condition_Number;

    --  ------------------------------------------------------------------------
    --
    --     function Sign (A, B : Float) return Float is
    --          Result : Float := A;
    --     begin
    --          if B < 0.0 then
    --              Result := -A;
    --          end if;
    --          return Result;
    --     end Sign;

end SVD;
