      * 日期计算参数
       01 DATE-CALC-PARAMS.
          05 DC-FUNCTION-CODE       PIC X.
             88 DC-ADD-DAYS         VALUE 'A'.
             88 DC-SUB-DAYS         VALUE 'S'.
             88 DC-DIFF-DAYS        VALUE 'D'.
             88 DC-CHECK-DATE       VALUE 'C'.
          05 DC-BASE-DATE           PIC X(8).
          05 DC-TARGET-DATE         PIC X(8).
          05 DC-DAYS                PIC S9(5) COMP-3.
          05 DC-RETURN-CODE         PIC X.
             88 DC-SUCCESS          VALUE '0'.
             88 DC-ERROR            VALUE '9'. 