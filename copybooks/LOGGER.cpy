      * 日志参数
       01 LOG-PARAMS.
          05 LOG-LEVEL              PIC X.
             88 LOG-INFO            VALUE 'I'.
             88 LOG-WARNING         VALUE 'W'.
             88 LOG-ERROR           VALUE 'E'.
             88 LOG-DEBUG           VALUE 'D'.
          05 LOG-MODULE             PIC X(8).
          05 LOG-MESSAGE            PIC X(100).
          05 LOG-RETURN-CODE        PIC X.
             88 LOG-SUCCESS         VALUE '0'.
             88 LOG-FAILURE         VALUE '9'. 