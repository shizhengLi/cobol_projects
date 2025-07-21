      ******************************************************************
      * Premium Payment Processing Batch Program
      * Function: Read payment update file, update payment records in database
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPMUPD.
       AUTHOR. DEMO.
       DATE-WRITTEN. 2025-07-15.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMENT-FILE ASSIGN TO EXTERNAL PAYFILE
           ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT REPORT-FILE ASSIGN TO EXTERNAL REPFILE
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYMENT-FILE
           RECORD CONTAINS 58 CHARACTERS
           DATA RECORD IS PAYMENT-RECORD-IN.
       01  PAYMENT-RECORD-IN         PIC X(58).
       
       FD  REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE               PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS            PIC XX VALUE SPACES.
       01  WS-EOF-FLAG               PIC X VALUE 'N'.
          88 END-OF-FILE             VALUE 'Y'.
       01  WS-COUNTERS.
          05 WS-READ-COUNT           PIC 9(5) VALUE ZEROS.
          05 WS-PROCESS-COUNT        PIC 9(5) VALUE ZEROS.
          05 WS-ERROR-COUNT          PIC 9(5) VALUE ZEROS.
       
       01  WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR      PIC 9(4).
             10 WS-CURRENT-MONTH     PIC 9(2).
             10 WS-CURRENT-DAY       PIC 9(2).
          05 WS-CURRENT-TIME.
             10 WS-CURRENT-HOUR      PIC 9(2).
             10 WS-CURRENT-MINUTE    PIC 9(2).
             10 WS-CURRENT-SECOND    PIC 9(2).
             10 WS-CURRENT-MSEC      PIC 9(2).
          05 WS-DIFF-FROM-GMT        PIC S9(4).
       
       01  WS-ACCOUNTING-PERIOD      PIC X(6).
       
      * Report related fields
       01  HL-HEADING-LINE-1.
          05 FILLER                  PIC X(20) VALUE SPACES.
          05 FILLER                  PIC X(40) VALUE 
             'PREMIUM PAYMENT PROCESSING REPORT'.
          05 FILLER                  PIC X(20) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'DATE: '.
          05 HL-DATE                 PIC X(10).
          05 FILLER                  PIC X(10) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'TIME: '.
          05 HL-TIME                 PIC X(8).
          
       01  HL-HEADING-LINE-2.
          05 FILLER                  PIC X(132) VALUE ALL '-'.
          
       01  HL-HEADING-LINE-3.
          05 FILLER                  PIC X(10) VALUE 'POLICY ID'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'PAY DATE'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'PAY METHOD'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'AMOUNT'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(12) VALUE 'PAYMENT ID'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(16) VALUE 'REFERENCE NO'.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 FILLER                  PIC X(10) VALUE 'RESULT'.
          
       01  DL-DETAIL-LINE.
          05 DL-POLICY-ID            PIC X(10).
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-PAYMENT-DATE         PIC X(10).
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-PAYMENT-METHOD       PIC X(10).
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-AMOUNT               PIC ZZ,ZZZ,ZZ9.99.
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-PAYMENT-ID           PIC X(12).
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-REFERENCE-NO         PIC X(16).
          05 FILLER                  PIC X(5) VALUE SPACES.
          05 DL-RESULT               PIC X(10).
       
       01  TL-TOTAL-LINE.
          05 FILLER                  PIC X(20) VALUE SPACES.
          05 FILLER                  PIC X(20) VALUE 'TOTAL PROCESSED:'.
          05 TL-TOTAL-PROCESSED      PIC ZZ,ZZ9.
          05 FILLER                  PIC X(10) VALUE SPACES.
          05 FILLER                  PIC X(20) VALUE 'TOTAL ERRORS:'.
          05 TL-TOTAL-ERRORS         PIC ZZ,ZZ9.
          
      * Payment record structure
       01 PAYMENT-RECORD.
          05 PR-POLICY-ID            PIC X(10).
          05 PR-PAYMENT-DATE         PIC X(8).
          05 PR-PAYMENT-METHOD       PIC X(2).
          05 PR-AMOUNT               PIC 9(8)V99.
          05 PR-PAYMENT-ID           PIC X(12).
          05 PR-REFERENCE-NO         PIC X(16).
       
      * SQL Communication Area
       01  SQLCA.
           05  SQLCAID                PIC X(8).
           05  SQLCABC                PIC S9(9) COMP.
           05  SQLCODE                PIC S9(9) COMP.
           05  SQLERRM.
               49  SQLERRML           PIC S9(4) COMP.
               49  SQLERRMC           PIC X(70).
           05  SQLERRP                PIC X(8).
           05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP.
           05  SQLWARN.
               10 SQLWARN0            PIC X.
               10 SQLWARN1            PIC X.
               10 SQLWARN2            PIC X.
               10 SQLWARN3            PIC X.
               10 SQLWARN4            PIC X.
               10 SQLWARN5            PIC X.
               10 SQLWARN6            PIC X.
               10 SQLWARN7            PIC X.
           05  SQLEXT                 PIC X(8).
       
      * Date calculation parameters
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
       
      * Log parameters
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

       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INIT-PROCESS
           PERFORM 2000-PROCESS-DATA UNTIL END-OF-FILE
           PERFORM 3000-END-PROCESS
           GOBACK.

       1000-INIT-PROCESS.
      * Initialize processing
           MOVE 'N' TO WS-EOF-FLAG
           MOVE ZEROS TO WS-READ-COUNT
                         WS-PROCESS-COUNT
                         WS-ERROR-COUNT
                         
      * Get current date and time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           
      * Set accounting period (current year/month)
           STRING WS-CURRENT-YEAR
                  WS-CURRENT-MONTH
             DELIMITED BY SIZE
             INTO WS-ACCOUNTING-PERIOD
           END-STRING
           
      * Open files
           OPEN INPUT PAYMENT-FILE
           OPEN OUTPUT REPORT-FILE
           
      * Write report header
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURRENT-YEAR DELIMITED BY SIZE
                  '-'
                  WS-CURRENT-MONTH DELIMITED BY SIZE
                  '-'
                  WS-CURRENT-DAY DELIMITED BY SIZE
             INTO HL-DATE
           END-STRING
           
           STRING WS-CURRENT-HOUR DELIMITED BY SIZE
                  ':'
                  WS-CURRENT-MINUTE DELIMITED BY SIZE
                  ':'
                  WS-CURRENT-SECOND DELIMITED BY SIZE
             INTO HL-TIME
           END-STRING
           
           WRITE REPORT-LINE FROM HL-HEADING-LINE-1
           WRITE REPORT-LINE FROM HL-HEADING-LINE-2
           WRITE REPORT-LINE FROM HL-HEADING-LINE-3
           WRITE REPORT-LINE FROM HL-HEADING-LINE-2
           
      * Call logging subroutine to log start message
           MOVE 'I' TO LOG-LEVEL
           MOVE 'INSPMUPD' TO LOG-MODULE
           MOVE 'Premium payment processing started' TO LOG-MESSAGE
           CALL 'INSPMLOG_EN' USING LOG-PARAMS
           
      * Read first record
           PERFORM 2100-READ-PAYMENT-FILE.
           
       2000-PROCESS-DATA.
      * Process each payment record
           ADD 1 TO WS-PROCESS-COUNT
           
      * Parse payment record
           MOVE PAYMENT-RECORD-IN(1:10) TO PR-POLICY-ID
           MOVE PAYMENT-RECORD-IN(11:8) TO PR-PAYMENT-DATE
           MOVE PAYMENT-RECORD-IN(19:2) TO PR-PAYMENT-METHOD
           
      * Note: Assume amount is 10 bytes, format 9(8)V99
           COMPUTE PR-AMOUNT = FUNCTION NUMVAL(PAYMENT-RECORD-IN(21:10))
           
           MOVE PAYMENT-RECORD-IN(31:12) TO PR-PAYMENT-ID
           MOVE PAYMENT-RECORD-IN(43:16) TO PR-REFERENCE-NO
           
      * Verify policy exists (call subroutine)
           CALL 'INSPOLVD' USING PR-POLICY-ID SQLCODE
           
           IF SQLCODE = 0
      * Policy exists, insert payment record
               MOVE SPACES TO DL-RESULT
               PERFORM 2200-INSERT-PAYMENT
               IF SQLCODE = 0
                   MOVE 'SUCCESS' TO DL-RESULT
               ELSE
                   MOVE 'FAILED' TO DL-RESULT
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
           ELSE
      * Policy doesn't exist, record error
               MOVE 'NO POLICY' TO DL-RESULT
               ADD 1 TO WS-ERROR-COUNT
           END-IF
           
      * Format report line data
           MOVE PR-POLICY-ID TO DL-POLICY-ID
           
      * Format date (YYYYMMDD => YYYY-MM-DD)
           STRING PR-PAYMENT-DATE(1:4) DELIMITED BY SIZE
                  '-'
                  PR-PAYMENT-DATE(5:2) DELIMITED BY SIZE
                  '-'
                  PR-PAYMENT-DATE(7:2) DELIMITED BY SIZE
             INTO DL-PAYMENT-DATE
           END-STRING
           
      * Format payment method code
           EVALUATE PR-PAYMENT-METHOD
               WHEN '01'
                   MOVE 'BANK' TO DL-PAYMENT-METHOD
               WHEN '02'
                   MOVE 'CREDIT' TO DL-PAYMENT-METHOD
               WHEN '03'
                   MOVE 'CASH' TO DL-PAYMENT-METHOD
               WHEN OTHER
                   MOVE 'UNKNOWN' TO DL-PAYMENT-METHOD
           END-EVALUATE
           
           MOVE PR-AMOUNT TO DL-AMOUNT
           MOVE PR-PAYMENT-ID TO DL-PAYMENT-ID
           MOVE PR-REFERENCE-NO TO DL-REFERENCE-NO
           
      * Write report detail line
           WRITE REPORT-LINE FROM DL-DETAIL-LINE
           
      * Read next record
           PERFORM 2100-READ-PAYMENT-FILE.
           
       2100-READ-PAYMENT-FILE.
      * Read payment file record
           READ PAYMENT-FILE INTO PAYMENT-RECORD-IN
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-READ-COUNT
           END-READ.
           
       2200-INSERT-PAYMENT.
      * Call subroutine to insert payment record to database
           CALL 'INSPMINS' USING PAYMENT-RECORD
                                  WS-ACCOUNTING-PERIOD
                                  SQLCODE.
                                  
       3000-END-PROCESS.
      * End processing
           MOVE WS-PROCESS-COUNT TO TL-TOTAL-PROCESSED
           MOVE WS-ERROR-COUNT TO TL-TOTAL-ERRORS
           
      * Write report summary line
           WRITE REPORT-LINE FROM HL-HEADING-LINE-2
           WRITE REPORT-LINE FROM TL-TOTAL-LINE
           
      * Call logging subroutine to log end message
           MOVE 'I' TO LOG-LEVEL
           MOVE 'INSPMUPD' TO LOG-MODULE
           STRING 'Premium payment processing batch ended. Processed: ' 
                  WS-PROCESS-COUNT
                  ' Errors: ' 
                  WS-ERROR-COUNT
             DELIMITED BY SIZE
             INTO LOG-MESSAGE
           END-STRING
           CALL 'INSPMLOG_EN' USING LOG-PARAMS
           
      * Close files
           CLOSE PAYMENT-FILE
           CLOSE REPORT-FILE.
