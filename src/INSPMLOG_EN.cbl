      ******************************************************************
      * Log Recording Subroutine
      * Function: Record program logs
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPMLOG_EN.
       AUTHOR. DEMO.
       DATE-WRITTEN. 2025-07-15.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO EXTERNAL LOGFILE
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE
           RECORD CONTAINS 200 CHARACTERS
           DATA RECORD IS LOG-RECORD.
       01  LOG-RECORD               PIC X(200).
       
       WORKING-STORAGE SECTION.
      * Working variables
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
           
       01  WS-TIMESTAMP               PIC X(19).
       01  WS-LOG-LINE                PIC X(200).
       01  WS-FILE-OPENED             PIC X VALUE 'N'.
          88 FILE-IS-OPENED           VALUE 'Y'.
       01  WS-FILE-STATUS             PIC XX VALUE SPACES.
          
       LINKAGE SECTION.
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
       
       PROCEDURE DIVISION USING LOG-PARAMS.
       0000-MAIN-PROCESS.
      * Get current timestamp
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURRENT-YEAR DELIMITED BY SIZE
                  '-'
                  WS-CURRENT-MONTH DELIMITED BY SIZE
                  '-'
                  WS-CURRENT-DAY DELIMITED BY SIZE
                  ' '
                  WS-CURRENT-HOUR DELIMITED BY SIZE
                  ':'
                  WS-CURRENT-MINUTE DELIMITED BY SIZE
                  ':'
                  WS-CURRENT-SECOND DELIMITED BY SIZE
             INTO WS-TIMESTAMP
           END-STRING
           
      * If file not opened, open it
           IF NOT FILE-IS-OPENED
               OPEN EXTEND LOG-FILE
               IF WS-FILE-STATUS = "35"
                   OPEN OUTPUT LOG-FILE
               END-IF
               MOVE 'Y' TO WS-FILE-OPENED
           END-IF
           
      * Format log line
           STRING WS-TIMESTAMP DELIMITED BY SIZE
                  ' ['
                  LOG-LEVEL DELIMITED BY SIZE
                  '] '
                  LOG-MODULE DELIMITED BY SIZE
                  ' - '
                  LOG-MESSAGE DELIMITED BY SIZE
             INTO WS-LOG-LINE
           END-STRING
           
      * Write log
           WRITE LOG-RECORD FROM WS-LOG-LINE
           
           MOVE '0' TO LOG-RETURN-CODE
           
           GOBACK.
       END PROGRAM INSPMLOG_EN.
