      ******************************************************************
      * 保单验证子程序
      * 功能：验证保单ID是否存在于数据库中
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPOLVD.
       AUTHOR. DEMO.
       DATE-WRITTEN. 2025-07-15.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * SQL通信区域
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
           
       01  WS-POLICY-COUNT            PIC S9(4) COMP.
       
       LINKAGE SECTION.
       01  LS-POLICY-ID               PIC X(10).
       01  LS-SQLCODE                 PIC S9(9) COMP.
       
       PROCEDURE DIVISION USING LS-POLICY-ID, LS-SQLCODE.
       0000-MAIN-PROCESS.
      * 检查保单是否存在
           MOVE 0 TO WS-POLICY-COUNT
           
      * 这里在实际情况下会执行SQL查询
      * 为了模拟DB2操作，我们使用简单的判断逻辑
      * 在实际项目中，这里会有EXEC SQL语句
           IF LS-POLICY-ID = 'P000000001' OR 
              LS-POLICY-ID = 'P000000002' OR
              LS-POLICY-ID = 'P000000003' OR
              LS-POLICY-ID = 'P000000004' OR
              LS-POLICY-ID = 'P000000005' OR
              LS-POLICY-ID = 'P000000006' OR
              LS-POLICY-ID = 'P000000007' OR
              LS-POLICY-ID = 'P000000008'
               MOVE 1 TO WS-POLICY-COUNT
               MOVE 0 TO SQLCODE
           ELSE
               MOVE 0 TO WS-POLICY-COUNT
               MOVE 100 TO SQLCODE
           END-IF
           
      * 返回SQLCODE
           MOVE SQLCODE TO LS-SQLCODE
           
           GOBACK.
       END PROGRAM INSPOLVD. 
