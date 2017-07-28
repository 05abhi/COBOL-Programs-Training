 IDENTIFICATION DIVISION.
 PROGRAM-ID. FH06.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
* 
 FILE-CONTROL.
     SELECT EMPLOYEE-FILE-IN ASSIGN TO 'INPUTSORT.DAT'.
     SELECT WORK-FILE        ASSIGN TO 'WORKFILE.DAT'.
     SELECT EMPLOYEE-FILE    ASSIGN TO 'EMPFILE.DAT'.
     SELECT REPORT-FILE      ASSIGN TO 'REPORT.REP'.
*     
 DATA DIVISION.
 FILE SECTION.
 FD  EMPLOYEE-FILE-IN
     RECORD CONTAINS 50 CHARACTERS
     RECORDING MODE IS F.
 01  EMP-FILE-RECORD   PIC X(50).
 
 SD  WORK-FILE.
 01  WORK-RECORD.
     03 FILLER         PIC X(20). 
     03 W-EMP-LOC      PIC X(03).
     03 FILLER         PIC X(08).
     03 W-EMP-TECH     PIC X(04).    
     03 FILLER         PIC X(15).    
     
 FD  EMPLOYEE-FILE
     RECORD CONTAINS 50 CHARACTERS
     RECORDING MODE IS F.
 01  EMPLOYEE-RECORD.
     03 EMP-ID         PIC X(05).
     03 EMP-NAME       PIC X(15).
     03 EMP-LOC        PIC X(03).
     03 EMP-DOB.        
        04 EMP-DOB-Y   PIC X(04).
        04  EMP-DOB-M  PIC X(02).
        04  EMP-DOB-D  PIC X(02).
     03 EMP-TECH       PIC X(05).
     03 EMP-EARN       PIC 9(05)V99.
     03 EMP-DEDN       PIC 9(05)V99.
     
 FD  REPORT-FILE
     RECORD CONTAINS 80 CHARACTERS
     RECORDING MODE IS F.
 01  REPORT-RECORD PIC X(80).
* 
 WORKING-STORAGE SECTION.
 01  HEADING-LINE1.
     03  FILLER           PIC X(06)      VALUE 'DATE:'.
     03  P-DATE           PIC X(10)      VALUE SPACES.
     03  FILLER           PIC X(50)      VALUE SPACES.
     03  FILLER           PIC X(06)      VALUE 'TIME:'.
     03  P-TIME.
         05  WS-TIME-HH   PIC 9(02)      VALUE ZERO.
         05  FILLER       PIC X(01)      VALUE ':'.
         05  WS-TIME-MM   PIC 9(02)      VALUE ZERO.
         05  FILLER       PIC X(01)      VALUE ':'.
         05  WS-TIME-SS   PIC 9(02)      VALUE ZERO.
*         
 01  HEADING-LINE2.
     03  FILLER           PIC X(20) VALUE 'LOCATION-WISE / TECH'.
     03  FILLER           PIC X(46) VALUE 'TECHNOLOGY-WISE LIST'.
     03  FILLER           PIC X(06)      VALUE 'PAGE:'.
     03  P-PAGE           PIC 9(02)      VALUE 01.
* 
 01  HEADING-LINE3.    
     03  FILLER           PIC X(05)      VALUE 'LOC: '.
     03  H3-LOC           PIC X(03)      VALUE SPACES.
     03  FILLER           PIC X(08)      VALUE SPACES.
     03  FILLER           PIC X(06)      VALUE 'TECH: '.
     03  H3-TECH          PIC X(04)      VALUE SPACES.
* 
 01  HEADING-LINE4.
     03  FILLER           PIC X(06)      VALUE 'ID'.
     03  FILLER           PIC X(16)      VALUE 'NAME'.
     03  FILLER           PIC X(04)      VALUE 'LOC'.
     03  FILLER           PIC X(11)      VALUE 'BIRTH DATE'.
     03  FILLER           PIC X(06)      VALUE 'TECH'.
     03  FILLER           PIC X(11)      VALUE '  EARNINGS '.
     03  FILLER           PIC X(11)      VALUE 'DEDUCTIONS '.
     03  FILLER           PIC X(10)      VALUE ' TOTAL SAL'.
* 
 01  DETAIL-LINE.
     03  P-ID             PIC X(05)      VALUE SPACES.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-NAME           PIC X(15)      VALUE SPACES.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-LOC            PIC X(03)      VALUE SPACES.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-DOB            PIC X(10)      VALUE SPACES.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-TECH           PIC X(05)      VALUE SPACES.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-EARN           PIC ZZZ,ZZ9.99 VALUE ZERO.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-DEDN           PIC ZZZ,ZZ9.99 VALUE ZERO.
     03  FILLER           PIC X(01)      VALUE SPACES.
     03  P-SAL            PIC ZZZ,ZZ9.99 VALUE ZERO.
* 
 01  ENDING-LINE1-I.
     03  TT-EARN-I        PIC 9(06)V99   VALUE ZERO.
     03  TT-DEDN-I        PIC 9(06)V99   VALUE ZERO.
     03  TT-SAL-I         PIC 9(06)V99   VALUE ZERO.
     03  TT-LINE-I        PIC 9(02)      VALUE ZERO.
* 
 01  ENDING-LINE1.    
     03  FILLER           PIC X(26)      VALUE 'TECHNOLOGY TOTAL'.
     03  TT-LINE          PIC Z9.
     03  FILLER           PIC X(15)      VALUE ALL ' '. 
     03  TT-EARN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  TT-DEDN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  TT-SAL           PIC ZZZ,ZZ9.99.
* 
 01  ENDING-LINE2-I.
     03  LT-EARN-I        PIC 9(06)V99   VALUE ZERO.
     03  LT-DEDN-I        PIC 9(06)V99   VALUE ZERO.
     03  LT-SAL-I         PIC 9(06)V99   VALUE ZERO.
     03  LT-LINE-I        PIC 9(02)      VALUE ZERO.
* 
 01  ENDING-LINE2.    
     03  FILLER           PIC X(26)      VALUE 'LOCATION TOTALS'.
     03  LT-LINE          PIC Z9.
     03  FILLER           PIC X(15)      VALUE ALL ' '. 
     03  LT-EARN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  LT-DEDN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  LT-SAL           PIC ZZZ,ZZ9.99.
* 
 01  ENDING-LINE3-I.
     03  CT-EARN-I        PIC 9(06)V99   VALUE ZERO.
     03  CT-DEDN-I        PIC 9(06)V99   VALUE ZERO.
     03  CT-SAL-I         PIC 9(06)V99   VALUE ZERO.
     03  CT-LINE-I        PIC 9(02)      VALUE ZERO.
* 
 01  ENDING-LINE3.    
     03  FILLER           PIC X(26)      VALUE 'COMPANY TOTALS'.
     03  CT-LINE          PIC Z9.
     03  FILLER           PIC X(15)      VALUE ALL ' '. 
     03  CT-EARN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  CT-DEDN          PIC ZZZ,ZZ9.99.
     03  FILLER           PIC X(01)      VALUE ' '.
     03  CT-SAL           PIC ZZZ,ZZ9.99.
* 
 01  ENDING-LINE4.    
     03  FILLER           PIC X(34)      VALUE ALL '+'.
     03  FILLER           PIC X(11)      VALUE 'END OF PAGE'.
     03  FILLER           PIC X(35)      VALUE ALL '+'.
* 
 01  WS-VARIABLES.                                   
     03  WS-DATE          PIC 9(08)      VALUE ZERO.
     03  WS-TIME                         VALUE ZERO.
         05  WS-TIME-HH   PIC 9(02).
         05  WS-TIME-MM   PIC 9(02).
         05  WS-TIME-SS   PIC 9(02).
         05  WS-TIME-FS   PIC 9(02).
     03  WS-SAL           PIC 9(06)V99   VALUE ZERO.
     03  TEMP-LOC         PIC X(03)      VALUE SPACES.
     03  TEMP-TECH        PIC X(04)      VALUE SPACES.
 01  WS-FILE-STAT         PIC X(02).
      88  OPEN-SUCCESS                   VALUE '00'.
 01  WS-FILE-FLAG         PIC X(01)      VALUE 'N'.
      88  END-OF-FILE                    VALUE 'Y'.
 01  WS-REP-STAT          PIC X(02).
      88  OPEN-SUCCESS                   VALUE '00'.
 01  WS-REP-FLAG          PIC X(02)      VALUE 'NN'.
      88  END-OF-FILE1                   VALUE '10'.
 01  WS-TOT-EMP           PIC 9(02)      VALUE ZERO.
* 
 PROCEDURE DIVISION.
 0000-MAIN-PARA.
     PERFORM 1000-SORT-PARA
     PERFORM 2000-INITIAL-PARA
     PERFORM 5000-PROCESS-PARA UNTIL END-OF-FILE
     PERFORM 9000-END-PARA
     STOP RUN.
* 
 1000-SORT-PARA.
     SORT WORK-FILE
          ON ASCENDING KEY W-EMP-LOC
          ON ASCENDING KEY W-EMP-TECH
          USING EMPLOYEE-FILE-IN
          GIVING EMPLOYEE-FILE.     
* 
 2000-INITIAL-PARA.         
     OPEN INPUT EMPLOYEE-FILE
     OPEN OUTPUT REPORT-FILE
     PERFORM 3000-DATE-TIME-PARA
     PERFORM 4000-READ-PARA
     MOVE EMP-LOC   TO TEMP-LOC
     MOVE EMP-TECH  TO TEMP-TECH
     MOVE EMP-LOC   TO H3-LOC
     MOVE EMP-TECH  TO H3-TECH
     WRITE REPORT-RECORD FROM HEADING-LINE1
     WRITE REPORT-RECORD FROM HEADING-LINE2
     WRITE REPORT-RECORD FROM HEADING-LINE3
     WRITE REPORT-RECORD FROM HEADING-LINE4.
* 
 3000-DATE-TIME-PARA.
     ACCEPT WS-DATE FROM DATE
     ACCEPT WS-TIME FROM TIME
     MOVE WS-DATE(1:4)   TO P-DATE(7:4)
     MOVE WS-DATE(5:2)   TO P-DATE(4:2)
     MOVE WS-DATE(7:2)   TO P-DATE(1:2)
     MOVE '/'            TO P-DATE(3:1)
                            P-DATE(6:1)
     MOVE CORRESPONDING WS-TIME TO P-TIME.
* 
 4000-READ-PARA.
     READ EMPLOYEE-FILE
        AT END
           MOVE 'Y' TO WS-FILE-FLAG
     END-READ.
* 
 5000-PROCESS-PARA.    
     IF EMP-LOC = TEMP-LOC
        IF EMP-TECH = TEMP-TECH
           NEXT SENTENCE  
        ELSE   
           WRITE REPORT-RECORD FROM ENDING-LINE1 
           MOVE ZERO TO TT-EARN-I
           MOVE ZERO TO TT-DEDN-I
           MOVE ZERO TO TT-SAL-I
           MOVE ZERO TO TT-LINE-I
           PERFORM 6000-TECH-CHANGE-PARA  
        END-IF   
     ELSE   
        WRITE REPORT-RECORD FROM ENDING-LINE1
        WRITE REPORT-RECORD FROM ENDING-LINE2
           MOVE ZERO TO TT-EARN-I
           MOVE ZERO TO TT-DEDN-I
           MOVE ZERO TO TT-SAL-I
           MOVE ZERO TO TT-LINE-I
           MOVE ZERO TO LT-EARN-I
           MOVE ZERO TO LT-DEDN-I
           MOVE ZERO TO LT-SAL-I
           MOVE ZERO TO LT-LINE-I
        PERFORM 6000-TECH-CHANGE-PARA
        PERFORM 7000-LOC-CHANGE-PARA
     END-IF.   
     PERFORM 8000-PRINT-PARA
     PERFORM 4000-READ-PARA.
* 
 6000-TECH-CHANGE-PARA.    
     MOVE EMP-TECH       TO TEMP-TECH
     MOVE EMP-LOC        TO H3-LOC
     MOVE EMP-TECH       TO H3-TECH
     ADD 1               TO P-PAGE
     WRITE REPORT-RECORD FROM ENDING-LINE4
     WRITE REPORT-RECORD FROM HEADING-LINE1 AFTER ADVANCING PAGE
     WRITE REPORT-RECORD FROM HEADING-LINE2
     WRITE REPORT-RECORD FROM HEADING-LINE3
     WRITE REPORT-RECORD FROM HEADING-LINE4.
* 
 7000-LOC-CHANGE-PARA.
     MOVE EMP-LOC        TO TEMP-LOC
     MOVE EMP-LOC        TO H3-LOC
     MOVE EMP-TECH       TO H3-TECH.
* 
 8000-PRINT-PARA.
     MOVE EMP-ID         TO P-ID.
     MOVE EMP-NAME       TO P-NAME.
     MOVE EMP-LOC        TO P-LOC.
     MOVE EMP-TECH       TO P-TECH.
     MOVE EMP-EARN       TO P-EARN.
     MOVE EMP-DEDN       TO P-DEDN.
     MOVE EMP-DOB(1:4)   TO P-DOB(7:4)
     MOVE EMP-DOB(5:2)   TO P-DOB(4:2)
     MOVE EMP-DOB(7:2)   TO P-DOB(1:2)
     MOVE '/'            TO P-DOB(3:1)
                            P-DOB(6:1)
     COMPUTE WS-SAL = EMP-EARN - EMP-DEDN
     MOVE WS-SAL         TO P-SAL.
     ADD EMP-EARN        TO CT-EARN-I
     ADD EMP-DEDN        TO CT-DEDN-I
     ADD 1               TO CT-LINE-I
     ADD EMP-EARN        TO TT-EARN-I
     ADD EMP-DEDN        TO TT-DEDN-I
     ADD 1               TO TT-LINE-I
     MOVE TT-LINE-I      TO TT-LINE
     MOVE TT-EARN-I      TO TT-EARN
     MOVE TT-DEDN-I      TO TT-DEDN
     SUBTRACT TT-DEDN-I  FROM TT-EARN-I GIVING TT-SAL-I
     MOVE TT-SAL-I       TO TT-SAL
     ADD EMP-EARN        TO LT-EARN-I
     ADD EMP-DEDN        TO LT-DEDN-I
     ADD 1               TO LT-LINE-I
     MOVE LT-LINE-I      TO LT-LINE
     MOVE LT-EARN-I      TO LT-EARN
     MOVE LT-DEDN-I      TO LT-DEDN
     SUBTRACT LT-DEDN-I  FROM LT-EARN-I GIVING LT-SAL-I
     MOVE LT-SAL-I       TO LT-SAL
     WRITE REPORT-RECORD FROM DETAIL-LINE.
* 
 9000-END-PARA.
     SUBTRACT CT-DEDN-I  FROM CT-EARN-I GIVING CT-SAL-I
     MOVE CT-EARN-I      TO CT-EARN 
     MOVE CT-DEDN-I      TO CT-DEDN
     MOVE CT-SAL-I       TO CT-SAL
     MOVE CT-LINE-I      TO CT-LINE
     WRITE REPORT-RECORD FROM ENDING-LINE1
     WRITE REPORT-RECORD FROM ENDING-LINE2
     WRITE REPORT-RECORD FROM ENDING-LINE3
     WRITE REPORT-RECORD FROM ENDING-LINE4
     CLOSE EMPLOYEE-FILE
     CLOSE REPORT-FILE.
*
