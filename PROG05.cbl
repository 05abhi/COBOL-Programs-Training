 IDENTIFICATION DIVISION.
 PROGRAM-ID. PROG05.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-INPUT1.
     03 WS-IN1              PIC X(10).
     03 WS-IN2              PIC X(10).
 01  WS-OUTPUT1.
     03 WS-OUT1             PIC X(10).
     03 WS-OUT2             PIC X(10).
 01  WS-INPUT2              PIC X(5).
 01  WS-OUTPUT2             PIC X(3).
 01  WS-VARIABLE            PIC X(10). 
*
 PROCEDURE DIVISION.
     ACCEPT WS-INPUT1
     MOVE WS-IN1 TO WS-OUT1
     MOVE WS-IN2 TO WS-OUT2
*
     ACCEPT WS-INPUT2
     MOVE WS-INPUT2 TO WS-OUTPUT2
*
     DISPLAY '-----------------------------'
     DISPLAY 'INPUT1 - ', WS-INPUT1
     DISPLAY 'OUTPUT1 - ', WS-OUTPUT1
     DISPLAY '-----------------------------'
*
     DISPLAY 'INPUT2 - ', WS-INPUT2
     DISPLAY 'OUTPUT2- ', WS-OUTPUT2
     DISPLAY '-----------------------------'
     ACCEPT WS-VARIABLE.
     STOP RUN.
