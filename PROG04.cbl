 IDENTIFICATION DIVISION.
 PROGRAM-ID. PROG04.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-DATE-6          PIC 9(06).
 01  WS-DATE-8          PIC 9(08).
 01  WS-TIME            PIC 9(08).
 01  ws-variable        pic x(10).
 PROCEDURE DIVISION.
     ACCEPT WS-DATE-6 FROM DATE
     ACCEPT WS-DATE-8 FROM DATE
     ACCEPT WS-TIME   FROM TIME
     DISPLAY '==========='
     DISPLAY 'CURRENT DATE1 ', WS-DATE-6
     DISPLAY '==========='
     DISPLAY 'CURRENT DATE2 ', WS-DATE-8
     DISPLAY '==========='
     DISPLAY 'CURRENT TIME  ', WS-TIME
     DISPLAY '==========='
     accept ws-variable
     STOP RUN.
