 IDENTIFICATION DIVISION.
 PROGRAM-ID. PROG06.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-A              PIC 9(02).
 01  WS-B              PIC 9(02).
 01  WS-OUTPUT.
     03 WS-ADD         PIC 9(05).
     03 WS-SUB         PIC S9(02).
     03 WS-MUL         PIC 9(02).
     03 WS-DIV         PIC 9(02)V99.
 01  WS-DISPLAY.
     03 WS-NEG         PIC -9(02).
     03 WS-DEC         PIC 9(02).99.
 01  WS-DISPLAY.
     03 WS-O2          PIC Z9.
     03 WS-O3          PIC ZZ9.
     03 WS-O4          PIC ZZZ9.
     03 WS-O5          PIC ZZZZ9.
 01  WS-VARIABLE       PIC X(10).
 PROCEDURE DIVISION.
     ACCEPT WS-A
     ACCEPT WS-B
*
     ADD WS-A TO WS-B GIVING WS-ADD
     SUBTRACT WS-A FROM WS-B GIVING WS-SUB
     MULTIPLY WS-A BY WS-B GIVING WS-MUL
     DIVIDE WS-A BY WS-B GIVING WS-DIV
*
     MOVE WS-SUB TO WS-NEG
     MOVE WS-DIV TO WS-DEC
     MOVE WS-ADD TO WS-O5
*
     DISPLAY '-----------------------------'
     DISPLAY 'A - ', WS-A
     DISPLAY 'B - ', WS-B
     DISPLAY '-----------------------------'
     DISPLAY 'OUTPUT - '
     DISPLAY 'ADDITION - ', WS-O5
     DISPLAY 'SUBTRACTION - ', WS-NEG
     DISPLAY 'MULTIPLICATION - ', WS-MUL
     DISPLAY 'DIVISION - ', WS-DEC
     DISPLAY '-----------------------------'
     ACCEPT WS-VARIABLE
     STOP RUN.
