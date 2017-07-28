 IDENTIFICATION DIVISION.
 PROGRAM-ID. PROG07.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-A              PIC 9(02).
 01  WS-B              PIC 9(02).
 01  WS-OUTPUT.
     03 WS-ADD         PIC 9(05).
     03 WS-SUB         PIC S9(02).
     03 WS-MUL         PIC 9(03).
     03 WS-DIV         PIC 9(02)V99.
 01  WS-DISPLAY.
     03 WS-NEG         PIC -9(02).
     03 WS-DEC         PIC 9(02).99.
 01  WS-DISPLAY.
     03 WS-O2          PIC Z9.
     03 WS-O3          PIC ZZ9.
     03 WS-O4          PIC ZZZ9.
     03 WS-O5          PIC ZZZZ9.
 01  WS-VARIABLE       PIC X(09).
 PROCEDURE DIVISION.
     ACCEPT WS-A
     ACCEPT WS-B
     IF WS-A IS NOT NUMERIC OR WS-B IS NOT NUMERIC
        DISPLAY 'INPUTS ARE NOT NUMERIC'
        STOP RUN
     ELSE
*
     DISPLAY '-----------------------------'
     DISPLAY 'A - ', WS-A
     DISPLAY 'B - ', WS-B
     DISPLAY '-----------------------------'
     DISPLAY 'OUTPUT - '
*
     ADD WS-A TO WS-B GIVING WS-ADD
     MOVE WS-ADD TO WS-O5
     DISPLAY 'ADDITION - ', WS-O5
*
     SUBTRACT WS-A FROM WS-B GIVING WS-SUB
     MOVE WS-SUB TO WS-NEG
     DISPLAY 'SUBTRACTION - ', WS-NEG
*
     MULTIPLY WS-A BY WS-B GIVING WS-MUL
     DISPLAY 'MULTIPLICATION - ', WS-MUL
*
     IF WS-B = 0
        DISPLAY 'ZERO DIVIDE NOT POSSIBLE - POSITIVE CHECK'
     ELSE
        DIVIDE WS-A BY WS-B GIVING WS-DIV
        MOVE WS-DIV TO WS-DEC
        DISPLAY 'DIVISION - ', WS-DEC
     END-IF.
*
     ACCEPT WS-VARIABLE
     STOP RUN.
