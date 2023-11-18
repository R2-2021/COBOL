      *    *** CALL TEST

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST04.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PAR1         PIC  9(005) VALUE ZERO.
           03  WK-PAR2         PIC S9(005)V99 VALUE ZERO.
           03  WK-PAR3         PIC S9(005)V99 VALUE ZERO.
           03  WK-PAR4         PIC S9(005)V99 VALUE ZERO.
           03  WK-PAR4-D       PIC ---,--9.99.

           03  WK-PIN1-CNT     USAGE BINARY-LONG VALUE 0.
           03  WK-POT1-CNT     USAGE BINARY-LONG VALUE 0.

       PROCEDURE   DIVISION.
       M100-10.

           DISPLAY "TEST04 START"
           DISPLAY "PAR1 ｲﾝﾌﾟｯﾄ ＋は1、－は２、＊は３、/は４"
           ACCEPT  WK-PAR1

      *    *** サイン付、小数点入力可
           DISPLAY "PAR2 数字　S9(5).99 ｲﾝﾌﾟｯﾄ"
           ACCEPT  WK-PAR2

           DISPLAY "PAR3 数字　S9(5).99 ｲﾝﾌﾟｯﾄ"
           ACCEPT  WK-PAR3

           CALL    "TEST04S1"  USING    WK-PAR1 WK-PAR2 WK-PAR3 WK-PAR4

      *    *** END
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** END
       S900-10.
           MOVE    WK-PAR4     TO      WK-PAR4-D.
           DISPLAY "計算結果=" WK-PAR4-D.
           DISPLAY "TEST04 END".
       S900-EX.
           EXIT.
