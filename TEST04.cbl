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
           DISPLAY "PAR1 ���߯� �{��1�A�|�͂Q�A���͂R�A/�͂S"
           ACCEPT  WK-PAR1

      *    *** �T�C���t�A�����_���͉�
           DISPLAY "PAR2 �����@S9(5).99 ���߯�"
           ACCEPT  WK-PAR2

           DISPLAY "PAR3 �����@S9(5).99 ���߯�"
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
           DISPLAY "�v�Z����=" WK-PAR4-D.
           DISPLAY "TEST04 END".
       S900-EX.
           EXIT.
