      *    *** Floydfs triangle

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST114.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       DATA                    DIVISION.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST114 ".

      *    *** 
           03  WK-LINE         PIC  9(004) VALUE 5.
           03  WK-NUM          PIC  9(004) VALUE ZERO.
           03  WK-NUM-D        PIC  BZZZ9  VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL L > WK-LINE
      *    *** Floydfs triangle
                   PERFORM S100-10     THRU    S100-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-PGM-NAME

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** Floydfs triangle
       S100-10.

           PERFORM VARYING L2 FROM 1 BY 1
                   UNTIL   L2 > L

                   ADD     1           TO      WK-NUM
                   MOVE    WK-NUM      TO      WK-NUM-D

                   IF      L2          =       L
                           DISPLAY WK-NUM-D
                   ELSE
                           DISPLAY WK-NUM-D NO ADVANCING
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-PGM-NAME

           DISPLAY WK-PGM-NAME " END"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
