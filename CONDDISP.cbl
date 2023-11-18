      *    *** JOB COND DISPLAY (RETURN-CODE)

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             CONDDISP.

       DATA                    DIVISION.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "CONDDISP".

           03  WK-REC          PIC  X(100) VALUE SPACE.
           03  WK-RETURN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-A            PIC S9(009)V99 VALUE ZERO.
           03  WK-B            PIC S9(009)V99 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** RETURN-CODE DISPLAY
           PERFORM S100-10     THRU    S100-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-REC

           ACCEPT  WK-RETURN   FROM    ENVIRONMENT "RETURN-CODE"

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** RETURN-CODE DISPLAY
       S100-10.

           DISPLAY WK-PGM-NAME " RETURN-CODE" WK-RETURN
           COMPUTE WK-A = ( 1.00 / 3.00 ) * 3.00
           DISPLAY WK-A
           
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-REC

           DISPLAY WK-PGM-NAME " END"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
