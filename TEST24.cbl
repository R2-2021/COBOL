      *    *** INDEX FILE RANDOM READ “ü—Í
      *    *** SEQ FILE o—Í

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST24.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION INDEXED
           ACCESS RANDOM
           RECORD KEY PIN1-KEY.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-KEY        PIC  9(004).
           03  PIN1-DATA       PIC  X(2048).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(2052).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST24  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST22.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST24.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-DATA1        PIC S9V9(9) VALUE ZERO.
           03  WK-DATA2        PIC  9(10)  VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 10

                   MOVE    FUNCTION RANDOM TO  WK-DATA1
                   COMPUTE WK-DATA2 = WK-DATA1 * 100
                   MOVE    WK-DATA2    TO      PIN1-KEY
                   IF      PIN1-KEY    =       ZERO
                           MOVE    1           TO      PIN1-KEY
                   END-IF

      *    *** READ PIN1
                   PERFORM S100-10     THRU    S100-EX

                   IF      WK-PIN1-STATUS =    ZERO
      *    *** WRITE POT1
                       PERFORM S200-10     THRU    S200-EX
                   END-IF
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

           OPEN    I-O         PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE   "M"          TO      WFD-TYPE
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S100-10.

           READ    PIN1-F KEY PIN1-KEY
               INVALID KEY
                  DISPLAY WK-PGM-NAME " PIN1-F READ ERROR KEY=" PIN1-KEY
      *             CONTINUE
               NOT INVALID
                   CONTINUE
           END-READ

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
      *    *** 23 READ INVALID KEY 
               IF  WK-PIN1-STATUS =    23
                   CONTINUE
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S200-10.

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC

           MOVE    PIN1-REC    TO      POT1-REC
           WRITE   POT1-REC
           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .

       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
