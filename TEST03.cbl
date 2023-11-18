      *    *** SORT ƒTƒ“ƒvƒ‹

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST03.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
                               STATUS   WK-SIO1-STATUS.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
           03  SIO1-KEY1       PIC  9(010).
           03  SIO1-KEY2       PIC  9(010).
           03  FILLER          PIC  X(060).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  FILLER          PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST03  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST03.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32X.POT1".
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "TEST03.SIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST03.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-SIO1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1-RL-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1-RT-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1-RL-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1-RT-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           SORT    SIO1-F
                   ASCENDING  KEY SIO1-KEY1
                   ASCENDING  KEY SIO1-KEY2
      *             DESCENDING KEY SIO1-KEY2

      *    *** READ AND RELEASE
                   INPUT  PROCEDURE S100-SEC   THRU    S100-EX

      *    *** RETURN AND WRITE
                   OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-SEC                SECTION.
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *    *** SORT-F ‚ÍOPEN ‚¢‚ç‚È‚¢
           OPEN    INPUT       PIN1-F
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
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ AND RELEASE
       S100-SEC                SECTION.
       S100-10.

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                           ADD     1           TO      WK-PIN1-CNT
                           RELEASE SIO1-REC    FROM    PIN1-REC

                           IF  WK-SIO1-STATUS = ZERO
                               ADD     1           TO     WK-SIO1-RL-CNT
                           ELSE
                               DISPLAY WK-PGM-NAME
                                       " SIO1-F RELEASE ERROR STATUS="
                                       WK-SIO1-STATUS
                               STOP    RUN
                           END-IF
                   ELSE
                       IF  WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.

           PERFORM UNTIL WK-SIO1-EOF = HIGH-VALUE

                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF

                       NOT AT END
                           WRITE   POT1-REC    FROM    SIO1-REC
                           IF      WK-POT1-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                               STOP    RUN
                           END-IF
                           ADD     1           TO      WK-POT1-CNT
                           ADD     1           TO      WK-SIO1-RT-CNT
                   END-RETURN

                   IF  WK-SIO1-STATUS NOT = ZERO AND 10
                       DISPLAY WK-PGM-NAME
                               " SIO1-F RETURN ERROR STATUS="
                               WK-SIO1-STATUS
                       STOP    RUN
                   END-IF
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-SEC                SECTION.
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
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-SIO1-RL-CNT TO   WK-SIO1-RL-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RL¹Ý½³= " WK-SIO1-RL-CNT-E
                   " (" WK-SIO1-F-NAME ")"
           MOVE    WK-SIO1-RT-CNT TO   WK-SIO1-RT-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RT¹Ý½³= " WK-SIO1-RT-CNT-E
                   " (" WK-SIO1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
