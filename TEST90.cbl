      *    *** ’a¶“úA‚P–œ“úŒãA‚Q–œ“úŒãA‚R–œ“úŒã

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST90.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** –¢Žg—p
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ’a¶“úƒf[ƒ^
      *    *** C.FILEITEM.T023.bat
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F.
       01  POT1-REC.
           03  POT1-B-NISUU    PIC  9(006).
           03                  PIC  X(001).
           03  POT1-BIR-DATE.
             05  POT1-B-YYYY   PIC  9(004).
             05  POT1-B-P1     PIC  X(001).
             05  POT1-B-MM     PIC  9(002).
             05  POT1-B-P2     PIC  X(001).
             05  POT1-B-DD     PIC  9(002).
             05  POT1-B-WEEK-NK PIC N(001).
           03                  PIC  X(001).
           03  POT1-1-NISUU    PIC  9(006).
           03                  PIC  X(001).
           03  POT1-1-DATE.
             05  POT1-1-YYYY   PIC  9(004).
             05  POT1-1-P1     PIC  X(001).
             05  POT1-1-MM     PIC  9(002).
             05  POT1-1-P2     PIC  X(001).
             05  POT1-1-DD     PIC  9(002).
             05  POT1-1-WEEK-NK PIC N(001).
           03                  PIC  X(001).
           03  POT1-2-NISUU    PIC  9(006).
           03                  PIC  X(001).
           03  POT1-2-DATE.
             05  POT1-2-YYYY   PIC  9(004).
             05  POT1-2-P1     PIC  X(001).
             05  POT1-2-MM     PIC  9(002).
             05  POT1-2-P2     PIC  X(001).
             05  POT1-2-DD     PIC  9(002).
             05  POT1-2-WEEK-NK PIC N(001).
           03                  PIC  X(001).
           03  POT1-3-NISUU    PIC  9(006).
           03                  PIC  X(001).
           03  POT1-3-DATE.
             05  POT1-3-YYYY   PIC  9(004).
             05  POT1-3-P1     PIC  X(001).
             05  POT1-3-MM     PIC  9(002).
             05  POT1-3-P2     PIC  X(001).
             05  POT1-3-DD     PIC  9(002).
             05  POT1-3-WEEK-NK PIC N(001).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST90  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST90.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST90.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
      *             PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** WRITE POT1 ’a¶“úAˆê–œ“úŒãŒvŽZ
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

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1 ’a¶“úA‚PC‚QC‚R–œ“úŒãŒvŽZ
       S100-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    "."         TO      POT1-B-P1
                                       POT1-B-P2
                                       POT1-1-P1
                                       POT1-1-P2
                                       POT1-2-P1
                                       POT1-2-P2
                                       POT1-3-P1
                                       POT1-3-P2

      *    *** 710000 1943.11.30‰Î
      *    *** 767500 2101.05.05–Ø
           PERFORM VARYING I FROM 710000 BY 1
                   UNTIL I > 767500

                   MOVE    "R"         TO      WDW-DATE2-ID
                   COMPUTE WDW-NISUU = I
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   MOVE    WDW-NISUU   TO      POT1-B-NISUU
                   MOVE    WDW-DATE2-YYYY TO   POT1-B-YYYY
                   MOVE    WDW-DATE2-MM TO     POT1-B-MM
                   MOVE    WDW-DATE2-DD TO     POT1-B-DD
                   MOVE    WDW-DATE2-WEEK-NK TO POT1-B-WEEK-NK

                   COMPUTE WDW-NISUU = I + 10000
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   MOVE    WDW-NISUU   TO      POT1-1-NISUU
                   MOVE    WDW-DATE2-YYYY TO   POT1-1-YYYY
                   MOVE    WDW-DATE2-MM TO     POT1-1-MM
                   MOVE    WDW-DATE2-DD TO     POT1-1-DD
                   MOVE    WDW-DATE2-WEEK-NK TO POT1-1-WEEK-NK

                   COMPUTE WDW-NISUU = I + 20000
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   MOVE    WDW-NISUU   TO      POT1-2-NISUU
                   MOVE    WDW-DATE2-YYYY TO   POT1-2-YYYY
                   MOVE    WDW-DATE2-MM TO     POT1-2-MM
                   MOVE    WDW-DATE2-DD TO     POT1-2-DD
                   MOVE    WDW-DATE2-WEEK-NK TO POT1-2-WEEK-NK

                   COMPUTE WDW-NISUU = I + 30000
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   MOVE    WDW-NISUU   TO      POT1-3-NISUU
                   MOVE    WDW-DATE2-YYYY TO   POT1-3-YYYY
                   MOVE    WDW-DATE2-MM TO     POT1-3-MM
                   MOVE    WDW-DATE2-DD TO     POT1-3-DD
                   MOVE    WDW-DATE2-WEEK-NK TO POT1-3-WEEK-NK

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                    " PIN1-F CLOSE ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           END-PERFORM
           .
       S100-EX.
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
                                       POT1-REC

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
