      *    *** ACCEPT Œ…“ü—Íƒ`ƒFƒbƒN

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM10.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F.
       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM10".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM10.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM10.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-IMG          PIC  X(500) VALUE SPACE.
           03  WK-HTTPS        PIC  X(500) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** POT1 ’Ç‰Á
           PERFORM UNTIL SW-YES =      "Y"
                   PERFORM S110-10     THRU    S110-EX
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
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *     IF      WK-PIN1-CNT >= 1 AND <=250
      *             MOVE    "P"         TO      WFD-ID
      *             MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *             MOVE    1           TO      WFD-SU
      *             MOVE    100         TO      WFD-LEN
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC
      *     END-IF
           .
       S010-EX.
           EXIT.

      *    *** PIN1 READ
       S020-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS NOT = ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           WRITE   POT1-REC    FROM    PIN1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO     WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " ƒ^ƒCƒgƒ‹ “ü—Í"
                   ACCEPT  WK-TITLE
      *             DISPLAY WK-PGM-NAME " ƒ^ƒCƒgƒ‹ OK ? Y/N"
      *             ACCEPT  SW-YES
                   MOVE    "Y"         TO      SW-YES
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " ‚h‚l‚f “ü—Í"
                   ACCEPT  WK-IMG
      *             DISPLAY WK-PGM-NAME " ‚h‚l‚f OK ? Y/N"
      *             ACCEPT  SW-YES
                   MOVE    "Y"         TO      SW-YES
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " ‚g‚s‚s‚o‚r “ü—Í"
                   ACCEPT  WK-HTTPS
      *             DISPLAY WK-PGM-NAME " ‚g‚s‚s‚o‚r OK ? Y/N"
      *             ACCEPT  SW-YES
                   MOVE    "Y"         TO      SW-YES
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           STRING  WK-TITLE DELIMITED BY "  "
                   " ,"     DELIMITED SIZE
                   WK-IMG   DELIMITED BY SPACE
                   " ,"     DELIMITED SIZE
                   WK-HTTPS DELIMITED BY SPACE
                   " ,"     DELIMITED SIZE
              INTO POT1-REC
           END-STRING

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO     WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           DISPLAY WK-PGM-NAME " INPUT END Y OR N"
           ACCEPT  SW-YES
           .
       S110-EX.
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
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
