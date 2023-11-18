      *    *** BINARY SEQUENTIAL ƒŒƒR[ƒh’· •¡” ‚s‚d‚r‚s

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM08.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING IN SIZE FROM 1 TO 1080 
      *     RECORD VARYING IN SIZE TO 100 
      *     RECORD VARYING DEPENDING ON WK-PIN1-LEN.

       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(120).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING IN SIZE FROM 1 TO 1080
      *     RECORD VARYING DEPENDING ON WK-POT1-LEN.
       01  POT1-REC.
           03  POT1-DATA       PIC  X(120).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM08".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM08.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST38.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) 
               VALUE "youtube.YUIKAORI2.html".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM08.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.

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
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO OR 4
                   ADD     1           TO      WK-PIN1-CNT
      *             DISPLAY "PIN1-LEN=" WK-PIN1-LEN

           IF WK-PIN1-CNT >= 1 AND <=250
           MOVE    "P"         TO      WFD-ID
           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           MOVE    1           TO      WFD-SU
           MOVE    100         TO      WFD-LEN
           CALL    "FILEDUMP"   USING  WFD-FILEDUMP-AREA
                                       PIN1-REC
           END-IF
           ELSE
                   IF      WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

      *     MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
      *     MOVE    1080          TO      WK-POT1-LEN
           MOVE    PIN1-DATA   TO      POT1-DATA
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     MOVE    2           TO      WFD-SU
      *     MOVE    100         TO      WFD-LEN
      *     CALL    "FILEDUMP"   USING  WFD-FILEDUMP-AREA
      *                                 POT1-REC

           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
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
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
