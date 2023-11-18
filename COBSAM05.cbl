      *    *** サブルーチン　DATETIME CHECK

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM05.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
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

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-I1         PIC  X(100).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-I1         PIC  X(100).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM05".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM05.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM05.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-DATA1        PIC  X(001) VALUE "A".
           03  WK-DATA2        PIC  X(010) VALUE "ABC" & "DEF".
           03  WK-DATA3        PIC  9(010) VALUE ZERO.
           03  WK-DATA4        PIC  9V9(5) VALUE ZERO.
           03  WK-DATA5        PIC S9(9)V99 VALUE ZERO.
           03  WK-DATA5-X      REDEFINES WK-DATA5
                               PIC  X(011).
           03  WK-DATA6        PIC  X(010) VALUE  SPACE.
           03  WK-DATA7        PIC  X(040) VALUE
               "3D彼女 リアルガール(第2シーズン)".
           03  WK-SEED         PIC  9(036) VALUE  ZERO.
           03  WDT-DATETIME    PIC  X(021) VALUE  ZERO.

           03  WK-A1           COMP-2 VALUE 12.34.
           03  WK-A2           COMP-2 VALUE 56.78.
           03  WK-A3           COMP-2 VALUE 0.
           03  WK-A4           PIC  X(010) VALUE "　".
           03  WK-A5           PIC  X(10100) VALUE ALL "ABCDE".
      *     03  WK-LEN          PIC  9(005) SYNC VALUE 5.
      *     03  WK-LEN          BINARY-LONG VALUE 5.
           03  WK-LEN          BINARY-LONG SYNC VALUE 5.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16
                               PIC  X(001).

       01  PIC-XX.
           05  FILLER          PIC  X(001) VALUE LOW-VALUE.
           05  PIC-X           PIC  X(001) VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX
                               PIC  9(004) COMP-X.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-I       BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-KEY     PIC  X(010) VALUE SPACE.
             05  TBL01-DATA    PIC  X(070) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-1           USAGE BINARY-LONG VALUE 1.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  O               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-I            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** 処理
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
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    SPACE       TO      POT1-REC
           .

       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT

                   CALL    "CBL_OC_DUMP" USING   PIN1-REC

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     MOVE    "PIN1-REC"  TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC

           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** 処理
       S100-10.

           COMPUTE WK-A3 =  WK-A1 * WK-A2
           DISPLAY "WK-A1=" WK-A1
           DISPLAY "WK-A2=" WK-A2
           DISPLAY "WK-A3=" WK-A3

           DISPLAY " "
           DISPLAY "WK-A1"
           CALL    "COBDUMP"    USING   WK-A1

           DISPLAY " "
           DISPLAY "WK-A2"
           CALL    "COBDUMP"    USING   WK-A2

           DISPLAY " "
           DISPLAY "WK-A3"
           CALL    "COBDUMP"    USING   WK-A3 WK-LEN

           DISPLAY " "
           DISPLAY "WK-A4"
           CALL    "COBDUMP"    USING   WK-A4

      *     DISPLAY " "
      *     DISPLAY "WK-A5"
      *     CALL    "COBDUMP"    USING   WK-A5

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
