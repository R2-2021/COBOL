      *    *** ORGANIZATION RECORD BINARY SEQUENTIAL TEST
      *    *** INPUT,OUTPUT 同じレコード長にすれば、FILEITEM BYNARY INPUT
      *    *** で処理可能になる

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM07.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *     ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *     ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(10).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-POT1-LEN.
       01  POT1-REC            PIC  X(10).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM07".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM07.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST38.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM07.POT1"

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WFD-ID1         PIC  X(010) VALUE "ABCDEABCDE".
           03  WFD-ID1X        PIC  X(011) VALUE "ABCDEABCDE ".
           03  WFD-ID2         PIC  9(005) VALUE ZERO.
           03  WFD-ID3         PIC  X(002) VALUE "CD".
           03  WFD-ID3X        PIC  X(002) VALUE "AB".
           03  WFD-ID8         PIC  X(002) VALUE "DE".
           03  WK-ZONE         PIC  S9(10)V99 VALUE -12345.67. 
           03  WK-PACK         PIC  S9(10)V99 VALUE -12345.67 
                                           PACKED-DECIMAL.
           03  WK-NUMVAL       PIC  S9(10)V99 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-I       BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-KEY     PIC X(010) VALUE SPACE.
             05  TBL01-DATA    PIC X(070) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG SYNC VALUE 1.

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

      *    *** 行NOカット
           PERFORM S100-10     THRU    S100-EX
     *             UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

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
           .

       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

      *     MOVE    HIGH-VALUE  TO      PIN1-REC
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO OR 04
                   ADD     1           TO        WK-PIN1-CNT

                   MOVE    "P"         TO      WFD-ID
                   MOVE    1           TO      WFD-SU
                   MOVE    WK-PIN1-CNT TO      WFD-SEQ
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               PIN1-REC
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

      *    *** TEST
       S100-10.

      *     PERFORM VARYING I FROM 1 BY 1
      *             UNTIL PIN1-REC (I:1) = SPACE
      *             MOVE    SPACE       TO     PIN1-REC (I:1)
      *     END-PERFORM

           MOVE    HIGH-VALUE  TO      POT1-REC
           MOVE    PIN1-REC    TO      POT1-REC
           MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
      *    *** IN=10 BYTE,OT=9 BYTE だとX"0D" しか入らない
      *    *** IN=10 BYTE,OT=12 BYTE だとSPACE 2BYTE 追加するのでINPUT と
      *    *** 位置がずれる
      *    *** ＩＮ＝＞ＯＵＴ 同じレコード長でないとダメ
      *     MOVE    12           TO      WK-POT1-LEN
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           PERFORM S100-10     THRU    S100-EX

           MOVE    "P"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           MOVE    WK-POT1-CNT TO      WFD-SEQ
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT = ZERO
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
