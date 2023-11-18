      *    *** ORGANIZATION RECORD BINARY SEQUENTIAL 調査

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM04.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *    *** バイナリーファイル下記だと、ちゃんと処理出来ない
      *    *** ＣＲＬＦ　レコード上に無くなる
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
       01  PIN1-REC.
           03  PIN1-I1         PIC  X(10000).
      
       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
      *    *** 可変長で出力すると、自動的に１，３にＭＸＸがセットされる
           03  POT1-I1         OCCURS 1 TO 10000
                               DEPENDING ON WK-POT1-LEN
                               PIC  X(001).
      *     03  FILLER          PIC  X(077).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM04".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM03.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM04.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     USAGE BINARY-LONG VALUE ZERO.
           03  WK-POT1-LEN     USAGE BINARY-LONG VALUE ZERO.

           03  WK-PIN1-CNT     USAGE BINARY-LONG VALUE ZERO.
           03  WK-POT1-CNT     USAGE BINARY-LONG VALUE ZERO.
          03  WK-STR          PIC  X(003) VALUE "%XX".
           03  WK-VAL          PIC  X(001) VALUE X"20".
           03  WK-DATA1        PIC  X(001) VALUE "A".
             88  WK-DATA1-88               VALUE "Z"
                                           FALSE "B".
      *    *** & は ALL があるとエラーになる
      *     03  WK-DATA2        PIC  X(010) VALUE  ALL "ABC" & "DEF".
           03  WK-DATA2        PIC  X(010) VALUE  "ABC" & "DEF".
           03  WK-DATA3        PIC  9(010) VALUE  ZERO.
           03  WK-DATA4        PIC  9V9(5) VALUE  ZERO.
           03  WK-DATA5        PIC S9(9)V99 VALUE ZERO.
           03  WK-DATA5-X      REDEFINES WK-DATA5
                               PIC  X(011).
           03  WK-DATA6        PIC  X(010) VALUE  SPACE.
           03  WK-DATA7        PIC  X(040) VALUE
               "3D彼女 リアルガール(第2シーズン)".
           03  WK-SEED         PIC  9(036) VALUE  ZERO.
           03  WDT-DATETIME     PIC  X(021) VALUE  ZERO.

           03  WK-A1           COMP-2 VALUE 12.34.
           03  WK-A2           COMP-2 VALUE 56.78.
           03  WK-A3           COMP-2 VALUE ZERO.

      *

      *     COPY copybook REPLACING ==:tag:== BY ==ws==   subitems BY 16.
      *     COPY copybook REPLACING ==:tag:== BY ==old==  subitems BY 0.

       01 form pic $-z(7)9.9(8).
       01 newline       PIC x VALUE x"0a"
      *    *** 38 文字まで指定可能
       01 big-value pic $$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$9.99.

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
             05  TBL01-I       USAGE BINARY-LONG VALUE ZERO.
             05  TBL01-KEY     PIC X(010) VALUE SPACE.
             05  TBL01-DATA    PIC X(070) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-1           USAGE BINARY-LONG VALUE 1.

       01  INDEX-AREA.
           03  I               USAGE BINARY-LONG VALUE ZERO.
           03  I-MAX           USAGE BINARY-LONG VALUE ZERO.
           03  I1              USAGE BINARY-LONG VALUE ZERO.
           03  I2              USAGE BINARY-LONG VALUE ZERO.
           03  J               USAGE BINARY-LONG VALUE ZERO.
           03  O               USAGE BINARY-LONG VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "0".

       01  SAVE-AREA.
           03  SV-I            USAGE BINARY-LONG VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** PIN1 READ
           PERFORM S100-10     THRU    S100-EX

      *    *** POT1 編集,WRITE
           PERFORM S200-10     THRU    S200-EX
                   UNTIL WK-PIN1-EOF = HIGH-VALUE

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

           MOVE    "P"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    "WK-DATA7"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA7
           .
       S010-EX.
           EXIT.

      *    *** PIN1 READ
       S100-10.

           MOVE    HIGH-VALUE  TO      PIN1-REC
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO OR 4
                   ADD     1           TO        WK-PIN1-CNT
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

           DISPLAY WK-PIN1-CNT " " WK-PIN1-LEN " " WK-PIN1-STATUS

           MOVE    "P"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    "PIN1-REC"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
           .
       S100-EX.
           EXIT.

      *    *** POT1 編集,WRITE
       S200-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 10000       OR
                           I > WK-PIN1-LEN
               IF      PIN1-REC (I:1)       = X"FF" AND 
                       PIN1-REC (I + 1 : 1) = X"FF"
                       MOVE    10000       TO      I
               ELSE
                   IF      PIN1-REC (I:1)       = X"0D" AND 
                           PIN1-REC (I + 1 : 1) = X"0A"
                           ADD     1           TO      O
                           COMPUTE WK-POT1-LEN = O + 1
                           MOVE    X"0D0A"     TO      POT1-REC (O:2)
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           IF WK-POT1-CNT = 1 OR 2
                             DISPLAY WK-POT1-LEN
                             DISPLAY POT1-REC (1:75)
                           END-IF

           MOVE    "P"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
                           MOVE    ZERO        TO      O
                           ADD     1           TO      I
                           MOVE    SPACE       TO      POT1-REC
                   ELSE
                           ADD     1           TO      O
                           MOVE    PIN1-REC (I:1) TO   POT1-REC (O:1)
                           IF WK-POT1-CNT = ZERO OR 1
                             DISPLAY "I="  I " " PIN1-REC (I:1)
                                     " O=" O " " POT1-REC (O:1)
                           END-IF
                   END-IF
               END-IF
           END-PERFORM

      *    *** PIN1 READ
           PERFORM S100-10     THRU    S100-EX

           IF      WK-PIN1-EOF =       HIGH-VALUE AND
                   O           >       ZERO
           DISPLAY "EOF ?" 
                   ADD     1           TO      O
                   COMPUTE WK-POT1-LEN = O + 1
                   MOVE    X"0D0A"     TO      POT1-REC (O:2)
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   MOVE    ZERO        TO      O

           MOVE    "P"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
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
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
