      *    *** 文字タイプチェック USAGE 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM03.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.

       REPOSITORY.
      *    *** この指定をしないと、PI,E はエラーになる
           FUNCTION ALL INTRINSIC.

       SPECIAL-NAMES.
      *    *** この指定で、＄＝＞￥ 編集記号￥使える
           CURRENCY SIGN IS "\".

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *    ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-KEY        PIC  X(010).
           03  PIN1-DATA       PIC  X(070).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-ZD         PIC S9(009).
           03  FILLER          PIC  X(001).
           03  POT1-PD         PIC S9(015) PACKED-DECIMAL.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-1                 COMP-1.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-2                 COMP-2.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-3     PIC S9(009) COMP-3.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-4     PIC S9(009) COMP-4.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-5     PIC S9(009) COMP-5.
           03  FILLER          PIC  X(001).
           03  POT1-COMP-X     PIC S9(009) COMP-X.
           03  FILLER          PIC  X(001).
           03  POT1-BINARY     PIC S9(009) BINARY.
           03  FILLER          PIC  X(001).
      *    03  POT1-BINARY-C-LONG                BINARY-C-CHAR
      *    03  FILLER          PIC  X(001).
           03  POT1-BINARY-CHAR                  BINARY-CHAR.
           03  FILLER          PIC  X(001).
           03  POT1-BINARY-DOUBLE                BINARY-DOUBLE.
           03  FILLER          PIC  X(001).
      *    03  POT1-BINARY-INT                   BINARY-INT.
      *    03  FILLER          PIC  X(001).
           03  POT1-BINARY-LONG                  BINARY-LONG.
           03  FILLER          PIC  X(001).
      *    03  POT1-BINARY-LONG-LONG             BINARY-LONG-LONG.
      *    03  FILLER          PIC  X(001).
           03  POT1-BINARY-SHORT                 BINARY-SHORT.
           03  FILLER          PIC  X(001).
           03  POT1-CRLF       PIC  X(002).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM03".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM03.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM03.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "COBSAM03.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

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
      *     03  WK-KIN1         PIC  -$$$,$$$,$$9 VALUE ZERO.
           03  WK-KIN2         PIC  -\\\\,\\\,\\9 VALUE ZERO.
           03  WK-KIN3         PIC  \\\\,\\\,\\9- VALUE ZERO.

           03  WK-SEED         PIC  9(006) VALUE  ZERO.
           03  WDT-DATETIME    PIC  X(021) VALUE  ZERO.

           03  WK-A1           COMP-2 VALUE 12.34.
           03  WK-A2           COMP-2 VALUE 56.78.
           03  WK-A3           COMP-2 VALUE ZERO.

           03  WK-B1           PIC  X(015) VALUE ALL "ABC".
           03  WK-B2           PIC  X(015) VALUE
      *    *** ＫＥＹ１計
               X"EFBCABEFBCA5EFBCB9EFBC91E8A888".
           03  WK-B3.
      *    *** ABCＫＥＹ１計
             05                PIC  X(003) VALUE "ABC"
             05                PIC  X(012) VALUE
               X"EFBCA5EFBCB9EFBC91E8A888".
      *    *** ＫＥＹ１計
           03  WK-B4
             05                PIC  X(004) VALUE X"EFBDB1".
             05                PIC  X(012) VALUE
               X"EFBCA5EFBCB9EFBC91E8A888".
           03  WK-B5           PIC  X(015) VALUE ALL "1Aa".
           03  WK-B6           PIC  X(015) VALUE ALL "\1A".

       01  IN-DATA IS          GLOBAL PIC X(80).
       01  ED-DATA IS          GLOBAL.
           02                  ELM OCCURS 8 TIMES PIC X(16).

       01 Begin-Time.
           05 BT-HH            PIC 9(2).
           05 BT-MM            PIC 9(2).
           05 BT-SS            PIC 9(2).
           05 BT-HU            PIC 9(2).
       01  Binary-Item         BINARY-LONG SIGNED VALUE ZERO.
       01  Comp-Item           COMP    PIC S9(9) VALUE ZERO.
       01  Comp-5-Item         COMP-5  PIC S9(9) VALUE ZERO.
       01  Display-Item        DISPLAY PIC S9(9) VALUE ZERO.
       01  End-Time.
           05 ET-HH            PIC 9(2).
           05 ET-MM            PIC 9(2).
           05 ET-SS            PIC 9(2).
           05 ET-HU            PIC 9(2).
       78 Repeat-Count        VALUE 10000000.
      *78  Repeat-Count        VALUE 100.
       01  Time-Diff           PIC ZZ9.99.

      *

      *    COPY copybook REPLACING ==:tag:== BY ==ws==   subitems BY 16.
      *    COPY copybook REPLACING ==:tag:== BY ==old==  subitems BY 0.

       01  form                pic \-z(7)9.9(8).
       01  newline             PIC x VALUE x"0a"
      *    *** 38 文字まで指定可能
      * 01 big-value pic $$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$9.99.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  誕生日              PIC 9(8).
       01  通算日              PIC 9(8) BINARY.
       01  余り                PIC 9(8) BINARY.

       01  曜日テーブル.
           02 PIC N(3) VALUE NC"日曜日".
           02 PIC N(3) VALUE NC"月曜日".
           02 PIC N(3) VALUE NC"火曜日".
           02 PIC N(3) VALUE NC"水曜日".
           02 PIC N(3) VALUE NC"木曜日".
           02 PIC N(3) VALUE NC"金曜日".
           02 PIC N(3) VALUE NC"土曜日".
       01  REDEFINES 曜日テーブル.
           02 曜日 OCCURS 7 TIMES PIC N(3).

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
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-I            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX
      *    *** READ PIN1 パターン２
      *     PERFORM S030-10     THRU    S030-EX

      *     PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE
      *    *** 
      *             PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *    *** READ PIN1 パターン２
      *             PERFORM S030-10     THRU    S030-EX
      *     END-PERFORM



      *    *** OTHER UTF8
      *     PERFORM S120-10     THRU    S120-EX

      *    *** -fnotrunc 有り、無し 処理時間 TEST
      *    *** C:\Users\koko\Documents\COBOL>cobc -x cobsam03.cbl -fnotrunc
      *     PERFORM S130-10     THRU    S130-EX

      *    *** OTHER TEST
           PERFORM S200-10     THRU    S200-EX

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
      *     IF      WK-PIN1-STATUS NOT =  ZERO
      *             DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
      *                     WK-PIN1-STATUS
      *             STOP    RUN
      *     END-IF
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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
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

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
                   IF      WK-PIN1-STATUS =    10
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

      *    *** PIN1 OPEN でFILESTATUS 無、存在しないファイル OPEN し
      *    *** READ でも FILESTATUS 無だと、AT END 条件が発生しないで、
      *    *** ループしてしまう
      *    *** 
      *    *** READ PIN1 パターン２
       S030-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS =    ZERO OR 10
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .

       S030-EX.
           EXIT.

      *    *** 
       S100-10.

           MOVE    ALL "*"     TO      POT1-REC
           MOVE    WK-POT1-CNT TO      
                                       POT1-ZD
                                       POT1-PD
                                       POT1-COMP-1
                                       POT1-COMP-2
                                       POT1-COMP-3
                                       POT1-COMP-4
                                       POT1-COMP-5
                                       POT1-COMP-X
                                       POT1-BINARY
                                       POT1-BINARY-CHAR
                                       POT1-BINARY-DOUBLE
                                       POT1-BINARY-LONG
                                       POT1-BINARY-SHORT
           MOVE    X"0D0A"     TO      POT1-CRLF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF WK-POT1-CNT < 11

               DISPLAY " "
               DISPLAY POT1-ZD
               DISPLAY POT1-PD
               DISPLAY POT1-COMP-1
               DISPLAY POT1-COMP-2
               DISPLAY POT1-COMP-3
               DISPLAY POT1-COMP-4
               DISPLAY POT1-COMP-5
               DISPLAY POT1-COMP-X
               DISPLAY POT1-BINARY
               DISPLAY POT1-BINARY-CHAR
               DISPLAY POT1-BINARY-DOUBLE
               DISPLAY POT1-BINARY-LONG
               DISPLAY POT1-BINARY-SHORT

           END-IF

           MOVE    "X"         TO      WFD-ID
           MOVE    "ZD"        TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-ZD

           MOVE    "X"         TO      WFD-ID
           MOVE    "PD"        TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-PD

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-1"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-1

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-2"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-2

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-3"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-3

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-4"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-4

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-5"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-5

           MOVE    "X"         TO      WFD-ID
           MOVE    "COMP-X"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-COMP-X

           MOVE    "X"         TO      WFD-ID
           MOVE    "BINARY"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-BINARY

           MOVE    "X"         TO      WFD-ID
           MOVE    "BINARY-CHA" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-BINARY-CHAR

           MOVE    "X"         TO      WFD-ID
           MOVE    "BINARY-DOU" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-BINARY-DOUBLE

           MOVE    "X"         TO      WFD-ID
           MOVE    "BINARY-LON" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-BINARY-LONG

           MOVE    "X"         TO      WFD-ID
           MOVE    "BINARY-SHO" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-BINARY-SHORT

           MOVE    "P"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S100-EX.
           EXIT.

      *    *** UTF8 TEST
       S120-10.

           WRITE    POT2-REC   FROM    WK-B1
           ADD      1          TO      WK-POT2-CNT

           WRITE    POT2-REC   FROM    WK-B2
           ADD      1          TO      WK-POT2-CNT

           WRITE    POT2-REC   FROM    WK-B3
           ADD      1          TO      WK-POT2-CNT

           WRITE    POT2-REC   FROM    WK-B4
           ADD      1          TO      WK-POT2-CNT

           WRITE    POT2-REC   FROM    WK-B5
           ADD      1          TO      WK-POT2-CNT

           WRITE    POT2-REC   FROM    WK-B6
           ADD      1          TO      WK-POT2-CNT
           .
       S120-EX.
           EXIT.

      *    *** -fnotrunc 有り、無し 処理時間 TEST
      *    *** C:\Users\koko\Documents\COBOL>cobc -x cobsam03.cbl -fnotrunc
       S130-10.

      *    *** ?-fnotrunc PICTUREに従ってバイナリフィールドを切り捨てません。
      *    *** 指定指定無
      *    *** C:\Users\koko\Documents\COBOL>COBSAM03
      *    *** COBSAM03  START
      *    *** USAGE DISPLAY:  10.49 SECONDS
      *    *** USAGE COMP:      8.29 SECONDS
      *    *** USAGE COMP-5:    0.03 SECONDS
      *    *** USAGE BINARY:    0.03 SECONDS

      *    *** COBSAM03  END
      *    *** COBSAM03  PIN1 ｹﾝｽｳ = +0000010000
      *    *** COBSAM03  POT1 ｹﾝｽｳ = +0000010000
      *    *** COBSAM03 START=19/11/23 19:39:23.46 END=19/11/23 19:39:47.94 土(SAT)
      *    *** 処理時間         24.48秒でした

      *    *** C:\Users\koko\Documents\COBOL>cobc -x cobsam03.cbl -fnotrunc
      *    *** 指定有
      *    *** C:\Users\koko\Documents\COBOL>COBSAM03
      *    *** COBSAM03  START
      *    *** USAGE DISPLAY:   2.07 SECONDS
      *    *** USAGE COMP:      0.44 SECONDS
      *    *** USAGE COMP-5:    0.02 SECONDS
      *    *** USAGE BINARY:    0.03 SECONDS

      *    *** COBSAM03  END
      *    *** COBSAM03  PIN1 ｹﾝｽｳ = +0000010000
      *    *** COBSAM03  POT1 ｹﾝｽｳ = +0000010000
      *    *** COBSAM03 START=19/11/23 19:41:20.89 END=19/11/23 19:41:28.47 土(SAT)
      *    *** 処理時間          7.58秒でした


       010-Test-Usage-DISPLAY.
           ACCEPT Begin-Time FROM TIME END-ACCEPT
           PERFORM Repeat-Count TIMES ADD 7 TO Display-Item END-PERFORM
           PERFORM 100-Determine-Time-Diff
           DISPLAY 'USAGE DISPLAY: ' Time-Diff ' SECONDS' END-DISPLAY
           .
       020-Test-Usage-COMP.
           ACCEPT Begin-Time FROM TIME END-ACCEPT
           PERFORM Repeat-Count TIMES ADD 7 TO Comp-Item END-PERFORM
           PERFORM 100-Determine-Time-Diff
           DISPLAY 'USAGE COMP:   ' Time-Diff ' SECONDS' END-DISPLAY
           .
       030-Test-Usage-COMP-5 .
           ACCEPT Begin-Time FROM TIME END-ACCEPT
           PERFORM Repeat-Count TIMES ADD 7 TO Comp-5-Item END-PERFORM
           PERFORM 100-Determine-Time-Diff
           DISPLAY 'USAGE COMP-5: ' Time-Diff ' SECONDS' END-DISPLAY
           .
       040-Test-Usage-BINARY.
           ACCEPT Begin-Time FROM TIME END-ACCEPT
           PERFORM Repeat-Count TIMES ADD 7 TO Binary-Item END-PERFORM
           PERFORM 100-Determine-Time-Diff
           DISPLAY 'USAGE BINARY: ' Time-Diff ' SECONDS' END-DISPLAY
           .
       S130-EX.
           EXIT.

      *    *** OTHER TEST
       S200-10.

      *     MOVE    -123456789  TO      WK-KIN1

           MOVE    -12345678901 TO     WK-KIN2
                                       WK-KIN3
           COMPUTE WK-KIN2 = -12345678901 
               ON SIZE ERROR DISPLAY "WK-KIN2  ON SIZE ERROR "
           END-COMPUTE
           COMPUTE WK-KIN3 = -12345678901 
               ON SIZE ERROR DISPLAY "WK-KIN3  ON SIZE ERROR "
           END-COMPUTE
           DISPLAY "WK-KIN2=" WK-KIN2
           DISPLAY "WK-KIN3=" WK-KIN3
           DISPLAY
        "先頭の１が消える、\ マークも消えてしまう、数字優先で出力される"
      *    *** \ が消えるのは、コンパイラーの仕様かも
      *    *** 他のコンパイラーでは、￥マーク優先かも
      *    *** WK-KIN3 では、\マーク優先、数字カットされる

           MOVE    -1234567890 TO      WK-KIN2
                                       WK-KIN3
           COMPUTE WK-KIN2 = -1234567890
               ON SIZE ERROR DISPLAY "WK-KIN2  ON SIZE ERROR "
           END-COMPUTE
           COMPUTE WK-KIN3 = -1234567890 
               ON SIZE ERROR DISPLAY "WK-KIN3  ON SIZE ERROR "
           END-COMPUTE
           DISPLAY "WK-KIN2=" WK-KIN2
           DISPLAY "WK-KIN3=" WK-KIN3
           DISPLAY "\ マーク消えてしまう、数字優先で出力される" 

           MOVE    -123456789  TO      WK-KIN2
                                       WK-KIN3
           COMPUTE WK-KIN2 = -123456789
               ON SIZE ERROR DISPLAY "WK-KIN2  ON SIZE ERROR "
           END-COMPUTE
           COMPUTE WK-KIN3 = -123456789
               ON SIZE ERROR DISPLAY "WK-KIN3  ON SIZE ERROR "
           END-COMPUTE
           DISPLAY "WK-KIN2=" WK-KIN2
           DISPLAY "WK-KIN3=" WK-KIN3

           MOVE    -12345678   TO      WK-KIN2
                                       WK-KIN3
           COMPUTE WK-KIN2 = -12345678
               ON SIZE ERROR DISPLAY "WK-KIN2  ON SIZE ERROR "
           END-COMPUTE
           COMPUTE WK-KIN3 = -12345678
               ON SIZE ERROR DISPLAY "WK-KIN3  ON SIZE ERROR "
           END-COMPUTE
           DISPLAY "WK-KIN2=" WK-KIN2
           DISPLAY "WK-KIN3=" WK-KIN3

      *     DISPLAY "あなたの誕生日は？ 例:19690123 >> " WITH NO ADVANCING
           DISPLAY "あなたの誕生日は？ 例:19690123 >> " 
           ACCEPT 誕生日
      *----------------------------------------------------------------
      * INTEGER-OF-DATE関数で1601年1月1日(月曜日)からの通算日を求め、
      * 7で割った余りから、曜日を特定します。
      *----------------------------------------------------------------
           COMPUTE 通算日 = FUNCTION INTEGER-OF-DATE(誕生日)
           COMPUTE 余り = FUNCTION REM(通算日 7)
      *----------------------------------------------------------------
           DISPLAY "あなたは、" 曜日(余り + 1) "生まれです。"

      *    *** function指定しても、指定しなくても、結果同じになる
           display function pi space function e
           display pi space e
           .

       S200-EX.
           EXIT.


       100-Determine-Time-Diff.
           ACCEPT End-Time FROM TIME END-ACCEPT
           COMPUTE Time-Diff =
           ( (ET-HH * 360000 + ET-MM * 6000 + ET-SS * 100 + ET-HU)
           - (BT-HH * 360000 + BT-MM * 6000 + BT-SS * 100 + BT-HU) )
           / 100
           .

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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
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
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
