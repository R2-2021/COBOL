      *    *** OSS COBOL 数字最大桁　チェック
      *    *** 関数データ　チェック

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM09.

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

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING DEPENDING ON WK-PIN1-LEN.

       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(120).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(200).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM09".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM09.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM09.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** 38 文字まで指定可能
           03  WK-SU-P         PIC S9(036) VALUE 
           123456789012345678901234567890123456.
           03  WK-SU-M         PIC S9(036) VALUE 
           -123456789012345678901234567890123456.
           03  WK-SU2-P        PIC S9(036) VALUE ZERO.
           03  WK-SU2-M        PIC S9(036) VALUE ZERO.
           03  WK-SU3          PIC S9V9(35) VALUE ZERO.
           03  WK-SU4          PIC S9(036) VALUE 100000000000000.
           03  WK-SU5          PIC S9(036) VALUE 1400000000.
           03  WK-SU6          PIC S9(036) VALUE ZERO.
           03  WK-SU7          PIC  X(007) VALUE ZERO.
           03  WK-PAI          PIC 9V9(35) VALUE ZERO.
           03  WK-E            PIC 9V9(35) VALUE ZERO.
           03  WK-FLOAT-SHORT              VALUE ZERO FLOAT-SHORT.
           03  WK-FLOAT-LONG               VALUE ZERO FLOAT-LONG.
           03  WK-BINARY-LONG              VALUE ZERO BINARY-LONG.
           03  WK-BINARY-DOUBLE            VALUE ZERO BINARY-DOUBLE.
           03  WK-COMP-1                   VALUE ZERO COMP-1.
           03  WK-COMP-2                   VALUE ZERO COMP-2.
           03  WK-COMP-3       PIC S9(009) VALUE ZERO COMP-3.
           03  WK-COMP-4       PIC S9(018) VALUE ZERO COMP-4.
           03  WK-COMP-5       PIC S9(018) VALUE ZERO COMP-5.
           03  WK-COMP-X       PIC S9(036) VALUE ZERO COMP-X.

           03  WK-TIT1.
             05  FILLER        PIC  X(040) VALUE "COBSAM09".
             05  WK-TIT1-TIT   PIC  X(060) VALUE
                 "＊＊＊　数字桁　チェック　＊＊＊".
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.

           03  WK-HAI1.
             05                PIC  X(196) VALUE ALL "-".

           03  WK-MID1.
             05                PIC  X(035) VALUE SPACE.
             05                PIC  X(012) VALUE "Ｚ編集　数字".
             05                PIC  X(037) VALUE SPACE.
             05                PIC  X(012) VALUE "Ｚ編集　数字".
             05                PIC  X(037) VALUE SPACE.
             05                PIC  X(012) VALUE "￥編集　数字".
             05                PIC  X(038) VALUE SPACE.
             05                PIC  X(012) VALUE "￥編集　数字".

           03  WK-MID2.
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "溝".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "穣".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "?".
             05                PIC  X(004) VALUE SPACE.
             05                PIC  X(002) VALUE "垓".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "京".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "兆".
             05                PIC  X(004) VALUE SPACE.
             05                PIC  X(002) VALUE "億".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "万".
             05                PIC  X(003) VALUE SPACE.
             05                PIC  X(002) VALUE "一".

           03  WK-MID3.
             05                PIC  X(047) VALUE
           "    *    *    *     *    *    *     *    *    *".

           03  WK-MEI1.
             05  WK-MEI1-P-Z   PIC
             ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9
                               VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE SPACE.

             05  WK-MEI1-M-Z   PIC
            -ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9
                               VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE SPACE.

             05  WK-MEI1-P-E   PIC
            \\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\9
                               VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE SPACE.

             05  WK-MEI1-M-E   PIC
           -\\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\\,\\9
                               VALUE ZERO.

           03  WK-MEI2.
             05  WK-MEI2-KOMO  PIC  X(016) VALUE SPACE.
             05  WK-MEI2-ITEM  PIC
     *       -9.99999999999999999999999999999999999.

           03  WK-MEI3.
             05  WK-MEI3-KOMO  PIC  X(016) VALUE SPACE.
             05  WK-MEI3-ITEM  PIC ZZZ,ZZZ,ZZZ VALUE ZERO.

           03  WK-MEI4.
             05  WK-MEI4-KOMO  PIC  X(016) VALUE SPACE.
             05  WK-MEI4-ITEM  PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ VALUE ZERO.

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
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** POT1 WRITE
      *             PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** SU CHECK
           PERFORM S100-10     THRU    S100-EX
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

           MOVE    WDT-DATE-YY  TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM  TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD  TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH  TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI  TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS  TO      WK-TIT1-SS

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
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
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

           IF      WK-PIN1-STATUS =    ZERO OR 4
                   ADD     1           TO      WK-PIN1-CNT

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

      *    *** 
       S100-10.

           MOVE    1           TO      WK-TIT1-PAGE
           WRITE   POT1-REC    FROM    WK-TIT1
           WRITE   POT1-REC    FROM    WK-MID1
           WRITE   POT1-REC    FROM    WK-MID2
           WRITE   POT1-REC    FROM    WK-MID3
           WRITE   POT1-REC    FROM    WK-HAI1

           MOVE    "ドル：　　　　　" TO WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-SU4      TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

           MOVE    "ドル*１０５＝円" TO WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           COMPUTE WK-MEI1-P-Z = WK-SU4 * 105.0
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

           MOVE    "中国人口：　　　" TO WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-SU5      TO      WK-MEI1-P-Z
                                       WK-SU6
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "一人当たり円だと" TO WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           MOVE    "円／人口　　　　" TO WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           COMPUTE WK-MEI1-P-Z = ( WK-SU4 * 105.0 ) / WK-SU5
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT


           MOVE    WK-SU-P     TO      WK-SU2-P
           MOVE    WK-SU-M     TO      WK-SU2-M

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36

                   MOVE    WK-SU2-P    TO      WK-MEI1-P-Z
                                               WK-MEI1-P-E

                   MOVE    WK-SU2-M    TO      WK-MEI1-M-Z
                                               WK-MEI1-M-E
                   WRITE   POT1-REC    FROM    WK-MEI1

      *             IF      WK-POT1-STATUS NOT =  ZERO
      *                     DISPLAY WK-PGM-NAME 
      *                             " POT1-F WRITE ERROR STATUS="
      *                             WK-POT1-STATUS
      *                     STOP    RUN
      *             END-IF

                   ADD     1           TO      WK-POT1-CNT

                   COMPUTE WK-SU2-P = WK-SU2-P / 10
                   COMPUTE WK-SU2-M = WK-SU2-M / 10

           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "π：　　　　" TO   WK-MEI2-KOMO
           MOVE    FUNCTION PI TO      WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "ｅ：　　　　" TO   WK-MEI2-KOMO
           MOVE    FUNCTION E  TO      WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "ＳＩＮ(60)：" TO   WK-MEI2-KOMO
           MOVE    FUNCTION SIN(60) TO WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "ＣＯＳ(60)：" TO   WK-MEI2-KOMO
           MOVE    FUNCTION COS(60) TO WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "ＴＡＮ(60)：" TO   WK-MEI2-KOMO
           MOVE    FUNCTION TAN(60) TO WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "１／７ 9V9(35)：" TO   WK-MEI2-KOMO
           COMPUTE WK-MEI2-ITEM = 1.0 / 7.0
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "１／７＊７：" TO   WK-MEI2-KOMO
           COMPUTE WK-SU3 = 1.0 / 7.0 * 7.0
      *     COMPUTE WK-MEI2-ITEM = WK-SU3 * 7
           MOVE    WK-SU3      TO      WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "FLOAT-SHORT 1/7:" TO   WK-MEI2-KOMO
           COMPUTE WK-FLOAT-SHORT = 1 / 7
           MOVE    WK-FLOAT-SHORT TO   WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           DISPLAY "WK-FLOAT-SHORT=" WK-FLOAT-SHORT

           MOVE    "FLOAT-LONG 1/7：" TO   WK-MEI2-KOMO
           COMPUTE WK-FLOAT-LONG = 1 / 7
           MOVE    WK-FLOAT-LONG TO    WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

      *     DISPLAY WK-FLOAT-LONG

           MOVE    "COMP-1    ：" TO   WK-MEI2-KOMO
           COMPUTE WK-COMP-1 = 1 / 7
           MOVE    WK-COMP-1   TO      WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COMP-2    ：" TO   WK-MEI2-KOMO
           COMPUTE WK-COMP-2 = 1 / 7
           MOVE    WK-COMP-2   TO      WK-MEI2-ITEM
           WRITE   POT1-REC    FROM    WK-MEI2
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COMP-2 2**2：" TO  WK-MEI4-KOMO
           MOVE    ZERO        TO      WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2147483648 まで出力可能
           MOVE    2147483648  TO      WK-COMP-2
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2147483648 まで出力可能
           COMPUTE WK-COMP-2 = 2147483648 + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 1 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 63 + 0 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *    *** COMP-2 は SIZE ERROR 発生しない
      *    *** ７桁左にシフトしてしまう、SIZE ERROR も発生しない
      *    *** かなりやばい
           COMPUTE WK-COMP-2 = 9223372036854775808 + 0
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 2 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 63 + 1 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *     COMPUTE WK-COMP-2 = 922337 2036854775808 + 1
           COMPUTE WK-COMP-2 = 2036854775808 + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 3 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 63 + 1 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *     COMPUTE WK-COMP-2 = 922337 2036854775808 + 1
      *    *** １６桁までは、計算出来るようだ！
           COMPUTE WK-COMP-2 = 3372036854775808 + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 4 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 63 + 1 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *     COMPUTE WK-COMP-2 = 922337 2036854775808 + 1
      *    *** １８桁までは、計算出来るようだ！
           PERFORM VARYING I FROM 1 BY 1
      *         UNTIL I > 1000 
               UNTIL I > 10 
                 OR  WK-COMP-2 = ZERO
               COMPUTE WK-COMP-2 = 100000000000000000
                             + 800000000000000000
                             +  99999999999999000
                             + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 5 SIZE ERROR"
               END-COMPUTE

               DISPLAY WK-COMP-2

               MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
               WRITE   POT1-REC    FROM    WK-MEI1
               ADD     1           TO      WK-POT1-CNT
           END-PERFORM
      *    *** + 9999999999999999 なら
      *    ***  909,999,999,999,999,744 <= 結果正確ではないが、出た

      *    *** + 89999999999999999 なら
      *    ***  989,999,999,999,999,744 <= 結果

      *    *** + 99999999999999000 なら
      *    ***  999,999,999,999,998,720 <= 結果

      *    ***  この付近がCOMP-2 の上限　１８桁がＭＡＸ

      *    *** + 99999999999999999 なら
      *    ***  0 <= 結果

      *    *** + 100000000000000000 なら
      *    ***  0 <= 結果

      *    *** COMP-2 なら、2 ** 63 + 1 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *     COMPUTE WK-COMP-2 = 922337 2036854775808 + 1
      *    *** １６桁までは、計算出来るようだ！
      *    *** １７桁 10000000000000000 も出来た
           COMPUTE WK-COMP-2 = 99999999999999999 + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 6 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 63 + 1 =
      *    *** 9,223,372,036,854,775,808 + 0 も出力可能
      *     COMPUTE WK-COMP-2 = 922337 2036854775808 + 1
      *     *** この桁で＋１されなくなる　SIZE ERROR も発生しない
           COMPUTE WK-COMP-2 = 23372036854775808 + 1
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 7 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

      *    *** COMP-2 なら、2 ** 53 + 1 =
      *    *** 9,007,199,254,740,992 + 1 も出力可能
      *     *** この桁で＋１されなくなる　SIZE ERROR も発生しない
           COMPUTE WK-COMP-2 = (2 ** 53 + 2 ** 53)  * 10
                   SIZE ERROR 
                       DISPLAY "WK-COMP-2 8 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-COMP-2   TO      WK-MEI1-P-Z
           WRITE   POT1-REC    FROM    WK-MEI1
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "BINARY-LONG：" TO   WK-MEI3-KOMO
      *     COMPUTE WK-BINARY-LONG = WK-SU-P
           MOVE    123456789     TO      WK-BINARY-LONG
      *    *** SIZE ERROR 発生すると、 変更されない
      *    *** 123456789 が出力される
      *    *** 2 ** 31 = 2147483648
      *     COMPUTE WK-BINARY-LONG = 999999999 + 1
      *     COMPUTE WK-BINARY-LONG = 2147483648 - 1
      *    *** 9桁になると、 SIZE ERROR 発生しない
           COMPUTE WK-BINARY-LONG  = 1000000000 - 1
      *     COMPUTE WK-BINARY-LONG = 2147483648 - 1
                   SIZE ERROR 
      *                 DISPLAY "WK-BINARY-L 2147483648 - 1 SIZE ERROR"
                       DISPLAY "WK-BINARY-L 1000000000 - 1 SIZE ERROR"
           END-COMPUTE
           MOVE    WK-BINARY-LONG TO   WK-MEI3-ITEM
           WRITE   POT1-REC    FROM    WK-MEI3
           ADD     1           TO      WK-POT1-CNT

      *    *** 10桁になると、 SIZE ERROR 発生する
      *     COMPUTE WK-BINARY-LONG  = 999999999 - 1
      *     ADD     1 999999999 GIVING WK-BINARY-LONG
           COMPUTE WK-BINARY-LONG  = 2 ** 29
                   SIZE ERROR 
                       DISPLAY "WK-BINARY-L 2 SIZE ERROR " 
                               WK-BINARY-LONG
           END-COMPUTE
      *     END-ADD

      *     MOVE    123456789012 TO     WK-BINARY-LONG
      *     DISPLAY "WK-BINARY-LONG=" WK-BINARY-LONG
      *    表示数 マイナスになる
      *     MOVE    123456789   TO     WK-BINARY-LONG
      *     DISPLAY "WK-BINARY-LONG=" WK-BINARY-LONG
           MOVE    WK-BINARY-LONG TO   WK-MEI3-ITEM
           WRITE   POT1-REC    FROM    WK-MEI3
           ADD     1           TO      WK-POT1-CNT

           MOVE    "BINARY-DOUBLE：" TO   WK-MEI4-KOMO
      *     COMPUTE WK-BINARY-DOUBLE = WK-SU-P
           MOVE    123456789012345678     TO      WK-BINARY-DOUBLE
      *    ***  SIZE ERROR 発生すると、 変更されない
      *    ***  123456789012345678 が出力される
           COMPUTE WK-BINARY-DOUBLE = 999999999999999999 + 1
                   SIZE ERROR
                       DISPLAY "WK-BINARY-D SIZE ERROR"
           END-COMPUTE
           MOVE    WK-BINARY-DOUBLE TO WK-MEI4-ITEM
           WRITE   POT1-REC    FROM    WK-MEI4
           ADD     1           TO      WK-POT1-CNT

           MOVE    "?123456"   TO      WK-SU7
           IF      NUMVAL(WK-SU7) >=   123456
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) THEN"
           ELSE
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) ELSE"
           END-IF

           MOVE    "?THU?HI"   TO      WK-SU7
           IF      NUMVAL(WK-SU7) >=   12345
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) THEN"
           ELSE
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) ELSE"
           END-IF

           MOVE    "?123456"     TO      WK-SU7
           IF      NUMVAL(WK-SU7) >=   123456
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) THEN"
           ELSE
                   DISPLAY "WK-SU7=" WK-SU7 " NUMVAL(WK-SU7) ELSE"
           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     MOVE    2           TO      WFD-SU
      *     MOVE    100         TO      WFD-LEN
      *     CALL    "FILEDUMP"   USING  WFD-FILEDUMP-AREA
      *                                 POT1-REC
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"


           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
