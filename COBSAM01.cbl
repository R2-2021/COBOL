      *    *** 文字タイプチェック USAGE 
      
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM01.

       ENVIRONMENT             DIVISION.
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
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-KEY        PIC  X(010).
           03  PIN1-DATA       PIC  X(070).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-KEY        PIC  X(010).
           03  POT1-DATA       PIC  X(070).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM01".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM01.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     USAGE BINARY-LONG VALUE ZERO.

           03  WK-PIN1-CNT     USAGE BINARY-LONG VALUE ZERO.
           03  WK-POT1-CNT     USAGE BINARY-LONG VALUE ZERO.

           03  WK-UTF8-1.
             05  FILLER        PIC  X(036) VALUE 
                 "%E7%9F%B3%E5%8E%9F%E5%A4%8F%E7%B9%94".
           03  WK-UTF8-2.
             05  FILLER        PIC  X(012) VALUE LOW-VALUE.
           03  WK-BUF1-L       USAGE BINARY-LONG VALUE 36.
           03  WK-BUF2-L       USAGE BINARY-LONG VALUE 12.
           03  WK-STR          PIC  X(003) VALUE "%XX".
           03  WK-VAL          PIC  X(001) VALUE X"20".
           03  WK-DATA1        PIC  X(001) VALUE "A".
             88  WK-DATA1-88               VALUE "Z"
                                           FALSE "B".

           03  WK-A1           COMP-2 VALUE 12.34.
           03  WK-A2           COMP-2 VALUE 56.78.
           03  WK-A3           COMP-2 VALUE ZERO.

      *    *** COMP-1.COMP-2 少数点を考慮して、計算している
      *    *** このコンパイラーだからか？　他のコンパイラーでは、結果不明
      *    *** WK-A1 * WK-A2 => WK-A3
      *    *** WK-A3 => WK-A4
      *    *** WK-A4 = 700.6651 になる
           03  WK-A4           PIC  ----,--9.9999 VALUE ZERO.

      *    *** 6 BYTE (9+2+1)/2 1:SIGN
           03  WK-PACK01       PIC S9(9)V99 VALUE ZERO PACKED-DECIMAL.



      *    *** COMP,COMP-4,COMP-X,BINARY は１６進数で内部コード表現している
      *    *** その他は内部コード１６進数不明　WK-BINARYで始まるもの全て


      *    *** 4,8 BYTE PIC の内容による
      *    ***  123456789012345.67=>789012345.67 11桁で桁落ちする
      *     03  WK-COMP         PIC S9(9)V99 VALUE ZERO COMP.
           03  WK-COMP         PIC S9(3)V99 VALUE ZERO COMP.

      *    *** PIC で指定した桁に編集される、符号も無くなる
           03  WK-COMP2        PIC  9(003)  VALUE ZERO COMP.

      *    *** COMP-1,COMP-2 少数点以下していないが、少数点以下数値送って
      *    *** も少数点以下入っている
      *    *** 4 BYTE , DISPLAYでは、DOUBLEに編集している
      *    *** 123456789012345.67=>123456788103168.00000 桁は合ってるが
      *    *** 数値に誤差がある
           03  WK-COMP-1                    VALUE ZERO COMP-1.
      *    *** PIC 指定するとエラー
      *    03  WK-COMP-12      PIC  9(004)  VALUE ZERO COMP-1.

      *    *** 8 BYTE　２０桁？まで表現可、一番優秀　少数点判断している
      *    *** COMP-1,COMP-2 少数点以下していないが、少数点以下数値送って
      *    *** も少数点以下入っている
      *    *** 123456789012345.67=>123456789012345.671875 桁,数値とも
      *    *** 合ってる
           03  WK-COMP-2                    VALUE ZERO COMP-2.
      *    *** PIC 指定するとエラー
      *    03  WK-COMP-22      PIC  9(004)  VALUE ZERO COMP-2.

      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする
      *    *** 6 BYTE , PACKED-DECIMALと同じ
           03  WK-COMP-3       PIC S9(9)V99 VALUE ZERO COMP-3.

      *    *** PIC で指定した桁に編集される、符号も無くなる
           03  WK-COMP-32      PIC  9(004)  VALUE ZERO COMP-3.

      *    *** 8 BYTE , COMPと同じ
      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする
           03  WK-COMP-4       PIC S9(9)V99 VALUE ZERO COMP-4.

      *    *** PIC で指定した桁に編集される、符号も無くなる
           03  WK-COMP-42      PIC  9(004)  VALUE ZERO COMP-4.

      *    *** 8 BYTE , 数値左詰めで入っている
      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする
           03  WK-COMP-5       PIC S9(9)V99 VALUE ZERO COMP-5.

      *    *** PIC で指定した桁に編集される、符号も無くなる
           03  WK-COMP-52      PIC  9(004)  VALUE ZERO COMP-5.

      *    *** 5 BYTE , 数値COMP,COMP-4と同じ値入っている
      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする
           03  WK-COMP-X       PIC S9(9)V99 VALUE ZERO COMP-X.

      *    *** 2 BYTE , PIC の桁数で　バイト数変わる
      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする

      *    *** PIC で指定した桁に編集される、符号も無くなる
           03  WK-COMP-X2      PIC  9(004)  VALUE ZERO COMP-X.

      *    *** 8 BYTE , COMPと同じ値入っている
      *    *** 123456789012345.67=>789012345.67 桁は合ってるが、桁落ちする
           03  WK-BINARY       PIC S9(9)V99 VALUE ZERO BINARY.

      *    *** 1 BYTE , 値 BINARY-LONG の１桁目が入っている
           03  WK-BINARY-CHAR          VALUE ZERO BINARY-CHAR.
           03  WK-BINARY-CHAR-S        VALUE ZERO BINARY-CHAR SIGNED.
           03  WK-BINARY-CHAR-U        VALUE ZERO BINARY-CHAR UNSIGNED.

      *    *** 下記、値、左詰めで入っている
      *    *** 4 BYTE
      *    *** 桁オーバーすると変な値入る　１０桁以上だと変になる
           03  WK-BINARY-C-LONG        VALUE ZERO BINARY-C-LONG.
           03  WK-BINARY-C-LONG-S      VALUE ZERO BINARY-C-LONG SIGNED.
           03  WK-BINARY-C-LONG-U      VALUE ZERO BINARY-C-LONG UNSIGNED.

      *    *** 8 BYTE　２０桁？まで表現可、一番桁数で優秀
      *    *** 少数点不可、整数のみ
           03  WK-BINARY-DOUBLE        VALUE ZERO BINARY-DOUBLE.
           03  WK-BINARY-DOUBLE-S      VALUE ZERO BINARY-DOUBLE SIGNED.
           03  WK-BINARY-DOUBLE-U      VALUE ZERO BINARY-DOUBLE UNSIGNED.

      *    *** 4 BYTE
      *    *** 桁オーバーすると変な値入る　１０桁以上だと変になる
           03  WK-BINARY-LONG          VALUE ZERO BINARY-LONG.
           03  WK-BINARY-LONG-S        VALUE ZERO BINARY-LONG SIGNED.
           03  WK-BINARY-LONG-U        VALUE ZERO BINARY-LONG UNSIGNED.

      *    *** 2 BYTE 123456.79=> -07616(SHORT,-S)や57920(U)
      *    *** 桁オーバーすると変な値入る
           03  WK-BINARY-SHORT         VALUE ZERO BINARY-SHORT.
           03  WK-BINARY-SHORT-S       VALUE ZERO BINARY-SHORT SIGNED.
           03  WK-BINARY-SHORT-U       VALUE ZERO BINARY-SHORT UNSIGNED.

      *    *** 4 BYTE 123456.89 => -123456
           03  WK-SIGNED-INT           VALUE ZERO SIGNED-INT.
      *    *** 8 BYTE 123456.89 => -123456
           03  WK-SIGNED-LONG          VALUE ZERO SIGNED-LONG.
      *    *** 2 BYTE 123456.89 => +07616 符号逆、値も不明
           03  WK-SIGNED-SHORT         VALUE ZERO SIGNED-SHORT.

      *    *** 4 BYTE
      *    *** 桁オーバーすると変な値入る　１０桁以上だと変になる
           03  WK-UNSIGNED-INT         VALUE ZERO UNSIGNED-INT.
      *    *** 8 BYTE
           03  WK-UNSIGNED-LONG        VALUE ZERO UNSIGNED-LONG.
      *    *** 2 BYTE
      *    *** 桁オーバーすると変な値入る　１０桁以上だと変になる
           03  WK-UNSIGNED-SHORT       VALUE ZERO UNSIGNED-SHORT.

      *    *** GRAMMER.PDF 7.10.73 USAGE CLAUSE コンパイルエラー
      *    03  WK-FIXED-LENGTH-INTEGERS VALUE ZERO FIXED-LENGTH-INTEGERS.
      *    03  WK-FLOAT-BINARY-32       VALUE ZERO FLOAT-BINARY-32.
      *    03  WK-FLOAT-BINARY-64       VALUE ZERO FLOAT-BINARY-64.
      *    03  WK-FLOAT-BINARY-128      VALUE ZERO FLOAT-BINARY-128.

      *    03  WK-FLOAT-DECIMA-16       VALUE ZERO FLOAT-DECIMA-16.
      *    03  WK-FLOAT-DECIMA-34       VALUE ZERO FLOAT-DECIMA-34.
           03  WK-FLOAT-LONG            VALUE ZERO FLOAT-LONG.
           03  WK-FLOAT-SHORT           VALUE ZERO FLOAT-SHORT.
      *     03  WK-FLOAT-DOUBLE          VALUE ZERO FLOAT-DOUBLE.



      *    *** -,$ は内部数値桁より、１桁多くする
      *    *** マイナスは出ない
           03  WK-HENSHU01     PIC ZZZ,ZZ9.99 VALUE ZERO.
      *    *** １００万の位出ない
           03  WK-HENSHU02     PIC ----,--9.99 VALUE ZERO.
      *    *** １００万の位出ない
           03  WK-HENSHU03     PIC -$$$$,$$9.99 VALUE ZERO.
      *    *** １０万の位出ない
           03  WK-HENSHU04     PIC -99,999.99 VALUE ZERO.
      *    *** マイナスは出ない
           03  WK-HENSHU05     PIC 999,999.99 VALUE ZERO.
      *    *** １００万の位にする
           03  WK-HENSHU06     PIC -$ZZZ,ZZ9.99 VALUE ZERO.
      *
       01  form pic $-z(7)9.9(8).

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
           03  K               USAGE BINARY-LONG VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "0".

       01  SAVE-AREA.
           03  SV-I            USAGE BINARY-LONG VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.
           PERFORM S010-10     THRU    S010-EX

      *    PERFORM S100-10     THRU    S100-EX

           PERFORM S200-10     THRU    S200-EX
      *            UNTIL WK-PIN1-EOF = HIGH-VALUE

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

      *    *** PIN1 READ
       S100-10.
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
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
           .
         S100-EX.
           EXIT.

      *    *** 
       S200-10.

           MOVE    123456789012345.67   TO      WK-PACK01
                                       WK-COMP
                                       WK-COMP2
                                       WK-COMP-1
                                       WK-COMP-2
                                       WK-COMP-3
                                       WK-COMP-32
                                       WK-COMP-4
                                       WK-COMP-42
                                       WK-COMP-5
                                       WK-COMP-52
                                       WK-COMP-X
                                       WK-COMP-X2

                                       WK-BINARY

                                       WK-BINARY-CHAR
                                       WK-BINARY-CHAR-S
                                       WK-BINARY-CHAR-U

                                       WK-BINARY-C-LONG
                                       WK-BINARY-C-LONG-S
                                       WK-BINARY-C-LONG-U

                                       WK-BINARY-DOUBLE
                                       WK-BINARY-DOUBLE-S
                                       WK-BINARY-DOUBLE-U

                                       WK-BINARY-LONG
                                       WK-BINARY-LONG-S
                                       WK-BINARY-LONG-U

                                       WK-BINARY-SHORT
                                       WK-BINARY-SHORT-S
                                       WK-BINARY-SHORT-U

                                       WK-SIGNED-INT
                                       WK-SIGNED-LONG
                                       WK-SIGNED-SHORT

                                       WK-UNSIGNED-INT
                                       WK-UNSIGNED-LONG
                                       WK-UNSIGNED-SHORT

      *    *** WK-FLOAT-LONG 定義しなくても、コンパイルエラーにならない
                                       WK-FLOAT-LONG
                                       WK-FLOAT-SHORT

           DISPLAY "001="              WK-PACK01
           DISPLAY "002="              WK-COMP
           DISPLAY "002X="             WK-COMP2
           DISPLAY "003="              WK-COMP-1
           DISPLAY "004="              WK-COMP-2
           DISPLAY "005="              WK-COMP-3
           DISPLAY "005X="             WK-COMP-32
           DISPLAY "006="              WK-COMP-4
           DISPLAY "006X="             WK-COMP-42
           DISPLAY "007="              WK-COMP-5
           DISPLAY "007X="             WK-COMP-52
           DISPLAY "008="              WK-COMP-X
           DISPLAY "008X="             WK-COMP-X2

           DISPLAY "009="              WK-BINARY

           DISPLAY "010="              WK-BINARY-CHAR
           DISPLAY "011="              WK-BINARY-CHAR-S
           DISPLAY "012="              WK-BINARY-CHAR-U

           DISPLAY "013="              WK-BINARY-C-LONG
           DISPLAY "014="              WK-BINARY-C-LONG-S
           DISPLAY "015="              WK-BINARY-C-LONG-U

           DISPLAY "016="              WK-BINARY-DOUBLE
           DISPLAY "017="              WK-BINARY-DOUBLE-S
           DISPLAY "018="              WK-BINARY-DOUBLE-U

           DISPLAY "019="              WK-BINARY-LONG
           DISPLAY "020="              WK-BINARY-LONG-S
           DISPLAY "021="              WK-BINARY-LONG-U

           DISPLAY "022="              WK-BINARY-SHORT
           DISPLAY "023="              WK-BINARY-SHORT-S
           DISPLAY "024="               WK-BINARY-SHORT-U

           DISPLAY "025="              WK-SIGNED-INT
           DISPLAY "026="              WK-SIGNED-LONG
           DISPLAY "027="              WK-SIGNED-SHORT

           DISPLAY "028="              WK-UNSIGNED-INT
           DISPLAY "029="              WK-UNSIGNED-LONG
           DISPLAY "030="              WK-UNSIGNED-SHORT

      *    *** WK-FLOAT-LONG 定義しなくても、コンパイルエラーにならない
           DISPLAY "031="              WK-FLOAT-LONG
           DISPLAY "032="              WK-FLOAT-SHORT


           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " PACK"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-PACK01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP2"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP2

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-1"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-1

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-2"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-2
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-3"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-3
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-32"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-32
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-4"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-4
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-42"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-42
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-5"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-5
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-52"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-52
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-X"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-X
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-X2"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-X2

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " BINARY"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-CHAR" TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-C-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-C-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-LONG" TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINA-C-L-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINA-C-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-SHO" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-S-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-S-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-INT" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-INT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-LON" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-SHO" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-SHORT

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-I-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-INT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-S-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-SHORT

      *    *** WK-FLOAT-LONG 定義しなくても、コンパイルエラーにならない
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "FLOAT-LONG" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-FLOAT-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "FLOAT-SHO" TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-FLOAT-SHORT




           MOVE    -123456.78 TO      WK-PACK01
                                       WK-COMP
                                       WK-COMP2
                                       WK-COMP-1
                                       WK-COMP-2
                                       WK-COMP-3
                                       WK-COMP-32
                                       WK-COMP-4
                                       WK-COMP-42
                                       WK-COMP-5
                                       WK-COMP-52
                                       WK-COMP-X
                                       WK-COMP-X2

                                       WK-BINARY

                                       WK-BINARY-CHAR
                                       WK-BINARY-CHAR-S
                                       WK-BINARY-CHAR-U

                                       WK-BINARY-C-LONG
                                       WK-BINARY-C-LONG-S
                                       WK-BINARY-C-LONG-U

                                       WK-BINARY-DOUBLE
                                       WK-BINARY-DOUBLE-S
                                       WK-BINARY-DOUBLE-U

                                       WK-BINARY-LONG
                                       WK-BINARY-LONG-S
                                       WK-BINARY-LONG-U

                                       WK-BINARY-SHORT
                                       WK-BINARY-SHORT-S
                                       WK-BINARY-SHORT-U

                                       WK-SIGNED-INT
                                       WK-SIGNED-LONG
                                       WK-SIGNED-SHORT

                                       WK-UNSIGNED-INT
                                       WK-UNSIGNED-LONG
                                       WK-UNSIGNED-SHORT

           DISPLAY "001="              WK-PACK01
           DISPLAY "002="              WK-COMP
           DISPLAY "002X="             WK-COMP2
           DISPLAY "003="              WK-COMP-1
           DISPLAY "004="              WK-COMP-2
           DISPLAY "005="              WK-COMP-3
           DISPLAY "005X="             WK-COMP-32
           DISPLAY "006="              WK-COMP-4
           DISPLAY "006X="             WK-COMP-42
           DISPLAY "007="              WK-COMP-5
           DISPLAY "007X="             WK-COMP-52
           DISPLAY "008="              WK-COMP-X
           DISPLAY "008X="             WK-COMP-X2

           DISPLAY "009="              WK-BINARY

           DISPLAY "010="              WK-BINARY-CHAR
           DISPLAY "011="              WK-BINARY-CHAR-S
           DISPLAY "012="              WK-BINARY-CHAR-U

           DISPLAY "013="              WK-BINARY-C-LONG
           DISPLAY "014="              WK-BINARY-C-LONG-S
           DISPLAY "015="              WK-BINARY-C-LONG-U

           DISPLAY "016="              WK-BINARY-DOUBLE
           DISPLAY "017="              WK-BINARY-DOUBLE-S
           DISPLAY "018="              WK-BINARY-DOUBLE-U

           DISPLAY "019="              WK-BINARY-LONG
           DISPLAY "020="              WK-BINARY-LONG-S
           DISPLAY "021="              WK-BINARY-LONG-U

           DISPLAY "022="              WK-BINARY-SHORT
           DISPLAY "023="              WK-BINARY-SHORT-S
           DISPLAY "024="              WK-BINARY-SHORT-U

           DISPLAY "025="              WK-SIGNED-INT
           DISPLAY "026="              WK-SIGNED-LONG
           DISPLAY "027="              WK-SIGNED-SHORT

           DISPLAY "028="              WK-UNSIGNED-INT
           DISPLAY "029="              WK-UNSIGNED-LONG
           DISPLAY "030="              WK-UNSIGNED-SHORT

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " PACK"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-PACK01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP2"    TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP2

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-1"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-1

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-2"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-2
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-3"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-3
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-32"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-32
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-4"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-4
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-42"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-42
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-5"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-5
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-52"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-52
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-X"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-X
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " COMP-X2"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-COMP-X2

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    " BINARY"   TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-CHAR" TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-C-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-C-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-CHAR-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-LONG" TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINA-C-L-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINA-C-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-C-LONG-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-D-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-DOUBLE-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-LONG-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-SHO" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-S-S" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT-S
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "BINARY-S-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-BINARY-SHORT-U

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-INT" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-INT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-LON" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "SIGNED-SHO" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-SIGNED-SHORT

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-I-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-INT
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-L-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-LONG
           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UNSIGN-S-U" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UNSIGNED-SHORT



           MOVE    123456.78   TO      WK-HENSHU01
                                       WK-HENSHU02
                                       WK-HENSHU03
                                       WK-HENSHU04
                                       WK-HENSHU05
                                       WK-HENSHU06

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU01   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU02   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU02

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU03   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU03

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU04   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU04

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU05   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU05

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU06   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU06



           MOVE    -123456.78  TO      WK-HENSHU01
                                       WK-HENSHU02
                                       WK-HENSHU03
                                       WK-HENSHU04
                                       WK-HENSHU05
                                       WK-HENSHU06

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU01   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU02   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU02

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU03   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU03

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU04   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU04

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU05   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU05

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU06   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU06



           MOVE    WK-COMP     TO      WK-HENSHU01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU05   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU05

           MOVE    WK-COMP-1   TO      WK-HENSHU01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU05   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU05

           MOVE    WK-COMP-2   TO      WK-HENSHU01

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "HENSHU05   " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-HENSHU05

           COMPUTE WK-A3 = WK-A1 * WK-A2
           MOVE    WK-A3       TO      WK-A4
           DISPLAY WK-A4

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "WK-A1      " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-A1

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "WK-A2      " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-A2

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "WK-A3      " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-A3

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "WK-A4      " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-A4

           MOVE    -12345678.12345678 TO form

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "form       " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       form

      *     PERFORM VARYING I FROM 1 BY 3
      *             UNTIL I > 12
      *             
      *     03  WK-UTF8-1
      *       05  FILLER        PIC  X(036) VALUE 
      *           "%E7%9F%B3%E5%8E%9F%E5%A4%8F%E7%B9%94"
      *     03  WK-UTF8-2
      *       05  FILLER        PIC  X(012) VALUE LOW-VALUE

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UTF8-1     " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UTF8-1

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *    *** 1,000,000 => 8.73秒
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > 1000000
                   CALL    "DECODE02"  USING   WK-UTF8-1
                                               WK-BUF1-L
                                               WK-UTF8-2
                                               WK-BUF2-L

      *             WRITE   POT1-REC    FROM    WK-UTF8-2
      *             ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "DECODE02"  TO      WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "M"         TO      WFD-TYPE
      *     MOVE    "UTF8-2     " TO    WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-UTF8-2
      *     GO S200-20

      *    *** 1,000,000 => 25.84秒
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > 1000000
               MOVE    0           TO      J
      *    *** BYTE を １６進数に変換
               PERFORM VARYING I FROM 1 BY 3
                   UNTIL I > 36
                   MOVE    WK-UTF8-1 (I:3) TO WK-STR
      *    *** %LR ==> H"LR" の16進数に変換する
                   CALL    "DECODE01" USING   WK-STR
                                              WK-VAL
                   ADD     1          TO      J
                   MOVE    WK-VAL     TO      WK-UTF8-2 (J:1)

      *           IF J = 12
      *             WRITE   POT1-REC    FROM    WK-UTF8-2
      *             ADD     1           TO      WK-POT1-CNT
      *           END-IF
               END-PERFORM
           END-PERFORM

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "DECODE01"  TO      WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "X"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    "UTF8-2     " TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-UTF8-2
      *     CALL    "COBDUMP"   USING   WK-UTF8-2
           .
       S200-20.

      *    *** html %xx%xx... => 石原夏織になった
           WRITE   POT1-REC   FROM     WK-UTF8-2
           ADD     1          TO       WK-POT1-CNT

      *    *** 88 TEST
           DISPLAY "WK-DATA1=" WK-DATA1
      *    *** WK-DATA1=A が入っている　ELSEとなる
           IF      WK-DATA1-88
                   DISPLAY "THEN 1"
           ELSE
                   DISPLAY "ELSE 1"
           END-IF

           MOVE    "Z"        TO       WK-DATA1
           DISPLAY "WK-DATA1=" WK-DATA1
      *    *** WK-DATA1=Z が入っている　THENとなる
           IF      WK-DATA1-88
                   DISPLAY "THEN 2"
           ELSE
                   DISPLAY "ELSE 2"
           END-IF

           MOVE    "A"         TO      WK-DATA1
           DISPLAY "WK-DATA1=" WK-DATA1
      *    *** WK-DATA1=A が入っている　ELSEとなる
           IF      WK-DATA1-88
                   DISPLAY "THEN 3"
           ELSE
                   DISPLAY "ELSE 3"
           END-IF

           SET     WK-DATA1-88 TO      TRUE
           DISPLAY "WK-DATA1=" WK-DATA1
      *    *** WK-DATA1=Z が入っている　THENとなる
           IF      WK-DATA1-88
                   DISPLAY "THEN 4"
           ELSE
                   DISPLAY "ELSE 4"
           END-IF

           SET     WK-DATA1-88 TO      FALSE
           DISPLAY "WK-DATA1=" WK-DATA1
      *    *** WK-DATA1=B が入っている　ELSEとなる
      *    *** データ定義でFALSE,指定しないとコンパイルエラーになる
           IF      WK-DATA1-88
                   DISPLAY "THEN 5"
           ELSE
                   DISPLAY "ELSE 5"
           END-IF
           .


      *     PERFORM S100-10     THRU      S100-EX
       S200-EX.
           EXIT.

      *    *** POT1 WRITE 1
       S210-10.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   MOVE    TBL01-KEY  (I) TO   POT1-KEY

                   WRITE   POT1-REC  
                   ADD     1          TO       WK-POT1-CNT
           END-PERFORM
           .
       S210-EX.
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
