      *    *** ＣＳＶデータ固定長にする
      *    *** 項目ごとの長さは、自動で最大値の項目長さにして出力
      *    *** UNSTRING,STRING 使用
      *    *** １０桁以下の項目で数字、スペースのみで構成される項目は右寄せに
      *    *** する

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST42.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** パラメータファイル
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 任意ＣＳＶ可変長ファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 固定長ファイル
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F.
       01  PRM1-REC            PIC  X(080).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(65535).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(65535).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST42  ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST42.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE SPACE.
           03  WK-POT1-F-NAME  PIC  X(032) VALUE SPACE.

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-I1      PIC  X(1000) VALUE SPACE.
           03  WK-UNST-PTR     BINARY-LONG SYNC VALUE ZERO.
           03  WK-STO-PTR      BINARY-LONG SYNC VALUE ZERO.
           03  WK-DELI         PIC  X(001) VALUE SPACE.
           03  WK-NUM-ITEM     PIC  X(010) VALUE SPACE.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-POS          BINARY-LONG SYNC VALUE ZERO.
           03  WK-POSZ         PIC  ZZZZ9  VALUE ZERO.
           03  WK-LENZ         PIC  ZZZZ9  VALUE ZERO.

           03  WK-PRM1DT.
             05  WK-PRM1DT01   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT02   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT03   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT04   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT05   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT06   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT07   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT08   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT09   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT10   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT11   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT12   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT13   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT14   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT15   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT16   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT17   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT18   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT19   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT20   PIC  X(032) VALUE SPACE.
           03  WK-PRM1DT-R     REDEFINES WK-PRM1DT.
             05  WK-PRM1DT-T   OCCURS 20
                               PIC  X(032).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-POS     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-TYPE    PIC  X(002) VALUE SPACE.
             05  TBL01-SHIFT   PIC  X(001) VALUE SPACE.
             05  TBL01-LEN     BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-NIKAIME      PIC  X(001) VALUE "N".
           03  SW-YES          PIC  X(001) VALUE "Y".
           03  SW-NUM          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S050-10     THRU    S050-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF = HIGH-VALUE
      *    *** PIN1 CHECK
                   PERFORM S030-10     THRU    S030-EX
      *    *** PIN1 READ
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           MOVE    I           TO      I-MAX
           MOVE    1           TO      WK-POS
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   DISPLAY " "
                   DISPLAY "I=" I
                           " TBL01-TYPE="  TBL01-TYPE  (I)
                           " TBL01-SHIFT=" TBL01-SHIFT (I)
                           " TBL01-LEN="   TBL01-LEN   (I)
                   MOVE    WK-POS      TO      WK-POSZ
                   MOVE    TBL01-LEN (I) TO    WK-LENZ
                   DISPLAY "(" WK-POSZ ":" WK-LENZ ")"
      *    *** + 1 はカンマ分
                   COMPUTE WK-POS = WK-POS + TBL01-LEN (I) + 1
           END-PERFORM



      *    *** PIN1 CLOSE,OPEN 2
           PERFORM S040-10     THRU    S040-EX

           MOVE    "Y"         TO      SW-NIKAIME
           MOVE    LOW-VALUE   TO      WK-PIN1-EOF

      *    *** READ PIN1 二回目
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF = HIGH-VALUE

      *    *** PIN1 CSV を WRITE POT1 固定長に変換１
      *            PERFORM S100-10      THRU    S100-EX

      *    *** PIN1 CSV を WRITE POT1 固定長に変換２
                  PERFORM S200-10      THRU    S200-EX

      *    *** READ PIN1
                  PERFORM S020-10      THRU    S020-EX
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

           ACCEPT  WK-ARGUMENT-NUMBER FROM      ARGUMENT-NUMBER

      *    *** PRM1-F 指定無し（ARGUMENT-NUMBER=0）、既定値使用
      *    *** ARGUMENT-NUMBER=1 の時、PRM1-F 指定する
           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-PRM1-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PRM1-F-NAME

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PRM1-F 1個まで指定可"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
               IF      SW-NIKAIME      =       "Y"
                   ADD     1           TO      WK-PIN1-CNT
               ELSE
                   CONTINUE
               END-IF
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE

                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    100         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC
      *                                 WFD-LEN
           .
       S020-EX.
           EXIT.

      *    *** PIN1 CHECK
       S030-10.

           MOVE    1           TO      WK-UNST-PTR
           MOVE    ZERO        TO      I
           MOVE    SPACE       TO      WK-PIN1-I1
           MOVE    WK-PIN1-LEN TO      L2

           PERFORM UNTIL WK-UNST-PTR > WK-PIN1-LEN
                   UNSTRING PIN1-REC
                       DELIMITED BY ","
      *    *** PIN1-RECを大きくすると、COUNT L はインプットレコード長に
      *    *** 合わせた長さがセットされる
                           INTO WK-PIN1-I1 DELIMITER WK-DELI COUNT L
                           WITH POINTER WK-UNST-PTR

                   ADD     1           TO      I
                   IF      I           >       100
                           DISPLAY WK-PGM-NAME " PIN1-F,TBL01 OVER I=" I
                           STOP    RUN
                   END-IF

      *    *** Iは項目の位置、TBL01-LEN(I)に項目ごとの最大値をセット
                   IF      WK-PIN1-LEN >       L
                       IF      TBL01-LEN (I) <     L
                           MOVE    L           TO      TBL01-LEN (I)
                       END-IF
      *    *** 入力ＣＳＶで－１はカンマ分引いて最後の項目長さ求める
      *    *** 最後の項目の長さＬはレコード長多めに取っているので正確で
      *    *** ないため、一つ前の項目で次の項目の長さ求める
                       COMPUTE L2 = L2 - L - 1
                   ELSE
      *    *** 最後の項目のLが違っているため、微調整する
                       IF      TBL01-LEN (I) <     L2
                           MOVE    L2          TO      TBL01-LEN (I)
                       END-IF
                   END-IF
      *             DISPLAY "L       =" L
      *             DISPLAY "L2      =" L2
      *             DISPLAY "I       =" I
      *             DISPLAY "PIN1-I1 =" WK-PIN1-I1(1:70)
      *             DISPLAY "PIN1-LEN=" WK-PIN1-LEN
      *             DISPLAY "LEN(I)  =" TBL01-LEN (I)
           END-PERFORM
           .
       S030-EX.
           EXIT.

      *    *** PIN1,CLOSE,OPEN 2
       S040-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN 2 ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           .
       S040-EX.
           EXIT.

      *    *** READ PRM1
       S050-10.
           PERFORM UNTIL WK-PRM1-EOF =   HIGH-VALUE

               READ    PRM1-F

               IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
                   UNSTRING PRM1-REC
                       DELIMITED BY "," OR "=" OR SPACE
                       INTO
                       WK-PRM1DT01
                       WK-PRM1DT02
                       WK-PRM1DT03
                       WK-PRM1DT04
                       WK-PRM1DT05
                       WK-PRM1DT06
                       WK-PRM1DT07
                       WK-PRM1DT08
                       WK-PRM1DT09
                       WK-PRM1DT10
                       WK-PRM1DT11
                       WK-PRM1DT12
                       WK-PRM1DT13
                       WK-PRM1DT14
                       WK-PRM1DT15
                       WK-PRM1DT16
                       WK-PRM1DT17
                       WK-PRM1DT18
                       WK-PRM1DT19
                       WK-PRM1DT20

                   EVALUATE TRUE
                       WHEN WK-PRM1DT01(1:3) =    "POS"
      *    *** POS= チェック
                           PERFORM S051-10     THRU    S051-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-IN"
      *    *** F-IN= チェック
                           PERFORM S054-10     THRU    S054-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-OT"
      *    *** F-OT= チェック
                           PERFORM S055-10     THRU    S055-EX

                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   IF      WK-PRM1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
               END-IF
           END-PERFORM

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
           .
       S050-EX.
           EXIT.

      *    *** POS= CHECK
       S051-10.

      *    *** 指定が無い項目は、そのまま出力
           IF      FUNCTION NUMVAL(WK-PRM1DT02) >=     1 AND
                   FUNCTION NUMVAL(WK-PRM1DT02) <=     100
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT02) TO P
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F POS=N PARA ERROR="
                           PRM1-REC
                 DISPLAY WK-PGM-NAME " POS=N 1-100の範囲で違う数字 指定"
                   STOP    RUN
           END-IF

           ADD     1           TO      TBL01-CNT (P)
           IF      TBL01-CNT (P) >     1
                   DISPLAY WK-PGM-NAME " PRM1-F POS=N PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " POS=N N同じ数字を指定している"
                           " 1-100の範囲で違う数字 指定"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT03(1:4) = "TYPE"
                   IF      WK-PRM1DT04(1:2) = "CH" OR "ZD"
                           MOVE    WK-PRM1DT04 TO      TBL01-TYPE (P)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F POS=N TYPE= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F POS=N"
                                   " TYPE= CH,ZDで指定"
                           STOP    RUN
                   END-IF

           END-IF

           IF      WK-PRM1DT05(1:5) = "SHIFT"
                   IF      WK-PRM1DT06(1:1) = "L" OR "C" OR "R"
                           MOVE    WK-PRM1DT06 TO      TBL01-SHIFT (P)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F POS=N SHIFT= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " POS=N SHIFT= L,C,Rで指定"
                                   " L:左寄せ､C=中央寄せ､R=右寄せ"
                           STOP    RUN
                   END-IF
           END-IF
           .
       S051-EX.
           EXIT.

      *    *** F-IN CHECK
       S054-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                           ACCEPT  WK-PIN1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-PIN1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-PIN1-F-NAME
           END-IF           .
       S054-EX.
           EXIT.

      *    *** F-OT CHECK
       S055-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " OUTPUT FILE NAME"
                           ACCEPT  WK-POT1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-POT1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-POT1-F-NAME
           END-IF
           .
       S055-EX.
           EXIT.

      *    *** PIN1 CSV を WRITE POT1 固定長に変換１
       S100-10.

      *     DISPLAY "S100-10 " PIN1-REC (1:80)
      *     DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
           MOVE    1           TO      WK-UNST-PTR
                                       WK-STO-PTR
                                       J
           MOVE    ZERO        TO      I
           MOVE    SPACE       TO      POT1-REC
                                       WK-PIN1-I1

           PERFORM UNTIL WK-UNST-PTR > WK-PIN1-LEN
      *    *** UNSTRING,STRING の INTO 項目 後ろスペースが入っている
      *    *** （２，３項目目）長さ可変長なのに、MOVEと同じ仕様？
                   UNSTRING PIN1-REC
                       DELIMITED BY ","
                       INTO WK-PIN1-I1 DELIMITER WK-DELI COUNT L 
                       WITH POINTER WK-UNST-PTR

                   ADD     1           TO      I

      *    *** １０バイト以下の項目で数字、スペースのみで構成される項目は
      *    *** 右寄せに変換する
                   MOVE    "N"         TO      SW-NUM
                   IF      TBL01-LEN(I) <=     10
                       PERFORM VARYING K FROM 1 BY 1
                               UNTIL K > TBL01-LEN(I) OR
                                     K > 10
                           IF      WK-PIN1-I1 (K:1) =  SPACE OR
                                   WK-PIN1-I1 (K:1) IS  NUMERIC
                               MOVE    "Y"         TO      SW-NUM
                           ELSE
                               MOVE    10          TO      K
                           END-IF
                       END-PERFORM
                   END-IF

                   IF      SW-NUM      =       "Y"
                       MOVE    TBL01-LEN(I)  TO      K2
                       MOVE    WK-PIN1-I1(1:TBL01-LEN(I)) TO WK-NUM-ITEM
                       MOVE    SPACE       TO
                               WK-PIN1-I1 (1:TBL01-LEN(I))
                       PERFORM VARYING K FROM TBL01-LEN(I) BY -1
                               UNTIL K < 1
                           IF      WK-NUM-ITEM (K:1) NOT = SPACE
                               MOVE    WK-NUM-ITEM (K:1) TO
                                       WK-PIN1-I1 (K2:1)
                               ADD     -1          TO      K2
                           END-IF
                       END-PERFORM
                    END-IF

                   STRING WK-PIN1-I1 DELIMITED BY ","
                          INTO POT1-REC
                          WITH POINTER WK-STO-PTR
      *    *** I=1 の時、2つ目の、WK-STO-PTRセットする
                   COMPUTE J = TBL01-LEN(I) + J
                   MOVE    J           TO      WK-STO-PTR

      *             DISPLAY "WK-PIN1-I1 =" WK-PIN1-I1 (1:70)
      *             DISPLAY "WK-UNST-PTR=" WK-UNST-PTR
      *             DISPLAY "WK-STO-PTR =" WK-STO-PTR
      *             DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
      *             DISPLAY "WK-DELI=" WK-DELI
      *             DISPLAY "L          =" L
      *             DISPLAY "I          =" I
      *             DISPLAY "TBL01-LEN  =" TBL01-LEN(I)
           END-PERFORM

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PIN1 CSV を WRITE POT1 固定長に変換２
       S200-10.

      *     DISPLAY "S100-10 " PIN1-REC (1:80)
      *     DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
           MOVE    1           TO      WK-UNST-PTR
                                       WK-STO-PTR
                                       J
           MOVE    ZERO        TO      I
           MOVE    SPACE       TO      POT1-REC
                                       WK-PIN1-I1

           PERFORM UNTIL WK-UNST-PTR > WK-PIN1-LEN
      *    *** UNSTRING,STRING の INTO 項目 後ろスペースが入っている
      *    *** （２，３項目目）長さ可変長なのに、MOVEと同じ仕様？
                   UNSTRING PIN1-REC
                       DELIMITED BY ","
                       INTO WK-PIN1-I1 DELIMITER WK-DELI COUNT L 
                       WITH POINTER WK-UNST-PTR

                   ADD     1           TO      I
                   IF      I           <       101 AND
                           TBL01-CNT (I) =     1

      *                     DISPLAY ":" WK-PIN1-I1 (1:TBL01-LEN(I))
      *                            ": 変更前 TBL01-LEN(I)=" TBL01-LEN(I)
                           CALL "C$JUSTIFY"    USING 
                                              WK-PIN1-I1(1:TBL01-LEN(I))
                                              TBL01-SHIFT (I)
      *    *** TYPE=ZD,SHIFT=R は前SPACE を ZEROにする
                           IF      TBL01-TYPE(I) =     "ZD" AND
                                   TBL01-SHIFT(I) =    "R"
                                   INSPECT WK-PIN1-I1 (1:TBL01-LEN(I)) 
                                           REPLACING 
                                           LEADING SPACE BY ZERO
                           END-IF
      *                     DISPLAY ":" WK-PIN1-I1 (1:TBL01-LEN(I))
      *                             ": 変換後 C$JUSTIFY " TBL01-SHIFT (I)
                   END-IF

      *    *** 項目の後にカンマ自動挿入
                   STRING WK-PIN1-I1 (1:TBL01-LEN(I)) 
                          ","  DELIMITED BY SIZE
                          INTO POT1-REC
                          WITH POINTER WK-STO-PTR

      *    *** I=1 の時、2つ目の、WK-STO-PTRセットする
                   COMPUTE J = TBL01-LEN(I) + J + 1
                   MOVE    J           TO      WK-STO-PTR

      *             DISPLAY "WK-PIN1-I1 =" WK-PIN1-I1 (1:70)
      *             DISPLAY "WK-UNST-PTR=" WK-UNST-PTR
      *             DISPLAY "WK-STO-PTR =" WK-STO-PTR
      *             DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
      *             DISPLAY "WK-DELI=" WK-DELI
      *             DISPLAY "L          =" L
      *             DISPLAY "I          =" I
      *             DISPLAY "TBL01-LEN  =" TBL01-LEN(I)
           END-PERFORM

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE 2 ERROR STATUS="
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
           MOVE    WK-PRM1-CNT TO     WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO     WK-PIN1-CNT-E
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
