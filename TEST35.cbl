      *    *** アニメ一覧の出力
      *    *** PRINT AREA 2次元でセット＿（４０行／１ページ）
      *    *** 改行数、Ａ４縦、Ａ４横＿プログラムで設定
      *    *** KEY ブレイク追加

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST35.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** アニメデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アニメ一覧
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(250).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST35  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST35.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "TEST28_201110_201810.csv".
      *         "TEST28_201110_201810.UTF8.csv".
                "TEST50SJIS.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST35.POT1".

           03  WK-PIN1-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS       PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.

           03  WK-SEQNO        PIC  X(004) VALUE SPACE.
           03  WK-YYYY         PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  X(002) VALUE SPACE.
           03  WK-KISETU       PIC  X(002) VALUE SPACE.
           03  WK-TITLE        PIC  X(020) VALUE SPACE.
           03  WK-TITLE2       PIC  X(020) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.
           03  WK-YYYYMM-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.

           03  WK-TIT1.
             05  FILLER        PIC  X(040) VALUE
                 "＊＊＊＿アニメタイトル一覧表＿＊＊＊＿＿".
             05  FILLER        PIC  X(010) VALUE SPACE.
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

           03  WK-TIT1-A4T.
             05  FILLER        PIC  X(040) VALUE "TEST35-T".
             05  WK-TIT1-A4T-1 PIC  X(076) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05  FILLER        PIC  X(070) VALUE "TEST35-Y".
             05  WK-TIT1-A4Y-1 PIC  X(076) VALUE SPACE.

      *    *** 印刷ページ設定でページあたりの行文字数、縦行数が
      *    *** 変わるので、以下設定で印刷する

      *    *** 印刷ページ設定
      *    *** 半角フォント＿ＭＳゴシック
      *    *** 全角フォント＿ＭＳゴシック
      *    *** フォント高＿２６ｍｍ、７．３ｐｔ
      *    *** 行送り０％
      *    *** 余白＿上１０、下１０、右１０、左１０ｍｍ
      *    *** 
      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４

      *    *** MAX=146,A4縦用
      *    *** 34*4=136
           03  WK-MID1-A4T  PIC  X(136) VALUE ALL
              " NO.   年 季 タイトル＿＿＿＿＿＿ ".
           03  WK-HAI-A4T   PIC  X(136) VALUE ALL
              "--------------------------------- ".

      *    *** MAX=213,A4横用
      *    *** 34*6=204
           03  WK-MID1-A4Y  PIC  X(204) VALUE ALL
              " NO.   年 季 タイトル＿＿＿＿＿＿ ".
           03  WK-HAI-A4Y   PIC  X(204) VALUE ALL
              "--------------------------------- ".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 101
                               PIC  X(213) VALUE SPACE.

       01  KEY-AREA.
             05  KEY-OLD.
               07  KEY-OYYYY   PIC  X(004) VALUE LOW-VALUE.
               07  KEY-OKISETU PIC  X(002) VALUE LOW-VALUE.
             05  KEY-NEW.
               07  KEY-NYYYY   PIC  X(004) VALUE LOW-VALUE.
               07  KEY-NKISETU PIC  X(002) VALUE LOW-VALUE.

       01  CNS-AREA.
      *    *** PX の印字位置
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 6.
           03  CNS-P3          BINARY-LONG SYNC VALUE 11.
           03  CNS-P4          BINARY-LONG SYNC VALUE 14.
      *    *** PX の桁数
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 20.
      *    *** P1-PX の印字合計桁数＿スペース含む
           03  CNS-L-SIZE      BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO
           03  PX              BINARY-LONG SYNC VALUE ZERO

      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.

      *    *** "1" = A4縦,
      *    *** "0" = A4横
           03  SW-A4TATE       PIC  X(001) VALUE "1".

      *    *** "1" = 1行＿改行
      *    *** "2" = 2行＿改行
           03  SW-KAIGYO       PIC  X(001) VALUE "1".

      *    *** "0" = 明細無し
      *    *** "1" = 明細出力
           03  SW-MEISAI       PIC  X(001) VALUE "1".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM WITH TEST AFTER
                   UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      WK-PIN1-EOF NOT =   HIGH-VALUE
      *    *** MEISAI PRINT TBL SET
                           PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-IF

                   IF      KEY-OLD     =       KEY-NEW
                       CONTINUE
                   ELSE
      *    *** YYYY,MM ブレイク
                       PERFORM S130-10     THRU    S130-EX

                       IF      KEY-OYYYY   =       KEY-NYYYY
                           CONTINUE
                       ELSE
      *    *** YYYY ブレイク
                           PERFORM S140-10     THRU    S140-EX
                           IF      KEY-NEW     =       HIGH-VALUE

      *    *** 0件でも、ＡＴ＿ＥＮＤ時、件数出力
      *    *** AT END PRINT
                               PERFORM S150-10     THRU    S150-EX
                           ELSE
                               CONTINUE
                           END-IF
                       END-IF
                   END-IF
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

           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

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

      *    *** + 4 は各項目の最終桁スペースにする為
           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + 4
      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４

           IF      SW-A4TATE   =       "1"
      *    *** 146 バイト目文字があると改行してしまうが、CNS-L-SIZEの
      *    *** 最終桁はスペースなので、146 を＿そのままCNS-L-SIZEで割る
      *    *** 割算で商を求め、商 * CNS-L-SIZEを求める
                   COMPUTE C = 146 / CNS-L-SIZE
                   COMPUTE C = C   * CNS-L-SIZE
      *    *** 50 = 101 / 2
                   IF      SW-KAIGYO   =       "2"
                           MOVE    50          TO      R
                   ELSE
      *    *** 101 = 104 - 3 (ヘッダー)
                           MOVE   101          TO      R
                   END-IF
           ELSE
                   COMPUTE C = 213 / CNS-L-SIZE
                   COMPUTE C = C   * CNS-L-SIZE
                   IF      SW-KAIGYO   =       "2"
      *    *** 34 = 68 / 2
                           MOVE    34          TO      R
                   ELSE
      *    *** 68 = 71 - 3 (ヘッダー)
                           MOVE    68          TO      R
                   END-IF
           END-IF

           MOVE    CNS-P1      TO      P1
           MOVE    CNS-P2      TO      P2
           MOVE    CNS-P3      TO      P3
           MOVE    CNS-P4      TO      P4
           .
       S010-EX.
           EXIT.

      *    *** PIN1-F READ
       S020-10.

           MOVE    KEY-NEW     TO      KEY-OLD

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                            DELIMITED BY ","
                       INTO
      *                      WK-SEQNO
                            WK-YYYY
                            WK-MM
                            WK-KISETU
                            WK-TITLE2
                            WK-SITE

                   MOVE    WK-YYYY     TO      KEY-NYYYY
      *             MOVE    WK-MM       TO      KEY-NMM
                   MOVE    WK-KISETU   TO      KEY-NKISETU
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               KEY-NEW
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *    *** 19,1 から漢字始まる時、セットしない
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      M 

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 19
      *    *** 漢字か？一部漢字でない部分あり、
      *    *** 後から指定された部分も指定してない
               IF ( WK-TITLE2 (I:2) >= X"8140" AND 
                    WK-TITLE2 (I:2) <= X"9FFC" )   OR
                  ( WK-TITLE2 (I:2) >= X"E040" AND 
                    WK-TITLE2 (I:2) <= X"EAA4" )
                         MOVE   WK-TITLE2 (I:2) TO   WK-TITLE (M:2)
      *    *** J 1,3,5...
                         ADD     1           TO      I
                         ADD     2           TO      M
                   ELSE
                         MOVE    WK-TITLE2 (I:1) TO   WK-TITLE (M:1)
      *    *** J 1,2,3...
                         ADD     1           TO      M
                   END-IF
           END-PERFORM
           .
       S020-EX.
           EXIT.

      *    *** PRINT MEISAI TBL SET
       S100-10.

           ADD     1           TO      WK-YYYYMM-CNT
                                       WK-YYYY-CNT
           IF      SW-MEISAI   =       "0"
                   CONTINUE
           ELSE
      *         MOVE    WK-SEQNO    TO      PR-LINE (J) (P1:CNS-P1-L)
                   MOVE    WK-PIN1-CNT TO      WK-CNT
                   MOVE    WK-CNT      TO      PR-LINE (J) (P1:CNS-P1-L)
                   MOVE    WK-YYYY     TO      PR-LINE (J) (P2:CNS-P2-L)
                   MOVE    WK-KISETU   TO      PR-LINE (J) (P3:CNS-P3-L)
                   MOVE    WK-TITLE    TO      PR-LINE (J) (P4:CNS-P4-L)

                   ADD     1           TO      J
                   MOVE    1           TO      J2
      *    *** PRINT TBL WRITE CHECK
                   PERFORM S110-10     THRU    S110-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PRINT TBL WRITE CHECK
       S110-10.

           IF      J           >       R
                   MOVE    1           TO       J
                   ADD     CNS-L-SIZE  TO       P1
                   IF      P1          >        C

      *    *** PRINT TBL WRITE
                           PERFORM S120-10     THRU    S120-EX

                           MOVE    SPACE       TO      PRINT-AREA
                           MOVE    CNS-P1      TO      P1
                           MOVE    CNS-P2      TO      P2
                           MOVE    CNS-P3      TO      P3
                           MOVE    CNS-P4      TO      P4
                   ELSE
                           ADD     CNS-L-SIZE  TO      P2
                           ADD     CNS-L-SIZE  TO      P3
                           ADD     CNS-L-SIZE  TO      P4
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** PRINT TBL WRITE
       S120-10.

           ADD     1           TO      WK-PAGE
           MOVE    WK-PAGE     TO      WK-TIT1-PAGE

           IF      SW-A4TATE   =       "1"
                   MOVE    WK-TIT1     TO      WK-TIT1-A4T-1
                   WRITE   POT1-REC    FROM    WK-TIT1-A4T
                   ADD     1           TO      WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           MOVE    SPACE       TO  POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO  WK-POT1-CNT
                   ELSE
                           CONTINUE
                   END-IF
                   WRITE   POT1-REC    FROM    WK-MID1-A4T
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-HAI-A4T
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   MOVE    WK-TIT1     TO      WK-TIT1-A4Y-1
                   WRITE   POT1-REC    FROM    WK-TIT1-A4Y
                   ADD     1           TO      WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           MOVE    SPACE       TO  POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO  WK-POT1-CNT
                   ELSE
                           CONTINUE
                   END-IF
                   WRITE   POT1-REC    FROM    WK-MID1-A4Y
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-HAI-A4Y
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           PERFORM VARYING K  FROM 1 BY 1
                   UNTIL   K > R
                   WRITE   POT1-REC    FROM     PR-LINE(K)
                   ADD     1           TO       WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           IF      SW-A4TATE =       "0"
                                   IF      K         =       R
                                           CONTINUE
                                   ELSE
                                           MOVE    SPACE     TO POT1-REC
                                           WRITE   POT1-REC
                                           ADD     1     TO  WK-POT1-CNT
                                   END-IF
                            ELSE
                                   MOVE    SPACE     TO POT1-REC
                                   WRITE   POT1-REC
                                   ADD     1     TO  WK-POT1-CNT
                            END-IF
                   ELSE
                            CONTINUE
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** YYYY,MM(季節) ブレイク
       S130-10.

      *    *** 明細出力後は、J2=1
           IF      J2          =       1
                   ADD     1           TO      J
           END-IF
      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** YYYY,MM は明細と同じ位置に印字なので、下記でよい
           IF      KEY-OYYYY   =       LOW-VALUE
               AND KEY-OKISETU =       LOW-VALUE
                   CONTINUE
           ELSE
                   MOVE    KEY-OYYYY   TO      PR-LINE (J) (P2:CNS-P2-L)
                   MOVE    KEY-OKISETU TO      PR-LINE (J) (P3:CNS-P3-L)
           END-IF

           COMPUTE PX = P1 + 13
           MOVE    "** 年月小計" TO    PR-LINE (J) (PX:11)
           MOVE    WK-YYYYMM-CNT TO    WK-CNT
           COMPUTE PX = PX + 11
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:4)
           COMPUTE PX = PX + 4
           MOVE    " 件**"     TO      PR-LINE (J) (PX:5)
           MOVE    ZERO        TO      WK-YYYYMM-CNT

           ADD     2           TO      J
           MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S130-EX.
           EXIT.

      *    *** YYYY ブレイク
       S140-10.

      *    *** 明細出力後は、J2=1、明細出力直後にYYYY ブレイクは無いので
      *    *** このIF は不要と思われるが残す

           IF      J2          =       1
                   ADD     1           TO      J
           END-IF

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** YYYY は明細と同じ位置に印字なので、下記でよい
           IF      KEY-OYYYY   =       LOW-VALUE
                   CONTINUE
           ELSE
                   MOVE    KEY-OYYYY   TO      PR-LINE (J) (P2:CNS-P2-L)
           END-IF

           COMPUTE PX = P1 + 13
           MOVE    "** 年＿小計" TO    PR-LINE (J) (PX:11)
           MOVE    WK-YYYY-CNT TO      WK-CNT
           COMPUTE PX = PX + 11
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:4)
           COMPUTE PX = PX + 4
           MOVE    " 件**"     TO      PR-LINE (J) (PX:5)
           MOVE    ZERO        TO      WK-YYYY-CNT

           IF      KEY-NEW     =       HIGH-VALUE
      *    *** YYYY ブレイク後,総 合計 2行改行にする
                   ADD     2           TO      J
                   MOVE    J           TO      J2
           ELSE
      *    *** YYYY ブレイク後,明細出力又は年 小計 2行改行にする
                   ADD     3           TO      J
                   MOVE    J           TO      J2
           END-IF

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S140-EX.
           EXIT.

      *    *** AT END PRINT
       S150-10.

           COMPUTE PX = P1 + 13
           MOVE    "** 総＿合計" TO    PR-LINE (J) (PX:11)
           MOVE    WK-PIN1-CNT TO      WK-CNT
           COMPUTE PX = PX + 11
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:4)
           COMPUTE PX = PX + 4
           MOVE    " 件**"     TO      PR-LINE (J) (PX:5)

      *    *** PRINT TBL WRITE
           PERFORM S120-10     THRU    S120-EX
           .
       S150-EX.
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
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 ﾍﾟｰｼﾞ= " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
