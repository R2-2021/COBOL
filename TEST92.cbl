      *    *** アニメ一覧の出力
      *    *** PRINT AREA 2次元でセット　（４０行／１ページ）
      *    *** 改行数、Ａ４縦、Ａ４横　プログラムで設定
      *    *** KEY ブレイク追加
      *    *** TEST35 => TEST92 (UTF8 入力に変更)

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST92.

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
           03  FILLER          PIC  X(350).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST92  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST92.PIN1".
      *    *** 漢字コードUTF8
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "TEST28_201901.POT1".
      *         "TEST28_2019.POT1".
               "TEST28_2021.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST92.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

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
           03  WK-KISETU       PIC  X(003) VALUE SPACE.
           03  WK-TITLE        PIC  X(060) VALUE SPACE.
           03  WK-TITLE2       PIC  X(060) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.
           03  WK-YYYYMM-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.
           03  WK-CNT2         PIC  X(012) VALUE SPACE.

      *    *** 初期値 MODE=AK   (ANK=>KANJI)
           03  WK-MODE         PIC  X(002) VALUE "AK".
      *    *** 初期値 HENKAN=UU (UTF8=>UTF8)
           03  WK-HENKAN       PIC  X(006) VALUE "UU".

           03  WK-TIT1.
      *    *** ＊＊＊　アニメタイト
             05  FILLER        PIC  X(030) VALUE
       X"EFBC8AEFBC8AEFBC8AEFBEA0E382A2E3838BE383A1E382BFE382A4E38388".
      *    *** ル一覧表　＊＊＊
             05  FILLER        PIC  X(024) VALUE
       X"E383ABE4B880E8A6A7E8A1A8EFBEA0EFBC8AEFBC8AEFBC8A".
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

           03  WK-TIT1-A4T.
             05  FILLER        PIC  X(040) VALUE "TEST92-T".
             05  WK-TIT1-A4T-1 PIC  X(082) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05  FILLER        PIC  X(070) VALUE "TEST92-Y".
             05  WK-TIT1-A4Y-1 PIC  X(082) VALUE SPACE.

      *    *** 印刷ページ設定でページあたりの行文字数、縦行数が
      *    *** 変わるので、以下設定で印刷する

      *    *** 印刷ページ設定
      *    *** 半角フォント　ＭＳゴシック
      *    *** 全角フォント　ＭＳゴシック
      *    *** フォント高　２６ｍｍ、７．３ｐｔ
      *    *** 行送り０％
      *    *** 余白　上１０、下１０、右１０、左１０ｍｍ
      *    *** 
      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４

      *    *** MAX=146,A4縦用
      *    *** 61*2=122
           03  WK-MID1-A4T.
             05                OCCURS 2.
               07              PIC  X(007) VALUE " NO.   ".
      *    *** 年
               07              PIC  X(003) VALUE X"E5B9B4".
               07              PIC  X(001) VALUE SPACE.
      *    *** 季
               07              PIC  X(003) VALUE X"E5ADA3".
               07              PIC  X(001) VALUE SPACE.
      *    *** タイトル
               07              PIC  X(012) VALUE
                   X"E382BFE382A4E38388E383AB".
               07              PIC  X(033) VALUE SPACE.
      *    *** 54*2=108
           03  WK-HAI-A4T   PIC  X(108) VALUE ALL
           "----------------------------------------------------- ".

      *    *** MAX=213,A4横用
      *    *** 61*3=183 (7+3+3+8+33=54)
           03  WK-MID1-A4Y.
             05                OCCURS 3.
               07              PIC  X(007) VALUE " NO.   ".
      *    *** 年
               07              PIC  X(003) VALUE X"E5B9B4".
               07              PIC  X(001) VALUE SPACE.
      *    *** 季
               07              PIC  X(003) VALUE X"E5ADA3".
               07              PIC  X(001) VALUE SPACE.
      *    *** タイトル
               07              PIC  X(012) VALUE
                   X"E382BFE382A4E38388E383AB".
               07              PIC  X(033) VALUE SPACE.
      *    *** 54*3=162
           03  WK-HAI-A4Y   PIC  X(162) VALUE ALL
           "----------------------------------------------------- ".
      *    *** EFBEA0 = 漢字スペース（空白表示）
           03  WK-SPACE.
             05                PIC  X(010) VALUE SPACE.
             05                PIC  X(003) VALUE X"EFBEA0".
             05                PIC  X(001) VALUE SPACE.
             05                PIC  X(060) VALUE ALL X"EFBEA0".
      *    *** 小計、総合計
           03  WK-TOTAL.
             05                PIC  X(045) VALUE ALL X"EFBEA0".
             05                PIC  X(012) VALUE
      *    *** 明細件数
                 X"E6988EE7B4B0E4BBB6E695B0".
             05                PIC  X(003) VALUE X"EFBEA0".
             05  WK-CNT-E      PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
      *    *** 件
             05                PIC  X(003) VALUE X"E4BBB6".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 101
                               PIC  X(350) VALUE SPACE.

       01  KEY-AREA.
             05  KEY-OLD.
               07  KEY-OYYYY   PIC  X(004) VALUE LOW-VALUE.
      *         07  KEY-OMM     PIC  X(002) VALUE LOW-VALUE.
               07  KEY-OKISETU PIC  X(003) VALUE LOW-VALUE.
             05  KEY-NEW.
               07  KEY-NYYYY   PIC  X(004) VALUE LOW-VALUE.
      *         07  KEY-NMM     PIC  X(002) VALUE LOW-VALUE.
               07  KEY-NKISETU PIC  X(003) VALUE LOW-VALUE.

       01  CNS-AREA.
      *    *** PX の印字位置
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 6.
           03  CNS-P3          BINARY-LONG SYNC VALUE 11.
           03  CNS-P4          BINARY-LONG SYNC VALUE 15.
      *    *** PX の桁数
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 3.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 60.
      *    *** 表示上の長さ 5+5+(3/3*2+1)+(60/3*2+1)=54
           03  CNS-PJ-L        BINARY-LONG SYNC VALUE 54.
      *    *** プリントエリア上の長さ WK-SPACE
           03  CNS-PS-L        BINARY-LONG SYNC VALUE 74.
      *    *** P1-PX の印字合計桁数　スペース含む
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
           03  P4              BINARY-LONG SYNC VALUE ZERO.
           03  PX              BINARY-LONG SYNC VALUE ZERO.

      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.

      *    *** "1" = A4縦,
      *    *** "0" = A4横
           03  SW-A4TATE       PIC  X(001) VALUE "0".

      *    *** "1" = 1行　改行
      *    *** "2" = 2行　改行
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
                   IF    WK-PIN1-EOF   NOT =   HIGH-VALUE
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

      *    *** 0件でも、ＡＴ　ＥＮＤ時、件数出力
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

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

      *    *** + 4 は各項目の最終桁スペースにする為
           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + 4
      *    *** 行あたりの文字数：横時２１３  縦時１４６
      *    *** 縦方向の行数：    横時７１    縦時１０４

           IF      SW-A4TATE   =       "1"
      *    *** 146 バイト目文字があると改行してしまうが、CNS-L-SIZEの
      *    *** 最終桁はスペースなので、146 を　そのままCNS-L-SIZEで割る
      *    *** 割算で商を求め、商 * CNS-L-SIZEを求める
                   COMPUTE C = 146 / ( CNS-PJ-L + 1 )
                   COMPUTE C = C   * ( CNS-PS-L + 1 )
      *    *** 50 = 101 / 2
                   IF      SW-KAIGYO   =       "2"
                           MOVE    50          TO      R
                   ELSE
      *    *** 101 = 104 - 3 (ヘッダー)
                           MOVE   101          TO      R
                   END-IF
           ELSE
                   COMPUTE C = 213 / ( CNS-PJ-L + 1 )
                   COMPUTE C = C   * ( CNS-PS-L + 1 )
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

      *    *** 漢字スペース無表示
           MOVE    ALL X"EFBEA0" TO    WK-TITLE

      *    *** UTF8 => UTF8 (ANK=>KANJI)
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    60          TO      WDE05-BUF1-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-TITLE2
                                       WK-TITLE
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

      *    *** 明細出力後は、改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

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

           COMPUTE PX = P1 + 14
      *    *** 漢字スペース無表示
           MOVE   ALL X"EFBEA0" TO     PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
      *    *** ＊＊＊年月小計＊＊＊
           MOVE
         X"EFBC8AEFBC8AEFBC8AE5B9B4E69C88E5B08FE8A888EFBC8AEFBC8AEFBC8A"
                               TO      PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** 改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           MOVE    WK-YYYYMM-CNT TO    WK-CNT-E
           MOVE    WK-TOTAL    TO      PR-LINE (J) (P1:74)

           MOVE    ZERO        TO      WK-YYYYMM-CNT

           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S130-EX.
           EXIT.

      *    *** YYYY ブレイク
       S140-10.

      *    *** 改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** YYYY は明細と同じ位置に印字なので、下記でよい
           IF      KEY-OYYYY   =       LOW-VALUE
                   CONTINUE
           ELSE
                   MOVE    KEY-OYYYY   TO      PR-LINE (J) (P2:CNS-P2-L)
                   MOVE    X"EFBEA0"   TO      PR-LINE (J) (P3:CNS-P3-L)
           END-IF

           COMPUTE PX = P1 + 14
      *    *** 漢字スペース無表示
           MOVE   ALL X"EFBEA0" TO     PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
      *    *** ＊＊＊年　小計＊＊＊
           MOVE
         X"EFBC8AEFBC8AEFBC8AE5B9B4EFBEA0E5B08FE8A888EFBC8AEFBC8AEFBC8A"
                               TO      PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** 改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           MOVE    WK-YYYY-CNT TO      WK-CNT-E
           MOVE    WK-TOTAL    TO      PR-LINE (J) (P1:74)

           MOVE    ZERO        TO      WK-YYYY-CNT

           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S140-EX.
           EXIT.

      *    *** AT END PRINT
       S150-10.

      *    *** 改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           COMPUTE PX = P1 + 6
      *    *** 　
           MOVE    X"EFBEA0"   TO      PR-LINE (J) (PX:03)

           COMPUTE PX = P1 + 14
      *    *** 漢字スペース無表示
           MOVE   ALL X"EFBEA0" TO     PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
      *    *** ＊＊＊総　合計＊＊＊
           MOVE
         X"EFBC8AEFBC8AEFBC8AE7B78FEFBEA0E59088E8A888EFBC8AEFBC8AEFBC8A"
                               TO      PR-LINE (J) (PX:30)
           COMPUTE PX = PX + 30
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** 改行する
           MOVE    WK-SPACE    TO      PR-LINE (J) (P1:CNS-PS-L)
           ADD     1           TO      J

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           MOVE    WK-PIN1-CNT TO      WK-CNT-E
           MOVE    WK-TOTAL    TO      PR-LINE (J) (P1:74)

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

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

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
