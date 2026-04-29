      *    *** アニメ一覧の出力
      *    *** PRINT AREA 2次元でセット
      *    *** 下線セット、Ａ４縦、Ａ４横  プログラムで設定

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST31.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** アニメ  タイトルデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アニメ一覧
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
             05                PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(300).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST31  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "TEST28_201110_2018XX.CSV".
                "TEST28_202601SJIS.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST31.POT1".

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
           03  WK-TITLE2       PIC  X(022) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.

           03  WK-CNT          PIC  -ZZZ,ZZ9 VALUE SPACE.

           03  WK-TIT1.
             05                PIC  X(006) VALUE "＊＊＊".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(020) VALUE "アニメタイトル一覧表".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(006) VALUE "＊＊＊".
             05                PIC  X(014) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05                PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.
             05                PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.

           03  WK-TIT1-A4T.
             05                PIC  X(040) VALUE "TEST31-T".
             05  WK-TIT1-A4T-1 PIC  X(076) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05                PIC  X(070) VALUE "TEST31-Y".
             05  WK-TIT1-A4Y-1 PIC  X(076) VALUE SPACE.

      *    *** 印刷ページ設定でページあたりの行文字数、縦行数が
      *    *** 変わるので、以下設定で印刷する

      *    *** 印刷ページ設定
      *    *** 半角フォント  ＭＳゴシック
      *    *** 全角フォント  ＭＳゴシック
      *    *** フォント高  ２７ｍｍ、７．６ｐｔ
      *    *** 行送り０％
      *    *** 余白  上１０、下１０、右１０、左１０ｍｍ
      *    *** 
      *    *** 行あたりの文字数：横時１９７  縦時１３５
      *    *** 縦方向の行数：    横時７０    縦時１０２

           03  WK-YOKO-MOJI    BINARY-LONG SYNC VALUE 197
           03  WK-YOKO-GYO     BINARY-LONG SYNC VALUE  70.
           03  WK-TATE-MOJI    BINARY-LONG SYNC VALUE 135.
           03  WK-TATE-GYO     BINARY-LONG SYNC VALUE 102.
           03  WK-GYO-4        BINARY-LONG SYNC VALUE ZERO.
           03  WK-REMAINDER    BINARY-LONG SYNC VALUE ZERO.

      *    *** MAX=135,A4縦用
      *    *** 39*3=117
           03  WK-MID1-A4T.
             05                PIC  X(117) VALUE ALL
                 "     SEQ    年 季 タイトル             ".
      *           1234**7890*23*56789012345678901234*

      *    *** MAX=197,A4横用
      *    *** 39*5=195
           03  WK-MID1-A4Y.
             05                PIC  X(195) VALUE ALL
                 "     SEQ    年 季 タイトル             ".

      *    *** 画面項目
           03  WK-KEI1         PIC  X(002) VALUE "─".
           03  WK-KEI2         PIC  X(002) VALUE "│".
           03  WK-KEI3         PIC  X(002) VALUE "┌".
           03  WK-KEI4         PIC  X(002) VALUE "┐".
           03  WK-KEI5         PIC  X(002) VALUE "┘".
           03  WK-KEI6         PIC  X(002) VALUE "└".
           03  WK-KEI7         PIC  X(002) VALUE "├".
           03  WK-KEI8         PIC  X(002) VALUE "┬".
           03  WK-KEI9         PIC  X(002) VALUE "┤".
           03  WK-KEI10        PIC  X(002) VALUE "┴".
           03  WK-KEI11        PIC  X(002) VALUE "┼".

           03  WK-KEI1-A4T.
             05                OCCURS 3.
               07              PIC  X(038) VALUE ALL "─".
               07              PIC  X(001) VALUE SPACE.

           03  WK-KEI1-A4Y.
             05                OCCURS 5.
               07              PIC  X(038) VALUE ALL "─".
               07              PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 102
                               PIC  X(197) VALUE SPACE.

       01  CNS-AREA.
      *    *** PX の印字位置
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 11.
           03  CNS-P3          BINARY-LONG SYNC VALUE 16.
           03  CNS-P4          BINARY-LONG SYNC VALUE 19.

      *    *** PX の桁数
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 8.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 20.

      *    *** P1-PX の印字合計桁数  スペース含む
           03  CNS-L-SIZE      BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C2              BINARY-LONG SYNC VALUE 1.
           03  C3              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  J2              BINARY-LONG SYNC VALUE 1.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO
           03  PX              BINARY-LONG SYNC VALUE ZERO

           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
      *    *** "1" = A4縦,
      *    *** "0" = A4横
      *     03  SW-A4TATE       PIC  X(001) VALUE "1".
           03  SW-A4TATE       PIC  X(001) VALUE "0".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU      S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PRINT TABLE SET
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** AT END 時処理
           PERFORM S120-10     THRU    S120-EX

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
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *    *** 5は定義の*の数、印字しない所
           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + 5

      *    *** WK-REMAINDER 縦、横どちらも、ゼロなので、未使用とする
           IF      SW-A4TATE   =       "1"
      *    *** 割算で商を求め、商 * CNS-L-SIZEを求める
                   COMPUTE C3 = WK-TATE-MOJI / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
                   COMPUTE WK-GYO-4 = WK-TATE-GYO - 4
                   DIVIDE WK-GYO-4 BY 2 GIVING R 
                          REMAINDER WK-REMAINDER
           ELSE
                   COMPUTE C3 = WK-YOKO-MOJI / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
                   COMPUTE WK-GYO-4 = WK-YOKO-GYO - 4
                   DIVIDE WK-GYO-4 BY 2 GIVING R 
                          REMAINDER WK-REMAINDER
           END-IF

           MOVE    CNS-P1      TO      P1
           MOVE    CNS-P2      TO      P2
           MOVE    CNS-P3      TO      P3
           MOVE    CNS-P4      TO      P4
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
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
           END-READ

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
                         ADD    1    TO     I
                         ADD    2    TO     M
                   ELSE
                         MOVE   WK-TITLE2 (I:1) TO   WK-TITLE (M:1)
      *    *** J 1,2,3...
                         ADD    1    TO     M
                   END-IF
           END-PERFORM
           .
       S020-EX.
           EXIT.

      *    *** PRINT TABLE SET
       S100-10.

           CALL    "C$JUSTIFY" USING   WK-SEQNO "R"
           MOVE    WK-SEQNO    TO      PR-LINE (J) (P1:CNS-P1-L)
           MOVE    WK-YYYY     TO      PR-LINE (J) (P2:CNS-P2-L)
           MOVE    WK-KISETU   TO      PR-LINE (J) (P3:CNS-P3-L)
           MOVE    WK-TITLE    TO      PR-LINE (J) (P4:CNS-P4-L)

           ADD     1           TO      J
           IF      J           >       R
                   MOVE    1           TO      J
                   ADD     1           TO      C2
                   IF      C2          >       C3
      *    *** PRINT TBL WRITE
                           PERFORM S110-10     THRU    S110-EX

                           MOVE    SPACE       TO      PRINT-AREA
                           MOVE    CNS-P1      TO      P1
                           MOVE    CNS-P2      TO      P2
                           MOVE    CNS-P3      TO      P3
                           MOVE    CNS-P4      TO      P4
                           MOVE    1           TO      C2
                   ELSE
                           ADD     CNS-L-SIZE  TO      P1
                           ADD     CNS-L-SIZE  TO      P2
                           ADD     CNS-L-SIZE  TO      P3
                           ADD     CNS-L-SIZE  TO      P4
                   END-IF
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PRINT TBL WRITE
       S110-10.

           ADD     1           TO      WK-PAGE
           MOVE    WK-PAGE     TO      WK-TIT1-PAGE
           IF      SW-A4TATE   =       "1"
                   MOVE    WK-TIT1     TO      WK-TIT1-A4T-1
                   MOVE    WK-TIT1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    SPACE       TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

           ELSE
                   MOVE    WK-TIT1     TO      WK-TIT1-A4Y-1
                   MOVE    WK-TIT1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    SPACE       TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

           END-IF

           PERFORM VARYING K FROM 1 BY 1
                   UNTIL   K > R
                   MOVE    PR-LINE(K)  TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   IF      SW-A4TATE   =       "1"
                           MOVE    WK-KEI1-A4T TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX

                   ELSE
                           MOVE    WK-KEI1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX

                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** AT END 時処理
      *    *** 0件でも、ＡＴ  ＥＮＤ時、件数出力
       S120-10.

           COMPUTE PX = P1 + 10
           MOVE    "*** "      TO      PR-LINE (J) (PX:4)
           MOVE    WK-PIN1-CNT TO      WK-CNT
           COMPUTE PX = PX + 4
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:8)
           COMPUTE PX = PX + 8
           MOVE    " 件 ***"   TO      PR-LINE (J) (PX:7)

      *    *** PRINT TBL WRITE
           PERFORM S110-10     THRU    S110-EX
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 頁   = " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
