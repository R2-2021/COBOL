      *    *** YouTube html ゆいかおり、石原夏織　Ｗatch　Ｌist 作成
      *    *** 
      *    *** チャプターの時、表題と表示時間ずれているが、
      *    *** TEST69で直すの難しい為、TEST69で出力されるWATCHの秒数変更する
      *    *** 
      *    *** JOB TEST69
      *    ***        |
      *    ***     TEST68

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST68.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** WATCH DATA　ＵＴＦ８
      *    *** TEST10 => TEST66 => TEST69,TEST68 にした
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** twiiter,instagram データ　未使用
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** HTML データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(500).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST68  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
      *         "TEST28_201110_202007.csv".
               "TEST69.POT3".

           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST68.PIN2".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST68.POT1".

      *    *** 漢字はＳＪＩＳでないと文字化けする
           03  WK-POT1-F-NAME.
             05  PIC X(023) VALUE "C:\Users\koko\OneDrive\".
             05  PIC X(012) VALUE "ドキュメント".
             05  PIC X(013) VALUE "\HTML\YouTube".
             05  PIC X(004) VALUE "声優".
             05  PIC X(006) VALUE "\index".
             05  WK-POT1-F-NAME21 PIC X(200) VALUE SPACE.
           03  WK-POT1-F-NAME2  PIC X(200) VALUE SPACE.
           03  WK-POT1-F-NAME2U PIC X(200) VALUE SPACE.

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-SITE1        PIC  X(200) VALUE SPACE.
           03  WK-SITE2        PIC  X(200) VALUE SPACE.
           03  WK-SITE3        PIC  X(200) VALUE SPACE.
           03  WK-SITE4        PIC  X(200) VALUE SPACE.
           03  WK-SITE5        PIC  X(200) VALUE SPACE.
           03  WK-KENSAKU      PIC  X(200) VALUE SPACE.
           03  WK-NUM          PIC  9(003) VALUE ZERO.

           03  WK-YOUTUBE-TITLE PIC  X(1024) VALUE SPACE.
      *    *** USER名（作成者）
           03  WK-TEXT2        PIC  X(1024) VALUE SPACE.
      *    *** タイトル名
           03  WK-TITLE        PIC  X(1024) VALUE SPACE.
      *    *** CHANNEL,USER アドレス
           03  WK-HTTPS        PIC  X(1024) VALUE SPACE.
      *    *** WATCH アドレス,検索 アドレス
           03  WK-WATCH        PIC  X(1024) VALUE SPACE.
      *    *** 画像 アドレス
           03  WK-IMG          PIC  X(1024) VALUE SPACE.
      *    *** 通常　タイトル・作成者等有り、再生時間　時分秒
      *    *** ／PLAYLISTの時、タイトル１，２、
           03  WK-LABEL        PIC  X(1024) VALUE SPACE.
      *    *** 再生時間
           03  WK-LABEL2       PIC  X(1024) VALUE SPACE.
      *    *** PLAYLIST アドレス
           03  WK-PLAYLIST     PIC  X(1024) VALUE SPACE.
      *    *** VIDEO 本数
           03  WK-VIDEOCOUNT   PIC  X(1024) VALUE SPACE.
      *    *** 通常は1.何か月前、2.視聴回数
      *    *** ／1.PLAYLISTの時、2.タイトル１，２、
      *    *** 何か月前
           03  WK-SIMPLETEXT   PIC  X(1024) VALUE SPACE.
      *    *** 視聴回数
           03  WK-SIMPLETEXT2  PIC  X(1024) VALUE SPACE.
      *    *** 再生リストの全体を見る等
           03  WK-TEXT3        PIC  X(1024) VALUE SPACE.

      *    *** 秒数
           03  WK-BYOU         PIC  9(006) VALUE ZERO.
           03  WK-HH-X.
             05  WK-HH         PIC  9(002) VALUE ZERO.
           03  WK-MM-X.
             05  WK-MM         PIC  9(002) VALUE ZERO.
           03  WK-SS-X.
             05  WK-SS         PIC  9(002) VALUE ZERO.

           03  WK-TEXT2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-LABEL-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-LABEL2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-PLAYLIST-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-VIDEOCOUNT-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIMPLETEXT-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIMPLETEXT2-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT3-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-SHICHO-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-MAN-CNT      BINARY-LONG SYNC VALUE ZERO.
           03  WK-OKU-CNT      BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=SU (SJIS=>UTF8)
           03  WK-HENKAN       PIC  X(002) VALUE "SU".
           03  WK-SAISEILIST.
      *    *** 再生リストの全体を見る
             05  PIC  X(010) VALUE   X"E5868DE7949FE383AAE3".
             05  PIC  X(023) VALUE
             X"82B9E38388E381AEE585A8E4BD93E38292E8A68BE3828B".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-TITLE
                               INDEXED BY TBL01-IDX.
             05  TBL01-TITLE   PIC  X(100) VALUE HIGH-VALUE.
      *    *** PIN2 twiiter,instagram
             05  TBL01-SITE    OCCURS 5
                               PIC  X(200) VALUE SPACE.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".
           03  SW-PLAYLIST     PIC  X(001) VALUE "N".
           03  SW-HONNO-DOUGA  PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN 1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** PIN1 WK-TEXT2 (10:1) がスペースなら、.bat で入力した
      *    *** ファイル名をTITELに使う
           IF      WK-TEXT2 (10:1) =   SPACE
      *    *** コード変換　SJIS=>UTF8
                   PERFORM S040-10     THRU    S040-EX
           END-IF
           MOVE    WK-TEXT2 (2:) TO    WK-YOUTUBE-TITLE

      *    *** OPEN 2
           PERFORM S012-10     THRU    S012-EX

      *    *** WRITE POT1 (HTML 前データ出力)
           PERFORM S050-10     THRU    S050-EX



      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 TBL SET
                   PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** TBL01 SORT
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-TITLE



      *    *** #NN link 出力
           PERFORM UNTIL WK-PIN1-EOF =  HIGH-VALUE
                   EVALUATE PIN1-REC (1:1)
                       WHEN "#"
      *    *** #NN レコード編集3
      *    *** TWICE TEXT : #TWICE3 対応
                           IF    PIN1-REC (2:2) IS NUMERIC
                               PERFORM S130-10     THRU    S130-EX
                           END-IF
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,OPEN PIN1
           PERFORM S060-10     THRU    S060-EX



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   EVALUATE PIN1-REC (1:1)
                       WHEN "%"
                           CONTINUE
                       WHEN "#"
      *    *** TWICE TEXT : #TWICE3 対応
                           IF    PIN1-REC (2:2) IS NUMERIC
                               IF      PIN1-REC (1:3) =     "#01"
      *    *** #NN レコード編集1
                                   PERFORM S110-10     THRU    S110-EX
                               ELSE
      *    *** #NN レコード編集2
                                   PERFORM S120-10     THRU    S120-EX
                               END-IF
                           ELSE

                               IF      WK-SIMPLETEXT2 (2:1) = ":"
                                    OR WK-SIMPLETEXT2 (3:1) = ":"
      *    *** チャプター秒数変更
                                   PERFORM S101-10     THRU    S101-EX
                               END-IF
      *    *** <td> データ出力
                               PERFORM S100-10     THRU    S100-EX
                           END-IF
                       WHEN " "
                           CONTINUE
                       WHEN OTHER

                           IF      WK-SIMPLETEXT2 (2:1) = ":"
                                OR WK-SIMPLETEXT2 (3:1) = ":"
      *    *** チャプター秒数変更
                               PERFORM S101-10     THRU    S101-EX
                           END-IF
      *    *** <td> データ出力
                           PERFORM S100-10     THRU    S100-EX
                   END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   IF      WK-TITLE    =
      *    *** チャンネルに移動 2件続けて、無いと思われるので、
      *    *** １件読み飛ばしする
      *    *** チャンネルに移動
                   X"E38381E383A3E383B3E3838DE383ABE381ABE7A7BBE58B95"
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   ELSE
                           CONTINUE
                   END-IF

      *    *** チャプターの時、その他、折りたたむは１つ前のチャプターと同じ
      *    *** 秒数開始なので、
      *    *** １件読み飛ばしする
      *    *** その他
                   IF      WK-TITLE  (1:9)   = X"E3819DE381AEE4BB96"
      *    *** 折りたたむ
                       AND WK-LABEL2 (1:15)  = 
                                   X"E68A98E3828AE3819FE3819FE38280"
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

      *    *** WRITE POT1 (HTML 後データ出力)
           PERFORM S070-10     THRU    S070-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN 1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           ACCEPT  WK-ARGUMENT-NUMBER FROM      ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-POT1-F-NAME2 FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-POT1-F-NAME2

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " POT1-F 1個まで指定可"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

      *    *** サブルーチンでFILEDUMP 使用時は,FILEDUMP のOPEN先にCALLする
           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           SET     TBL01-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN 2
       S012-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 200
                   OR WK-POT1-F-NAME2 (I:1) = SPACE
               MOVE    WK-POT1-F-NAME2 (I:1) TO WK-POT1-F-NAME21 (I:1)
           END-PERFORM

           IF      I + 12      >       200
                   DISPLAY WK-PGM-NAME " POT1-F FILE NAME OVER I="
                           I
                   STOP    RUN
           END-IF
           MOVE    "youtube.html" TO  WK-POT1-F-NAME21 (I:12)

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   DISPLAY WK-PGM-NAME " POT1-F-NAME=" WK-POT1-F-NAME
                   STOP    RUN
           END-IF
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-TEXT2
                                       WK-IMG
                                       WK-HTTPS
                                       WK-TITLE
                                       WK-WATCH
                                       WK-LABEL
                                       WK-LABEL2
                                       WK-PLAYLIST
                                       WK-VIDEOCOUNT
                                       WK-SIMPLETEXT
                                       WK-SIMPLETEXT2

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT  AT  END
                   IF      SW-FIRST    =       "Y"
                       CONTINUE
                   ELSE
                       ADD     1           TO      WK-PIN1-CNT
                   END-IF
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-TEXT2      COUNT WK-TEXT2-LEN
                           WK-IMG        COUNT WK-IMG-LEN
                           WK-HTTPS      COUNT WK-HTTPS-LEN
                           WK-TITLE      COUNT WK-TITLE-LEN
                           WK-WATCH      COUNT WK-WATCH-LEN
                           WK-LABEL      COUNT WK-LABEL-LEN
                           WK-LABEL2     COUNT WK-LABEL2-LEN
                           WK-PLAYLIST   COUNT WK-PLAYLIST-LEN
                           WK-VIDEOCOUNT COUNT WK-VIDEOCOUNT-LEN
                           WK-SIMPLETEXT COUNT WK-SIMPLETEXT-LEN
                           WK-SIMPLETEXT2 COUNT WK-SIMPLETEXT2-LEN
                           WK-TEXT3      COUNT WK-TEXT3-LEN

                   MOVE    ZERO        TO      WK-MAN-CNT
                   MOVE    ZERO        TO      WK-OKU-CNT
                   INSPECT WK-SIMPLETEXT (1:WK-SIMPLETEXT-LEN) TALLYING
      *    *** 万
                           WK-MAN-CNT FOR ALL X"E4B887"
      *    *** 億
                           WK-OKU-CNT FOR ALL X"E58484"
                   IF      WK-MAN-CNT  =       ZERO
                       AND WK-OKU-CNT  =       ZERO
                       MOVE    ZERO        TO      WK-SHICHO-CNT
                       INSPECT WK-SIMPLETEXT (1:WK-SIMPLETEXT-LEN)
                           TALLYING
      *    *** 回視聴
                           WK-SHICHO-CNT FOR ALL X"E59B9EE8A696E881B4"
                       IF      WK-SHICHO-CNT NOT = ZERO
      *    *** .=>,
                           INSPECT WK-SIMPLETEXT (1:WK-SIMPLETEXT-LEN)
                                   REPLACING ALL "." BY ","
                       END-IF
                   END-IF

                   MOVE    ZERO        TO      WK-MAN-CNT
                   MOVE    ZERO        TO      WK-OKU-CNT
                   INSPECT WK-SIMPLETEXT2 (1:WK-SIMPLETEXT2-LEN) 
                           TALLYING
      *    *** 万
                           WK-MAN-CNT FOR ALL X"E4B887"
      *    *** 億
                           WK-OKU-CNT FOR ALL X"E58484"
                   IF      WK-MAN-CNT  =       ZERO
                       AND WK-OKU-CNT  =       ZERO
                       MOVE    ZERO        TO      WK-SHICHO-CNT
                       INSPECT WK-SIMPLETEXT2(1:WK-SIMPLETEXT2-LEN)
                          TALLYING
      *    *** 回視聴
                           WK-SHICHO-CNT FOR ALL X"E59B9EE8A696E881B4"
                       IF      WK-SHICHO-CNT NOT = ZERO
      *    *** .=>,
                           INSPECT WK-SIMPLETEXT2 (1:WK-SIMPLETEXT2-LEN)
                                   REPLACING ALL "." BY ","
                       END-IF
                   END-IF

      *    *** 確認済み
                   IF     WK-LABEL2 (1:12) = X"E7A2BAE8AA8DE6B888E381BF"
                           MOVE    SPACE       TO     WK-LABEL2
                           MOVE    ZERO        TO     WK-LABEL2-LEN
                   END-IF
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-TITLE2
                                       WK-SITE1
                                       WK-SITE2
                                       WK-SITE3
                                       WK-SITE4
                                       WK-SITE5

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN2-CNT
      *    *** 256バイトまでしか入らない
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE2
                           WK-SITE1
                           WK-SITE2
                           WK-SITE3
                           WK-SITE4
                           WK-SITE5
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           IF      TBL01-IDX   >       1000
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2   TO      TBL01-TITLE (TBL01-IDX)
           MOVE    WK-SITE1    TO      TBL01-SITE  (TBL01-IDX 1)
           MOVE    WK-SITE2    TO      TBL01-SITE  (TBL01-IDX 2)
           MOVE    WK-SITE3    TO      TBL01-SITE  (TBL01-IDX 3)
           MOVE    WK-SITE4    TO      TBL01-SITE  (TBL01-IDX 4)
           MOVE    WK-SITE5    TO      TBL01-SITE  (TBL01-IDX 5)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** コード変換 SJIS=>UTF8
       S040-10.

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    200         TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-POT1-F-NAME2
                                       WK-POT1-F-NAME2U

      *    *** ,無いので1024入っているため、リセット
      *    *** 9 はYouTube の長さ
           MOVE    9           TO      WK-TEXT2-LEN
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 200
      *                OR WK-POT1-F-NAME2U (I:3) = X"E38080"
                      OR WK-POT1-F-NAME2U (I:3) = SPACE
                   MOVE    WK-POT1-F-NAME2U (I:1) TO  
                           WK-TEXT2 (I + 9:1)
                   ADD     1       TO    WK-TEXT2-LEN
           END-PERFORM
           .
       S040-EX.
           EXIT.

      *    *** WRITE POT1 (HTML 前データ出力)
       S050-10.

           IF      WK-IMG (1:8) =      "PLAYLIST"
                   MOVE    "Y"         TO      SW-PLAYLIST
           END-IF

           MOVE    "<DOCTYPE html>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<html>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<head>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<meta charset="utf-8">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<title>"   TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-YOUTUBE-TITLE TO POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</title>"  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE
           '<link rel="stylesheet" type="text/css" href="mystyle.css">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</head>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<body>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<h1>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-YOUTUBE-TITLE TO POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</h1>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<br>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<img src="' TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** YouTube XXXXXXX SET
      *     MOVE    WK-TEXT2 (10:WK-TEXT2-LEN - 9) 
      *                         TO      POT1-REC (1:WK-TEXT2-LEN - 9)
      *     MOVE    ".jpg"      TO      POT1-REC (WK-TEXT2-LEN - 8:4)
           MOVE    WK-HTTPS    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '" alt=""'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *     MOVE    '" style="position:relative; left:400px;'
           MOVE    '" style="float:right; '
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ' width:500; height:auto; " ><br>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S050-EX.
           EXIT.

      *    *** CLOSE,OPEN PIN1
       S060-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           MOVE    LOW-VALUE   TO      WK-PIN1-EOF
           MOVE    "N"         TO      SW-FIRST
           .
       S060-EX.
           EXIT.

      *    *** WRITE POT1 (HTML 後データ出力)
       S070-10.

           MOVE    '</tr></table><a href="#top">TOP</a></body></html>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S070-EX.
           EXIT.

      *    *** <td> データ出力
       S100-10.

           ADD     1           TO      I
           IF      I           >       6
                   MOVE    1           TO      I
                   MOVE    "</tr>"     TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "<tr>"      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    "<td>"      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<p class="welcome2">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      WK-PLAYLIST (1:1) = SPACE
                   MOVE    WK-WATCH    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   MOVE    WK-PLAYLIST TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    '">'        TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** IMG CUT
           MOVE    '<img src="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-IMG      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '" alt=""'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ' class="welcome2"><br><br>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-TITLE    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a>"      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 本の動画
           IF      WK-TEXT2 (WK-TEXT2-LEN - 12:12) =
                   X"E69CACE381AEE58B95E794BB"
                   MOVE    "Y"         TO      SW-HONNO-DOUGA
           END-IF

      *    *** ビデオ本数同じ時、VIDEOCOUNT をクリアー
           IF      WK-VIDEOCOUNT (1:WK-VIDEOCOUNT-LEN - 1)
                 = WK-TEXT2 (1:WK-VIDEOCOUNT-LEN - 1)
                   MOVE    SPACE       TO      WK-VIDEOCOUNT
           END-IF

      *    *** VIDEO 本数
           IF      WK-VIDEOCOUNT (1:1) = SPACE
                   CONTINUE
           ELSE
                   MOVE    '<br><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** TEST69 で 本の動画、本目 追加した
                   MOVE    WK-VIDEOCOUNT (1:WK-VIDEOCOUNT-LEN) TO
                           POT1-REC (1:)
      *             IF      SW-HONNO-DOUGA =    "Y"
      *    *** 本の動画
      *                     MOVE    X"E69CACE381AEE58B95E794BB" TO
      *                             POT1-REC (1 + WK-VIDEOCOUNT-LEN:12)
      *             ELSE
      *                 IF      SW-PLAYLIST =       "Y"
      *    *** 本目
      *                     MOVE    X"E69CACE79BAE" TO
      *                             POT1-REC (1 + WK-VIDEOCOUNT-LEN:6)
      *                 ELSE
      *    *** 本
      *                     MOVE    X"E69CAC"   TO
      *                             POT1-REC (1 + WK-VIDEOCOUNT-LEN:3)
      *                 END-IF
      *             END-IF
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           IF      WK-PLAYLIST (1:1) = SPACE
      *    *** PLAYLIST でない時、

      *    *** 何か月前／タイトル１
                   IF      WK-SIMPLETEXT (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-SIMPLETEXT (1:WK-SIMPLETEXT-LEN)
                                   TO POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** PLAYLIST でない時、再生時間のみ、出力
      *    *** 再生時間　1
                   IF      WK-LABEL (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** 視聴回数／タイトル２
                   IF      WK-SIMPLETEXT2 (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-SIMPLETEXT2 (1:WK-SIMPLETEXT2-LEN)
                                   TO POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** 再生時間　２
                   IF      WK-LABEL2 (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL2   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           ELSE
      *    *** PLAYLIST の時、

      *    *** タイトル?
                   IF      WK-TEXT3 (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-TEXT3 (1:WK-TEXT3-LEN)
                                   TO POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** タイトル１
                   IF      WK-SIMPLETEXT (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-SIMPLETEXT (1:WK-SIMPLETEXT-LEN)
                                   TO POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** 再生時間１
                   IF      WK-LABEL (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** タイトル２
                   IF      WK-SIMPLETEXT2 (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-SIMPLETEXT2 (1:WK-SIMPLETEXT2-LEN)
                                   TO POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** 再生時間２
                   IF      WK-LABEL2 (1:1) = SPACE
                           CONTINUE
                   ELSE
                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL2   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-IF

           IF      WK-TEXT2 (1:1) = SPACE
                   IF      WK-WATCH (1:45) =
                   "https://www.youtube.com/results?search_query="
                       MOVE    '<br><br>'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
      *    *** 検索
                       MOVE    X"E6A49CE7B4A2" TO  POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   ELSE
                       CONTINUE
                   END-IF
           ELSE
      *    *** https://www.youtube.com のみの時、
               IF      WK-HTTPS (1:24) =   "https://www.youtube.com "

      *    *** 再生リストの全体を見る
                  IF      WK-TITLE (1:33) =   WK-SAISEILIST
                       MOVE    '<br><br><a href="'
                                       TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-HTTPS    TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    '">'        TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-TEXT2    TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    "</a>"      TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   ELSE
                       CONTINUE
                   END-IF
               ELSE
                   IF      WK-WATCH (1:45) =
                       "https://www.youtube.com/results?search_query="
                       MOVE    '<br><br>'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
      *    *** 検索
                       MOVE    X"E6A49CE7B4A2" TO  POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   ELSE
                       CONTINUE
                   END-IF

                   MOVE    '<br><br><a href="'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-HTTPS    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '">'        TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TEXT2    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "</a>"      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               END-IF
           END-IF

           MOVE    "</p></td>" TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** チャプター秒数変更
       S101-10.

      *    *** WK-SIMPLETEXT2-LEN は+2　の長さが入っている

           MOVE    ZERO        TO      WK-BYOU
           EVALUATE TRUE
      *    *** M:SS SSは２桁と思われる
               WHEN WK-SIMPLETEXT2-LEN = 6
                   MOVE    WK-SIMPLETEXT2 (3:2) TO WK-SS-X (1:2)
                   IF      WK-SIMPLETEXT2 (2:1) = ":"
                           MOVE    ZERO        TO      WK-MM
                           MOVE    WK-SIMPLETEXT2 (1:1) TO WK-MM-X (2:1)
                           COMPUTE WK-BYOU = WK-MM * 60 + WK-SS
                   END-IF
      *    *** MM:SS MM,SSは２桁と思われる
               WHEN WK-SIMPLETEXT2-LEN = 7
                   MOVE    WK-SIMPLETEXT2 (4:2) TO WK-SS-X (1:2)
                   IF      WK-SIMPLETEXT2 (3:1) = ":"
                           MOVE    WK-SIMPLETEXT2 (1:2) TO WK-MM-X (1:2)
                           COMPUTE WK-BYOU = WK-MM * 60 + WK-SS
                   END-IF
      *    *** H:MM:SS MM,SSは２桁と思われる
               WHEN WK-SIMPLETEXT2-LEN = 9
                   MOVE    WK-SIMPLETEXT2 (6:2) TO WK-SS-X (1:2)
                   IF      WK-SIMPLETEXT2 (2:1) = ":"
                           MOVE    ZERO        TO      WK-HH
                           MOVE    WK-SIMPLETEXT2 (1:1) TO WK-HH-X (2:1)
                           MOVE    WK-SIMPLETEXT2 (3:2) TO WK-MM-X (1:2)
                           COMPUTE WK-BYOU = WK-HH * 3600
                                           + WK-MM * 60 + WK-SS
                   END-IF
      *    *** HH:MM:SS HH,MM,SSは２桁と思われる
               WHEN WK-SIMPLETEXT2-LEN = 10
                   MOVE    WK-SIMPLETEXT2 (7:2) TO WK-SS-X (1:2)
                   IF      WK-SIMPLETEXT2 (3:1) = ":"
                           MOVE    WK-SIMPLETEXT2 (1:2) TO WK-HH-X (1:2)
                           MOVE    WK-SIMPLETEXT2 (4:2) TO WK-MM-X (1:2)
                           COMPUTE WK-BYOU = WK-HH * 3600
                                           + WK-MM * 60 + WK-SS
                   END-IF
           END-EVALUATE

      *    *** WK-WATCH-LEN も後ろにスペースがあるため、調整した位置で比較
           IF      WK-WATCH (WK-WATCH-LEN - 1: 1) = "s"
               AND WK-WATCH (WK-WATCH-LEN - 2: 1) IS NUMERIC
                   EVALUATE TRUE
                       WHEN WK-WATCH (WK-WATCH-LEN - 3: 1) = "="
                        AND WK-WATCH (WK-WATCH-LEN - 5: 2) = "&t"
                           MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN - 2: 6)
                           MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 4: 1)
                           ADD      5          TO      WK-WATCH-LEN
                       WHEN WK-WATCH (WK-WATCH-LEN - 4: 1) = "="
                        AND WK-WATCH (WK-WATCH-LEN - 6: 2) = "&t"
                           MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN - 3: 6)
                           MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 3: 1)
                           ADD      4          TO      WK-WATCH-LEN
                       WHEN WK-WATCH (WK-WATCH-LEN - 5: 1) = "="
                        AND WK-WATCH (WK-WATCH-LEN - 7: 2) = "&t"
                           MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN - 4: 6)
                           MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 2: 1)
                           ADD      3          TO      WK-WATCH-LEN
                       WHEN WK-WATCH (WK-WATCH-LEN - 6: 1) = "="
                        AND WK-WATCH (WK-WATCH-LEN - 8: 2) = "&t"
                           MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN - 5: 6)
                           MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 1: 1)
                           ADD      2          TO      WK-WATCH-LEN
                       WHEN WK-WATCH (WK-WATCH-LEN - 7: 1) = "="
                        AND WK-WATCH (WK-WATCH-LEN - 9: 2) = "&t"
                           MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN - 6: 6)
                           MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 0: 1)
                           ADD      1          TO      WK-WATCH-LEN
                   END-EVALUATE
           ELSE
                   MOVE     "&t="        TO
                                       WK-WATCH (WK-WATCH-LEN + 0: 3) 
                   MOVE     WK-BYOU    TO
                                       WK-WATCH (WK-WATCH-LEN + 3: 6)
                   MOVE     "s"        TO
                                       WK-WATCH (WK-WATCH-LEN + 9: 1)
                   ADD      10         TO      WK-WATCH-LEN
           END-IF
           .
       S101-EX.
           EXIT.

      *    *** #NN レコード編集1
       S110-10.

           MOVE    '<h2><br><a name="  ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:2) TO   POT1-REC (18:2)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (5:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a></h2><table><tr>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I
           .
       S110-EX.
           EXIT.

      *    *** #NN レコード編集2
       S120-10.

           MOVE    '</tr></table><a href="#top">TOP</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** <br> １つだと、自動的に連番が段落に入る 入らない時もある
      *    *** <br> ２つだと、自動的に連番が段落に入らない
           MOVE    '<h2><br><a name="  ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:2) TO   POT1-REC (18:2)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (5:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a></h2><table><tr>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I

           .
       S120-EX.
           EXIT.

      *    *** #NN レコード編集3
       S130-10.

           IF      PIN1-REC (1:3) =    "#01"
                   MOVE '<a href="https://www.instagram.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** instagram.com/home
                   MOVE    "instagram"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="https://twitter.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** twitter.com/
                   MOVE    "twitter"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE '<a href="https://www.animatetimes.com/seiyu/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 声優ニュース
                   MOVE    X"E5A3B0E584AAE3838BE383A5E383BCE382B9"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="index.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    
              '<img src="image/icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
      *    *** 声優
                   MOVE    X'E5A3B0E584AA'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="indexanime.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    
              '<img src="image/icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
      *    *** アニメ年代順
                   MOVE    X'E382A2E3838BE383A1E5B9B4E4BBA3E9A086'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="indexanimesort.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    
              '<img src="image/icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** アニメタイトル順
                   MOVE    
                   X'E382A2E3838BE383A1E382BFE382A4E38388E383ABE9A086'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

           END-IF

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NN
           MOVE    PIN1-REC (1:3) TO   POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '">'    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NN ＸＸＸＸＸ => NN ＸＸＸＸＸ
           MOVE    PIN1-REC (5:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '</a><br>'  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
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
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
