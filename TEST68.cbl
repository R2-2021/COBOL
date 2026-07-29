      *    *** YouTube html ゆいかおり、石原夏織　Ｗatch　Ｌist 作成
      *    *** 
      *    *** チャプターの時、表題と表示時間ずれているが、
      *    *** TEST69で直すの難しい為、TEST69で出力されるWATCHの秒数変更する
      *    *** 
      *    *** JOB TEST69 youtube.aiscream.html
      *    ***        |
      *    ***     TEST142
      *    ***        |
      *    ***     TEST68 aiscream

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST68.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** WATCH DATA　ＵＴＦ８
      *    *** TEST10 => TEST66 => TEST69,TEST68 にした
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** playlist タイトル不明セット
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST69.POT2 CHANNEL 再セット
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.


      *    *** HTML データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(10000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(500).

       FD  PIN3-F
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST68  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
      *         "TEST28_201110_202007.csv".
      *         "TEST69.POT3".
               "TEST142.POT1".

           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST68.PIN2".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST68.POT1".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST69.POT2".

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

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
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

           03  WK-YOUTUBE-TITLE PIC X(1000) VALUE SPACE.
      *    *** USER名（作成者）
           03  WK-TEXT2        PIC  X(1000) VALUE SPACE.
      *    *** タイトル名
           03  WK-TITLE        PIC  X(1000) VALUE SPACE.
      *    *** CHANNEL,USER アドレス
           03  WK-HTTPS        PIC  X(1000) VALUE SPACE.
      *    *** WATCH アドレス,検索 アドレス
           03  WK-WATCH        PIC  X(1000) VALUE SPACE.
           03  WK-WATCH2       PIC  X(1000) VALUE SPACE.
      *    *** 画像 アドレス
           03  WK-IMG          PIC  X(1000) VALUE SPACE.
      *    *** 通常　タイトル・作成者等有り、再生時間　時分秒
      *    *** ／PLAYLISTの時、タイトル１，２、
           03  WK-LABEL        PIC  X(1000) VALUE SPACE.
      *    *** 再生時間
           03  WK-LABEL2       PIC  X(1000) VALUE SPACE.
      *    *** PLAYLIST アドレス
           03  WK-PLAYLIST     PIC  X(1000) VALUE SPACE.
      *    *** VIDEO 本数
           03  WK-VIDEOCOUNT   PIC  X(1000) VALUE SPACE.
      *    *** 通常は1.何か月前、2.視聴回数
      *    *** ／1.PLAYLISTの時、2.タイトル１，２、
      *    *** 何か月前
           03  WK-SIMPLETEXT   PIC  X(1000) VALUE SPACE.
      *    *** 視聴回数
           03  WK-SIMPLETEXT2  PIC  X(1000) VALUE SPACE.
      *    *** 再生リストの全体を見る等
           03  WK-TEXT3        PIC  X(1000) VALUE SPACE.
      *    *** 予備１
           03  WK-YOBI1        PIC  X(1000) VALUE SPACE.
      *    *** 予備２
           03  WK-YOBI2        PIC  X(1000) VALUE SPACE.
      *    *** 予備３
           03  WK-YOBI3        PIC  X(1000) VALUE SPACE.

      *    *** 秒数
           03  WK-BYOU         PIC  9(006) VALUE ZERO.
           03  WK-HH-X.
             05  WK-HH         PIC  9(002) VALUE ZERO.
           03  WK-MM-X.
             05  WK-MM         PIC  9(002) VALUE ZERO.
           03  WK-SS-X.
             05  WK-SS         PIC  9(002) VALUE ZERO.

      *    *** この動画のチャプター数:
           03  WK-KONODOUGA..
             05    PIC  X(017) VALUE
               X"E38193E381AEE58B95E794BBE381AEE383".
             05    PIC  X(017) VALUE
               X"81E383A3E38397E382BFE383BCE695B03A".

      *    *** コラボレーション チャンネル
           03  WK-KORABO.
             05                PIC  X(020) VALUE
                 X"E382B3E383A9E3839CE383ACE383BCE382B7E383".
             05                PIC  X(020) VALUE
                 X"A7E383B320E38381E383A3E383B3E3838DE383AB".

           03  WK-VIDEOID      PIC X(050) VALUE SPACE.
           03  WK-VIDEOID2     PIC X(050) VALUE SPACE.
           03  WK-CHANNEL      PIC X(100) VALUE SPACE.
           03  WK-TEXT         PIC X(1000) VALUE SPACE.
           03  WK-TEXTX        PIC X(1000) VALUE SPACE.
           03  WK-ITEM1        PIC X(1000) VALUE SPACE.
           03  WK-ITEM2        PIC X(1000) VALUE SPACE.

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
           03  WK-VIDEOID-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-VIDEOID2-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-YOBI1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-YOBI2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-YOBI3-LEN    BINARY-LONG SYNC VALUE ZERO.

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
           03  WK-SITE1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-SITE1-LEN-MAX BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 300
                               ASCENDING KEY IS TBL01-SITE
                               INDEXED BY TBL01-IDX.
             05  TBL01-SITE    PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL01-TITLE   PIC  X(1000) VALUE HIGH-VALUE.

       01  TBL-AREA2.
           03  TBL02-AREA      OCCURS 2000
                               INDEXED BY TBL02-IDX
             05  TBL02-VIDEOID PIC  X(050) VALUE SPACE.
             05  TBL02-CHANNEL PIC  X(100) VALUE SPACE.
             05  TBL02-TEXT    PIC  X(1000) VALUE SPACE.

             05  TBL02-SEQ     BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-VIDEOID-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-CHANNEL-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-TEXT-LEN    BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".
           03  SW-PLAYLIST     PIC  X(001) VALUE "N".
           03  SW-HONNO-DOUGA  PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-DEBUG        PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-WATCH        PIC  X(1000) VALUE SPACE.

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
                   ASCENDING KEY TBL01-SITE



      *    *** READ PIN3
           PERFORM S080-10     THRU    S080-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** PIN3 TBL SET
                   PERFORM S082-10     THRU    S082-EX
      *    *** READ PIN3
                   PERFORM S080-10     THRU    S080-EX
           END-PERFORM

           IF      SW-DEBUG    =       "Y"
               PERFORM VARYING I2 FROM 1 BY 1
                       UNTIL I2 > I2-MAX
           MOVE    I2          TO      TBL02-IDX
      *     SET     TBL02-IDX   TO      I2
           DISPLAY TBL02-IDX
                   IF TBL02-VIDEOID (TBL02-IDX) = SPACE
               OR TBL02-CHANNEL (TBL02-IDX) = SPACE
               OR TBL02-TEXT    (TBL02-IDX) = SPACE

           MOVE    "P"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    TBL02-SEQ(I2) TO    WFD-SEQ
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL02-VIDEOID (TBL02-IDX)

           MOVE    "P"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    TBL02-SEQ(I2) TO    WFD-SEQ
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL02-CHANNEL (TBL02-IDX)

           MOVE    "P"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    TBL02-SEQ(I2) TO    WFD-SEQ
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL02-TEXT    (TBL02-IDX) (1:100)
                   END-IF
               END-PERFORM
           END-IF



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

           MOVE    SPACE       TO      WK-WATCH2
           UNSTRING WK-WATCH
                    DELIMITED BY "&pp" OR "&list" OR SPACE
                    INTO
                    WK-WATCH2

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   EVALUATE TRUE
                       WHEN PIN1-REC (1:1) = "%"
                           CONTINUE
                       WHEN PIN1-REC (1:1) = "#"
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
      *                 WHEN PIN1-REC (1:1) = " "
      *                     CONTINUE
                       WHEN OTHER
                           IF      WK-SIMPLETEXT2 (2:1) = ":"
                                OR WK-SIMPLETEXT2 (3:1) = ":"
      *    *** チャプター秒数変更
                               PERFORM S101-10     THRU    S101-EX
                           END-IF
      *                     IF      WK-WATCH2 NOT = SV-WATCH
                           IF      WK-WATCH2 (1:1) NOT = SPACE
                                OR WK-PLAYLIST (1:32) =
                                   "https://www.youtube.com/playlist"
      *                         AND SV-WATCH NOT = SPACE
      *    *** <td> データ出力
                               PERFORM S100-10     THRU    S100-EX
                           END-IF
                   END-EVALUATE

                   MOVE    SPACE       TO      SV-WATCH
                   UNSTRING WK-WATCH
                            DELIMITED BY "&pp" OR "&list" OR SPACE
                            INTO
                            SV-WATCH

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

                   MOVE    SPACE       TO      WK-WATCH2
                   UNSTRING WK-WATCH
                            DELIMITED BY "&pp" OR "&list" OR SPACE
                            INTO
                            WK-WATCH2
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
                               PIN2-F
                               PIN3-F

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
           SET     TBL02-IDX   TO      1

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
                                       WK-TEXT3
                                       WK-YOBI1
                                       WK-YOBI2
                                       WK-YOBI3

           MOVE    ZERO        TO      WK-TEXT2-LEN
                                       WK-IMG-LEN
                                       WK-HTTPS-LEN
                                       WK-TITLE-LEN
                                       WK-WATCH-LEN
                                       WK-LABEL-LEN
                                       WK-LABEL2-LEN
                                       WK-PLAYLIST-LEN
                                       WK-VIDEOCOUNT-LEN
                                       WK-SIMPLETEXT-LEN
                                       WK-SIMPLETEXT2-LEN
                                       WK-TEXT3-LEN
                                       WK-YOBI1-LEN
                                       WK-YOBI2-LEN
                                       WK-YOBI3-LEN

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
                           WK-YOBI1      COUNT WK-YOBI1-LEN
                           WK-YOBI2      COUNT WK-YOBI2-LEN
                           WK-YOBI3      COUNT WK-YOBI3-LEN

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
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-SITE1
                                       WK-TITLE2
                                       
           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN2-CNT
      *    *** 256バイトまでしか入らない
                   UNSTRING PIN2-REC DELIMITED BY ","
                           INTO
                           WK-SITE1 COUNT WK-SITE1-LEN
                           WK-TITLE2
           END-READ

           IF      WK-SITE1-LEN >      WK-SITE1-LEN-MAX
                   MOVE    WK-SITE1-LEN TO     WK-SITE1-LEN-MAX
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           IF      TBL01-IDX   >       300
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2   TO      TBL01-TITLE (TBL01-IDX)
           MOVE    WK-SITE1    TO      TBL01-SITE  (TBL01-IDX)
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
           OPEN    INPUT       PIN1-F

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

      *    *** READ PIN3
       S080-10.

           READ    PIN3-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT  AT  END
                   MOVE    " ,"        TO   PIN3-REC (WK-PIN3-LEN + 1:2)
                   ADD     2           TO      WK-PIN3-LEN
                   ADD     1           TO      WK-PIN3-CNT

      *    *** 下記時点で、READ 終了にする

      *    ***  label : 検索フィルタ
                   IF    ( PIN3-REC (1:9) = " label : "
                       AND PIN3-REC (9:18) =
                           X"E6A49CE7B4A2E38395E382A3E383ABE382BF" )
                     OR
      *    ***  text : 検索フィルタ
                         ( PIN3-REC (1:8) = " text : "
                       AND PIN3-REC (9:18) =
                           X"E6A49CE7B4A2E38395E382A3E383ABE382BF" )
                           MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
                   END-IF

                   IF      PIN3-REC (1:17) =   ", watchEndpoint :"

                       COMPUTE I3 = TBL02-IDX - 1
                       PERFORM VARYING I2 FROM I3 BY -1
                           UNTIL I2 < 1
      *                        OR TBL02-VIDEOID (I2) NOT = SPACE
                           IF      TBL02-VIDEOID (I2) = SPACE
                               AND WK-VIDEOID NOT = SPACE
                                   MOVE    WK-VIDEOID  TO
                                           TBL02-VIDEOID (I2)
                                   MOVE    WK-VIDEOID-LEN TO
                                           TBL02-VIDEOID-LEN (I2)
                           END-IF

                           MOVE    SPACE       TO      WK-VIDEOID
                                                       WK-VIDEOID2
                           MOVE    ZERO        TO      WK-VIDEOID-LEN
                                                       WK-VIDEOID2-LEN
                           MOVE    SPACE       TO      WK-CHANNEL
                           MOVE    ZERO        TO      WK-CHANNEL-LEN
                       END-PERFORM
                   END-IF

                   IF      PIN3-REC (1:8) =    " title :"
                           MOVE    SPACE       TO      WK-CHANNEL
                           MOVE    ZERO        TO      WK-CHANNEL-LEN
                   END-IF

                   UNSTRING PIN3-REC DELIMITED BY " videoId : " OR " ,"
                           INTO
                           WK-ITEM1   COUNT WK-ITEM1-LEN
                           WK-VIDEOID2 COUNT WK-VIDEOID2-LEN
                           WK-ITEM2   COUNT WK-ITEM2-LEN
                   END-UNSTRING

                   IF      WK-VIDEOID  =       SPACE
                       AND WK-VIDEOID2 NOT =   SPACE
                       AND WK-VIDEOID2 NOT =
                           " webPageType : WEB_PAGE_TYPE_CHANNEL"
                       AND WK-VIDEOID2 NOT =
                           " webPageType : WEB_PAGE_TYPE_UNKNOWN"
                       AND WK-VIDEOID2 (1:8) NOT = " width :"
                       AND WK-VIDEOID2 NOT = " styleRuns :["
                       AND WK-VIDEOID2 NOT = " onTap :"
                       AND WK-VIDEOID2 NOT =
                           " webPageType : WEB_PAGE_TYPE_WATCH"
                           MOVE    WK-VIDEOID2     TO WK-VIDEOID
                           MOVE    WK-VIDEOID2-LEN TO WK-VIDEOID-LEN
                   END-IF

                   IF      PIN3-REC (1:16) =   " url : /channel/"
                        OR PIN3-REC (1:09) =   " url : /@"
                       MOVE    SPACE       TO      WK-CHANNEL
                       MOVE    ZERO        TO      WK-CHANNEL-LEN
                       UNSTRING PIN3-REC DELIMITED BY " url : " OR " ,"
                           INTO
                           WK-ITEM1   COUNT WK-ITEM1-LEN
                           WK-CHANNEL COUNT WK-CHANNEL-LEN
                           WK-ITEM2   COUNT WK-ITEM2-LEN
                       END-UNSTRING
                   END-IF

                   IF      PIN3-REC (1:16) =   " url : /watch?v="
                       AND WK-VIDEOID = SPACE

                       MOVE    ZERO    TO      WK-VIDEOID-LEN
                       MOVE    1       TO      P
                       PERFORM VARYING I2 FROM 17 BY 1
                           UNTIL PIN3-REC (I2:1) = SPACE OR "&"

                               MOVE  PIN3-REC (I2:1) TO WK-VIDEOID (P:1)
                               ADD   1         TO       WK-VIDEOID-LEN
                                                        P
                       END-PERFORM
                   END-IF

                   COMPUTE I3 = TBL02-IDX - 1
                   PERFORM VARYING I2 FROM I3 BY -1
                           UNTIL I2 < 1
                              OR TBL02-CHANNEL (I2) NOT = SPACE
                           IF      TBL02-CHANNEL (I2) = SPACE
                                   MOVE    WK-CHANNEL  TO
                                           TBL02-CHANNEL (I2)
                                   MOVE    WK-CHANNEL-LEN TO
                                           TBL02-CHANNEL-LEN (I2)
                           END-IF
                   END-PERFORM

                   MOVE    SPACE       TO      WK-TEXT
                   MOVE    ZERO        TO      WK-TEXT-LEN
                   UNSTRING PIN3-REC DELIMITED BY " label : "
                         OR " text : "
                         OR " content : "
                         OR " ,"
                           INTO
                           WK-ITEM1   COUNT WK-ITEM1-LEN
                           WK-TEXT    COUNT WK-TEXT-LEN
                   END-UNSTRING

                   IF    ( WK-TEXT     =      
                           " webPageType : WEB_PAGE_TYPE_CHANNEL"
                        OR " webPageType : WEB_PAGE_TYPE_SHORTS"
                        OR " webPageType : WEB_PAGE_TYPE_SEARCH"
                        OR " webPageType : WEB_PAGE_TYPE_WATCH"
                        OR " webPageType : WEB_PAGE_TYPE_UNKNOWN"
                        OR " webPageType : WEB_PAGE_TYPE_BROWSE"
                        OR " style : BUTTON_VIEW_MODEL_STYLE_MONO"
                        OR " watchEndpointSupportedOnesieConfig :"
                        OR " navigationEndpoint :"
                        OR " onTap :" 
                        OR " thumbnail :"
                       

                          )
                      OR   WK-TEXT (1:16) =    " playerParams : "
                      OR   WK-TEXT (1:14) =    " playlistId : "
                      OR   WK-TEXT (1:10) =    " params : "
                      OR   WK-TEXT (1:01) =    ","
                      OR   WK-TEXT (1:08) =    " width :"
                      
      *    *** 秒
                      OR ( WK-TEXT (WK-TEXT-LEN - 2:3) =    X"E7A792"
                       AND WK-TEXT-LEN < 23)
                           MOVE    SPACE       TO      WK-TEXT
                           MOVE    ZERO        TO      WK-TEXT-LEN
                   END-IF
           END-READ
           .
       S080-EX.
           EXIT.

      *    *** PIN3 TBL SET
       S082-10.

           IF      TBL02-IDX   >       2000
                   DISPLAY WK-PGM-NAME
                           " TBL02-TBL OVER TBL02-IDX=" TBL02-IDX
                   STOP    RUN
           END-IF

           IF       ( PIN3-REC(1:08) = " text : "
      *    *** SPACE
                AND ( PIN3-REC(9:6) = SPACE
      *    *** 再生
                 OR PIN3-REC(9:6) = X"E5868DE7949F"
      *    *** 再生中
                 OR PIN3-REC(9:9) = X"E5868DE7949FE4B8AD"
      *    *** ループ再生
                 OR PIN3-REC(9:15) =
                    X"E383ABE383BCE38397E5868DE7949F"
      *    *** シャッフル再生
                 OR PIN3-REC(9:21) =
                    X"E382B7E383A3E38383E38395E383ABE5868DE7949F"
      *    *** フィルタ
                 OR PIN3-REC(9:12) = X"E38395E382A3E383ABE382BF"
      *    *** 検索オプション
                 OR PIN3-REC(9:21) =
                    X"E6A49CE7B4A2E382AAE38397E382B7E383A7E383B3"
      *    *** すべて表示
                 OR PIN3-REC(9:15) = X"E38199E381B9E381A6E8A1A8E7A4BA"
      *    *** すべて再生
                 OR PIN3-REC(9:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** フィードバックを送信
                 OR PIN3-REC(9:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** 共有
                 OR PIN3-REC(9:6) = X"E585B1E69C89"
      *    *** 動画
                 OR PIN3-REC(9:6) = X"E58B95E794BB"
      *    *** カーソルを合わせて再生
                 OR ( PIN3-REC(9:10) = 
                    X"E382ABE383BCE382BDE3"
                 AND  PIN3-REC(19:23) = 
                    X"83ABE38292E59088E3828FE3819BE381A6E5868DE7949F" )
      *    *** 関連する検索から
                 OR PIN3-REC(9:24) = 
                    X"E996A2E980A3E38199E3828BE6A49CE7B4A2E3818BE38289"
      *    *** この動画には
                 OR PIN3-REC(9:18) = 
                    X"E38193E381AEE58B95E794BBE381ABE381AF"
      *    *** おすすめ動画
                 OR PIN3-REC(9:18) = 
                    X"E3818AE38199E38199E38281E58B95E794BB"
      *    *** ショート
                  OR PIN3-REC(9:12) = X"E382B7E383A7E383BCE38388"
      *    *** 配信
                  OR PIN3-REC(9:6) = X"E9858DE4BFA1"
      *    *** 作成した再生リスト
                 OR PIN3-REC(9:27) =
               X"E4BD9CE68890E38197E3819FE5868DE7949FE383AAE382B9E38388"
      *    *** 作成した再生リスト
                 OR PIN3-REC(9:21) =
                    X"E996A2E980A3E38381E383A3E383B3E3838DE383AB"
      *    *** チャンネル登録
                 OR PIN3-REC(9:21) =
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
      *    *** ミュージック ビデオ
      *           OR PIN3-REC(9:28) =
      *       X"E3839FE383A5E383BCE382B8E38383E382AF20E38393E38387E382AA"
      *    *** ログイン
                 OR PIN3-REC(9:12) = X"E383ADE382B0E382A4E383B3"
      *    *** キャンセル
                 OR PIN3-REC(9:15) = X"E382ADE383A3E383B3E382BBE383AB"
      *    *** キーボード ショートカット
                 OR ( PIN3-REC(9:19) = 
                    X"E382ADE383BCE3839CE383BCE3838920E382B7"
                 AND  PIN3-REC(9 + 19:18) = 
                    X"E383A7E383BCE38388E382ABE38383E38388" )
      *    *** 本日更新
                 OR PIN3-REC(9:12) = X"E69CACE697A5E69BB4E696B0"
      *    *** 登録解除
                  OR PIN3-REC(9:12) = X"E799BBE98CB2E8A7A3E999A4"
      *    ***  のチャンネル登録を
                 OR PIN3-REC(9:28) = 
             X"20E381AEE38381E383A3E383B3E3838DE383ABE799BBE98CB2E38292"
      *    *** この動画のチャプター数:
                 OR  PIN3-REC(9:34) = WK-KONODOUGA
      *    *** チャプター数
                 OR  PIN3-REC(9:18) =
               X"E38381E383A3E38397E382BFE383BCE695B0"
      *    *** のチャンネル登録を
                 OR  PIN3-REC(9:27) =
           X"E381AEE38381E383A3E383B3E3838DE383ABE799BBE98CB2E38292"
      *    *** 視聴 ,
                   OR PIN3-REC (WK-PIN3-LEN - 7:8) = X"E8A696E881B4202C"
      *    *** キューに追加
                 OR PIN3-REC(9:18) =
                    X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"

                  ))
               OR

                  ( PIN3-REC(1:11) = " content : "
                AND
      *    *** SPACE
                  ( PIN3-REC(12:5) = SPACE
                 OR
      *    *** すべて再生
                    PIN3-REC(12:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** ミックスリスト
                 OR PIN3-REC(12:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    *** キューに追加
                 OR PIN3-REC(12:18) =
                    X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
      *    *** フィードバックを送信
                 OR PIN3-REC(12:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** プレイリスト
                 OR PIN3-REC(12:18) =
                    X"E38397E383ACE382A4E383AAE382B9E38388"
      *    *** すべてのポッドキャストを表示
                 OR  ( PIN3-REC(12:20) =
                       X"E38199E381B9E381A6E381AEE3839DE38383E383"
                   AND PIN3-REC(32:22) = 
                       X"89E382ADE383A3E382B9E38388E38292E8A1A8E7A4BA" )
      *    *** カスタマイズされた通知のみ
                 OR  ( PIN3-REC(12:20) =
                       X"E382ABE382B9E382BFE3839EE382A4E382BAE381"
                   AND PIN3-REC(32:21) = 
                       X"95E3828CE3819FE9809AE79FA5E381AEE381BF" )  
      *    *** 共有
                 OR   PIN3-REC(12:06) =
                       X"E585B1E69C89"
      *    *** ショート
                 OR   PIN3-REC(12:12) =
                       X"E382B7E383A7E383BCE38388"
      *    *** 再生リストに保存
                 OR PIN3-REC(12:24) =
                     X"E5868DE7949FE383AAE382B9E38388E381ABE4BF9DE5AD98"
      *    *** コラボレーター
                 OR PIN3-REC(12:21) =
                       X"E382B3E383A9E3839CE383ACE383BCE382BFE383BC"
      *    *** 登録解除
                 OR PIN3-REC(12:12) =
                       X"E799BBE98CB2E8A7A3E999A4"
      *    *** カスタマイズされた
                 OR PIN3-REC(12:27) =
               X"E382ABE382B9E382BFE3839EE382A4E382BAE38195E3828CE3819F"
      *    *** なし
                 OR PIN3-REC(12:6) = X"E381AAE38197"
      *    *** すべて再生
                 OR PIN3-REC(12:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** YouTube
                 OR PIN3-REC(12:7) = "YouTube"
      *    ***  か月前 ,
              OR PIN3-REC (WK-PIN3-LEN - 11:12) = 
                  X"20E3818BE69C88E5898D202C"
      *    *** 視聴 ,
                  OR PIN3-REC (WK-PIN3-LEN - 7:8) = X"E8A696E881B4202C"
                       ))

                 OR
                    ( PIN3-REC(1:09) = " label : "
      *    *** SPACE
                AND ( PIN3-REC(10:06) = SPACE
      *    *** 次へ
                   OR PIN3-REC(10:06) = X"E6ACA1E381B8"
      *    *** キューに追加
                   OR PIN3-REC(10:18) =
                         X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
      *    *** 操作メニュー
                   OR PIN3-REC(10:18) =
                         X"E6938DE4BD9CE383A1E3838BE383A5E383BC"
      *    *** 興味なし
                   OR PIN3-REC(10:12) =
                         X"E88888E591B3E381AAE38197"
      *    *** 後で見る
                   OR PIN3-REC(10:12) =
                         X"E5BE8CE381A7E8A68BE3828B"
      *    *** 追加済み
                   OR PIN3-REC(10:12) =
                         X"E8BFBDE58AA0E6B888E381BF"
      *    *** 前へ
                   OR PIN3-REC(10:06) = X"E5898DE381B8" 
      *    *** チャンネルに移動
                   OR PIN3-REC(10:24) = 
                     X"E38381E383A3E383B3E3838DE383ABE381ABE7A7BBE58B95"
      *    *** フィードバックを送信
                 OR PIN3-REC(10:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** その他
                   OR PIN3-REC(10:9) = X"E3819DE381AEE4BB96"
      *    *** その他の操作
                   OR PIN3-REC(10:18) =
                     X"E3819DE381AEE4BB96E381AEE6938DE4BD9C"
      *    *** 確認済み
                   OR PIN3-REC(10:12) =
                     X"E7A2BAE8AA8DE6B888E381BF"
      *    *** 折りたたむ
                   OR PIN3-REC(10:15) =
                     X"E68A98E3828AE3819FE3819FE38280"
      *    *** 公式アーティスト チャンネル
                   OR ( PIN3-REC(10:20) =
                    X"E585ACE5BC8FE382A2E383BCE38386E382A3E382"
                   AND PIN3-REC(30:20) =
                    X"B9E3838820E38381E383A3E383B3E3838DE383AB")
      *    *** 字幕
                   OR PIN3-REC (10:6) = X"E5AD97E5B995"
      *    *** 視聴
                  OR PIN3-REC (WK-PIN3-LEN - 7:08) = X"E8A696E881B4202C"
      *    *** 閉じる
                   OR PIN3-REC(10:9) = X"E99689E38198E3828B"
      *    *** すべて表示
                  OR PIN3-REC(10:15) = X"E38199E381B9E381A6E8A1A8E7A4BA"
      *    *** YouTube ミックスリスト
                  OR PIN3-REC(10:29) =
           X"596F755475626520E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    *** ショート
                  OR PIN3-REC(10:12) = X"E382B7E383A7E383BCE38388"
      *    *** チャンネル登録者数
                  OR PIN3-REC(10:27) =
           X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
      *    *** チャンネル登録
                 OR PIN3-REC(10:21) =
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
      *    *** 設定
                   OR PIN3-REC(10:6) = X"E8A8ADE5AE9A"
      *    *** キャンセル
                  OR PIN3-REC(10:15) = X"E382ADE383A3E383B3E382BBE383AB"
      *    *** 共有
                  OR PIN3-REC(10:6) = X"E585B1E69C89"
      *    *** 登録解除
                  OR PIN3-REC(10:12) = X"E799BBE98CB2E8A7A3E999A4"
      *    *** チャンネル「
                  OR PIN3-REC(10:18) =
                     X"E38381E383A3E383B3E3838DE383ABE3808C"
      *    *** コラボレーション チャンネル
                  OR PIN3-REC(10:40) = WK-KORABO
      *    *** キャンセル
                  OR PIN3-REC(10:15) = X"E382ADE383A3E383B3E382BBE383AB"

                     ))

               OR (  PIN3-REC (1:13) = " url : https:" 
                 OR  WK-CHANNEL (1:07) = " https:"
                 OR  WK-CHANNEL (1:09) = "/watch?v="
                 OR  WK-CHANNEL (1:08) = "/shorts/"
                 OR  WK-CHANNEL (1:09) = "/results?"
                 OR  WK-CHANNEL (1:13) = "//i.ytimg.com"
                   )
               OR ( WK-VIDEOID = SPACE 
                AND WK-CHANNEL = SPACE
                AND WK-TEXT    = SPACE )
               OR ( WK-TEXT    = SPACE )
                   CONTINUE
           ELSE
                   MOVE    WK-VIDEOID  TO      TBL02-VIDEOID (TBL02-IDX)
                   MOVE    WK-CHANNEL  TO      TBL02-CHANNEL (TBL02-IDX)
                   MOVE    WK-TEXT     TO      TBL02-TEXT    (TBL02-IDX)

                   MOVE WK-VIDEOID-LEN  TO TBL02-VIDEOID-LEN (TBL02-IDX)
                   MOVE WK-CHANNEL-LEN  TO TBL02-CHANNEL-LEN (TBL02-IDX)
                   MOVE WK-TEXT-LEN     TO TBL02-TEXT-LEN    (TBL02-IDX)
                   MOVE    WK-PIN3-CNT  TO      TBL02-SEQ (TBL02-IDX)

                   MOVE    TBL02-IDX   TO      I2-MAX
                   SET     TBL02-IDX   UP  BY  1
           END-IF
           .
       S082-EX.
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

                           MOVE    WK-SIMPLETEXT TO    WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-LABEL    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-SIMPLETEXT2 TO   WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-LABEL2   TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL2   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** TEXT3
                   IF      WK-TEXT3 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-TEXT3    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-TEXT3    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI1
                   IF      WK-YOBI1 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI1    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI1    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI2
                   IF      WK-YOBI2 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI2    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI2    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI3
                   IF      WK-YOBI3 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI3    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI3    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           ELSE
      *    *** PLAYLIST の時、

      *    *** 
                    SEARCH ALL TBL01-AREA
                        AT  END 
                           CONTINUE
      *                  WHEN TBL01-SITE (TBL01-IDX)  (1:WK-PLAYLIST-LEN)
      *                     = WK-PLAYLIST (1:WK-PLAYLIST-LEN)
                        WHEN TBL01-SITE (TBL01-IDX) (1:WK-SITE1-LEN-MAX)
                           = WK-PLAYLIST (1:WK-SITE1-LEN-MAX)


                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    TBL01-TITLE (TBL01-IDX)
                                               TO      POT1-REC (1:)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-SEARCH

                   IF      WK-WATCH (1:1) NOT = SPACE

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '<a href="' TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-WATCH    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '">'        TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    "FIRST VIDEOS" TO   POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    "</a>"      TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** タイトル?
                   IF      WK-TEXT3 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-TEXT3    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-SIMPLETEXT TO    WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-LABEL    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-SIMPLETEXT2 TO   WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

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

                           MOVE    WK-LABEL2   TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-LABEL2   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI1
                   IF      WK-YOBI1 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI1    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI1    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI2
                   IF      WK-YOBI2 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI2    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI2    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** YOBI3
                   IF      WK-YOBI3 (1:1) = SPACE
                           CONTINUE
                   ELSE

                           MOVE    WK-YOBI3    TO      WK-TEXTX
      *    *** CHANNEL 有無 CHECK
                           PERFORM S140-10     THRU    S140-EX

                           MOVE    '<br><br>'  TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-YOBI3    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-IF

      *    *** キューに追加
           IF      WK-TEXT2 (1:18) = 
                   X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
                   MOVE    SPACE       TO      WK-TEXT2
                   MOVE    ZERO        TO      WK-TEXT2-LEN
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

                   IF      WK-HTTPS (1:1) = SPACE
                       MOVE    '<br><br>'  TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-TEXT2    TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   ELSE
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



                   MOVE    '<a href="https://twitter.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** X
                   MOVE    "X"         TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.instagram.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** instagram
                   MOVE    "instagram"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'      TO      POT1-REC
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

                   MOVE    '</a><br><br>'  TO      POT1-REC
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

                   MOVE    '</a><br><br>'  TO      POT1-REC
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

           MOVE    '</a><br><br>'  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CHANNEL 有無 CHECK
       S140-10.

      *    *** https://www.youtube.com/watch?v=L1mFVp9IM9c

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I2-MAX
                        OR SW-SET = "Y"
                   MOVE    TBL02-TEXT-LEN (I2) TO L
                   MOVE    TBL02-VIDEOID-LEN (I2) TO L2
                   IF      TBL02-TEXT (I2) (1:L) = WK-TEXTX (1:L)
                       AND TBL02-VIDEOID (I2) (1:1) NOT = SPACE
                       AND TBL02-VIDEOID (I2) (1:L2) = WK-WATCH2 (33:L2)
                           MOVE    "Y"         TO      SW-SET

                           MOVE    '<a href="' TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    "https://www.youtube.com" TO
                                       POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           WRITE   POT1-REC    FROM    
                                   TBL02-CHANNEL (I2)
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '">'        TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-PERFORM
           .
       S140-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   PIN3-F
                   POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 件数 = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
