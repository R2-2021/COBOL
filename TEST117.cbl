      *    *** YouTube 動画サムネイル、再生リスト対応
      *    *** TEST103.XXXXXXXX.PIN1 PIN2 自動作成
      *    *** AV以外対応、PRM1 レイアウト変更したので、
      *    *** ACCEPT でB001 等入力に変更
      *    *** 
      *    *** 《XXXXX》 XXXXX:タイトル取り出し,PIN2作成する
      *    *** 
      *    *** PRM1-F で作成対象ファイル（ＣＣＴＶ電視劇再生リスト）を指定する
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1 
      *    *** 本日更新、ＮＮ日：更新等Google スプレッドシート上で行削除
      *    *** 総行数が３で割り切れるか確認しておく、
      *    *** htmlでダウンロード後、URL抽出、隣の列に貼り付け、
      *    *** 更にplaylistのhtmlを動画の隣の列に貼り付け、
      *    *** CSVでダウンロード後
      *    *** ”はカットして，＝＞．変更する
      *    *** =>
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN2
      *    *** 
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1　ダブっているので
      *    *** PLAYLIST 同じならカットする
      *    *** 
      *    *** TEST124 (ｉＱＩＹＩのみ,YOUKU ROMANCE)
      *    ***    |
      *    *** TEST117
      *    ***    |
      *    *** TEST104 TEST103.POT1 を作成する
      *    ***    |
      *    *** TEST53 032
      *    ***    |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST117.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** YouTube USER 指定 漢字の時，ＵＴＦ８で指定
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1 <= やめる
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN2
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.POT3
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(080).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(1000).

       FD  POT3-F.
       01  POT3-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST117 ".

      *    *** PRM1 でXXXXXXXX を１件目に指定、ＵＴＦ８で指定、
      *    *** ＳＪＩＳに変換可能な時のみ
           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST103.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(064) VALUE 
               "TEST103.XXXXXXXX.PIN1".
      *    *** は未使用とする
           03  WK-POT1-F-NAME  PIC  X(064) VALUE
               "TEST117.POT1".
           03  WK-POT2-F-NAME  PIC  X(064) VALUE
               "TEST103.XXXXXXXX.PIN2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST117.POT3".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE3-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE4-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE        PIC  X(1000) VALUE SPACE.
           03  WK-TITLE2       PIC  X(300) VALUE SPACE.
           03  WK-TITLE3       PIC  X(300) VALUE SPACE.
           03  WK-TITLE4       PIC  X(300) VALUE SPACE.
           03  WK-TITLE5       PIC  X(300) VALUE SPACE.
           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-REC2         PIC  X(1000) VALUE SPACE.
           03  WK-WATCH        PIC  X(300) VALUE SPACE.
           03  WK-PLAYLIST     PIC  X(300) VALUE SPACE.
           03  WK-FILE-NAME    PIC  X(064) VALUE SPACE.
           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-ID           PIC  X(004) VALUE SPACE.
           03  WK-SONOTA.

      *    *** ￥ 一番最後にするため
             05                PIC  X(003) VALUE X"EFBFA5".
      *    *** その他
             05                PIC  X(009) VALUE X"E3819DE381AEE4BB96".
             05                PIC  X(001) VALUE ",".
      *    *** その他
             05                PIC  X(009) VALUE X"E3819DE381AEE4BB96".
             05                PIC  X(003) VALUE ",0,".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-KAKKO        PIC  X(001) VALUE "N".
           03  SW-HIT1         PIC  X(001) VALUE "N".
           03  SW-HIT2         PIC  X(001) VALUE "N".
           03  SW-SURA         PIC  X(001) VALUE "N".
           03  SW-YES          PIC  X(001) VALUE "N".

      *    *** TBL01 未使用に変更
       01  TBL-AREA.
      *    03  TBL01-AREA      OCCURS 2000.
           03  TBL01-AREA      OCCURS 1.
             05 TBL01-PLAYLIST PIC  X(300) VALUE SPACE.
             05 TBL01-REC      PIC  X(1000) VALUE SPACE.

       01  TBL-AREA2.
           03  TBL02-AREA      OCCURS 4000
                               ASCENDING KEY TBL02-TITLE3.
             05 TBL02-TITLE2   PIC  X(1000) VALUE HIGH-VALUE.
             05 TBL02-TITLE3   PIC  X(300) VALUE HIGH-VALUE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN,READ PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX
           IF      PIN1-REC (3:21) =
      *    *** 本の動画再生中
                   X"E69CACE381AEE58B95E794BBE5868DE7949FE4B8AD"
                OR PIN1-REC (4:21) =
                   X"E69CACE381AEE58B95E794BBE5868DE7949FE4B8AD"
                OR PIN1-REC (5:21) =
                   X"E69CACE381AEE58B95E794BBE5868DE7949FE4B8AD"
                OR PIN1-REC (6:21) =
                   X"E69CACE381AEE58B95E794BBE5868DE7949FE4B8AD"

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-IF

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** TEST103.XXXXXXXX.PIN2 自動作成
      *             PERFORM S100-10     THRU    S100-EX
      *    *** TEST103.XXXXXXXX.PIN2 自動作成 1件から複数レコード出力
                   PERFORM S130-10     THRU    S130-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

      *    *** 再生リストの全体
                   IF      PIN1-REC (1:24) =
                   X"E5868DE7949FE383AAE382B9E38388E381AEE585A8E4BD93"
      *    *** すべてのポッドキャス
                       OR  PIN1-REC (1:30) =
       X"E38199E381B9E381A6E381AEE3839DE38383E38389E382ADE383A3E382B9"

      *    *** 2件読み飛ばし、再生リスト対応
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX

                           IF      WK-PIN1-EOF NOT = HIGH-VALUE
      *    *** READ PIN1
                                   PERFORM S020-10     THRU    S020-EX
                           END-IF
                   END-IF
           END-PERFORM

      *    *** POT1 TITLE2X 順に出力するために，ＳＯＲＴする
      *    *** 
      *    *** 内部SORTなので、漢数字、一、二、三等が順番に並ばない、
      *    *** COBSORTでソートし直す必要有り
           SORT    TBL02-AREA
                   ASCENDING KEY TBL02-TITLE3

      *    *** POT1 WRITE
      *     PERFORM S110-10     THRU    S110-EX

      *    *** POT2,その他 WRITE
           PERFORM S120-10     THRU    S120-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           PERFORM UNTIL SW-YES =      "Y" OR "y"

                   DISPLAY WK-PGM-NAME " TEST103.PRM1 のどのIDを"
                           "入力するか B001 等入力"

                   ACCEPT  WK-ID

                   DISPLAY "ID=" WK-ID
                   DISPLAY "ID  OK ? Y(y)/N"
                   ACCEPT  SW-YES
           END-PERFORM

           OPEN    INPUT       PRM1-F
           PERFORM UNTIL PRM1-REC (1:4) = WK-ID
                   READ    PRM1-F
                           AT  END
                           DISPLAY WK-PGM-NAME " PRM1-F ID 無しエラー"
                           STOP    RUN
                   END-READ 
                   ADD     1           TO      WK-PRM1-CNT
           END-PERFORM

      *    *** ファイル名は漢字のみか、１バイト系のみのどちらかに編集する
           IF      PRM1-REC (5:1) >=   X"E0" AND <= X"EF"
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    WK-PRM1-LEN TO      WDE05-BUF1-LEN
                   MOVE    WK-PRM1-CNT TO      WDE05-BUF1-CNT
      *    *** ファイル名 ＵＴＦ８＝＞ＳＪＩＳに変換
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               PRM1-REC (5:)
                                               WK-FILE-NAME
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
      *                                         WK-POT1-F-NAME (1:8)
                                               WK-POT2-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (9:)
      *                                         WK-POT1-F-NAME (9:)
                                               WK-POT2-F-NAME (9:)
      *             MOVE    ".PIN1X"    TO      WK-PIN1-F-NAME
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
      *             MOVE    ".PIN1"     TO      WK-POT1-F-NAME
      *                                         (WDE05-BUF2-LEN + 9:5)
                   MOVE    ".PIN2"     TO      WK-POT2-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
           ELSE
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
      *                                         WK-POT1-F-NAME (1:8)
                                               WK-POT2-F-NAME (1:8)
                   MOVE    PRM1-REC (5:) TO    WK-PIN1-F-NAME (9:)
      *                                         WK-POT1-F-NAME (9:)
                                               WK-POT2-F-NAME (9:)
      *             MOVE    ".PIN1X"    TO      WK-PIN1-F-NAME
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WK-PRM1-LEN + 9:5)
      *             MOVE    ".PIN1"     TO      WK-POT1-F-NAME
      *                                         (WK-PRM1-LEN + 9:5)
                   MOVE    ".PIN2"     TO      WK-POT2-F-NAME
                                               (WK-PRM1-LEN + 9:5)
                   MOVE    PRM1-REC    TO      WK-FILE-NAME
           END-IF

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F
                               POT2-F
                               POT3-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-WATCH
                                       WK-PLAYLIST
           MOVE    ZERO        TO      WK-TITLE-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT

                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL PIN1-REC (J:7) =    ",https:"
                              OR J > WK-PIN1-LEN

                           EVALUATE TRUE
                               WHEN PIN1-REC (J:1) =    ","
                                   MOVE    "."         TO
                                          PIN1-REC (J:1)
                               WHEN PIN1-REC (J:1) =    '"'
                                   MOVE    SPACE       TO
                                          PIN1-REC (J:1)
                           END-EVALUATE
                   END-PERFORM

                   UNSTRING PIN1-REC
                       DELIMITED BY ","
                       INTO
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-WATCH
                           WK-PLAYLIST
                   END-UNSTRING
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** TEST103.XXXXXXXX.PIN2 自動作成
      *    *** 1件から1レコード出力
       S100-10.

           MOVE    SPACE       TO      WK-TITLE2
           MOVE    "N"         TO      SW-KAKKO
           MOVE    ZERO        TO      J2
                                       WK-COUNT-1

      *    *** 》
           INSPECT WK-TITLE TALLYING
                   WK-COUNT-1 FOR ALL X"E3808B"

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-TITLE-LEN

                   EVALUATE TRUE
      *    *** 【毎日19点更新】はサーチ、スキップ
                       WHEN WK-TITLE (J:23) =
                       X"E38090E6AF8FE697A53139E782B9E69BB4E696B0E38091"
                           ADD     22          TO      J
      *    *** 【毎日19:00更新】はサーチ、スキップ
                       WHEN WK-TITLE (J:17) =
                       X"E38090E6AF8FE697A531393A3030E69BB4E696B0E38091"
                           ADD     16          TO      J

      *    *** 【毎日六日21:00更新】はサーチ、スキップ
                       WHEN WK-TITLE (J:29) =
           X"E38090E6AF8FE591A8E585ADE697A532313A3030E69BB4E696B0E38091"
                           ADD     28          TO      J

      *    *** 【?20:00(GMT+8)】
      *    *** 【?19:00(GMT+8)】はサーチ、スキップ
                       WHEN WK-TITLE (J:21) =
                           X"E38090E28FB032303A303028474D542B3829E38091"
                        OR X"E38090E28FB031393A303028474D542B3829E38091"
                           ADD     20          TO      J
      *    *** 【ESP SUB】
      *    *** 【ESP USB】
      *    *** 【ENG SUB】はサーチ、スキップ
                       WHEN WK-TITLE (J:13) =
                           X"E3809045535020535542E38091"
                        OR X"E3809045535020555342E38091"
                        OR X"E38090454E4720535542E38091"
                           ADD     12          TO      J
      *    *** 【Multi SUB】はサーチ、スキップ
                       WHEN WK-TITLE (J:15) =
                           X"E380904D756C746920535542E38091"
                           ADD     14          TO      J
      *    *** 【CCTV《
                       WHEN WK-TITLE (J:10) = X"E3809043435456E3808A"
                           MOVE    "Y"         TO      SW-KAKKO
                           ADD     9           TO      J
      *    *** 》有りの時、【、】はチェックしない
                       WHEN WK-COUNT-1 NOT = ZERO
      *    *** 《
                            AND ( WK-TITLE (J:3) = X"E3808A"
      *    *** 》
                              OR  WK-TITLE (J:3) = X"E3808B" )

      *    *** 《
                                IF  WK-TITLE (J:3) = X"E3808A"
                                    MOVE    "Y"         TO      SW-KAKKO
                                    ADD     2           TO      J
                                ELSE
                                    MOVE    WK-TITLE-LEN TO     J
                                END-IF
      *    *** 《
      *                 WHEN WK-COUNT-1 = ZERO
      *                  AND WK-TITLE (J:3) = X"E3808A"
      *                     MOVE    "Y"         TO      SW-KAKKO
      *                     ADD     2           TO      J

      *    *** 》
      *                 WHEN WK-COUNT-1 = ZERO
      *                  AND WK-TITLE (J:3) = X"E3808B"
      *                     MOVE    WK-TITLE-LEN TO     J

                       WHEN WK-COUNT-1 = ZERO
      *    *** 【
                        AND WK-TITLE (J:3) = X"E38090"
                           MOVE    "Y"         TO      SW-KAKKO
                           ADD     2           TO      J

      *    *** 】
                       WHEN WK-COUNT-1 = ZERO
                        AND WK-TITLE (J:3) = X"E38091"
                           MOVE    WK-TITLE-LEN TO     J

                       WHEN OTHER 

                           IF      SW-KAKKO    =       "Y"
                                   ADD     1           TO      J2
                                   MOVE    WK-TITLE (J:1) TO
                                           WK-TITLE2 (J2:1)
                           END-IF
                   END-EVALUATE
           END-PERFORM

      *    *** TEST104 でその他に分類される
      *     IF SW-KAKKO = "N" 
      *         DISPLAY "カッコが無いレコードNO=" WK-PIN1-CNT 
      *     END-IF

           MOVE    J2          TO      WK-TITLE2-LEN
           MOVE    ZERO        TO      J3
           IF      SW-KAKKO    =       "Y"
                   ADD     1           TO      J3
                   MOVE    SPACE       TO      WK-REC
                   MOVE    WK-TITLE2 (1:WK-TITLE2-LEN) TO
                           WK-REC (J3:WK-TITLE2-LEN)
                   ADD     WK-TITLE2-LEN TO    J3

                   MOVE    ","         TO      WK-REC (J3:1)
                   ADD     1           TO      J3

                   MOVE    WK-TITLE2 (1:WK-TITLE2-LEN) TO
                           WK-REC (J3:WK-TITLE2-LEN)
                   ADD     WK-TITLE2-LEN TO    J3

                   MOVE    ",0,"       TO      WK-REC (J3:3)

                   MOVE    "N"         TO      SW-HIT2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > I-MAX
                           IF      TBL02-TITLE2 (I) =    WK-REC
                                   MOVE    "Y"         TO      SW-HIT2
                           END-IF
                   END-PERFORM

                   IF      I           >       2000
                           DISPLAY WK-PGM-NAME " TBL02 OVER I=" I
                           STOP    RUN
                   END-IF

                   IF      SW-HIT2     =       "N"
                           MOVE    WK-REC      TO      TBL02-TITLE2 (I)
      *                     MOVE    WK-TITLE2 (1:WK-TITLE2-LEN) TO
      *                             TBL02-TITLE2X (I)
                           MOVE    I           TO      I-MAX
                   END-IF
           END-IF

           MOVE    "N"         TO      SW-HIT1
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > K-MAX
                   IF      TBL01-PLAYLIST (K) = WK-PLAYLIST
                           MOVE    "Y"         TO      SW-HIT1
                   END-IF
           END-PERFORM

           IF      K           >       2000
                   DISPLAY WK-PGM-NAME " TBL01 OVER K=" K
                   STOP    RUN
           END-IF

           IF      SW-HIT1     =       "N"
                   MOVE    WK-PLAYLIST TO      TBL01-PLAYLIST (K)
                   MOVE    PIN1-REC    TO      TBL01-REC      (K)
                   MOVE    K           TO      K-MAX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** POT1 WRITE
       S110-10.

           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > K-MAX
                   WRITE   POT1-REC    FROM    TBL01-REC (K)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** POT2,その他 WRITE
       S120-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

                   MOVE    TBL02-TITLE2 (I) TO POT2-REC

                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT

                   UNSTRING TBL02-TITLE2 (I)
                           DELIMITED BY ","
                           INTO
                           POT3-REC
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT

           END-PERFORM

      *     IF      WK-FILE-NAME (1:WK-PRM1-LEN)
      *           = "ｉＱＩＹＩ　アイチーイー"
      *          OR "ｉＱＩＹＩ　チャイニーズ　シアター"
      *          OR "ｉＱＩＹＩ　オリエンタル　ファンタジー"
      *          OR "YOUKU ROMANCE"
      *          OR "マンゴーＴＶ青少年劇場"

      *             MOVE    "FULL,FULL,0," TO   POT2-REC

      *             WRITE   POT2-REC
      *             ADD     1           TO      WK-POT2-CNT
      *     END-IF

           MOVE    WK-SONOTA   TO      POT2-REC
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S120-EX.
           EXIT.

      *    *** TEST103.XXXXXXXX.PIN2 自動作成
      *    *** 1件から複数レコード出力
       S130-10.

           MOVE    SPACE       TO      WK-TITLE2
           MOVE    ZERO        TO      J2
           MOVE    "N"         TO      SW-SURA

      *    *** 》
      *     INSPECT WK-TITLE TALLYING
      *             WK-COUNT-1 FOR ALL X"E3808B"

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-TITLE-LEN

                   EVALUATE TRUE

      *    *** 《
                       WHEN WK-TITLE (J:3) = X"E3808A"
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** 》
                       WHEN WK-TITLE (J:3) = X"E3808B"
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** 【
                       WHEN WK-TITLE (J:3) = X"E38090"
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** 】
                       WHEN WK-TITLE (J:3) = X"E38091"
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    ***  x , X 
                       WHEN WK-TITLE (J:3) = " x " OR " X "
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** ×
                       WHEN WK-TITLE (J:2) = X"C397"
                           ADD     1           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** ×
                       WHEN WK-TITLE (J:3) = X"E29893"
                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** |
                       WHEN WK-TITLE (J:1) = "|" OR "(" OR ")" 
                           OR "[" OR "]"

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** /
                       WHEN WK-TITLE (J:1) = "/" 
                           MOVE    "Y"         TO      SW-SURA

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
      *    *** / の時、SW-SURA=N にしない
      *                             MOVE    "N"         TO      SW-SURA
                           END-IF

      *    *** ｜
                       WHEN WK-TITLE (J:3) = X"EFBD9C" 
      *    *** ｜太い縦線
                                          OR X"E4B8A8"
      *    *** ― 横線
                                          OR X"E28094"
      *    *** “
                                          OR X"E2809C"
      *    *** （
                                          OR X"EFBC88"
      *    *** ）
                                          OR X"EFBC89"
      *    *** ‐
                                          OR X"E28090"

                           ADD     2           TO      J

                           IF      J2          >       ZERO
      *    *** TBL01,TBL02 SET
                                   PERFORM S132-10     THRU    S132-EX

                                   MOVE    SPACE       TO      WK-TITLE2
                                   MOVE    ZERO        TO      J2
                                   MOVE    "N"         TO      SW-SURA
                           END-IF

                       WHEN OTHER
                           ADD     1           TO      J2

      *    *** 先頭のスペースカット
                           IF      J2          =       1
                              AND ( WK-TITLE (J:1) =    SPACE
                                 OR WK-TITLE (J:1) =    "#" 
                                 OR WK-TITLE (J:1) =    "-" 
                                 OR WK-TITLE (J:1) =    "." 
                                 OR WK-TITLE (J:1) =    "(" 
                                     )
                                   MOVE    ZERO        TO      J2
                           ELSE
                                   MOVE    WK-TITLE (J:1) TO
                                           WK-TITLE2 (J2:1)
                           END-IF
                   END-EVALUATE
           END-PERFORM

           IF      J2          >       ZERO
      *         AND WK-TITLE2 (J2:1) NOT =  SPACE

      *    *** TBL01,TBL02 SET
                   PERFORM S132-10     THRU    S132-EX

                   MOVE    SPACE       TO      WK-TITLE2
                   MOVE    ZERO        TO      J2
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** TBL01,TBL02 SET
      *    *** 《,》,【,】 がある時、このルーチンに来てテーブルセットする
       S132-10.

      *    *** 1つ前が/有りなら、#付ける、1名しか無い時、#付加は無理なので
      *    *** 複数名指定有りの時、#付ける、俳優名以外の時も/あるがそのまま付ける
           IF      SW-SURA     =       "Y"
      *    *** 同一の項目でもMOVE可能
                   MOVE    WK-TITLE2 (1:J2) TO WK-TITLE2 (2:J2)
                   MOVE    "#"         TO      WK-TITLE2 (1:1)
                   ADD     1           TO      J2
           END-IF

      *     IF      J2          =       ZERO
      *             GO  TO  S132-EX
      *     END-IF

      *    *** 最後のスペースカット
           IF      WK-TITLE2 (J2:1) =  SPACE
                   IF      J2          =       1
                           GO  TO  S132-EX
                   ELSE
                           ADD     -1          TO      J2
                   END-IF
           END-IF

           MOVE    J2          TO      WK-TITLE2-LEN
           MOVE    ","         TO      WK-TITLE2 (WK-TITLE2-LEN + 1:1)

           MOVE    ZERO        TO      J3

           ADD     1           TO      J3
           MOVE    SPACE       TO      WK-REC

           IF      SW-SURA     =       "Y"
                   MOVE    WK-TITLE2 (2:WK-TITLE2-LEN - 1) TO
                           WK-REC (J3:WK-TITLE2-LEN - 1)
                   ADD     WK-TITLE2-LEN -1 TO J3
           ELSE
                   MOVE    WK-TITLE2 (1:WK-TITLE2-LEN) TO
                           WK-REC (J3:WK-TITLE2-LEN)
                   ADD     WK-TITLE2-LEN TO    J3
           END-IF

           MOVE    SPACE       TO      WK-TITLE3
           UNSTRING WK-TITLE2
                   DELIMITED BY ","
                   INTO
                   WK-TITLE3 COUNT WK-TITLE3-LEN

           MOVE    WK-TITLE3   TO      WK-TITLE4
           MOVE    WK-TITLE3-LEN TO    WK-TITLE4-LEN

           INSPECT WK-TITLE3 CONVERTING 
                   "abcdefghijklmnopqrstuvwxyz"
                TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

           INSPECT WK-TITLE3 REPLACING 
      *    *** 一
                                        ALL X"E4B880" BY X"E28680"
      *    *** 二
                                        ALL X"E4BA8C" BY X"E28681"
      *    *** 三
                                        ALL X"E4B889" BY X"E28682"
      *    *** 四
                                        ALL X"E59B9B" BY X"E28683"
      *    *** 五
                                        ALL X"E4BA94" BY X"E28684"
      *    *** 六
                                        ALL X"E585AD" BY X"E28685"
      *    *** 七
                                        ALL X"E4B883" BY X"E28686"
      *    *** 八
                                        ALL X"E585AB" BY X"E28687"
      *    *** 九
                                        ALL X"E4B99D" BY X"E28688"
      *    *** 十
                                        ALL X"E58D81" BY X"E28689"

           MOVE    ","         TO      WK-REC (J3:1)
           ADD     1           TO      J3

           MOVE    WK-TITLE2 (1:WK-TITLE2-LEN) TO
                   WK-REC (J3:WK-TITLE2-LEN)
           ADD     WK-TITLE2-LEN TO    J3

           MOVE    ",0,"       TO      WK-REC (J3:3)

           MOVE    "N"         TO      SW-HIT2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   IF      TBL02-TITLE2 (I) =    WK-REC
                           MOVE    "Y"         TO      SW-HIT2
                   END-IF
           END-PERFORM

           IF      I           >       4000
                   DISPLAY WK-PGM-NAME " TBL02 OVER 1 I=" I
                   STOP    RUN
           END-IF

           IF      SW-HIT2     =       "N"
                   MOVE    WK-REC      TO      TBL02-TITLE2 (I)
                   MOVE    WK-TITLE3   TO      TBL02-TITLE3 (I)
                   MOVE    I           TO      I-MAX
           END-IF

      *    *** WK-TITLE4 は WK-TITLE3 のSORT INSPECT 変更前の状態
           MOVE    ZERO        TO      WK-COUNT-1
      *    *** SPACE+X"E0"-X"EF" 漢字含むか
           PERFORM VARYING L FROM 1 BY 1
                   UNTIL L > WK-TITLE4-LEN
                     OR WK-COUNT-1 NOT = ZERO
                   IF      WK-TITLE4 (L:2) >=  X"20E0" AND <=  X"20EF"
                           ADD     1           TO      WK-COUNT-1
                   END-IF
           END-PERFORM
      *    *** L WK-COUNT-1 = 1 の時､X"E3"　の位置

      *    *** もう１つTBL02 作成する
      *    *** 下記式で結果同じだった、AND先に結果出して、その後ORで比較している
      *    *** （）あった方が確実と思われる
           IF      WK-COUNT-1  >        ZERO
             AND (( WK-TITLE4 (1:1) >= "a" AND <= "z" )
               OR ( WK-TITLE4 (1:1) >= "A" AND <= "Z" ))
      *       AND ( WK-TITLE4 (1:1) >= "a" AND <= "z" 
      *         OR  WK-TITLE4 (1:1) >= "A" AND <= "Z" )
             AND ( WK-TITLE4 (WK-TITLE4-LEN - 2:1) >= X"E0" 
                   AND <= X"EF")

                   COMPUTE WK-TITLE4-LEN = WK-TITLE4-LEN - L + 1
                   MOVE    ZERO        TO      J3

                   ADD     1           TO      J3
                   MOVE    SPACE       TO      WK-REC2

      *    *** TEST103.ｉＱＩＹＩ　アイチーイー.PIN1 コメント長すぎるとカラム7のエラーが出る
      *    *** Love You Seven Times 七吉祥 FULL-EP36 | Yang Chaoyue 超越
      *    ***  x Ding Yuxi 丁禹兮 | ‐Kiwi Only | FULL‐
      *    ***  【iQIYI |Join the Membership and enjoy full episodes now!,
      *    ***  https://www.youtube.com/watch?v=Xyp7WQ07NNA&amp,
      *    ***  https://www.youtube.com/playlist?list=PLlCrV9TCfzMbWzpxGJRPyc-QrYN67DGtd
      *    *** Yang Chaoyue 超越 ?はUTF8の為、SJISでは表示出来ない
      *    *** 超越 をWK-REC2 にセットする

      *    *** 漢字部分のみセット
                   MOVE    WK-TITLE4 (L:WK-TITLE4-LEN) TO
                           WK-REC2 (J3:WK-TITLE4-LEN)
                   ADD     WK-TITLE4-LEN TO    J3

                   MOVE    "#"         TO      WK-TITLE5 (1:1)
                   MOVE    WK-REC2     TO      WK-TITLE5 (2:)

                   MOVE    ","         TO      WK-REC2 (J3:1)
                   ADD     1           TO      J3

      *    *** SW-SURA = "Y" と同じ処理とする（俳優名と思われる為、先頭に出力）
                   MOVE    "#"         TO      WK-REC2 (J3:1)
                   ADD     1           TO      J3

                   MOVE    WK-TITLE4 (L:WK-TITLE4-LEN) TO
                           WK-REC2 (J3:WK-TITLE4-LEN)
                   ADD     WK-TITLE4-LEN TO    J3

                   MOVE    ",0,"       TO      WK-REC2 (J3:3)

      *    *** WK-TITLE5 SORT KEY 情報変更する
                   INSPECT WK-TITLE5 CONVERTING 
                          "abcdefghijklmnopqrstuvwxyz"
                       TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

                   INSPECT WK-TITLE5 REPLACING 
      *    *** 一
                                        ALL X"E4B880" BY X"E28680"
      *    *** 二
                                        ALL X"E4BA8C" BY X"E28681"
      *    *** 三
                                        ALL X"E4B889" BY X"E28682"
      *    *** 四
                                        ALL X"E59B9B" BY X"E28683"
      *    *** 五
                                        ALL X"E4BA94" BY X"E28684"
      *    *** 六
                                        ALL X"E585AD" BY X"E28685"
      *    *** 七
                                        ALL X"E4B883" BY X"E28686"
      *    *** 八
                                        ALL X"E585AB" BY X"E28687"
      *    *** 九
                                        ALL X"E4B99D" BY X"E28688"
      *    *** 十
                                        ALL X"E58D81" BY X"E28689"

                   MOVE    "N"         TO      SW-HIT2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > I-MAX
                           IF      TBL02-TITLE2 (I) =    WK-REC2
                                   MOVE    "Y"         TO      SW-HIT2
                           END-IF
                   END-PERFORM

                   IF      I           >       4000
                           DISPLAY WK-PGM-NAME " TBL02 OVER 2 I=" I
                           STOP    RUN
                   END-IF

                   IF      SW-HIT2     =       "N"
                           MOVE    WK-REC2     TO      TBL02-TITLE2 (I)
                           MOVE    WK-TITLE5   TO      TBL02-TITLE3 (I)
                           MOVE    I           TO      I-MAX
                   END-IF
           END-IF
           .
       S132-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   POT1-F
                   POT2-F
                   POT3-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 件数 = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
