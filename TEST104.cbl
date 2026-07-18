      *    *** YouTube/MissAV 動画サムネイル、自動付加
      *    *** 
      *    *** TEST103 => TEST104 TBL から PIO1-F に変更
      *    *** TEST104 => TEST104
      *    *** (Walk East.csv) TEST103.PIN1 => TEST103.POT1
      *    *** 
      *    *** 再生リスト対応、サムネイルクリックでPLAYLIST 出力
      *    *** TEST103.ＣＣＴＶ電視劇再生リスト.PIN1
      *    *** 
      *    *** 再生リスト対応はTEST104のみ修正した
      *    *** 
      *    *** PIN2 "=>SPACE か？
      *    *** その他最後にあるか？
      *    *** タイトルに漢数字あるとき、COBSORTしたか？
      *    *** 
      *    *** 関連アプリ
      *    *** TEST117
      *    *** TEST130

      *    ***   動画     再生リスト(PLAYLIST)対応
      *    *** TEST104    TEST117
      *    ***    |          |
      *    *** TEST53 032 TEST104
      *    ***    |          |
      *    *** TEST54     TEST53 032
      *    ***               |
      *    ***            TEST54
      *    ***            
      *    ***            
      *    *** MissAV用 TEST103.Ｍｉｓｓａｖ.PIN2 女優名追加データ自動作成
      *    *** 
      *    *** TEST132
      *    ***    |
      *    *** TEST104
      *    ***    |
      *    *** TEST53 032
      *    ***    |
      *    *** TEST54
      *    *** 
      *    *** 出力はどちらも、TEST103.POT1

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST104.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** YouTube USER 指定 漢字の時，ＵＴＦ８で指定
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ジャパリに<br>挿入で処理するタイトル指定
       SELECT PRM2-F           ASSIGN   WK-PRM2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** Youtube動画 タイトル、動画urlデータ
      *    *** Youtube動画 CTRL+ A,CTRL+ C でＥｘｃｅｌに貼り付け、
      *    *** ＳＯＲＴ後、不要部分カット、Googleスプレッドに貼り付け、
      *    *** ダウンロード（ｈｔｍｌ）後、展開し、ＵＲＬ抽出後、
      *    *** URL抽出：https://www.ahref.org/urlchu.php
      *    *** （重複を除く、ドメインだけ抽出する　はチェックマーク外す）
      *    *** ＵＲＬをＥｘｃｅｌの別の列に貼り付け、ＣＳＶ（ＵＴＦ８）
      *    *** で出力後、サクラエディターで出力モード（ＢＯＭ）をＯＦＦ
      *    *** で再出力する
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** グルーピング データ
      *    *** EX.
      *    *** 上海,上海,0,
      *    *** 陸家嘴,上海,1,
      *    *** ＸＸＸ，上海,2,
      *    *** 陸家嘴は上海に集約する、1は、１つ前の場所に集約を意味する
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 中間ファイル
       SELECT PIO1-F           ASSIGN   WK-PIO1-F-NAME
           ORGANIZATION INDEXED
           ACCESS MODE RANDOM
           RECORD KEY PIO1-KEY.

      *    *** サムネイルimg データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(100).

       FD  PRM2-F
           RECORD VARYING DEPENDING ON WK-PRM2-LEN.
       01  PRM2-REC.
           03                  PIC  X(100).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(10000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  PIO1-F.
       01  PIO1-REC.
           03  PIO1-KEY        PIC  9(007).
           03  PIO1-NO-IDX     PIC  9(002).
           03  PIO1-NO         OCCURS 15
                               PIC  9(004).
           03  PIO1-DATA.
             05                PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(10000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST104 ".

      *    *** PRM1 でXXXXXXXX を１件目に指定、ＵＴＦ８も可能だが、
      *    *** ＳＪＩＳに変換可能な時のみ
           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST103.PRM1".
           03  WK-PRM2-F-NAME  PIC  X(032) VALUE "TEST103.PRM2".
           03  WK-PIN1-F-NAME  PIC  X(128) VALUE 
               "TEST103.XXXXXXXX.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
      *         "TEST103.Walk East.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(064) VALUE 
               "TEST103.XXXXXXXX.PIN2".
           03  WK-PIO1-F-NAME  PIC  X(032) VALUE "TEST104.PIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST103.POT1".

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PRM2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PRM2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1R-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1W-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PRM2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1R-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1W-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PRM2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-PLAYLIST-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-T-LEN        BINARY-LONG SYNC VALUE ZERO.
           03  WK-REL-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE        PIC  X(1000) VALUE SPACE.
           03  WK-TITLE2       PIC  X(1000) VALUE SPACE.
           03  WK-WATCH        PIC  X(300) VALUE SPACE.
           03  WK-WATCH2       PIC  X(300) VALUE SPACE.
           03  WK-PLAYLIST     PIC  X(300) VALUE SPACE.
           03  WK-REC          PIC  X(10000) VALUE SPACE.
           03  WK-REL          PIC  9(004) VALUE ZERO.
           03  WK-SEQ          BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEQ-EDIT.
             05  WK-SEQ-E      PIC  ZZ,ZZ9 VALUE ZERO.
             05                PIC  X(002) VALUE ". ". 
           03  WK-FILE-NAME    PIC  X(064) VALUE SPACE.
      *    *** 登録チャンネルが追加されました
           03  WK-TOUROKU.
             05                PIC  X(025) VALUE
                X"E799BBE98CB2E38381E383A3E383B3E3838DE383ABE3818CE8".
             05                PIC  X(020) VALUE
                X"BFBDE58AA0E38195E3828CE381BEE38197E3819F".
           03  WK-UNCEN        BINARY-LONG SYNC VALUE ZERO.

           03  WK-KEY.
             05  WK-OKEY.
               07  WK-ONO      PIC  X(020) VALUE LOW-VALUE.
             05  WK-NKEY.
               07  WK-NNO      PIC  X(020) VALUE LOW-VALUE.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT       PIC  X(004) VALUE SPACE.

           03  WK-ITEM1        PIC  X(100) VALUE SPACE.
           03  WK-ITEM2        PIC  X(100) VALUE SPACE.
           03  WK-ITEM3        PIC  X(100) VALUE SPACE.
           03  WK-ITEM4        PIC  X(100) VALUE SPACE.
           03  WK-ITEM5        PIC  X(100) VALUE SPACE.
           03  WK-ITEM6        PIC  X(100) VALUE SPACE.
           03  WK-ITEM5-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM6-LEN    BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J1              BINARY-LONG SYNC VALUE ZERO.
           03  J1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  K2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K3              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  M1              BINARY-LONG SYNC VALUE ZERO.
           03  M1-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-SHARP        PIC  X(001) VALUE "N".
           03  SW-END          PIC  X(001) VALUE "N".
           03  SW-HIT          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 4000.
             05  TBL01-NO      PIC  9(004) VALUE ZERO.
             05  TBL01-ITEM    PIC  X(300) VALUE SPACE.
             05  TBL01-ITEM-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HEAD    PIC  X(300) VALUE SPACE.
             05  TBL01-HEAD-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  TBL02-AREA      OCCURS 100.
             05  TBL02-ITEM    PIC  X(300) VALUE SPACE.
             05  TBL02-ITEM-LEN BINARY-LONG SYNC VALUE ZERO.

           03  TBL03-AREA      OCCURS 1000.
             05  TBL03-ITEM    PIC  X(100) VALUE SPACE.
             05  TBL03-ITEM-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL03-CNT     BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN,READ PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
                   IF      WK-PIN2-LEN NOT =   ZERO
      *    *** TBL01 SET
                           PERFORM S032-10     THRU    S032-EX

                   END-IF
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** READ PRM2
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-PRM2-EOF = HIGH-VALUE
                   IF      PRM2-REC (1:1) =    SPACE OR "*"
                           CONTINUE
                   ELSE
      *    *** TBL02 SET
                           PERFORM S042-10     THRU    S042-EX
                   END-IF

      *    *** READ PRM2
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

      *    *** WRITE POT1 %ヘッダー出力,1件目
           PERFORM S110-10     THRU    S110-EX

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
      *    *** 画像サムネイル付加
                   IF      WK-PIN1-LEN =       ZERO
      *    *** ジャパリ
                        OR PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"

      *    *** ZERO バイト長、ジャパリは出力しない
                           CONTINUE
                   ELSE
                           IF      WK-WATCH2 (1:23) =
                                   "https://www.youtube.com"
      *    *** 画像サムネイル付加 YOUTUBE
                                   PERFORM S100-10     THRU    S100-EX
                           ELSE
                               IF      WK-OKEY     NOT =   WK-NKEY
      *    *** 画像サムネイル付加 MISSAV
                                   PERFORM S200-10     THRU    S200-EX
                               ELSE
      *    *** https 降順でダウンロードしても、無修正（uncensored-leak）が
      *    *** 後になってしまうため、無修正（uncensored-leak）の時出力する
      *    *** ABF-164 ニートな姫にもほどがある！ 無職でヲタク、性欲溜まりまくりの干物美少女がチ●ポの快感を思い出しちゃった！ 八掛うみ【MGSだけのおまけ映像付き+35分】,https://missav.ai/dm40/ja/abf-164

                                   IF     PIN1-REC (WK-PIN1-LEN - 14:15)
      *    *** 同じＩＤで無修正（uncensored-leak）が２件目以降にある時出力する
                                        = "uncensored-leak"
      *    *** 画像サムネイル付加 MISSAV
                                       PERFORM S200-10     THRU  S200-EX
                                   END-IF
                               END-IF
                           END-IF
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

      *    *** 再生リストの全体
                   IF      PIN1-REC (1:24) =
                   X"E5868DE7949FE383AAE382B9E38388E381AEE585A8E4BD93"
      *    *** すべてのポッドキャス
                       OR  PIN1-REC (1:30) =
       X"E38199E381B9E381A6E381AEE3839DE38383E38389E382ADE383A3E382B9"
      *    *** 登録チャンネルが追加されました
      *                 OR  PIN1-REC (1:45) = WK-TOUROKU

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
           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX

                   IF      TBL01-CNT (K1) NOT = ZERO
                           MOVE    SPACE       TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
      *    *** PIN1-Fのジャパリは使わない、再作成する
      *    *** ジャパリ
                           MOVE    X"E382B8E383A3E38391E383AA"
                                               TO      POT1-REC

                           IF      SW-SEARCH   =       "Y"
                               MOVE    "<br>"  TO      POT1-REC (13:4)
                               ADD     1       TO      WK-SEQ
                               MOVE    WK-SEQ  TO      WK-SEQ-E
                               MOVE    WK-SEQ-EDIT TO  POT1-REC (17:8)
                               MOVE    TBL01-HEAD(K1) 
                                       (1:TBL01-HEAD-LEN(K1)) TO
                                       POT1-REC (25:TBL01-HEAD-LEN(K1))
                           ELSE
      *    *** 八掛うみ に姫崎あむのジャパリが出力されるのは、PIN2で
      *    *** 姫崎あむ,姫崎あむ,0,
      *    *** まな,まな,1,
      *    *** が指定されている為、タイトルに”まな”があり、
      *    *** 姫崎あむのジャパリが出力される
      *     IF  TBL01-HEAD(K1) (1:12) = X"E5A7ABE5B48EE38182E38280"
      *       DISPLAY "S100-10 K1=" K1 
      *     END-IF
                               MOVE    TBL01-HEAD(K1) 
                                       (1:TBL01-HEAD-LEN(K1)) TO
                                       POT1-REC (13:TBL01-HEAD-LEN(K1))
                           END-IF
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           PERFORM VARYING K2 FROM 1 BY 1
                                   UNTIL K2 > K2-MAX

      *    *** PIO1-F => WRITE POT1 に変更
                                   PERFORM S120-10     THRU    S120-EX
                           END-PERFORM
                   END-IF
           END-PERFORM

      *    *** CLOSE,END DISPLAY
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

      *    *** PIO1-F ZERO件に初期化
           OPEN    OUTPUT      PIO1-F
           CLOSE   PIO1-F

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
      *         WHEN 0
      *             MOVE    1           TO      WK-ACCEPT
               WHEN 1
                   ACCEPT  WK-ACCEPT FROM ARGUMENT-VALUE

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " PRM1-F 読み込み 1個まで指定可"
                   DISPLAY WK-PGM-NAME 
                           " TEST104 A001 <=例 PRM1のレコード1,4を指定"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PRM1-F
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL SW-HIT = "Y"
                      OR WK-PRM1-EOF = HIGH-VALUE
                   READ    PRM1-F
                           AT  END
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F 0ｹﾝ OR 指定レコードＮｏ無し"
                           STOP    RUN
                   END-READ
                   ADD     1           TO      WK-PRM1-CNT

      *             IF      WK-ACCEPT   =       WK-PRM1-CNT
                   IF      WK-ACCEPT   =       PRM1-REC (1:4)
                           MOVE    "Y"         TO      SW-HIT
                   END-IF
           END-PERFORM

           IF      SW-HIT      =       "N"
                   DISPLAY WK-PGM-NAME
                           " PRM1-F 指定レコードＮｏ無し,A001,B001,C001"
                           " 等指定する"
                   STOP    RUN
           ELSE
                   ADD     -4          TO      WK-PRM1-LEN
                   MOVE    PRM1-REC (5:) TO    PRM1-REC (1:)
           END-IF

      *    *** ファイル名は漢字のみか、１バイト系のみのどちらかに編集する
           IF      PRM1-REC (1:1) >=   X"E0" AND <= X"EF"
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    WK-PRM1-LEN TO      WDE05-BUF1-LEN
                   MOVE    WK-PRM1-CNT TO      WDE05-BUF1-CNT
      *    *** ファイル名 ＵＴＦ８＝＞ＳＪＩＳに変換
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               PRM1-REC
                                               WK-FILE-NAME
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-PIN2-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (9:)
      *    *** WK-FILE-NAME = SJIS
      *    *** ＭｉｓｓＡＶ
                   IF      WK-FILE-NAME (1:12) =
                           X"826C82898293829382608275"
                           MOVE    WK-FILE-NAME (1:12) TO
                                   WK-PIN2-F-NAME (9:)
                           MOVE    12          TO      L
                   ELSE
                           MOVE    WK-FILE-NAME TO   WK-PIN2-F-NAME (9:)
                   END-IF
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
      *    *** ＭｉｓｓＡＶ
                   IF      WK-FILE-NAME (1:12) =
                           X"826C82898293829382608275"
                           MOVE    ".PIN2"     TO      WK-PIN2-F-NAME
                                               (21:5)
                   ELSE
                           MOVE    ".PIN2"     TO      WK-PIN2-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
                   END-IF
           ELSE
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-PIN2-F-NAME (1:8)
                   MOVE    PRM1-REC    TO      WK-PIN1-F-NAME (9:)
                                               WK-PIN2-F-NAME (9:)
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WK-PRM1-LEN + 9:5)
                   MOVE    ".PIN2"     TO      WK-PIN2-F-NAME
                                               (WK-PRM1-LEN + 9:5)
           END-IF

           OPEN    INPUT       PRM2-F
                               PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F
                   I-O         PIO1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    WK-NKEY     TO      WK-OKEY
           MOVE    SPACE       TO      WK-TITLE
                                       WK-WATCH2
                                       WK-PLAYLIST
           MOVE    ZERO        TO      WK-PLAYLIST-LEN

           READ    PIN1-F
               AT END

      *             DISPLAY WK-PGM-NAME " TITLE-ID ダブリ"
      *             PERFORM VARYING J1 FROM 1 BY 1
      *                     UNTIL J1 > J1-MAX
      *                     IF      TBL03-CNT (J1) NOT = ZERO 
      *                             DISPLAY TBL03-ITEM (J1) (1:80) 
      *                     END-IF
      *             END-PERFORM

                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT

                   UNSTRING PIN1-REC
                           DELIMITED BY SPACE
                           INTO
                           WK-NNO
                   END-UNSTRING

                   MOVE    ",,"        TO  PIN1-REC (WK-PIN1-LEN + 1:2)

                   PERFORM VARYING L FROM 1 BY 1
                           UNTIL L > WK-PIN1-LEN
                              OR PIN1-REC (L:6) = ",https"
                           IF      PIN1-REC (L:1) =    ","
                                   MOVE    "."         TO
                                           PIN1-REC (L:1)
                           END-IF
                           IF      PIN1-REC (L:1) =    '"'
                                   MOVE    SPACE       TO
                                           PIN1-REC (L:1)
                           END-IF
                           IF      PIN1-REC (L:1) =    '#'
                                   MOVE    SPACE       TO
                                           PIN1-REC (L:1)
                           END-IF
                   END-PERFORM

                   UNSTRING PIN1-REC
                       DELIMITED BY "," OR "#"
                       INTO
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-WATCH2   COUNT WK-WATCH2-LEN
                           WK-PLAYLIST COUNT WK-PLAYLIST-LEN
                   END-UNSTRING

                   IF      WK-TITLE-LEN >      2000
                        OR WK-WATCH2-LEN >     300
                        OR WK-PLAYLIST-LEN >   300
                           DISPLAY WK-PGM-NAME " WK-領域 長さオーバー"
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                                   " WK-TITLE-LEN=" WK-TITLE-LEN
                                   " WK-WATCH2-LEN=" WK-WATCH2-LEN
                                   " WK-PLAYLIST-LEN=" WK-PLAYLIST-LEN
                          STOP    RUN
                   END-IF

      *    *** TBL03 SET
      *             PERFORM S022-10     THRU    S022-EX
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** TBL03 SET
       S022-10.

           ADD     1           TO      J1
           IF      J1          >       1000
                   DISPLAY WK-PGM-NAME " TBL03 OVER J1=" J1
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      TBL03-ITEM     (J1)
           MOVE    ZERO        TO      TBL03-ITEM-LEN (J1)
                                       TBL03-CNT      (J1)

           UNSTRING WK-WATCH2
                   DELIMITED BY "/" OR SPACE OR "-uncensored-leak"
                   INTO
                   WK-ITEM1
                   WK-ITEM2
                   WK-ITEM3
                   WK-ITEM4
                   WK-ITEM5 COUNT WK-ITEM5-LEN
                   WK-ITEM6 COUNT WK-ITEM6-LEN
           END-UNSTRING

           IF      WK-ITEM6-LEN =      ZERO
                   MOVE    WK-ITEM5    TO      TBL03-ITEM     (J1)
                   MOVE    WK-ITEM5-LEN TO     TBL03-ITEM-LEN (J1)
           ELSE
                   MOVE    WK-ITEM6    TO      TBL03-ITEM     (J1)
                   MOVE    WK-ITEM6-LEN TO     TBL03-ITEM-LEN (J1)
           END-IF

           IF      TBL03-ITEM-LEN (J1) > 100
                   DISPLAY WK-PGM-NAME " WK-領域 長さオーバー"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                           " TBL03-ITEM-LEN (J1)=" TBL03-ITEM-LEN (J1)
                   STOP    RUN
           END-IF

           MOVE    J1          TO      J1-MAX

           MOVE    J1          TO      J2
           MOVE    TBL03-ITEM-LEN (J1) TO L4

           PERFORM VARYING J1 FROM 1 BY 1
                   UNTIL J1 = J1-MAX
                   IF      TBL03-ITEM (J2) (1:L4) = 
                           TBL03-ITEM (J1) (1:L4)
                           ADD     1           TO       TBL03-CNT (J1)
                   END-IF
           END-PERFORM
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** TBL01 SET
       S032-10.

           ADD     1           TO      K1
           IF      K1          >       4000
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      TBL01-ITEM     (K1)
                                       TBL01-HEAD     (K1)
           MOVE    ZERO        TO      TBL01-ITEM-LEN (K1)
                                       TBL01-HEAD-LEN (K1)
                                       WK-REL
                                       WK-REL-LEN

           UNSTRING PIN2-REC
                   DELIMITED BY ","
                   INTO
                   TBL01-ITEM (K1) COUNT TBL01-ITEM-LEN (K1)
                   TBL01-HEAD (K1) COUNT TBL01-HEAD-LEN (K1)
                   WK-REL          COUNT WK-REL-LEN
           END-UNSTRING

           IF      TBL01-ITEM-LEN (K1) > 300
                OR TBL01-HEAD-LEN (K1) > 300
                OR WK-REL-LEN >        4
                OR WK-REL-LEN =        ZERO
                   DISPLAY WK-PGM-NAME " WK-領域 長さオーバー"
                           " WK-PIN2-CNT=" WK-PIN2-CNT
                           " TBL01-ITEM-LEN (K1)=" TBL01-ITEM-LEN (K1)
                           " TBL01-HEAD-LEN (K1)=" TBL01-HEAD-LEN (K1)
                           " WK-REL-LEN=" WK-REL-LEN
                   STOP    RUN
           END-IF

           IF      WK-REL      =       ZERO
                   MOVE    K1          TO      TBL01-NO (K1)
           ELSE
                   MOVE    K1          TO      TBL01-NO (K1)
                   COMPUTE TBL01-NO (K1) = TBL01-NO (K1) - WK-REL
           END-IF

           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** READ PRM2
       S040-10.

           READ    PRM2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PRM2-EOF
               NOT AT END
                   ADD     1           TO      WK-PRM2-CNT
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** TBL02 SET
       S042-10.

           ADD     1           TO      M1
           IF      M1          >       100
                   DISPLAY WK-PGM-NAME " TBL02 OVER M1=" M1
                   STOP    RUN
           END-IF

           MOVE    PRM2-REC    TO      TBL02-ITEM     (M1)
           MOVE    WK-PRM2-LEN TO      TBL02-ITEM-LEN (M1)

           MOVE    M1          TO      M1-MAX
           .
       S042-EX.
           EXIT.

      *    *** 画像サムネイル付加 YOUTUBE
       S100-10.

      *    *** PIO1-F 上限 9,999,999件までとする
           ADD     1           TO      K2
           IF      K2          >       9999999
                   DISPLAY WK-PGM-NAME " PIO1-F OVER K2=" K2
                   STOP    RUN
           END-IF


           MOVE    SPACE       TO      WK-WATCH
           MOVE    ZERO        TO      WK-WATCH-LEN
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:32) =
                         "https://www.youtube.com/watch?v="

                   EVALUATE TRUE
                       WHEN PIN1-REC (J:7) =    ",https:"
                           CONTINUE
                       WHEN PIN1-REC (J:1) =    ","
                           MOVE    "."         TO      PIN1-REC (J:1)
                       WHEN PIN1-REC (J:1) =    '"'
                           MOVE    SPACE       TO      PIN1-REC (J:1)
      *                 WHEN PIN1-REC (J:4) =    '""",' 
      *                     MOVE    '"  ,'      TO      PIN1-REC (J:4)
      *                     ADD     3           TO      J
      *                 WHEN PIN1-REC (J:2) =    '",' 
      *                     MOVE    ' ,'        TO      PIN1-REC (J:2)
      *                     ADD     1           TO      J
      *                 WHEN PIN1-REC (J:3) =    ' ""' 
      *                     MOVE    '  "'       TO      PIN1-REC (J:3)
      *                     ADD     2           TO      J
      *                 WHEN PIN1-REC (J:3) =    '"" ' 
      *                     MOVE    '"  '       TO      PIN1-REC (J:3)
      *                     ADD     2           TO      J
      *                 WHEN PIN1-REC (J:1) =    '"' 
      *                  AND ( PIN1-REC (J + 1:1) >= X"E0" AND <= X"E9" )
      *                     MOVE    SPACE       TO      PIN1-REC (J:1)
      *                     ADD     1           TO      J
      *    *** 、=>．
      *                 WHEN PIN1-REC (J:3) =    X"E38081" 
      *                     MOVE    X"EFBC8E"   TO      PIN1-REC (J:3)
      *                     ADD     2           TO      J
                   END-EVALUATE

                   IF      J + 32      >       WK-PIN1-LEN
                           DISPLAY WK-PGM-NAME " www.youtube.com/watch"
                                   " 無エラー"
                           DISPLAY WK-PGM-NAME " WK-PIN1-CNT="
                                   WK-PIN1-CNT
                           STOP    RUN
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      WK-REC
                                       PIO1-REC
           MOVE    ZERO        TO      PIO1-NO-IDX
                                       PIO1-NO (1)
                                       PIO1-NO (2)
                                       PIO1-NO (3)
                                       PIO1-NO (4)
                                       PIO1-NO (5)
                                       PIO1-NO (6)
                                       PIO1-NO (7)
                                       PIO1-NO (8)
                                       PIO1-NO (9)
                                       PIO1-NO (10)
                                       PIO1-NO (11)
                                       PIO1-NO (12)
                                       PIO1-NO (13)
                                       PIO1-NO (14)
                                       PIO1-NO (15)
           MOVE    ZERO        TO      WK-T-LEN
           UNSTRING PIN1-REC
                   DELIMITED BY "&t=" OR "&amp" OR ",,     "
      *             DELIMITED BY ","
                   INTO
                   WK-REC     COUNT    WK-T-LEN
           END-UNSTRING

           MOVE    WK-T-LEN    TO      WK-PIN1-LEN

           MOVE    WK-PIN1-LEN TO      I
           ADD     1           TO      I
           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

      *    *** TEST103.大海去旅行.PIN1 の１件目
      *    *** 1400年?史的?空寺，整个寺?建在?崖峭壁上，古人是怎?做到的？【大海去旅行】,https://www.youtube.com/watch?v=_moDYerE1T0,,
      *    *** ,,はREAD直後にセット                                                  |<=J  https のhの位置                     |<=WK-PIN1-LEN
      *    ***                                                                       <= 32 バイト                  =>

      *    *** 
      *    *** WK-WATCH-LEN はhttps://www.youtube.com/watch?v=　以降の文字数
      *    *** - 31 https のhの位置の為、1桁減らす
           COMPUTE WK-WATCH-LEN = WK-PIN1-LEN - J - 31
           MOVE    PIN1-REC (J + 32:WK-WATCH-LEN)
                               TO      WK-WATCH

           MOVE    "https://i.ytimg.com/vi/" TO WK-REC (I:23)
           ADD     23          TO      I

      *    *** https://i.ytimg.com/vi/GCzl5-_Anss/maxresdefault.jpg 1280*720
      *    *** https://i.ytimg.com/vi/GCzl5-_Anss/sddefault.jpg 640*480
      *    *** https://i.ytimg.com/vi/GCzl5-_Anss/hqdefault.jpg 480*360

           IF      WK-WATCH-LEN NOT =  ZERO
                   MOVE    WK-WATCH    TO      WK-REC (I:WK-WATCH-LEN)
                   ADD     WK-WATCH-LEN TO     I
                   MOVE    "/hqdefault.jpg" TO WK-REC (I:14)
                   ADD     14          TO      I
           END-IF

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

           MOVE    WK-PLAYLIST TO      WK-REC (I:WK-PLAYLIST-LEN)
           ADD     WK-PLAYLIST-LEN TO  I

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

           MOVE    "N"         TO      SW-YES
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL PIN1-REC (K:6) = ",https"
      *                OR SW-YES = "Y"

                   PERFORM VARYING K1 FROM 1 BY 1
                           UNTIL K1 > K1-MAX

                       IF      TBL01-ITEM-LEN (K1) NOT = ZERO
                           AND PIN1-REC        (K:TBL01-ITEM-LEN (K1)) =
                               TBL01-ITEM (K1) (1:TBL01-ITEM-LEN (K1))
                           AND PIO1-NO-IDX <= 14

                           EVALUATE TRUE
                             WHEN PIO1-NO-IDX = ZERO
                               MOVE    1           TO      PIO1-NO-IDX
                               MOVE    TBL01-NO (K1) TO    PIO1-NO (1)
                               MOVE    "Y"         TO      SW-YES
                               ADD     1           TO
                                       TBL01-CNT (TBL01-NO (K1))
                             WHEN PIO1-NO-IDX = 1
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    2           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (2)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 2
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    3           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (3)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 3
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    4           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (4)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 4
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    5           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (5)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 5
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    6           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (6)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 6
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    7           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (7)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 7
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    8           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (8)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 8
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    9           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (9)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 9
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    10          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(10)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 10
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    11          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(11)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 11
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    12          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(12)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 12
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    13          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(13)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 13
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                    OR PIO1-NO (13)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    14          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(14)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 14
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                    OR PIO1-NO (13)    =   TBL01-NO (K1)
                                    OR PIO1-NO (14)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    15          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(15)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-PERFORM
           END-PERFORM

      *    *** テーブルヒットしない時、テーブルの最後、その他にする
      *    *** PIN1 最後にその他,,0 を入れる
           IF      SW-YES      =       "N"
                   MOVE    1           TO      PIO1-NO-IDX
                   MOVE    K1-MAX      TO      PIO1-NO (1)
                   ADD     1           TO      TBL01-CNT (K1-MAX)
           END-IF

      *    *** K2 は出力したＫＥＹにする
           MOVE    K2          TO      PIO1-KEY
           MOVE    WK-REC      TO      PIO1-DATA
           WRITE   PIO1-REC
                   INVALID KEY 
                   DISPLAY WK-PGM-NAME " PIO1-F WRITE ERROR KEY="
                           PIO1-KEY
                   STOP    RUN
           END-WRITE

           ADD     1           TO      WK-PIO1W-CNT
           MOVE    K2          TO      K2-MAX

          .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 %ヘッダー出力
       S110-10.

           MOVE    "% "        TO      POT1-REC (1:2)
           MOVE    PRM1-REC    TO      POT1-REC (3:)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING M1 FROM 1 BY 1
                   UNTIL M1 > M1-MAX
                   IF      PRM1-REC        (1:TBL02-ITEM-LEN (M1))
                         = TBL02-ITEM (M1) (1:TBL02-ITEM-LEN (M1))
                           MOVE    "Y"         TO      SW-SEARCH
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** PIO1-F => WRITE POT1 に変更
       S120-10.

           MOVE    K2          TO      PIO1-KEY
           READ    PIO1-F
                   INVALID KEY
                   DISPLAY WK-PGM-NAME " PIO1-F RAED ERROR KEY="
                           PIO1-KEY
                   STOP    RUN
           END-READ
           ADD     1           TO      WK-PIO1R-CNT

           PERFORM VARYING K3 FROM 1 BY 1
                   UNTIL K3 > PIO1-NO-IDX
                   IF      K1          =       PIO1-NO (K3)
                           MOVE    PIO1-DATA   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** 画像サムネイル付加 MISSAV
       S200-10.

      *    *** PIO1-F 上限 9,999,999件までとする
           ADD     1           TO      K2
           IF      K2          >       9999999
                   DISPLAY WK-PGM-NAME " PIO1-F OVER K2=" K2
                   STOP    RUN
           END-IF

           MOVE    ZERO        TO      WK-UNCEN
           INSPECT PIN1-REC TALLYING
                   WK-UNCEN FOR ALL "uncensored-leak"

           MOVE    WK-TITLE    TO      WK-TITLE2
           MOVE    WK-TITLE-LEN TO     WK-TITLE2-LEN

      *    *** " => SPACE 対応
           IF      WK-TITLE2 (1:1) =   SPACE
                   ADD     -1          TO      WK-TITLE2-LEN
           END-IF

           MOVE    "N"         TO      SW-END
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL SW-END = "Y"
                      OR I > WK-TITLE2-LEN
      *    *** 【MGS
                   IF      WK-TITLE2 (I:6) =   X"E380904D4753"
      *    *** 【限定
                        OR WK-TITLE2 (I:9) =   
                           X"E38090E99990E5AE9A"
                           MOVE    "Y"         TO      SW-END
                           COMPUTE WK-TITLE2-LEN = I - 1
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-END
           MOVE    SPACE       TO      WK-TITLE
           PERFORM VARYING I FROM WK-TITLE2-LEN BY -1
                   UNTIL SW-END = "Y"
                      OR I < 1

                   IF      WK-TITLE2 (I:1) =   SPACE
                           MOVE    "Y"         TO      SW-END
                           MOVE    WK-TITLE2 (1:I) TO
                                   WK-TITLE  (1:I)

                           MOVE    "<br><br>"      TO
                                   WK-TITLE  (I:8)
                           COMPUTE L  = WK-TITLE2-LEN - I
                           COMPUTE I2 = I + 8
                           COMPUTE WK-TITLE-LEN = I + 8

      *    *** 女優名セット
                           MOVE    WK-TITLE2 (I + 1:L) TO
                                   WK-TITLE  (I2:L)
                           COMPUTE I2 = I2 + L
                           COMPUTE WK-TITLE-LEN = WK-TITLE-LEN 
                                                + L

                           IF      WK-UNCEN    NOT =   ZERO

                                   MOVE    "<br><br>"      TO
                                           WK-TITLE  (I2:8)
                                   COMPUTE I2 = I2 + 8
                                   COMPUTE WK-TITLE-LEN =
                                          WK-TITLE-LEN + 8
      *    *** 無修正
                                   MOVE    X"E784A1E4BFAEE6ADA3" TO
                                          WK-TITLE (I2:9)

                                   COMPUTE I2 = I2 + 9
                                   COMPUTE WK-TITLE-LEN =
                                           WK-TITLE-LEN + 9
                           END-IF
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      WK-WATCH
           MOVE    ZERO        TO      WK-WATCH-LEN
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:17) =
                         "https://missav.ai"

                   EVALUATE TRUE
                       WHEN PIN1-REC (J:7) =    ",https:"
                           CONTINUE
                       WHEN PIN1-REC (J:1) =    ","
                           MOVE    "."         TO      PIN1-REC (J:1)
                       WHEN PIN1-REC (J:1) =    '"'
                           MOVE    SPACE       TO      PIN1-REC (J:1)
                   END-EVALUATE

                   IF      J + 17      >       WK-PIN1-LEN
                           DISPLAY WK-PGM-NAME " https://missav.ai"
                                   " 無エラー"
                           DISPLAY WK-PGM-NAME " WK-PIN1-CNT="
                                   WK-PIN1-CNT
                           STOP    RUN
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      WK-REC
                                       PIO1-REC
           MOVE    ZERO        TO      PIO1-NO-IDX
                                       PIO1-NO (1)
                                       PIO1-NO (2)
                                       PIO1-NO (3)
                                       PIO1-NO (4)
                                       PIO1-NO (5)
                                       PIO1-NO (6)
                                       PIO1-NO (7)
                                       PIO1-NO (8)
                                       PIO1-NO (9)
                                       PIO1-NO (10)
                                       PIO1-NO (11)
                                       PIO1-NO (12)
                                       PIO1-NO (13)
                                       PIO1-NO (14)
                                       PIO1-NO (15)
           MOVE    1           TO      I

           MOVE    WK-TITLE    TO      WK-REC (I:WK-TITLE-LEN)
           ADD     WK-TITLE-LEN TO     I

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

           MOVE    WK-WATCH2   TO      WK-REC (I:WK-WATCH2-LEN)
           ADD     WK-WATCH2-LEN TO    I

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

      *    *** MUDR-042 あの日からずっと…。 緊縛調教中出しされる制服美少女 ひなみれん,
      *    *** https://missav.ai/dm59/ja/mudr-042
      *    *** サムネイル画像 NOW PRINTING の為、強制置換え

           IF      WK-WATCH2 (1:45) = 
                   "https://missav.ai/ja/mudr-042-uncensored-leak"
                    MOVE
                 "https://img.supjav.com/images/2026/02/mudr042pl.jpg ," 
                                       TO      WK-REC (I:53)
                    ADD     53         TO      I
           ELSE
      *    *** IMG SET
                   PERFORM S220-10     THRU    S220-EX
           END-IF

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

           PERFORM VARYING L3 FROM 1 BY 1
                   UNTIL PIN1-REC (L3:1) = SPACE
                   CONTINUE
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM VARYING K FROM L3 BY 1
      *    *** 無修正のジャパリを出力したい為、タイトルに無修正が含まれない
      *    *** 為、行最後までチェックし、httpsの中の”uncensored-leak”
      *    *** をチェックするため
      *             UNTIL PIN1-REC (K:6) = ",https"
                   UNTIL K > WK-PIN1-LEN

                   PERFORM VARYING K1 FROM 1 BY 1
                           UNTIL K1 > K1-MAX

                       IF      TBL01-ITEM-LEN (K1) NOT = ZERO
                           AND PIN1-REC        (K:TBL01-ITEM-LEN (K1)) =
                               TBL01-ITEM (K1) (1:TBL01-ITEM-LEN (K1))
                           AND PIO1-NO-IDX <= 14

                           EVALUATE TRUE
                             WHEN PIO1-NO-IDX = ZERO
                               MOVE    1           TO      PIO1-NO-IDX
                               MOVE    TBL01-NO (K1) TO    PIO1-NO (1)
                               MOVE    "Y"         TO      SW-YES
                               ADD     1           TO
                                       TBL01-CNT (TBL01-NO (K1))
                             WHEN PIO1-NO-IDX = 1
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    2           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (2)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 2
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    3           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (3)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 3
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    4           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (4)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 4
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    5           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (5)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 5
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    6           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (6)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 6
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    7           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (7)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 7
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    8           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (8)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 8
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    9           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (9)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 9
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    10          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(10)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 10
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    11          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(11)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 11
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    12          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(12)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 12
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    13          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(13)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 13
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                    OR PIO1-NO (13)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    14          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(14)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 14
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                    OR PIO1-NO (5)     =   TBL01-NO (K1)
                                    OR PIO1-NO (6)     =   TBL01-NO (K1)
                                    OR PIO1-NO (7)     =   TBL01-NO (K1)
                                    OR PIO1-NO (8)     =   TBL01-NO (K1)
                                    OR PIO1-NO (9)     =   TBL01-NO (K1)
                                    OR PIO1-NO (10)    =   TBL01-NO (K1)
                                    OR PIO1-NO (11)    =   TBL01-NO (K1)
                                    OR PIO1-NO (12)    =   TBL01-NO (K1)
                                    OR PIO1-NO (13)    =   TBL01-NO (K1)
                                    OR PIO1-NO (14)    =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    15          TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO(15)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-PERFORM
           END-PERFORM

      *    *** テーブルヒットしない時、テーブルの最後、その他にする
      *    *** PIN1 最後にその他,,0 を入れる
           IF      SW-YES      =       "N"
                   MOVE    1           TO      PIO1-NO-IDX
                   MOVE    K1-MAX      TO      PIO1-NO (1)
                   ADD     1           TO      TBL01-CNT (K1-MAX)
           END-IF

      *    *** K2 は出力したＫＥＹにする
           MOVE    K2          TO      PIO1-KEY
           MOVE    WK-REC      TO      PIO1-DATA
           WRITE   PIO1-REC
                   INVALID KEY 
                   DISPLAY WK-PGM-NAME " PIO1-F WRITE ERROR KEY="
                           PIO1-KEY
                   STOP    RUN
           END-WRITE

           ADD     1           TO      WK-PIO1W-CNT
           MOVE    K2          TO      K2-MAX

          .
       S200-EX.
           EXIT.

      *    *** IMG SET
       S220-10.

      *    *** 2025.01.09 アドレス変わった 
      *    *** https://missav.ws/ja/fc2-ppv-3865704 <= https://missav.com/xxx
      *    *** https://fourhoi.com/fc2-ppv-3865704/cover-n.jpg => https://fivetiu.com/xxx
      *    *** 変更後　https://fivetiu.com/ でもサムネイル表示されるので、
      *    *** 修正しない

      *    *** IMG セットに規則性あったので、テーブルセットを止める
      *    *** https://fivetiu.com/miae-045/cover-n.jpg

      *     MOVE    "https://fivetiu.com/"
           MOVE    "https://fourhoi.com/"
                               TO      WK-REC (I:20)
           ADD     20          TO      I

           MOVE    ZERO        TO      L2
           MOVE    "N"         TO      SW-SHARP

           PERFORM VARYING L FROM WK-WATCH2-LEN BY -1
                   UNTIL WK-WATCH2 (L:1) = "/"
                      OR L < 1
                   ADD     1           TO      L2
           END-PERFORM

      *    *** miae-045 SET
           MOVE    WK-WATCH2 (L + 1:L2) TO WK-REC (I:L2)
           ADD     L2          TO      I

           MOVE    "/cover-n.jpg" TO   WK-REC (I:12)
           ADD     12          TO      I

           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I
           .
       S220-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PRM2-F
                   PIN1-F
                   PIN2-F
                   PIO1-F
                   POT1-F

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
           MOVE    WK-PRM2-CNT TO      WK-PRM2-CNT-E
           DISPLAY WK-PGM-NAME " PRM2 件数 = " WK-PRM2-CNT-E
                   " (" WK-PRM2-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIO1R-CNT TO     WK-PIO1R-CNT-E
           DISPLAY WK-PGM-NAME " PIO1R 件数= " WK-PIO1R-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-PIO1W-CNT TO     WK-PIO1W-CNT-E
           DISPLAY WK-PGM-NAME " PIO1W 件数= " WK-PIO1W-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
