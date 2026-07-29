      *    *** Youtube html ゆいかおり 解析
      *    *** TEST66.cbl => TEST69.cbl
      *    *** 
      *    *** JOB TEST69
      *    ***        |
      *    ***     TEST68
      *    ***     
      *    *** C.TEST69.bat で実行する
      *    *** 
      *    *** Windows Power Shell ならエラー１個所だった
      *    *** >CHCP 65001
      *    *** >.\C.TEST69.BAT
      *    *** 
      *    *** 連続して実行すると、プログラム名等が読み込まれず、異常終了
      *    *** する、ＣＴＲＬ＋Ｃ、ＣＴＲＬ＋Ｖでコマンドプロンプトで
      *    *** 実行すると多少、上手く実行される、コマンドにＵＴＦ８の漢字
      *    *** が含まれている為、ダメなようだ、去年位作ったときは、
      *    *** このようなエラーは無かった
      *    *** 
      *    *** 通常、.batはＳＪＩＳで実行するようだ、Ｙｏｕｔｕｂｅの
      *    *** データ利用しているので、ACCEPTで入力するファイル名、ＵＴＦ８
      *    *** で指定している

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST69.

       ENVIRONMENT             DIVISION.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ ＵＴＦ８
      *    *** TEST10.POT1 => TEST69.PIN1
      *    *** youtube_ゆいかおり.py 作成したhtml 等をインプットする
      *    *** import requests
      *    *** res = requests.get('https://www.youtube.com/results?search_query=%E3%82%86%E3%81%84%E3%81%8B%E3%81%8A%E3%82%8A')
      *    *** with open('youtube.ゆいかおり.html','w',encoding='utf8') as file:
      *    ***     file.write(res.text)

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE WATCH データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE IMG データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE WATCH,CHANNEL データ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** RESULTS 一時退避 データ
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PLAYLIST 一時退避 データ
       SELECT POT5-F           ASSIGN   WK-POT5-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** WATCHLIST 一時退避 データ
       SELECT POT6-F           ASSIGN   WK-POT6-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** MIXLIST 一時退避 データ
       SELECT POT7-F           ASSIGN   WK-POT7-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SHORTLIST 一時退避 データ
       SELECT POT8-F           ASSIGN   WK-POT8-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
      *    *** レコード長制限不明、この長さでも処理出来る
      *    *** 2ギガがＭＡＸか？ 2*1024*1024=2,097,152
       01  PIN1-REC.
      *     03  FILLER          PIC  X(500000).
           03  FILLER          PIC  X(2000000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(10000).

       FD  POT3-F.
       01  POT3-REC.
           03  FILLER          PIC  X(10000).

       FD  POT4-F.
       01  POT4-REC.
           03  FILLER          PIC  X(10000).

       FD  POT5-F.
       01  POT5-REC.
           03  FILLER          PIC  X(10000).

       FD  POT6-F.
       01  POT6-REC.
           03  FILLER          PIC  X(10000).

       FD  POT7-F.
       01  POT7-REC.
           03  FILLER          PIC  X(10000).

       FD  POT8-F.
       01  POT8-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST69  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST69.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.石原夏織.POT1".
      *    *** Python で作成
      *     03  WK-PIN1-F-NAME  PIC X(032) VALUE "youtube.石原夏織.html".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST69.PIN1".
      *    *** C.TEST69.bat でインプットファイル名指定
      *    *** TEST69 youtube.増田未亜.html
           03  WK-PIN1-F-NAME  PIC  X(100) VALUE "TEST69.XXXX.html".
           03  WK-PIN1-F-NAME-UTF8
                               PIC  X(200) VALUE SPACE.
      *         "youtube.ゆいかおり.html".
      *         "youtube.滝川みう.html".
      *         "youtube.佐藤麗華.html".
      *          SPACE.
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST69.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST69.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST69.POT3".
           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST69.POT4".
           03  WK-POT5-F-NAME  PIC  X(032) VALUE "TEST69.POT5".
           03  WK-POT6-F-NAME  PIC  X(032) VALUE "TEST69.POT6".
           03  WK-POT7-F-NAME  PIC  X(032) VALUE "TEST69.POT7".
           03  WK-POT8-F-NAME  PIC  X(032) VALUE "TEST69.POT8".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT5-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT6-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT7-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT8-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-MAX-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT2    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT5-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT6-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT6-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT7-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT7-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT8-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT8-CNTR    BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-LEN-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT5-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT6-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT6-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT7-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT7-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT8-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT8-CNTR-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHECK-CNT    PIC S9(005) VALUE ZERO.
           03  WK-PLAYLIST-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCHLIST-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-MIXLIST-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-SHORTLIST-CNT BINARY-LONG SYNC VALUE ZERO.

           03  WK-HTTPS-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-2-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-FIRST-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-TEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH2-L     BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-TEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-RESULTS-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PLAYLIST-L   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT3-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXTX-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-LABEL-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-LABEL2-L     BINARY-LONG SYNC VALUE ZERO.
           03  WK-LABELX-L     BINARY-LONG SYNC VALUE ZERO.
           03  WK-VIDEOCOUNT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIMPLETEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIMPLETEXT2-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-PQ-L         BINARY-LONG SYNC VALUE ZERO.
           03  WK-KOSHIKI      BINARY-LONG SYNC VALUE ZERO.
           03  WK-OFFICIAL1    BINARY-LONG SYNC VALUE ZERO.
           03  WK-OFFICIAL2    BINARY-LONG SYNC VALUE ZERO.
           03  WK-OFFICIAL3    BINARY-LONG SYNC VALUE ZERO.
           03  WK-OFFICIAL4    BINARY-LONG SYNC VALUE ZERO.
           03  WK-BUF2-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BUF1-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-CONTENT-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CONTENT2-L   BINARY-LONG SYNC VALUE ZERO.
           03  WK-VIDEORENDERER-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-TEXT-OFFICIAL-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-OFFICIAL-L  BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM-CNT     BINARY-LONG SYNC VALUE ZERO.

      *    *** 
           03  WK-HTTPS        PIC  X(1000) VALUE SPACE.
           03  WK-HTTPS-2      PIC  X(1000) VALUE SPACE.
           03  WK-HTTPS-FIRST  PIC  X(1000) VALUE SPACE.
           03  WK-CHANNEL      PIC  X(1000) VALUE SPACE.
           03  WK-CHANNEL-TEXT PIC  X(1000) VALUE SPACE.
           03  WK-WATCH        PIC  X(1000) VALUE SPACE.
           03  WK-WATCH2       PIC  X(1000) VALUE SPACE.
           03  WK-WATCH-TEXT   PIC  X(1000) VALUE SPACE.
           03  WK-PLAYLIST     PIC  X(1000) VALUE SPACE.
           03  WK-RESULTS      PIC  X(1000) VALUE SPACE.
           03  WK-TEXT         PIC  X(1000) VALUE SPACE.
           03  WK-TEXT2        PIC  X(1000) VALUE SPACE.
           03  WK-TEXT3        PIC  X(1000) VALUE SPACE.
           03  WK-TEXTX        PIC  X(1000) VALUE SPACE.
           03  WK-LABEL        PIC  X(1000) VALUE SPACE.
           03  WK-LABEL2       PIC  X(1000) VALUE SPACE.
           03  WK-LABELX       PIC  X(1000) VALUE SPACE.
           03  WK-VIDEOCOUNT   PIC  X(1000) VALUE SPACE.
           03  WK-SIMPLETEXT   PIC  X(1000) VALUE SPACE.
           03  WK-SIMPLETEXT2  PIC  X(1000) VALUE SPACE.
           03  WK-UNST         PIC  X(1000) VALUE SPACE.
           03  WK-CONTENT      PIC  X(1000) VALUE SPACE.
           03  WK-CONTENT2     PIC  X(1000) VALUE SPACE.
           03  WK-NO           PIC  9(002) VALUE ZERO.
           03  WK-PQ           PIC  X(200) VALUE SPACE.
           03  WK-PQ2          PIC  X(200) VALUE SPACE.
           03  WK-QUERY        PIC  X(200) VALUE SPACE.
           03  WK-SJIS         PIC  X(100) VALUE SPACE.
           03  WK-VIDEOIDS     PIC  X(020) VALUE SPACE.
           03  WK-TEXT-OFFICIAL PIC X(1000) VALUE SPACE.
           03  WK-CHANNEL-OFFICIAL PIC X(1000) VALUE SPACE.
           03  WK-SAISEILIST.
      *    *** 再生リストの全体を見る
             05  PIC  X(010) VALUE   X"E5868DE7949FE383AAE3".
             05  PIC  X(023) VALUE
             X"82B9E38388E381AEE585A8E4BD93E38292E8A68BE3828B".

      *    *** ＼毎週月曜日・隔週で配信
           03  WK-MAISHUU.
             05    PIC  X(018) VALUE
               X"EFBCBCE6AF8EE980B1E69C88E69B9CE697A5".
             05    PIC  X(018) VALUE
               X"E383BBE99A94E980B1E381A7E9858DE4BFA1".
      *    *** この動画のチャプター数:
           03  WK-KONODOUGA..
             05    PIC  X(017) VALUE
               X"E38193E381AEE58B95E794BBE381AEE383".
             05    PIC  X(017) VALUE
               X"81E383A3E38397E382BFE383BCE695B03A".
      *    *** コラボレーション チャンネル
           03  WK-KORABO.
             05               PIC  X(020) VALUE
                 X"E382B3E383A9E3839CE383ACE383BCE382B7E383".
             05               PIC  X(020) VALUE
                 X"A7E383B320E38381E383A3E383B3E3838DE383AB".
           03  WK-HISTORY.
             05  PIC  X(030) VALUE "https://i.ytimg.com/vi/Fj7zFNB".
             05  PIC  X(030) VALUE "lyQE/hq720.jpg?sqp=-oaymwEjCOg".
             05  PIC  X(030) VALUE "CEMoBSFryq4qpAxUIARUAAAAAGAElA".
             05  PIC  X(030) VALUE "ADIQj0AgKJDeAE=&rs=AOn4CLBeeOH".
             05  PIC  X(023) VALUE "8mepWANZ1m0Ro8ybcD9wruw".


           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  L5              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  N1              BINARY-LONG SYNC VALUE ZERO.
           03  N2              BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  S               BINARY-LONG SYNC VALUE ZERO.
           03  S-MAX           BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
      *     03  SW-TITLE        PIC  X(001) VALUE "N".
      *     03  SW-TITLE2       PIC  X(001) VALUE "N".
           03  SW-HTTPS        PIC  X(001) VALUE "N".
           03  SW-HTTPS-FIRST  PIC  X(001) VALUE "N".
           03  SW-CHANNEL      PIC  X(001) VALUE "N".
           03  SW-WATCH        PIC  X(001) VALUE "N".
           03  SW-PLAYLIST     PIC  X(001) VALUE "N".
           03  SW-TEXT         PIC  X(001) VALUE "N".
           03  SW-LABEL        PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".
           03  SW-FIRST2       PIC  X(001) VALUE "Y".
           03  SW-NUM          PIC  X(001) VALUE "Y".
           03  SW-KENSAKU      PIC  X(001) VALUE "N".
           03  SW-RESULTS      PIC  X(001) VALUE "N".
           03  SW-DOUGA        PIC  X(001) VALUE "N".
           03  SW-DOUGA2       PIC  X(001) VALUE "N".
           03  SW-MIXLIST      PIC  X(001) VALUE "N".
           03  SW-THUMBNAIL    PIC  X(001) VALUE "N".
           03  SW-WATCHENDPOINT PIC X(001) VALUE "N".
           03  SW-SHORT        PIC  X(001) VALUE "N".
           03  SW-CHANNEL-NAME PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-STOP         PIC  X(001) VALUE "Y".
           03  SW-VIDEORENDERER PIC X(001) VALUE "N".

           03  SW-DEBUG        PIC  X(001) VALUE "N".
      *     03  SW-DEBUG        PIC  X(001) VALUE "Y".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000.
             05  TBL01-WATCH   PIC  X(1000) VALUE HIGH-VALUE.

       01  SAVE-AREA.
      *     03  SV-TEXT         PIC  X(1000) VALUE SPACE.
      *     03  SV-TEXT-L       BINARY-LONG SYNC VALUE ZERO.
           03  SV-HTTPS        PIC  X(1000) VALUE SPACE.
           03  SV-HTTPS-L      BINARY-LONG SYNC VALUE ZERO.
           03  SV-LABEL        PIC  X(1000) VALUE SPACE.
           03  SV-LABEL-L      BINARY-LONG SYNC VALUE ZERO.
           03  SV-CONTENT      PIC  X(1000) VALUE SPACE.
           03  SV-CONTENT-L    BINARY-LONG SYNC VALUE ZERO.
           03  SV-CHANNEL-TEXT PIC  X(1000) VALUE SPACE.
           03  SV-CHANNEL-TEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  SV-WATCH        PIC  X(1000) VALUE SPACE.
           03  SV-VIDEOIDS     PIC  X(020) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN 1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** html 解析2
                   PERFORM S310-10     THRU    S310-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,OPEN PIN1
           PERFORM S013-10     THRU    S013-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** html 解析
                   PERFORM S200-10     THRU    S200-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM



      *    *** POT2
      *    *** OPEN 1 POT2,CLOSE,OPEN
      *     PERFORM S011-10     THRU    S011-EX


      *    *** READ POT2
      *     PERFORM S022-10     THRU    S022-EX

      *     PERFORM UNTIL WK-POT2-EOF = HIGH-VALUE

      *    *** html 解析
      *    *** POT1-REC 分析 => POT3-REC WRITE
      *             MOVE    POT2-REC    TO      POT1-REC

      *                 MOVE    ZERO        TO      WK-POT1-LEN
      *                 PERFORM VARYING N1 FROM 1 BY 10
      *                         UNTIL POT1-REC (N1:10) = SPACE
      *                         CONTINUE
      *                 END-PERFORM
      *                 PERFORM TEST AFTER
      *                         VARYING N2 FROM N1 BY -1
      *                         UNTIL POT1-REC (N2:1) NOT = SPACE
      *                            OR N2 < 1
      *                     IF      N2 >= 1
      *                         AND POT1-REC (N2:1) NOT = SPACE
      *                         MOVE    N2          TO      WK-POT1-LEN
      *                     END-IF
      *                 END-PERFORM

      *             MOVE    WK-POT1-LEN TO      P1
      *             PERFORM S220-10     THRU    S220-EX

      *             MOVE    SPACE       TO      POT1-REC

      *    *** READ POT2
      *             PERFORM S022-10     THRU    S022-EX
      *     END-PERFORM



      *    *** MIXLIST
      *    *** OPEN 5 POT7,CLOSE,OPEN
           PERFORM S018-10     THRU    S018-EX

      *    *** READ POT7
           PERFORM S060-10     THRU    S060-EX

           PERFORM UNTIL WK-POT7-EOF = HIGH-VALUE

      *    *** MIXLIST DATA POT7=>POT3
                   PERFORM S270-10     THRU    S270-EX

      *    *** READ POT7
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM



      *    *** PLAYLIST
      *    *** OPEN 3 POT5,CLOSE,OPEN
           PERFORM S014-10     THRU    S014-EX

      *    *** READ POT5
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-POT5-EOF = HIGH-VALUE

      *    *** PLAYLIST DATA POT5=>POT3
                   PERFORM S250-10     THRU    S250-EX

      *    *** READ POT5
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM



      *    *** SHORTLIST
      *    *** OPEN 5 POT8,CLOSE,OPEN
           PERFORM S019-10     THRU    S019-EX

      *    *** READ POT8
           PERFORM S070-10     THRU    S070-EX

           PERFORM UNTIL WK-POT8-EOF = HIGH-VALUE

      *    *** SHORTLIST DATA POT8=>POT3
                   PERFORM S280-10     THRU    S280-EX

      *    *** READ POT5
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM



      *    *** WATCHLIST
      *    *** OPEN 4 POT6,CLOSE,OPEN
           PERFORM S016-10     THRU    S016-EX

      *    *** READ POT6
           PERFORM S050-10     THRU    S050-EX

           PERFORM UNTIL WK-POT6-EOF = HIGH-VALUE

      *    *** WATCHLIST DATA POT6=>POT3
                   PERFORM S260-10     THRU    S260-EX

      *    *** READ POT5
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM



      *    *** 検索
      *    *** OPEN 2 POT4,CLOSE,OPEN
           PERFORM S012-10     THRU    S012-EX

      *    *** READ POT4
           PERFORM S030-10     THRU    S030-EX

           IF      WK-POT4-EOF NOT =   HIGH-VALUE
                   MOVE    "#   YouTube Search List"
                                       TO      POT3-REC
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF

           PERFORM UNTIL WK-POT4-EOF = HIGH-VALUE

      *    *** RESULTS DATA POT4=>POT3
                   PERFORM S240-10     THRU    S240-EX

      *    *** READ POT4
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM



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

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-PIN1-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PIN1-F-NAME

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PIN1-F 1個まで指定可"
                   STOP    RUN
           END-EVALUATE

      *    *** TEST69 youtube.アイカツ！ミュージックビデオ.html
           MOVE    ZERO        TO      L2

      *    *** HENKAN=SU SJIS => UTF8
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    91          TO      WDE05-BUF1-LEN
           MOVE    200         TO      WDE05-BUF2-LEN
           MOVE    "SU"        TO      WDE05-HENKAN
           MOVE    "AA"        TO      WDE05-MODE
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-PIN1-F-NAME (9:91)
                                       WK-PIN1-F-NAME-UTF8

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL WK-PIN1-F-NAME-UTF8 (J:5) =  '.html'
                   ADD     1           TO      L2
           END-PERFORM

           MOVE    L2          TO      L
           MOVE    WK-PIN1-F-NAME-UTF8 (1:L) TO WK-PQ (1:L)
           MOVE    L           TO      WK-PQ-L

           MOVE    ",,"        TO      WK-PQ (WK-PQ-L + 1:2)
           ADD     2           TO      WK-PQ-L

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F
                               POT2-F
                               POT3-F
                               POT4-F
                               POT5-F
                               POT6-F
                               POT7-F
                               POT8-F

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN 1
       S011-10.

           CLOSE   POT2-F

           OPEN    INPUT       POT2-F
           .
       S011-EX.
           EXIT.

      *    *** OPEN 2
       S012-10.

           CLOSE   POT4-F

           OPEN    INPUT       POT4-F
           .
       S012-EX.
           EXIT.

      *    *** CLOSE,OPEN PIN1
       S013-10.

           CLOSE   PIN1-F

           OPEN    INPUT       PIN1-F
           MOVE    LOW-VALUE   TO      WK-PIN1-EOF
           .
       S013-EX.
           EXIT.

      *    *** OPEN 3
       S014-10.

           CLOSE   POT5-F

           OPEN    INPUT       POT5-F

           IF      WK-PLAYLIST-CNT >   ZERO
                   MOVE    "#   YouTube Play List"
                               TO      POT3-REC
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF
           .
       S014-EX.
           EXIT.

      *    *** OPEN 4
       S016-10.

           CLOSE   POT6-F

           OPEN    INPUT       POT6-F

           IF      WK-WATCHLIST-CNT >   ZERO
                   MOVE    "#   YouTube Watch List"
                               TO      POT3-REC
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF
           .
       S016-EX.
           EXIT.

      *    *** OPEN 5
       S018-10.

           CLOSE   POT7-F

           OPEN    INPUT       POT7-F

           IF      WK-MIXLIST-CNT >   ZERO
                   MOVE    "#   YouTube Mix List"
                               TO      POT3-REC
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF
           .
       S018-EX.
           EXIT.

      *    *** OPEN 5
       S019-10.

           CLOSE   POT8-F

           OPEN    INPUT       POT8-F

           IF      WK-SHORTLIST-CNT >  ZERO
                   MOVE    "#   YouTube Short List"
                                       TO      POT3-REC
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF
           .
       S019-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                   MOVE    "N"         TO      SW-FIRST2
               NOT AT END
                   IF      SW-FIRST2   =       "N"
                           ADD     1           TO      WK-PIN1-CNT
                   END-IF
                   IF      WK-PIN1-LEN >       WK-PIN1-MAX-LEN
                           MOVE    WK-PIN1-LEN TO      WK-PIN1-MAX-LEN
                   END-IF
                   IF      WK-PIN1-LEN >=      2000000
                           DISPLAY WK-PGM-NAME " PIN1-F LEN OVER"
                                   " 2000000 =< "
                                   WK-PIN1-LEN
                           STOP    RUN
                   END-IF
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** READ POT2
       S022-10.

           READ    POT2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT2-EOF
               NOT AT END
                   ADD     1           TO      WK-POT2-CNTR
           END-READ
           .
       S022-EX.
           EXIT.

      *    *** READ POT4
       S030-10.

           READ    POT4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT4-EOF
               NOT AT END
                   ADD     1           TO      WK-POT4-CNTR
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** READ POT5
       S040-10.

           READ    POT5-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT5-EOF
               NOT AT END
                   ADD     1           TO      WK-POT5-CNTR
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** READ POT6
       S050-10.

           READ    POT6-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT6-EOF
               NOT AT END
                   ADD     1           TO      WK-POT6-CNTR
           END-READ
           .
       S050-EX.
           EXIT.

      *    *** READ POT7
       S060-10.

           READ    POT7-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT7-EOF
               NOT AT END
                   ADD     1           TO      WK-POT7-CNTR
           END-READ
           .
       S060-EX.
           EXIT.

      *    *** READ POT8
       S070-10.

           READ    POT8-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT8-EOF
               NOT AT END
                   ADD     1           TO      WK-POT8-CNTR
           END-READ
           .
       S070-EX.
           EXIT.

      *    *** html 解析
       S200-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      P1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   MOVE    I           TO      WK-CHECK-CNT
      *             IF      WK-CHECK-CNT =      ZERO
      *                     DISPLAY "I=" I
      *             END-IF

      *             IF      PIN1-REC(I:1) =     "<" OR ">" OR "[" OR "{"
      *                                      OR "]" OR "}" OR ";"
                   IF      PIN1-REC(I:1) =     "<" OR ">" OR "{"
                                            OR "}" OR ";"
      *                                      OR ","

                       IF    ( POT1-REC (2:8) = "thumbnail"
                OR POT1-REC (1:9) = "link rel="
                OR POT1-REC (2:7) = "content"
                OR POT1-REC (2:3) = "url"
                OR POT1-REC (1:5) = "title"
                OR POT1-REC (2:5) = "title"
                OR POT1-REC (2:4) = "text"
                OR POT1-REC (2:5) = "label"
                OR POT1-REC (2:5) = "tooltip"
                OR POT1-REC (2:5) = "entityId"
                OR POT1-REC (2:10) = "simpletext"
                OR POT1-REC (3:10) = "simpletext"
                OR POT1-REC (3:13) = "watchEndpoint"
                OR POT1-REC (1:16) =  "], videoCount : "
                OR POT1-REC (1:11) = " videoId : "
                OR POT1-REC (1:16) = " videoRenderer :"
                 )
                               WRITE   POT2-REC    FROM    POT1-REC
                               ADD     1           TO      WK-POT2-CNT
                       END-IF

                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    ZERO        TO      WK-POT1-LEN
                       PERFORM VARYING N1 FROM 1 BY 10
                               UNTIL POT1-REC (N1:10) = SPACE
                               CONTINUE
                       END-PERFORM

                       PERFORM TEST AFTER
                               VARYING N2 FROM N1 BY -1
                               UNTIL POT1-REC (N2:1) NOT = SPACE
                                  OR N2 < 1
                           IF      N2 >= 1
                               AND POT1-REC (N2:1) NOT = SPACE
                               MOVE    N2          TO      WK-POT1-LEN
                           END-IF
                       END-PERFORM

      *    *** POT1-REC 分析 => POT3-REC WRITE
                       PERFORM S220-10     THRU    S220-EX

                       MOVE    SPACE       TO      POT1-REC
                       MOVE    ZERO        TO      P1
                                                   L
                   ELSE

                       ADD     1           TO      P1
                       EVALUATE TRUE
      *    *** 韓国アイドル対応 
      *    *** ITZY \"WANNABE\" M/V
                           WHEN PIN1-REC(I:2) = '\"'
                               MOVE    '"'         TO    POT1-REC (P1:1)
                               ADD     1           TO      I

                           WHEN PIN1-REC(I:6) = '\u0026'
                               MOVE    "&"         TO    POT1-REC (P1:1)
                               ADD     5           TO      I

      *                     WHEN PIN1-REC(I:6) = '"PQ":"'
      *    *** 実行しないようにする
                           WHEN PIN1-REC(I:5) = "XXXXX"
                               MOVE    ZERO        TO      L2
                               ADD     I 6         GIVING  J2
                               PERFORM VARYING J FROM J2 BY 1
                                   UNTIL PIN1-REC (J:2) =  '",'
                                       ADD     1           TO      L2
                               END-PERFORM
                               MOVE    L2        TO      L
                               MOVE    PIN1-REC(J2:L) TO WK-PQ (1:L)
                               MOVE    ",,"      TO      WK-PQ (L + 1:2)
                               ADD     L 2       GIVING  WK-PQ-L

      *    *** 登録チャンネルの再生リストの時、query=はセットされていない
      *    *** PQ : もない
      *    *** WK-PQ セット出来ない query=から、セット
      *                     WHEN PIN1-REC(I:45) = 

      *                   'https://www.youtube.com/results?search_query='
      *                      AND WK-PQ (1:3) = SPACE
      *    *** 実行しないようにする
                           WHEN PIN1-REC(I:5) = "XXXXX"
                               MOVE    ZERO        TO      L2
                               MOVE    SPACE       TO      WK-QUERY
                               ADD     I 45        GIVING  J2
                               PERFORM VARYING J FROM J2 BY 1
                                   UNTIL PIN1-REC (J:2) =  '",'
      *                                OR PIN1-REC (J:1) = "+"
                                       ADD     1           TO      L2
                               END-PERFORM
                               MOVE    L2        TO      L
                               MOVE    PIN1-REC(J2:L)
                                                 TO      WK-QUERY
      *    *** query=%XX%YY... => XY 変換
                               CALL    "DECODE02" USING  WK-QUERY
                                                         WK-BUF1-L
                                                         WK-PQ
                                                         WK-BUF2-L
                               INSPECT WK-PQ REPLACING ALL "+" BY SPACE
                                                       ALL "," BY "."
                               MOVE    WK-BUF2-L TO      L
                               MOVE    ",,"      TO      WK-PQ (L + 1:2)
                               ADD     L 2       GIVING  WK-PQ-L

      *                     WHEN PIN1-REC(I:50) = 
      *    *** 石原夏織
      *    *** 立川絢香 対応 \u003d は =
      *              'https://www.youtube.com/results?search_query\u003d'
      *                      AND WK-PQ (1:3) = SPACE
      *    *** 実行しないようにする
                           WHEN PIN1-REC(I:5) = "XXXXX"
                               MOVE    ZERO        TO      L2
                               MOVE    SPACE       TO      WK-QUERY
                               ADD     I 50        GIVING  J2
                               PERFORM VARYING J FROM J2 BY 1
                                   UNTIL PIN1-REC (J:2) =  '",'
      *                                OR PIN1-REC (J:1) = "+"
                                       ADD     1           TO      L2
                               END-PERFORM
                               MOVE    L2        TO      L
                               MOVE    PIN1-REC(J2:L)
                                                 TO      WK-QUERY
      *    *** query=%XX%YY... => XY 変換
                               CALL    "DECODE02" USING  WK-QUERY
                                                         WK-BUF1-L
                                                         WK-PQ
                                                         WK-BUF2-L
                               INSPECT WK-PQ REPLACING ALL "+" BY SPACE
                                                       ALL "," BY "."

                               MOVE    WK-BUF2-L TO      L
                               MOVE    ",,"      TO      WK-PQ (L + 1:2)
                               ADD     L 2       GIVING  WK-PQ-L

                           WHEN PIN1-REC(I:1) NOT = '"'
                               MOVE    PIN1-REC (I:1) TO POT1-REC (P1:1)

                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
                   END-IF

           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** WRITE POT3
       S210-10.

      *    *** テンキーまたはがあった時動画削除する
      *    *** YouTube ミュージック ビデオ.PLAYLIST 最後の動画、テキスト
      *    *** いっぱい存在するため
           IF      WK-LABEL (1:21) = 
                   X"E38386E383B3E382ADE383BCE381BEE3819FE381AF"
                   GO  TO  S210-20
           END-IF
      *    *** SHORT nnn回視聴 クリアーする、ズレる為
           IF      SW-SHORT    =       "Y"
                   MOVE     SPACE      TO      WK-SIMPLETEXT
                   MOVE     ZERO       TO      WK-SIMPLETEXT-L
           END-IF

      *    *** 長さ WK-TEXT-L で内容同じかチェックし、同じならTEXTをクリアー
           IF      WK-TEXT (1:1) NOT = SPACE
               AND WK-TEXT (1:WK-TEXT-L) = WK-LABEL (1:WK-TEXT-L)
                   MOVE    SPACE       TO      WK-TEXT
                   MOVE    ZERO        TO      WK-TEXT-L
           END-IF

           IF      SV-CHANNEL-TEXT (1:1) =   SPACE
               AND WK-CHANNEL-TEXT (1:1) NOT = SPACE
                   MOVE    WK-CHANNEL-TEXT TO    SV-CHANNEL-TEXT
                   MOVE    WK-CHANNEL-TEXT-L TO  SV-CHANNEL-TEXT-L
           END-IF

      *    *** 検索時、チャンネル表示しない
           IF      SW-RESULTS  =       "Y"
                   MOVE    "N"         TO      SW-CHANNEL
                   MOVE    SPACE       TO      WK-CHANNEL
                                               WK-CHANNEL-TEXT
                   MOVE    ZERO        TO      WK-CHANNEL-L
                                               WK-CHANNEL-TEXT-L
                   IF      WK-HTTPS (1:1) =    SPACE
                           MOVE    WK-HTTPS-FIRST TO   WK-HTTPS
                           MOVE    WK-HTTPS-FIRST-L TO WK-HTTPS-L
                           MOVE    "Y"         TO      SW-HTTPS
                   END-IF
           END-IF

           MOVE    SPACE       TO      POT3-REC

           IF      SW-FIRST    =       "Y"

                   MOVE    "N"         TO      SW-FIRST

                   IF      WK-TEXT-OFFICIAL (1:1) NOT = SPACE
                           MOVE    WK-TEXT-OFFICIAL TO WK-CHANNEL-TEXT
                           MOVE    WK-TEXT-OFFICIAL-L TO 
                                   WK-CHANNEL-TEXT-L
                   END-IF
                   IF      WK-CHANNEL-OFFICIAL (1:1) NOT = SPACE
                           MOVE    WK-CHANNEL-OFFICIAL TO WK-CHANNEL
                           MOVE    WK-CHANNEL-OFFICIAL-L TO WK-CHANNEL-L
                   END-IF

                   MOVE    "%"         TO      POT3-REC (1:1)
                   MOVE    "YouTube "  TO      POT3-REC (2:8)

                   IF      WK-PQ-L     =       ZERO
                       MOVE    ",,"        TO      POT3-REC (10:2)
                       MOVE    2           TO      WK-PQ-L
                   ELSE
      *                 INSPECT WK-PQ (1:WK-PQ-L - 2) 
      *                         REPLACING ALL "," BY "."
                       MOVE    WK-PQ       TO      POT3-REC (10:WK-PQ-L)
                   END-IF

                   COMPUTE K = WK-PQ-L + 10
                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)

                   ADD     WK-HTTPS-FIRST-L  TO      K
                   MOVE    " ,"        TO      POT3-REC (K:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT

      *    *** OFFICIAL CHECK
                   PERFORM S230-10     THRU    S230-EX

                   IF      WK-KOSHIKI  =       ZERO
                       AND WK-OFFICIAL1 =      ZERO
                       AND WK-OFFICIAL2 =      ZERO
                       AND WK-OFFICIAL3 =      ZERO
                       AND WK-OFFICIAL4 =      ZERO
                       MOVE    "#   YouTube Top List"
                                       TO      POT3-REC
                   ELSE
                       MOVE    "#   YouTube Official Channel"
                                       TO      POT3-REC
                   END-IF
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT3-REC (2:2)

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** XXXX YouTube Official Channel
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   IF      WK-CHANNEL (1:1) =   SPACE
                           CONTINUE
                   ELSE
                           IF    WK-CHANNEL-TEXT (1:1) = SPACE
                               IF    WK-TEXT2 (1:1) = SPACE
                                   MOVE    WK-CONTENT  TO
                                           POT3-REC (K:WK-CONTENT-L)
                                   ADD     WK-CONTENT-L TO K
                               ELSE
                                   MOVE    WK-TEXT2  TO
                                           POT3-REC (K:WK-TEXT2-L)
                                   ADD     WK-TEXT2-L TO K
                               END-IF
                           ELSE
                               MOVE    WK-CHANNEL-TEXT TO
                                       POT3-REC (K:WK-CHANNEL-TEXT-L)
                               ADD     WK-CHANNEL-TEXT-L TO K
                           END-IF

                           MOVE    ","         TO      POT3-REC (K:1)
                           ADD     1           TO      K

                           MOVE    "https://www.youtube.com"
                                               TO      POT3-REC (K:23)
                           ADD     23          TO      K

                           MOVE    WK-CHANNEL  TO      
                                   POT3-REC (K:WK-CHANNEL-L)
                           ADD     WK-CHANNEL-L TO     K
                   END-IF

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT

                   IF      WK-PQ (1:3) =       "ych"
                           MOVE    WK-PQ (4:)  TO      WK-PQ2
                           MOVE    WK-PQ2      TO      WK-PQ
                           COMPUTE WK-PQ-L = WK-PQ-L - 3
                   END-IF

      *    *** # : hashtag
      *    *** https://www.youtube.com/hashtag/石原夏織
                   PERFORM S300-10     THRU    S300-EX


                 IF      WK-CHANNEL (1:2) =   "/@"
                      OR WK-CHANNEL (1:8) =   "/channel"
                   IF      WK-CHANNEL (WK-CHANNEL-L - 8:9) = "/featured"
                           COMPUTE WK-CHANNEL-L = WK-CHANNEL-L - 9
                   END-IF
      *    *** VIDEOS
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** 動画
                   MOVE    X"E58B95E794BB" TO POT3-REC (K:6)
                   ADD     6           TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/videos"   TO      POT3-REC (K:7)
                           ADD     7           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** ショート
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** ショート
                   MOVE    X"E382B7E383A7E383BCE38388" TO
                           POT3-REC (K:12)
                   ADD     12          TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/shorts"   TO      POT3-REC (K:7)
                   ADD     7           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** ライブ
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** ライブ
                   MOVE    X"E383A9E382A4E38396" TO
                           POT3-REC (K:9)
                   ADD     9           TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/streams"  TO      POT3-REC (K:8)
                   ADD     8           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** リリース
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** リリース
                   MOVE    X"E383AAE383AAE383BCE382B9" TO
                           POT3-REC (K:12)
                   ADD     12          TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/releases" TO      POT3-REC (K:9)
                   ADD     9           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** 再生リスト
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** 再生リスト
                   MOVE    X"E5868DE7949FE383AAE382B9E38388" TO
                           POT3-REC (K:15)
                   ADD     15          TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/playlists" TO     POT3-REC (K:10)
                   ADD     10          TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT



      *    *** 投稿
                   MOVE    SPACE       TO      POT3-REC

                   MOVE    1           TO      K
                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    WK-HTTPS-FIRST TO
                           POT3-REC (K:WK-HTTPS-FIRST-L)
                   ADD     WK-HTTPS-FIRST-L  TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

      *    *** 投稿
                   MOVE    X"E68A95E7A8BF" TO
                           POT3-REC (K:6)
                   ADD     9           TO      K

                   MOVE    ","         TO      POT3-REC (K:1)
                   ADD     1           TO      K

                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      
                           POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K

                   MOVE    "/posts"    TO      POT3-REC (K:6)
                   ADD     6           TO      K

                   MOVE    " ,"        TO      POT3-REC (K:2)
                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT

                 END-IF
           END-IF

      *    *** 長さ WK-TEXT2-L で内容同じかチェックし、同じならTEXTをクリアー
           IF      WK-TEXT2 (1:1) NOT = SPACE
               AND WK-TEXT2 (1:WK-TEXT2-L) = WK-CONTENT (1:WK-TEXT2-L)
                   MOVE    SPACE       TO      WK-CONTENT
                   MOVE    ZERO        TO      WK-CONTENT-L
           END-IF

           IF      SV-VIDEOIDS NOT =   WK-VIDEOIDS
                   MOVE    SPACE       TO      SV-HTTPS
                                               SV-LABEL
                                               SV-CONTENT
                   MOVE    ZERO        TO      SV-HTTPS-L
                                               SV-LABEL-L
           END-IF

           IF      SW-HTTPS-FIRST =    "Y"
               IF      WK-HTTPS (1:1) =    SPACE
      *    *** 直前のHTTPSセットに変更
      *             MOVE    WK-HTTPS-FIRST TO   WK-HTTPS
      *             MOVE    WK-HTTPS-FIRST-L TO WK-HTTPS-L
                   MOVE    SV-HTTPS    TO      WK-HTTPS
                   MOVE    SV-HTTPS-L  TO      WK-HTTPS-L
                   MOVE    "Y"         TO      SW-HTTPS
               END-IF
           END-IF

           IF      WK-LABEL (1:1) =    SPACE
               AND SV-LABEL (1:1) NOT = SPACE
                   MOVE    SV-LABEL    TO      WK-LABEL
                   MOVE    SV-LABEL-L  TO      WK-LABEL-L
                   MOVE    "Y"         TO      SW-LABEL
           END-IF

           IF      WK-CONTENT (1:1) =    SPACE
               AND SV-CONTENT (1:1) NOT = SPACE
                   MOVE    SV-CONTENT    TO    WK-CONTENT
                   MOVE    SV-CONTENT-L  TO    WK-CONTENT-L
           END-IF

      *    *** PLAYLIST 最終のLABEL と内容違う為、クリアー
      *    *** 再生リストを保存
           IF      WK-LABEL (1:24) =
                   X"E5868DE7949FE383AAE382B9E38388E38292E4BF9DE5AD98"
      *    *** ライブラリから削除
               AND WK-LABEL2 (1:27) =
               X"E383A9E382A4E38396E383A9E383AAE3818BE38289E5898AE999A4"
                   MOVE    SPACE       TO      WK-LABEL
                                               WK-LABEL2
           END-IF

      *    *** 興味なし
           IF      WK-LABEL2 (1:12) =  X"E88888E591B3E381AAE38197"
                   MOVE    WK-LABEL    TO      WK-LABEL2
                   MOVE    WK-LABEL-L  TO      WK-LABEL2-L

      *    *** ライブラリから削除
               IF      WK-LABELX (1:27) =
               X"E383A9E382A4E38396E383A9E383AAE3818BE38289E5898AE999A4"
                   MOVE    SPACE       TO      WK-LABEL
                   MOVE    ZERO        TO      WK-LABEL-L
               ELSE
                   MOVE    WK-LABELX   TO      WK-LABEL
                   MOVE    WK-LABELX-L TO      WK-LABEL-L
               END-IF
           ELSE
               CONTINUE
           END-IF

           IF     WK-CONTENT (1:33) NOT = WK-SAISEILIST

                  IF      WK-LABEL (1:1) =    SPACE
                      AND WK-CONTENT (1:1) NOT = SPACE
                          MOVE    WK-CONTENT  TO      WK-LABEL
                          MOVE    WK-CONTENT-L TO     WK-LABEL-L
                          MOVE    SPACE       TO      WK-CONTENT
                          MOVE    ZERO        TO      WK-CONTENT-L
                  END-IF

                  IF      WK-LABEL2 (1:1) =    SPACE
                      AND WK-CONTENT2 (1:1) NOT = SPACE
                          MOVE    WK-CONTENT2  TO      WK-LABEL2
                          MOVE    WK-CONTENT2-L TO     WK-LABEL2-L
                          MOVE    SPACE       TO      WK-CONTENT2
                          MOVE    ZERO        TO      WK-CONTENT2-L
                  END-IF
           END-IF

           IF      SW-RESULTS  =       "Y"
               IF  WK-TEXT (1:1) =     SPACE

                   MOVE    SPACE       TO      WK-TEXT
                   UNSTRING WK-WATCH (23:)
                           DELIMITED BY "=" OR SPACE
                           INTO
                           WK-UNST       COUNT WK-BUF1-L

                   CALL    "DECODE02"  USING   WK-UNST
                                               WK-BUF1-L
                                               WK-TEXT
                                               WK-BUF2-L
                   MOVE    WK-BUF2-L   TO      WK-TEXT-L
                   INSPECT WK-TEXT (1:WK-BUF2-L) 
                           REPLACING ALL "+" BY SPACE

                   MOVE    SPACE       TO      WK-TEXT2
                   MOVE    SPACE       TO      WK-TEXT3
                                               WK-TEXTX
                                               WK-LABEL
                                               WK-LABEL2
                                               WK-LABELX
                                               WK-VIDEOCOUNT
                                               WK-SIMPLETEXT
                                               WK-SIMPLETEXT2
                                               WK-CONTENT
                                               WK-CONTENT2
               ELSE

                   MOVE    SPACE       TO      WK-TEXT2
                   MOVE    SPACE       TO      WK-TEXT3
                                               WK-TEXTX
                                               WK-LABEL
                                               WK-LABEL2
                                               WK-LABELX
                                               WK-VIDEOCOUNT
                                               WK-SIMPLETEXT
                                               WK-SIMPLETEXT2
                                               WK-CONTENT
                                               WK-CONTENT2
               END-IF

           ELSE

               IF ( WK-TEXT (1:1) NOT = SPACE
                AND WK-TEXT (1:WK-TEXT-L) = WK-TEXT2 (1:WK-TEXT2-L))

               OR ( WK-TEXT (1:1) NOT = SPACE
                AND WK-TEXT (1:WK-TEXT-L) = WK-CHANNEL-TEXT
                                           (1:WK-CHANNEL-TEXT-L))

                   IF ( WK-TEXT (1:1) NOT = SPACE
                    AND WK-TEXT (1:WK-TEXT-L) = WK-SIMPLETEXT 
                                                   (1:WK-SIMPLETEXT-L))
                           MOVE    SPACE       TO      WK-SIMPLETEXT
                           MOVE    ZERO        TO      WK-SIMPLETEXT-L
                   END-IF

                   MOVE    SPACE       TO      WK-TEXT
                   MOVE    ZERO        TO      WK-TEXT-L

                   PERFORM VARYING I2 FROM 1 BY 1
                           UNTIL I2 > WK-LABEL-L
      *    *** 作成者
                           OR WK-LABEL (I2:10) = X"E4BD9CE68890E880853A"
                       MOVE    WK-LABEL (I2:1) TO   WK-TEXT (I2:1)
                       ADD     1               TO   WK-TEXT-L
                   END-PERFORM

      *    *** LABEL からセットしたので、
      *    *** 強制スペースセット
                   MOVE    SPACE       TO      WK-LABEL
                   MOVE    ZERO        TO      WK-LABEL-L
               ELSE
                   IF (WK-LABEL (1:1) NOT = SPACE
                   AND WK-LABEL (1:WK-LABEL-L) = WK-TEXT2(1:WK-TEXT2-L))
                       MOVE    SPACE       TO      WK-LABEL
                       MOVE    ZERO        TO      WK-LABEL-L
                   ELSE
                       IF ( WK-TEXT (1:1) NOT = SPACE
                        AND WK-TEXT (1:WK-TEXT-L) = WK-SIMPLETEXT 
                                                   (1:WK-SIMPLETEXT-L))
                           MOVE    SPACE       TO      WK-SIMPLETEXT
                           MOVE    ZERO        TO      WK-SIMPLETEXT-L
                       END-IF
                   END-IF
               END-IF
           END-IF



      *    *** 01
           MOVE    SPACE       TO      POT3-REC

           MOVE    1           TO      K

           IF      SW-DEBUG    =       "Y"

             AND ( WK-POT1-CNT >= 11183
             AND   WK-POT1-CNT <= 11265 ) 

                   DISPLAY " "
                   DISPLAY "S210-10 ***********************************"
                   DISPLAY "WK-POT1-CNT=" WK-POT1-CNT
                   DISPLAY "WK-POT2-CNT=" WK-POT2-CNT
                   DISPLAY "WK-POT3-CNT=" WK-POT3-CNT
                   DISPLAY "SW-CHANNEL =" SW-CHANNEL
                   DISPLAY "WK-CHANNEL =" WK-CHANNEL (1:60)
                   DISPLAY "WK-CHANNEL-TEXT =" WK-CHANNEL-TEXT (1:60)
                   DISPLAY "SW-WATCH   =" SW-WATCH
                   DISPLAY "SW-PLAYLIST=" SW-PLAYLIST
                   DISPLAY "WK-PLAYLIST=" WK-PLAYLIST (1:60)
                   DISPLAY "WK-RESULTS =" WK-RESULTS (1:60)
                   DISPLAY "SW-RESULTS =" SW-RESULTS
                   DISPLAY "WK-WATCH   =" WK-WATCH (1:100)
                   DISPLAY "WK-HTTPS   =" WK-HTTPS (1:200)
                   DISPLAY "WK-CHANNEL =" WK-CHANNEL (1:60)
                   DISPLAY "WK-TEXT    =" WK-TEXT    (1:60)
                   DISPLAY "WK-TEXT2   =" WK-TEXT2   (1:60)
                   DISPLAY "WK-TEXTX   =" WK-TEXTX (1:60)
                   DISPLAY "WK-LABEL   =" WK-LABEL (1:60)
                   DISPLAY "WK-CONTENT =" WK-CONTENT (1:60)
                   DISPLAY "WK-CONTENT2=" WK-CONTENT2 (1:60)
                   DISPLAY "WK-VIDEOIDS =" WK-VIDEOIDS
                   DISPLAY "SV-VIDEOIDS =" SV-VIDEOIDS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-CONTENT
                                               WK-SJIS
                   DISPLAY "WK-CONTENT = "     WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-CONTENT2
                                               WK-SJIS
                   DISPLAY "WK-CONTENT2= "     WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    120         TO      WDE05-BUF1-LEN
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-TEXT
                                               WK-SJIS
                   DISPLAY "WK-TEXT    =" WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                        WK-TEXT2 (1:40)

                                               WK-TEXT2
                                               WK-SJIS
                   DISPLAY "WK-TEXT2   =" WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-TEXT3
                                               WK-SJIS
                   DISPLAY "WK-TEXT3 = "       WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-CHANNEL-TEXT
                                               WK-SJIS
                   DISPLAY "WK-CHANNEL-TEXT =" WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-SIMPLETEXT
                                               WK-SJIS
                   DISPLAY "WK-SIMPLETEXT ="   WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-SIMPLETEXT2
                                               WK-SJIS
                   DISPLAY "WK-SIMPLETEXT2="   WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-LABEL
                                               WK-SJIS
                   DISPLAY "WK-LABEL       = " WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-LABEL2
                                               WK-SJIS
                   DISPLAY "WK-LABEL2 = "      WK-SJIS

           END-IF

           EVALUATE TRUE
               WHEN SW-CHANNEL= "Y"
                   IF    WK-CHANNEL-TEXT (1:1) = SPACE
                       IF    WK-TEXT2 (1:1)    =      SPACE
                           MOVE    WK-TEXT    TO
                                   POT3-REC (K:WK-TEXT-L)
                           ADD     WK-TEXT-L  TO      K
                           MOVE    "Y"        TO      SW-CHANNEL-NAME
                       ELSE
                           MOVE    WK-TEXT2    TO
                                   POT3-REC (K:WK-TEXT2-L)
                           ADD     WK-TEXT2-L  TO      K
      *                      CONTINUE
                       END-IF
                   ELSE
                       MOVE    WK-CHANNEL-TEXT TO
                               POT3-REC (K:WK-CHANNEL-TEXT-L)
                       ADD     WK-CHANNEL-TEXT-L  TO      K
                   END-IF

               WHEN  SW-RESULTS = "Y"
      *    *** N 本の動画 等
                   MOVE    WK-TEXT2    TO      POT3-REC (K:WK-TEXT2-L)
                   ADD     WK-TEXT2-L  TO      K

               WHEN SW-PLAYLIST = "Y"

      *    *** WK-SAISEILIST = 再生リストの全体を見る
                   IF      WK-TEXT (1:WK-TEXT-L) = WK-SAISEILIST
                           MOVE    WK-TEXT2   TO POT3-REC (K:WK-TEXT2-L)
                           ADD     WK-TEXT2-L TO      K
                   ELSE
                           MOVE    WK-TEXT   TO   POT3-REC (K:WK-TEXT-L)
                           ADD     WK-TEXT-L TO      K
      *    *** TEXT からセットしたので、
      *    *** 強制スペースセット
                           MOVE    SPACE       TO      WK-TEXT
                           MOVE    ZERO        TO      WK-TEXT-L
                   END-IF

               WHEN OTHER
                   MOVE    WK-TEXT2   TO      POT3-REC (K:WK-TEXT2-L)
                   ADD     WK-TEXT2-L TO      K
           END-EVALUATE

      *    *** 02
           MOVE    " ,"         TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-HTTPS (1:1) = SPACE
                   MOVE    SV-HTTPS    TO      POT3-REC (K:WK-HTTPS-L)
                   ADD     SV-HTTPS-L  TO      K
           ELSE
                   MOVE    WK-HTTPS    TO      POT3-REC (K:WK-HTTPS-L)
                   ADD     WK-HTTPS-L  TO      K
           END-IF

      *    *** 03
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-CHANNEL (1:1) =  SPACE
                   CONTINUE
           ELSE
                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-CHANNEL  TO      POT3-REC (K:WK-CHANNEL-L)
                   ADD     WK-CHANNEL-L TO     K
           END-IF

      *    *** 04
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           EVALUATE TRUE
               WHEN SW-PLAYLIST = "Y"
      *    *** 再生リストの全体を見る
                   MOVE    X"E5868DE7949FE383AAE3"
                                       TO     POT3-REC (K:10)
                   MOVE
             X"82B9E38388E381AEE585A8E4BD93E38292E8A68BE3828B"
                                       TO     POT3-REC (K + 10:23)

                   ADD     33          TO      K
      *             IF      WK-LABEL (1:1) NOT =  SPACE
      *                 AND WK-SIMPLETEXT (1:1) = SPACE
      *                     MOVE    WK-LABEL    TO      WK-SIMPLETEXT
      *                     MOVE    WK-LABEL-L  TO      WK-SIMPLETEXT-L
      *             ELSE
      *                 IF  WK-LABEL (1:1) NOT =  SPACE
      *                 AND WK-SIMPLETEXT2 (1:1) = SPACE
      *                     MOVE    WK-LABEL    TO      WK-SIMPLETEXT2
      *                     MOVE    WK-LABEL-L  TO      WK-SIMPLETEXT2-L
      *                 ELSE
      *                     IF  WK-LABEL (1:1) NOT =  SPACE
      *                     AND WK-TEXT3 (1:1) = SPACE
      *                         MOVE    WK-LABEL    TO      WK-TEXT3
      *                         MOVE    WK-LABEL-L  TO      WK-TEXT3-L
      *                     ELSE
      *                         CONTINUE
      *                     END-IF
      *                 END-IF
      *             END-IF
      *             MOVE    WK-TEXT2    TO      WK-LABEL
      *             ADD     WK-TEXT2-L  TO      WK-LABEL-L

      *             MOVE    WK-CONTENT  TO      WK-SIMPLETEXT
      *             MOVE    WK-CONTENT-L TO     WK-SIMPLETEXT-L

               WHEN SW-RESULTS = "Y"
                   MOVE    WK-TEXT     TO      POT3-REC (K:WK-TEXT-L)
                   ADD     WK-TEXT-L   TO      K

               WHEN SW-WATCH = "Y"
      *    *** LABEL にタイトルがある可能性大

      *      IF       WK-TEXT (1:WK-TEXT-L) = WK-CHANNEL-TEXT
      *                                    (1:WK-CHANNEL-TEXT-L)
      *            OR
      *               WK-TEXT (1:WK-TEXT-L) = WK-WATCH-TEXT
      *                                    (1:WK-WATCH-TEXT-L)
      *            OR
      *               WK-TEXT (1:WK-TEXT-L) = WK-TEXT2
      *                                    (1:WK-TEXT2-L)
      *            OR
      *             (   WK-TEXT-L         = ZERO 
      *             AND WK-CHANNEL-TEXT-L = ZERO )

               IF      WK-LABEL (1:1) =    SPACE
      *    *** 秒
                    OR WK-LABEL (WK-LABEL-L - 3:3) =    X"E7A792"
                   IF  SW-CHANNEL-NAME = "N"
                       MOVE    WK-TEXT     TO     POT3-REC (K:WK-TEXT-L)

                       ADD     WK-TEXT-L   TO      K
                   ELSE
                       MOVE    SPACE       TO      POT3-REC (K:1)
                       ADD     1           TO      K
                   END-IF
               ELSE

                   PERFORM VARYING I2 FROM 1 BY 1
                           UNTIL I2 > WK-LABEL-L
      *    *** 作成者
                           OR WK-LABEL (I2:10) = X"E4BD9CE68890E880853A"
                       MOVE    WK-LABEL (I2:1) TO  POT3-REC (K:1)
                       ADD     1           TO      K
                   END-PERFORM
      *    *** LABEL からセットしたので、
      *    *** 強制スペースセット
                   MOVE    SPACE       TO      WK-LABEL
                   MOVE    ZERO        TO      WK-LABEL-L
               END-IF

               WHEN OTHER
                   MOVE    WK-TEXT     TO      POT3-REC (K:WK-TEXT-L)
                   ADD     WK-TEXT-L   TO      K

           END-EVALUATE

      *    *** 05
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-WATCH (1:1) =    SPACE
                   IF      WK-RESULTS (1:1) =    SPACE
                           CONTINUE
                   ELSE
                       MOVE    "https://www.youtube.com"
                                           TO      POT3-REC (K:23)
                       ADD     23          TO      K

                       MOVE    WK-RESULTS   TO POT3-REC (K:WK-RESULTS-L)
                       ADD     WK-RESULTS-L  TO      K
                   END-IF
           ELSE
                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-WATCH    TO      POT3-REC (K:WK-WATCH-L)
                   ADD     WK-WATCH-L  TO      K
           END-IF

      *    *** 06
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

      *    *** PLAYLIST に CHANNEL=タイトル無い時が有る為、
      *    *** 一つ前のCHANNELセット
      *     IF      SW-PLAYLIST =       "Y"
      *         AND WK-LABEL (1:1) =    SPACE
      *             MOVE    SV-TEXT     TO      WK-LABEL
      *             MOVE    SV-TEXT-L   TO      WK-LABEL-L
      *     END-IF

           IF      WK-LABEL (1:1) =    SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
      *         IF      WK-WATCH-CNT =      1
      *              OR SW-PLAYLIST  =      "Y"
               IF      SW-RESULTS   =      "Y"
                   IF  WK-TEXT (1:1) =      SPACE
                       MOVE    SPACE       TO      POT3-REC (K:1)
                       ADD     1           TO      K
                   ELSE
                       MOVE    WK-TEXT (1:WK-TEXT-L) TO 
                               POT3-REC (K:WK-TEXT-L)
                       ADD     WK-TEXT-L   TO      K
                   END-IF
               ELSE
                  IF   ( WK-TEXT (1:1) NOT = SPACE
                     AND WK-TEXT (1:WK-TEXT-L) = WK-LABEL(1:WK-LABEL-L))
                       MOVE    SPACE       TO      POT3-REC (K:1)
                       ADD     1           TO      K
                  ELSE

                       PERFORM VARYING I2 FROM 1 BY 1
                               UNTIL I2 > WK-LABEL-L
      *    *** 作成者
                           OR WK-LABEL (I2:10) = X"E4BD9CE68890E880853A"
                           MOVE    WK-LABEL (I2:1) TO   POT3-REC (K:1)

                           ADD     1           TO      K
                       END-PERFORM
                   END-IF
               END-IF
           END-IF

      *    *** 07
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-LABEL2 (1:1) =   SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
                   MOVE    WK-LABEL2   TO      POT3-REC (K:WK-LABEL2-L)
                   ADD     WK-LABEL2-L TO      K
           END-IF

      *    *** 08
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-PLAYLIST (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)
                   ADD     23          TO      K

                   MOVE    WK-PLAYLIST (1:WK-PLAYLIST-L) TO
                           POT3-REC (K:WK-PLAYLIST-L)
                   ADD     WK-PLAYLIST-L TO    K
           END-IF

      *    *** 09
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-VIDEOCOUNT (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
                   IF      SW-DOUGA    =       "N"
                       AND SW-PLAYLIST =       "N"
                       INSPECT WK-VIDEOCOUNT (1:WK-VIDEOCOUNT-L) 
                           REPLACING ALL 
      *    *** 本　の動画
                           X"E69CACE38080E381AEE58B95E794BB"
                           BY 
      *    *** 本目の動画
                           X"E69CACE79BAEE381AEE58B95E794BB" 
                   END-IF

                   MOVE    WK-VIDEOCOUNT TO POT3-REC (K:WK-VIDEOCOUNT-L)

                   ADD     WK-VIDEOCOUNT-L TO  K
           END-IF

      *    *** 10
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-SIMPLETEXT (1:WK-SIMPLETEXT-L) = WK-SAISEILIST
                   MOVE    SPACE       TO      WK-SIMPLETEXT
                   MOVE    ZERO        TO      WK-SIMPLETEXT-L
           END-IF

           IF      WK-SIMPLETEXT (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
                   MOVE    WK-SIMPLETEXT TO POT3-REC (K:WK-SIMPLETEXT-L)
                   ADD     WK-SIMPLETEXT-L TO  K
           END-IF

      *    *** 11
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-SIMPLETEXT2 (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
                   MOVE  WK-SIMPLETEXT2 TO POT3-REC (K:WK-SIMPLETEXT2-L)
                   ADD     WK-SIMPLETEXT2-L TO  K
           END-IF

      *    *** 12
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

           IF      WK-TEXT3 (1:1) =    SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
           ELSE
               IF      SW-PLAYLIST =       "Y"
      *    *** WK-TEXT3 のビデオ本数クリアー
               AND WK-TEXT3 (1:WK-TEXT3-L) = WK-VIDEOCOUNT(1:WK-TEXT3-L) 
                   MOVE    SPACE       TO      POT3-REC (K:1)
                   ADD     1           TO      K
               ELSE
                   MOVE    WK-TEXT3    TO      POT3-REC (K:WK-TEXT3-L)
                   ADD     WK-TEXT3-L  TO      K
               END-IF
           END-IF

      *    *** 13
           MOVE    " ,"        TO      POT3-REC (K:2)
           ADD     2           TO      K

      *     IF      WK-HTTPS (1:20) =   "https://i9.ytimg.com"
      *          OR WK-HTTPS (1:33) = "https://lh3.googleusercontent.com"
      *          OR WK-WATCH (1:01) =   SPACE
      *             CONTINUE
      *     ELSE

           IF      WK-HTTPS (1:20) =   "https://i.ytimg.com/"
                OR WK-HTTPS (1:21) =   "https://yt3.ggpht.com" 
                OR WK-HTTPS (1:20) =   "https://i9.ytimg.com"
               OR WK-HTTPS (1:34) = "https://lh3.googleusercontent.com/"
               OR WK-HTTPS (1:34) = "https://yt3.googleusercontent.com/"
               OR SV-HTTPS (1:1) NOT = SPACE
               OR WK-RESULTS (1:1) NOT = SPACE


               IF  WK-WATCH (1:1)  =   SPACE
               AND WK-PLAYLIST (1:1) = SPACE
               AND WK-RESULTS (1:1) =  SPACE
                   CONTINUE
               ELSE
      *    *** キューに追加
                   IF  WK-LABEL2 (1:18) =
                       X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
                       CONTINUE
                   ELSE
      *                 MOVE    WK-TEXT     TO      SV-TEXT
      *                 MOVE    WK-TEXT-L   TO      SV-TEXT-L

                       EVALUATE TRUE
                         WHEN SW-RESULTS  =       "Y"
                           WRITE   POT4-REC    FROM    POT3-REC
                           ADD     1           TO      WK-POT4-CNT

                         WHEN SW-PLAYLIST =       "Y"
                           WRITE   POT5-REC    FROM    POT3-REC
                           ADD     1           TO      WK-POT5-CNT
                                                 WK-PLAYLIST-CNT

      *    *** SW-MIXLIST 以降のWATCH はすべてMIXLIST として処理
                         WHEN SW-MIXLIST =       "Y"
                           WRITE   POT7-REC    FROM    POT3-REC
                           ADD     1           TO      WK-POT7-CNT
                                                       WK-MIXLIST-CNT
                         WHEN OTHER
      *    *** キューに追加
                           IF  WK-TEXT3 (1:18) NOT = 
                               X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
                               IF      WK-WATCH (1:7) = "/shorts"
      *    *** WATCH SERACH short分
                                   PERFORM S290-10     THRU    S290-EX
                                   IF  SW-SEARCH = "Y"
                                       CONTINUE
                                   ELSE
                                       WRITE   POT8-REC    FROM POT3-REC
                                       ADD     1          TO WK-POT8-CNT
                                                             S
                                                       WK-SHORTLIST-CNT
                                       MOVE    WK-WATCH(1:WK-WATCH-L) TO
                                               TBL01-WATCH (S)
                                       MOVE    S       TO      S-MAX
                                   END-IF
                               ELSE
      *    *** WATCH SERACH WATCH分
                                   PERFORM S291-10     THRU    S291-EX
      *    *** SHORT で出力したWATCH分は出力しない
                                   IF  SW-SEARCH = "Y"
                                       CONTINUE
                                   ELSE
                                       WRITE   POT6-REC    FROM POT3-REC
                                       ADD     1          TO WK-POT6-CNT
                                                       WK-WATCHLIST-CNT
                                   END-IF
                               END-IF
                           END-IF
                       END-EVALUATE

                       IF      WK-HTTPS (1:1) NOT = SPACE
      *                     AND SV-HTTPS (1:1) =     SPACE
                           MOVE    WK-HTTPS    TO      SV-HTTPS
                           MOVE    WK-HTTPS-L  TO      SV-HTTPS-L
                       END-IF

                       IF      WK-LABEL (1:1) NOT = SPACE
                           MOVE    WK-LABEL    TO      SV-LABEL
                           MOVE    WK-LABEL-L  TO      SV-LABEL-L
                       END-IF

                       IF      WK-CONTENT (1:1) NOT = SPACE
                           MOVE    WK-CONTENT  TO      SV-CONTENT
                           MOVE    WK-CONTENT-L TO     SV-CONTENT-L
                       END-IF

                   END-IF
               END-IF
           ELSE
               CONTINUE
           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "S210-10"   TO      WFD-ITEM
      *     MOVE    WK-POT3-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT3-REC (1:300)

           .
       S210-20.

           IF      WK-WATCH (1:1) NOT = SPACE
                   MOVE    WK-WATCH    TO      SV-WATCH
           END-IF

           IF      WK-VIDEOIDS (1:1) NOT = SPACE
                   MOVE    WK-VIDEOIDS TO      SV-VIDEOIDS
           END-IF

           MOVE    SPACE       TO      WK-HTTPS
                                       WK-HTTPS-2
                                       WK-CHANNEL
                                       WK-CHANNEL-TEXT
                                       WK-TEXT
                                       WK-TEXT2
                                       WK-TEXT3
                                       WK-TEXTX
                                       WK-LABEL
                                       WK-LABEL2
                                       WK-LABELX
                                       WK-WATCH
                                       WK-WATCH-TEXT
                                       WK-RESULTS
                                       WK-PLAYLIST
                                       WK-VIDEOCOUNT
                                       WK-SIMPLETEXT
                                       WK-SIMPLETEXT2
                                       WK-CONTENT
                                       WK-CONTENT2
                                       WK-VIDEOIDS
                                       POT3-REC

           MOVE    ZERO        TO      WK-HTTPS-L
                                       WK-HTTPS-2-L
                                       WK-CHANNEL-L
                                       WK-CHANNEL-TEXT-L
                                       WK-TEXT-L
                                       WK-TEXT2-L
                                       WK-TEXT3-L
                                       WK-TEXTX-L
                                       WK-LABEL-L
                                       WK-LABEL2-L
                                       WK-LABELX-L
                                       WK-WATCH-L
                                       WK-WATCH-TEXT-L
                                       WK-WATCH-CNT
                                       WK-RESULTS-L
                                       WK-PLAYLIST-L
                                       WK-VIDEOCOUNT-L
                                       WK-SIMPLETEXT-L
                                       WK-SIMPLETEXT2-L
                                       WK-CONTENT-L
                                       WK-CONTENT2-L
                                       WK-ITEM-CNT

           MOVE    "N"         TO
                                       SW-CHANNEL
                                       SW-HTTPS
                                       SW-WATCH
                                       SW-PLAYLIST
                                       SW-MIXLIST
                                       SW-TEXT
                                       SW-LABEL
                                       SW-KENSAKU
                                       SW-RESULTS
                                       SW-DOUGA
                                       SW-DOUGA2
                                       SW-WATCHENDPOINT
                                       SW-SHORT
                                       SW-CHANNEL-NAME

           MOVE    "N"         TO      SW-THUMBNAIL

           .
       S210-EX.
           EXIT.

       S220-10.

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "S220-10"   TO      WFD-ITEM
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

           IF      SW-DEBUG = "Y" 

             AND ( WK-POT1-CNT >= 11183
             AND   WK-POT1-CNT <= 11265 ) 

             AND ( POT1-REC (2:8) = "thumbnail"
                OR POT1-REC (1:9) = "link rel="
                OR POT1-REC (2:7) = "content"
                OR POT1-REC (2:3) = "url"
                OR POT1-REC (1:5) = "title"
                OR POT1-REC (2:5) = "title"
                OR POT1-REC (2:4) = "text"
                OR POT1-REC (2:5) = "label"
                OR POT1-REC (2:5) = "tooltip"
                OR POT1-REC (2:5) = "entityId"
                OR POT1-REC (2:10) = "simpletext"
                OR POT1-REC (3:10) = "simpleText"
                OR POT1-REC (3:13) = "watchEndpoint"
                OR POT1-REC (3:14) = "searchEndpoint"
                OR POT1-REC (1:16) =  "], videoCount : "
                OR POT1-REC (1:11) =  " videoId : "
                OR POT1-REC (1:16) = " videoRenderer :"
                 )
              DISPLAY " "
              DISPLAY "S220-10 ***************************************"
              DISPLAY "WK-POT1-CNT=" WK-POT1-CNT
              DISPLAY "WK-POT2-CNT=" WK-POT2-CNT
              DISPLAY "WK-POT3-CNT=" WK-POT3-CNT
              DISPLAY "WK-HTTPS   =" WK-HTTPS  (1:200)
              DISPLAY "WK-TEXT    =" WK-TEXT  (1:50)
              DISPLAY "WK-TEXT2   =" WK-TEXT2 (1:50)
              DISPLAY "WK-TEXTX   =" WK-TEXTX (1:50)
              DISPLAY "WK-CONTENT =" WK-CONTENT (1:50)
              DISPLAY "WK-CONTENT2=" WK-CONTENT2(1:50)
              DISPLAY "WK-LABEL   =" WK-LABEL (1:50)
              DISPLAY "WK-LABEL2  =" WK-LABEL2 (1:50)
              DISPLAY "WK-WATCH   =" WK-WATCH (1:50)
              DISPLAY "WK-PLAYLIST=" WK-PLAYLIST (1:50)
              DISPLAY "WK-RESULTS =" WK-RESULTS (1:50)
              DISPLAY "WK-CHANNEL =" WK-CHANNEL (1:60)
              DISPLAY "SW-FIRST   =" SW-FIRST
              DISPLAY "SW-CHANNEL =" SW-CHANNEL
              DISPLAY "SW-HTTPS   =" SW-HTTPS
              DISPLAY "SW-WATCH   =" SW-WATCH
              DISPLAY "SW-PLAYLIST=" SW-PLAYLIST
              DISPLAY "SW-RESULTS =" SW-RESULTS
              DISPLAY "SW-MIXLIST =" SW-MIXLIST
              DISPLAY "WK-VIDEOIDS=" WK-VIDEOIDS
              DISPLAY "SV-VIDEOIDS=" SV-VIDEOIDS
              DISPLAY "POT1-REC   =" POT1-REC (1:60)

           END-IF

      *     IF      POT1-REC (1:13) = " thumbnails :"
      *             MOVE    "Y"         TO      SW-THUMBNAIL
      *             MOVE    SPACE       TO      WK-CHANNEL
      *             MOVE    ZERO        TO      WK-CHANNEL-L
      *             MOVE    "N"         TO      SW-CHANNEL
      *     END-IF
      *     ELSE
      *         IF      POT1-REC (1:10) = " videoId :"
      *              PERFORM VARYING K FROM 11 BY 1
      *                     UNTIL SW-THUMBNAIL = "Y" 
      *                        OR POT1-REC (K:4) = SPACE
      *                    IF      POT1-REC (K:11) = "thumbnail :"
      *                        MOVE    "Y"         TO      SW-THUMBNAIL
      *                    END-IF
      *              END-PERFORM
      *         ELSE
      *             CONTINUE
      *         END-IF
      *     END-IF

           EVALUATE TRUE
      *         WHEN POT1-REC(1:13) = " thumbnails :"
      *    *** このクリアー止める
      *         WHEN POT1-REC(1:17) = ", watchEndpoint :"
               WHEN POT1-REC(1:17) = ", XXXXXXXXXXXXX  :"
      *          OR  POT1-REC(1:25) = 
      *              ", watchPlaylistEndpoint :"
      *         OR   POT1-REC(1:18) = ", searchEndpoint :"
      *           AND SW-THUMBNAIL = "Y"

      *         WHEN POT1-REC(1:10) = " videoId :"
                AND SW-HTTPS =       "N"

                   MOVE    SPACE       TO      WK-HTTPS
                                               WK-HTTPS-2
                                               WK-CHANNEL
                                               WK-CHANNEL-TEXT
                                               WK-TEXT
                                               WK-TEXT2
                                               WK-TEXT3
                                               WK-TEXTX
                                               WK-LABEL
                                               WK-LABEL2
                                               WK-LABELX
                                               WK-WATCH
                                               WK-WATCH-TEXT
                                               WK-RESULTS
                                               WK-PLAYLIST
                                               WK-VIDEOCOUNT
                                               WK-SIMPLETEXT
                                               WK-SIMPLETEXT2
                                               WK-CONTENT
                                               WK-CONTENT2
                                               WK-VIDEOIDS
                                               POT3-REC

                   MOVE    ZERO        TO      WK-HTTPS-L
                                               WK-HTTPS-2-L
                                               WK-CHANNEL-L
                                               WK-CHANNEL-TEXT-L
                                               WK-TEXT-L
                                               WK-TEXT2-L
                                               WK-TEXT3-L
                                               WK-TEXTX-L
                                               WK-LABEL-L
                                               WK-LABEL2-L
                                               WK-LABELX-L
                                               WK-WATCH-L
                                               WK-WATCH-TEXT-L
                                               WK-WATCH-CNT
                                               WK-RESULTS-L
                                               WK-PLAYLIST-L
                                               WK-VIDEOCOUNT-L
                                               WK-SIMPLETEXT-L
                                               WK-SIMPLETEXT2-L
                                               WK-CONTENT-L
                                               WK-CONTENT2-L
                                               WK-ITEM-CNT

                   MOVE    "N"         TO      
                                               SW-CHANNEL
                                               SW-HTTPS
                                               SW-WATCH
                                               SW-PLAYLIST
                                               SW-MIXLIST
                                               SW-TEXT
                                               SW-LABEL
                                               SW-KENSAKU
                                               SW-RESULTS
                                               SW-DOUGA
                                               SW-DOUGA2
                                               SW-WATCHENDPOINT
                                               SW-SHORT
                                               SW-CHANNEL-NAME

      *         WHEN POT1-REC(1:06) = "/title"
      *              MOVE    "N"         TO      SW-TITLE

      *    *** タイトルに - YouTube ある時、カット
      *         WHEN SW-TITLE       = "Y"
      *    *** TWICE.PLAYLIST ここを実行する
      *              INSPECT POT1-REC (1:P1) REPLACING ALL "," BY "."
      *              IF      POT1-REC (P1 - 9:10) =  " - YouTube"
      *                      MOVE    POT1-REC (1:P1 - 10) TO WK-PQ
      *                      COMPUTE WK-PQ-L = P1 - 10
      *              ELSE
      *                      MOVE    POT1-REC    TO      WK-PQ
      *                      MOVE    P1          TO      WK-PQ-L
      *              END-IF
      *              MOVE    ".PLAYLIST,PLAYLIST," TO
      *                      WK-PQ (WK-PQ-L + 1:19)
      *              ADD     19          TO      WK-PQ-L
      *         WHEN POT1-REC(1:05) = "title"
      *              MOVE    "Y"         TO      SW-TITLE
      *    *** ページのソースを表示で貼り付け、該当レコードのみ処理した時
      *    *** の title
      *    *** text : J.S. Bach: The Violin Concertos
      *         WHEN POT1-REC(1:08) = " title :"
      *              MOVE    "Y"         TO      SW-TITLE2

      *    *** 検索オプション の時、リセット
      *         WHEN ( POT1-REC(1:08)  = " text : "
      *            AND POT1-REC(9:21)  = 
      *                X"E6A49CE7B4A2E382AAE38397E382B7E383A7E383B3" )
      *              MOVE    "N"         TO      SW-HTTPS

      *         WHEN ( POT1-REC(1:13) = " thumbnails :"

               WHEN POT1-REC(1:11) = " videoId : "
                   MOVE    SPACE       TO      WK-VIDEOIDS
                   UNSTRING POT1-REC (12:)
                           DELIMITED BY " , "
                           INTO
                           WK-VIDEOIDS

               WHEN
                      SW-RESULTS     = "Y"
                 AND  SW-FIRST       = "Y"
                 AND (POT1-REC(1:23) = " searchFilterRenderer :" 
                  OR  POT1-REC(1:18) = ", searchEndpoint :" 
                  OR  POT1-REC(1:17) = ", watchEndpoint :")
                   MOVE    SPACE       TO      WK-RESULTS
                   MOVE    ZERO        TO      WK-RESULTS-L
                   MOVE    "N"         TO      SW-RESULTS

               WHEN
                ((    SW-PLAYLIST    = "Y"
                 AND (POT1-REC(1:25) = ", watchPlaylistEndpoint :"
                   OR POT1-REC(1:18) = ", watchEndpoint :" ))

                OR (  SW-RESULTS     = "Y"
                 AND  SW-FIRST       = "N"
                 AND (POT1-REC(1:23) = " searchFilterRenderer :" 
                  OR  POT1-REC(1:18) = ", searchEndpoint :" 
                  OR  POT1-REC(1:17) = ", watchEndpoint :"))

                OR (( SW-HTTPS =       "Y"
      *    *** N でもＯＫにする、セーブエリアからセットする,S210-10実行する
                  OR "N" )
                  AND POT1-REC(1:17) = ", watchEndpoint :" )

                OR ( POT1-REC(1:09)  = " label : "
      *    *** アルバム
                  AND POT1-REC(10:12) = X"E382A2E383ABE38390E383A0" )

                OR  ( POT1-REC(1:16)  = " text : YouTube "
      *    *** ホーム
                  AND POT1-REC(17:09) = X"E3839BE383BCE383A0" ))

                    IF      WK-HTTPS (1:1) = SPACE
                        AND SV-HTTPS (1:1) NOT = SPACE
                            MOVE    SV-HTTPS    TO       WK-HTTPS
                            MOVE    SV-HTTPS-L  TO       WK-HTTPS-L
                    END-IF

                    IF      SW-FIRST    =     "Y"
      *                  IF      WK-CHANNEL    =       SPACE
                        IF      WK-WATCH (1:1) =       SPACE
                            AND WK-PLAYLIST (1:1) =    SPACE
                            AND WK-RESULTS  (1:1) =    SPACE
                            CONTINUE
                        ELSE
      *    *** WRITE POT3
                            PERFORM S210-10   THRU    S210-EX
                        END-IF
                    ELSE
      *                  IF       WK-WATCH (1:1) NOT = SPACE
      *                       AND WK-WATCH (1:WK-WATCH-L) 
      *                         = SV-WATCH (1:WK-WATCH-L)
      *                         CONTINUE
      *                  ELSE
      *    *** SW-FIRST = "N"
      *    *** WRITE POT3
                            PERFORM S210-10   THRU    S210-EX
      *                  END-IF
                    END-IF

      *         WHEN POT1-REC(1:06) = " PQ : "
      *             MOVE    ZERO        TO      L2
      *             PERFORM VARYING J FROM 7 BY 1
      *                     UNTIL POT1-REC (J:2) = " ,"
      *                        OR J > P1
      *                     ADD     1           TO      L2
      *             END-PERFORM
      *             MOVE    L2        TO      L

      *             MOVE    POT1-REC(7:L) TO  WK-PQ (1:L)
      *             MOVE    L         TO      WK-PQ-L

      *    *** link rel= playlist ?
               WHEN POT1-REC(01:9) = "link rel="
                   MOVE    "N"         TO      SW-PLAYLIST
                   PERFORM VARYING J FROM 10 BY 1
                           UNTIL SW-PLAYLIST = "Y"
                              OR J > P1
                           IF    POT1-REC (J:30) =
                                 "https://m.youtube.com/playlist"
                               MOVE    "Y"         TO      SW-PLAYLIST
                           END-IF
                   END-PERFORM

      *    ***  simpleText : ミックスリスト
               WHEN ( POT1-REC(01:14) = " simpleText : "
                AND POT1-REC(15:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388" )
      *    ***  text : ミックスリスト
      *         WHEN POT1-REC(01:14) = ", text : "
      *          AND POT1-REC(15:21) =
      *              X"E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    ***  content :  ミックスリスト
               OR ( POT1-REC(01:11) = " content : "
                AND POT1-REC(12:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388" )
      *    ***  iconName : MIX , title : ミックスリスト
               OR ( POT1-REC(01:26) = " iconName : MIX , title : "
                AND POT1-REC(27:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388" )

      *             MOVE    "#   YouTube Mix List"
      *                                 TO      POT3-REC
      *             ADD     1           TO      WK-NO
      *             MOVE    WK-NO       TO      POT3-REC (2:2)*

      *             WRITE   POT3-REC
      *             ADD     1           TO      WK-POT3-CNT
                   MOVE    "Y"         TO      SW-MIXLIST
      *     DISPLAY "SW-MIXLIST=" SW-MIXLIST

               WHEN POT1-REC(1:27) = " url : https://i.ytimg.com/"
      *           OR POT1-REC(1:24) = " url : https://yt3.ggpht"
                 OR POT1-REC(1:27) = " url : https://i9.ytimg.com"
                 OR POT1-REC(1:41) = 
                    " url : https://lh3.googleusercontent.com/"

                 OR POT1-REC(1:16) = " url : //i.ytimg"
      *           OR POT1-REC(1:18) = " url : //yt3.ggpht"
                 OR POT1-REC(1:21) = " url : //i9.ytimg.com"
                 OR POT1-REC(1:35) = 
                    " url : //lh3.googleusercontent.com/"
                 OR POT1-REC(1:35) = 
                    " url : //yt3.googleusercontent.com/"

                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L

      *    *** PLAYLIST の最初のサムネイルおかしいので、リセットして、
      *    *** 再セット
      *             IF      SW-HTTPS    =       "Y"
      *                 AND WK-HTTPS (1:17) = "https://yt3.ggpht"
      *                     MOVE    "N"         TO      SW-HTTPS
      *             END-IF

      *    *** N,Y 両方聞いた時は、最後のサムネイル採用
      *    *** Nのみ聞いた時は、最初のサムネイル採用
      *             IF  SW-HTTPS   =     "N" OR "Y"
                   IF  SW-HTTPS   =     "N"
      *    *** 滝川みうの時
                       IF  POT1-REC(1:13) = " url : https:"

                           IF     POT1-REC(1:27) =
      *    *** 2022.08.12 時点、滝川みゆにi9.ytimg.com 無かった
                                  " url : https://i9.ytimg.com"
                               MOVE    SV-HTTPS    TO       WK-HTTPS
                               MOVE    SV-HTTPS-L  TO       WK-HTTPS-L
                           ELSE
                               MOVE    POT1-REC(8:L) TO  WK-HTTPS (1:L)
                               MOVE    L         TO      WK-HTTPS-L
                           END-IF
                       ELSE
      *    *** 藤間桜の時
                           MOVE    "https:"      TO  WK-HTTPS (1:6)
                           MOVE    POT1-REC(8:L) TO  WK-HTTPS (7:L)
                           COMPUTE WK-HTTPS-L = L + 6
                       END-IF

                       MOVE    "Y"       TO      SW-HTTPS
                   ELSE
                       IF     POT1-REC(1:27) =
                              " url : https://i9.ytimg.com"
      *    ***  SW-HTTPS = "Y" なので１つ前のもの使う
                              CONTINUE
                       ELSE
                           IF  POT1-REC(1:13) = " url : https:"
                               MOVE    POT1-REC(8:L) TO WK-HTTPS-2 (1:L)
                               MOVE    L         TO      WK-HTTPS-2-L
                           ELSE
                               MOVE    "https:"      TO WK-HTTPS-2 (1:6)
                               MOVE    POT1-REC(8:L) TO WK-HTTPS-2 (7:L)
                               COMPUTE WK-HTTPS-2-L = L + 6
                           END-IF
                       END-IF
                   END-IF

      *    *** アーチストの画像のため、優先してセット
                   IF      WK-HTTPS (1:34) =
                           "https://lh3.googleusercontent.com/"
                           MOVE    WK-HTTPS    TO      SV-HTTPS
                           MOVE    WK-HTTPS-L  TO      SV-HTTPS-L
                   END-IF

               WHEN POT1-REC(1:07) = " text :"
      *          AND P1 = 7
                AND POT1-REC(8:5) =  SPACE
                    CONTINUE

               WHEN POT1-REC(1:08) = " text : "
      *    *** 再生
                AND ( POT1-REC(9:6) = X"E5868DE7949F"
      *    *** 再生中
                 OR POT1-REC(9:9) = X"E5868DE7949FE4B8AD"
      *    *** ループ再生
                 OR POT1-REC(9:15) =
                    X"E383ABE383BCE38397E5868DE7949F"
      *    *** シャッフル再生
                 OR POT1-REC(9:21) =
                    X"E382B7E383A3E38383E38395E383ABE5868DE7949F"
      *    *** フィルタ
                 OR POT1-REC(9:12) = X"E38395E382A3E383ABE382BF"
      *    *** 検索オプション
                 OR POT1-REC(9:21) =
                    X"E6A49CE7B4A2E382AAE38397E382B7E383A7E383B3"
      *    *** すべて表示
                 OR POT1-REC(9:15) = X"E38199E381B9E381A6E8A1A8E7A4BA"
      *    *** すべて再生
                 OR POT1-REC(9:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** フィードバックを送信
                 OR POT1-REC(9:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** 共有
                 OR POT1-REC(9:6) = X"E585B1E69C89"
      *    *** 動画
                 OR POT1-REC(9:6) = X"E58B95E794BB"
      *    *** カーソルを合わせて再生
                 OR ( POT1-REC(9:10) = 
                    X"E382ABE383BCE382BDE3"
                 AND  POT1-REC(19:23) = 
                    X"83ABE38292E59088E3828FE3819BE381A6E5868DE7949F" )
      *    *** 関連する検索から
                 OR POT1-REC(9:24) = 
                    X"E996A2E980A3E38199E3828BE6A49CE7B4A2E3818BE38289"
      *    *** この動画には
                 OR POT1-REC(9:18) = 
                    X"E38193E381AEE58B95E794BBE381ABE381AF"
      *    *** おすすめ動画
                 OR POT1-REC(9:18) = 
                    X"E3818AE38199E38199E38281E58B95E794BB"
      *    *** ショート
                  OR POT1-REC(9:12) = X"E382B7E383A7E383BCE38388"
      *    *** 配信
                  OR POT1-REC(9:6) = X"E9858DE4BFA1"
      *    *** 作成した再生リスト
                 OR POT1-REC(9:27) =
               X"E4BD9CE68890E38197E3819FE5868DE7949FE383AAE382B9E38388"
      *    *** 作成した再生リスト
                 OR POT1-REC(9:21) =
                    X"E996A2E980A3E38381E383A3E383B3E3838DE383AB"
      *    *** チャンネル登録
                 OR POT1-REC(9:21) =
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
      *    *** ミュージック ビデオ
      *           OR POT1-REC(9:28) =
      *       X"E3839FE383A5E383BCE382B8E38383E382AF20E38393E38387E382AA"
      *    *** ログイン
                 OR POT1-REC(9:12) = X"E383ADE382B0E382A4E383B3"
      *    *** キャンセル
                 OR POT1-REC(9:15) = X"E382ADE383A3E383B3E382BBE383AB"
      *    *** キーボード ショートカット
                 OR ( POT1-REC(9:19) = 
                    X"E382ADE383BCE3839CE383BCE3838920E382B7"
                 AND  POT1-REC(9 + 19:18) = 
                    X"E383A7E383BCE38388E382ABE38383E38388" )
      *    *** 本日更新
                 OR POT1-REC(9:12) = X"E69CACE697A5E69BB4E696B0"
      *    *** 登録解除
                  OR POT1-REC(9:12) = X"E799BBE98CB2E8A7A3E999A4"
      *    *** のチャンネル登録を解除しますか？
                 OR ( POT1-REC(9:20) = 
                    X"E381AEE38381E383A3E383B3E3838DE383ABE799"
                 AND  POT1-REC(29:28) = 
            X"BBE98CB2E38292E8A7A3E999A4E38197E381BEE38199E3818BEFBC9F")
      *    *** この動画のチャプター数:
                 OR  POT1-REC(9:34) = WK-KONODOUGA
      *    *** チャプター数
                 OR  POT1-REC(9:18) =
               X"E38381E383A3E38397E382BFE383BCE695B0"

                  )
                    CONTINUE

               WHEN POT1-REC(1:08) = " text : "
      *    *** 「
                AND POT1-REC(9:03) = X"E3808C"
                AND P1             = 12
                    MOVE     "Y"        TO      SW-KENSAKU

               WHEN POT1-REC(1:08) = " text : "
      *    *** 」に関する検索
                AND POT1-REC(9:21) = 
                    X"E3808DE381ABE996A2E38199E3828BE6A49CE7B4A2"
                    MOVE     "N"        TO      SW-KENSAKU

      *    *** TEXT SKIP
               WHEN POT1-REC(1:08) = " text : "
                AND SW-KENSAKU     = "Y"
                    CONTINUE

      *    *** TEXT
               WHEN POT1-REC(1:08) = " text : "
                   IF      POT1-REC (10:12) =
      *    *** 本の動画
                           X"E69CACE381AEE58B95E794BB" 
                           MOVE    "Y"         TO      SW-DOUGA
                   END-IF
      *    *** 一番最後に１バイトスペースがあるので、(P1 - 18:18)でよい
                   IF      POT1-REC (P1 - 18:18) =
      *    *** 本以上の動画
                           X"E69CACE4BBA5E4B88AE381AEE58B95E794BB" 
                           MOVE    "Y"         TO      SW-DOUGA2
                   END-IF

                   MOVE    ZERO        TO      L2
                   MOVE    "Y"         TO      SW-NUM
                   PERFORM VARYING J FROM 9 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
      *    *** ：
                              OR POT1-REC (J:3) = X"EFBC9A"
                              OR J > P1
                           ADD     1           TO      L2
                           IF      J  <  P1
      *    *** J=P1 はSPACEの為、一つ手前までチェック
                             IF      POT1-REC (J:1) IS  NUMERIC
                                  OR POT1-REC (J:1) =   ","
                                  CONTINUE
                             ELSE
                                 MOVE    "N"         TO      SW-NUM
                             END-IF
                           END-IF
                   END-PERFORM

      *    *** ,で終わる時、NNの動画本数の数字ではない
                   IF      POT1-REC (J:2) =    " ,"
                           MOVE    "N"         TO      SW-NUM
                   END-IF

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L
                   INSPECT POT1-REC(9:L) REPLACING ALL "," BY "."

      *    *** ページのソースを表示で貼り付け、該当レコードのみ処理した時
      *    *** text : J.S. Bach: The Violin Concertos
      *             IF      SW-TITLE2   =       "Y"
      *                 MOVE    "N"         TO      SW-TITLE2
      *                 IF      WK-PQ       =       SPACE
      *                     MOVE    POT1-REC(9:L) TO    WK-PQ
      *                     MOVE    L           TO      WK-PQ-L
      *                     MOVE    ",,"        TO  WK-PQ (WK-PQ-L + 1:2)
      *                     ADD     2           TO      WK-PQ-L
      *                 END-IF
      *             END-IF

      *    *** TEXT 全部数字の時、ビデオ本数の可能性髙い
                   IF  SW-NUM     =     "Y"
                       MOVE    POT1-REC(9:L) TO  WK-VIDEOCOUNT (1:L)
                       MOVE    L         TO      WK-VIDEOCOUNT-L
                   END-IF

                   MOVE    POT1-REC(9:L) TO  WK-TEXTX (1:L)
                   MOVE    L         TO      WK-TEXTX-L

                   IF      SW-NUM    =       "Y"
                       MOVE    POT1-REC(9:L)  TO WK-VIDEOCOUNT (1:L)
      *    *** 本以上の動画 有の時、VIDEOCOUNT,TEXT にはセットしない
                     IF      SW-DOUGA2 =       "Y"
                       CONTINUE
                     ELSE
                       MOVE    L      TO      WK-VIDEOCOUNT-L

      *    ***  本　の動画
                       MOVE X"20E69CACE38080E381AEE58B95E794BB" 
                                      TO      WK-VIDEOCOUNT(1 + L:16)
                       ADD  16        TO      WK-VIDEOCOUNT-L
                     END-IF
                   ELSE
                       IF  WK-TEXT (1:1) =   SPACE

                           IF  POT1-REC(9:18) = 
      *    *** キューに追加
                           X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
                               MOVE    SPACE    TO    WK-TEXT
                               PERFORM VARYING I2 FROM 1 BY 1
                                   UNTIL I2 > WK-LABEL-L
      *    *** 作成者 をカット WK-LABEL => WK-TEXT
                                OR WK-LABEL (I2:10) =
                                   X"E4BD9CE68890E880853A"
                                   MOVE    WK-LABEL (I2:1) TO
                                           WK-TEXT (I2:1)

                               END-PERFORM
                               MOVE    I2        TO      WK-TEXT-L

                               MOVE    SPACE     TO      WK-LABEL
                               MOVE    ZERO      TO      WK-LABEL-L
                           ELSE
                               MOVE    POT1-REC(9:L) TO  WK-TEXT (1:L)
                               MOVE    L         TO      WK-TEXT-L
                           END-IF
                       ELSE
                           IF  WK-TEXT2 (1:1) =   SPACE
                               MOVE    POT1-REC(9:L) TO  WK-TEXT2 (1:L)
                               MOVE    L         TO      WK-TEXT2-L
                           ELSE
                               IF  WK-TEXT3 (1:1) =   SPACE
                                   MOVE POT1-REC(9:L) TO  WK-TEXT3 (1:L)
                                   MOVE    L         TO      WK-TEXT3-L
                               ELSE
      *                             CONTINUE
      *    *** L < 100 の時のみセット

                                 COMPUTE L5 = 1000 - 8 - L - WK-TEXT3-L
      *                           IF    L  <  100
                                 IF    WK-ITEM-CNT < 6
                                   AND L5 > ZERO
                                   ADD     1          TO     WK-ITEM-CNT
                                   MOVE    WK-TEXT3-L TO     L4
                                   MOVE    "<br><br>" TO
                                           WK-TEXT3 (L4 + 1:8)
                                   ADD     8          TO     WK-TEXT3-L

      *     DISPLAY WK-POT1-CNT "  L4=" L4  " L=" L " "

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    L           TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC (9:L)

                                   MOVE    WK-TEXT3-L TO     L4
                                   MOVE    POT1-REC(9:L) TO
                                           WK-TEXT3 (L4 + 1:L)
                                   ADD     L          TO     WK-TEXT3-L
                                 END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "Y"       TO      SW-TEXT

      *    *** ショート動画でこのコマンドの後にタイトル名有り
               WHEN POT1-REC(1:12) = " entityId : "
                   MOVE    ZERO        TO      L2
      *    *** entityId 有りの時、クリアーする
      *    *** SHORT /url : /shorts/nfskAvYsd5c 使ったらズレ治った
      *    ***  url : /watch?v= 使うと動画、視聴数、タイトルズレる
                   MOVE    SPACE       TO      WK-CONTENT
                   MOVE    ZERO        TO      WK-CONTENT-L
                   PERFORM VARYING J FROM 13 BY 1
                           UNTIL POT1-REC (J:3) = " : "
                              OR J > P1
                           CONTINUE
                   END-PERFORM

                   COMPUTE L3 = J + 3
                   PERFORM VARYING J FROM L3 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
      *    ***  - ショート動画を再生
                               OR ( POT1-REC (J:15) =
                                X"202D20E382B7E383A7E383BCE38388"
                                AND POT1-REC (J + 15:15) =
                                X"E58B95E794BBE38292E5868DE7949F" )
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   INSPECT POT1-REC(L3:L) REPLACING ALL "," BY "."
      *    *** LAST LABEL
                   MOVE    POT1-REC(L3:L) TO WK-LABELX (1:L)
                   MOVE    L         TO      WK-LABELX-L

      *             IF  SW-LABEL    =     "N"
                   IF  WK-LABEL (1:1) =  SPACE
                       MOVE    POT1-REC(L3:L) TO WK-LABEL (1:L)
                       MOVE    L         TO      WK-LABEL-L
                   ELSE
                     IF  WK-LABEL2 (1:1) =  SPACE
                       MOVE    POT1-REC(L3:L) TO WK-LABEL2 (1:L)
                       MOVE    L         TO      WK-LABEL2-L
                   END-IF

               WHEN POT1-REC(1:11) = " content : "
                AND POT1-REC(12:5) = SPACE
                    CONTINUE

               WHEN POT1-REC(1:11) = " content : "
      *    *** すべて再生
                AND (POT1-REC(12:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** ミックスリスト
                 OR POT1-REC(12:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    *** キューに追加
                 OR POT1-REC(12:18) =
                    X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
      *    *** フィードバックを送信
                 OR POT1-REC(12:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** プレイリスト
                 OR POT1-REC(12:18) =
                    X"E38397E383ACE382A4E383AAE382B9E38388"
      *    *** すべてのポッドキャストを表示
                 OR  ( POT1-REC(12:20) =
                       X"E38199E381B9E381A6E381AEE3839DE38383E383"
                   AND POT1-REC(32:22) = 
                       X"89E382ADE383A3E382B9E38388E38292E8A1A8E7A4BA" )
      *    *** カスタマイズされた通知のみ
                 OR  ( POT1-REC(12:20) =
                       X"E382ABE382B9E382BFE3839EE382A4E382BAE381"
                   AND POT1-REC(32:21) = 
                       X"95E3828CE3819FE9809AE79FA5E381AEE381BF" )  
      *    *** 共有
                 OR   POT1-REC(12:06) =
                       X"E585B1E69C89"
      *    *** ショート
                 OR   POT1-REC(12:12) =
                       X"E382B7E383A7E383BCE38388"
      *    *** 再生リストに保存
                 OR POT1-REC(12:24) =
                     X"E5868DE7949FE383AAE382B9E38388E381ABE4BF9DE5AD98"
      *    *** コラボレーター
                 OR POT1-REC(12:21) =
                       X"E382B3E383A9E3839CE383ACE383BCE382BFE383BC"
                       )

                    CONTINUE

      *    *** CONTENT セットしても未使用とする、ショート動画、コンテンツ名と
      *    *** WATCH 内容がずれる為、一部表示されないので使う
               WHEN POT1-REC(1:11) = " content : "

                  EVALUATE TRUE
                    WHEN WK-CONTENT = SPACE

                       MOVE    ZERO        TO      L2
                       PERFORM VARYING J FROM 12 BY 1
                               UNTIL POT1-REC (J:2) = " ,"
                                  OR J > P1
                               ADD     1           TO      L2
                       END-PERFORM

      *    *** L2 は1000でまでとする
                       IF      L2          >       1000
                           MOVE    1000        TO      L2
                       END-IF

                       MOVE    L2        TO      L

                       INSPECT POT1-REC(12:L) REPLACING ALL "," BY "."
                       MOVE    POT1-REC(12:L) TO WK-CONTENT (1:L)
                       MOVE    L         TO      WK-CONTENT-L

                    WHEN WK-SIMPLETEXT =  SPACE

                       MOVE    ZERO        TO      L2
                       PERFORM VARYING J FROM 12 BY 1
                               UNTIL POT1-REC (J:2) = " ,"
                                  OR J > P1
                               ADD     1           TO      L2
                       END-PERFORM

      *    *** L2 は1000でまでとする
                       IF      L2          >       1000
                           MOVE    1000        TO      L2
                       END-IF

                       MOVE    L2        TO      L

                       INSPECT POT1-REC(12:L) REPLACING ALL "," BY "."
                       MOVE    POT1-REC(12:L) TO WK-SIMPLETEXT (1:L)
                       MOVE    L         TO      WK-SIMPLETEXT-L

                    WHEN WK-SIMPLETEXT2 =  SPACE

                       MOVE    ZERO        TO      L2
                       PERFORM VARYING J FROM 12 BY 1
                               UNTIL POT1-REC (J:2) = " ,"
                                  OR J > P1
                               ADD     1           TO      L2
                       END-PERFORM

      *    *** L2 は1000でまでとする
                       IF      L2          >       1000
                           MOVE    1000        TO      L2
                       END-IF

                       MOVE    L2        TO      L

                       INSPECT POT1-REC(12:L) REPLACING ALL "," BY "."
                       MOVE    POT1-REC(12:L) TO WK-SIMPLETEXT2 (1:L)
                       MOVE    L         TO      WK-SIMPLETEXT2-L

                    WHEN WK-CONTENT2 = SPACE

                       MOVE    ZERO        TO      L2
                       PERFORM VARYING J FROM 12 BY 1
                               UNTIL POT1-REC (J:2) = " ,"
                                  OR J > P1
                               ADD     1           TO      L2
                       END-PERFORM

      *    *** L2 は1000でまでとする
                       IF      L2          >       1000
                           MOVE    1000        TO      L2
                       END-IF

                       MOVE    L2        TO      L

                       INSPECT POT1-REC(12:L) REPLACING ALL "," BY "."
                       MOVE    POT1-REC(12:L) TO WK-CONTENT2 (1:L)
                       MOVE    L         TO      WK-CONTENT2-L

                  END-EVALUATE

               WHEN POT1-REC(1:09) = " label : "
      *    *** 次へ
                AND ( POT1-REC(10:06) = X"E6ACA1E381B8"
      *    *** キューに追加
                   OR POT1-REC(10:18) =
                         X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
      *    *** 操作メニュー
                   OR POT1-REC(10:18) =
                         X"E6938DE4BD9CE383A1E3838BE383A5E383BC"
      *    *** 興味なし
                   OR POT1-REC(10:12) =
                         X"E88888E591B3E381AAE38197"
      *    *** 後で見る
                   OR POT1-REC(10:12) =
                         X"E5BE8CE381A7E8A68BE3828B"
      *    *** 追加済み
                   OR POT1-REC(10:12) =
                         X"E8BFBDE58AA0E6B888E381BF"
      *    *** 前へ
                   OR POT1-REC(10:06) = X"E5898DE381B8" 
      *    *** チャンネルに移動
                   OR POT1-REC(10:24) = 
                     X"E38381E383A3E383B3E3838DE383ABE381ABE7A7BBE58B95"
      *    *** フィードバックを送信
                 OR POT1-REC(10:30) =
         X"E38395E382A3E383BCE38389E38390E38383E382AFE38292E98081E4BFA1"
      *    *** その他
                   OR POT1-REC(10:9) = X"E3819DE381AEE4BB96"
      *    *** その他の操作
                   OR POT1-REC(10:18) =
                     X"E3819DE381AEE4BB96E381AEE6938DE4BD9C"
      *    *** 確認済み
                   OR POT1-REC(10:12) =
                     X"E7A2BAE8AA8DE6B888E381BF"
      *    *** 折りたたむ
                   OR POT1-REC(10:15) =
                     X"E68A98E3828AE3819FE3819FE38280"
      *    *** 公式アーティスト チャンネル
                   OR ( POT1-REC(10:20) =
                    X"E585ACE5BC8FE382A2E383BCE38386E382A3E382"
                   AND POT1-REC(30:20) =
                    X"B9E3838820E38381E383A3E383B3E3838DE383AB")
      *    *** 字幕
                   OR POT1-REC(10:6) = X"E5AD97E5B995"
      *    *** 視聴
                   OR POT1-REC(WK-POT1-LEN - 5:6) = X"E8A696E881B4"
      *    *** 閉じる
                   OR POT1-REC(10:9) = X"E99689E38198E3828B"
      *    *** すべて表示
                  OR POT1-REC(10:15) = X"E38199E381B9E381A6E8A1A8E7A4BA"
      *    *** YouTube ミックスリスト
                  OR POT1-REC(10:29) =
           X"596F755475626520E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    *** ショート
                  OR POT1-REC(10:12) = X"E382B7E383A7E383BCE38388"
      *    *** チャンネル登録者数
                  OR POT1-REC(10:27) =
           X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
      *    *** チャンネル登録
                 OR POT1-REC(10:21) =
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
      *    *** 設定
                   OR POT1-REC(10:6) = X"E8A8ADE5AE9A"
      *    *** キャンセル
                  OR POT1-REC(10:15) = X"E382ADE383A3E383B3E382BBE383AB"
      *    *** 共有
                  OR POT1-REC(10:6) = X"E585B1E69C89"
      *    *** 登録解除
                  OR POT1-REC(10:12) = X"E799BBE98CB2E8A7A3E999A4"
      *    *** チャンネル「
                  OR POT1-REC(10:18) =
                     X"E38381E383A3E383B3E3838DE383ABE3808C"
      *    *** コラボレーション チャンネル
                  OR POT1-REC(10:40) = WK-KORABO
                     )
                   CONTINUE

      *    *** 通常　タイトル・作成者等有り、再生時間　時分秒
      *    *** ／PLAYLISTの時、タイトル１，２、
               WHEN POT1-REC(1:09) = " label : "
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 10 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   INSPECT POT1-REC(10:L) REPLACING ALL "," BY "."

      *    *** LAST LABEL
                   MOVE    POT1-REC(10:L) TO WK-LABELX (1:L)
                   MOVE    L         TO      WK-LABELX-L

      *             IF  SW-LABEL    =     "N"
                   IF  WK-LABEL (1:1) =  SPACE
                       MOVE    POT1-REC(10:L) TO WK-LABEL (1:L)
                       MOVE    L         TO      WK-LABEL-L
                   ELSE
                     IF  WK-LABEL2 (1:1) =  SPACE
                       MOVE    POT1-REC(10:L) TO WK-LABEL2 (1:L)
                       MOVE    L         TO      WK-LABEL2-L
      *    *** HH:MM:SS
      *    *** H:MM:SS
      *    *** MM:SS等はクリアー
                       IF ( WK-SIMPLETEXT (2:1) =  ":"
                         OR WK-SIMPLETEXT (3:1) =  ":" )
                       AND
      *    *** 時間
                          ( WK-LABEL2 (3:6) = X"E69982E99693"
                         OR WK-LABEL2 (4:6) = X"E69982E99693" )
                           MOVE    SPACE     TO      WK-SIMPLETEXT
                           MOVE    ZERO      TO      WK-SIMPLETEXT-L
                       END-IF

      *    *** 時間同じの入っている
                       IF  WK-LABEL (1:L) = WK-LABEL2 (1:L)
                           MOVE    SPACE     TO      WK-LABEL
                           MOVE    ZERO      TO      WK-LABEL-L
                       END-IF
                     ELSE
                       CONTINUE
                     END-IF
                   END-IF
                   MOVE    "Y"       TO      SW-LABEL

      *    *** tooltip  url : /results?search_query= とlink
               WHEN POT1-REC(1:12) = ", tooltip : "
                AND (
      *    *** 共有
                    POT1-REC(13:6) = X"E585B1E69C89"
                    )
                    CONTINUE

      *    *** tooltip  url : /results?search_query= とlink
               WHEN POT1-REC(1:12) = ", tooltip : "
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 13 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   INSPECT POT1-REC(13:L) REPLACING ALL "," BY "."

      *    *** LAST LABEL
                   MOVE    POT1-REC(13:L) TO WK-TEXT (1:L)
                   MOVE    L         TO      WK-TEXT-L

               WHEN POT1-REC(1:15) =  " url : /channel"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                      IF   WK-CHANNEL-TEXT (1:1) = SPACE
                       AND WK-TEXTX NOT = SPACE
                           MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                           MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                       END-IF
                   END-IF

               WHEN POT1-REC(1:12) =  " url : /user"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

      *    *** user も CHANNEL　にセットする
                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                      IF   WK-CHANNEL-TEXT (1:1) = SPACE
                       AND WK-TEXTX NOT = SPACE
                           MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                           MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                      END-IF
                   END-IF

      *         WHEN POT1-REC(1:10) =  " url : /c/"
               WHEN POT1-REC(1:10) =  " url : /X/"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

      *    *** c も CHANNEL　にセットする
                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                      IF   WK-CHANNEL-TEXT (1:1) = SPACE
                       AND WK-TEXTX NOT = SPACE
                            MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                            MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                       END-IF
                   END-IF

               WHEN POT1-REC(1:9) =  " url : /@"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                      IF   WK-CHANNEL-TEXT (1:1) = SPACE
                       AND WK-TEXTX NOT = SPACE
                           MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                           MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                      END-IF
                   END-IF

               WHEN POT1-REC(1:15) =  " url : /watch?v"

                   ADD     1           TO      WK-WATCH-CNT
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

      *    *** WATCHの時、最後のHTTPS(サムネイル)使う
      *     DISPLAY WK-POT1-CNT " " WK-HTTPS-2 (WK-HTTPS-2-L - 9:10)
                   IF      WK-HTTPS-2 (1:1) NOT = SPACE
                        AND WK-HTTPS-2 (WK-HTTPS-2-L - 9:10) NOT =
                           "frame0.jpg"
                           MOVE    WK-HTTPS-2  TO      WK-HTTPS
                           MOVE    WK-HTTPS-2-L TO     WK-HTTPS-L
                   END-IF

                   IF SW-WATCH   =     "N"
                       MOVE    SPACE     TO      WK-WATCH
                       MOVE    POT1-REC(08:L) TO WK-WATCH (1:L)
                       MOVE    "Y"       TO      SW-WATCH
                       MOVE    L         TO      WK-WATCH-L

      *    *** 直前のTEXT 内容セット
                       MOVE    WK-TEXTX  TO      WK-WATCH-TEXT
                       MOVE    WK-TEXTX-L TO     WK-WATCH-TEXT-L
                       
                   END-IF

                   IF      WK-HTTPS (1:1) =    SPACE
                      IF      SV-HTTPS (1:1) NOT = SPACE
                           MOVE    "Y"         TO      SW-HTTPS
                           MOVE    SV-HTTPS    TO      WK-HTTPS
                           MOVE    SV-HTTPS-L  TO      WK-HTTPS-L
                      END-IF
                   END-IF

               WHEN POT1-REC(1:15) =  " url : /shorts/"

                   ADD     1           TO      WK-WATCH-CNT
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   IF SW-WATCH   =     "N"
                       MOVE    SPACE     TO      WK-WATCH
                       MOVE    POT1-REC(08:L) TO WK-WATCH (1:L)
                       MOVE    "Y"       TO      SW-WATCH
                                                 SW-SHORT
                       MOVE    L         TO      WK-WATCH-L

      *    *** 直前のTEXT 内容セット
                       MOVE    WK-TEXTX  TO      WK-WATCH-TEXT
                       MOVE    WK-TEXTX-L TO     WK-WATCH-TEXT-L
                       
                   END-IF

      *             IF      WK-HTTPS (1:1) =    SPACE
      *                IF      SV-HTTPS (1:1) NOT = SPACE
      *                     MOVE    "Y"         TO      SW-HTTPS
      *                     MOVE    SV-HTTPS    TO      WK-HTTPS
      *                     MOVE    SV-HTTPS-L  TO      WK-HTTPS-L
      *                END-IF
      *             END-IF

      *    ***  url : /shorts/の時、IMG(HTTPS)後に来るので、クリアーする
      *             IF      SW-WATCH =       "Y"
      *                 MOVE    SPACE    TO      WK-HTTPS
      *                 MOVE    ZERO     TO      WK-HTTPS-L
      *                 MOVE    "N"      TO      SW-HTTPS
      *             END-IF
      *    *** XXXXXXXXXXX の長さ変わった時はまた修正する
                    MOVE    
                       "https://i.ytimg.com/vi/XXXXXXXXXXX/frame0.jpg"
                                     TO      WK-HTTPS
                    MOVE    WK-WATCH (9:11) TO WK-HTTPS (24:11)
                    MOVE    45       TO      WK-HTTPS-L

               WHEN POT1-REC(1:16) =  " url : /playlist"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   IF  SW-PLAYLIST=     "N"
                       MOVE    POT1-REC(08:L) TO WK-PLAYLIST (1:L)
                       MOVE    "Y"       TO      SW-PLAYLIST
      *     DISPLAY "SW-PLAYLIST=" SW-PLAYLIST
                       MOVE    L         TO      WK-PLAYLIST-L

      *    *** 直前のTEXT 内容セット 
      *    *** 再生リストの全体を見る => TEXT3へ

                       IF    WK-TEXTX (1:WK-TEXTX-L) NOT = WK-SAISEILIST
                           IF    WK-TEXTX (1:WK-TEXTX-L) 
                               = WK-TEXT  (1:WK-TEXT-L)
                               CONTINUE
                           ELSE
                               MOVE    WK-TEXTX  TO      WK-TEXT3
                               MOVE    WK-TEXTX-L TO     WK-TEXT3-L
                           END-IF
                       ELSE
                           CONTINUE
                       END-IF
                   END-IF

                   IF      WK-HTTPS (1:1) =    SPACE
                      IF      SV-HTTPS (1:1) NOT = SPACE
                           MOVE    "Y"         TO      SW-HTTPS
                           MOVE    SV-HTTPS    TO      WK-HTTPS
                           MOVE    SV-HTTPS-L  TO      WK-HTTPS-L
                      END-IF
                   END-IF

      *         WHEN POT1-REC(1:30) =  " url : /results?search_query= "
      *              CONTINUE

               WHEN POT1-REC(1:29) =  " url : /results?search_query="
                AND POT1-REC(30:1) NOT = SPACE
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
      *                        OR POT1-REC (J:3) = "&sp"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

      *    *** results も WATCH　にセットする
                   IF SW-RESULTS =     "N"
                       MOVE    SPACE     TO      WK-RESULTS
                       MOVE    POT1-REC(08:L) TO WK-RESULTS (1:L)
                       MOVE    "Y"       TO      SW-RESULTS
                       MOVE    L         TO      WK-RESULTS-L
                   END-IF

               WHEN POT1-REC(1:16) =  "], videoCount : "
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 17 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L

                   MOVE    POT1-REC(17:L) TO WK-VIDEOCOUNT (1:L)
                   MOVE    L         TO      WK-VIDEOCOUNT-L

      *    *** 本目 *
      *             MOVE    X"E69CACE79BAE" TO WK-VIDEOCOUNT (1 + L:6)
      *             ADD     6         TO      WK-VIDEOCOUNT-L
      *    ***  本　の動画
                   MOVE X"20E69CACE38080E381AEE58B95E794BB" 
                                  TO      WK-VIDEOCOUNT(1 + L:16)
                   ADD  16        TO      WK-VIDEOCOUNT-L

               WHEN POT1-REC(1:14) =  " simpleText : "
      *    *** 今日 は対象外にする
      *        AND ( POT1-REC(15:06) = X"E4BB8AE697A5" )
      *            MOVE    "N"         TO      SW-HTTPS
      *            MOVE    SPACE       TO      WK-WATCH
      *            MOVE    ZERO        TO      WK-WATCH-L
      *         WHEN POT1-REC(1:14) =  " simpleText : "
      *    *** この再生リストを保存
              AND ( POT1-REC(15:30) =  
         X"E38193E381AEE5868DE7949FE383AAE382B9E38388E38292E4BF9DE5AD98"
                 OR
      *    *** 
                    POT1-REC(15:07) = "YouTube"
                 OR
      *    *** 黒三角横
                    POT1-REC(15:03) = X"E296B6"
                 OR
      *    *** チャンネル登録
                    POT1-REC(15:21) = 
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
                 OR
      *    *** 他の人はこちらも検索
                    POT1-REC(15:30) = 
         X"E4BB96E381AEE4BABAE381AFE38193E381A1E38289E38282E6A49CE7B4A2"
                 OR
      *    *** このチャンネル
                    POT1-REC(15:21) = 
                    X"E38193E381AEE38381E383A3E383B3E3838DE383AB"
      *    *** ログイン
                 OR POT1-REC(15:12) = X"E383ADE382B0E382A4E383B3"
      *    *** ショート
                 OR POT1-REC(15:12) = X"E382B7E383A7E383BCE38388"
      *    *** YouTube ミックスリスト
                 OR POT1-REC(15:29) =
           X"596F755475626520E3839FE38383E382AFE382B9E383AAE382B9E38388"
      *    *** 作成した再生リスト
                 OR POT1-REC(15:21) =
                    X"E996A2E980A3E38381E383A3E383B3E3838DE383AB"
      *    *** チャンネル登録
                 OR POT1-REC(15:21) =
                    X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2"
      *    *** ミュージック ビデオ
      *           OR POT1-REC(15:28) =
      *       X"E3839FE383A5E383BCE382B8E38383E382AF20E38393E38387E382AA"
      *    *** 他の人はこちらも視聴しています
                 OR ( POT1-REC(15:24) =
             X"E4BB96E381AEE4BABAE381AFE38193E381A1E38289E38282"
                 AND  POT1-REC(15 + 24:21) =
             X"E8A696E881B4E38197E381A6E38184E381BEE38199" )
      *    *** 関連する検索から
                 OR POT1-REC(15:24) =
             X"E996A2E980A3E38199E3828BE6A49CE7B4A2E3818BE38289"
      *    *** すべて再生
                 OR POT1-REC(15:15) = X"E38199E381B9E381A6E5868DE7949F"
      *    *** 再生リストを報告
                 OR POT1-REC(15:24) = 
                    X"E5868DE7949FE383AAE382B9E38388E38292E5A0B1E5918A"
      *    *** 不適切なコンテンツ
                 OR POT1-REC(15:27) =
               X"E4B88DE981A9E58887E381AAE382B3E383B3E38386E383B3E38384"
      *    ***  ＼毎週月曜日・隔週で配信
                 OR POT1-REC(15:36) = WK-MAISHUU
                    )
                    CONTINUE

      *    *** 通常は何か月前、視聴回数／PLAYLISTの時、タイトル１，２、
      *    *** チャンネルの再生リストの時は、
               WHEN POT1-REC(1:14) =  " simpleText : "

                   MOVE    ZERO        TO      L2
                   MOVE    "Y"         TO      SW-NUM
                   PERFORM VARYING J FROM 15 BY 1
                           UNTIL J > P1
                           ADD     1           TO      L2
                           IF      J  <  P1
      *    *** J=P1 はSPACEの為、一つ手前までチェック
                             IF      POT1-REC (J:1) IS  NUMERIC
                                  OR POT1-REC (J:1) =   ","
                                  CONTINUE
                             ELSE
                                 MOVE    "N"         TO      SW-NUM
                             END-IF
                           END-IF
                   END-PERFORM

      *    *** L2 は1000でまでとする
                   IF      L2          >       1000
                           MOVE    1000        TO      L2
                   END-IF

                   MOVE    L2        TO      L
                   INSPECT POT1-REC(15:L) REPLACING ALL "," BY "."

      *    *** 登録チャンネルの再生リストの時、SIMPLETEXTはビデオ本数の
      *    *** 数字のみセットされてる、カットする
      *    *** ビデオ本数は、TEXTにセットされている。
      *    ***  text : 2 本の動画
                   IF      SW-NUM    =       "Y"
                       MOVE    POT1-REC(15:L) TO WK-VIDEOCOUNT (1:L)
                       MOVE    L         TO      WK-VIDEOCOUNT-L

      *    ***  本　の動画
                       MOVE X"20E69CACE38080E381AEE58B95E794BB" 
                                      TO      WK-VIDEOCOUNT(1 + L:16)
                       ADD  16        TO      WK-VIDEOCOUNT-L
                   ELSE
                     IF      WK-SIMPLETEXT (1:1) = SPACE
                       MOVE    POT1-REC(15:L) TO WK-SIMPLETEXT (1:L)
      *                 INSPECT WK-SIMPLETEXT (1:L) 
      *                             REPLACING ALL "," BY "."
                       MOVE    L         TO      WK-SIMPLETEXT-L
                     ELSE
      *    *** 内容同じなら、セットしない
                       IF      WK-SIMPLETEXT (1:L) = POT1-REC(15:L)
                                 CONTINUE
                       ELSE
                         IF      WK-SIMPLETEXT2 (1:1) = SPACE
                           MOVE    POT1-REC(15:L) TO WK-SIMPLETEXT2(1:L)
      *                     INSPECT WK-SIMPLETEXT2(1:L) 
      *                             REPLACING ALL "," BY "."
                           MOVE    L         TO      WK-SIMPLETEXT2-L
                         ELSE
                           CONTINUE
                         END-IF
                       END-IF
                     END-IF
                   END-IF

           END-EVALUATE

           IF       POT1-REC(1:17) = ", watchEndpoint :"
      *          AND SW-HTTPS =       "N"

                   MOVE    SPACE       TO      WK-HTTPS
                                               WK-HTTPS-2
                                               WK-CHANNEL
                                               WK-CHANNEL-TEXT
                                               WK-TEXT
                                               WK-TEXT2
                                               WK-TEXT3
                                               WK-TEXTX
                                               WK-LABEL
                                               WK-LABEL2
                                               WK-LABELX
                                               WK-WATCH
                                               WK-WATCH-TEXT
                                               WK-RESULTS
                                               WK-PLAYLIST
                                               WK-VIDEOCOUNT
                                               WK-SIMPLETEXT
                                               WK-SIMPLETEXT2
                                               WK-CONTENT
                                               WK-CONTENT2
                                               WK-VIDEOIDS
                                               POT3-REC

                   MOVE    ZERO        TO      WK-HTTPS-L
                                               WK-HTTPS-2-L
                                               WK-CHANNEL-L
                                               WK-CHANNEL-TEXT-L
                                               WK-TEXT-L
                                               WK-TEXT2-L
                                               WK-TEXT3-L
                                               WK-TEXTX-L
                                               WK-LABEL-L
                                               WK-LABEL2-L
                                               WK-LABELX-L
                                               WK-WATCH-L
                                               WK-WATCH-TEXT-L
                                               WK-WATCH-CNT
                                               WK-RESULTS-L
                                               WK-PLAYLIST-L
                                               WK-VIDEOCOUNT-L
                                               WK-SIMPLETEXT-L
                                               WK-SIMPLETEXT2-L
                                               WK-CONTENT-L
                                               WK-CONTENT2-L
                                               WK-ITEM-CNT

                   MOVE    "N"         TO      
                                               SW-CHANNEL
                                               SW-HTTPS
                                               SW-WATCH
                                               SW-PLAYLIST
                                               SW-MIXLIST
                                               SW-TEXT
                                               SW-LABEL
                                               SW-KENSAKU
                                               SW-RESULTS
                                               SW-DOUGA
                                               SW-DOUGA2
                                               SW-WATCHENDPOINT
                                               SW-SHORT
                                               SW-CHANNEL-NAME
           END-IF

           .
       S220-EX.
           EXIT.

      *    *** official、公式 CHECK
       S230-10.
           MOVE    ZERO        TO      WK-KOSHIKI
                                       WK-OFFICIAL1
                                       WK-OFFICIAL2
                                       WK-OFFICIAL3
                                       WK-OFFICIAL4
           INSPECT WK-TEXT2 TALLYING
      *    *** 公式
               WK-KOSHIKI   FOR ALL X"E585ACE5BC8F"
               WK-OFFICIAL1 FOR ALL "official"
               WK-OFFICIAL2 FOR ALL "OFFICIAL"
               WK-OFFICIAL3 FOR ALL "Official"
      *    *** オフィシャル
               WK-OFFICIAL4 FOR ALL 
                                 X"E382AAE38395E382A3E382B7E383A3E383AB"

           .
       S230-EX.
           EXIT.

       S240-10.
           WRITE   POT3-REC    FROM    POT4-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S240-EX.
           EXIT.

       S250-10.
           WRITE   POT3-REC    FROM    POT5-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S250-EX.
           EXIT.

       S260-10.
           WRITE   POT3-REC    FROM    POT6-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S260-EX.
           EXIT.

       S270-10.
           WRITE   POT3-REC    FROM    POT7-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S270-EX.
           EXIT.

       S280-10.
           WRITE   POT3-REC    FROM    POT8-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S280-EX.
           EXIT.

      *    *** WATCH SERACH short分
      *    *** ショート動画分、インプットにデータダブっているため
       S290-10.

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > S-MAX
                      OR SW-SEARCH = "Y"
                   IF      WK-WATCH (1:WK-WATCH-L) 
                         = TBL01-WATCH (J) (1:WK-WATCH-L) 
                           MOVE    "Y"         TO      SW-SEARCH
                   END-IF
           END-PERFORM
           .
       S290-EX.
           EXIT.

      *    *** WATCH SERACH watch分
      *    *** ショート動画分、インプットにデータダブっているため
       S291-10.

      *    *** /watch?v=pkhRSEm0I1M
           UNSTRING WK-WATCH (10:)
                    DELIMITED BY "&" OR SPACE
                    INTO
                    WK-WATCH2   COUNT WK-WATCH2-L

      *    *** TBL01 はSHORTのみセットしてある
      *    *** /shorts/3G16vrjE4pE
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > S-MAX
                      OR SW-SEARCH = "Y"
                   IF      WK-WATCH2 (1:WK-WATCH2-L) 
                         = TBL01-WATCH (J) (9:WK-WATCH2-L) 
                           MOVE    "Y"         TO      SW-SEARCH
                   END-IF
           END-PERFORM
           .
       S291-EX.
           EXIT.

      *    *** # : hashtag
       S300-10.

           MOVE    SPACE       TO      POT3-REC

           MOVE    1           TO      K

           MOVE    ","         TO      POT3-REC (K:1)
           ADD     1           TO      K

           MOVE    WK-HTTPS-FIRST TO
                   POT3-REC (K:WK-HTTPS-FIRST-L)
           ADD     WK-HTTPS-FIRST-L  TO      K

           MOVE    " , ,"      TO      POT3-REC (K:4)
           ADD     4           TO      K

           MOVE    "#"         TO      POT3-REC (K:1)
           ADD     1           TO      K

           EVALUATE TRUE

      *    *** クラフトワーク
                    WHEN WK-PQ (1:21) =
                         X"E382AFE383A9E38395E38388E383AFE383BCE382AF"
                         MOVE
                         X"E382AFE383A9E38395E38388E383AFE383BCE382AF"
                                             TO      POT3-REC (K:21)
                         ADD     21          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** J_S_Bach
                    WHEN WK-PQ (1:8) = "J_S_Bach"
                         MOVE    "J_S_Bach"  TO      POT3-REC (K:8)
                         ADD     8           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** TWICE
                    WHEN WK-PQ (1:5) = "TWICE"
                         MOVE    "TWICE"     TO      POT3-REC (K:5)
                         ADD     5           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ウェイクアップガールズ
                    WHEN WK-PQ (1:14) = "Wake_Up_Girls!"
                        MOVE X"E382A6E382A7E382A4E382AFE382A2E38383E383"
                                             TO      POT3-REC (K:20)
                         ADD     20          TO      K
                         MOVE    X"97E382ACE383BCE383ABE382BA"
                                             TO      POT3-REC (K:13)
                         ADD     13          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** YOUKU
                    WHEN WK-PQ (1:5) = "YOUKU"
                         MOVE    "youku"     TO      POT3-REC (K:5)
                         ADD     5           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** アイカツ
                    WHEN WK-PQ (1:12) = X"E382A2E382A4E382ABE38384"
                         MOVE    X"E382A2E382A4E382ABE38384"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                    WHEN WK-PQ (1:12) =
      *    *** 映画音楽
                         X"E698A0E794BBE99FB3E6A5BD"
                         MOVE    X"E698A0E794BBE99FB3E6A5BD"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                    WHEN WK-PQ (1:9) =
      *    *** 王心凌
                         X"E78E8BE5BF83E5878C"
                         MOVE    X"E78E8BE5BF83E5878C"
                                             TO      POT3-REC (K:9)
                         ADD     9           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                    WHEN WK-PQ (1:12) =
      *    *** 音羽舞桜
                         X"E99FB3E7BEBDE8889EE6A19C"
                         MOVE    X"E99FB3E7BEBDE8889EE6A19C"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 韓国映画
                    WHEN WK-PQ (1:12) = X"E99F93E59BBDE698A0E794BB"
                         MOVE    X"E99F93E59BBDE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 韓国ドラマ
                    WHEN WK-PQ(1:15) = X"E99F93E59BBDE38389E383A9E3839E"
                         MOVE    X"E99F93E59BBDE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 香港映画
                    WHEN WK-PQ (1:12) = X"E9A699E6B8AFE698A0E794BB"
                         MOVE    X"E9A699E6B8AFE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 香港ドラマ
                    WHEN WK-PQ(1:15) = X"E9A699E6B8AFE38389E383A9E3839E"
                         MOVE    X"E9A699E6B8AFE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 台湾映画
                    WHEN WK-PQ (1:12) = X"E58FB0E6B9BEE698A0E794BB"
                         MOVE    X"E58FB0E6B9BEE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 台湾ドラマ
                    WHEN WK-PQ(1:15) = X"E58FB0E6B9BEE38389E383A9E3839E"
                         MOVE    X"E58FB0E6B9BEE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 中国映画
                    WHEN WK-PQ (1:12) = X"E4B8ADE59BBDE698A0E794BB"
                         MOVE    X"E4B8ADE59BBDE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 中国ドラマ
                    WHEN WK-PQ(1:15) = X"E4B8ADE59BBDE38389E383A9E3839E"
                         MOVE    X"E4B8ADE59BBDE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ナショナルジオグラフィック
                    WHEN ( WK-PQ(1:20) =
                         X"E3838AE382B7E383A7E3838AE383ABE382B8E382"
                     AND WK-PQ(21:19) =
                         X"AAE382B0E383A9E38395E382A3E38383E382AF" )
      *    *** ナショナル_ジオグラフィック
                    OR ( WK-PQ(1:20) =
                         X"E3838AE382B7E383A7E3838AE383AB5FE382B8E3"
                     AND WK-PQ(21:20) =
                         X"82AAE382B0E383A9E38395E382A3E38383E382AF" )
                    OR ( WK-PQ(4:20) =
                         X"E3838AE382B7E383A7E3838AE383AB5FE382B8E3"
                     AND WK-PQ(24:20) =
                         X"82AAE382B0E383A9E38395E382A3E38383E382AF" )
      *    *** ディズニープラス
                         MOVE    
                     X"E38387E382A3E382BAE3838BE383BCE38397E383A9E382B9"
                                             TO      POT3-REC (K:24)
                         ADD     24          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ヒストリーチャンネル
                    WHEN WK-PQ(1:30) =
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"
                      OR WK-PQ(4:30) =
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"

      *    *** 古代の宇宙人
                      OR WK-PQ(1:18) =
                         X"E58FA4E4BBA3E381AEE5AE87E5AE99E4BABA"
      *    *** HISTORY
                      OR WK-PQ(1:7) = "HISTORY"
      *    *** ヒストリーチャンネル
                         MOVE    
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"
                                             TO      POT3-REC (K:30)
                         ADD     30          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ディスカバリーチャンネル
                    WHEN ( WK-PQ(1:20) =
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                     AND WK-PQ(21:16) =
                         X"BCE38381E383A3E383B3E3838DE383AB" )
                    OR ( WK-PQ(4:20) =
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                     AND WK-PQ(24:16) =
                         X"BCE38381E383A3E383B3E3838DE383AB" )
                         MOVE
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                                             TO      POT3-REC (K:20)
                         MOVE
                         X"BCE38381E383A3E383B3E3838DE383AB"
                                             TO     POT3-REC (K + 20:16)
                         ADD     36          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** アニマルプラネット
                    WHEN WK-PQ(1:27) =
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                      OR WK-PQ(4:27) =
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                         MOVE
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                                             TO      POT3-REC (K:27)
                         ADD     27          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                    WHEN OTHER
                     IF      WK-PQ-L     =       2
                           MOVE    ","         TO      POT3-REC (K:1)
                           ADD     1           TO      K
                     ELSE

                       IF      WK-PQ (WK-PQ-L - 18:19) = 
                               ".PLAYLIST,PLAYLIST,"
                           MOVE    WK-PQ (1:WK-PQ-L - 19) TO
                                   POT3-REC (K:WK-PQ-L - 19)
                           COMPUTE K = K + WK-PQ-L - 19

                           MOVE    ","         TO      POT3-REC (K:1)
                           ADD     1           TO      K
                       ELSE
      *    *** WL-PQ にカンマ２つ含むため
                           MOVE    WK-PQ (1:WK-PQ-L - 1) TO
                                   POT3-REC (K:WK-PQ-L - 1)
                           COMPUTE K = K + WK-PQ-L - 1
                       END-IF
                     END-IF
                   END-EVALUATE

                   MOVE    "https://www.youtube.com/hashtag/"
                                       TO      POT3-REC (K:32)
                   ADD     32          TO      K

                   EVALUATE TRUE

      *    *** クラフトワーク
                    WHEN WK-PQ (1:21) =
                         X"E382AFE383A9E38395E38388E383AFE383BCE382AF"
                         MOVE
                         X"E382AFE383A9E38395E38388E383AFE383BCE382AF"
                                             TO      POT3-REC (K:21)
                         ADD     21          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** J_S_Bach
                    WHEN WK-PQ (1:8) = "J_S_Bach"
                         MOVE    "J_S_Bach"  TO      POT3-REC (K:8)
                         ADD     8           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** TWICE
                    WHEN WK-PQ (1:5) = "TWICE"
                         MOVE    "TWICE"     TO      POT3-REC (K:5)
                         ADD     5           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ウェイクアップガールズ
                    WHEN WK-PQ (1:14) = "Wake_Up_Girls!"
                        MOVE X"E382A6E382A7E382A4E382AFE382A2E38383E383"
                                             TO      POT3-REC (K:20)
                         ADD     20          TO      K
                         MOVE    X"97E382ACE383BCE383ABE382BA"
                                             TO      POT3-REC (K:13)
                         ADD     13          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** YOUKU
                    WHEN WK-PQ (1:5) = "YOUKU"
                         MOVE    "youku"     TO      POT3-REC (K:5)
                         ADD     5           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** アイカツ
                    WHEN WK-PQ (1:12) = X"E382A2E382A4E382ABE38384"
                         MOVE    X"E382A2E382A4E382ABE38384"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 映画音楽
                    WHEN WK-PQ (1:12) = X"E698A0E794BBE99FB3E6A5BD"
                         MOVE    X"E698A0E794BBE99FB3E6A5BD"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 王心凌
                    WHEN WK-PQ (1:9) = X"E78E8BE5BF83E5878C"
                         MOVE    X"E78E8BE5BF83E5878C"
                                             TO      POT3-REC (K:9)
                         ADD     9           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 音羽舞桜
                    WHEN WK-PQ (1:12) = X"E99FB3E7BEBDE8889EE6A19C"
                         MOVE    X"E99FB3E7BEBDE8889EE6A19C"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 韓国映画
                    WHEN WK-PQ (1:12) = X"E99F93E59BBDE698A0E794BB"
                         MOVE    X"E99F93E59BBDE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 韓国ドラマ
                    WHEN WK-PQ(1:15) = X"E99F93E59BBDE38389E383A9E3839E"
                         MOVE    X"E99F93E59BBDE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 香港映画
                    WHEN WK-PQ (1:12) = X"E9A699E6B8AFE698A0E794BB"
                         MOVE    X"E9A699E6B8AFE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 香港ドラマ
                    WHEN WK-PQ(1:15) = X"E9A699E6B8AFE38389E383A9E3839E"
                         MOVE    X"E9A699E6B8AFE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 台湾映画
                    WHEN WK-PQ (1:12) = X"E58FB0E6B9BEE698A0E794BB"
                         MOVE    X"E58FB0E6B9BEE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 台湾ドラマ
                    WHEN WK-PQ(1:15) = X"E58FB0E6B9BEE38389E383A9E3839E"
                         MOVE    X"E58FB0E6B9BEE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 中国映画
                    WHEN WK-PQ (1:12) = X"E4B8ADE59BBDE698A0E794BB"
                         MOVE    X"E4B8ADE59BBDE698A0E794BB"
                                             TO      POT3-REC (K:12)
                         ADD     12          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** 中国ドラマ
                    WHEN WK-PQ(1:15) = X"E4B8ADE59BBDE38389E383A9E3839E"
                         MOVE    X"E4B8ADE59BBDE38389E383A9E3839E"
                                             TO      POT3-REC (K:15)
                         ADD     15          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ナショナルジオグラフィック
                    WHEN ( WK-PQ(1:20) =
                         X"E3838AE382B7E383A7E3838AE383ABE382B8E382"
                     AND WK-PQ(21:19) =
                         X"AAE382B0E383A9E38395E382A3E38383E382AF" )
      *    *** ナショナル_ジオグラフィック
                    OR ( WK-PQ(1:20) =
                         X"E3838AE382B7E383A7E3838AE383AB5FE382B8E3"
                     AND WK-PQ(21:20) =
                         X"82AAE382B0E383A9E38395E382A3E38383E382AF" )
                    OR ( WK-PQ(4:20) =
                         X"E3838AE382B7E383A7E3838AE383AB5FE382B8E3"
                     AND WK-PQ(24:20) =
                         X"82AAE382B0E383A9E38395E382A3E38383E382AF" )
      *    *** ディズニープラス
                         MOVE    
                     X"E38387E382A3E382BAE3838BE383BCE38397E383A9E382B9"
                                             TO      POT3-REC (K:24)
                         ADD     24          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ヒストリーチャンネル
                    WHEN WK-PQ(1:30) =
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"
                      OR WK-PQ (4:30) =
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"

      *    *** 古代の宇宙人
                      OR WK-PQ (1:18) =
                         X"E58FA4E4BBA3E381AEE5AE87E5AE99E4BABA"
      *    *** HISTORY
                      OR WK-PQ (1:7) = "HISTORY"
      *    *** ヒストリーチャンネル
                         MOVE    
         X"E38392E382B9E38388E383AAE383BCE38381E383A3E383B3E3838DE383AB"
                                             TO      POT3-REC (K:30)
                         ADD     30          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** ディスカバリーチャンネル
                    WHEN ( WK-PQ(1:20) =
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                     AND WK-PQ(21:16) =
                         X"BCE38381E383A3E383B3E3838DE383AB" )
                    OR ( WK-PQ(4:20) =
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                     AND WK-PQ(24:16) =
                         X"BCE38381E383A3E383B3E3838DE383AB" )
                         MOVE
                         X"E38387E382A3E382B9E382ABE38390E383AAE383"
                                             TO      POT3-REC (K:20)
                         MOVE
                         X"BCE38381E383A3E383B3E3838DE383AB"
                                             TO     POT3-REC (K + 20:16)
                         ADD     36          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** アニマルプラネット
                    WHEN WK-PQ(1:27) =
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                    OR WK-PQ(4:27) =
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                         MOVE
               X"E382A2E3838BE3839EE383ABE38397E383A9E3838DE38383E38388"
                                             TO      POT3-REC (K:27)
                         ADD     27          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

      *    *** tommy.february6
                    WHEN WK-PQ (1:15) = "tommy.february6"
                         MOVE    "tommyfebruary6"  TO POT3-REC (K:14)
                         ADD     14          TO      K

                         MOVE    ","         TO      POT3-REC (K:1)
                         ADD     1           TO      K

                    WHEN OTHER
                     IF      WK-PQ-L     =       2
                           MOVE    SPACE       TO      POT3-REC (K:1)
                           ADD     1           TO      K
                     ELSE
                       IF      WK-PQ (WK-PQ-L - 18:19) =
                               ".PLAYLIST,PLAYLIST,"
                           MOVE    WK-PQ (1:WK-PQ-L - 19) TO
                                   POT3-REC (K:WK-PQ-L - 19)
                           COMPUTE K = K + WK-PQ-L - 19

                           MOVE    ","         TO      POT3-REC (K:1)
                           ADD     1           TO      K
                       ELSE
                           MOVE    WK-PQ (1:WK-PQ-L - 1) TO
                                   POT3-REC (K:WK-PQ-L - 1)
                           COMPUTE K = K + WK-PQ-L - 1
                       END-IF
                     END-IF
           END-EVALUATE

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           .
       S300-EX.
           EXIT.

      *    *** html 解析2
       S310-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      P1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   IF      PIN1-REC(I:1) =     "<" OR ">" OR "{"
                                            OR "}" OR ";"
                       ADD     1           TO      WK-POT1-CNT2

                       EVALUATE TRUE

      *    **** 小倉唯
      *                     WHEN  WK-PQ (1:9) = X"E5B08FE58089E594AF" 
      *    *** 未使用にする
                           WHEN  WK-PQ (1:9) = X"000000000000000000" 
                               IF  WK-POT1-CNT2 > 2040
                               AND  SW-HTTPS-FIRST =  "N"
                               AND POT1-REC (1:7) = " url : "
                                   UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ," OR SPACE
                                          INTO
                                          WK-HTTPS-FIRST 
                                          COUNT WK-HTTPS-FIRST-L
                                   MOVE    "Y"       TO 
                                           SW-HTTPS-FIRST
                               END-IF

                               IF  WK-POT1-CNT2 > 2040
                                   AND POT1-REC (1:8) = " text : "
                                   AND POT1-REC (28:8) = "OFFICIAL"
                                   AND WK-TEXT-OFFICIAL (1:1) = SPACE
                                       UNSTRING POT1-REC (9:)
                                          DELIMITED BY " ,"
                                          INTO WK-TEXT-OFFICIAL COUNT
                                               WK-TEXT-OFFICIAL-L
                               END-IF

                               IF  WK-POT1-CNT2 > 2040
                                AND POT1-REC (1:16) = " url : /channel/"
                                   AND WK-CHANNEL-OFFICIAL (1:1) = SPACE
      *                                 MOVE    "https:" TO  
      *                                        WK-CHANNEL-OFFICIAL (1:6)
                                       UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ,"
                                         INTO WK-CHANNEL-OFFICIAL (1:33)
                                             COUNT WK-CHANNEL-OFFICIAL-L
      *                                 ADD 6 TO WK-CHANNEL-OFFICIAL-L
                               END-IF

      *    **** ychヒストリーチャンネル
                           WHEN ( WK-PQ (1:10) = X"796368E38392E382B9E3" 
                             AND WK-PQ (11:23) = 
                     X"8388E383AAE383BCE38381E383A3E383B3E3838DE383AB" )
      *    **** ヒストリーチャンネル
                             OR ( WK-PQ (1:07) = X"E38392E382B9E3" 
                              AND WK-PQ (8:23) = 
                     X"8388E383AAE383BCE38381E383A3E383B3E3838DE383AB" )
      *    *** 古代の宇宙人
                             OR 　 WK-PQ (1:18) = 
                                 X"E58FA4E4BBA3E381AEE5AE87E5AE99E2BC88" 
                               IF  SW-HTTPS-FIRST =  "N"
                                   MOVE    WK-HISTORY  TO
                                           WK-HTTPS-FIRST
                                   MOVE    143         TO
                                           WK-HTTPS-FIRST-L
                                   MOVE    "Y"       TO 
                                           SW-HTTPS-FIRST
                               END-IF

      *    *** 西條和
                           WHEN WK-PQ (1:9) = X"E8A5BFE6A29DE5928C"
      *    *** 香港ドラマ
                             OR WK-PQ (1:15) = 
                                X"E9A699E6B8AFE38389E383A9E3839E"

                               IF    POT1-REC(1:16) = " videoRenderer :"
                                   MOVE    "Y"       TO
                                           SW-VIDEORENDERER
                                   ADD     1         TO     
                                           WK-VIDEORENDERER-CNT
                               END-IF

                               IF      SW-VIDEORENDERER =  "Y"
                                   AND SW-HTTPS-FIRST =  "N"
                                   AND POT1-REC (1:7) = " url : "

                                   IF     WK-VIDEORENDERER-CNT = 2
                                       UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ," OR SPACE
                                          INTO
                                          WK-HTTPS-FIRST 
                                          COUNT WK-HTTPS-FIRST-L
                                       MOVE    "Y"       TO 
                                               SW-HTTPS-FIRST
                                   ELSE
                                       CONTINUE
                                   END-IF
                               END-IF

      *    *** 天城サリー
                           WHEN WK-PQ (1:15) = 
                                X"E5A4A9E59F8EE382B5E383AAE383BC"

                               IF    POT1-REC(1:16) = " videoRenderer :"
                                   MOVE    "Y"       TO
                                           SW-VIDEORENDERER
                                   ADD     1         TO     
                                           WK-VIDEORENDERER-CNT
                               END-IF

                               IF      SW-VIDEORENDERER =  "Y"
                                   AND SW-HTTPS-FIRST =  "N"
                                   AND POT1-REC (1:7) = " url : "

                                   IF     WK-VIDEORENDERER-CNT = 4
                                       UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ," OR SPACE
                                          INTO
                                          WK-HTTPS-FIRST 
                                          COUNT WK-HTTPS-FIRST-L
                                       MOVE    "Y"       TO 
                                               SW-HTTPS-FIRST
                                   ELSE
                                       CONTINUE
                                   END-IF
                               END-IF

                           WHEN OTHER
                               IF  SW-HTTPS-FIRST =  "N"
                                   EVALUATE TRUE
                                       WHEN POT1-REC (1:30) =
                                       " url : https://i.ytimg.com/vi/"

                                   UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ," OR SPACE
                                          INTO
                                   WK-HTTPS-FIRST COUNT WK-HTTPS-FIRST-L
                                   MOVE    "Y"       TO   SW-HTTPS-FIRST

                                       WHEN POT1-REC (1:35) =
                                   " url : //yt3.googleusercontent.com/"

                                   MOVE "https:" TO WK-HTTPS-FIRST (1:6)
                                   UNSTRING POT1-REC (8:)
                                          DELIMITED BY " ," OR SPACE
                                          INTO
                                   WK-HTTPS-FIRST (7:)
                                          COUNT WK-HTTPS-FIRST-L
                                   ADD     6         TO WK-HTTPS-FIRST-L
                                   MOVE    "Y"       TO   SW-HTTPS-FIRST
                                   END-EVALUATE
                               END-IF
                       END-EVALUATE

                       MOVE    SPACE       TO      POT1-REC
                       MOVE    ZERO        TO      P1
                                                   L
                   ELSE

                       ADD     1           TO      P1
                       EVALUATE TRUE
      *    *** 韓国アイドル対応 
      *    *** ITZY \"WANNABE\" M/V
                           WHEN PIN1-REC(I:2) = '\"'
                               MOVE    '"'         TO    POT1-REC (P1:1)
                               ADD     1           TO      I

                           WHEN PIN1-REC(I:6) = '\u0026'
                               MOVE    "&"         TO    POT1-REC (P1:1)
                               ADD     5           TO      I


                           WHEN PIN1-REC(I:1) NOT = '"'
                               MOVE    PIN1-REC (I:1) TO POT1-REC (P1:1)

                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
                   END-IF

           END-PERFORM
           .
       S310-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F
                   POT2-F
                   POT3-F
                   POT4-F
                   POT5-F
                   POT6-F
                   POT7-F
                   POT8-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

           DISPLAY WK-PGM-NAME " END"

           MOVE    WK-PIN1-MAX-LEN TO  WK-PIN1-LEN-E
           DISPLAY WK-PGM-NAME " PIN1 長さ = " WK-PIN1-LEN-E

           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT2-CNTR TO     WK-POT2-CNTR-E
           DISPLAY WK-PGM-NAME " POT2 件数R= " WK-POT2-CNTR-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4 件数 = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"
           MOVE    WK-POT4-CNTR TO     WK-POT4-CNTR-E
           DISPLAY WK-PGM-NAME " POT4 件数R= " WK-POT4-CNTR-E
                   " (" WK-POT4-F-NAME ")"
           MOVE    WK-POT5-CNT TO      WK-POT5-CNT-E
           DISPLAY WK-PGM-NAME " POT5 件数 = " WK-POT5-CNT-E
                   " (" WK-POT5-F-NAME ")"
           MOVE    WK-POT5-CNTR TO     WK-POT5-CNTR-E
           DISPLAY WK-PGM-NAME " POT5 件数R= " WK-POT5-CNTR-E
                   " (" WK-POT5-F-NAME ")"
           MOVE    WK-POT6-CNT TO      WK-POT6-CNT-E
           DISPLAY WK-PGM-NAME " POT6 件数 = " WK-POT6-CNT-E
                   " (" WK-POT6-F-NAME ")"
           MOVE    WK-POT6-CNTR TO     WK-POT6-CNTR-E
           DISPLAY WK-PGM-NAME " POT6 件数R= " WK-POT6-CNTR-E
                   " (" WK-POT6-F-NAME ")"
           MOVE    WK-POT7-CNT TO      WK-POT7-CNT-E
           DISPLAY WK-PGM-NAME " POT7 件数 = " WK-POT7-CNT-E
                   " (" WK-POT7-F-NAME ")"
           MOVE    WK-POT7-CNTR TO     WK-POT7-CNTR-E
           DISPLAY WK-PGM-NAME " POT7 件数R= " WK-POT7-CNTR-E
                   " (" WK-POT7-F-NAME ")"
           MOVE    WK-POT8-CNT TO      WK-POT8-CNT-E
           DISPLAY WK-PGM-NAME " POT8 件数 = " WK-POT8-CNT-E
                   " (" WK-POT8-F-NAME ")"
           MOVE    WK-POT8-CNTR TO     WK-POT8-CNTR-E
           DISPLAY WK-PGM-NAME " POT8 件数R= " WK-POT8-CNTR-E
                   " (" WK-POT8-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
