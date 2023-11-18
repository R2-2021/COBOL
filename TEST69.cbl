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
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE WATCH データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE IMG データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE WATCH,CHANNEL データ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** RESULTS 一時退避 データ
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
                               STATUS   WK-POT4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PLAYLIST 一時退避 データ
       SELECT POT5-F           ASSIGN   WK-POT5-F-NAME
                               STATUS   WK-POT5-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** WATCHLIST 一時退避 データ
       SELECT POT6-F           ASSIGN   WK-POT6-F-NAME
                               STATUS   WK-POT6-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** MIXLIST 一時退避 データ
       SELECT POT7-F           ASSIGN   WK-POT7-F-NAME
                               STATUS   WK-POT7-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
      *    *** レコード長制限不明、この長さでも処理出来る
      *    *** 2ギガがＭＡＸか？ 2*1024*1024=2,097,152
       01  PIN1-REC.
      *     03  FILLER          PIC  X(500000).
           03  FILLER          PIC  X(620000).

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
           03  WK-PIN1-F-NAME  PIC  X(100) VALUE "TEST69.PIN1".
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

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT5-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT6-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT7-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT5-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT6-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-POT7-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT5-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT6-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT6-CNTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT7-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT7-CNTR    BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT5-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT6-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT6-CNTR-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT7-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT7-CNTR-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHECK-CNT    PIC S9(005) VALUE ZERO.
           03  WK-PLAYLIST-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCHLIST-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-MIXLIST-CNT  BINARY-LONG SYNC VALUE ZERO.

      *    *** <title>
           03  WK-TITLE        PIC  X(1024) VALUE SPACE.
      *    *** 退避エリア
           03  WK-HTML0        PIC  X(1024) VALUE SPACE.
      *    *** タイトル名
           03  WK-HTML1        PIC  X(1024) VALUE SPACE.
      *    *** channel,watch サイト
           03  WK-HTML2        PIC  X(1024) VALUE SPACE.
      *    *** img= src=XXX",data-thumb="XXX" XXX
           03  WK-HTML3        PIC  X(1024) VALUE SPACE.

           03  WK-TITLE-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML3-L      BINARY-LONG SYNC VALUE ZERO.

           03  WK-HTTPS-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-2-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-TEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-TEXT-L BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH-CNT    BINARY-LONG SYNC VALUE ZERO.
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
           03  WK-BUF1-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BUF2-L       BINARY-LONG SYNC VALUE ZERO.

      *    *** 
           03  WK-HTTPS        PIC  X(1024) VALUE SPACE.
           03  WK-HTTPS-2      PIC  X(1024) VALUE SPACE.
           03  WK-CHANNEL      PIC  X(1024) VALUE SPACE.
           03  WK-CHANNEL-TEXT PIC  X(1024) VALUE SPACE.
           03  WK-WATCH        PIC  X(1024) VALUE SPACE.
           03  WK-WATCH-TEXT   PIC  X(1024) VALUE SPACE.
           03  WK-PLAYLIST     PIC  X(1024) VALUE SPACE.
           03  WK-TEXT         PIC  X(1024) VALUE SPACE.
           03  WK-TEXT2        PIC  X(1024) VALUE SPACE.
           03  WK-TEXT3        PIC  X(1024) VALUE SPACE.
           03  WK-TEXTX        PIC  X(1024) VALUE SPACE.
           03  WK-LABEL        PIC  X(1024) VALUE SPACE.
           03  WK-LABEL2       PIC  X(1024) VALUE SPACE.
           03  WK-LABELX       PIC  X(1024) VALUE SPACE.
           03  WK-VIDEOCOUNT   PIC  X(1024) VALUE SPACE.
           03  WK-SIMPLETEXT   PIC  X(1024) VALUE SPACE.
           03  WK-SIMPLETEXT2  PIC  X(1024) VALUE SPACE.
           03  WK-UNST         PIC  X(1024) VALUE SPACE.
           03  WK-NO           PIC  9(002) VALUE ZERO.
           03  WK-PQ           PIC  X(200) VALUE SPACE.
           03  WK-QUERY        PIC  X(200) VALUE SPACE.
           03  WK-SJIS         PIC  X(040) VALUE SPACE.
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
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
           03  SW-TITLE        PIC  X(001) VALUE "N".
           03  SW-TITLE2       PIC  X(001) VALUE "N".
           03  SW-HTTPS        PIC  X(001) VALUE "N".
           03  SW-CHANNEL      PIC  X(001) VALUE "N".
           03  SW-WATCH        PIC  X(001) VALUE "N".
           03  SW-PLAYLIST     PIC  X(001) VALUE "N".
           03  SW-TEXT         PIC  X(001) VALUE "N".
           03  SW-LABEL        PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".
           03  SW-NUM          PIC  X(001) VALUE "Y".
           03  SW-KENSAKU      PIC  X(001) VALUE "N".
           03  SW-RESULTS      PIC  X(001) VALUE "N".
           03  SW-DOUGA        PIC  X(001) VALUE "N".
           03  SW-DOUGA2       PIC  X(001) VALUE "N".
           03  SW-MIXLIST      PIC  X(001) VALUE "N".
           03  SW-DEBUG        PIC  X(001) VALUE "N".
           03  SW-THUMBNAIL    PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-TEXT         PIC  X(1024) VALUE SPACE.
           03  SV-TEXT-L       BINARY-LONG SYNC VALUE ZERO.
           03  SV-HTTPS        PIC  X(1024) VALUE SPACE.
           03  SV-HTTPS-L      BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN 1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** html 解析
                   PERFORM S200-10     THRU    S200-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM



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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F OPEN ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT5-F
           IF      WK-POT5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT5-F OPEN ERROR STATUS="
                           WK-POT5-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT6-F
           IF      WK-POT6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT6-F OPEN ERROR STATUS="
                           WK-POT6-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT7-F
           IF      WK-POT7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT7-F OPEN ERROR STATUS="
                           WK-POT7-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN 2
       S012-10.

           CLOSE   POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F CLOSE ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F OPEN ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF
           .
       S012-EX.
           EXIT.

      *    *** OPEN 3
       S014-10.

           CLOSE   POT5-F
           IF      WK-POT5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT5-F CLOSE ERROR STATUS="
                           WK-POT5-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       POT5-F
           IF      WK-POT5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT5-F OPEN ERROR STATUS="
                           WK-POT5-STATUS
                   STOP    RUN
           END-IF

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
           IF      WK-POT6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT6-F CLOSE ERROR STATUS="
                           WK-POT6-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       POT6-F
           IF      WK-POT6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT6-F OPEN ERROR STATUS="
                           WK-POT6-STATUS
                   STOP    RUN
           END-IF

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
           IF      WK-POT7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT7-F CLOSE ERROR STATUS="
                           WK-POT7-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       POT7-F
           IF      WK-POT7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT7-F OPEN ERROR STATUS="
                           WK-POT7-STATUS
                   STOP    RUN
           END-IF

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

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ POT4
       S030-10.

           READ    POT4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-POT4-EOF
               NOT AT END
                   ADD     1           TO      WK-POT4-CNTR
           END-READ

           IF      WK-POT4-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " POT4-F READ ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF
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

           IF      WK-POT5-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " POT5-F READ ERROR STATUS="
                           WK-POT5-STATUS
                   STOP    RUN
           END-IF
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

           IF      WK-POT6-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " POT6-F READ ERROR STATUS="
                           WK-POT6-STATUS
                   STOP    RUN
           END-IF
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

           IF      WK-POT7-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " POT7-F READ ERROR STATUS="
                           WK-POT7-STATUS
                   STOP    RUN
           END-IF
           .
       S060-EX.
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
                       IF      POT1-REC(1:5) =     " url " OR " text"
                                OR " labe" OR " simp" OR ", sim"
                                OR " thum"
                               WRITE   POT2-REC    FROM    POT1-REC
                               ADD     1           TO      WK-POT2-CNT
                       END-IF

                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

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

                           WHEN PIN1-REC(I:6) = '"PQ":"'
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
                           WHEN PIN1-REC(I:45) = 

                         'https://www.youtube.com/results?search_query='
                            AND WK-PQ (1:3) = SPACE
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

                           WHEN PIN1-REC(I:50) = 
      *    *** 立川絢香 対応 \u003d は =
                    'https://www.youtube.com/results?search_query\u003d'
                            AND WK-PQ (1:3) = SPACE
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

           MOVE    SPACE       TO      POT3-REC

           IF      SW-FIRST    =       "Y"
                   MOVE    "N"         TO      SW-FIRST

                   MOVE    "%"         TO      POT3-REC (1:1)
                   MOVE    "YouTube "  TO      POT3-REC (2:8)
                   IF      WK-PQ-L     =       ZERO
                       MOVE    ",,"        TO      POT3-REC (10:2)
                       MOVE    2           TO      WK-PQ-L
                   ELSE
                       MOVE    WK-PQ       TO      POT3-REC (10:WK-PQ-L)
                   END-IF

                   COMPUTE K = WK-PQ-L + 10
                   MOVE    WK-HTTPS    TO      POT3-REC (K:WK-HTTPS-L)

                   ADD     WK-HTTPS-L  TO      K
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
                       MOVE    "#   YouTube List TOP"
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
                   MOVE    WK-HTTPS    TO      POT3-REC (K:WK-HTTPS-L)

                   ADD     WK-HTTPS-L  TO      K
                   MOVE    " ,"        TO      POT3-REC (K:2)

                   ADD     2           TO      K
                   MOVE    " ,"        TO      POT3-REC (K:2)

                   ADD     2           TO      K
      *             MOVE    WK-TEXT2    TO      POT3-REC (K:WK-TEXT2-L)
                   MOVE    WK-CHANNEL-TEXT TO
                           POT3-REC (K:WK-CHANNEL-TEXT-L)

      *             ADD     WK-TEXT2-L  TO      K
                   ADD     WK-CHANNEL-TEXT-L TO K
                   MOVE    ","         TO      POT3-REC (K:1)

                   ADD     1           TO      K
                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)

                   ADD     23          TO      K
                   MOVE    WK-CHANNEL  TO      POT3-REC (K:WK-CHANNEL-L)

                   ADD     WK-CHANNEL-L TO     K
                   MOVE    " ,"        TO      POT3-REC (K:2)

                   ADD     2           TO      K

                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT

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
               END-IF
           ELSE

               IF  WK-TEXT (1:WK-TEXT-L) = WK-TEXT2 (1:WK-TEXT2-L)
                OR WK-TEXT (1:WK-TEXT-L) = WK-CHANNEL-TEXT
                                           (1:WK-CHANNEL-TEXT-L)
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
                   CONTINUE
               END-IF
           END-IF



           MOVE    SPACE       TO      POT3-REC

           MOVE    1           TO      K

           IF      SW-DEBUG    =       "Y"
                   DISPLAY " "
                   DISPLAY "WK-POT1-CNT=" WK-POT1-CNT
                   DISPLAY "WK-POT2-CNT=" WK-POT2-CNT
                   DISPLAY "WK-POT3-CNT=" WK-POT3-CNT
                   DISPLAY "SW-CHANNEL =" SW-CHANNEL
                   DISPLAY "WK-CHANNEL =" WK-CHANNEL (1:60)
                   DISPLAY "SW-WATCH   =" SW-WATCH
                   DISPLAY "SW-PLAYLIST=" SW-PLAYLIST
                   DISPLAY "SW-RESULTS =" SW-RESULTS
                   DISPLAY "WK-WATCH   =" WK-WATCH (1:60)
                   DISPLAY "WK-HTTPS   =" WK-HTTPS (1:20)
                   DISPLAY "WK-CHANNEL =" WK-CHANNEL (1:60)

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    120         TO      WDE05-BUF1-LEN
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                        WK-TEXT (1:40)
                                               WK-TEXT
                                               WK-SJIS
      *            DISPLAY "SW-TEXT    =" WK-TEXT (1:40)
                   DISPLAY "SW-TEXT    =" WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                        WK-TEXT2 (1:40)
               
                                               WK-TEXT2
                                               WK-SJIS
      *            DISPLAY "SW-TEXT2   =" WK-TEXT2 (1:40)
                   DISPLAY "SW-TEXT2   =" WK-SJIS

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
      *                                        WK-CHANNEL-TEXT (1:40)
                                               WK-CHANNEL-TEXT
                                               WK-SJIS
      *            DISPLAY "WK-CHANNEL-TEXT =" WK-CHANNEL-TEXT (1:40)
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
                   DISPLAY "WK-SIMPLETEXT2 = " WK-SJIS

      *    *** HENKAN=SU SJIS <= UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-LABEL
                                               WK-SJIS

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
                   MOVE    WK-CHANNEL-TEXT TO
                           POT3-REC (K:WK-CHANNEL-TEXT-L)
                   ADD     WK-CHANNEL-TEXT-L  TO      K
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

           MOVE    ","         TO      POT3-REC (K:1)

           ADD     1           TO      K
           MOVE    WK-HTTPS    TO      POT3-REC (K:WK-HTTPS-L)

           ADD     WK-HTTPS-L  TO      K
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K
           MOVE    "https://www.youtube.com"
                               TO      POT3-REC (K:23)

           ADD     23          TO      K
           MOVE    WK-CHANNEL  TO      POT3-REC (K:WK-CHANNEL-L)

           ADD     WK-CHANNEL-L TO     K
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
                       MOVE    WK-TEXT     TO     POT3-REC (K:WK-TEXT-L)

                       ADD     WK-TEXT-L   TO      K
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
               WHEN OTHER
                   MOVE    WK-TEXT     TO      POT3-REC (K:WK-TEXT-L)

                   ADD     WK-TEXT-L   TO      K

           END-EVALUATE
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K
           MOVE    "https://www.youtube.com"
                               TO      POT3-REC (K:23)

           ADD     23          TO      K
           MOVE    WK-WATCH    TO      POT3-REC (K:WK-WATCH-L)

           ADD     WK-WATCH-L  TO      K
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K

      *    *** PLAYLIST に CHANNEL=タイトル無い時が有る為、
      *    *** 一つ前のCHANNELセット
           IF      SW-PLAYLIST =       "Y"
               AND WK-LABEL (1:1) =    SPACE
                   MOVE    SV-TEXT     TO      WK-LABEL
                   MOVE    SV-TEXT-L   TO      WK-LABEL-L
           END-IF

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
                  IF      WK-TEXT (1:WK-TEXT-L) = WK-LABEL (1:WK-TEXT-L)
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
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K
           IF      WK-LABEL2 (1:1) =   SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)

                   ADD     1           TO      K
           ELSE
                   MOVE    WK-LABEL2   TO      POT3-REC (K:WK-LABEL2-L)

                   ADD     WK-LABEL2-L TO      K
           END-IF
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K
           IF      WK-PLAYLIST (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)

                   ADD     1           TO      K
           ELSE
                   MOVE    "https://www.youtube.com"
                                       TO      POT3-REC (K:23)

                   ADD     23          TO      K
                   MOVE    WK-PLAYLIST TO     POT3-REC (K:WK-PLAYLIST-L)

                   ADD     WK-PLAYLIST-L TO    K
           END-IF
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
           MOVE    " ,"        TO      POT3-REC (K:2)

      *    *** HENKAN=SU SJIS <= UTF8
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    "US"        TO      WDE05-HENKAN
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-SIMPLETEXT
                                       WK-SJIS

           ADD     2           TO      K
           IF      WK-SIMPLETEXT (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)

                   ADD     1           TO      K
           ELSE
                   MOVE    WK-SIMPLETEXT TO POT3-REC (K:WK-SIMPLETEXT-L)

                   ADD     WK-SIMPLETEXT-L TO  K
           END-IF
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     2           TO      K
           IF      WK-SIMPLETEXT2 (1:1) = SPACE
                   MOVE    SPACE       TO      POT3-REC (K:1)

                   ADD     1           TO      K
           ELSE
                   MOVE  WK-SIMPLETEXT2 TO POT3-REC (K:WK-SIMPLETEXT2-L)

                   ADD     WK-SIMPLETEXT2-L TO  K
           END-IF
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

               IF  WK-WATCH (1:1)  =   SPACE
               AND WK-PLAYLIST (1:1) = SPACE
                   CONTINUE
               ELSE
      *    *** キューに追加
                   IF  WK-LABEL2 (1:18) =
                       X"E382ADE383A5E383BCE381ABE8BFBDE58AA0"
                       CONTINUE
                   ELSE
                       MOVE    WK-TEXT     TO      SV-TEXT
                       MOVE    WK-TEXT-L   TO      SV-TEXT-L

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
                           WRITE   POT6-REC    FROM    POT3-REC
                           ADD     1           TO      WK-POT6-CNT
                                                       WK-WATCHLIST-CNT

                           MOVE    WK-HTTPS    TO      SV-HTTPS
                           MOVE    WK-HTTPS-L  TO      SV-HTTPS-L
                       END-EVALUATE
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
                                       WK-PLAYLIST
                                       WK-VIDEOCOUNT
                                       WK-SIMPLETEXT
                                       WK-SIMPLETEXT2
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
                                       WK-PLAYLIST-L
                                       WK-VIDEOCOUNT-L
                                       WK-SIMPLETEXT-L
                                       WK-SIMPLETEXT2-L

           MOVE    "N"         TO      SW-CHANNEL
                                       SW-HTTPS
                                       SW-WATCH
                                       SW-PLAYLIST
                                       SW-TEXT
                                       SW-LABEL
                                       SW-KENSAKU
                                       SW-RESULTS
                                       SW-DOUGA
                                       SW-DOUGA2
           .
       S210-EX.
           EXIT.

       S220-10.

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "S220-10"   TO      WFD-ITEM
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

           IF WK-POT2-CNT >= -1 AND <= -1
              DISPLAY " "
              DISPLAY "WK-POT1-CNT=" WK-POT1-CNT
              DISPLAY "WK-POT2-CNT=" WK-POT2-CNT
              DISPLAY "WK-POT3-CNT=" WK-POT3-CNT
              DISPLAY "POT1-REC   =" POT1-REC (1:30)
              DISPLAY "WK-TEXT    =" WK-TEXT  (1:30)
              DISPLAY "WK-TEXT2   =" WK-TEXT2 (1:30)
              DISPLAY "WK-TEXTX   =" WK-TEXTX (1:30)
              DISPLAY "WK-LABEL   =" WK-LABEL (1:30)
              DISPLAY "WK-LABEL2  =" WK-LABEL2 (1:30)
              DISPLAY "WK-WATCH   =" WK-WATCH (1:30)
              DISPLAY "SW-WATCH   =" SW-WATCH
              DISPLAY "SW-HTTPS   =" SW-HTTPS
              DISPLAY "SW-RESULTS =" SW-RESULTS

           END-IF

      *    MOVE    "N"         TO      SW-THUMBNAIL

      *     IF      POT1-REC (1:12) = " thumbnail :"
      *             MOVE    "Y"         TO      SW-THUMBNAIL
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
               WHEN POT1-REC(1:13) = " thumbnails :"

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
                                               WK-PLAYLIST
                                               WK-VIDEOCOUNT
                                               WK-SIMPLETEXT
                                               WK-SIMPLETEXT2
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
                                               WK-PLAYLIST-L
                                               WK-VIDEOCOUNT-L
                                               WK-SIMPLETEXT-L
                                               WK-SIMPLETEXT2-L

                   MOVE    "N"         TO      SW-CHANNEL
                                               SW-HTTPS
                                               SW-WATCH
                                               SW-PLAYLIST
                                               SW-TEXT
                                               SW-LABEL
                                               SW-KENSAKU
                                               SW-RESULTS
                                               SW-DOUGA
                                               SW-DOUGA2

               WHEN POT1-REC(1:06) = "/title"
                    MOVE    "N"         TO      SW-TITLE

      *    *** タイトルに - YouTube ある時、カット
               WHEN SW-TITLE       = "Y"
                    INSPECT POT1-REC (1:P1) REPLACING ALL "," BY "."
                    IF      POT1-REC (P1 - 9:10) =  " - YouTube"
                            MOVE    POT1-REC (1:P1 - 10) TO WK-PQ
                            COMPUTE WK-PQ-L = P1 - 10
                    ELSE
                            MOVE    POT1-REC    TO      WK-PQ
                            MOVE    P1          TO      WK-PQ-L
                    END-IF
                    MOVE    ",PLAYLIST," TO     WK-PQ (WK-PQ-L + 1:10)
                    ADD     10          TO      WK-PQ-L
               WHEN POT1-REC(1:05) = "title"
                    MOVE    "Y"         TO      SW-TITLE
      *    *** ページのソースを表示で貼り付け、該当レコードのみ処理した時
      *    *** の title
      *    *** text : J.S. Bach: The Violin Concertos
               WHEN POT1-REC(1:08) = " title :"
                    MOVE    "Y"         TO      SW-TITLE2

      *    *** 検索オプション の時、リセット
      *         WHEN ( POT1-REC(1:08)  = " text : "
      *            AND POT1-REC(9:21)  = 
      *                X"E6A49CE7B4A2E382AAE38397E382B7E383A7E383B3" )
      *              MOVE    "N"         TO      SW-HTTPS

               WHEN ( POT1-REC(1:13) = " thumbnails :"
      *         WHEN ( POT1-REC(1:10) = " videoId :" 
      *            AND SW-THUMBNAIL   = "Y"
                  AND SW-HTTPS =       "Y" )
                OR  ( POT1-REC(1:13) = " thumbnails :"
      *          OR  ( POT1-REC(1:10) = " videoId :" 
                  AND SW-PLAYLIST =       "Y" )
                OR  ( POT1-REC(1:13) = " thumbnails :" 
                  AND SW-RESULTS     = "Y" )
                OR  ( POT1-REC(1:09)  = " label : "
      *    *** アルバム
                  AND POT1-REC(10:12) = X"E382A2E383ABE38390E383A0" )
                OR  ( POT1-REC(1:16)  = " text : YouTube "
      *    *** ホーム
                  AND POT1-REC(17:09) = X"E3839BE383BCE383A0" )

                    IF      SW-FIRST    =     "Y"
      *                  IF      WK-CHANNEL    =       SPACE
                        IF      WK-WATCH (1:1) =       SPACE
                            AND WK-PLAYLIST (1:1) =    SPACE
                            CONTINUE
                        ELSE
      *    *** WRITE POT3
                            PERFORM S210-10   THRU    S210-EX
                        END-IF
                    ELSE
      *    *** SW-FIRST = "N"
      *    *** WRITE POT3
                        PERFORM S210-10   THRU    S210-EX
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

      *    ***  simpleText :  ミックスリスト
               WHEN POT1-REC(01:14) = " simpleText : "
                AND POT1-REC(15:21) =
                    X"E3839FE38383E382AFE382B9E383AAE382B9E38388"

      *             MOVE    "#   YouTube Mix List"
      *                                 TO      POT3-REC
      *             ADD     1           TO      WK-NO
      *             MOVE    WK-NO       TO      POT3-REC (2:2)*

      *             WRITE   POT3-REC
      *             ADD     1           TO      WK-POT3-CNT
                   MOVE    "Y"         TO      SW-MIXLIST
      *     DISPLAY "SW-MIXLIST=" SW-MIXLIST

               WHEN POT1-REC(1:27) = " url : https://i.ytimg.com/"
                 OR POT1-REC(1:24) = " url : https://yt3.ggpht"
                 OR POT1-REC(1:27) = " url : https://i9.ytimg.com"
                 OR POT1-REC(1:41) = 
                    " url : https://lh3.googleusercontent.com/"

                 OR POT1-REC(1:16) = " url : //i.ytimg"
                 OR POT1-REC(1:18) = " url : //yt3.ggpht"
                 OR POT1-REC(1:21) = " url : //i9.ytimg.com"
                 OR POT1-REC(1:35) = 
                    " url : //lh3.googleusercontent.com/"
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
                       IF  POT1-REC(1:13) = " url : https:"
                           MOVE    POT1-REC(8:L) TO  WK-HTTPS-2 (1:L)
                           MOVE    L         TO      WK-HTTPS-2-L
                       ELSE
                           MOVE    "https:"      TO  WK-HTTPS-2 (1:6)
                           MOVE    POT1-REC(8:L) TO  WK-HTTPS-2 (7:L)
                           COMPUTE WK-HTTPS-2-L = L + 6
                       END-IF
                   END-IF
               WHEN POT1-REC(1:07) = " text :"
                AND P1 = 7
                    CONTINUE

               WHEN POT1-REC(1:08) = " text : "
      *    *** 再生中
                AND ( POT1-REC(9:9) = X"E5868DE7949FE4B8AD"
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
                 OR POT1-REC(9:15) = X"E38199E381B9E381A6E5868DE7949F" )
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

                   MOVE    L2        TO      L
                   INSPECT POT1-REC(9:L) REPLACING ALL "," BY "."

      *    *** ページのソースを表示で貼り付け、該当レコードのみ処理した時
      *    *** text : J.S. Bach: The Violin Concertos
                   IF      SW-TITLE2   =       "Y"
                       MOVE    "N"         TO      SW-TITLE2
                       IF      WK-PQ       =       SPACE
                           MOVE    POT1-REC(9:L) TO    WK-PQ
                           MOVE    L           TO      WK-PQ-L
                           MOVE    ",,"        TO  WK-PQ (WK-PQ-L + 1:2)
                           ADD     2           TO      WK-PQ-L
                       END-IF
                   END-IF

      *    *** TEXT 全部数字の時、ビデオ本数の可能性髙い
      *             IF  SW-NUM     =     "Y"
      *                 MOVE    POT1-REC(9:L) TO  WK-VIDEOCOUNT (1:L)
      *                 MOVE    L         TO      WK-VIDEOCOUNT-L
      *             END-IF

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
                                   CONTINUE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "Y"       TO      SW-TEXT
                       

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
                   OR POT1-REC(10:06) = X"E5898DE381B8" )
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

               WHEN POT1-REC(1:15) =  " url : /channel"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L

                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                       MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                       MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                   END-IF

               WHEN POT1-REC(1:12) =  " url : /user"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L

      *    *** user も CHANNEL　にセットする
                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                       MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                       MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                   END-IF

               WHEN POT1-REC(1:10) =  " url : /c/"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L

      *    *** c も CHANNEL　にセットする
                   IF SW-CHANNEL =     "N"
                       MOVE    POT1-REC(8:L) TO  WK-CHANNEL(1:L)
                       MOVE    "Y"       TO      SW-CHANNEL
                       MOVE    L         TO      WK-CHANNEL-L

      *    *** 直前のTEXT 内容セット
                       MOVE    WK-TEXTX  TO      WK-CHANNEL-TEXT
                       MOVE    WK-TEXTX-L TO     WK-CHANNEL-TEXT-L
                   END-IF

               WHEN POT1-REC(1:15) =  " url : /watch?v"
                   ADD     1           TO      WK-WATCH-CNT
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L
      *    *** WATCHの時、最後のHTTPS(サムネイル)使う
                   IF      WK-HTTPS-2 (1:1) NOT = SPACE
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

               WHEN POT1-REC(1:16) =  " url : /playlist"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
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

      *         WHEN POT1-REC(1:30) =  " url : /results?search_query= "
      *              CONTINUE

               WHEN POT1-REC(1:15) =  " url : /results"
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR POT1-REC (J:3) = "&sp"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
                   MOVE    L2        TO      L

      *    *** results も WATCH　にセットする
                   IF SW-RESULTS =     "N"
                       MOVE    SPACE     TO      WK-WATCH
                       MOVE    POT1-REC(08:L) TO WK-WATCH (1:L)
                       MOVE    "Y"       TO      SW-RESULTS
                       MOVE    L         TO      WK-WATCH-L
                   END-IF

               WHEN POT1-REC(1:16) =  "], videoCount : "
                   MOVE    ZERO        TO      L2
                   PERFORM VARYING J FROM 17 BY 1
                           UNTIL POT1-REC (J:2) = " ,"
                              OR J > P1
                           ADD     1           TO      L2
                   END-PERFORM
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
                 OR
      *    *** ログイン
                    POT1-REC(15:12) = X"E383ADE382B0E382A4E383B3" )
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
                       INSPECT WK-SIMPLETEXT (1:L) 
                                   REPLACING ALL "," BY "."
                       MOVE    L         TO      WK-SIMPLETEXT-L
                     ELSE
      *    *** 内容同じなら、セットしない
                       IF      WK-SIMPLETEXT (1:L) = WK-SIMPLETEXT2(1:L)
                                 CONTINUE
                       ELSE
                         IF      WK-SIMPLETEXT2 (1:1) = SPACE
                           MOVE    POT1-REC(15:L) TO WK-SIMPLETEXT2(1:L)
                           INSPECT WK-SIMPLETEXT2(1:L) 
                                   REPLACING ALL "," BY "."
                           MOVE    L         TO      WK-SIMPLETEXT2-L
                         ELSE
                           CONTINUE
                         END-IF
                       END-IF
                     END-IF
                   END-IF

           END-EVALUATE
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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F CLOSE ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT5-F
           IF      WK-POT5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT5-F CLOSE ERROR STATUS="
                           WK-POT5-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT6-F
           IF      WK-POT6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT6-F CLOSE ERROR STATUS="
                           WK-POT6-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT7-F
           IF      WK-POT7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT7-F CLOSE ERROR STATUS="
                           WK-POT7-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 ｹﾝｽｳ = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4 ｹﾝｽｳ = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"
           MOVE    WK-POT4-CNTR TO     WK-POT4-CNTR-E
           DISPLAY WK-PGM-NAME " POT4 ｹﾝｽｳR= " WK-POT4-CNTR-E
                   " (" WK-POT4-F-NAME ")"
           MOVE    WK-POT5-CNT TO      WK-POT5-CNT-E
           DISPLAY WK-PGM-NAME " POT5 ｹﾝｽｳ = " WK-POT5-CNT-E
                   " (" WK-POT5-F-NAME ")"
           MOVE    WK-POT5-CNTR TO     WK-POT5-CNTR-E
           DISPLAY WK-PGM-NAME " POT5 ｹﾝｽｳR= " WK-POT5-CNTR-E
                   " (" WK-POT5-F-NAME ")"
           MOVE    WK-POT6-CNT TO      WK-POT6-CNT-E
           DISPLAY WK-PGM-NAME " POT6 ｹﾝｽｳ = " WK-POT6-CNT-E
                   " (" WK-POT6-F-NAME ")"
           MOVE    WK-POT6-CNTR TO     WK-POT6-CNTR-E
           DISPLAY WK-PGM-NAME " POT6 ｹﾝｽｳR= " WK-POT6-CNTR-E
                   " (" WK-POT6-F-NAME ")"
           MOVE    WK-POT7-CNT TO      WK-POT7-CNT-E
           DISPLAY WK-PGM-NAME " POT7 ｹﾝｽｳ = " WK-POT7-CNT-E
                   " (" WK-POT7-F-NAME ")"
           MOVE    WK-POT7-CNTR TO     WK-POT7-CNTR-E
           DISPLAY WK-PGM-NAME " POT7 ｹﾝｽｳR= " WK-POT7-CNTR-E
                   " (" WK-POT7-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
