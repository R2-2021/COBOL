      *    *** bookmarks.html XVIデータ抽出
      *    *** PIN3,PIN4,PIN5のデータはPOT1,POT4両方に出力
      *    *** PIN1データは、#aduxvi女優名あーお の直前までは、
      *    *** POT1,POT4両方に出力
      *    *** #aduxvi可愛い 直前までは、POT1 のみに出力
      *    *** 以降はPOT4 のみに出力
      *    *** XVIは女優分（POT1）とその他分（POT4）に分けて出力+
      *    *** 
      *    *** https://www.xvideos.com/video と 
      *    *** https://www.xvideos.red/video でvideo以降の数字が同じ物も
      *    *** あるが、どちらか一方がPIN2にIMG登録されていれば問題ない、
      *    *** ２つ登録されていても、SORTしてるので先にHITしたIMGがセット
      *    *** される
      *    *** 例 https://www.xvideos.red/video73987457/_ と
      *    ***    https://www.xvideos.com/video73987457/_
      *    ***
      *    *** タイトル１桁目 #=>_# にする、内部記号と競合するため
      *    *** 
      *    *** ダブリ POT5 は TEST75 でPIN2のインプットにして、TEST70.POT1
      *    *** をPIN1 にしてマッチしたPOT1 をXVI=>XVID に変更して、
      *    *** 不要なジャパリを削除して、TEST70..POT1 に置き換え
      *    *** TEST53 TEST54 を実行する
      *    ***
      *    *** JOB TEST10   TESTXX   TEST84
      *    ***        |        |        |
      *    ***        |--------|--------|
      *    ***     TEST70
      *    ***        |
      *    ***     TEST53
      *    ***        |
      *    ***     TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST70.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ ＵＴＦ８
      *    *** TEST10.POT1 => TEST70.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 キーワード追加データ
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 女優名追加データ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 NUM,ALPHA追加データ
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
                               STATUS   WK-PIN5-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XVI データ (女優分)
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG アンマッチデータ　次回 IMG HTML 追加してIMG データ
      *    *** TEST70.PIN2 へ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TITLE PIN2 マッチング済　データ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XVI データ (その他分)
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
                               STATUS   WK-POT4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XVI データ (ダブリ分)
       SELECT POT5-F           ASSIGN   WK-POT5-F-NAME
                               STATUS   WK-POT5-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN3-F
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(100).

       FD  PIN4-F
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03  FILLER          PIC  X(100).

       FD  PIN5-F
           RECORD VARYING DEPENDING ON WK-PIN5-LEN.
       01  PIN5-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       FD  POT3-F.
       01  POT3-REC.
           03  FILLER          PIC  X(1000).

       FD  POT4-F.
       01  POT4-REC.
           03  FILLER          PIC  X(1000).

       FD  POT5-F.
       01  POT5-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST70  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10X.POT1".
      *    *** TEST10 で
      *    "bookmarks.html" => TEST10.POT1
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST70.PIN2".
      *    *** 検索　キーワード
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST70.PIN3".
      *    *** 検索　女優名
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST70.PIN4".
      *    *** 検索　ＮＵＭ＿ＡＬＰＨＡ
      *     03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST70.PIN5".
      *    *** 検索　t TEST84
      *     03  WK-PIN5-F-NAME  PIC  X(032) VALUE "xvitag_t_s.csv".
      *     03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST108.POT1".
           03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST110.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST70.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST70.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST70.POT3".
           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST70.POT4".
           03  WK-POT5-F-NAME  PIC  X(032) VALUE "TEST70.POT5".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN5-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT5-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN5-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT5-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT5-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-A            PIC  X(1000) VALUE SPACE.
           03  WK-ID           PIC  X(010) VALUE SPACE.
           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-IMG          PIC  X(500) VALUE SPACE.
           03  WK-PIN5-ITEM1   PIC  X(100) VALUE SPACE.
           03  WK-PIN5-ITEM2   PIC  X(100) VALUE SPACE.
           03  WK-DISP         PIC  X(020) VALUE SPACE.

           03  WK-A-LEN        BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-ITEM2-LEN BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE 2000.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** テーブルサイズ変更したら、I-MAX も変更する
           03  TBL01-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL01-ID
                               INDEXED BY TBL01-IDX.
             05  TBL01-ID      PIC  X(010) VALUE HIGH-VALUE.
             05  TBL01-IMG     PIC  X(500) VALUE SPACE.
             05  TBL01-TITLE   PIC  X(500) VALUE SPACE.
             05  TBL01-PIN2-REC PIC X(1000) VALUE SPACE.
             05  TBL01-SET     PIC  X(001) VALUE SPACE.
             05  TBL01-IMG-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-RED     PIC  X(001) VALUE SPACE.
             05  TBL01-COM     PIC  X(001) VALUE SPACE.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
           03  SW-A            PIC  X(001) VALUE "N".
           03  SW-XVI          PIC  X(001) VALUE "N".
           03  SW-SONOTA       PIC  X(001) VALUE "N".
      *    *** #aduxvi女優名あーお が出てくるまで、POT1,POT4 両方に出力
      *    *** に変更
           03  SW-BOTH         PIC  X(001) VALUE "Y".
           03  SW-TAGA         PIC  X(001) VALUE "N".
           03  SW-SKIP         PIC  X(001) VALUE "Y".
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".
           03  SW-RED          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
                   IF  PIN2-REC (1:29) = "https://www.xvideos.com/video"
                    OR PIN2-REC (1:28) = "http://www.xvideos.com/video"
                    OR PIN2-REC (1:29) = "https://www.xvideos.red/video"
      *    *** PIN2 TBL SET
                       PERFORM S032-10     THRU    S032-EX
                   END-IF

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** TBL01 SORT
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-ID



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

                   IF      PIN1-REC (10:29) = 
                           "https://www.xvideos.com/video"
                        OR PIN1-REC (10:28) = 
                           "http://www.xvideos.com/video"
                        OR PIN1-REC (10:29) =
                           "https://www.xvideos.red/video"
      *    *** PIN1 TBL SET
                           PERFORM S022-10     THRU    S022-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM



      *    *** CLOSE,OPEN PIN1
           PERFORM S012-10     THRU    S012-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** XVI まで読み飛ばし
           PERFORM UNTIL   SW-XVI = "Y"
                        OR WK-PIN1-EOF   =     HIGH-VALUE
                   IF      PIN1-REC (1:3) = "XVI"
                       AND WK-PIN1-LEN =    3

      *    *** WRITE POT1,POT4 HEAD
                       PERFORM S040-10     THRU    S040-EX
                       MOVE    "Y"         TO      SW-XVI
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           IF      SW-XVI      =       "N"
                   DISPLAY WK-PGM-NAME " PIN1-REC XVI 無 SW-XVI="
                           SW-XVI
                   DISPLAY WK-PGM-NAME " TEST10 再実行してみる"
           END-IF

      *    *** READ PIN1 </H3>
           IF      WK-PIN1-EOF NOT =   HIGH-VALUE
                   PERFORM S020-10     THRU    S020-EX
           END-IF

      *    *** PIN3 => PIN5 に変更した為、READ 止める
      *    *** READ PIN3
      *     PERFORM S050-10     THRU    S050-EX
           MOVE    HIGH-VALUE  TO      WK-PIN3-EOF

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE

      *    *** 検索 キーワード追加データ出力
      *    *** WRITE POT1
                   PERFORM S052-10     THRU    S052-EX

      *    *** READ PIN3
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM



      *    *** WRITE POT1 HEAD 2
           PERFORM S042-10     THRU    S042-EX

      *    *** READ PIN4
           PERFORM S060-10     THRU    S060-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE

      *    *** 検索 女優名追加データ出力
      *    *** WRITE POT1
                   PERFORM S062-10     THRU    S062-EX

      *    *** READ PIN4
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM



      *    *** ジャパリaduxvi-Search-Num-Alpha WRITE 止める
      *    *** WRITE POT1 HEAD 3
      *     PERFORM S044-10     THRU    S044-EX

      *    *** READ PIN5
           PERFORM S070-10     THRU    S070-EX

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE

      *    *** ジャパリ
                   IF      PIN5-REC (1:12) = X"E382B8E383A3E38391E383AA"
                           MOVE    PIN5-REC    TO      POT1-REC
      *    *** WRITE POT1 OR POT4
                           PERFORM S080-10     THRU    S080-EX
      *    *** WRITE POT4
                           PERFORM S090-10     THRU    S090-EX
                   ELSE
      *    *** 検索 NUM,APLHA追加データ出力
      *    *** WRITE POT1
                           PERFORM S072-10     THRU    S072-EX
                   END-IF

      *    *** READ PIN5
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM



           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** #aduxvi可愛い
                   IF      PIN1-REC (1:15) =
                           X"616475787669E58FAFE6849BE38184"
                           MOVE    "Y"         TO      SW-SONOTA
                   END-IF
      *    *** #aduxvi女優名あーお
                   IF      PIN1-REC (1:25) =
                   X"23616475787669E5A5B3E584AAE5908DE38182E383BCE3818A"
                           MOVE    "N"         TO      SW-BOTH
                   END-IF

      *    *** html 解析
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
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

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F OPEN ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F OPEN ERROR STATUS="
                           WK-PIN5-STATUS
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           SET     TBL01-IDX   TO      1

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

           MOVE    "XVI"       TO      POT5-REC
           WRITE   POT5-REC
           ADD     1           TO      WK-POT5-CNT

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT5-REC
           WRITE   POT5-REC
           ADD     1           TO      WK-POT5-CNT

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** CLOSE,OPEN PIN1
       S012-10.

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
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                   MOVE    "N"         TO      SW-FIRST
               NOT AT END
                   IF      SW-FIRST    =       "N"
                           ADD     1           TO      WK-PIN1-CNT
                   END-IF
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT = " WK-PIN1-CNT
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** PIN1 TBL SET
       S022-10.

           IF      PIN1-REC (10:29) = "https://www.xvideos.com/video"
                OR PIN1-REC (10:29) = "https://www.xvideos.red/video"
                   UNSTRING PIN1-REC (39:11) DELIMITED BY "/"
                       INTO WK-ID
           ELSE
      *    *** こちらは、.COM のみと思われる
                   UNSTRING  PIN1-REC (38:11) DELIMITED BY "/"
                       INTO WK-ID
           END-IF

           SEARCH  ALL TBL01-AREA
               AT END
                   CONTINUE
               WHEN TBL01-ID (TBL01-IDX)  =  WK-ID

                   IF      PIN1-REC (30:3) =   "red"
                       IF  TBL01-RED (TBL01-IDX) NOT = SPACE
      *                 AND SW-RED = "Y"
                           DISPLAY WK-PGM-NAME " ダブリ WK-ID=" WK-ID
                         " TBL01-RED (TBL01-IDX)=" TBL01-RED (TBL01-IDX)
                         " TBL01-COM (TBL01-IDX)=" TBL01-COM (TBL01-IDX)
                         " PIN1-REC (30:3)=" PIN1-REC (30:3)

                           MOVE    WK-ID       TO      POT5-REC
                           WRITE   POT5-REC
                           ADD     1           TO      WK-POT5-CNT

                       ELSE
                           CONTINUE
                       END-IF
                   ELSE
                       IF  TBL01-COM (TBL01-IDX) NOT = SPACE
      *                 AND SW-RED = "N"
                           DISPLAY WK-PGM-NAME " ダブリ WK-ID=" WK-ID
                         " TBL01-RED (TBL01-IDX)=" TBL01-RED (TBL01-IDX)
                         " TBL01-COM (TBL01-IDX)=" TBL01-COM (TBL01-IDX)
                         " PIN1-REC (30:3)=" PIN1-REC (30:3)

                           MOVE    WK-ID       TO      POT5-REC
                           WRITE   POT5-REC
                           ADD     1           TO      WK-POT5-CNT
                       ELSE
                           CONTINUE
                       END-IF
                   END-IF

                   IF      PIN1-REC (30:3) =   "red"
                           MOVE    "Y"         TO  TBL01-RED (TBL01-IDX)
                   ELSE
                           MOVE    "Y"         TO  TBL01-COM (TBL01-IDX)
                   END-IF
           END-SEARCH
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-IMG
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-IMG-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE  COUNT WK-TITLE-LEN
                           WK-IMG    COUNT WK-IMG-LEN
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

           IF      TBL01-IDX   >       I-MAX
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

      *    *** ID=NNNNNNNN が標準だが、
      *    *** /videoNNNNN/
      *    *** /videoNNNNNN/ 等もある、将来の変更の為、N１０桁
      *    *** /videoNNNNNNNNNN/ まで対応する
           IF      WK-TITLE (1:29) = "https://www.xvideos.com/video"
                OR WK-TITLE (1:29) = "https://www.xvideos.red/video"
                   UNSTRING WK-TITLE (30:11) DELIMITED BY "/"
                       INTO TBL01-ID (TBL01-IDX)
           ELSE
                   UNSTRING WK-TITLE (29:11) DELIMITED BY "/"
                       INTO TBL01-ID (TBL01-IDX)
           END-IF

           MOVE    WK-IMG      TO      TBL01-IMG      (TBL01-IDX)
           MOVE    WK-IMG-LEN  TO      TBL01-IMG-LEN  (TBL01-IDX)
           MOVE    PIN2-REC    TO      TBL01-PIN2-REC (TBL01-IDX)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1,POT4 HEAD
       S040-10.

           MOVE    "% XVI,"    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "% XVIS,"   TO      POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

      *    *** 下記、WRITE 止める
           IF      SW-SKIP     NOT =   "Y"

                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-Search" TO  POT1-REC (13:13)

      *    *** WRITE POT1 OR POT4
                   PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
                   PERFORM S090-10     THRU    S090-EX
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 2
       S042-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    "aduxvi-Search-Actress-name"
                               TO      POT1-REC (13:26)

      *    *** WRITE POT1 OR POT4
           PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
           PERFORM S090-10     THRU    S090-EX
           .
       S042-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 3
       S044-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    "aduxvi-Search-Num-Alpha"
                               TO      POT1-REC (13:23)

      *    *** WRITE POT1 OR POT4
           PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
           PERFORM S090-10     THRU    S090-EX
           .
       S044-EX.
           EXIT.

      *    *** READ PIN3
       S050-10.

           READ    PIN3-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ

           IF      WK-PIN3-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF
           .
       S050-EX.
           EXIT.

      *    *** 検索キーワード追加データ出力
      *    *** WRITE POT1 HEAD
       S052-10.

           MOVE    PIN3-REC    TO      POT1-REC

           ADD     WK-PIN3-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

      *     MOVE    "https://www.xvideos.com/?k="  
           MOVE    "https://www.xvideos.red/?k="  
                               TO      POT1-REC (J:27)
           ADD     27          TO      J

           MOVE    PIN3-REC (1:WK-PIN3-LEN)  
                               TO      POT1-REC (J:WK-PIN3-LEN)
           ADD     WK-PIN3-LEN TO      J

           MOVE    "&premium=1"
                               TO      POT1-REC (J:10)
           ADD     10          TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1 OR POT4
           PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
           PERFORM S090-10     THRU    S090-EX
           .
       S052-EX.
           EXIT.

      *    *** READ PIN4
       S060-10.

           READ    PIN4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ

           IF      WK-PIN4-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN4-F READ ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF
           .
       S060-EX.
           EXIT.

      *    *** 検索女優名追加データ出力
      *    *** WRITE POT1 HEAD
       S062-10.

           MOVE    PIN4-REC    TO      POT1-REC

           ADD     WK-PIN4-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

      *     MOVE    "https://www.xvideos.com/?k="  
           MOVE    "https://www.xvideos.red/?k="  
                               TO      POT1-REC (J:27)
           ADD     27          TO      J

           MOVE    PIN4-REC (1:WK-PIN4-LEN)  
                               TO      POT1-REC (J:WK-PIN4-LEN)
           ADD     WK-PIN4-LEN TO      J

           MOVE    "&premium=1"
                               TO      POT1-REC (J:10)
           ADD     10          TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1 OR POT4
           PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
           PERFORM S090-10     THRU    S090-EX
           .
       S062-EX.
           EXIT.

      *    *** READ PIN5
       S070-10.

           READ    PIN5-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
                   UNSTRING PIN5-REC
                           DELIMITED BY "," OR SPACE
                           INTO
                           WK-PIN5-ITEM1  COUNT WK-PIN5-ITEM1-LEN
                           WK-PIN5-ITEM2  COUNT WK-PIN5-ITEM2-LEN
      *             IF      PIN5-REC (13:5) = "tag-a"
      *    *** ジャパリに変更
                   IF      PIN5-REC (1:12) = X"E382B8E383A3E38391E383AA"
                           MOVE    "Y"         TO      SW-TAGA
                   END-IF
           END-READ

           IF      WK-PIN5-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN5-F READ ERROR STATUS="
                           WK-PIN5-STATUS
                   STOP    RUN
           END-IF
           .
       S070-EX.
           EXIT.

      *    *** 検索NUM,ALPHA追加データ出力
      *    *** WRITE POT1 HEAD
       S072-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    PIN5-REC (1:WK-PIN5-ITEM1-LEN)
                               TO      POT1-REC (1:WK-PIN5-ITEM1-LEN)

      *     ADD     WK-PIN5-LEN 1 GIVING J
           ADD     WK-PIN5-ITEM1-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

      *     MOVE    "https://www.xvideos.com/?k="  
           MOVE    "https://www.xvideos.red/?k="  
                               TO      POT1-REC (J:27)
           ADD     27          TO      J

      *     MOVE    PIN5-REC (1:WK-PIN5-LEN)  
      *                         TO      POT1-REC (J:WK-PIN5-LEN)
      *     ADD     WK-PIN5-LEN TO      J

      *    *** PIN5 tag-a 以降はITEM2 英語を検索キーにする
      *    *** 表示はITEM1の日本語にする
           IF      SW-TAGA     =       "N"
                   MOVE    PIN5-REC (1:WK-PIN5-ITEM1-LEN)  
                               TO      POT1-REC (J:WK-PIN5-ITEM1-LEN)
                   ADD     WK-PIN5-ITEM1-LEN TO J
           ELSE
                   MOVE    WK-PIN5-ITEM2 (1:WK-PIN5-ITEM2-LEN)  
                               TO      POT1-REC (J:WK-PIN5-ITEM2-LEN)
                   ADD     WK-PIN5-ITEM2-LEN TO J
           END-IF

           MOVE    "&premium=1"
                               TO      POT1-REC (J:10)
           ADD     10          TO      J

           MOVE    " ,OF ,"  
                               TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1 OR POT4
           PERFORM S080-10     THRU    S080-EX

      *    *** WRITE POT4
           PERFORM S090-10     THRU    S090-EX
           .
       S072-EX.
           EXIT.

      *    *** WRITE POT1 OR POT4
       S080-10.

      *    *** 初期値 SW-SONOTA = "N" ,SW-BOTH = "Y"

      *    *** #aduxvi女優名あーお の時、SW-BOTH = "N"
      *    *** =>     SW-SONOTA = "N" ,SW-BOTH = "N"

      *    *** #aduxvi可愛い の時、SW-SONOTA = "Y"
      *    *** =>     SW-SONOTA = "Y" ,SW-BOTH = "N"

           IF      SW-SONOTA   =       "N"
                   WRITE   POT1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           ELSE
      *    *** ブックマーク ,chrome-native:
      *    *** 不要なデータカットする
               IF  POT1-REC (1:18) =   
                   X"E38396E38383E382AFE3839EE383BCE382AF"
                 AND
                   POT1-REC (19:16) =  " ,chrome-native:"
                   CONTINUE
               ELSE

      *    *** img画面表示がコントロール出来るようになったので、
      *    *** その他以降も全データ、POT1-REC に出力に変更
                   WRITE   POT1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
      *
                   WRITE   POT4-REC    FROM    POT1-REC

                   IF      WK-POT4-STATUS =    ZERO
                           ADD     1           TO      WK-POT4-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT4-F WRITE ERROR STATUS="
                                   WK-POT4-STATUS
                           STOP    RUN
                   END-IF
               END-IF
           END-IF
           .
       S080-EX.
           EXIT.

      *    *** WRITE POT4
       S090-10.

           WRITE   POT4-REC    FROM    POT1-REC

           IF      WK-POT4-STATUS =    ZERO
                   ADD     1           TO      WK-POT4-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT4-F WRITE ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF
           .
       S090-EX.
           EXIT.

      *    *** WRITE POT4
       S092-10.

      *    *** aduxvi! から aduxviプロフィール・ＸＸ 等までPOT4へも出力
           IF      SW-BOTH     =       "Y"
                   WRITE   POT4-REC    FROM    POT1-REC
                   IF      WK-POT4-STATUS =    ZERO
                           ADD     1           TO      WK-POT4-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT4-F WRITE ERROR STATUS="
                                   WK-POT4-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S092-EX.
           EXIT.

      *    *** html 解析
       S100-10.

           EVALUATE TRUE

               WHEN PIN1-REC (1:3) = "<H3"
                   MOVE    "Y"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   AND
      *    *** H3 の #aduxvi は残す
                    PIN1-REC (1:10) = "#aduxvi-br"
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      POT1-REC (13:WK-PIN1-LEN)

      *    *** WRITE POT1 OR POT4
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3
      *    *** WRITE POT4
                   PERFORM S092-10     THRU    S092-EX

               WHEN SW-H3 = "Y"
                   AND
      *    *** H3 の #XXX はカット
                    PIN1-REC (1:1) = "#"
                   MOVE    "N"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      POT1-REC (13:WK-PIN1-LEN)

      *    *** WRITE POT1 OR POT4
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3
      *    *** WRITE POT4
                   PERFORM S092-10     THRU    S092-EX

               WHEN PIN1-REC (1:9) = "<A HREF="""
                   MOVE    "Y"         TO      SW-A
                   MOVE    SPACE       TO      WK-A
                   MOVE    ZERO        TO      I2
                   PERFORM VARYING I FROM 10 BY 1
                           UNTIL I > WK-PIN1-LEN
                           OR PIN1-REC (I:1) = '"'
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1) TO   WK-A (I2:1)
                   END-PERFORM
                   MOVE    I2          TO      WK-A-LEN

      *    *** 前ステップで、<A HREF=を取り出したら
               WHEN SW-A  = "Y"
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    ZERO        TO      I3
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-PIN1-LEN
                              OR PIN1-REC (I:7) = "http://"
                              OR ( I NOT = 1 
                               AND PIN1-REC (I:8) = "https://" )

                       EVALUATE TRUE
      *    *** タイトル先頭にhttps:// ならSPACEまでカット
                           WHEN I = 1
                            AND PIN1-REC (I:8) = "https://"
                               PERFORM VARYING I5 FROM 9 BY 1
                                   UNTIL I5 > WK-PIN1-LEN
                                      OR PIN1-REC (I5:1) = SPACE
      *    *** UTF8 漢字スペース
                                      OR PIN1-REC (I5:3) = X"E38080"
                               END-PERFORM
                               IF      PIN1-REC (I5:3) = X"E38080"
                                   ADD     I5 2        GIVING  I
                               ELSE
                                   MOVE    I5          TO      I
                               END-IF
                           WHEN PIN1-REC (I:13) = "- XVIDEOS.COM"
                               ADD     12          TO      I
                               IF      PIN1-REC (1:1) =    "#"

      *    *** HENKAN=US UTF8 => SJIS
                                   MOVE    "CHANGE"    TO WDE05-ID
                                   MOVE    "US"        TO WDE05-HENKAN
                                   MOVE    "AA"        TO WDE05-MODE
                                   MOVE    30          TO WDE05-BUF1-LEN
                                   MOVE    20          TO WDE05-BUF2-LEN
                                   CALL    "DECODE05"  USING
                                           WDE05-DECODE05-AREA
                                           PIN1-REC (1:30)
                                           WK-DISP
                                   DISPLAY WK-PGM-NAME 
                                   " PIN1-F (1,1) = '#' タイトル ERROR "
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                                   " PIN1-REC (1:20) =" WK-DISP
                                   STOP    RUN
                               END-IF
                           WHEN PIN1-REC (I:12) = "- XV PREMIUM"
                               ADD     11          TO      I
                               IF      PIN1-REC (1:1) =    "#"

      *    *** HENKAN=US UTF8 => SJIS
                                   MOVE    "CHANGE"    TO WDE05-ID
                                   MOVE    "US"        TO WDE05-HENKAN
                                   MOVE    "AA"        TO WDE05-MODE
                                   MOVE    30          TO WDE05-BUF1-LEN
                                   MOVE    20          TO WDE05-BUF2-LEN
                                   CALL    "DECODE05"  USING
                                           WDE05-DECODE05-AREA
                                           PIN1-REC (1:30)
                                           WK-DISP
                                   DISPLAY WK-PGM-NAME 
                                   " PIN1-F (1,1) = '#' タイトル ERROR "
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                                   " PIN1-REC (1:20) =" WK-DISP
                                   STOP    RUN
                               END-IF
      *    *** '
                           WHEN PIN1-REC (I:5) = "&#39;"
                               ADD     4           TO      I
                           WHEN OTHER
                               ADD     1           TO      I3
                               IF      PIN1-REC (I:1) = ","
                                   MOVE    "."    TO    PIN1-REC (I:1)
                               END-IF
                               MOVE    PIN1-REC (I:1) TO POT1-REC (I3:1)
                       END-EVALUATE
                   END-PERFORM

                   ADD     2           TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   ADD     1           TO      I3
                   MOVE    WK-A (1:I2) TO      POT1-REC (I3:I2)

                   ADD     2 I2        TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   MOVE    "N"         TO      SW-SEARCH
                   IF      WK-A (1:29) = "https://www.xvideos.com/video"
                        OR WK-A (1:28) = "http://www.xvideos.com/video"
                        OR WK-A (1:29) = "https://www.xvideos.red/video"
      *    *** IMG SET
                       PERFORM S110-10     THRU    S110-EX
                   ELSE
                       ADD     2           TO      I3
                       MOVE    ","         TO      POT1-REC (I3:1)
                   END-IF

                   IF      SW-SEARCH   =       "Y"
                       EVALUATE TRUE

      *    *** red,com の両方有る時、COMを出力、WK-ID同じ物REDは出力しない
                           WHEN TBL01-RED (TBL01-IDX) = "Y"
                            AND TBL01-COM (TBL01-IDX) = "Y"
                               IF      WK-A (21:3) = "red"
                                   CONTINUE
                               ELSE
      *    *** WRITE POT1 OR POT4
                                   PERFORM S080-10     THRU    S080-EX
      *    *** WRITE POT4
                                   PERFORM S092-10     THRU    S092-EX
                                   MOVE    "*"         TO
                                                  TBL01-SET (TBL01-IDX)
                               END-IF

      *    *** red しかない時、REDを出力
                           WHEN TBL01-RED (TBL01-IDX) = "Y"
                            AND TBL01-COM (TBL01-IDX) = SPACE
      *    *** WRITE POT1 OR POT4
                               PERFORM S080-10     THRU    S080-EX
      *    *** WRITE POT4
                               PERFORM S092-10     THRU    S092-EX
                               MOVE    "*"         TO
                                                  TBL01-SET (TBL01-IDX)

      *    *** com しかない時、COMを出力
                           WHEN TBL01-RED (TBL01-IDX) = SPACE
                            AND TBL01-COM (TBL01-IDX) = "Y"
      *    *** WRITE POT1 OR POT4
                               PERFORM S080-10     THRU    S080-EX
      *    *** WRITE POT4
                               PERFORM S092-10     THRU    S092-EX
                               MOVE    "*"         TO
                                                  TBL01-SET (TBL01-IDX)

      *    *** この組合は無いはず
                           WHEN OTHER
                               CONTINUE
                       END-EVALUATE
                   ELSE
      *    *** WRITE POT1 OR POT4
                       PERFORM S080-10     THRU    S080-EX
      *    *** WRITE POT4
                       PERFORM S092-10     THRU    S092-EX
                   END-IF
                   MOVE    "N"         TO      SW-A

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S100-EX.
           EXIT.

      *    *** IMG SET
       S110-10.

           IF      WK-A (1:29) = "https://www.xvideos.com/video"
                OR WK-A (1:29) = "https://www.xvideos.red/video"
                   UNSTRING WK-A (30:11) DELIMITED BY "/"
                       INTO WK-ID
           ELSE
                   UNSTRING  WK-A (29:11) DELIMITED BY "/"
                       INTO WK-ID
           END-IF

           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH
                   ADD     2           TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   MOVE    WK-A (1:WK-A-LEN) TO POT2-REC
                   MOVE    " , , ,"    TO      POT2-REC (WK-A-LEN + 1:6)
                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF

               WHEN TBL01-ID (TBL01-IDX)  =  WK-ID

                   MOVE    "Y"         TO      SW-SEARCH
                   ADD     1           TO      I3
                   MOVE    TBL01-IMG-LEN (TBL01-IDX)
                                       TO      I4
                   MOVE    TBL01-IMG (TBL01-IDX) (1:I4)
                                       TO      POT1-REC (I3:I4)
      *             MOVE    "*"         TO      TBL01-SET (TBL01-IDX)

                   ADD     2 I4        TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)
           END-SEARCH
           .
       S110-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR TBL01-ID (I) = HIGH-VALUE
      *    *** TBL01-SET (I) = "*" マッチング済
                   IF      TBL01-SET (I) =     "*"
      *                     WRITE   POT3-REC    FROM    TBL01-ID (I) 
      *    *** TEST10 INPUT ブックマーク 逐次削除されるので、
      *    *** 画像リンクデータも整理するため、TEST70.POT3 追加した
      *    *** 次回 TEST70.PIN2 インプットにする
                       WRITE   POT3-REC    FROM    TBL01-PIN2-REC (I) 

                       IF      WK-POT3-STATUS =    ZERO
                               ADD     1           TO      WK-POT3-CNT
                       ELSE
                               DISPLAY WK-PGM-NAME 
                                       " POT3-F WRITE ERROR STATUS="
                                       WK-POT3-STATUS
                               STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM

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

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F CLOSE ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F CLOSE ERROR STATUS="
                           WK-PIN5-STATUS
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
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 ｹﾝｽｳ = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 ｹﾝｽｳ = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-PIN5-CNT TO      WK-PIN5-CNT-E
           DISPLAY WK-PGM-NAME " PIN5 ｹﾝｽｳ = " WK-PIN5-CNT-E
                   " (" WK-PIN5-F-NAME ")"
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
           MOVE    WK-POT5-CNT TO      WK-POT5-CNT-E
           DISPLAY WK-PGM-NAME " POT5 ｹﾝｽｳ = " WK-POT5-CNT-E
                   " (" WK-POT5-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
