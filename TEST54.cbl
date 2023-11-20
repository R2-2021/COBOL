      *    *** YouTube html ｗｉｋｉ汎用 作成
      *    ***
      *    *** TEST53 TEST55 TEST56 TEST57 TEST70 TEST74 TEST78 TEST83
      *    ***   ↓     ↓     ↓     ↓     ↓     ↓     ↓     ↓
      *    *** TEST54 TEST53 TEST53 TEST53 TEST53 TEST53 TEST53 TEST53
      *    ***          ↓     ↓     ↓     ↓     ↓     ↓     ↓
      *    ***        TEST54 TEST54 TEST54 TEST54 TEST53 TEST54 TEST54
      *    ***
      *    *** TEST53 01-12
      *    *** TEST55 13 芸能人 ひらがな順
      *    *** TEST56 14 2000年代生まれの女優 名前順、誕生日順
      *    *** TEST57 15 アイドル大図鑑（グループ順）、（名前順)
      *    *** TEST60 18 XVI 国別
      *    *** TEST74 24 Qosmio_DIR
      *    *** TEST70 21 XVI （女優分）
      *    *** TEST78 25 XVI2
      *    *** TEST83 28 expo_jam_2018
      *    *** TEST89 29 DMM 検索結果　画像拡大表示
      *    *** TEST70 30 XVIS (その他分）
      *    *** TEST70 32 Youtube 動画サムネイル拡大

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST54.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ｗｉｋｉデータ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** twiiter,instagram等 データ
      *    *** 
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
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST54  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST53.POT1".

           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST54.PIN2".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST54.POT1".
           03  WK-POT1-F-NAME.
             05 PIC X(023) VALUE "C:\Users\xxxx\OneDrive\".
             05 PIC X(012) VALUE "ドキュメント".
             05 PIC X(013) VALUE "\HTML\YouTube".
             05 PIC X(004) VALUE "汎用".
             05 PIC X(006) VALUE "\index".
             05  WK-POT1-F-NAME21 PIC X(200) VALUE SPACE.

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT2    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT2-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** YouTube
           03  WK-TITLE-HEAD.
             05                PIC  X(200) VALUE SPACE.

           03  WK-TITLE2       PIC  X(200) VALUE SPACE.
           03  WK-SITE-TBL.
             05  WK-SITE       OCCURS 20
                               PIC  X(500) VALUE SPACE.
           03  WK-SITE1        PIC  X(500) VALUE SPACE.
           03  WK-SITE2        PIC  X(500) VALUE SPACE.
           03  WK-KENSAKU      PIC  X(500) VALUE SPACE.
           03  WK-WIDTH        PIC  9(002) VALUE 8.
           03  WK-K2           PIC  9(010) VALUE ZERO.

           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-TITLE-A      PIC  X(500) VALUE SPACE.
           03  WK-ITEM3        PIC  X(500) VALUE SPACE.
           03  WK-ITEM4        PIC  X(500) VALUE SPACE.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-A-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM3-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM4-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-LEFT-POS     PIC  9(004) VALUE ZERO.
           03  WK-SU           PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-SITE1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-FILE         PIC  X(002) VALUE SPACE.
      *    *** 日本の女優一覧2000年代生まれ（誕生日順）
           03  WK-2000JYOYU. 
        05 PIC X(020) VALUE X"E697A5E69CACE381AEE5A5B3E584AAE4B880E8A6".
        05 PIC X(020) VALUE X"A732303030E5B9B4E4BBA3E7949FE381BEE3828C".
            05 PIC X(018) VALUE X"EFBC88E8AA95E7949FE697A5E9A086EFBC89".
           03  WK-SHIROUTO.
        05 PIC X(020) VALUE X"202D20E7B4A0E4BABAE381A8E38381E383A3E383".
        05 PIC X(019) VALUE X"B3E3838DE383ABE381AEE3839AE383BCE382B8".
           03  WK-SITE-NAME    PIC  X(200) VALUE SPACE.
           03  WK-MODEL-CHANNELS.
      *    *** のモデル・チャンネルのページ 
             05  PIC  X(020) VALUE
                 X"E381AEE383A2E38387E383ABE383BBE38381E383".
             05  PIC  X(022) VALUE
                 X"A3E383B3E3838DE383ABE381AEE3839AE383BCE382B8".
      *    *** aduxviチャンネル・イメージ
           03  WK-CHANNEL-IMAGE.
             05  PIC  X(018) VALUE
                 X"616475787669E38381E383A3E383B3E3838D".
             05  PIC  X(018) VALUE
                 X"E383ABE383BBE382A4E383A1E383BCE382B8".
      *    *** aduxviプロフィール・アニメ
           03  WK-PROFILE-ANIME.
             05  PIC  X(018) VALUE
                 X"616475787669E38397E383ADE38395E382A3".
             05  PIC  X(018) VALUE
                 X"E383BCE383ABE383BBE382A2E3838BE383A1".
     

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=SU (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(002) VALUE "US".

      *    *** 変換前 が入っているデータ
      * 01  WDE03-BUF1          PIC  X(001) ANY LENGTH.

      *    *** 変換前のデータの長さ
       01  WDE03-BUF1-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** 16進数 変換後 が入っているデータ
      *    *** 富士通のNETCOBOLの資料によると、項目最大長は64770バイトである
       01  WDE03-BUF2.
      *    *** LLL...
           03  WDE03-BUF2-L-TBL.
             05  WDE03-BUF2-L  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** RRR...
           03  WDE03-BUF2-R-TBL.
             05  WDE03-BUF2-R  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** LRLR...
           03  WDE03-BUF2-LR-TBL.
             05  WDE03-BUF2-LR-TBL2 OCCURS 65536.
               07  WDE03-BUF2-L2 PIC  X(001) VALUE SPACE.
               07  WDE03-BUF2-R2 PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-WA           PIC  X(001) VALUE "N".
           03  SW-CHANGE       PIC  X(001) VALUE "N".
           03  SW-TIE-UP       PIC  X(001) VALUE "N".
           03  SW-DO           PIC  X(001) VALUE "N".
           03  SW-WO           PIC  X(001) VALUE "N".
           03  SW-IDOLZUKAN    PIC  X(001) VALUE "N".
           03  SW-YOUTUBE      PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-TITLE
                               INDEXED BY TBL01-IDX.
             05  TBL01-TITLE   PIC  X(500) VALUE HIGH-VALUE.
             05  TBL01-TITLE2-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-IMG     PIC  X(500) VALUE HIGH-VALUE.
             05  TBL01-IMG-LEN BINARY-LONG SYNC VALUE ZERO.
      *    *** PIN2 twiiter,instagram
             05  TBL01-SITE    OCCURS 20
                               PIC  X(500) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN 1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** コード変換 UTF8=>SJIS
           PERFORM S040-10     THRU    S040-EX

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
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   EVALUATE PIN1-REC (1:1)
                       WHEN "%"
                           CONTINUE
                       WHEN "#"
      *    *** #NN レコード編集3
                           PERFORM S130-10     THRU    S130-EX
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
                   EVALUATE TRUE
                       WHEN PIN1-REC (1:1) = "$"
                           CONTINUE
                       WHEN PIN1-REC (1:1) = "%"
                           CONTINUE
                       WHEN PIN1-REC (1:1) = "#"
                           IF      PIN1-REC (1:4) =    "#001"
      *    *** #NN レコード編集1
                                   PERFORM S110-10     THRU    S110-EX
                           ELSE
                               IF      PIN1-REC (9:10) =   "#aduxvi-br"
                                                        OR "#aduDMM-br"
                                   CONTINUE
                               ELSE
      *    *** #NN レコード編集2
                                   PERFORM S120-10     THRU    S120-EX
                               END-IF
                           END-IF
      *    *** (1:1) = SPACE はカット
                       WHEN PIN1-REC (1:1) = " " AND WK-FILE NOT = "32"
                           CONTINUE
                       WHEN OTHER
      *    *** <td> データ出力
                           IF      WK-TITLE    NOT =  SPACE
                                   PERFORM S100-10     THRU    S100-EX
                           END-IF
                   END-EVALUATE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** WRITE POT1 (HTML 後データ出力)
           PERFORM S070-10     THRU    S070-EX

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

           MOVE    "O"         TO      WFD-ID
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

           DISPLAY "OPEN POT1=" WK-POT1-F-NAME

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE-A
                                       WK-ITEM3
                                       WK-ITEM4
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE-A-LEN
                                       WK-ITEM3-LEN
                                       WK-ITEM4-LEN

           IF      WK-PIN1-STATUS =    ZERO
                   IF      SW-FIRST    =       "Y"
                       ADD     1           TO      WK-PIN1-CNT
                   ELSE
                       ADD     1           TO      WK-PIN1-CNT2
                   END-IF
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-TITLE-A  COUNT WK-TITLE-A-LEN
                           WK-ITEM3    COUNT WK-ITEM3-LEN
                           WK-ITEM4    COUNT WK-ITEM4-LEN

      *    *** 後ろスペースカット
                   IF      WK-TITLE (WK-TITLE-LEN:1) = SPACE
                           ADD     -1          TO      WK-TITLE-LEN
                       IF      WK-TITLE (WK-TITLE-LEN:1) = SPACE
                           ADD     -1          TO      WK-TITLE-LEN
                       END-IF
                   END-IF
                   IF      WK-TITLE-A (WK-TITLE-A-LEN:1) = SPACE
                           ADD     -1          TO      WK-TITLE-A-LEN
                       IF      WK-TITLE-A (WK-TITLE-A-LEN:1) = SPACE
                           ADD     -1          TO      WK-TITLE-A-LEN
                       END-IF
                   END-IF

                   IF      WK-PIN1-CNT =       1
                           MOVE    PIN1-REC (2:2) TO     WK-WIDTH
                           MOVE    PIN1-REC (4:2) TO     SW-YOUTUBE
                           MOVE    PIN1-REC (6:2) TO     WK-FILE
      *    *** 21=XVI,22=DMM,30=XVIS
      *    *** 上記のWK-FILEの時でtag-aより前は、横８列にする
                           IF      WK-FILE     =       "21" OR "22"
                                                    OR "30"
                                   MOVE    "08"        TO      WK-WIDTH
                           END-IF
                           MOVE    WK-TITLE (9:) TO      WK-TITLE-HEAD
                           IF      WK-TITLE-HEAD (1:27) =
      *    *** アイドル大図鑑（グ
               X"E382A2E382A4E38389E383ABE5A4A7E59BB3E99191EFBC88E382B0"
      *    *** 日本の女優一覧2000年代生まれ（誕生日順）
                               OR  WK-TITLE-HEAD (1:58) =  WK-2000JYOYU
                               OR  WK-TITLE-HEAD (1:07) =  "E-girls"
      *    *** 楽器分類別一覧
                               OR  PIN1-REC (6:2)       =  "19"
      *    *** XVI , DMM
                               OR  PIN1-REC (6:2)       =  "21" OR "22"
      *    *** Qosmio
                               OR  PIN1-REC (6:2)       =  "24"
      *    *** XVI2
                               OR  PIN1-REC (6:2)       =  "25"
      *    *** XVIS
                               OR  PIN1-REC (6:2)       =  "30"
      *    *** 31=YoutubeChannel
                               OR  PIN1-REC (6:2)       =  "31"
      *    *** 32=Youtube 動画サムネイル拡大
                               OR  PIN1-REC (6:2)       =  "32"
                               MOVE    "Y"         TO      SW-IDOLZUKAN
                           END-IF
                           
                   END-IF

                   IF      PIN1-REC (1:1) =      "#"
      *    *** 21=XVI,22=DMM,30=XVIS
      *    *** 上記のWK-FILEの時でtag-a以降は、横５列にする
                       IF  ((( WK-FILE     =       "21" OR "30" )
                          AND  PIN1-REC (2:3) >=   "045" )
                          OR  ( WK-FILE     =       "22"
                          AND  PIN1-REC (2:3) >=   "019" ))
                         AND ( SW-FIRST    =       "N" )
                               MOVE    "05"        TO      WK-WIDTH
                       END-IF
      *    *** シングル
                       IF PIN1-REC (9:12) = X"E382B7E383B3E382B0E383AB" 

      *    *** オリジナルアルバム
                       OR PIN1-REC (9:27) =
               X"E382AAE383AAE382B8E3838AE383ABE382A2E383ABE38390E383A0"

      *    *** ベストアルバム
                       OR PIN1-REC (9:21) =
               X"E38399E382B9E38388E382A2E383ABE38390E383A0"
                           MOVE    "Y"           TO      SW-CHANGE
                       ELSE
                           MOVE    "N"           TO      SW-CHANGE
                       END-IF
                   END-IF

                   IF      SW-CHANGE   =       "Y"
      *    *** 曲名を先頭にする
                       MOVE    WK-ITEM3    TO      WK-TITLE
                       MOVE    WK-ITEM3-LEN TO     WK-TITLE-LEN
                   END-IF

                   IF      PIN1-REC (1:1) =      "#"
      *    *** タイアップ
                           IF      PIN1-REC (9:15) =
                                   X"E382BFE382A4E382A2E38383E38397"
                           MOVE    "Y"           TO      SW-TIE-UP
                       ELSE
                           MOVE    "N"           TO      SW-TIE-UP
                       END-IF
                   END-IF

                   IF      SW-TIE-UP =       "Y"
      *    *** タイアップ、２項目無い時、曲名ないので、SPACEにする
                           IF      WK-TITLE-A  =       SPACE
                               MOVE    SPACE       TO      WK-TITLE
                           END-IF
                   END-IF

      *    *** ジャパリど　有
                   IF      PIN1-REC (1:5) =    "$DO=Y"
                           MOVE    "Y"         TO      SW-DO
                   END-IF

      *    *** ジャパリを　有
                   IF      PIN1-REC (1:5) =    "$WO=Y"
                           MOVE    "Y"         TO      SW-WO
                   END-IF
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F

           MOVE    SPACE       TO      WK-TITLE2
                                       WK-SITE-TBL
           MOVE    ZERO        TO      WK-SITE1-LEN

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT

      *    *** 256バイトまでしか入らない
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE2     COUNT WK-TITLE2-LEN
                           WK-SITE (001) COUNT WK-SITE1-LEN
                           WK-SITE (002)
                           WK-SITE (003)
                           WK-SITE (004)
                           WK-SITE (005)
                           WK-SITE (006)
                           WK-SITE (007)
                           WK-SITE (008)
                           WK-SITE (009)
                           WK-SITE (010)
                           WK-SITE (011)
                           WK-SITE (012)
                           WK-SITE (013)
                           WK-SITE (014)
                           WK-SITE (015)
                           WK-SITE (016)
                           WK-SITE (017)
                           WK-SITE (018)
                           WK-SITE (019)
                           WK-SITE (020)
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
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
           MOVE    WK-TITLE2-LEN TO    TBL01-TITLE2-LEN (TBL01-IDX)

           IF      WK-SITE (01)(WK-SITE1-LEN - 4:5) = ".jpg " OR ".svg "
                OR WK-SITE (01)(WK-SITE1-LEN - 3:4) = ".jpg" OR ".svg "
                   MOVE    WK-SITE (01) TO     TBL01-IMG  (TBL01-IDX)
                   MOVE    SPACE       TO      TBL01-SITE (TBL01-IDX 1)
           ELSE
                   MOVE    SPACE TO    TBL01-IMG  (TBL01-IDX)
                   MOVE    WK-SITE (01) TO     TBL01-SITE (TBL01-IDX 1)
           END-IF

           MOVE    WK-SITE (02) TO     TBL01-SITE (TBL01-IDX 2)
           MOVE    WK-SITE (03) TO     TBL01-SITE (TBL01-IDX 3)
           MOVE    WK-SITE (04) TO     TBL01-SITE (TBL01-IDX 4)
           MOVE    WK-SITE (05) TO     TBL01-SITE (TBL01-IDX 5)
           MOVE    WK-SITE (06) TO     TBL01-SITE (TBL01-IDX 6)
           MOVE    WK-SITE (07) TO     TBL01-SITE (TBL01-IDX 7)
           MOVE    WK-SITE (08) TO     TBL01-SITE (TBL01-IDX 8)
           MOVE    WK-SITE (09) TO     TBL01-SITE (TBL01-IDX 9)
           MOVE    WK-SITE (10) TO     TBL01-SITE (TBL01-IDX 10)
           MOVE    WK-SITE (11) TO     TBL01-SITE (TBL01-IDX 11)
           MOVE    WK-SITE (12) TO     TBL01-SITE (TBL01-IDX 12)
           MOVE    WK-SITE (13) TO     TBL01-SITE (TBL01-IDX 13)
           MOVE    WK-SITE (14) TO     TBL01-SITE (TBL01-IDX 14)
           MOVE    WK-SITE (15) TO     TBL01-SITE (TBL01-IDX 15)
           MOVE    WK-SITE (16) TO     TBL01-SITE (TBL01-IDX 16)
           MOVE    WK-SITE (17) TO     TBL01-SITE (TBL01-IDX 17)
           MOVE    WK-SITE (18) TO     TBL01-SITE (TBL01-IDX 18)
           MOVE    WK-SITE (19) TO     TBL01-SITE (TBL01-IDX 19)
           MOVE    WK-SITE (20) TO     TBL01-SITE (TBL01-IDX 20)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** コード変換 UTF8=>SJIS
       S040-10.

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    200         TO      WDE05-BUF2-LEN
           COMPUTE WDE05-BUF1-LEN = 200 - 5
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-TITLE (9:)
                                       WK-POT1-F-NAME21

           PERFORM VARYING I FROM 200 BY -1
                   UNTIL I < 2
                      OR WK-POT1-F-NAME21 (I:1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           MOVE    ".html"     TO      WK-POT1-F-NAME21 (I + 1:5)
          .
       S040-EX.
           EXIT.

      *    *** WRITE POT1 (HTML 前データ出力)
       S050-10.

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

           MOVE    WK-TITLE-HEAD TO    POT1-REC
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

           MOVE    WK-TITLE-HEAD TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</h1>"
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
           MOVE    ZERO        TO      WK-PIN1-CNT
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

      *    *** WK-FILE=23 お菓子系．ｃｏｍ GO TO でSEARCH SKIP の為
      *    *** １件前の情報が残っている為クリアー
           MOVE    "N"         TO      SW-SEARCH
      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   IF      WK-ITEM3    =       "OF "
                       IF      L           =       ZERO
                           ADD     1           TO      I
                       ELSE
                           CONTINUE
                       END-IF
                   ELSE
                           ADD     1           TO      I
                   END-IF
           ELSE
                   ADD     1           TO      I
           END-IF

           IF      I           >       WK-WIDTH
                   MOVE    1           TO      I
                   MOVE    "</tr>"     TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "<tr>"      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   IF      WK-ITEM3    =       "OF "
                       IF      L           =       ZERO
      *                     MOVE    '<td valign="top">' TO      POT1-REC
                           MOVE    '<td>'      TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

      *                     IF      WK-FILE     =       "25"
                               MOVE    '<p class="welcome">'
                                               TO      POT1-REC
      *                     ELSE
      *                         MOVE    '<p class="welcome2">'
      *                                         TO      POT1-REC
      *                     END-IF
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                       ELSE
                           CONTINUE
                       END-IF
                   ELSE
      *                     MOVE    '<td valign="top">' TO      POT1-REC
                           MOVE    '<td>'      TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

      *                     IF      WK-FILE     =       "25"
                           IF      WK-WIDTH     =       8
                               MOVE    '<p class="welcome">'
                                               TO      POT1-REC
                           ELSE
                               MOVE    '<p class="welcome2">'
                                               TO      POT1-REC
                           END-IF
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           ELSE
      *             MOVE    '<td valign="top">' TO      POT1-REC
                   MOVE    '<td>'      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 26=お菓子系２,23=お菓子系．ｃｏｍ
                   EVALUATE TRUE
                       WHEN WK-FILE     =      "23"
                           MOVE    '<p class="welcome03">'
                                       TO      POT1-REC
                       WHEN WK-FILE     =      "26"
      *    *** 28=expo_jam_2018
                                            OR "28"
      *    *** 29=DMM 検索
                                            OR "29"
      *    *** 32=Youtube 動画サムネイル拡大
                                            OR "32"
                           MOVE    '<p class="welcome2">'
                                       TO      POT1-REC
                       WHEN OTHER
                           MOVE    '<p class="welcome">'
                                       TO      POT1-REC
                   END-EVALUATE
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
      *    *** 29=DMM 検索
                                    OR "29"
      *    *** 32=Youtube 動画サムネイル拡大
                                    OR "32"
             IF  WK-ITEM3      =       "OF "
                 CONTINUE
             ELSE
      *    ***ここで指定した、アドレスは、単体でこのアドレスのみ表示
               IF    WK-TITLE-A (1:29) = "https://www.xvideos.com/video"
                  OR WK-TITLE-A (1:28) = "http://www.xvideos.com/video"
                  OR WK-TITLE-A (1:29) = "https://www.xvideos.red/video"
                  OR WK-TITLE-A (1:39) = 
      *    *** グローバルポルノ俳優
                     "https://www.xvideos.com/pornstars-index"
                  OR WK-TITLE-A (1:44) = 
      *    *** アダルト女優
                     "https://www.xvideos.com/porn-actresses-index"
                  OR WK-TITLE-A (1:38) = 
      *    *** 素人
                     "https://www.xvideos.com/amateurs-index"
      *            OR WK-TITLE-A (1:41) = 
      *    *** 素人とチャンネルのページ
      *               "https://www.xvideos.com/amateurs-channels"
                  OR WK-TITLE-A (1:43) = 
      *    *** チャットレディ
                     "https://www.xvideos.com/webcam-models-index"
                  OR WK-TITLE-A (1:43) = 
      *    *** アダルトモデル
                     "https://www.xvideos.com/erotic-models-index"
                  OR WK-TITLE-A (1:32) = 
      *    *** プロファイル
                     "https://www.xvideos.com/profiles"
                  OR WK-TITLE-A (1:33) = 
      *    *** チャンネル
                     "https://www.xvideos.com/channels/"
                  OR WK-TITLE-A (1:26) = 
      *    *** c/
                     "https://www.xvideos.com/c/"

      *    *** DMM
      *    *** https://www.dmm.co.jp/
                  OR ( WK-TITLE-A (1:22) = 
                     "https://www.dmm.co.jp/" 
                      AND 
                     ( WK-TITLE-A (23:7) NOT = "search/"
      *    *** 動画
                   AND WK-TITLE-A (23:22) NOT = 
                       "digital/-/list/search/" 
      *    *** -ビデオ
                   AND WK-TITLE-A (23:29) NOT = 
                       "digital/videoa/-/list/search/" 
      *    *** -素人
                   AND WK-TITLE-A (23:29) NOT = 
                       "digital/videoc/-/list/search/"
      *    *** -アニメ
                   AND WK-TITLE-A (23:28) NOT = 
                       "digital/anime/-/list/search/"
      *    *** -成人映画
                   AND WK-TITLE-A (23:31) NOT = 
                       "digital/nikkatsu/-/list/search/"
      *    *** -月額動画
                   AND WK-TITLE-A (23:22) NOT = 
                       "monthly/-/list/search/"
      *    *** -単体作品、 - 無料エロ動画
                   AND WK-TITLE-A (23:17) NOT = 
                       "litevideo/-/list/"
      *    *** -エロ動画・アダルトビデオ
                   AND WK-TITLE-A (23:24) NOT = 
                       "digital/videoa/-/list/=/"
      *    *** 女優・サムネイル
      *             AND WK-TITLE-A (23:9) NOT = 
      *                 "litevideo"
                       ))

      *    *** YOUTUBE
      *    *** https://www.youtube.com/watch?v=
                  OR   WK-TITLE-A (1:32) = 
                     "https://www.youtube.com/watch?v="

                       MOVE    "<a href='" TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-TITLE-A  TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
      *    ***WK-FILE 21=XVI,30=XVIS
                       IF      WK-FILE     =       "21" OR "30"
                         IF  WK-ITEM3 (1:11) =   "videos-best"
                          OR WK-ITEM3 (1:13) =   "videos-rating"
                           MOVE    ","         TO      POT1-REC (1:1)
                           MOVE    WK-ITEM3    TO      POT1-REC (2:)
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    WK-ITEM4    TO      WK-ITEM3
                           MOVE    WK-ITEM4-LEN TO     WK-ITEM3-LEN
                           MOVE    SPACE       TO      WK-ITEM4
                           MOVE    ZERO        TO      WK-ITEM4-LEN
                         ELSE
                           IF  WK-ITEM3 (1:7) =   "premium"
                           AND WK-ITEM4 (1:6) =   "rating"
                             MOVE    ",premium,rating" 
                                               TO      POT1-REC
                             MOVE    SPACE     TO      WK-ITEM3
                             WRITE   POT1-REC
                             ADD     1         TO      WK-POT1-CNT
                           ELSE
                             CONTINUE
                           END-IF
                         END-IF
                       END-IF

                       MOVE    "'><br>" 
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       MOVE    '<img src="'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-ITEM3    TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

      *    *** loading="lazy" は、ｉｍｇ読み込みを遅らせる
      *    *** スクロールで画像のところに来てはじめて読み込みが行われます。
                       MOVE    '" loading="lazy" alt=""'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    '><br><br>'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-TITLE    TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    "</a>" 
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       GO  TO  S100-30
               ELSE
                       CONTINUE
               END-IF
             END-IF
           END-IF

      *    *** WK-FILE=26 お菓子系 IMG有り,23=お菓子系．ｃｏｍ
           IF      WK-FILE     =       "23" OR "26"
      *    *** 28=expo_jam_2018
                                    OR "28"
      *    *** 29=DMM 検索
                                    OR "29"
      *    *** 31=YoutubeChannel
                                    OR "31"
      *    *** 32=Youtube 動画サムネイル拡大
                                    OR "32"
                   IF      WK-TITLE-A  =       SPACE
                           CONTINUE
                   ELSE
                           GO  TO  S100-20
                   END-IF
           ELSE
                   CONTINUE
           END-IF

      *    *** 15=アイドル大図鑑 名前順、グループ順
           IF      WK-FILE     =       "15"
                   IF      WK-TITLE-A  =       SPACE
                       AND WK-ITEM3 (1:1) =    SPACE
                           CONTINUE
                   ELSE
                           GO  TO  S100-20
                   END-IF
           ELSE
                   CONTINUE
           END-IF

      *    *** IMG 表示しない
      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   IF      WK-ITEM3    =       "OF "
                           GO  TO  S100-20
                   ELSE
                           PERFORM VARYING M FROM 1 BY 1
                                   UNTIL M > WK-TITLE-LEN
                               OR  WK-TITLE (M:15) =
      *    *** の検索結果
                                    X"E381AEE6A49CE7B4A2E7B590E69E9C"
                               OR  WK-TITLE (M:15) =
      *    *** を検索する
                                    X"E38292E6A49CE7B4A2E38199E3828B"
                               OR  WK-TITLE (M:14) =
      *    *** (単体作品)
                                    X"28E58D98E4BD93E4BD9CE5938129"
                               OR  WK-TITLE (M:24) =
      *    *** のモデルのページ
                   X"E381AEE383A2E38387E383ABE381AEE3839AE383BCE382B8"
                               OR  WK-TITLE (M:39) =
      *    ***  - 素人とチャンネルのページ
                                                     WK-SHIROUTO
                               OR  WK-TITLE (M:42) =
      *    *** のモデル・チャンネルのページ
                                                     WK-MODEL-CHANNELS
                               OR  WK-TITLE (M:21) =
      *    *** のアダルト女優
                           X"E381AEE382A2E38380E383ABE38388E5A5B3E584AA"
                                   CONTINUE

                           END-PERFORM

                           IF      M           >       WK-TITLE-LEN
                               MOVE    WK-TITLE-LEN TO     K2
                           ELSE
                               ADD     -1 M         GIVING K2
                           END-IF
                   END-IF
           ELSE
                   MOVE     WK-TITLE-LEN TO    K2
           END-IF

      *     PERFORM VARYING K FROM 1 BY 1
      *             UNTIL K > WK-TITLE-LEN
      *                OR WK-TITLE (K:1) = SPACE
      *             MOVE    K           TO      K2
      *     END-PERFORM

      *     SEARCH  ALL TBL01-AREA
           SET     TBL01-IDX    TO     1
           SEARCH  TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL01-TITLE (TBL01-IDX) (1:K2) =  WK-TITLE (1:K2)
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

      *     MOVE    K2          TO      WK-K2
      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "K2"        TO      WFD-ITEM
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-K2

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "WK-TITLE"  TO      WFD-ITEM
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-TITLE

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "SW-SEARCH" TO      WFD-ITEM
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 SW-SEARCH

      *    *** SAYAKA = Sayaka Tsutsumi が有る為、別人と判定する
           IF      SW-SEARCH   =       "Y"
                   IF      TBL01-TITLE2-LEN (TBL01-IDX) =  K2
                           CONTINUE
                   ELSE
                           MOVE    "N"         TO      SW-SEARCH
                   END-IF
           END-IF
           .
       S100-20.

      *    *** 26=お菓子系２,23=お菓子系．ｃｏｍ
           IF      WK-FILE     =       "26" OR "23" 
      *    *** 28=expo_jam_2018
                                    OR "28"
      *    *** 29=DMM 検索
                                    OR "29"
      *    *** 32=Youtube 動画サムネイル拡大
                                    OR "32"
      *    *** 15=アイドル大図鑑 名前順、グループ順
      *    *** 15 は、グループ名順、
      *    *** グループ名、ＨＴＴＰ、ＩＭＧ、または
      *    *** グループ名、ＳＰＡＣＥ、ＳＰＡＣＥ、または
      *    *** タレント名、ＩＭＧ、または
      *    *** タレント名、ＳＰＡＣＥ
      *    *** 名前順は
      *    *** タレント名、ＩＭＧ、または
      *    *** タレント名、ＳＰＡＣＥ

                                    OR "15"
               IF      WK-TITLE-A  NOT =   SPACE
                   AND WK-ITEM3 (1:1) NOT = SPACE
                   MOVE    "<a href='" TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE-A  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "'><br>" 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<img src="'
                                           TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-ITEM3 (1:WK-ITEM3-LEN) TO  POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" loading="lazy" alt=""'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   GO  TO  S100-20-1
               END-IF
           ELSE
               CONTINUE
           END-IF

      *    *** 31=YoutubeChannel
           IF      WK-FILE     =       "31"
               IF      WK-TITLE-A  NOT =   SPACE
                   AND WK-ITEM3 (1:1) NOT = SPACE
                   MOVE    "<a href='" TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE-A  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "'><br>" 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<img src="'
                                           TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-ITEM3 (1:WK-ITEM3-LEN) TO  POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" loading="lazy" alt=""'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   GO  TO  S100-30
               END-IF
           ELSE
               CONTINUE
           END-IF

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   MOVE    "<a href='" TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE-A  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "'><br>" 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** IF SW-SEARCH = "Y" の時は、
      *    *** C:\Users\xxxx\OneDrive\ドキュメント\HTML\YouTube汎用\IMAGE に
      *    *** 女優名.JPG が指定されているか、
      *    *** TEST54.PIN2 に女優名,女優画像(dmmの画像アドレスをコピー),...
      *    ***  が指定されている
      *    *** 女優画像 はimageに指定しなくても可とする
      *    *** 
                   IF      SW-SEARCH   =       "Y"
                       MOVE    '<img src="'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       IF      TBL01-IMG (TBL01-IDX) = SPACE
                           MOVE    "image\"    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-TITLE (1:K2) TO  POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '.jpg" loading="lazy" alt=""'
                                               TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                       ELSE
                           MOVE    TBL01-IMG (TBL01-IDX) TO  POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '" loading="lazy" alt=""'
                                               TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                       END-IF

                       MOVE    '><br><br>'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** 21=XVI,25=XVI2,30=XVIS
                   IF      WK-FILE     =       "21"
                                            OR "25"
                                            OR "30"
                       IF      WK-ITEM3    =       "OF "
                               CONTINUE
                       ELSE
                               MOVE    WK-TITLE    TO      POT1-REC
                               WRITE   POT1-REC
                               ADD     1           TO      WK-POT1-CNT

                               MOVE    '<br>'      TO      POT1-REC
                               WRITE   POT1-REC
                               ADD     1           TO      WK-POT1-CNT
                       END-IF
                   END-IF

                   EVALUATE TRUE
      *    *** 22=DMM
                       WHEN WK-FILE   = "22"
                           MOVE    WK-TITLE    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    '<br>'      TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
      *    *** 21=XVI,25=XVI2,30=XVIS
                       WHEN ( WK-FILE   = "21"
                                       OR "25" OR "30" )
                        AND WK-ITEM3  = "OF"
                           MOVE    WK-TITLE    TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           IF      WK-FILE     =       "25"
                               MOVE    '<br>'      TO      POT1-REC
                               WRITE   POT1-REC
                               ADD     1           TO      WK-POT1-CNT

                               MOVE    NUMVAL (WK-ITEM4) TO WK-SU
                               WRITE   POT1-REC    FROM    WK-SU
                               ADD     1           TO      WK-POT1-CNT
                           END-IF

                           MOVE    '<br>'      TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                       WHEN WK-TITLE-A (1:27) = 
                            "https://www.xvideos.com/?k="
                           MOVE    "SEARCH"    TO      POT1-REC
                       WHEN WK-TITLE-A (1:31) = 
                            "https://www.xvideos.com/models/"
                           MOVE    "MODELS"    TO      POT1-REC
                       WHEN WK-TITLE-A (1:33) = 
                            "https://www.xvideos.com/pornstars"
                           MOVE    "PORNSTARS" TO      POT1-REC
      *    *** https://www.xvideos.com/model-channels/gina-gerson =>
      *    *** https://www.xvideos.com/pornstar-channels/gina-gerson
      *    *** に変換してる
                       WHEN WK-TITLE-A (1:34) = 
                            "https://www.xvideos.com/model-channels/"
                           MOVE    "MODEL-CHANNELS" TO      POT1-REC
                       WHEN OTHER
                           MOVE    WK-TITLE-A (25:30) TO POT1-REC
                   END-EVALUATE
      *             WRITE   POT1-REC
      *             ADD     1           TO      WK-POT1-CNT

                   MOVE    "</a>" 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-ITEM3    =       "OF "
                           GO  TO  S100-40
                   ELSE
                           CONTINUE
                   END-IF
           END-IF

      *    *** 女性、男性
           IF      WK-TITLE    =       X"E5A5B3E680A7"
                                    OR X"E794B7E680A7"
                   MOVE    WK-TITLE    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S100-40
           END-IF
           .

       S100-20-1.

      *    *** 29=DMM 検索
           IF      WK-FILE     =       "29"
      *    *** 32=Youtube 動画サムネイル拡大
                                    OR "32"
                   GO  TO  S100-30
           END-IF
      *    *** 18=XVI 国別
           IF      WK-FILE     =       "18"
                   MOVE    "<a href='https://www.xvideos.com/?k="
                               TO      POT1-REC
           ELSE
      *    *** YouTube %23=#
                   MOVE
           "<a href='https://www.youtube.com/results?search_query=%23"
                               TO      POT1-REC
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   MOVE    K2          TO      WK-TITLE-LEN
           END-IF

           MOVE    WK-TITLE-LEN TO     WDE03-BUF1-LEN
           CALL    "DECODE03"  USING   WK-TITLE
                                       WDE03-BUF1-LEN
                                       WDE03-BUF2

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      J2
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-TITLE-LEN

               EVALUATE TRUE
                   WHEN WK-TITLE (J:1) = SPACE
                       ADD     1           TO      J2
                       MOVE    "+"         TO      POT1-REC (J2:1)

                   WHEN WK-TITLE (J:1) = "#" OR "$" OR "%" OR "&" OR "'"
                       OR "=" OR "|" OR "^" OR "\" 
                 OR "@" OR "[" OR ";" OR ":" OR "]" OR "." OR "/" OR "\"
                       OR "`" OR "{" OR "+" OR "}" OR "?" 
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

      *    *** 1ﾊﾞｲﾄ記号、英字、数字、ｶﾀｶﾅ そのままセット
                   WHEN  ( WK-TITLE (J:1) >= "!" 
                       AND WK-TITLE (J:1) <= "~" )
                      OR ( WK-TITLE (J:1) >= "｡"
                       AND WK-TITLE (J:1) <= "ﾟ" )
                       ADD     1           TO      J2
                       MOVE    WK-TITLE (J:1) TO   POT1-REC (J2:1)

      *    *** 3文字一度に変換 UTF8 ３文字体系のみ対応
                   WHEN OTHER
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

                       ADD     1           TO      J
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

                       ADD     1           TO      J
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
               END-EVALUATE
 
               IF      J2          >       500
                       DISPLAY WK-PGM-NAME "WK-PIN1-CNT2=" WK-PIN1-CNT2
                               " WK-TITLE-LEN ERROR=" WK-TITLE-LEN
                               " J2=" J2 " S100-20"
                       STOP    RUN
               ELSE
      *    *** キーワード ２０文字までとする
      *                     IF      J2          >=      60
      *                             MOVE    WK-TITLE-A-LEN TO   J
      *                     END-IF
                       CONTINUE
               END-IF
           END-PERFORM

           .
       S100-21.
      *     MOVE    WK-TITLE    TO      POT1-REC

           WRITE   POT1-REC
           MOVE    POT1-REC    TO      WK-KENSAKU
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>" 
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 15=アイドル大図鑑 名前順、グループ順
      *    *** FILE=15 WK-ITEM3=IMG
           IF      WK-FILE     =       "15"
               IF      WK-TITLE-A  =       SPACE
                   IF  WK-ITEM3 (1:1) =    SPACE
                       CONTINUE
                   ELSE
                       MOVE    '<img src="'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    WK-ITEM3 (1:WK-ITEM3-LEN)
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    '" loading="lazy" alt="">'
                                           TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       MOVE    '<br><br>'
                                       TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       GO  TO  S100-21-10
                   END-IF
               ELSE
      *    *** WK-TITLE-A NOT = SPACE AND WK-ITEM3 (1:1) NOT = SPACE は
      *    *** グループ名、ＨＴＴＰ、ＩＭＧ　前のステップで記述済
      *    *** WK-TITLE-A NOT = SPACE AND WK-ITEM3 (1:1) = SPACE はない
                   GO  TO  S100-21-10
               END-IF
           END-IF

      *    *** FILE=27 韓国女優,WK-TITLE-A=IMG
           IF      WK-FILE     =       "27"
               IF      WK-TITLE-A  =       SPACE
                       CONTINUE
               ELSE
                   MOVE    '<img src="'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE-A  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" loading="lazy" alt="">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S100-21-10
               END-IF
           END-IF

           IF      SW-SEARCH   =       "Y"
      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
               IF      WK-FILE     =       "21" OR "22"
                                        OR "25" OR "30"
                   CONTINUE
               ELSE
                   MOVE    '<img src="image\'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE (1:K2) TO  POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.jpg" loading="lazy" alt="">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               END-IF
           END-IF
           .
       S100-21-10.

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   MOVE    "Youtube"   TO      POT1-REC
           ELSE
                   IF      WK-ITEM4 (1:3) =     "<b>"
                           MOVE    "<b>"       TO      POT1-REC (1:3)
                           MOVE    WK-TITLE (1:WK-TITLE-LEN) 
                                               TO      POT1-REC (4:)
                           MOVE    "</b>"      TO      POT1-REC 
                                                    (4 + WK-TITLE-LEN:4)
                   ELSE
                           MOVE    WK-TITLE    TO      POT1-REC
                   END-IF
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a>" 
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      SW-YOUTUBE  =       "Y"
                   GO  TO  S100-22
           END-IF

      *    *** google
           MOVE
                   "<a href='https://www.google.co.jp/search?q="
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>google</a>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** wiki
           MOVE
                   "<a href='https://ja.wikipedia.org/wiki/"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>wiki</a>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .

       S100-22.
      *    *** WK-FILE=21 XVI, 22 DMM, 25 XVI2, 30 XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"

      *    *** 26=お菓子系２
                                    OR "26"

      *    *** 23=お菓子系．ｃｏｍ
                                    OR "23"

      *    *** 28=expo_jam_2018
                                    OR "28"
                   GO  TO  S100-30
           END-IF

           IF      WK-TITLE-A  =       SPACE
                   GO  TO  S100-30
           ELSE
      *    *** 15=アイドル大図鑑 名前順、グループ順
               IF      WK-FILE  = "15"
                       GO  TO  S100-30
               ELSE
                       CONTINUE
               END-IF
           END-IF

      *    *** FILE=27 韓国女優,WK-TITLE-A=IMG
           IF      WK-FILE     =       "27"
                   GO  TO  S100-30
           END-IF

      *    *** 18=XVI 国別
           IF      WK-FILE     =       "18"
                   CONTINUE
           ELSE

                   MOVE    "<br><br>"  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   MOVE    WK-TITLE-A  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    "<br>"      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 月
           IF    ( WK-TITLE-A (2:3) =  X"E69C88"
                OR WK-TITLE-A (3:3) =  X"E69C88" )
              AND
      *    *** 日
                 ( WK-TITLE-A (6:3) =  X"E697A5"
                OR WK-TITLE-A (7:3) =  X"E697A5"
                OR WK-TITLE-A (8:3) =  X"E697A5" )
                   GO  TO  S100-30
           END-IF

      *    *** YYYY年MM月DD日
      *    *** 月
           IF    ( WK-TITLE-A (09:3) =  X"E69C88"
                OR WK-TITLE-A (10:3) =  X"E69C88" )
              AND
      *    *** 日
                 ( WK-TITLE-A (13:3) =  X"E697A5"
                OR WK-TITLE-A (14:3) =  X"E697A5"
                OR WK-TITLE-A (15:3) =  X"E697A5" )
                   GO  TO  S100-30
           END-IF

      *    *** YYYY-MM-DD
           IF      WK-TITLE-A (01:4) IS  NUMERIC
               AND WK-TITLE-A (05:1) =   "-"
               AND WK-TITLE-A (06:2) IS  NUMERIC
               AND WK-TITLE-A (08:1) =   "-"
               AND WK-TITLE-A (09:2) IS  NUMERIC
                   GO  TO  S100-30
           END-IF

      *    *** YYYY-??-??
           IF      WK-TITLE-A (01:4) IS  NUMERIC
               AND WK-TITLE-A (05:1) =   "-"
               AND WK-TITLE-A (06:2) =   "??"
               AND WK-TITLE-A (08:1) =   "-"
               AND WK-TITLE-A (09:2) =   "??"
                   GO  TO  S100-30
           END-IF

      *    *** OPテーマ,EDテーマ
           IF    ( WK-TITLE-A (01:2) =   "OP" OR "ED" )
               AND WK-TITLE-A (03:9) =   X"E38386E383BCE3839E"
                   GO  TO  S100-30
           END-IF

      *    *** 18=XVI 国別
           IF      WK-FILE     =       "18"
                   MOVE    "<a href='https://www.xvideos.com/?k="
                               TO      POT1-REC
           ELSE

      *    *** YouTube %23=#
                   MOVE
           "<a href='https://www.youtube.com/results?search_query= %23"
                               TO      POT1-REC
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** WK-TITLE-A < 100 の時、カンマ有なので、その長さで処理
      *    *** 超える時、最初のSPACEか・手前までとする
           IF      WK-TITLE-A-LEN <    100
                   MOVE    WK-TITLE-A-LEN TO   WDE03-BUF1-LEN
           ELSE
                   MOVE    ZERO        TO      WDE03-BUF1-LEN
                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL J > WK-TITLE-A-LEN
                        OR WK-TITLE-A (J:1) = SPACE
      *    *** ・
                        OR WK-TITLE-A (J:3) = X"E383BB"
                           ADD     1           TO      WDE03-BUF1-LEN
                   END-PERFORM
           END-IF

           CALL    "DECODE03"  USING   WK-TITLE-A
                                       WDE03-BUF1-LEN
                                       WDE03-BUF2

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      J2
           PERFORM VARYING J FROM 1 BY 1
      *             UNTIL J > WK-TITLE-A-LEN
                   UNTIL J > WDE03-BUF1-LEN

               EVALUATE TRUE
                   WHEN WK-TITLE-A (J:1) = SPACE
                       ADD     1           TO      J2
                       MOVE    "+"         TO      POT1-REC (J2:1)

                 WHEN WK-TITLE-A (J:1) = "#" OR "$" OR "%" OR "&" OR "'"
                       OR "=" OR "|" OR "^" OR "\" 
                 OR "@" OR "[" OR ";" OR ":" OR "]" OR "." OR "/" OR "\"
                       OR "`" OR "{" OR "+" OR "}" OR "?" 
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

      *    *** 1ﾊﾞｲﾄ記号、英字、数字、ｶﾀｶﾅ そのままセット
                   WHEN  ( WK-TITLE-A (J:1) >= "!" 
                       AND WK-TITLE-A (J:1) <= "~" )
                      OR ( WK-TITLE-A (J:1) >= "｡"
                       AND WK-TITLE-A (J:1) <= "ﾟ" )
                       ADD     1           TO      J2
                       MOVE    WK-TITLE-A (J:1) TO POT1-REC (J2:1)

      *    *** 3文字一度に変換 UTF8 ３文字体系のみ対応
                   WHEN OTHER

                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

                       ADD     1           TO      J
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)

                       ADD     1           TO      J
                       ADD     1           TO      J2
                       MOVE    "%"         TO      POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                       ADD     1           TO      J2
                       MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
               END-EVALUATE
 
               IF      J2          >       500
                       DISPLAY WK-PGM-NAME "WK-PIN1-CNT2=" WK-PIN1-CNT2
                               " WK-TITLE-A-LEN ERROR=" WK-TITLE-A-LEN
                               " J2=" J2 " S100-22"
                       STOP    RUN
               ELSE
      *    *** キーワード ２０文字までとする
      *                     IF      J2          >=      60
      *                             MOVE    WK-TITLE-A-LEN TO   J
      *                     END-IF
                       CONTINUE
               END-IF
           END-PERFORM

           .
       S100-24.

      *    *** 18=XVI 国別
           IF     WK-FILE      =       "18"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "'><br>"    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE-A TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "</a>"      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S100-40
           END-IF

      *     MOVE    WK-TITLE-A  TO      POT1-REC

           WRITE   POT1-REC
           MOVE    POT1-REC    TO      WK-KENSAKU
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>YouTube</a>" 
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** google
           MOVE
                   "<a href='https://www.google.co.jp/search?q="
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>google</a>" 
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** wiki
           MOVE
                   "<a href='https://ja.wikipedia.org/wiki/"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "'><br>wiki</a>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S100-30.
      *     PERFORM VARYING K FROM 1 BY 1
      *             UNTIL K > WK-TITLE-LEN
      *                OR WK-TITLE (K:1) = SPACE
      *             MOVE    K           TO      K2
      *     END-PERFORM
           MOVE     WK-TITLE-LEN TO    K2

           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL01-TITLE (TBL01-IDX) (1:K2) =  WK-TITLE (1:K2)
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH   =       "Y"
                   IF      TBL01-TITLE2-LEN (TBL01-IDX) =  K2
                           CONTINUE
                   ELSE
                           MOVE    "N"         TO      SW-SEARCH
                   END-IF
           END-IF

           IF      SW-SEARCH   =       "Y"

               PERFORM VARYING K FROM 1 BY 1
                       UNTIL K > 20
                 IF      TBL01-SITE (TBL01-IDX K) (1:1) NOT = SPACE

                   MOVE    '<br>'      TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   MOVE    '<a href="' TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    SPACE       TO      POT1-REC
                                               WK-SITE-NAME
                   UNSTRING TBL01-SITE (TBL01-IDX K)
                           DELIMITED BY " !"
                           INTO
                           POT1-REC
                           WK-SITE-NAME

      *             MOVE    TBL01-SITE (TBL01-IDX K) TO POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '"><br>'    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-SITE-NAME = SPACE
                     IF      TBL01-SITE (TBL01-IDX K) (1:20) = 
                           "https://twitter.com/"
                           MOVE    "twitter"   TO      POT1-REC
                     ELSE
                       IF      TBL01-SITE (TBL01-IDX K) (1:26) = 
                           "https://www.instagram.com/"
                           MOVE    "instagram" TO      POT1-REC
                       ELSE
      *                     MOVE    "other"     TO      POT1-REC
                           EVALUATE TRUE
                               WHEN TBL01-SITE (TBL01-IDX K) (1:11) =
                                    "http://www."
                                   MOVE TBL01-SITE (TBL01-IDX K) (12:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:7) =
                                    "http://"
                                   MOVE TBL01-SITE (TBL01-IDX K) (8:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:12) =
                                    "https://www."
                                   MOVE TBL01-SITE (TBL01-IDX K) (13:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:8) =
                                    "https://"
                                   MOVE TBL01-SITE (TBL01-IDX K) (9:18)
                                               TO      POT1-REC
                               WHEN OTHER
                                   MOVE TBL01-SITE (TBL01-IDX K) (1:18)
                                               TO      POT1-REC
                           END-EVALUATE
                       END-IF
                   ELSE
                     MOVE    WK-SITE-NAME TO     POT1-REC
                   END-IF

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>'      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                 END-IF
               END-PERFORM
           END-IF
           .

       S100-40.

      *    *** 21=XVI , 22=DMM , 25=XVI2, 30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
      *    *** aduxvi.search WK-ITEM = "OF"
                   IF      WK-ITEM3    =       "OF "
                           ADD     1           TO      L
                       IF      L           >       9
                           MOVE    "</p></td>" TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    ZERO        TO      L
                       ELSE
                           CONTINUE
                       END-IF
                   ELSE
                           MOVE    "</p></td>" TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           ELSE
                   MOVE    "</p></td>" TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           .
       S100-EX.
           EXIT.

      *    *** #NN レコード編集1
       S110-10.

           MOVE    '<h2><br><a name="   ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:3) TO   POT1-REC (18:3)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (9:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      WK-WIDTH    =       "02"
                   MOVE    
               "</a></h2><table border='1' style='font-size: 30px'><tr>"
                                       TO      POT1-REC
           ELSE
                   MOVE    "</a></h2><table border='1'><tr>"
                                       TO      POT1-REC
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I
           .
       S110-EX.
           EXIT.

      *    *** #NN レコード編集2
       S120-10.

      *    *** 21=XVI,22=DMM,25=XVI2,30=XVIS
           IF      WK-FILE     =       "21" OR "22"
                                    OR "25" OR "30"
                   IF      L           =       ZERO
                           CONTINUE
                   ELSE
                           MOVE    "</p></td>" TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    ZERO        TO      L
                   END-IF
           ELSE
                   CONTINUE
           END-IF

           MOVE    '</tr></table><a href="#top">TOP</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** <br> １つだと、自動的に連番が段落に入る 入らない時もある
      *    *** <br> ２つだと、自動的に連番が段落に入らない
           MOVE    '<h2><br><a name="   ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:3) TO   POT1-REC (18:3)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (9:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      WK-WIDTH    =       "02"
                   MOVE    
               "</a></h2><table border='1' style='font-size: 30px'><tr>"
                                       TO      POT1-REC
           ELSE
                   MOVE    "</a></h2><table border='1'><tr>"
                                       TO      POT1-REC
           end-if
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I

           .
       S120-EX.
           EXIT.

      *    *** #NN レコード編集3
       S130-10.

           IF      PIN1-REC (1:4) =    "#001"



                   MOVE '<a href="https://twitter.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** twitter.com/
                   MOVE    "X"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.instagram.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** instagram.com/
                   MOVE    "Instagram"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>' TO   POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE    '<a href=' 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *             MOVE    '"C:\Users\xxxx\OneDrive\'

      *                                 TO      POT1-REC
      *             WRITE   POT1-REC
      *             ADD     1           TO      WK-POT1-CNT

      *             MOVE
      *    *** ドキュメント
      *          '%E3%83%89%E3%82%AD%E3%83%A5%E3%83%A1%E3%83%B3%E3%83%88'
      *                                 TO      POT1-REC
      *             WRITE   POT1-REC
      *             ADD     1           TO      WK-POT1-CNT

                   MOVE
      *    *** YouTube声優
      *             '\HTML\YouTube%E5%A3%B0%E5%84%AA\'
      *    *** YouTube
                   '"..\YouTube'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
      *    *** 声優
                   MOVE    X"E5A3B0E584AA"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '\index.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    
              '<img src="image\icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 声優
                   MOVE    X'E5A3B0E584AA'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>' TO   POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



      *    *** 26= お菓子系２目次
               IF      WK-FILE     =       "26"
                   MOVE    '<a href=' 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '"C:\Users\xxxx\OneDrive\'

                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE
      *    *** ドキュメント
                '%E3%83%89%E3%82%AD%E3%83%A5%E3%83%A1%E3%83%B3%E3%83%88'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE
      *    *** YouTube汎用
                   '\HTML\YouTube%E6%B1%8E%E7%94%A8\'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    'index'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** お菓子系２目次
                   MOVE    '%E3%81%8A%E8%8F%93%E5%AD%90%E7'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '%B3%BB%EF%BC%92%E7%9B%AE%E6%AC%A1'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    
              '<img src="image\icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** お菓子系２目次
                   MOVE    X'E3818AE88F93E5AD90E7B3BBEFBC92E79BAEE6ACA1'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>' TO   POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               END-IF

           END-IF

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NNN
           MOVE    PIN1-REC (1:4) TO   POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           EVALUATE TRUE

               WHEN SW-IDOLZUKAN = "Y"
      *    *** WK-FILE=25 XVI2
                   IF      WK-FILE     =       "25"
                       AND ( PIN1-REC (1:4) =   "#003" OR "#016" )
                       MOVE    '"><br>' TO     POT1-REC
                   ELSE
                       MOVE    '">'    TO      POT1-REC
                   END-IF

      *    *** あ、な、Ａ
      *    *** 文字の直前に、">を入れたい、文字を指定
               WHEN ((PIN1-REC (9:3) = X"E38182" OR X"E381AA"
                      OR X"EFBCA1" )
                 AND WK-PIN1-LEN =     11)
                 OR SW-WA       =      "Y"
                   MOVE    '">'    TO      POT1-REC

      *    *** あ、を
               WHEN PIN1-REC (9:3) >=  X"E38182" AND <= X"E38292"
                 AND WK-PIN1-LEN = 11
                   MOVE '" style="position:relative; left:0000px;">'
                                   TO      POT1-REC
                   ADD     16      TO      WK-LEFT-POS
                   MOVE    WK-LEFT-POS TO  POT1-REC (34:4)

      *    *** Ａ、Ｚ
               WHEN PIN1-REC (9:3) >=  X"EFBCA1" AND <= X"EFBCBA"
                 AND WK-PIN1-LEN = 11
                   MOVE '" style="position:relative; left:0000px;">'
                                   TO      POT1-REC
                   ADD     16      TO      WK-LEFT-POS
                   MOVE    WK-LEFT-POS TO  POT1-REC (34:4)

               WHEN OTHER
                   MOVE    '">'    TO      POT1-REC
           END-EVALUATE

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NNN ＸＸＸＸＸ => ＸＸＸＸＸ
           IF      PIN1-REC (9:10) =   "#aduxvi-br" OR "#aduDMM-br"
                   MOVE    SPACE       TO      POT1-REC
           ELSE
                   MOVE    PIN1-REC (9:) TO    POT1-REC
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 日本、一人プロジェクト対応
           EVALUATE TRUE

               WHEN SW-IDOLZUKAN = "Y"
      *    *** WK-FILE=25 XVI2
                   IF      WK-FILE     =       "25"
                       AND ( PIN1-REC (1:4) =   "#002" OR "#015" )
                       MOVE    '</a>'    TO     POT1-REC
                   ELSE
      *    *** 21=XVI,22=DMM,30=XVIS
                       IF      WK-FILE     =       "21" OR "22"
                                                OR "30"
      *    *** #003.　aduxvi-Search-Num-Alpha
                           IF    PIN1-REC (9:23) =
                                 "aduxvi-Search-Num-Alpha"
      *    *** #016.　tag-m
                              OR PIN1-REC (9:5) = "tag-m"
      *    *** #029.　tag-z
                              OR PIN1-REC (9:5) = "tag-z"
      *    *** #032.　aduxvi国別
                              OR PIN1-REC (9:12) = 
                                 X"616475787669E59BBDE588A5"
      *    *** aduxviチャンネル・イメージ <= チャンネルの最後変わったら変更する
                              OR PIN1-REC (9:36) = WK-CHANNEL-IMAGE
      *    *** aduxviプロフィール・アニメ <= プロフィールの最後変わったら変更する
                              OR PIN1-REC (9:36) = WK-PROFILE-ANIME
      *    *** aduDMM-Search-Actress-name
                              OR PIN1-REC (9:36) = 
                                 "aduDMM-Search-Actress-name"
      *    *** aduxvi-Search-Actress-name
                              OR PIN1-REC (9:26) = 
                                 "aduxvi-Search-Actress-name"
      *    *** #NNN.　お
      *    *** お
                              OR (PIN1-REC (9:3) = X"E3818A"
      *    *** ご
                                                OR X"E38194"
      *    *** ぞ
                                                OR X"E3819E"
      *    *** ど
                                                OR X"E381A9"
      *    *** の
                                                OR X"E381AE"
      *    *** ぽ
                                                OR X"E381BD"
      *    *** も
                                                OR X"E38282"
      *    *** よ
                                                OR X"E38288"
      *    *** うに”
                                                OR X"E38294"
      *    *** ９
                                                OR X"EFBC99"
      *    *** Ｚ
                                                OR X"EFBCBA" )
      *    *** aduxvi女優名あーお
      *    *** あー
                              OR (PIN1-REC (24:6) = X"E38182E383BC"
      *    *** か
                                                OR X"E3818BE383BC"
      *    *** さ
                                                OR X"E38195E383BC"
      *    *** た
                                                OR X"E3819FE383BC"
      *    *** な
                                                OR X"E381AAE383BC"
      *    *** は
                                                OR X"E381AFE383BC"
      *    *** ま
                                                OR X"E381BEE383BC"
      *    *** や
                                                OR X"E38284E383BC"
      *    *** ら
                                                OR X"E38289E383BC" )
      *    *** ブレイク用
                              OR PIN1-REC (9:10) = "#aduxvi-br"
      *    *** ブレイク用
                              OR PIN1-REC (9:10) = "#aduDMM-br"
                                 MOVE    '</a><br><br>' TO  POT1-REC
                           ELSE
                                 MOVE    '</a>&nbsp;&nbsp;&nbsp;' 
                                                 TO     POT1-REC
                           END-IF
                       ELSE
                           MOVE    '</a>&nbsp;&nbsp;&nbsp;' TO  POT1-REC
                       END-IF
                   END-IF

               WHEN WK-PIN1-LEN > 11
                   MOVE    '</a><br><br>' TO   POT1-REC

      *    *** わ、を
               WHEN ( PIN1-REC (9:3) = X"E3828F" OR X"E38292")
                  AND WK-PIN1-LEN =     11
                   IF    SW-WO  =  "Y"
      *    *** を
                     IF PIN1-REC (9:3) = X"E38292"
                       MOVE    '</a><br><br>' TO   POT1-REC
                       MOVE    ZERO        TO      WK-LEFT-POS
                       MOVE    "Y"         TO      SW-WA
                     ELSE
                       MOVE    '</a>'      TO      POT1-REC
                     END-IF
                   ELSE
                       MOVE    '</a><br><br>' TO   POT1-REC
                       MOVE    ZERO        TO      WK-LEFT-POS
                       MOVE    "Y"         TO      SW-WA
                   END-IF

      *    *** と, ど
               WHEN ( PIN1-REC (9:3) = X"E381A8" OR X"E381A9" )
                  AND WK-PIN1-LEN =     11
                   IF    SW-DO  = "Y"
      *    *** ど
                     IF PIN1-REC (9:3) = X"E381A9"
                        MOVE    '</a><br>'  TO      POT1-REC
                        MOVE    ZERO        TO      WK-LEFT-POS
                     ELSE
                        MOVE    '</a>'      TO      POT1-REC
                     END-IF
                   ELSE
                       MOVE    '</a><br>'  TO      POT1-REC
                       MOVE    ZERO        TO      WK-LEFT-POS
                   END-IF

      *    *** あ、を
               WHEN PIN1-REC (9:3) >=  X"E38182" AND <= X"E38292"
                   MOVE '</a>'     TO      POT1-REC

      *    *** Ｚ
               WHEN PIN1-REC (9:3) =   X"EFBCBA"
                   MOVE '</a><br><br>' TO  POT1-REC

      *    *** Ａ、Ｚ
               WHEN PIN1-REC (9:3) >=  X"EFBCA1" AND <= X"EFBCBA"
                   MOVE '</a>'     TO      POT1-REC
               WHEN OTHER
                   MOVE    '</a><br><br>'      TO      POT1-REC
           END-EVALUATE
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
           MOVE    WK-PIN1-CNT2 TO     WK-PIN1-CNT2-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT2-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
