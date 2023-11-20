      *    *** YouTube html 他 汎用インプットデータ 作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST53.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** WIKIから編集して入力するものと、
      *    *** TEST55 等プログラムで編集した入力がある
      *    ***

      *    *** ｗｉｋｉデータ等　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** HTML TEST54.PIN1 入力データ
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

      *    *** TEST55.POT1 の内容
      *%芸能人（名前順　男性）
      *$DO=Y
      *ジャパリあ
      *哀川翔,1961年5月24日 59歳,あいかわ,男性
      *相葉裕樹,1987年10月1日 33歳,あいば,男性

      *    *** TEST53.aikatsu.PIN1 の内容
      *%アイカツ！　シリーズ　登場人物・主題歌・挿入歌,

      *ジャパリアイカツ!の登場人物一覧 

      *星宮 いちご,諸星すみれ,歌,わか（霧島若歌）
      *霧矢 あおい,田所あずさ,歌,ふうり（上花楓裏）
      *紫吹 蘭,大橋彩香,歌,すなお（吉河順央）→ゆな（市倉有菜）

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.

       01  POT1-REC.
           03  FILLER          PIC  X(1000).

      *    *** HTML データ
      *    *** 1件目
      *    *** %NNKIITT,
      *    *** (1:1)=%   タイトルＩＤ
      *    *** (2:2)=NN  TEST54 で横方向の表示数(通常は０８)
      *    *** (4:2)=II  TEST54の出力項目指定
      *    *** (6:2)=AA  ACCEPT 入力値 (WK-FILE)
      *    *** (8:1)=K   区分 (W:女性、M:男性等 通常はスペース)
      *    *** (9:NN)=TT タイトル名（１バイトスペース、カンマ含まず）
      *    ***           NNは任意の長さ
      *    *** (9+NN:1)=,最終カラムはカンマ
      *    *** 
      *    *** 2件目以降
      *    *** 
      *    *** $DO=Y あいうえお順出力で”ど”から始まるを含む時、
      *    ***       TESTXX でセット
      *    *** 
      *    *** AA,BB,CC,DD,EE,...
      *    ***          通常はAA,BBのTEST54で項目出力

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST53  ".

           03  WK-PIN1-F-NAME  PIC  X(100) VALUE "TEST53.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST53.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-FILE         PIC  X(002) VALUE ZERO.
           03  WK-FILE-9       REDEFINES WK-FILE
                               PIC  9(002).
           03  WK-WIDTH        PIC  X(002) VALUE ZERO.
           03  WK-HIRAGANA     PIC  X(001) VALUE ZERO.
           03  WK-NO           PIC  9(003) VALUE ZERO.
           03  WK-SEX          PIC  9(001) VALUE ZERO.
           03  WK-KAKKO        PIC  9(002) VALUE ZERO.
           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-SAI          PIC  9(002) VALUE ZERO.
           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-FILE-NAME    OCCURS 100
                               PIC  X(080) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-HIRAGANA     PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** WRITE POT1
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY "PIN1 FILE NAME 数字=?"

                   DISPLAY " "
                   DISPLAY "01.TEST53_po_gr_ja.PIN1"
                   MOVE    
                   "   ポピュラー音楽の音楽家一覧 (日本・グループ)"
                               TO      WK-FILE-NAME (01)
                   DISPLAY WK-FILE-NAME (01)

                   DISPLAY " "
                   DISPLAY "02.TEST53_po_ko_ja.PIN1"
                   MOVE    "   ポピュラー音楽の音楽家一覧 (日本・個人)"
                               TO      WK-FILE-NAME (02)
                   DISPLAY WK-FILE-NAME (02)

                   DISPLAY " "
                   DISPLAY "03.TEST53_po_gr_wr.PIN1"
                   MOVE   
                    "   ポピュラー音楽の音楽家一覧 (日本以外・グループ)"
                               TO      WK-FILE-NAME (03)
                   DISPLAY WK-FILE-NAME (03)

                   DISPLAY " "
                   DISPLAY "04.TEST53_po_ko_wr.PIN1"
                   MOVE   
                     "   ポピュラー音楽の音楽家一覧 (日本以外・個人)"
                               TO      WK-FILE-NAME (04)
                   DISPLAY WK-FILE-NAME (04)

                   DISPLAY " "
                   DISPLAY "05.TEST53_girl_kr.PIN1"
                   MOVE    "   韓国のガール・グループ"
                               TO      WK-FILE-NAME (05)
                   DISPLAY WK-FILE-NAME (05)

                   DISPLAY " "
                   DISPLAY "06.TEST53_adolgirl_gr_ja.PIN1"
                   MOVE    "   日本の女性アイドルグループ"
                               TO      WK-FILE-NAME (06)
                   DISPLAY WK-FILE-NAME (06)

                   DISPLAY " "
                   DISPLAY "07.TEST53_girl_ko_ja.PIN1"
                   MOVE    "   日本の女性アイドル"
                               TO      WK-FILE-NAME (07)
                   DISPLAY WK-FILE-NAME (07)

                   DISPLAY " "
                   DISPLAY "08.TEST53_E-girls.PIN1"
                   MOVE    "   E-girls"
                               TO      WK-FILE-NAME (08)
                   DISPLAY WK-FILE-NAME (08)

                   DISPLAY " "
                   DISPLAY "09.TEST53_junioridol_ja.PIN1"
                   MOVE    "   ジュニアアイドル一覧"
                               TO      WK-FILE-NAME (09)
                   DISPLAY WK-FILE-NAME (09)

                   DISPLAY " "
                   DISPLAY "10.TEST53_aikatsu.PIN1"
                   MOVE    "   アイカツ主題歌・挿入歌"
                               TO      WK-FILE-NAME (10)
                   DISPLAY WK-FILE-NAME (10)

                   DISPLAY " "
                   DISPLAY "11.TEST53_talent_birthday.PIN1"
                   MOVE    "   芸能人・誕生日順　（女性・男性）"
                               TO      WK-FILE-NAME (11)
                   DISPLAY WK-FILE-NAME (11)

                   DISPLAY " "
                   DISPLAY "12.TEST53_talent_birthday.PIN1"
                   MOVE    "   芸能人・誕生日順　（女性）"
                               TO      WK-FILE-NAME (12)
                   DISPLAY WK-FILE-NAME (12)

                   DISPLAY " "
                   DISPLAY "13 はC.TEST55 で作成"
                   DISPLAY "13.TEST55.POT1"
                   MOVE    "   芸能人 名前順（女性・男性、女性、男性）"
                               TO      WK-FILE-NAME (13)
                   DISPLAY WK-FILE-NAME (13)

                   DISPLAY " "
                   DISPLAY "14 はC.TEST56 で作成"
                   DISPLAY "14.TEST56.POT1"
                   MOVE    
                   "   日本の女優一覧2000年代生まれ 名前順、誕生日順"
                               TO      WK-FILE-NAME (14)
                   DISPLAY WK-FILE-NAME (14)

                   DISPLAY " "
                   DISPLAY "15 はC.TEST57 で作成"
                   DISPLAY "15.TEST57.POT1"
                   MOVE    "   アイドル大図鑑 名前順、グループ順"
                               TO      WK-FILE-NAME (15)
                   DISPLAY WK-FILE-NAME (15)

                   DISPLAY " "
                   DISPLAY "16.TEST53_jyoyu1990_birthday.PIN1"
                   MOVE    "   日本の女優一覧1990年代生まれ"
                               TO      WK-FILE-NAME (16)
                   DISPLAY WK-FILE-NAME (16)

                   DISPLAY " "
                   DISPLAY "17 はC.TEST58 で作成"
                   DISPLAY "17.TEST58.POT1"
                   MOVE    "   日本の女優一覧"
                               TO      WK-FILE-NAME (17)
                   DISPLAY WK-FILE-NAME (17)

                   DISPLAY " "
                   DISPLAY "18は休止中"
                   DISPLAY "18 はC.TEST60 で作成"
                   DISPLAY "18.TEST60.POT1"
                   MOVE    "   世界の女優一覧"
                               TO      WK-FILE-NAME (18)
                   DISPLAY WK-FILE-NAME (18)

                   DISPLAY " "
                   DISPLAY "19.TEST53_gakkiall.PIN1"
                   MOVE    "   楽器分類別一覧"
                               TO      WK-FILE-NAME (19)
                   DISPLAY WK-FILE-NAME (19)

                   DISPLAY " "
                   DISPLAY "20.TEST53_CLASSIC.PIN1"
                   MOVE    "   クラシック作曲家一覧"
                               TO      WK-FILE-NAME (20)
                   DISPLAY WK-FILE-NAME (20)

                   DISPLAY " "
                   DISPLAY "21 はC.TEST70 で作成"
                   DISPLAY "21.TEST70.POT1"
                   MOVE    "   XVI"
                               TO      WK-FILE-NAME (21)
                   DISPLAY WK-FILE-NAME (21)

                   DISPLAY " "
                   DISPLAY "22 はC.TEST72 で作成"
                   DISPLAY "22.TEST72.POT1"
                   MOVE    "   DMM"
                               TO      WK-FILE-NAME (22)
                   DISPLAY WK-FILE-NAME (22)

                   DISPLAY " "
                   DISPLAY "23 はC.TEST79 で作成"
                   DISPLAY "23.TEST79.POT1"
                   MOVE    "   お菓子系．ｃｏｍ"
                               TO      WK-FILE-NAME (23)
                   DISPLAY WK-FILE-NAME (23)

                   DISPLAY " "
                   DISPLAY "24 はC.TEST74 で作成"
                   MOVE    "   Qosmio_G50"
                               TO      WK-FILE-NAME (24)
                   DISPLAY WK-FILE-NAME (24)

                   DISPLAY " "
                   DISPLAY "25 はC.TEST78 で作成"
                   DISPLAY "25.TEST78.POT1"
                   MOVE    "   XVI2"
                               TO      WK-FILE-NAME (25)
                   DISPLAY WK-FILE-NAME (25)

                   DISPLAY " "
                   DISPLAY "26 はC.TEST80 で作成"
                   DISPLAY "26.TEST80.POT1"
                   MOVE    "   お菓子系２"
                               TO      WK-FILE-NAME (26)
                   DISPLAY WK-FILE-NAME (26)

                   DISPLAY " "
                   DISPLAY "27 TEST53_actress_kr.PIN1"
                   MOVE    "   韓国女優"
                               TO      WK-FILE-NAME (27)
                   DISPLAY WK-FILE-NAME (27)

                   DISPLAY " "
                   DISPLAY "28 TEST83.POT1"
                   MOVE    "   expo_jam_2018"
                               TO      WK-FILE-NAME (28)
                   DISPLAY WK-FILE-NAME (28)

                   DISPLAY " "
                   DISPLAY "29 TEST89.POT1"
                   MOVE    "   DMM 検索 渚みつき"
                               TO      WK-FILE-NAME (29)
                   DISPLAY WK-FILE-NAME (29)

                   DISPLAY " "
                   DISPLAY "30 TEST70.POT4"
                   MOVE    "   XVIS"
                               TO      WK-FILE-NAME (30)
                   DISPLAY WK-FILE-NAME (30)

                   DISPLAY " "
                   DISPLAY "31 TEST97U.POT2"
                   MOVE    "   Youtube Channel"
                               TO      WK-FILE-NAME (31)
                   DISPLAY WK-FILE-NAME (31)

                   DISPLAY " "
                   DISPLAY "32 TEST103.POT1"
                   MOVE    "   Youtube 動画サムネイル拡大"
                               TO      WK-FILE-NAME (32)
                   DISPLAY WK-FILE-NAME (32)

                   DISPLAY " "
                   DISPLAY "33 TEST101.POT2"
                   MOVE    "   楽天検索"
                               TO      WK-FILE-NAME (33)
                   DISPLAY WK-FILE-NAME (33)

                   ACCEPT  WK-FILE
                   IF      WK-FILE     =   "01"  OR "02" OR "03" OR "04"
                           OR "05" OR "06" OR "07" OR "08" OR "09"
                           OR "10" OR "11" OR "12" OR "13" OR "14" 
                           OR "15" OR "16" OR "17" OR "18" OR "19"
                           OR "20" OR "21" OR "22" OR "23" OR "24"
                           OR "25" OR "26" OR "27" OR "28" OR "29"
                           OR "30" OR "31" OR "32" OR "33"
                           DISPLAY "FILE-NAME="
                           DISPLAY WK-FILE-NAME (WK-FILE-9)
                           DISPLAY " FILE NAME OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " FILE NAME 01-32 INPUT"
                   END-IF
           END-PERFORM

           IF      WK-FILE     =       "32"
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"

                   DISPLAY " "
                   DISPLAY "32 TEST103.POT1"
                   DISPLAY "   Youtube 動画サムネイル拡大"
                   DISPLAY "   WIDTH 02 OR 05 INPUT"

                   ACCEPT  WK-WIDTH
                   IF      WK-WIDTH     =   "02" OR "05"
                           DISPLAY " WIDTH OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " WIDTH 02 OR 05 INPUT"
                   END-IF
               END-PERFORM
           END-IF

           MOVE    "N"         TO      SW-YES
      *    *** 26=お菓子系２ データ分割
           IF      WK-FILE     =       "26"
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY "26.お菓子系２"
                   DISPLAY "1.あーお"
                   DISPLAY "2.かーこ"
                   DISPLAY "3.さーそ"
                   DISPLAY "4.たーと"
                   DISPLAY "5.なーの"
                   DISPLAY "6.はーほ"
                   DISPLAY "7.まーも"
                   DISPLAY "8.やーよ"
                   DISPLAY "9.らーん"

                   ACCEPT  WK-HIRAGANA
                   IF      WK-HIRAGANA     =   "1"  OR "2" OR "3" OR "4"
                           OR "5" OR "6" OR "7" OR "8" OR "9"
                           DISPLAY " ひらがな OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " ひらがな 1-9 INPUT"
                   END-IF
               END-PERFORM
           END-IF

           EVALUATE WK-FILE
               WHEN "01"
                   MOVE    "TEST53_po_gr_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "02"
                   MOVE    "TEST53_po_ko_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "03"
                   MOVE    "TEST53_po_gr_wr.PIN1" TO WK-PIN1-F-NAME
               WHEN "04"
                   MOVE    "TEST53_po_ko_wr.PIN1" TO WK-PIN1-F-NAME
               WHEN "05"
                   MOVE    "TEST53_girl_kr.PIN1" TO WK-PIN1-F-NAME
               WHEN "06"
                   MOVE   "TEST53_idolgirl_gr_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "07"
                   MOVE    "TEST53_girl_ko_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "08"
                   MOVE    "TEST53_E-girls.PIN1" TO WK-PIN1-F-NAME
               WHEN "09"
                   MOVE    "TEST53_junioridol_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "10"
      *    *** AIKATSU.txt　をCOBSORTでSORT後、「=>（、」=>）に変更して
      *    *** TEST53_aikatsu.PIN1 作成
                   MOVE    "TEST53_aikatsu.PIN1" TO WK-PIN1-F-NAME
               WHEN "11"
                   MOVE    "TEST53_talent_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "12"
                   MOVE    "TEST53_talent_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "13"
                   MOVE    "TEST55.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "14"
                   MOVE    "TEST56.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "15"
                   MOVE    "TEST57.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "16"
                   MOVE    "TEST53_jyoyu1990_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "17"
                   MOVE    "TEST58.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "18"
                   MOVE    "TEST60.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "19"
                   MOVE    "TEST53_gakkiall.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "20"
                   MOVE    "TEST53_CLASSIC.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "21"
                   MOVE    "TEST70.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "22"
                   MOVE    "TEST72.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "23"
                   MOVE    "TEST79.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "24"
                   MOVE    "TEST74.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "25"
                   MOVE    "TEST78.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "26"
                   MOVE    "TEST80.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "27"
                   MOVE    "TEST53_actress_kr.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "28"
                   MOVE    "TEST83.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "29"
                   MOVE    "TEST89.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "30"
                   MOVE    "TEST70.POT4"
                                       TO     WK-PIN1-F-NAME
               WHEN "31"
                   MOVE    "TEST97U.POT2"
                                       TO     WK-PIN1-F-NAME
               WHEN "32"
                   MOVE    "TEST103.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "33"
                   MOVE    "TEST101.POT2"
                                       TO     WK-PIN1-F-NAME

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

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
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

      *    *** WRITE POT1
       S100-10.

      *    *** [NN] => SPACE,[表示] => SPACE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
               IF      PIN1-REC (I:1) = "["
                   AND ( WK-FILE NOT = "21" AND "25" AND "30" )
                       EVALUATE TRUE
                           WHEN PIN1-REC (I + 2:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:3)
                           WHEN PIN1-REC (I + 3:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:4)
                           WHEN PIN1-REC (I + 4:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:5)
                           WHEN PIN1-REC (I + 5:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:6)
                           WHEN PIN1-REC (I + 6:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:7)
                           WHEN PIN1-REC (I + 7:1) = "]"
                                MOVE    SPACE       TO    PIN1-REC (I:8)
                       END-EVALUATE
               END-IF
           END-PERFORM

           EVALUATE TRUE

      *    *** ZERO byte コメントとし、カット
                WHEN WK-PIN1-LEN =     ZERO
                   IF      WK-FILE   =       "11" OR "12"
                       ADD     1           TO      WK-SEX
                       IF      WK-SEX      =       1
                           AND WK-FILE     =       "11"
      *    *** 男性,
                               MOVE    X"E794B7E680A72C" TO  POT1-REC

                               WRITE   POT1-REC
                               IF      WK-POT1-STATUS NOT =  ZERO
                                       DISPLAY WK-PGM-NAME 
                                           " POT1-F WRITE ERROR STATUS="
                                           WK-POT1-STATUS
                                       STOP    RUN
                               END-IF
                               ADD     1           TO      WK-POT1-CNT
                       ELSE
                               CONTINUE
                       END-IF
                   ELSE
                           CONTINUE
                   END-IF

      *    *** (1,1)= * コメントとし、カット
                WHEN PIN1-REC (1:1) =  "*"
                   CONTINUE

      *    *** (1,3)= （ コメントとし、カット
                WHEN PIN1-REC (1:3) =  X"EFBC88"
                   CONTINUE

      *    *** 日本アイドルグループ を対応
                WHEN PIN1-REC (1:1) = "$" OR "#"
      *    *** WK-FILE=24 Qosmio #M:... DIR そのまま出力
                   MOVE    PIN1-REC    TO      POT1-REC

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT

      *    *** ジャパリ　行 コメントとし、カット
      *    *** ジャパリ
               WHEN PIN1-REC (1:12) =  X"E382B8E383A3E38391E383AA"
      *    *** 行
               AND  PIN1-REC (16:3) =  X"E8A18C"
                   CONTINUE
      *    *** N位 カット (２行になってる)
               WHEN PIN1-REC (2:3) =   X"E4BD8D"
                 OR PIN1-REC (3:3) =   X"E4BD8D"
                   CONTINUE

      *    *** ジャパリ無、YYYY年
               WHEN ( WK-FILE    =       "11" OR "12" )
                AND PIN1-REC (1:4) IS  NUMERIC
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    "#"         TO      POT1-REC (1:1)
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT1-REC (2:3)
                   MOVE    "."         TO      POT1-REC (5:1)
      *    *** 　 UTF8
                   MOVE    X"E38080"   TO      POT1-REC (6:3)
                   MOVE    PIN1-REC    TO      POT1-REC (9:)
      *    *** 歳　再計算
                   MOVE    PIN1-REC (1:4) TO   WK-YYYY
                   COMPUTE WK-SAI = WK-YYYY - WDT-DATE-YYYY
                   MOVE    WK-SAI      TO   POT1-REC (4 + WK-PIN1-LEN:2)

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT
                   MOVE    ZERO        TO      WK-SEX

      *    *** 女性,
                   IF      WK-FILE     =       "11"
                       MOVE    X"E5A5B3E680A72C" TO      POT1-REC
                       WRITE   POT1-REC
                       IF      WK-POT1-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME 
                                       " POT1-F WRITE ERROR STATUS="
                                       WK-POT1-STATUS
                               STOP    RUN
                       END-IF
                       ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** ジャパリ無、あーを
               WHEN WK-PIN1-LEN    =   3
                AND PIN1-REC (1:1) =   X"E3"

                   EVALUATE TRUE
      *    *** 26=お菓子系２
                      WHEN WK-FILE = "26"
                          MOVE    "N"         TO      SW-HIRAGANA
                          IF    ( WK-HIRAGANA =       "1"
      *    *** あーお
                              AND PIN1-REC (1:3) >=   X"E38182"
                              AND PIN1-REC (1:3) <=   X"E3818A" ) OR

                                ( WK-HIRAGANA =       "2"
      *    *** かーご
                              AND PIN1-REC (1:3) >=   X"E3818B"
                              AND PIN1-REC (1:3) <=   X"E38194" ) OR

                                ( WK-HIRAGANA =       "3"
      *    *** さーぞ
                              AND PIN1-REC (1:3) >=   X"E38195"
                              AND PIN1-REC (1:3) <=   X"E3819E" ) OR

                                ( WK-HIRAGANA =       "4"
      *    *** たーど
                              AND PIN1-REC (1:3) >=   X"E3819F"
                              AND PIN1-REC (1:3) <=   X"E381A9" ) OR

                                ( WK-HIRAGANA =       "5"
      *    *** なーの
                              AND PIN1-REC (1:3) >=   X"E381AA"
                              AND PIN1-REC (1:3) <=   X"E381AE" ) OR

                                ( WK-HIRAGANA =       "6"
      *    *** はーぽ
                              AND PIN1-REC (1:3) >=   X"E381AF"
                              AND PIN1-REC (1:3) <=   X"E381BD" ) OR

                                ( WK-HIRAGANA =       "7"
      *    *** まーも
                              AND PIN1-REC (1:3) >=   X"E381BE"
                              AND PIN1-REC (1:3) <=   X"E381BF" ) OR

                                ( WK-HIRAGANA =       "7"
                              AND PIN1-REC (1:3) >=   X"E38280"
                              AND PIN1-REC (1:3) <=   X"E38282" ) OR

                                ( WK-HIRAGANA =       "8"
      *    *** やーよ
                              AND PIN1-REC (1:3) >=   X"E38283"
                              AND PIN1-REC (1:3) <=   X"E38288" ) OR

                                ( WK-HIRAGANA =       "9"
      *    *** らーん
                              AND PIN1-REC (1:3) >=   X"E38289"
                              AND PIN1-REC (1:3) <=   X"E38293" )
                              MOVE    "Y"           TO      SW-HIRAGANA

                              MOVE    SPACE       TO      POT1-REC
                              MOVE    "#"         TO      POT1-REC (1:1)
                              ADD     1           TO      WK-NO
                              MOVE    WK-NO       TO      POT1-REC (2:3)
                              MOVE    "."         TO      POT1-REC (5:1)
      *    *** 　 UTF8
                              MOVE    X"E38080"   TO      POT1-REC (6:3)
                              MOVE    PIN1-REC (1:3) TO   POT1-REC (9:3)
                              WRITE   POT1-REC
                              ADD     1           TO      WK-POT1-CNT
                          END-IF
                      WHEN OTHER
                          MOVE    SPACE       TO      POT1-REC
                          MOVE    "#"         TO      POT1-REC (1:1)
                          ADD     1           TO      WK-NO
                          MOVE    WK-NO       TO      POT1-REC (2:3)
                          MOVE    "."         TO      POT1-REC (5:1)
      *    *** 　 UTF8
                          MOVE    X"E38080"   TO      POT1-REC (6:3)
                          MOVE    PIN1-REC (1:3) TO   POT1-REC (9:3)
                          WRITE   POT1-REC
                          ADD     1           TO      WK-POT1-CNT
                   END-EVALUATE

      *    *** ジャパリ
               WHEN PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    "#"         TO      POT1-REC (1:1)
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT1-REC (2:3)
                   MOVE    "."         TO      POT1-REC (5:1)
      *    *** 　 UTF8
                   MOVE    X"E38080"   TO      POT1-REC (6:3)
                   IF      WK-PIN1-LEN <=       15
                           MOVE    PIN1-REC (13:3) TO  POT1-REC (9:3)
                   ELSE
                           MOVE    PIN1-REC (13:)  TO  POT1-REC (9:)
                   END-IF

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT

      *    *** 汎用タイトル
               WHEN PIN1-REC (1:1) = "%"
                   MOVE    PIN1-REC (1:1) TO   POT1-REC (1:1)
                   MOVE    WK-FILE        TO      POT1-REC (6:2)
      *    *** テーブル横方向表示数
                   MOVE    "08"           TO      POT1-REC (2:2)
      *    *** 区分(7:1)、タイトル名(8:NN)は前のPGMから引き継ぐ
                   MOVE    PIN1-REC (2: ) TO      POT1-REC (8:)
                   MOVE    SPACE       TO      POT1-REC (4:2)

                   EVALUATE TRUE
      *    *** 横方向、表示数セット
      *    *** 23=お菓子系．ｃｏｍ
                      WHEN WK-FILE = "23"
                           MOVE    "03"        TO      POT1-REC (2:2)
      *    *** 28=expo_jam_2018
                      WHEN WK-FILE = "28"
      *    *** 29=DMM 検索
                                  OR "29"
      *    *** 21=XVI,22=DMM,30=XVIS
                                  OR "21" OR "22"
                                  OR "30"
                           MOVE    "05"        TO      POT1-REC (2:2)
      *    *** 32=Youtube 動画サムネイル拡大
                      WHEN WK-FILE = "32"
                           MOVE    WK-WIDTH    TO      POT1-REC (2:2)
      *    *** 26=お菓子系２
                      WHEN WK-FILE = "26"
                           MOVE    "05"        TO      POT1-REC (2:2)
      *    *** タイトルにあ、か、さ、…、ら付ける
                           EVALUATE TRUE
                               WHEN WK-HIRAGANA = "1"
                                   MOVE    X"E38182" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "2"
                                   MOVE    X"E3818B" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "3"
                                   MOVE    X"E38195" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "4"
                                   MOVE    X"E3819F" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "5"
                                   MOVE    X"E381AA" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "6"
                                   MOVE    X"E381AF" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "7"
                                   MOVE    X"E381BE" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "8"
                                   MOVE    X"E38284" TO POT1-REC (24:3)
                               WHEN WK-HIRAGANA = "9"
                                   MOVE    X"E38289" TO POT1-REC (24:3)
                           END-EVALUATE
                           MOVE    ","         TO      POT1-REC (27:1)
                      WHEN WK-FILE = "09" OR "16" OR "17"
                           MOVE    "12"           TO      POT1-REC (2:2)
                      WHEN WK-FILE = "02" OR "05" OR "07"
                           MOVE    "10"           TO      POT1-REC (2:2)
      *    *** 18 はYOUTUBE のみ、出力
                      WHEN WK-FILE = "18"
                           MOVE    "08"           TO      POT1-REC (2:2)
                           MOVE    "YT"           TO      POT1-REC (4:2)
      *    *** 11 は（女性・男性）　追加
                      WHEN WK-FILE = "11"
                           MOVE
                           X"EFBC88E5A5B3E680A7E383BBE794B7E680A7EFBC89"
                                                  TO    POT1-REC (30:21)
                           MOVE    WDT-DATE-YYYY  TO    POT1-REC (52:04)
      *    *** 12 は（女性）　追加
                      WHEN WK-FILE = "12"
                           MOVE
                           X"EFBC88E5A5B3E680A7EFBC89"
                                                  TO    POT1-REC (30:12)
                           MOVE    PIN1-REC (45:24)
                                                  TO    POT1-REC (42:)
                           MOVE    WDT-DATE-YYYY  TO    POT1-REC (43:04)
      *    *** 14 日本の女優一覧2000年代生まれ 名前順、誕生日順
      *    *** 16 日本の女優一覧1990年代生まれ
      *                WHEN WK-FILE = "14" OR "16"
      *                     MOVE    "07"           TO      POT1-REC (2:2)
      *    *** 21 XVI
      *                WHEN WK-FILE = "21" OR "22"
      *                     MOVE    "06"           TO      POT1-REC (2:2)
                      WHEN OTHER
                           CONTINUE
                   END-EVALUATE

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT

      *    *** WK-FILE=21 XVI, 22 DMM, 24 Qosmio, 25 XVI2, 29 DMM 検索,
      *    *** 30 XVIS, 32 Youtube 動画サムネイル拡大
               WHEN WK-FILE = "21" OR "22" OR "24" OR "25" OR "29"
                           OR "30" OR "32"
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT

               WHEN OTHER
                   IF      WK-FILE     =       "11" OR "12"
                       MOVE    ZERO        TO     WK-KAKKO
                       INSPECT PIN1-REC TALLYING WK-KAKKO 
      *    *** （
                               FOR ALL X"EFBC88"
                       IF      WK-KAKKO =      1
                           INSPECT PIN1-REC 
                               REPLACING ALL X"EFBC88" BY "  ,"
                               REPLACING ALL X"EFBC89" BY "   "
                       ELSE
                           INSPECT PIN1-REC 
      *    *** ）（
                               REPLACING FIRST X"EFBC89EFBC88" BY
                                             "     ,"
                           INSPECT PIN1-REC 
      *    *** （
                               REPLACING ALL X"EFBC88" BY "   "
                           INSPECT PIN1-REC 
      *    *** ）
                               REPLACING ALL X"EFBC89" BY "   "
                       END-IF
                   ELSE
                       INSPECT PIN1-REC 
      *    *** （
                           REPLACING ALL "(" BY ","
      *    *** ）
                                     ALL ")" BY ","
      *    *** X"09"=HT(水平タブ)
                                     ALL X"2309" BY "$,"
                                     ALL X"09" BY ","
      *    *** （
                                     ALL X"EFBC88" BY "  ,"
      *    *** ）
                                     ALL X"EFBC89" BY "  ,"
      *    *** 、
                                     ALL X"E38081" BY "  ,"
      *    *** ・
      *                               ALL X"E383BB" BY "   "
                   END-IF
                   COMPUTE I = WK-PIN1-LEN + 1
                   MOVE    ","         TO      PIN1-REC (I:1)

                   EVALUATE TRUE
                       WHEN WK-FILE = "12"
                         IF WK-SEX = ZERO
                           WRITE   POT1-REC    FROM    PIN1-REC
                           IF      WK-POT1-STATUS NOT =  ZERO
                                   DISPLAY WK-PGM-NAME 
                                           " POT1-F WRITE ERROR STATUS="
                                           WK-POT1-STATUS
                                   STOP    RUN
                           END-IF
                           ADD     1           TO      WK-POT1-CNT
                         END-IF
      *    *** 26=お菓子系２
                       WHEN WK-FILE = "26"
                           IF      SW-HIRAGANA =       "Y"
                                   WRITE   POT1-REC    FROM    PIN1-REC
                                   ADD     1           TO    WK-POT1-CNT
                           END-IF
                       WHEN OTHER
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-EVALUATE
           END-EVALUATE

           .
       S100-EX.
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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
