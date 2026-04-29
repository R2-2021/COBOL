      *    *** YouTube html 汎用インプットデータ 作成

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
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** HTML TEST54.PIN1 入力データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.

       01  PIN1-REC.
           03  FILLER          PIC  X(10000).

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
           03  FILLER          PIC  X(10000).

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

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-FILE         PIC  X(003) VALUE ZERO.
           03  WK-FILE-9       REDEFINES WK-FILE
                               PIC  9(003).
           03  WK-WIDTH        PIC  X(002) VALUE ZERO.
           03  WK-HIRAGANA     PIC  X(001) VALUE ZERO.
           03  WK-NO           PIC  9(004) VALUE ZERO.
           03  WK-SEX          PIC  9(001) VALUE ZERO.
           03  WK-KAKKO        PIC  9(002) VALUE ZERO.
           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-SAI          PIC  9(002) VALUE ZERO.
           03  WK-REC          PIC  X(10000) VALUE SPACE.
           03  WK-FILE-NAME    OCCURS 200
                               PIC  X(080) VALUE SPACE.
           03  WK-KANMA-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-BR-CNT       BINARY-LONG SYNC VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT1       PIC  X(003) VALUE ZERO.
           03  WK-ACCEPT2       PIC  X(002) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-END          PIC  X(001) VALUE "N".
           03  SW-HIRAGANA     PIC  X(001) VALUE "N".
           03  SW-PER          PIC  X(001) VALUE "N".

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

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-ACCEPT1 FROM ARGUMENT-VALUE
      *    *** 入力値のチェックはしない
               WHEN 2
                   ACCEPT  WK-ACCEPT1 FROM ARGUMENT-VALUE
                   ACCEPT  WK-ACCEPT2 FROM ARGUMENT-VALUE
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " ARGUMENT-VALUE 指定無しか１個又は２個指定"
                           " TEST53 032 02 <=例"
                   STOP    RUN
           END-EVALUATE

           IF      WK-ARGUMENT-NUMBER = 1 OR 2
                   MOVE    "Y"         TO      SW-YES
                   MOVE    WK-ACCEPT1  TO      WK-FILE
           ELSE
                   MOVE    "N"         TO      SW-YES
           END-IF

           PERFORM UNTIL SW-YES =      "Y" OR "y"
                   DISPLAY " "
                   DISPLAY "PIN1 FILE NAME 数字=?"

                   DISPLAY " "
                   DISPLAY "001.TEST53_po_gr_ja.PIN1"
                   MOVE    
                   "   ポピュラー音楽の音楽家一覧 (日本・グループ)"
                               TO      WK-FILE-NAME (01)
                   DISPLAY WK-FILE-NAME (01)

                   DISPLAY " "
                   DISPLAY "002.TEST53_po_ko_ja.PIN1"
                   MOVE    "   ポピュラー音楽の音楽家一覧 (日本・個人)"
                               TO      WK-FILE-NAME (02)
                   DISPLAY WK-FILE-NAME (02)

                   DISPLAY " "
                   DISPLAY "003.TEST53_po_gr_wr.PIN1"
                   MOVE   
                    "   ポピュラー音楽の音楽家一覧 (日本以外・グループ)"
                               TO      WK-FILE-NAME (03)
                   DISPLAY WK-FILE-NAME (03)

                   DISPLAY " "
                   DISPLAY "004.TEST53_po_ko_wr.PIN1"
                   MOVE   
                     "   ポピュラー音楽の音楽家一覧 (日本以外・個人)"
                               TO      WK-FILE-NAME (04)
                   DISPLAY WK-FILE-NAME (04)

                   DISPLAY " "
                   DISPLAY "005.TEST53_girl_kr.PIN1"
                   MOVE    "   韓国のガール・グループ"
                               TO      WK-FILE-NAME (05)
                   DISPLAY WK-FILE-NAME (05)

                   DISPLAY " "
                   DISPLAY "006.TEST53_idolgirl_gr_ja.PIN1"
                   MOVE    "   日本の女性アイドルグループ"
                               TO      WK-FILE-NAME (06)
                   DISPLAY WK-FILE-NAME (06)

                   DISPLAY " "
                   DISPLAY "007.TEST53_girl_ko_ja.PIN1"
                   MOVE    "   日本の女性アイドル"
                               TO      WK-FILE-NAME (07)
                   DISPLAY WK-FILE-NAME (07)

                   DISPLAY " "
                   DISPLAY "008.TEST53_E-girls.PIN1"
                   MOVE    "   E-girls"
                               TO      WK-FILE-NAME (08)
                   DISPLAY WK-FILE-NAME (08)

                   DISPLAY " "
                   DISPLAY "009.TEST53_junioridol_ja.PIN1"
                   MOVE    "   ジュニアアイドル一覧"
                               TO      WK-FILE-NAME (09)
                   DISPLAY WK-FILE-NAME (09)

                   DISPLAY " "
                   DISPLAY "010.TEST53_aikatsu.PIN1"
                   MOVE    "   アイカツ主題歌・挿入歌"
                               TO      WK-FILE-NAME (10)
                   DISPLAY WK-FILE-NAME (10)

                   DISPLAY " "
                   DISPLAY "011.TEST53_talent_birthday.PIN1"
                   MOVE    "   芸能人・誕生日順　（女性・男性）"
                               TO      WK-FILE-NAME (11)
                   DISPLAY WK-FILE-NAME (11)

                   DISPLAY " "
                   DISPLAY "012.TEST53_talent_birthday.PIN1"
                   MOVE    "   芸能人・誕生日順　（女性）"
                               TO      WK-FILE-NAME (12)
                   DISPLAY WK-FILE-NAME (12)

                   DISPLAY " "
                   DISPLAY "013 はC.TEST55 で作成"
                   DISPLAY "013.TEST55.POT1"
                   MOVE    "   芸能人 名前順（女性・男性、女性、男性）"
                               TO      WK-FILE-NAME (13)
                   DISPLAY WK-FILE-NAME (13)

                   DISPLAY " "
                   DISPLAY "014 はC.TEST56 で作成"
                   DISPLAY "014.TEST56.POT1"
                   MOVE    
                   "   日本の女優一覧2000年代生まれ 名前順、誕生日順"
                               TO      WK-FILE-NAME (14)
                   DISPLAY WK-FILE-NAME (14)

                   DISPLAY " "
                   DISPLAY "015 はC.TEST57 で作成"
                   DISPLAY "015.TEST57.POT1"
                   MOVE    "   アイドル大図鑑 名前順、グループ順"
                               TO      WK-FILE-NAME (15)
                   DISPLAY WK-FILE-NAME (15)

                   DISPLAY " "
                   DISPLAY "016.TEST53_jyoyu1990_birthday.PIN1"
                   MOVE    "   日本の女優一覧1990年代生まれ"
                               TO      WK-FILE-NAME (16)
                   DISPLAY WK-FILE-NAME (16)

                   DISPLAY " "
                   DISPLAY "017 はC.TEST58 で作成"
                   DISPLAY "017.TEST58.POT1"
                   MOVE    "   日本の女優一覧"
                               TO      WK-FILE-NAME (17)
                   DISPLAY WK-FILE-NAME (17)

                   DISPLAY " "
                   DISPLAY "018 はC.TEST60 で作成"
                   DISPLAY "018.TEST60.POT1"
                   MOVE    "   世界の女優一覧"
                               TO      WK-FILE-NAME (18)
                   DISPLAY WK-FILE-NAME (18)

                   DISPLAY " "
                   DISPLAY "019.TEST53_gakkiall.PIN1"
                   MOVE    "   楽器分類別一覧"
                               TO      WK-FILE-NAME (19)
                   DISPLAY WK-FILE-NAME (19)

                   DISPLAY " "
                   DISPLAY "020.TEST53_CLASSIC.PIN1"
                   MOVE    "   クラシック作曲家一覧"
                               TO      WK-FILE-NAME (20)
                   DISPLAY WK-FILE-NAME (20)

                   DISPLAY " "
                   DISPLAY "021 はC.TEST70 で作成"
                   DISPLAY "021.TEST70.POT1"
                   MOVE    "   XVI"
                               TO      WK-FILE-NAME (21)
                   DISPLAY WK-FILE-NAME (21)

                   DISPLAY " "
                   DISPLAY "022 はC.TEST72 で作成"
                   DISPLAY "022.TEST72.POT1"
                   MOVE    "   DMM"
                               TO      WK-FILE-NAME (22)
                   DISPLAY WK-FILE-NAME (22)

                   DISPLAY " "
                   DISPLAY "023 はC.TEST79 で作成"
                   DISPLAY "023.TEST79.POT1"
                   MOVE    "   お菓子系．ｃｏｍ"
                               TO      WK-FILE-NAME (23)
                   DISPLAY WK-FILE-NAME (23)

                   DISPLAY " "
                   DISPLAY "024 はC.TEST122 で作成"
                   MOVE    "   Qosmio_G50"
                               TO      WK-FILE-NAME (24)
                   DISPLAY WK-FILE-NAME (24)

                   DISPLAY " "
                   DISPLAY "025 はC.TEST78 で作成"
                   DISPLAY "025.TEST78.POT1"
                   MOVE    "   XVI2"
                               TO      WK-FILE-NAME (25)
                   DISPLAY WK-FILE-NAME (25)

                   DISPLAY " "
                   DISPLAY "026 はC.TEST80 で作成"
                   DISPLAY "026.TEST80.POT1"
                   MOVE    "   お菓子系２"
                               TO      WK-FILE-NAME (26)
                   DISPLAY WK-FILE-NAME (26)

                   DISPLAY " "
                   DISPLAY "027 TEST53_actress_kr.PIN1"
                   MOVE    "   韓国女優"
                               TO      WK-FILE-NAME (27)
                   DISPLAY WK-FILE-NAME (27)

                   DISPLAY " "
                   DISPLAY "028 TEST83.POT1"
                   MOVE    "   expo_jam_2018"
                               TO      WK-FILE-NAME (28)
                   DISPLAY WK-FILE-NAME (28)

                   DISPLAY " "
      *    *** INPUT HTML 変更の為、現在使用不可、34 で実行する
                   DISPLAY "029 TEST89.POT1"
                   MOVE    "   DMM 検索 渚みつき"
                               TO      WK-FILE-NAME (29)
                   DISPLAY WK-FILE-NAME (29)

                   DISPLAY " "
                   DISPLAY "030 TEST70.POT4"
                   MOVE    "   XVIS"
                               TO      WK-FILE-NAME (30)
                   DISPLAY WK-FILE-NAME (30)

                   DISPLAY " "
                   DISPLAY "031 TEST97U.POT2"
                   MOVE    "   Youtube Channel"
                               TO      WK-FILE-NAME (31)
                   DISPLAY WK-FILE-NAME (31)

                   DISPLAY " "
                   DISPLAY "032 TEST103.POT1"
                   MOVE    "   Youtube/MissAV 動画サムネイル拡大"
                               TO      WK-FILE-NAME (32)
                   DISPLAY WK-FILE-NAME (32)

                   DISPLAY " "
                   DISPLAY "033 TEST101.POT2"
                   MOVE    "   楽天検索"
                               TO      WK-FILE-NAME (33)
                   DISPLAY WK-FILE-NAME (33)

                   DISPLAY " "
                   DISPLAY "034 TEST116.POT1"
                   MOVE    "   ＤＭＭ 動画サムネイル拡大"
                               TO      WK-FILE-NAME (34)
                   DISPLAY WK-FILE-NAME (34)

                   DISPLAY " "
                   DISPLAY "035 TEST53_honkon_gr.PIN1"
                   MOVE    "   香港の女性歌手"
                               TO      WK-FILE-NAME (35)
                   DISPLAY WK-FILE-NAME (35)

                   DISPLAY " "
                   DISPLAY "036 TEST53_honkon_man.PIN1"
                   MOVE    "   香港の男性歌手"
                               TO      WK-FILE-NAME (36)
                   DISPLAY WK-FILE-NAME (36)

                   DISPLAY " "
                   DISPLAY "037 TEST53_china_gr.PIN1"
                   MOVE    "   中国の女性歌手"
                               TO      WK-FILE-NAME (37)
                   DISPLAY WK-FILE-NAME (37)

                   DISPLAY " "
                   DISPLAY "038 TEST53_china_man.PIN1"
                   MOVE    "   中国の男性歌手"
                               TO      WK-FILE-NAME (38)
                   DISPLAY WK-FILE-NAME (38)

                   DISPLAY " "
                   DISPLAY "039 TEST53_taiwan_gr.PIN1"
                   MOVE    "   台湾の女性歌手"
                               TO      WK-FILE-NAME (39)
                   DISPLAY WK-FILE-NAME (39)

                   DISPLAY " "
                   DISPLAY "040 TEST53_taiwan_man.PIN1"
                   MOVE    "   台湾の男性歌手"
                               TO      WK-FILE-NAME (40)
                   DISPLAY WK-FILE-NAME (40)

                   DISPLAY " "
                  DISPLAY "041 TEST53_中国大陸女性アーティスト一覧.PIN1"
                   MOVE    "   中国大陸女性アーティスト一覧"
                               TO      WK-FILE-NAME (41)
                   DISPLAY WK-FILE-NAME (41)

                   DISPLAY " "
                  DISPLAY "042 TEST53_中国大陸男性アーティスト一覧.PIN1"
                   MOVE    "   中国大陸男性アーティスト一覧"
                               TO      WK-FILE-NAME (42)
                   DISPLAY WK-FILE-NAME (42)

                   DISPLAY " "
                   DISPLAY 
                   "043 TEST53_中国大陸グループアーティスト一覧.PIN1"
                   MOVE    "   中国大陸グループアーティスト一覧"
                               TO      WK-FILE-NAME (43)
                   DISPLAY WK-FILE-NAME (43)

                   DISPLAY " "
                   DISPLAY "044 TEST53_女性アーティスト一覧.PIN1"
                   MOVE    "   香港台湾女性アーティスト一覧"
                               TO      WK-FILE-NAME (44)
                   DISPLAY WK-FILE-NAME (44)

                   DISPLAY " "
                   DISPLAY "045 TEST53_男性アーティスト一覧.PIN1"
                   MOVE    "   香港台湾男性アーティスト一覧"
                               TO      WK-FILE-NAME (45)
                   DISPLAY WK-FILE-NAME (45)

                   DISPLAY " "
                   DISPLAY "046 TEST53_グループアーティスト一覧.PIN1"
                   MOVE    "   香港台湾グループアーティスト一覧"
                               TO      WK-FILE-NAME (46)
                   DISPLAY WK-FILE-NAME (46)

                   DISPLAY " "
                   DISPLAY "047 TEST53_中国系アーティスト一覧.PIN1"
                   MOVE    "   中国系アーティスト一覧"
                               TO      WK-FILE-NAME (47)
                   DISPLAY WK-FILE-NAME (47)

                   DISPLAY " "
                   DISPLAY "048 TEST53_中国系アーティスト一覧２.PIN1"
                   MOVE    "   中国系アーティスト２一覧"
                               TO      WK-FILE-NAME (48)
                   DISPLAY WK-FILE-NAME (48)

                   DISPLAY " "
                   DISPLAY "049 TEST53_中国系アーティスト一覧３.PIN1"
                   MOVE    "   中国系アーティスト３一覧"
                               TO      WK-FILE-NAME (49)
                   DISPLAY WK-FILE-NAME (49)

                   DISPLAY " "
                   DISPLAY "050 TEST74.POT1"
                   MOVE    "   MissAV"
                               TO      WK-FILE-NAME (50)
                   DISPLAY WK-FILE-NAME (50)

                   DISPLAY " "
                   DISPLAY "051 TEST53_girigiri_idol.PIN1"
                   MOVE    "   ぎりぎりジュニアアイドル"
                               TO      WK-FILE-NAME (51)
                   DISPLAY WK-FILE-NAME (51)

                   DISPLAY " "
                   DISPLAY "052 TEST53_NETFLIX.PIN1"
                   MOVE    "   Netflix"
                               TO      WK-FILE-NAME (52)
                   DISPLAY WK-FILE-NAME (52)

                   DISPLAY " "
                   DISPLAY "053 TEST53_shokubutsu.PIN1"
                   MOVE    "   shokubutsu"
                               TO      WK-FILE-NAME (53)
                   DISPLAY WK-FILE-NAME (53)
                   DISPLAY " "

                   DISPLAY "054 TEST53_zoo_doubutsu.PIN1"
                   MOVE    "   zoo_doubutsu"
                               TO      WK-FILE-NAME (54)
                   DISPLAY WK-FILE-NAME (54)
                   DISPLAY " "

                   DISPLAY "055 TEST53_shoudoubutsu.PIN1"
                   MOVE    "   shoudoubutsu"
                               TO      WK-FILE-NAME (55)
                   DISPLAY WK-FILE-NAME (55)
                   DISPLAY " "

                   DISPLAY "056 TEST53_neko.PIN1"
                   MOVE    "   neko"
                               TO      WK-FILE-NAME (56)
                   DISPLAY WK-FILE-NAME (56)
                   DISPLAY " "

                   DISPLAY "057 TEST53_inu.PIN1"
                   MOVE    "   inu"
                               TO      WK-FILE-NAME (57)
                   DISPLAY WK-FILE-NAME (57)
                   DISPLAY " "

                   DISPLAY "058 TEST53_world_ichiran.PIN1"
                   MOVE    "   world_ichiran"
                               TO      WK-FILE-NAME (58)
                   DISPLAY WK-FILE-NAME (58)
                   DISPLAY " "

                   DISPLAY "059 TEST53_nihon_kankochi.PIN1"
                   MOVE    "   nihon_kankochi"
                               TO      WK-FILE-NAME (59)
                   DISPLAY WK-FILE-NAME (59)
                   DISPLAY " "

                   DISPLAY "060 TEST53_100_meizan.PIN1"
                   MOVE    "   100_meizan"
                               TO      WK-FILE-NAME (60)
                   DISPLAY WK-FILE-NAME (60)
                   DISPLAY " "

                   DISPLAY "061 TEST53_actress_cn.PIN1"
                   MOVE    "   中国女優"
                               TO      WK-FILE-NAME (61)
                   DISPLAY WK-FILE-NAME (61)
                   DISPLAY " "

                   DISPLAY "062 TESTXXX.POT1 <= 062 は永久欠番にする"
      *    *** MiaaAV データ作成はTEST133 から TEST104 に変更して、
      *    *** 032として作成
      *    *** そのままだとhtml>YouTube汎用 に作ってしまうので
      *    *** html>YouTubeＡＸに作成するように、032=>062 に下記で変更している
                   MOVE    "   XXXXXXXXXXXXXXXXXXXXXXXX"
                               TO      WK-FILE-NAME (62)
                   DISPLAY WK-FILE-NAME (62)
                   DISPLAY " "

                   DISPLAY "063 TEST53_SHOWA.idol.PIN1"
                   MOVE    "   昭和アイドル"
                               TO      WK-FILE-NAME (63)
                   DISPLAY WK-FILE-NAME (63)
                   DISPLAY " "

                   DISPLAY "064 TEST53_girl_kr2.PIN1"
                   MOVE    "   音韓、女性韓国アイドルグループ"
                               TO      WK-FILE-NAME (64)
                   DISPLAY WK-FILE-NAME (64)
                   DISPLAY " "

                   ACCEPT  WK-FILE
                   IF      WK-FILE     =  
                             "001" OR "002" OR "003" OR "004" OR "005"
                          OR "006" OR "007" OR "008" OR "009" OR "010"
                          OR "011" OR "012" OR "013" OR "014" OR "015" 
                          OR "016" OR "017" OR "018" OR "019" OR "020" 
                          OR "021" OR "022" OR "023" OR "024" OR "025" 
                          OR "026" OR "027" OR "028" OR "029" OR "030" 
                          OR "031" OR "032" OR "033" OR "034" OR "035" 
                          OR "036" OR "037" OR "038" OR "039" OR "040" 
                          OR "041" OR "042" OR "043" OR "044" OR "045" 
                          OR "046" OR "047" OR "048" OR "049" OR "050" 
                          OR "051" OR "052" OR "053" OR "054" OR "055" 
                          OR "056" OR "057" OR "058" OR "059" OR "060" 
                          OR "061" OR "XXX" OR "063" OR "064"
                           DISPLAY "FILE-NAME="
                           DISPLAY WK-FILE-NAME (WK-FILE-9)
                           DISPLAY " FILE NAME OK ? Y(y)/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " FILE NAME 001-064 INPUT"
                   END-IF
           END-PERFORM

      *    *** 32=Youtube 動画サムネイル拡大
      *    *** 34=ＤＭＭ 動画サムネイル拡大
      *    *** 56=neko
      *    *** 62=ＭｉｓｓＡＶ 動画サムネイル拡大
           IF      WK-FILE     =       "032" OR "034"
                                    OR "056" OR "062"

               IF      WK-ARGUMENT-NUMBER = 2
                       MOVE    "Y"         TO      SW-YES
                       MOVE    WK-ACCEPT2  TO      WK-WIDTH
               ELSE
                       MOVE    "N"         TO      SW-YES
               END-IF
               PERFORM UNTIL SW-YES =      "Y" OR "y"

                   DISPLAY " "
                   DISPLAY "032 TEST103.POT1"
                   DISPLAY "   Youtube 動画サムネイル拡大"
                   DISPLAY "   WIDTH 02 OR 05 INPUT"

                   ACCEPT  WK-WIDTH
                   IF      WK-WIDTH     =   "02" OR "05"
                           DISPLAY " WIDTH OK ? Y(y)/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " WIDTH 02 OR 05 INPUT"
                   END-IF
               END-PERFORM
           END-IF

           MOVE    "N"         TO      SW-YES
      *    *** 26=お菓子系２ データ分割
      *    *** この処理、やめる
      *     IF      WK-FILE     =       "026"
           IF      WK-FILE     =       "XX"
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY "026.お菓子系２"
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
               WHEN "001"
                   MOVE    "TEST53_po_gr_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "002"
                   MOVE    "TEST53_po_ko_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "003"
                   MOVE    "TEST53_po_gr_wr.PIN1" TO WK-PIN1-F-NAME
               WHEN "004"
                   MOVE    "TEST53_po_ko_wr.PIN1" TO WK-PIN1-F-NAME
               WHEN "005"
                   MOVE    "TEST53_girl_kr.PIN1" TO WK-PIN1-F-NAME
               WHEN "006"
                   MOVE   "TEST53_idolgirl_gr_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "007"
                   MOVE    "TEST53_girl_ko_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "008"
                   MOVE    "TEST53_E-girls.PIN1" TO WK-PIN1-F-NAME
               WHEN "009"
                   MOVE    "TEST53_junioridol_ja.PIN1" TO WK-PIN1-F-NAME
               WHEN "010"
      *    *** AIKATSU.txt　をCOBSORTでSORT後、「=>（、」=>）に変更して
      *    *** TEST53_aikatsu.PIN1 作成
                   MOVE    "TEST53_aikatsu.PIN1" TO WK-PIN1-F-NAME
               WHEN "011"
                   MOVE    "TEST53_talent_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "012"
                   MOVE    "TEST53_talent_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "013"
                   MOVE    "TEST55.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "014"
                   MOVE    "TEST56.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "015"
                   MOVE    "TEST57.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "016"
                   MOVE    "TEST53_jyoyu1990_birthday.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "017"
                   MOVE    "TEST58.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "018"
                   MOVE    "TEST60.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "019"
                   MOVE    "TEST53_gakkiall.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "020"
                   MOVE    "TEST53_CLASSIC.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "021"
                   MOVE    "TEST70.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "022"
                   MOVE    "TEST72.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "023"
                   MOVE    "TEST79.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "024"
                   MOVE    "TEST122.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "025"
                   MOVE    "TEST78.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "026"
                   MOVE    "TEST80.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "027"
                   MOVE    "TEST53_actress_kr.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "028"
                   MOVE    "TEST83.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "029"
                   MOVE    "TEST89.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "030"
                   MOVE    "TEST70.POT4"
                                       TO     WK-PIN1-F-NAME
               WHEN "031"
                   MOVE    "TEST97U.POT2"
                                       TO     WK-PIN1-F-NAME
               WHEN "032"
                   MOVE    "TEST103.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "033"
                   MOVE    "TEST101.POT2"
                                       TO     WK-PIN1-F-NAME
               WHEN "034"
                   MOVE    "TEST116.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "035"
                   MOVE    "TEST53_honkon_gr.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "036"
                   MOVE    "TEST53_honkon_man.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "037"
                   MOVE    "TEST53_china_gr.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "038"
                   MOVE    "TEST53_china_man.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "039"
                   MOVE    "TEST53_taiwan_gr.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "040"
                   MOVE    "TEST53_taiwan_man.PIN1"
                                       TO     WK-PIN1-F-NAME
      *    *** TEST118.CBL で作成
               WHEN "041"
                   MOVE    "TEST53_中国大陸女性アーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "042"
                   MOVE    "TEST53_中国大陸男性アーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "043"
                   MOVE   "TEST53_中国大陸グループアーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "044"
                   MOVE    "TEST53_香港台湾女性アーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "045"
                   MOVE    "TEST53_香港台湾男性アーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "046"
                   MOVE   "TEST53_香港台湾グループアーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
      *    *** TEST119.CBL で作成
               WHEN "047"
                   MOVE    "TEST53_中国系アーティスト一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "048"
                   MOVE    "TEST53_中国系アーティスト２一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "049"
                   MOVE    "TEST53_中国系アーティスト３一覧.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "050"
                   MOVE    "TEST74.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "051"
                   MOVE    "TEST53_girigiri_idol.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "052"
                   MOVE    "TEST53_NETFLIX.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "053"
                   MOVE    "TEST53_shokubutsu.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "054"
                   MOVE    "TEST53_zoo_doubutsu.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "055"
                   MOVE    "TEST53_shoudoubutsu.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "056"
                   MOVE    "TEST53_neko.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "057"
                   MOVE    "TEST53_inu.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "058"
                   MOVE    "TEST53_world_ichiran.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "059"
                   MOVE    "TEST53_nihon_kankochi.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "060"
                   MOVE    "TEST53_100_meizan.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "061"
                   MOVE    "TEST53_actress_cn.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "062"
                   MOVE    "TESTXXX.POT1"
                                       TO     WK-PIN1-F-NAME
               WHEN "063"
                   MOVE    "TEST53_SHOWA.idol.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN "064"
                   MOVE    "TEST53_girl_kr2.PIN1"
                                       TO     WK-PIN1-F-NAME
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-FILE ERROR WK-FILE=" WK-FILE
                   STOP    RUN

           END-EVALUATE

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

      *    *** [NN] => SPACE,[表示] => SPACE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
               IF      PIN1-REC (I:1) = "["
                   AND ( WK-FILE NOT = "021" AND "025" AND "030" )
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
                   IF      WK-FILE   =       "011"
                       ADD     1           TO      WK-SEX
                       IF      WK-SEX      =       1
                           AND WK-FILE     =       "011"
      *    *** 男性,
                               MOVE    X"E794B7E680A72C" TO  POT1-REC

                               WRITE   POT1-REC
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
      *    *** ポピュラー日本グーループ等の対応
                WHEN PIN1-REC (1:3) =  X"EFBC88"
      *    *** WK-FILE=21 XVI, 22 DMM, 24 Qosmio, 25 XVI2, 29 DMM 検索,
      *    *** 30 XVIS, 32 Youtube 動画サムネイル拡大
      *    *** 34 DMM 動画サムネイル拡大
                 AND ( WK-FILE NOT = "021" AND "022" AND "024" AND "025" 
                       AND "029" AND "030" AND "032" AND "034" )
                   CONTINUE

      *    *** 日本アイドルグループ を対応
                WHEN PIN1-REC (1:1) = "$" OR "#"
      *    *** WK-FILE=24 Qosmio #M:... DIR そのまま出力
                   MOVE    PIN1-REC    TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** ジャパリ　行 コメントとし、カット
      *    *** 01=ポピュラー音楽の音楽家一覧 (日本・グループ) 等
      *    *** TEST53_po_gr_ja.PIN1
      *    *** ジャパリ
               WHEN PIN1-REC (1:12) =  X"E382B8E383A3E38391E383AA"
      *    *** あ
               AND             ( PIN1-REC (1:3) >=   X"E38182"
      *    *** か
                              OR PIN1-REC (1:3) >=   X"E3818B"
      *    *** さ
                              OR PIN1-REC (1:3) >=   X"E38195"
      *    *** た
                              OR PIN1-REC (1:3) >=   X"E3819F"
      *    *** な
                              OR PIN1-REC (1:3) >=   X"E381AA"
      *    *** は
                              OR PIN1-REC (1:3) >=   X"E381AF"
      *    *** ま
                              OR PIN1-REC (1:3) >=   X"E381BE"
      *    *** や
                              OR PIN1-REC (1:3) >=   X"E38283"
      *    *** ら
                              OR PIN1-REC (1:3) >=   X"E38289"
      *    *** わ
                              OR PIN1-REC (1:3) >=   X"E3828F" )
      *    *** 行
               AND  PIN1-REC (16:3) =  X"E8A18C"
      *    *** TEST103でジャパリで行を含むものあった為
               AND  WK-PIN1-LEN     =  18
                   CONTINUE
      *    *** N位 カット (２行になってる)
               WHEN PIN1-REC (2:3) =   X"E4BD8D"
                 OR PIN1-REC (3:3) =   X"E4BD8D"
                   CONTINUE

      *    *** ジャパリ無、YYYY年
               WHEN ( WK-FILE    =       "011" OR "012" )
                AND PIN1-REC (1:4) IS  NUMERIC
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    "#"         TO      POT1-REC (1:1)
                   ADD     1           TO      WK-NO
                   MOVE    WK-NO       TO      POT1-REC (2:4)
                   MOVE    "."         TO      POT1-REC (6:1)
      *    *** 　 UTF8
                   MOVE    X"E38080"   TO      POT1-REC (7:3)
                   MOVE    PIN1-REC    TO      POT1-REC (10:)
      *    *** 歳　再計算
                   MOVE    PIN1-REC (1:4) TO   WK-YYYY
                   COMPUTE WK-SAI = WK-YYYY - WDT-DATE-YYYY
                   MOVE    WK-SAI      TO   POT1-REC (5 + WK-PIN1-LEN:2)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   MOVE    ZERO        TO      WK-SEX

      *    *** 女性,
                   IF      WK-FILE     =       "11"
                       MOVE    X"E5A5B3E680A72C" TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   END-IF

      *    *** ジャパリ無、あーを
               WHEN WK-PIN1-LEN    =   3
                AND PIN1-REC (1:1) =   X"E3"

                   EVALUATE TRUE
      *    *** 26=お菓子系２
                      WHEN WK-FILE = "026"
                          MOVE    "N"         TO      SW-HIRAGANA
      *                    IF    ( WK-HIRAGANA =       "1"
                          IF    (
      *    *** あーお
                                  PIN1-REC (1:3) >=   X"E38182"
                              AND PIN1-REC (1:3) <=   X"E3818A" ) OR

      *                          ( WK-HIRAGANA =       "2"
                                (
      *    *** かーご
                                  PIN1-REC (1:3) >=   X"E3818B"
                              AND PIN1-REC (1:3) <=   X"E38194" ) OR

      *                          ( WK-HIRAGANA =       "3"
                                (
      *    *** さーぞ
                                  PIN1-REC (1:3) >=   X"E38195"
                              AND PIN1-REC (1:3) <=   X"E3819E" ) OR

      *                          ( WK-HIRAGANA =       "4"
                                (
      *    *** たーど
                                  PIN1-REC (1:3) >=   X"E3819F"
                              AND PIN1-REC (1:3) <=   X"E381A9" ) OR

      *                          ( WK-HIRAGANA =       "5"
                                (
      *    *** なーの
                                  PIN1-REC (1:3) >=   X"E381AA"
                              AND PIN1-REC (1:3) <=   X"E381AE" ) OR

      *                          ( WK-HIRAGANA =       "6"
                                (
      *    *** はーぽ
                                  PIN1-REC (1:3) >=   X"E381AF"
                              AND PIN1-REC (1:3) <=   X"E381BD" ) OR

      *                          ( WK-HIRAGANA =       "7"
                                (
      *    *** まーも E381XX,E382XX
                                  PIN1-REC (1:3) >=   X"E381BE"
                              AND PIN1-REC (1:3) <=   X"E381BF" ) OR

      *                          ( WK-HIRAGANA =       "7"
                                (
                                  PIN1-REC (1:3) >=   X"E38280"
                              AND PIN1-REC (1:3) <=   X"E38282" ) OR

      *                          ( WK-HIRAGANA =       "8"
                                (
      *    *** やーよ
                                  PIN1-REC (1:3) >=   X"E38283"
                              AND PIN1-REC (1:3) <=   X"E38288" ) OR

      *                          ( WK-HIRAGANA =       "9"
                                (
      *    *** らーん
                                  PIN1-REC (1:3) >=   X"E38289"
                              AND PIN1-REC (1:3) <=   X"E38293" )
                              MOVE    "Y"           TO      SW-HIRAGANA

                              MOVE    SPACE       TO      POT1-REC
                              MOVE    "#"         TO      POT1-REC (1:1)
                              ADD     1           TO      WK-NO
                              MOVE    WK-NO       TO      POT1-REC (2:4)
                              MOVE    "."         TO      POT1-REC (6:1)
      *    *** 　 UTF8
                              MOVE    X"E38080"   TO      POT1-REC (7:3)
                              MOVE    PIN1-REC (1:3) TO  POT1-REC (10:3)
                              WRITE   POT1-REC
                              ADD     1           TO      WK-POT1-CNT
                          END-IF
                      WHEN OTHER
                          MOVE    SPACE       TO      POT1-REC
                          MOVE    "#"         TO      POT1-REC (1:1)
                          ADD     1           TO      WK-NO
                          MOVE    WK-NO       TO      POT1-REC (2:4)
                          MOVE    "."         TO      POT1-REC (6:1)
      *    *** 　 UTF8
                          MOVE    X"E38080"   TO      POT1-REC (7:3)
                          MOVE    PIN1-REC (1:3) TO   POT1-REC (10:3)
                          WRITE   POT1-REC
                          ADD     1           TO      WK-POT1-CNT
                   END-EVALUATE

      *    *** ジャパリ
               WHEN PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"
                   IF      SW-PER      =       "N"
                           DISPLAY WK-PGM-NAME 
                                   " % タイトルレコード無しエラー"
                           STOP    RUN
                   END-IF
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    "#"         TO      POT1-REC (1:1)
                   IF      PIN1-REC (13:10) =  "#aduxvi-br"
                                           OR  "#aduDMM-br"
                                           OR  "#MissAV-br"
                       MOVE    ZERO        TO      POT1-REC (2:4)
                   ELSE
                       ADD     1           TO      WK-NO
                       MOVE    WK-NO       TO      POT1-REC (2:4)
                   END-IF
                   MOVE    "."         TO      POT1-REC (6:1)
      *    *** 　 UTF8
                   MOVE    X"E38080"   TO      POT1-REC (7:3)
                   IF      WK-PIN1-LEN <=       15
                           MOVE    PIN1-REC (13:3) TO  POT1-REC (10:3)
                   ELSE
                           MOVE    PIN1-REC (13:)  TO  POT1-REC (10:)
                   END-IF

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 汎用タイトル
               WHEN PIN1-REC (1:1) = "%"
                   MOVE    "Y"            TO      SW-PER
                   MOVE    PIN1-REC (1:1) TO      POT1-REC (1:1)
      *    *** テーブル横方向表示数
                   MOVE    "08"           TO      POT1-REC (2:2)
                   MOVE    SPACE          TO      POT1-REC (4:2)
                   MOVE    WK-FILE        TO      POT1-REC (6:3)
      *    *** 32=Youtube 動画サムネイル拡大
                   IF      WK-FILE        =       "032"
      *    *** ＭｉｓｓＡＶ
                       AND PIN1-REC (3:18) =
                           X"EFBCADEFBD89EFBD93EFBD93EFBCA1EFBCB6"
      *    *** 62=ＭｉｓｓＡＶ 動画サムネイル拡大
                           MOVE    "062"          TO      POT1-REC (6:3)
                   END-IF
      *    *** 区分(7:1)、タイトル名(8:NN)は前のPGMから引き継ぐ
                   MOVE    PIN1-REC (3: ) TO      POT1-REC (9:)

      *    *** 32=Youtube 動画サムネイル拡大
      *    *** 34=ＤＭＭ 動画サムネイル拡大
      *    *** 56=neko
      *    *** 62=ＭｉｓｓＡＶ 動画サムネイル拡大
                   IF      WK-FILE     =       "032" OR "034"
                                            OR "056" OR "062"
      *    *** タイトルの後に０２か０５付ける
                           MOVE    WK-WIDTH    TO      
                                   POT1-REC (WK-PIN1-LEN + 8:2)
                   END-IF

                   EVALUATE TRUE
      *    *** 横方向、表示数セット
      *    *** 23=お菓子系．ｃｏｍ
                      WHEN WK-FILE = "023"
                           MOVE    "03"        TO      POT1-REC (2:2)
      *    *** 28=expo_jam_2018
                      WHEN WK-FILE = "028"
      *    *** 29=DMM 検索
                                  OR "029"
      *    *** 21=XVI,22=DMM,30=XVIS
                                  OR "021" OR "022"
                                  OR "030"
      *    *** 35=香港の女性歌手
      *    *** 36=香港の男性歌手
      *    *** 37=中国の女性歌手
      *    *** 38=中国の男性歌手
      *    *** 39=台湾の女性歌手
      *    *** 40=台湾の男性歌手
      *    *** 41=中国大陸女性アーティスト一覧
      *    *** 42=中国大陸男性アーティスト一覧
      *    *** 43=中国大陸グループアーティスト一覧
      *    *** 44=香港台湾女性アーティスト一覧
      *    *** 45=香港台湾男性アーティスト一覧
      *    *** 46=香港台湾グループアーティスト一覧
      *    *** 47=中国系アーティスト一覧
      *    *** 48=中国系アーティスト２一覧
      *    *** 49=中国系アーティスト３一覧
      *    *** 50=MissAV
      *    *** 64=音韓、女性韓国アイドルグループ
                                  OR "035" OR "036" OR "037" OR "038"
                                  OR "039" OR "040" OR "041" OR "042"
                                  OR "043" OR "044" OR "045" OR "046"
                                  OR "047" OR "048" OR "049" OR "050"
                                  OR "064"
                           MOVE    "06"        TO      POT1-REC (2:2)
      *    *** 32=Youtube 動画サムネイル拡大
      *    *** 34=DMM 動画サムネイル拡大
      *    *** 56=neko
      *    *** 62=ＭｉｓｓＡＶ 動画サムネイル拡大
                      WHEN WK-FILE = "032" OR "034"
                                  OR "056" OR "062"
                           MOVE    WK-WIDTH    TO      POT1-REC (2:2)
      *    *** 26=お菓子系２
                      WHEN WK-FILE = "026"
                           MOVE    "06"        TO      POT1-REC (2:2)
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
                      WHEN WK-FILE = "009" OR "016" OR "017"
      *                     MOVE    "12"           TO      POT1-REC (2:2)
                           MOVE    "08"           TO      POT1-REC (2:2)
                      WHEN WK-FILE = "002" OR "005" OR "007"
      *                     MOVE    "10"           TO      POT1-REC (2:2)
                           MOVE    "08"           TO      POT1-REC (2:2)
      *    *** 18 はYOUTUBE のみ、出力
                      WHEN WK-FILE = "018"
                           MOVE    "08"           TO      POT1-REC (2:2)
                           MOVE    "YT"           TO      POT1-REC (4:2)
      *    *** 15 アイドル大図鑑 G OR N SET
                      WHEN WK-FILE = "015"
                           MOVE    PIN1-REC (2:1) TO      POT1-REC (4:1)
      *    *** 11 は芸能人（女性・男性）　追加
                      WHEN WK-FILE = "011"
                           MOVE
                           X"EFBC88E5A5B3E680A7E383BBE794B7E680A7EFBC89"
                                                  TO    POT1-REC (30:21)
                           MOVE    WDT-DATE-YYYY  TO    POT1-REC (52:04)
      *    *** 12 は芸能人（女性）　追加
                      WHEN WK-FILE = "012"
                           MOVE
                           X"EFBC88E5A5B3E680A7EFBC89"
                                                  TO    POT1-REC (30:12)
                           MOVE    PIN1-REC (45:24)
                                                  TO    POT1-REC (42:)
                           MOVE    WDT-DATE-YYYY  TO    POT1-REC (43:04)
      *    *** 14 日本の女優一覧2000年代生まれ 名前順、誕生日順
      *    *** 16 日本の女優一覧1990年代生まれ
      *                WHEN WK-FILE = "014" OR "016"
      *                     MOVE    "07"           TO      POT1-REC (2:2)
      *    *** 21 XVI
      *                WHEN WK-FILE = "021" OR "022"
      *                     MOVE    "06"           TO      POT1-REC (2:2)
                      WHEN OTHER
                           CONTINUE
                   END-EVALUATE

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** WK-FILE=21 XVI, 22 DMM, 24 Qosmio, 25 XVI2, 29 DMM 検索,
      *    *** 30 XVIS, 32 Youtube 動画サムネイル拡大
      *    *** 34 DMM 動画サムネイル拡大
      *    *** 50 MissAV
      *    *** 31 Youtube Channel
      *    *** 検索文字にカンマ＝、あってもそのまま出力
      *    *** 05 韓国のガール・グループ
      *    *** 56=neko
      *    *** 62=ＭｉｓｓＡＶ 動画サムネイル拡大

               WHEN WK-FILE = "021" OR "022" OR "024" OR "025" OR "029"
                           OR "030" OR "032" OR "034"
                           OR "050"
                           OR "031"
                           OR "005"
                           OR "056"
                           OR "062"
                   COMPUTE I = WK-PIN1-LEN + 1
                   MOVE    ","         TO      PIN1-REC (I:1)

                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT

               WHEN OTHER
      *    *** 11 は芸能人（女性・男性）
      *    *** 12 は芸能人（女性）
                   IF      WK-FILE     =       "011" OR "012"
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
                                             "  <br>"
                           INSPECT PIN1-REC 
      *    *** （
                               REPLACING ALL X"EFBC88" BY "  ,"
                           INSPECT PIN1-REC 
      *    *** ）
                               REPLACING ALL X"EFBC89" BY "   "
                       END-IF
                       MOVE    WK-YYYY TO  PIN1-REC (WK-PIN1-LEN + 2:4)
                       ADD     6           TO      WK-PIN1-LEN
      *    *** 年と月日　位置変更
                       PERFORM S110-10     THRU    S110-EX
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
      *    *** 12 は芸能人（女性）
                       WHEN WK-FILE = "012"
      *    *** 女性のみ出力
                         IF WK-SEX = ZERO
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                         END-IF
      *    *** 26=お菓子系２
                       WHEN WK-FILE = "026"
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

      *    *** 11 は芸能人（女性・男性）
      *    *** 12 は芸能人（女性）
      *    *** 年と月日　位置変更
       S110-10.

           MOVE    PIN1-REC    TO      WK-REC
           MOVE    SPACE       TO      PIN1-REC
           MOVE    ZERO        TO      WK-BR-CNT
                                       WK-KANMA-CNT
           MOVE    1           TO      P
           MOVE    "N"         TO      SW-END

           INSPECT WK-REC TALLYING
                   WK-BR-CNT FOR ALL "<br>"

           PERFORM VARYING L FROM 1 BY 1
                           UNTIL L > WK-PIN1-LEN
                              OR SW-END = "Y"
                   IF      WK-REC (L:1) =      ","
                           ADD     1           TO      WK-KANMA-CNT
                           MOVE    ","         TO      PIN1-REC (P:1)
                           ADD     1           TO      P
                                                       L

                           IF      WK-BR-CNT   =       ZERO
      *    *** <br> 無しの時
      *    *** 鈴木福  ,6月17日    2004 ,
      *    *** 変更後,PIN1-REC
      *    *** 鈴木福  ,2004年6月17日 ,

      *    *** 年数セット
                               MOVE    WK-REC (WK-PIN1-LEN - 4:4) TO
                                       PIN1-REC (P:4)
                               ADD     4           TO      P

      *    *** 年
                               MOVE    X"E5B9B4"   TO     PIN1-REC (P:3)
                               ADD     3           TO      P

                               PERFORM VARYING L2 FROM L BY 1
                                         UNTIL L2 > WK-PIN1-LEN
                                            OR SW-END = "Y"
                                   IF      WK-REC (L2:1) =      SPACE
                                       MOVE    "Y"         TO     SW-END
                                       MOVE    " ,"        TO
                                               PIN1-REC (P:2)
                                       ADD     2           TO      P
                                   ELSE
                                       MOVE    WK-REC (L2:1) TO
                                               PIN1-REC (P:1)
                                       ADD     1           TO      P
                                   END-IF
                               END-PERFORM
                           ELSE

      *    *** <br> 有りの時
      *    *** 岡村ほまれ  ,モーニング娘。  <br>5月9日    2005 ,
      *    *** 変更後,PIN1-REC
      *    *** 岡村ほまれ  ,モーニング娘。  <br>2005年5月9日 ,

                               PERFORM VARYING L2 FROM L BY 1
                                         UNTIL L2 > WK-PIN1-LEN
                                            OR SW-END = "Y"
                                   IF      WK-REC (L2:4) =      "<br>"
                                       MOVE    "<br>"       TO
                                               PIN1-REC (P:4)
                                       ADD     4           TO      P
                                                                   L2
      *    *** 年数セット
                                       MOVE   WK-REC (WK-PIN1-LEN - 4:4)
                                               TO PIN1-REC (P:4)
                                       ADD     4           TO      P

      *    *** 年
                                       MOVE    X"E5B9B4"   TO
                                               PIN1-REC (P:3)
                                       ADD     3           TO      P

                                       PERFORM VARYING L1 FROM L2 BY 1
                                               UNTIL L1 > WK-PIN1-LEN
                                                  OR SW-END = "Y"
                                           IF      WK-REC (L1:1) = SPACE
                                               MOVE    "Y"         TO
                                                       SW-END
                                               MOVE    " ,"        TO
                                                       PIN1-REC (P:2)
                                               ADD     2           TO  P
                                               MOVE    P           TO
                                                       WK-PIN1-LEN
                                           ELSE
                                               MOVE    WK-REC (L1:1) TO
                                                       PIN1-REC (P:1)
                                               ADD     1           TO  P
                                           END-IF
                                       END-PERFORM
                                   ELSE
                                       MOVE    WK-REC (L2:1) TO
                                               PIN1-REC (P:1)
                                       ADD     1           TO      P
                                   END-IF
                               END-PERFORM
                           END-IF
                   ELSE
      *    *** 最初のカンマ来るまで
                       MOVE    WK-REC (L:1) TO     PIN1-REC (P:1)
                       ADD     1           TO      P
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           CLOSE   POT1-F

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
