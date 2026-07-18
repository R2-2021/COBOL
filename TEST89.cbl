      *    *** DMM 検索結果 画像拡大表示 横６個
      *    *** 横６個の指定はTEST53.CBLで行う
      *    *** 
      *    *** 私は叡智を極める修行中の身であるため普段から複数の
      *    *** 無料アダルトサイトで修行を重ねていますので、＜＝他からの流用
      *    *** アクセスしやすいように、htmlを作っています。
      *    *** このプログラムは、このような内容の為、
      *    *** １８禁（１８歳未満使用不可）です。
      *    *** 使用については、参照者のモラルに任せます。
      *    *** 
      *    *** 旧 python 記述、
      *    *** 
      *    *** import requests
      *    *** # ＤＭＭ検索で渚みつき　後、ページ表示数１２０変更後、アドレスコピーする
      *    *** 
      *    *** res = requests.get('https://www.dmm.co.jp/litevideo/-/list/search/=/searchstr=%E6%B8%9A%E3%81%BF%E3%81%A4%E3%81%8D/n1=DgRJTglEBQ4GpoD6%2CYyI%2Cqs_/sort=date/')
      *    *** with open('dmm_渚みつき_単体作品.html','w',encoding='utf8') as file:
      *    ***     file.write(res.text)
      *    ***
      *    *** 下記を使用したpythonに変更した
      *    *** import os
      *    *** import time
      *    *** from selenium import webdriver
      *    *** 
      *    *** HTML 記述が変更になった為、このプログラム使用不可だったが、
      *    *** 修正して直した
      *    *** INPUT HTML 変更の為、現在使用不可、TEST53 034 で実行する
      *    *** 
      *    *** TEST10  
      *    ***   ↓
      *    *** TEST89     TEST116
      *    ***   ↓         ↓
      *    *** TEST53 029 TEST53 034
      *    ***   ↓         ↓
      *    *** TEST54     TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST89.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** python DMM_渚みつき.py より
      *    *** TEST10.POT1 HTML 解析データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** html データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(2000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST89  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST89.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** PIN1-REC 内容
      *<a href="https://www.dmm.co.jp/litevideo/-/detail/=/cid=crnx00028/?i3_ref=search&amp;i3_ord=1">

      *<span class="img">
      *<img src="//pics.dmm.co.jp/digital/video/crnx00028/crnx00028pt.jpg" alt="初めて彼女のできた童貞のボクのためセックスの練習相手になってくれたビッチな幼なじみ。 渚みつき">
      *</span>


      *    *** 動画再生サイト
           03  WK-HREF         PIC  X(250) VALUE SPACE.

      *    *** タイトル画像サイト
           03  WK-IMG
             05                PIC  X(006) VALUE "https:".
             05  WK-IMG2       PIC  X(250) VALUE SPACE.

      *    *** タイトル名称
           03  WK-ALT          PIC  X(1000) VALUE SPACE.

           03  WK-HREF-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-ALT-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-MIU.
             05                PIC  X(017) VALUE
                               X"E3839FE382A6E383BBE382B6E383BBE383".
             05                PIC  X(016) VALUE
                               X"B4E382A1E383BCE38381E383A3E383AB".

           03  WK-TITLE1.
      *    *** ジャパリ検索
             05                PIC  X(015)
                 VALUE X"E382B8E383A3E38391E383AAE6A49C"
             05                PIC  X(006)
                 VALUE X"E7B4A2".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO.
           03  X1              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-IMG          PIC  X(001) VALUE "N".
           03  SW-ALT          PIC  X(001) VALUE "N".
           03  SW-TITLE        PIC  X(001) VALUE "N".
           03  SW-POL          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
      *    *** <title> まで読み飛ばし
                   IF      PIN1-REC (1:7) = '<title>'

      *    *** READ PIN1 TITLE名
                           PERFORM S020-10     THRU    S020-EX

      *    *** WRITE POT1 TITLE
      *                     PERFORM S100-10     THRU    S100-EX

      *    *** WRITE POT1 TITLE 2
                           PERFORM S102-10     THRU    S102-EX
                           EXIT    PERFORM
                   END-IF
           END-PERFORM



      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** <ul id="list"> まで読み飛ばし
      *                OR PIN1-REC (1:14) = '<ul id="list">'

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1 DETAIL
      *             PERFORM S110-10     THRU    S110-EX

      *    *** WRITE POT1 DETAIL 2
                   PERFORM S120-10     THRU    S120-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

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
                   ADD     1          TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1 TITLE
       S100-10.

      *    *** % title名, <= 出力
           MOVE    "% "        TO      POT1-REC
           MOVE    3           TO      P
      *    *** 渚みつきの検索結果 - FANZA
      *    *** 8 =  - FANZA の長さ
           IF      PIN1-REC (1:WK-PIN1-LEN - 8) = 
      *    *** ポルチオの検索結果
               X"E3839DE383ABE38381E382AAE381AEE6A49CE7B4A2E7B590E69E9C"
                   MOVE    "Y"         TO      SW-POL
           END-IF
           MOVE    PIN1-REC (1:WK-PIN1-LEN - 8) 
                               TO      POT1-REC (P:WK-PIN1-LEN - 8)
           COMPUTE P = P + WK-PIN1-LEN - 8
           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           WRITE   POT1-REC    FROM    WK-TITLE1
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 TITLE 2
       S102-10.

      *    *** % title名, <= 出力
      *    *** % ＤＭＭ　
           MOVE    X"2520EFBCA4EFBCADEFBCADE38080"
                               TO      POT1-REC
           MOVE    15          TO      P

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:3) = " - "
                   MOVE    PIN1-REC (J:1) TO   POT1-REC (P:1)
                   ADD     1           TO      P
           END-PERFORM

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           WRITE   POT1-REC    FROM    WK-TITLE1
           ADD     1           TO      WK-POT1-CNT
           .
       S102-EX.
           EXIT.

      *    *** WRITE POT1 DETAIL
       S110-10.

           EVALUATE TRUE

               WHEN PIN1-REC(1:9) =    '<a href="'
                   MOVE    PIN1-REC(10:WK-PIN1-LEN - 11)
                                       TO      WK-HREF
                   COMPUTE WK-HREF-LEN = WK-PIN1-LEN - 11

               WHEN PIN1-REC(1:16) =   '<img src="//pics'
                   MOVE    "Y"         TO      SW-IMG
                   MOVE    "N"         TO      SW-ALT
                   MOVE    SPACE       TO      WK-IMG2
                                               WK-ALT
                   MOVE    ZERO        TO      I2
                                               I3
                   PERFORM VARYING I FROM 11 BY 1
                           UNTIL I > WK-PIN1-LEN

                       IF      PIN1-REC (I:7) =    '" alt="'
                           MOVE    "N"         TO      SW-IMG
                           MOVE    "Y"         TO      SW-ALT
                           ADD     7           TO      I
                       END-IF

                       IF      PIN1-REC (I:2) =    '">'
                           MOVE    "N"         TO      SW-ALT
                           ADD     2           TO      I
                       END-IF

                       IF      SW-IMG      =       "Y"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1) TO   WK-IMG2 (I2:1)
                       END-IF

                       IF      SW-ALT      =       "Y"
                           ADD     1           TO      I3
                           MOVE    PIN1-REC (I:1) TO   WK-ALT (I3:1)
                       END-IF
                   END-PERFORM

      *    *** IMG 解像度 pt=>ps
      *    *** pl はビデオタイトル画
                   ADD     I2 6        GIVING  WK-IMG-LEN
      *    *** SW-POL = "Y" ポルチオの検索結果 ?
                   IF      SW-POL      =       "Y"
      *    *** 3*6=18 6文字か？
                           IF      I3          >       18
      *    *** ミウ・ザ・ヴァーチャル
                               EVALUATE TRUE
                                   WHEN WK-ALT (1:33) = WK-MIU
                                       MOVE    "jm"        TO
                                           WK-IMG2 (WK-IMG-LEN - 11:2)
                                   WHEN WK-ALT (1:21) =
      *    *** みらい＆あんな
                           X"E381BFE38289E38184EFBC86E38182E38293E381AA"
                                   OR   WK-ALT (1:20) =
      *    *** 天才もりもと 2
                           X"E5A4A9E6898DE38282E3828AE38282E381A82032"
                                       MOVE    "jp"        TO
                                           WK-IMG2 (WK-IMG-LEN - 11:2)
                                   WHEN OTHER
                                       MOVE    "ps"        TO
                                           WK-IMG2 (WK-IMG-LEN - 11:2)
                               END-EVALUATE
                           ELSE
                               IF      WK-ALT (1:9) =      
      *    *** みずな
                                       X"E381BFE3819AE381AA"
                                   MOVE    "ps"        TO
                                           WK-IMG2 (WK-IMG-LEN - 11:2)
                               ELSE
                                   MOVE    "jm"        TO
                                           WK-IMG2 (WK-IMG-LEN - 11:2)
                               END-IF
                           END-IF
                   ELSE
                           MOVE    "ps"        TO
                                   WK-IMG2 (WK-IMG-LEN - 11:2)
                   END-IF
      *             MOVE    "pl"         TO   WK-IMG2 (WK-IMG-LEN - 11:2)
                   MOVE    I3          TO      WK-ALT-LEN

                   MOVE    WK-ALT      TO      POT1-REC
                   ADD     WK-ALT-LEN 1 GIVING P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    WK-HREF     TO      POT1-REC  (P:WK-HREF-LEN)
                   ADD     WK-HREF-LEN TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    WK-IMG      TO      POT1-REC (P:WK-IMG-LEN)
                   ADD     WK-IMG-LEN  TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1 DETAIL 2
       S120-10.

           EVALUATE TRUE

               WHEN PIN1-REC(1:35) =
                    '<a href="/litevideo/-/detail/=/cid='

                   MOVE
                       "https://www.dmm.co.jp/litevideo/-/detail/=/cid="
                                       TO      WK-HREF

                   MOVE    47          TO      P2
                   PERFORM VARYING P1 FROM 36 BY 1
                           UNTIL PIN1-REC(P1:1) = '"'
                           ADD     1           TO      P2
                           MOVE    PIN1-REC(P1:1) TO   WK-HREF (P2:1)
                   END-PERFORM
                   MOVE    P2          TO      WK-HREF-LEN

               WHEN PIN1-REC(WK-PIN1-LEN - 5:6) =   '.jpg">'
                   MOVE    "ps"        TO    PIN1-REC(WK-PIN1-LEN - 7:2)
                   MOVE    SPACE       TO      WK-ALT
                   MOVE    ZERO        TO      P3
                   PERFORM VARYING P1 FROM 11 BY 1
                           UNTIL PIN1-REC(P1:1) = '"'
                           ADD     1           TO      P3
                           IF      PIN1-REC(P1:1) =    ","
                               MOVE    "."         TO     PIN1-REC(P1:1)
                           END-IF

                           IF      PIN1-REC(P1:1) =    " "
                               IF (PIN1-REC(P1 + 1:1) >=   "a"
                               AND PIN1-REC(P1 + 1:1) <=   "z" )
                              OR  (PIN1-REC(P1 + 1:1) >=   "A"
                               AND PIN1-REC(P1 + 1:1) <=   "Z" )
                              OR  (PIN1-REC(P1 + 1:1) >=   "0"
                               AND PIN1-REC(P1 + 1:1) <=   "9" )
                                  MOVE   PIN1-REC(P1:1) TO WK-ALT (P3:1)
                               ELSE
                                  MOVE    "<br><br>"  TO    WK-ALT(P3:8)
                                  ADD     7           TO     P3
                               END-IF
                           ELSE
                               MOVE    PIN1-REC(P1:1) TO   WK-ALT (P3:1)
                           END-IF
                   END-PERFORM
                   MOVE    P3          TO      WK-ALT-LEN

      *    *** src=" まで読み飛ばし
                   PERFORM VARYING X1 FROM P1 BY 1
                           UNTIL PIN1-REC(X1:5) = 'src="'
                           CONTINUE
                   END-PERFORM

                   COMPUTE X1 = X1 + 5
                   MOVE    SPACE       TO      WK-IMG
                   MOVE    ZERO        TO      P4
                   PERFORM VARYING P1 FROM X1 BY 1
                           UNTIL PIN1-REC(P1:1) = '"'
                           ADD     1           TO      P4
                           MOVE    PIN1-REC(P1:1) TO   WK-IMG (P4:1)
                   END-PERFORM
                   MOVE    P4          TO      WK-IMG-LEN

                   MOVE    WK-ALT      TO      POT1-REC
                   ADD     WK-ALT-LEN 1 GIVING P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    WK-HREF     TO      POT1-REC (P:WK-HREF-LEN)
                   ADD     WK-HREF-LEN TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    WK-IMG      TO      POT1-REC (P:WK-IMG-LEN)
                   ADD     WK-IMG-LEN  TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   WRITE   POT1-REC
                   ADD     1            TO     WK-POT1-CNT

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S120-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

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
