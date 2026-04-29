      *    *** bookmarks.html MissAV データ抽出
      *    *** 
      *    *** 私は叡智を極める修行中の身であるため普段から複数の
      *    *** 無料アダルトサイトで修行を重ねていますので、＜＝他からの流用
      *    *** アクセスしやすいように、htmlを作っています。
      *    *** このプログラムは、このような内容の為、
      *    *** １８禁（１８歳未満使用不可）です。
      *    *** 使用については、参照者のモラルに任せます。
      *    *** 
      *    ***
      *    *** タイトル１桁目 #=>_# にする、内部記号と競合するため
      *    *** 
      *    ***
      *    *** JOB TEST10   TESTXX   TEST84
      *    ***        |        |        |
      *    ***        |--------|--------|
      *    ***     TEST74
      *    ***        |
      *    ***     TEST53
      *    ***        |
      *    ***     TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST74.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ ＵＴＦ８
      *    *** TEST10.POT1 => TEST74.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG データ
      *    *** 現在、PIN2は未使用
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST72 DMM用と同様にするため、PIN3-F は削除した

      *    *** 検索 女優名追加データ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 NUM,ALPHA追加データ
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XVI データ (女優分)
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG アンマッチデータ　次回 IMG HTML 追加してIMG データ
      *    *** TEST74.PIN2 へ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
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

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST74  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
      *    *** TEST10 で
      *    "bookmarks.html" => TEST10.POT1
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST74.PIN2".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST70.PIN4".
           03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST110.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST74.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST74.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN5-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN5-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-A            PIC  X(1000) VALUE SPACE.
           03  WK-ID           PIC  X(040) VALUE SPACE.
           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-IMG          PIC  X(500) VALUE SPACE.
           03  WK-DISP         PIC  X(020) VALUE SPACE.

           03  WK-A-LEN        BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-2      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-3      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-4      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-5      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-6      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-7      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-8      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-9      BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN5-ITEM1   PIC  X(100) VALUE SPACE.
           03  WK-PIN5-ITEM2   PIC  X(100) VALUE SPACE.

           03  WK-PIN5-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-ITEM2-LEN BINARY-LONG SYNC VALUE ZERO.

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
           03  I-MAX           BINARY-LONG SYNC VALUE 2000.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  J3-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J4              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** テーブルサイズ変更したら、I-MAX も変更する
           03  TBL01-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL01-ID
                               INDEXED BY TBL01-IDX.
             05  TBL01-ID      PIC  X(040) VALUE HIGH-VALUE.
             05  TBL01-IMG     PIC  X(500) VALUE SPACE.
             05  TBL01-TITLE   PIC  X(500) VALUE SPACE.
             05  TBL01-PIN2-REC PIC X(1000) VALUE SPACE.
             05  TBL01-SET     PIC  X(001) VALUE SPACE.
             05  TBL01-IMG-LEN BINARY-LONG SYNC VALUE ZERO.

           03  TBL03-AREA      OCCURS 5000.
             05  TBL03-ITEM    PIC  X(100) VALUE SPACE.
             05  TBL03-ITEM-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL03-CNT     BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
           03  SW-A            PIC  X(001) VALUE "N".
           03  SW-MISSAV       PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** IMG セットに規則性あったので、テーブルセットを止める

      *     PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *             IF  PIN2-REC (1:19) = "https://missav.com/"
      *    *** PIN2 TBL SET
      *                 PERFORM S032-10     THRU    S032-EX
      *             END-IF

      *    *** READ PIN2
      *             PERFORM S030-10     THRU    S030-EX
      *     END-PERFORM

      *    *** TBL01 SORT
      *     SORT    TBL01-AREA
      *             ASCENDING KEY TBL01-ID



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** MissAV まで読み飛ばし
           PERFORM UNTIL   SW-MISSAV = "Y"
                        OR WK-PIN1-EOF   =     HIGH-VALUE
                   IF      PIN1-REC (1:6) = "MissAV"
                       AND WK-PIN1-LEN =    6

      *    *** WRITE POT1 HEAD
                       PERFORM S040-10     THRU    S040-EX
                       MOVE    "Y"         TO      SW-MISSAV
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** READ PIN1 </H3>
           PERFORM S020-10     THRU    S020-EX



      *    *** WRITE POT1 HEAD 2
           PERFORM S042-10     THRU    S042-EX

      *    *** READ PIN4
           PERFORM S060-10     THRU    S060-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE

      *    *** 検索 女優名追加データ出力
      *    *** WRITE POT1 HEAD
                   PERFORM S062-10     THRU    S062-EX

      *    *** READ PIN4
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM



      *    *** READ PIN5
           PERFORM S070-10     THRU    S070-EX

      *     PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE
      *    *** ジャパリあ まで読み飛ばし
      *    *** DMM と同じにする、中国語削除
      *                OR PIN5-REC (13:3) = X"E38182"

      *    *** READ PIN5
      *             PERFORM S070-10     THRU    S070-EX

      *     END-PERFORM

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE

      *    *** ジャパリ
                   IF      PIN5-REC (1:12) = X"E382B8E383A3E38391E383AA"
                           MOVE    PIN5-REC    TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S080-10     THRU    S080-EX
                   ELSE

      *    *** 検索 NUM,APLHA追加データ出力
      *    *** WRITE POT1 HEAD
                           PERFORM S072-10     THRU    S072-EX
                   END-IF

      *    *** READ PIN5
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM



           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

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
                               PIN2-F
                               PIN4-F
                               PIN5-F
                   OUTPUT      POT1-F
                               POT2-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           SET     TBL01-IDX   TO      1

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END

                   DISPLAY WK-PGM-NAME " TITLE-ID ダブリ"
                   PERFORM VARYING J3 FROM 1 BY 1
                           UNTIL J3 > J3-MAX
                           IF      TBL03-CNT (J3) NOT = ZERO 
                                   DISPLAY TBL03-ITEM (J3) (1:80) 
                           END-IF
                   END-PERFORM

]                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
      *             MOVE    "N"         TO      SW-FIRST
               NOT AT END
      *             IF      SW-FIRST    =       "N"
                           ADD     1           TO      WK-PIN1-CNT
      *             END-IF
           END-READ
           .
       S020-EX.
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

      *    *** <A HREF="https://missav.com/dm19/ja/hunbl-122" ADD_DATE="1716811236" ICON="XXX">
      *    *** 皆瀬あかり HUNBL-122 ［失踪届］アカリ ボクの妹を探しています。 - MissAV.com | オンラインで無料

      *     IF      WK-TITLE (1:19) = "https://missav.com/"
           IF      WK-TITLE (1:19) = "https://missav.ai/"
                   UNSTRING WK-TITLE (20:41) DELIMITED BY SPACE
                       INTO TBL01-ID (TBL01-IDX)
           ELSE
                   CONTINUE
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

           MOVE    "% MissAV," TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S040-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 2
       S042-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    "MissAV-Search-Actress-name"
                               TO      POT1-REC (13:26)

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
            .
       S042-EX.
           EXIT.

      *    *** READ PIN4
       S060-10.

           READ    PIN4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ
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

      *     MOVE    "https://missav.com/ja/search/"
           MOVE    "https://missav.ai/ja/search/"
                               TO      POT1-REC (J:29)
           ADD     29          TO      J

           MOVE    PIN4-REC (1:WK-PIN4-LEN)  
                               TO      POT1-REC (J:WK-PIN4-LEN)
           ADD     WK-PIN4-LEN TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
           .
       S062-EX.
           EXIT.

      *    *** READ PIN5
       S070-10.

           MOVE    SPACE       TO      WK-PIN5-ITEM1
                                       WK-PIN5-ITEM2
           MOVE    ZERO        TO      WK-PIN5-ITEM1-LEN
                                       WK-PIN5-ITEM2-LEN

           READ    PIN5-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
                   UNSTRING PIN5-REC
                           DELIMITED BY ","
                           INTO
                           WK-PIN5-ITEM1 COUNT WK-PIN5-ITEM1-LEN
                           WK-PIN5-ITEM2 COUNT WK-PIN5-ITEM2-LEN
           END-READ
           .
       S070-EX.
           EXIT.

      *    *** 検索NUM,ALPHA追加データ出力
      *    *** WRITE POT1 HEAD
       S072-10.

           MOVE    WK-PIN5-ITEM1 TO    POT1-REC

           ADD     WK-PIN5-ITEM1-LEN 1 GIVING J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

      *     MOVE    "https://missav.com/ja/search/"
           MOVE    "https://missav.ai/ja/search/"
                               TO      POT1-REC (J:28)
      *     ADD     29          TO      J
           ADD     28          TO      J

           MOVE    PIN5-REC (1:WK-PIN5-ITEM1-LEN)  
                               TO      POT1-REC (J:WK-PIN5-ITEM1-LEN)
           ADD     WK-PIN5-ITEM1-LEN TO J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
           .
       S072-EX.
           EXIT.

      *    *** WRITE POT1
       S080-10.

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S080-EX.
           EXIT.

      *    *** html 解析
       S100-10.

           EVALUATE TRUE

               WHEN PIN1-REC (1:3) = "<H3"
                   MOVE    "Y"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   AND
      *    *** H3 の #MissAV-br は残す
                    PIN1-REC (1:10) = "#MissAV-br"
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      POT1-REC (13:WK-PIN1-LEN)

      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3

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

      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3

               WHEN PIN1-REC (1:9) = '<A HREF="'
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
                              OR PIN1-REC (I:12) = "- MissAV.com"

      *    *** 指定した文字以降削除して出力

      *    *** 出演のAV映画を
                              OR PIN1-REC (I:20) = 
                             X"E587BAE6BC94E381AE4156E698A0E794BBE38292"

      *    *** - MissAV
                              OR PIN1-REC (I:8) = 
                             X"2D204D6973734156"

      *    *** AVをオンラインで見る
                              OR PIN1-REC (I:29) = 
           X"4156E38292E382AAE383B3E383A9E382A4E383B3E381A7E8A68BE3828B"

      *    *** の検索結果
                              OR PIN1-REC (I:15) = 
                                 X"E381AEE6A49CE7B4A2E7B590E69E9C"
                       EVALUATE TRUE
                           WHEN PIN1-REC (I:1) = ","
                               ADD     1           TO      I3
                               MOVE    "."    TO    PIN1-REC (I:1)
                               MOVE    PIN1-REC (I:1) TO POT1-REC (I3:1)

      *    *** 、
      *                     WHEN PIN1-REC (I:3) = X"E38081"
      *                         ADD     1           TO      I3
      *    *** ．
      *                         MOVE    X"EFBC8E"   TO    PIN1-REC (I:3)
      *                         MOVE    PIN1-REC (I:3) TO POT1-REC (I3:3)
      *                         ADD     2           TO      I
      *                                                     I3

                           WHEN OTHER
                               ADD     1           TO      I3
                               MOVE    PIN1-REC (I:1) TO POT1-REC (I3:1)
                       END-EVALUATE
                   END-PERFORM


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


                   ADD     2           TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   ADD     1           TO      I3
                   MOVE    WK-A (1:I2) TO      POT1-REC (I3:I2)

                   ADD     2 I2        TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   MOVE    "N"         TO      SW-SEARCH
                   MOVE    ZERO        TO      WK-COUNT-1
                                               WK-COUNT-2
                                               WK-COUNT-3
                                               WK-COUNT-4
                                               WK-COUNT-5
                                               WK-COUNT-6
                                               WK-COUNT-7
                                               WK-COUNT-8
                                               WK-COUNT-9
                   INSPECT WK-A TALLYING
                           WK-COUNT-1 FOR ALL "actresses"
                           WK-COUNT-2 FOR ALL "search"
                           WK-COUNT-3 FOR ALL "makers"
                           WK-COUNT-4 FOR ALL "series"
                           WK-COUNT-5 FOR ALL "genres"
                           WK-COUNT-6 FOR ALL "tags"
                           WK-COUNT-7 FOR ALL "labels"
                           WK-COUNT-8 FOR ALL "actors"
                           WK-COUNT-9 FOR ALL "directors"

      *    *** "https://missav.com/ja " は検索先頭を除外のため
      *             IF      WK-A (1:22) = "https://missav.com/ja "
                   IF      WK-A (1:21) = "https://missav.ai/ja "
                        OR WK-COUNT-1 NOT = ZERO
                        OR WK-COUNT-2 NOT = ZERO
                        OR WK-COUNT-3 NOT = ZERO
                        OR WK-COUNT-4 NOT = ZERO
                        OR WK-COUNT-5 NOT = ZERO
                        OR WK-COUNT-6 NOT = ZERO
                        OR WK-COUNT-7 NOT = ZERO
                        OR WK-COUNT-8 NOT = ZERO
                        OR WK-COUNT-9 NOT = ZERO
                       ADD     2           TO      I3
                       MOVE    ","         TO      POT1-REC (I3:1)
                   ELSE
      *    *** IMG SET
      *                 PERFORM S110-10     THRU    S110-EX
      *    *** IMG SET2
                       PERFORM S120-10     THRU    S120-EX
                   END-IF

                   IF      SW-SEARCH   =       "Y"
      *                 EVALUATE TRUE
      *                     WHEN 
                               MOVE    "*"         TO
                                                  TBL01-SET (TBL01-IDX)

      *    *** この組合は無いはず
      *                     WHEN OTHER
      *                         CONTINUE
      *                 END-EVALUATE
                   ELSE
                       CONTINUE
                   END-IF
      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-A

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S100-EX.
           EXIT.

      *    *** IMG SET
       S110-10.

      *     IF      WK-A (1:19) = "https://missav.com/"
           IF      WK-A (1:18) = "https://missav.ai/"
                   UNSTRING WK-A (20:41) DELIMITED BY '"'
                       INTO WK-ID
           ELSE
                   CONTINUE
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

                   ADD     2 I4        TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)
           END-SEARCH
           .
       S110-EX.
           EXIT.

      *    *** IMG SET2
       S120-10.

      *    *** 2025.01.09 アドレス変わった 
      *    *** https://missav.ws/ja/fc2-ppv-3865704 <= https://missav.com/xxx
      *    *** https://fourhoi.com/fc2-ppv-3865704/cover-n.jpg => https://fivetiu.com/xxx
      *    *** 変更後　https://fivetiu.com/ でもサムネイル表示されるので、
      *    *** 修正しない

      *    *** IMG セットに規則性あったので、テーブルセットを止める
      *    *** https://fivetiu.com/miae-045/cover-n.jpg
           ADD     1           TO      I3
      *     MOVE    "https://fivetiu.com/"
           MOVE    "https://fourhoi.com/"
                               TO      POT1-REC (I3:20)

           ADD     20          TO      I3
           MOVE    ZERO        TO      L2

           PERFORM VARYING L FROM WK-A-LEN BY -1
                   UNTIL WK-A (L:1) = "/"
                   ADD     1           TO      L2
           END-PERFORM

      *    *** miae-045 SET
           MOVE    WK-A (L + 1:L2) TO  POT1-REC (I3:L2)
           ADD     L2          TO      I3

           MOVE    "/cover-n.jpg" TO   POT1-REC (I3:12)

           ADD     12 1        TO      I3
           MOVE    ","         TO      POT1-REC (I3:1)

           IF      WK-A        NOT =   "https://missav.ai/dm10/ja"
      *    *** TBL03 SET
                   PERFORM S122-10     THRU    S122-EX
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** TBL03 SET
       S122-10.

           ADD     1           TO      J3
           IF      J3          >       5000
                   DISPLAY WK-PGM-NAME " TBL03 OVER J3=" J3
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      TBL03-ITEM     (J3)
           MOVE    ZERO        TO      TBL03-ITEM-LEN (J3)
                                       TBL03-CNT      (J3)

           UNSTRING WK-A
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
                   MOVE    WK-ITEM5    TO      TBL03-ITEM     (J3)
                   MOVE    WK-ITEM5-LEN TO     TBL03-ITEM-LEN (J3)
           ELSE
                   MOVE    WK-ITEM6    TO      TBL03-ITEM     (J3)
                   MOVE    WK-ITEM6-LEN TO     TBL03-ITEM-LEN (J3)
           END-IF

           IF      TBL03-ITEM-LEN (J3) > 100
                   DISPLAY WK-PGM-NAME " WK-領域 長さオーバー"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                           " TBL03-ITEM-LEN (J3)=" TBL03-ITEM-LEN (J3)
                   STOP    RUN
           END-IF

           MOVE    J3          TO      J3-MAX

           MOVE    J3          TO      J4
           MOVE    TBL03-ITEM-LEN (J3) TO L4

           PERFORM VARYING J3 FROM 1 BY 1
                   UNTIL J3 = J3-MAX
                   IF      TBL03-ITEM (J4) (1:L4) = 
                           TBL03-ITEM (J3) (1:L4)
                           ADD     1           TO       TBL03-CNT (J3)
                   END-IF
           END-PERFORM
           .
       S122-EX.
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
      *    *** 画像リンクデータも整理するため、TEST74.POT3 追加した
      *    *** 次回 TEST74.PIN2 インプットにする
      *                 WRITE   POT3-REC    FROM    TBL01-PIN2-REC (I) 

      *                 IF      WK-POT3-STATUS =    ZERO
      *                         ADD     1           TO      WK-POT3-CNT
      *                 ELSE
      *                         DISPLAY WK-PGM-NAME 
      *                                 " POT3-F WRITE ERROR STATUS="
      *                                 WK-POT3-STATUS
      *                         STOP    RUN
      *                 END-IF
                           CONTINUE
                   END-IF
           END-PERFORM

           CLOSE   PIN1-F
                   PIN2-F
                   PIN4-F
                   PIN5-F
                   POT1-F
                   POT2-F

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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
