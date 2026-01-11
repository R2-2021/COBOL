      *    *** ＤＭＭ 動画サムネイル、自動付加
      *    *** 
      *    *** 私は叡智を極める修行中の身であるため普段から複数の
      *    *** 無料アダルトサイトで修行を重ねていますので、＜＝他からの流用
      *    *** アクセスしやすいように、htmlを作っています。
      *    *** このプログラムは、このような内容の為、
      *    *** １８禁（１８歳未満使用不可）です。
      *    *** 使用については、参照者のモラルに任せます。

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST116.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ＤＭＭ　抽出名指定、ＵＴＦ８で指定
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** DMM動画 ページ、CTRL+ A,CTRL+ C Googleスプレッドに貼り付け、
      *    *** 不要部分カット、
      *    *** ダウンロード（ｈｔｍｌ）後、.zipを右クリック、すべてを展開、
      *    *** サクラeditでコピー後、ネットのツール、ＵＲＬ抽出後、
      *    *** ＵＲＬをGoogleスプレッドに貼り付けの別の列に貼り付け、
      *    *** ダウンロード（ＣＳＶ（ＵＴＦ８））で出力後、
      *    *** サクラエディターで出力モード（ＢＯＭ）をＯＦＦ
      *    *** で再出力する
      *    *** ダウンロードしたファイル名は 
      *    *** TEST116.ＤＭＭ　ＸＸＸ.PIN1 に変更し、フォルダーCOBOLへ
      *    *** TEST116.PRM1で抽出名ＤＭＭ　ＸＸＸを指定する
      *    *** 
      *    *** 画像アドレス最後の文字、pl,ps,ptが有効
      *    *** https://pics.dmm.co.jp/digital/video/ymds00022/ymds00022ps.jpg
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG 変換パラメータ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** サムネイルimg データ
      *    *** タイトル,タイトル動画URL,タイトルサムネイル,
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
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
           03                  PIC  X(10000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST116 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST116.PRM1 ".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST116.PIN1".
      *    *** ファイル名はＳＪＩＳに変換してセット
           03  WK-PIN1-F-NAME  PIC  X(128) VALUE "XXXXXXXX.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(128) VALUE "TEST116.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST116.POT1".

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS1-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS3-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS4-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS5-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS6-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS7-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS8-LEN   BINARY-LONG SYNC VALUE ZERO.

           03  WK-FILE-NAME    PIC  X(128) VALUE SPACE.
           03  WK-TITLE        PIC  X(1000) VALUE SPACE.
           03  WK-TITLE2       PIC  X(1000) VALUE SPACE.
           03  WK-HTTPS        PIC  X(1000) VALUE SPACE.
           03  WK-HTTPS1       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS2       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS3       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS4       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS5       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS6       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS7       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS8       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS8B      PIC  X(100) VALUE SPACE.
           03  WK-VIDEO        PIC  X(005) VALUE SPACE.

      *    *** "jm" 
      *    *** "pt" 小さい
      *    *** "jp" 
      *    *** "ps" 少し大きい

           03  WK-IMG06.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG061     PIC  X(006)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG062     PIC  X(006)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG07.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG071     PIC  X(007)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG072     PIC  X(007)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG08.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG081     PIC  X(008)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG082     PIC  X(008)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG09.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG091     PIC  X(009)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG092     PIC  X(009)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG10.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG101     PIC  X(010)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG102     PIC  X(010)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG11.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG111     PIC  X(011)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG112     PIC  X(011)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG12.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG121     PIC  X(012)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG122     PIC  X(012)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG13.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG131     PIC  X(013)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG132     PIC  X(013)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG14.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG141     PIC  X(014)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG142     PIC  X(014)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG15.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG151     PIC  X(015)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG152     PIC  X(015)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG16.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG161     PIC  X(016)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG162     PIC  X(016)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG17.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG171     PIC  X(017)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG172     PIC  X(017)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG18.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".
             05  WK-IMG181     PIC  X(018)
             05                PIC  X(001) VALUE "/".
             05  WK-IMG182     PIC  X(018)
             05                PIC  X(006) VALUE "ps.jpg".

           03  WK-IMG-AM.
             05                PIC  X(039) VALUE
                 "https://pics.dmm.co.jp/digital/amateur/".

           03  WK-IMG-VI.
             05                PIC  X(037) VALUE
                 "https://pics.dmm.co.jp/digital/video/".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT       PIC  9(003) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-TITLE        PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-HIT          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 200.
             05  TBL01-CID     PIC  X(030) VALUE HIGH-VALUE.
             05  TBL01-CIDA    PIC  X(030) VALUE SPACE.
             05  TBL01-CIDA-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-VIDEO   PIC  X(005) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN,READ PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** WRITE POT1 %ヘッダー出力,1件目
           PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL01 SET
                   PERFORM S032-10     THRU    S032-EX

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** １件目ジャパリ無し
           IF      PIN1-REC (1:12) NOT =
      *    *** ジャパリ
                   X"E382B8E383A3E38391E383AA"
      *    *** WRITE POT1 ジャパリＤＭＭ 出力
                   PERFORM S130-10     THRU    S130-EX
           END-IF

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      WK-PIN1-LEN =       ZERO
                           CONTINUE
                   ELSE
                       IF      PIN1-REC (1:12) =
      *    *** ジャパリ
                               X"E382B8E383A3E38391E383AA"
                               WRITE   POT1-REC    FROM    PIN1-REC
                               ADD     1           TO      WK-POT1-CNT
                       ELSE
      *    *** 画像サムネイル付加
                           PERFORM S100-10     THRU    S100-EX
                       END-IF
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
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

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   MOVE    1           TO      WK-ACCEPT
               WHEN 1
                   ACCEPT  WK-ACCEPT FROM ARGUMENT-VALUE

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " PRM1-F 読み込みレコードＮｏ 1個まで指定可"
                   DISPLAY WK-PGM-NAME 
                           " TEST116 1 <=例 PRM1のレコードＮｏを指定"
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

                   IF      WK-ACCEPT   =       WK-PRM1-CNT
                           MOVE    "Y"         TO      SW-HIT
                   END-IF
           END-PERFORM

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
                   MOVE    "TEST116."  TO      WK-PIN1-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (9:)
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
           ELSE
                   MOVE    "TEST116."  TO      WK-PIN1-F-NAME (1:8)
                   MOVE    PRM1-REC    TO      WK-PIN1-F-NAME (9:)
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WK-PRM1-LEN + 9:5)
           END-IF

           OPEN    INPUT       PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           MOVE    "UTF8"      TO      WFD-KANJI
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-HTTPS
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-HTTPS-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   IF      PIN1-REC (1:12) NOT =
      *    *** ジャパリ
                           X"E382B8E383A3E38391E383AA"
                       MOVE    ', '    TO   PIN1-REC (WK-PIN1-LEN + 1:1)
                   END-IF
      *    *** "XXX 1,000回",https...
      *    *** YYY 1,000回, ,https...
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-PIN1-LEN
                              OR PIN1-REC (I:6) = ",https"
                           IF      PIN1-REC (I:1) =    ","
                               MOVE    "."         TO     PIN1-REC (I:1)
                           END-IF
                   END-PERFORM

                   UNSTRING PIN1-REC
                           DELIMITED BY ',' 
                           INTO
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-HTTPS    COUNT WK-HTTPS-LEN
                   END-UNSTRING
           END-READ
           .
       S020-EX.
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
           IF      K1          >       200
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      TBL01-CID       (K1)
                                       TBL01-CIDA      (K1)
                                       TBL01-VIDEO     (K1)
           MOVE    ZERO        TO      TBL01-CIDA-LEN  (K1)

           UNSTRING PIN2-REC
                   DELIMITED BY ","
                   INTO
                   TBL01-CID  (K1)
                   TBL01-CIDA (K1) COUNT TBL01-CIDA-LEN (K1)
                   TBL01-VIDEO (K1)

           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** 画像サムネイル付加
       S100-10.

           MOVE    SPACE       TO      POT1-REC

      *    *** １バイト左へずらす
           IF      WK-TITLE (1:1) = '"'
                   INSPECT WK-TITLE
                           REPLACING ALL '"' BY SPACE
                   MOVE    WK-TITLE (2:) TO  WK-TITLE
                   ADD     -2        TO      WK-TITLE-LEN
           END-IF

           MOVE    WK-TITLE-LEN TO     WK-TITLE2-LEN
           MOVE    WK-TITLE    TO      WK-TITLE2
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      J2
           MOVE    ZERO        TO      J3
           MOVE    "N"         TO      SW-TITLE

           PERFORM VARYING J FROM WK-TITLE2-LEN BY -1
                   UNTIL J < 1
                       OR SW-TITLE = "Y"
                   IF WK-TITLE2 (J:12) =
      *    *** 最後の位置から、再生回数 チェック
                         X"E5868DE7949FE59B9EE695B0"
                           MOVE    "X"         TO      SW-TITLE
                   END-IF
                   IF      WK-TITLE2 (J:1) = SPACE
                       AND SW-TITLE    =       "X"
      *    *** J3= 最後から見て、再生回数の最初のスペース位置
                           MOVE    J           TO      J3
                           MOVE    "Y"         TO      SW-TITLE
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-TITLE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-TITLE2-LEN
                      OR SW-TITLE = "Y"

                   EVALUATE TRUE

                       WHEN WK-TITLE2 (J:12) =
      *    *** 再生回数 なら<br><br>挿入
                         X"E5868DE7949FE59B9EE695B0"
      *    *** J3=ZERO 女優名無いと思われる、その他コメント形式
                           IF      J3          =       ZERO
                               MOVE    WK-TITLE2 (1 :J - 1) TO 
                                       WK-TITLE  (J2:J - 1)
                               COMPUTE J2 = J2 + (J - 1)

                               MOVE    "<br><br>"  TO    WK-TITLE (J2:8)
                               ADD     8           TO      J2

      *    *** 再生回数 以降セット
                               MOVE WK-TITLE2 (J:WK-TITLE2-LEN - J + 1)
                                       TO WK-TITLE  
                                       (J2:WK-TITLE2-LEN - J + 1)
                               COMPUTE J2 = J2 + WK-TITLE2-LEN - J + 1

      *    *** J2 は次の項目セット位置なので、-1でWK-TITLE の長さ求める
                               COMPUTE WK-TITLE-LEN = J2 - 1
                           ELSE

      *    *** 1バイト目から女優名前までセット
                               MOVE    WK-TITLE2  (1:J3 - 1) TO 
                                       WK-TITLE  (J2:J3 - 1)
                               COMPUTE J2 = J2 + J3 - 1

                               MOVE    "<br><br>"  TO    WK-TITLE (J2:8)
                               ADD     8           TO      J2

      *    *** 女優名セッ
      *    *** XXX 女優名再生回数 563 回,https:...
      *    ***    ↑     ↑            ↑
      *    ***    J3     J            WK-TITLE2-LEN
                               MOVE    WK-TITLE2 (J3 + 1:J - J3 - 1) TO 
                                       WK-TITLE  (J2    :J - J3 - 1)
                               COMPUTE J2 = J2 + (J - J3 - 1)

                               MOVE    "<br><br>"  TO    WK-TITLE (J2:8)
                               ADD     8           TO      J2

      *    *** 再生回数以降セット
                               MOVE    WK-TITLE2 
                                         (J :WK-TITLE2-LEN - J + 1) TO 
                                       WK-TITLE  
                                         (J2:WK-TITLE2-LEN - J + 1)
                               COMPUTE J2 = J2 + WK-TITLE2-LEN - J + 1

                               COMPUTE WK-TITLE-LEN = J2 - 1
                           END-IF
                           MOVE    "Y"         TO      SW-TITLE
                   END-EVALUATE
           END-PERFORM

           MOVE    1           TO      J2
           MOVE    WK-TITLE  (1:WK-TITLE-LEN) TO
                   POT1-REC (J2:WK-TITLE-LEN)
           ADD     WK-TITLE-LEN TO     J2

           MOVE    " ,"        TO      POT1-REC (J2:2)
           ADD     2           TO      J2

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

      *     IF      SW-TITLE    =       "N"
      *             DISPLAY WK-PGM-NAME " タイトル　再生回数 無エラー"
      *             DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
      *             STOP    RUN
      *     END-IF

           MOVE    SPACE       TO      WK-HTTPS1
                                       WK-HTTPS2
                                       WK-HTTPS3
                                       WK-HTTPS4
                                       WK-HTTPS5
                                       WK-HTTPS6
                                       WK-HTTPS7
                                       WK-HTTPS8
           MOVE    ZERO        TO      WK-HTTPS1-LEN
                                       WK-HTTPS2-LEN
                                       WK-HTTPS3-LEN
                                       WK-HTTPS4-LEN
                                       WK-HTTPS5-LEN
                                       WK-HTTPS6-LEN
                                       WK-HTTPS7-LEN
                                       WK-HTTPS8-LEN

      *    *** WK-HTTPS サンプル
      *    *** https://www.dmm.co.jp/litevideo/-/detail/=/cid=sone00084/
           UNSTRING WK-HTTPS
                   DELIMITED BY '/' 
                   INTO
                   WK-HTTPS1 COUNT WK-HTTPS1-LEN
                   WK-HTTPS2 COUNT WK-HTTPS2-LEN
                   WK-HTTPS3 COUNT WK-HTTPS3-LEN
                   WK-HTTPS4 COUNT WK-HTTPS4-LEN
                   WK-HTTPS5 COUNT WK-HTTPS5-LEN
                   WK-HTTPS6 COUNT WK-HTTPS6-LEN
                   WK-HTTPS7 COUNT WK-HTTPS7-LEN
                   WK-HTTPS8 COUNT WK-HTTPS8-LEN
           END-UNSTRING

           MOVE    WK-HTTPS8   TO      WK-HTTPS8B

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX
                      OR SW-SET = "Y"
                   IF      TBL01-CID (K1) (1:WK-HTTPS8-LEN)
                         = WK-HTTPS8 (1:WK-HTTPS8-LEN)
                           MOVE    TBL01-CIDA (K1) TO  WK-HTTPS8
                           MOVE    TBL01-CIDA-LEN (K1) TO WK-HTTPS8-LEN
                           MOVE    TBL01-VIDEO (K1) TO WK-VIDEO
                           MOVE    "Y"         TO      SW-SET
                           DISPLAY WK-PIN1-CNT 
                                   " B=" WK-HTTPS8B (1:30)
                                   " A=" WK-HTTPS8  (1:30)
                                   " WK-HTTPS8-LEN=" WK-HTTPS8-LEN
                                   " HTTPS=" WK-HTTPS (1:100)
                   END-IF
           END-PERFORM

           IF      SW-SET      =       "Y"
                   IF      WK-HTTPS8 (WK-HTTPS8-LEN - 2:3)= "jpg"
      *    *** XXXXjp.jpg,XXXXpl.jpg
                           PERFORM S120-10     THRU    S120-EX
                           GO  TO  S100-EX
                   END-IF
           END-IF

           IF      WK-HTTPS8 (WK-HTTPS8-LEN:1) IS NUMERIC
                   IF      WK-HTTPS8 (WK-HTTPS8-LEN - 3:1) IS NUMERIC
                       IF     WK-HTTPS8 (WK-HTTPS8-LEN - 4:1) IS NUMERIC
      *    *** 最後の文字が数字で
      *    *** 最後の文字から-３も-4も数字はそのまま
      *    *** +0000000002 B=cid=juq00115       A=cid=juq00115
                           CONTINUE
                       ELSE
      *    *** 最後の文字が数字で
      *    *** 最後の文字から-３が数字、-4が数字以外
      *    *** +0000000018 B=cid=h_452tmhp0077   A=cid=h_452tmhp00077
                           MOVE    WK-HTTPS8 (WK-HTTPS8-LEN - 3:4) TO
                                   WK-HTTPS8 (WK-HTTPS8-LEN - 2:4)
                           MOVE    "0"         TO
                                   WK-HTTPS8 (WK-HTTPS8-LEN - 3:1)
                           ADD     1           TO      WK-HTTPS8-LEN
                       END-IF
               ELSE
      *    *** 最後の文字が数字で
      *    *** 最後の文字から-３が数字以外、Vなので00追加
      *    *** +0000000001 B=cid=49cadv650      A=cid=49cadv00650
                       MOVE    WK-HTTPS8 (WK-HTTPS8-LEN - 2:3) TO
                               WK-HTTPS8 (WK-HTTPS8-LEN:3)
                       MOVE    "00"        TO
                               WK-HTTPS8 (WK-HTTPS8-LEN - 2:2)
                       ADD     2           TO      WK-HTTPS8-LEN
               END-IF
           ELSE
      *    *** 最後の文字数字以外
      *    *** 最後の文字から－４の文字が数字の時
               IF      WK-HTTPS8 (WK-HTTPS8-LEN - 4:1) IS NUMERIC
                       CONTINUE
               ELSE
      *    *** 最後の文字から－４の文字が数字以外の時、00追加
      *    *** +0000000046 B=cid=504ibw617z     A=cid=504ibw00617z
                       MOVE    WK-HTTPS8 (WK-HTTPS8-LEN - 3:4) TO
                               WK-HTTPS8 (WK-HTTPS8-LEN - 1:4)
                       MOVE    "00"        TO
                               WK-HTTPS8 (WK-HTTPS8-LEN - 3:2)
                       ADD     2           TO      WK-HTTPS8-LEN
               END-IF
           END-IF

      *     IF      WK-HTTPS8B  (1:20) NOT = WK-HTTPS8  (1:20)
      *             DISPLAY WK-PIN1-CNT
      *                   " B=" WK-HTTPS8B (1:20) " A=" WK-HTTPS8 (1:20)
      *     END-IF

           IF      WK-HTTPS8 (1:4) =   "cid="
                   EVALUATE TRUE
                       WHEN WK-HTTPS8-LEN = 10
                           MOVE    WK-HTTPS8 (5:06) TO  WK-IMG061
                                                        WK-IMG062
                       WHEN WK-HTTPS8-LEN = 11
                           MOVE    WK-HTTPS8 (5:07) TO  WK-IMG071
                                                        WK-IMG072
                       WHEN WK-HTTPS8-LEN = 12
                           MOVE    WK-HTTPS8 (5:08) TO  WK-IMG081
                                                        WK-IMG082
                       WHEN WK-HTTPS8-LEN = 13
                           MOVE    WK-HTTPS8 (5:09) TO  WK-IMG091
                                                        WK-IMG092
                       WHEN WK-HTTPS8-LEN = 14
                           MOVE    WK-HTTPS8 (5:10) TO  WK-IMG101
                                                        WK-IMG102
                       WHEN WK-HTTPS8-LEN = 15
                           MOVE    WK-HTTPS8 (5:11) TO  WK-IMG111
                                                        WK-IMG112
                       WHEN WK-HTTPS8-LEN = 16
                           MOVE    WK-HTTPS8 (5:12) TO  WK-IMG121
                                                        WK-IMG122
                       WHEN WK-HTTPS8-LEN = 17
                           MOVE    WK-HTTPS8 (5:13) TO  WK-IMG131
                                                        WK-IMG132
                       WHEN WK-HTTPS8-LEN = 18
                           MOVE    WK-HTTPS8 (5:14) TO  WK-IMG141
                                                        WK-IMG142
                       WHEN WK-HTTPS8-LEN = 19
                           MOVE    WK-HTTPS8 (5:15) TO  WK-IMG151
                                                        WK-IMG152
                       WHEN WK-HTTPS8-LEN = 20
                           MOVE    WK-HTTPS8 (5:16) TO  WK-IMG161
                                                        WK-IMG162
                       WHEN WK-HTTPS8-LEN = 21
                           MOVE    WK-HTTPS8 (5:17) TO  WK-IMG171
                                                        WK-IMG172
                       WHEN OTHER
                           MOVE    WK-HTTPS8 (5:18) TO  WK-IMG181
                                                        WK-IMG182
                   END-EVALUATE
           ELSE
                   DISPLAY WK-PGM-NAME " ＨＴＴＰＳ　ＣＩＤ 無エラー"
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-IF

           MOVE    WK-HTTPS    TO      POT1-REC (J2:WK-HTTPS-LEN)
           ADD     WK-HTTPS-LEN TO     J2

           MOVE    " ,"        TO      POT1-REC (J2:2)
           ADD     2           TO      J2

           EVALUATE TRUE

               WHEN WK-HTTPS8-LEN = 10
                   MOVE    WK-IMG06    TO      POT1-REC (J2:56)
                   ADD     56          TO      J2

               WHEN WK-HTTPS8-LEN = 11
                   MOVE    WK-IMG07    TO      POT1-REC (J2:58)
                   ADD     58          TO      J2

               WHEN WK-HTTPS8-LEN = 12
                   MOVE    WK-IMG08    TO      POT1-REC (J2:60)
                   ADD     60          TO      J2

               WHEN WK-HTTPS8-LEN = 13
                   MOVE    WK-IMG09    TO      POT1-REC (J2:62)
                   ADD     62          TO      J2

               WHEN WK-HTTPS8-LEN = 14
                   MOVE    WK-IMG10    TO      POT1-REC (J2:64)
                   ADD     64          TO      J2

               WHEN WK-HTTPS8-LEN = 15

      *    *** 姫川ゆうな 8時間 SPECIAL COLLECTION
      *                 WHEN WK-HTTPS8 (1:15) =  "cid=49cadv00650"
      *                     MOVE    "cadv00650" TO      WK-IMG091
      *                                                 WK-IMG092
      *                     MOVE    WK-IMG09    TO      POT1-REC (J2:62)
      *                     ADD     62          TO      J2
                   MOVE    WK-IMG11    TO      POT1-REC (J2:66)
                   ADD     66          TO      J2

               WHEN WK-HTTPS8-LEN = 16
                   MOVE    WK-IMG12    TO      POT1-REC (J2:68)
                   ADD     68          TO      J2

               WHEN WK-HTTPS8-LEN = 17
                   MOVE    WK-IMG13    TO      POT1-REC (J2:70)
                   ADD     70          TO      J2

               WHEN WK-HTTPS8-LEN = 18
                   MOVE    WK-IMG14    TO      POT1-REC (J2:72)
                   ADD     72          TO      J2

               WHEN WK-HTTPS8-LEN = 19
                   MOVE    WK-IMG15    TO      POT1-REC (J2:74)
                   ADD     74          TO      J2

               WHEN WK-HTTPS8-LEN = 20
                   MOVE    WK-IMG16    TO      POT1-REC (J2:76)
                   ADD     76          TO      J2

               WHEN WK-HTTPS8-LEN = 21
                   MOVE    WK-IMG17    TO      POT1-REC (J2:78)
                   ADD     78          TO      J2

               WHEN OTHER
                   MOVE    WK-IMG18    TO      POT1-REC (J2:80)
                   ADD     80          TO      J2
           END-EVALUATE

           MOVE    " ,"        TO      POT1-REC (J2:2)
           ADD     2           TO      J2

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 %ヘッダー出力
       S110-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    "% "        TO      POT1-REC (1:2)
           MOVE    PRM1-REC    TO      POT1-REC (3:WK-PRM1-LEN)
           MOVE    ","         TO      POT1-REC (WK-PRM1-LEN + 3:1)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

      *    *** XXXXjp.jpg,XXXXpl.jpg
       S120-10.

           MOVE    WK-HTTPS    TO      POT1-REC (J2:WK-HTTPS-LEN)
           ADD     WK-HTTPS-LEN TO     J2

           MOVE    " ,"        TO      POT1-REC (J2:2)
           ADD     2           TO      J2

           IF      WK-HTTPS8 (WK-HTTPS8-LEN - 2:6)= "jp.jpg"
                   MOVE    WK-IMG-AM   TO      POT1-REC (J2:39)
                   ADD     39          TO      J2
           ELSE
                   IF      WK-VIDEO    =       "video"
      *    *** XXXXpl.jpg video
                           MOVE    WK-IMG-VI   TO      POT1-REC (J2:37)
                           ADD     37          TO      J2
                   ELSE
      *    *** XXXXpl.jpg amateur
                           MOVE    WK-IMG-AM   TO      POT1-REC (J2:39)
                           ADD     39          TO      J2
                   END-IF
           END-IF

           IF      WK-HTTPS8 (1:4) =   "cid="
                   EVALUATE TRUE
      *    *** cid=pow033,cid=pow033jp.jpg,  + 6 = jp.jpg
      *    *** jp.jpg を除く

                       WHEN WK-HTTPS8-LEN = 08 + 6
                           MOVE    WK-HTTPS8 (5:06) TO POT1-REC (J2:4)
                           ADD     04          TO      J2

                       WHEN WK-HTTPS8-LEN = 09 + 6
                           MOVE    WK-HTTPS8 (5:06) TO POT1-REC (J2:5)
                           ADD     05          TO      J2

                       WHEN WK-HTTPS8-LEN = 10 + 6
                           MOVE    WK-HTTPS8 (5:06) TO POT1-REC (J2:6)
                           ADD     06          TO      J2

                       WHEN WK-HTTPS8-LEN = 11 + 6
                           MOVE    WK-HTTPS8 (5:07) TO POT1-REC (J2:7)
                           ADD     07          TO      J2

                       WHEN WK-HTTPS8-LEN = 12 + 6
                           MOVE    WK-HTTPS8 (5:08) TO POT1-REC (J2:8)
                           ADD     08          TO      J2

                       WHEN WK-HTTPS8-LEN = 13 + 6
                           MOVE    WK-HTTPS8 (5:09) TO POT1-REC (J2:9)
                           ADD     09          TO      J2

                       WHEN WK-HTTPS8-LEN = 14 + 6
                           MOVE    WK-HTTPS8 (5:10) TO POT1-REC (J2:10)
                           ADD     10          TO      J2

                       WHEN WK-HTTPS8-LEN = 15 + 6
                           MOVE    WK-HTTPS8 (5:11) TO POT1-REC (J2:11)
                           ADD     11          TO      J2

                       WHEN WK-HTTPS8-LEN = 16 + 6
                           MOVE    WK-HTTPS8 (5:12) TO POT1-REC (J2:12)
                           ADD     12          TO      J2

                       WHEN WK-HTTPS8-LEN = 17 + 6
                           MOVE    WK-HTTPS8 (5:13) TO POT1-REC (J2:13)
                           ADD     13          TO      J2

                       WHEN WK-HTTPS8-LEN = 18 + 6
                           MOVE    WK-HTTPS8 (5:14) TO POT1-REC (J2:14)
                           ADD     14          TO      J2

                       WHEN WK-HTTPS8-LEN = 19 + 6
                           MOVE    WK-HTTPS8 (5:15) TO POT1-REC (J2:15)
                           ADD     15          TO      J2

                       WHEN WK-HTTPS8-LEN = 20 + 6
                           MOVE    WK-HTTPS8 (5:16) TO POT1-REC (J2:16)
                           ADD     16          TO      J2

                       WHEN WK-HTTPS8-LEN = 21 + 6
                           MOVE    WK-HTTPS8 (5:17) TO POT1-REC (J2:17)
                           ADD     17          TO      J2

                       WHEN OTHER
                           MOVE    WK-HTTPS8 (5:18) TO POT1-REC (J2:18)
                           ADD     18          TO      J2
                   END-EVALUATE
           ELSE
                   DISPLAY WK-PGM-NAME " ＨＴＴＰＳ　ＣＩＤ 無エラー"
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-IF

           MOVE    "/"         TO      POT1-REC (J2:1)
           ADD     1           TO      J2

           MOVE    WK-HTTPS8 (5:WK-HTTPS8-LEN - 4) TO 
                   POT1-REC (J2:WK-HTTPS8-LEN - 4)
           ADD     WK-HTTPS8-LEN  -4   TO      J2

           MOVE    " ,"        TO      POT1-REC (J2:2)
           ADD     2           TO      J2

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1 ジャパリＤＭＭ 出力
       S130-10.

      *    *** ジャパリＤＭＭ
           MOVE    X"E382B8E383A3E38391E383AAEFBCA4EFBCADEFBCAD"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   PIN2-F
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
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
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
