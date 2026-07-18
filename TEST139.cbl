      *    *** みんなのAV.com 検索 検索結果 画像拡大表示 横６個
      *    *** 
      *    *** 私は叡智を極める修行中の身であるため普段から複数の
      *    *** 無料アダルトサイトで修行を重ねていますので、＜＝他からの流用
      *    *** アクセスしやすいように、htmlを作っています。
      *    *** このプログラムは、このような内容の為、
      *    *** １８禁（１８歳未満使用不可）です。
      *    *** 使用については、参照者のモラルに任せます。
      *    *** 
      *    *** 
      *    *** みんなのAV.com のAV女優リストで、ページのソースを表示
      *    *** CTRL+A CTRL+C でファイル minnano-av.XXXX.html を作成
      *    *** または、Pythonで該当ページのhtmlを取得してファイル作成する
      *    *** 
      *    *** TEST10  (PIN2 1件目にファイル名 minnano-av.XXXX.html を指定)
      *    ***   ↓
      *    *** TEST139
      *    ***   ↓
      *    *** TEST53 065
      *    ***   ↓
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST139.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST139 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST139.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-JYOYU-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DABURI-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-NEN-CNT      BINARY-LONG SYNC VALUE ZERO.
           03  WK-NEN-A        BINARY-LONG SYNC VALUE ZERO.
           03  WK-NEN-D        BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-JYOYU-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-DABURI-CNT-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** 女優情報サイト
           03  WK-HREF         PIC  X(100) VALUE SPACE.

      *    *** タイトル画像サイト
           03  WK-IMG          PIC  X(100) VALUE SPACE.

      *    *** 女優名称
           03  WK-ALT          PIC  X(100) VALUE SPACE.
           03  WK-ALT2         PIC  X(100) VALUE SPACE.

      *    *** ふりがな
           03  WK-FURI         PIC  X(100) VALUE SPACE.

      *    *** デビュー
           03  WK-DEBUT        PIC  X(100) VALUE SPACE.

           03  WK-HREF-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-ALT-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-FURI-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-DEBUT-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE.
      *    *** ジャパリ
             05                PIC  X(012)
                 VALUE X"E382B8E383A3E38391E383AA".
             05  WK-TITLE-FURI PIC  X(003) VALUE SPACE.

           03  WK-TITLE2.
      *    *** ジャパリ
             05                PIC  X(012)
                 VALUE X"E382B8E383A3E38391E383AA".
             05  WK-TITLE2-DEBUT PIC X(024) VALUE SPACE.

           03  WK-TITLE3.
      *    *** ジャパリ
             05                PIC  X(012)
                 VALUE X"E382B8E383A3E38391E383AA".
             05                PIC  X(010) VALUE "#Minnan-br".

           03  WK-TITLE4.
             05                PIC  X(012)
                 VALUE X"E382B8E383A3E38391E383AA".
      *    *** 女優数 
             05                PIC  X(10) VALUE X"E5A5B3E584AAE695B020".
             05  WK-TITLE4-CNT PIC  ZZZ,ZZ9 VALUE ZERO.
      *    ***  名
             05                PIC  X(004) VALUE X"20E5908D".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO.
           03  P5              BINARY-LONG SYNC VALUE ZERO.
           03  X1              BINARY-LONG SYNC VALUE ZERO.

      *    *** あいうえお順
       01  KEY-AREA.
           03  KEY-OLD.
             05  KEY-OFURI     PIC  X(003) VALUE LOW-VALUE.
           03  KEY-NEW.
             05  KEY-NFURI     PIC  X(003) VALUE LOW-VALUE.
      *    *** デビュー年順
           03  KEY2-OLD.
             05  KEY2-ODEBUT   PIC  X(007) VALUE LOW-VALUE.
           03  KEY2-NEW.
             05  KEY2-NDEBUT   PIC  X(007) VALUE LOW-VALUE.

       01  SW-AREA.
           03  SW-FURI         PIC  X(001) VALUE "N".
           03  SW-DEBUT        PIC  X(001) VALUE "N".
           03  SW-DABURI       PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 12000
                               ASCENDING  KEY IS TBL01-FURI
                               DESCENDING KEY IS TBL01-DEBUT1
                               INDEXED BY TBL01-IDX.
             05  TBL01-HREF    PIC  X(100) VALUE SPACE.
             05  TBL01-IMG     PIC  X(100) VALUE SPACE.
             05  TBL01-ALT     PIC  X(100) VALUE SPACE.
             05  TBL01-FURI    PIC  X(100) VALUE HIGH-VALUE.
             05  TBL01-DEBUT.
               07  TBL01-DEBUT1 PIC  X(004) VALUE LOW-VALUE.
               07  TBL01-DEBUT2 PIC  X(020) VALUE LOW-VALUE.

             05  TBL01-HREF-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-IMG-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ALT-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-FURI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DEBUT-LEN BINARY-LONG SYNC VALUE ZERO.

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
                           PERFORM S100-10     THRU    S100-EX
                           EXIT    PERFORM
                   END-IF
           END-PERFORM

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** TBL01 SET
                   PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           DISPLAY WK-PGM-NAME " K1-MAX=" K1-MAX

           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-FURI

      *    *** WRITE POT1 あいうえお順（昇順）
           PERFORM S120-10     THRU    S120-EX

           SORT    TBL01-AREA
                   DESCENDING KEY TBL01-DEBUT1
                   ASCENDING  KEY TBL01-FURI

      *    *** WRITE POT1 デビュー年（降順）、あいうえお順（昇順）
           PERFORM S130-10     THRU    S130-EX

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

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

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

           MOVE    "Minnano-av " TO    POT1-REC (P:11)
           ADD     11          TO      P

           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL PIN1-REC (I1:1) =  SPACE
                   MOVE    PIN1-REC (I1:1) TO  POT1-REC (P:1)
                   ADD     1           TO      P
           END-PERFORM

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** TBL01 SET
       S110-10.

           EVALUATE TRUE

               WHEN PIN1-REC(1:16) = '<a href="actress'
                AND PIN1-REC(17:1) NOT = "_"

                   MOVE    "https://www.minnano-av.com/"
                                       TO      WK-HREF
                   MOVE    27          TO      P2
                   PERFORM VARYING P1 FROM 10 BY 1
                           UNTIL PIN1-REC(P1:2) = '">'
                           ADD     1           TO      P2
                           IF      P2          >       100
                                   DISPLAY WK-PGM-NAME " WK-HREF OVER"
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC(P1:1) TO   WK-HREF (P2:1)
                   END-PERFORM
                   MOVE    P2          TO      WK-HREF-LEN

               WHEN PIN1-REC(1:19) = '<img src="p_actress'
                   MOVE    "https://www.minnano-av.com/"
                                       TO      WK-IMG
                   MOVE    27          TO      P3
                   PERFORM VARYING P1 FROM 11 BY 1
                           UNTIL PIN1-REC(P1:1) = '"'
                           ADD     1           TO      P3
                           IF      P3          >       100
                                   DISPLAY WK-PGM-NAME " WK-IMG OVER"
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC(P1:1) TO   WK-IMG (P3:1)
                   END-PERFORM
                   ADD     1           TO      P3
                   MOVE    "?newav"    TO      WK-IMG (P3:6)
                   COMPUTE WK-IMG-LEN = P3 + 5

      *    *** alt=" まで読み飛ばし
                   PERFORM VARYING X1 FROM P1 BY 1
                           UNTIL PIN1-REC(X1:5) = 'alt="'
                           CONTINUE
                   END-PERFORM

                   COMPUTE X1 = X1 + 5
                   MOVE    SPACE       TO      WK-ALT
                   MOVE    ZERO        TO      P4
                   PERFORM VARYING P1 FROM X1 BY 1
                           UNTIL PIN1-REC(P1:1) = '"'
                           ADD     1           TO      P4
                           IF      P4          >       100
                                   DISPLAY WK-PGM-NAME " WK-ALT OVER"
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC(P1:1) TO   WK-ALT (P4:1)
                   END-PERFORM
                   MOVE    P4          TO      WK-ALT-LEN

               WHEN PIN1-REC(1:16) = '<p class="furi">'
                   MOVE    "Y"         TO      SW-FURI

               WHEN SW-FURI = "Y"
                   MOVE    "N"         TO      SW-FURI
                   MOVE    SPACE       TO      WK-FURI
                   MOVE    ZERO        TO      P5
                   PERFORM VARYING P1 FROM 1 BY 1
                           UNTIL PIN1-REC(P1:3) = " / "
                           ADD     1           TO      P5
                           IF      P3          >       100
                                   DISPLAY WK-PGM-NAME " WK-FURI OVER"
                                   STOP    RUN
                           END-IF
                          MOVE    PIN1-REC(P1:1) TO   WK-FURI (P5:1)
                   END-PERFORM
                   MOVE    P5          TO      WK-FURI-LEN

                   ADD     1           TO      K1
                   IF      K1          >       12000
                           DISPLAY WK-PGM-NAME " TBL01 OVER K1= K1"
                           STOP    RUN
                   END-IF

                   MOVE    WK-HREF     TO      TBL01-HREF     (K1)
                   MOVE    WK-HREF-LEN TO      TBL01-HREF-LEN (K1)

                   MOVE    WK-IMG      TO      TBL01-IMG      (K1)
                   MOVE    WK-IMG-LEN  TO      TBL01-IMG-LEN  (K1)

                   MOVE    WK-ALT      TO      TBL01-ALT      (K1)
                   MOVE    WK-ALT-LEN  TO      TBL01-ALT-LEN  (K1)

                   MOVE    WK-FURI     TO      TBL01-FURI     (K1)
                   MOVE    WK-FURI-LEN TO      TBL01-FURI-LEN (K1)

                   MOVE    K1          TO      K1-MAX

               WHEN PIN1-REC(1:20) = '<p class="debut-info'
                   MOVE    "Y"         TO      SW-DEBUT

               WHEN SW-DEBUT = "Y"
                   MOVE    "N"         TO      SW-DEBUT
                   IF      PIN1-REC (1:21) =   
      *    *** （データなし）
                   X"EFBC88E38387E383BCE382BFE381AAE38197EFBC89"
                       MOVE    SPACE       TO      TBL01-DEBUT     (K1)
                       MOVE    1           TO      TBL01-DEBUT-LEN (K1)
                   ELSE
                       IF      WK-PIN1-LEN >       24
                               DISPLAY WK-PGM-NAME " WK-DEBUT OVER"
                               STOP    RUN
                       END-IF
                       MOVE    PIN1-REC    TO      TBL01-DEBUT     (K1)
                       MOVE    WK-PIN1-LEN TO      TBL01-DEBUT-LEN (K1)
                   END-IF

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1 あいうえお順（昇順）
       S120-10.

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX

                   MOVE    "N"         TO      SW-DABURI
                   IF      K1          >       1
                       ADD     K1 -1       GIVING  K2
                       IF      TBL01-ALT (K1)  =   TBL01-ALT (K2)
                           MOVE    "CHANGE"    TO      WDE05-ID
                           MOVE    WK-HENKAN   TO      WDE05-HENKAN
                           MOVE    WK-MODE     TO      WDE05-MODE
                           MOVE    100         TO      WDE05-BUF1-LEN
                           MOVE    K1          TO      WDE05-BUF1-CNT
      *    *** ファイル名 ＵＴＦ８＝＞ＳＪＩＳに変換
                           CALL    "DECODE05"  USING WDE05-DECODE05-AREA
                                                     TBL01-ALT (K1)
                                                     WK-ALT2
                           DISPLAY WK-PGM-NAME " ALT ダブリ１ " WK-ALT2
                           MOVE    "Y"         TO      SW-DABURI
                           ADD     1            TO     WK-DABURI-CNT
                       END-IF
                   END-IF

                   MOVE    TBL01-FURI (K1) TO  KEY-NFURI
                   IF      KEY-OFURI   NOT =   KEY-NFURI
                           MOVE    KEY-NFURI TO WK-TITLE-FURI
                           WRITE   POT1-REC    FROM    WK-TITLE
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
                   MOVE    KEY-NFURI   TO      KEY-OFURI

                   MOVE    TBL01-ALT      (K1) TO POT1-REC
                   ADD     TBL01-ALT-LEN  (K1) 1 GIVING P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-HREF-LEN (K1) TO L
                   MOVE    TBL01-HREF     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-HREF-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-IMG-LEN (K1) TO L
                   MOVE    TBL01-IMG     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-IMG-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-DEBUT-LEN (K1) TO L
                   MOVE    TBL01-DEBUT     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-DEBUT-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-FURI-LEN (K1) TO L
                   MOVE    TBL01-FURI     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-FURI-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   IF      SW-DABURI   =       "N"
                           WRITE   POT1-REC
                           ADD     1            TO     WK-POT1-CNT
                                                       WK-JYOYU-CNT
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1 デビュー年（降順）、あいうえお順（昇順）
       S130-10.

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX

                   MOVE    "N"         TO      SW-DABURI
                   IF      K1          >       1
                       ADD     K1 -1       GIVING  K2
                       IF      TBL01-ALT (K1)  =   TBL01-ALT (K2)
                           MOVE    "CHANGE"    TO      WDE05-ID
                           MOVE    WK-HENKAN   TO      WDE05-HENKAN
                           MOVE    WK-MODE     TO      WDE05-MODE
                           MOVE    100         TO      WDE05-BUF1-LEN
                           MOVE    K1          TO      WDE05-BUF1-CNT
      *    *** ファイル名 ＵＴＦ８＝＞ＳＪＩＳに変換
                           CALL    "DECODE05"  USING WDE05-DECODE05-AREA
                                                     TBL01-ALT (K1)
                                                     WK-ALT2
      *                     DISPLAY WK-PGM-NAME " ALT ダブリ２ " WK-ALT2
                           MOVE    "Y"         TO      SW-DABURI
      *    *** S120-10 カウントアップしているので、ここはコメントにする
      *                     ADD     1            TO     WK-DABURI-CNT
                       END-IF
                   END-IF

                   MOVE    TBL01-DEBUT (K1) TO KEY2-NDEBUT
                   IF      KEY2-ODEBUT NOT =   KEY2-NDEBUT
                       IF  TBL01-DEBUT (K1) = SPACE
      *    *** デビュー年月不詳
      *                     MOVE
      *               X"E38387E38393E383A5E383BCE5B9B4E69C88E4B88DE8A9B3" 
      *                                         TO      WK-TITLE2-DEBUT
      *    *** デビュー年月不詳は、ジャパリヘッダー出力しない
      *                     WRITE   POT1-REC    FROM    WK-TITLE2
      *                     ADD     1           TO      WK-POT1-CNT
                           CONTINUE
                       ELSE
                           ADD     1           TO      WK-NEN-CNT
                           DIVIDE  WK-NEN-CNT BY 10
                                   GIVING    WK-NEN-D
                                   REMAINDER WK-NEN-A
                           END-DIVIDE
                           IF      WK-NEN-A    =       1

      *    *** ブレイク出力
                               WRITE   POT1-REC    FROM    WK-TITLE3
                               ADD     1           TO      WK-POT1-CNT
                           END-IF

                           MOVE    KEY2-NDEBUT TO  WK-TITLE2-DEBUT
                           WRITE   POT1-REC    FROM    WK-TITLE2
                           ADD     1           TO      WK-POT1-CNT
                       END-IF
                   END-IF
                   MOVE    KEY2-NDEBUT TO      KEY2-ODEBUT

                   MOVE    TBL01-ALT      (K1) TO POT1-REC
                   ADD     TBL01-ALT-LEN  (K1) 1 GIVING P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-HREF-LEN (K1) TO L
                   MOVE    TBL01-HREF     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-HREF-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-IMG-LEN (K1) TO L
                   MOVE    TBL01-IMG     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-IMG-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-DEBUT-LEN (K1) TO L
                   MOVE    TBL01-DEBUT     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-DEBUT-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-FURI-LEN (K1) TO L
                   MOVE    TBL01-FURI     (K1) TO POT1-REC (P:L)
                   ADD     TBL01-FURI-LEN (K1) TO P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   IF      SW-DABURI   =       "N"
                       IF  KEY2-NDEBUT NOT =   SPACE
                           WRITE   POT1-REC
                           ADD     1            TO     WK-POT1-CNT
      *    *** S120-10 カウントアップしているので、ここはコメントにする
      *                                                 WK-JYOYU-CNT
                       END-IF
                   END-IF
           END-PERFORM

      *    *** 女優数出力
           MOVE    WK-JYOYU-CNT TO     WK-TITLE4-CNT
           WRITE   POT1-REC     FROM   WK-TITLE4
           ADD     1            TO     WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1  件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1  件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-JYOYU-CNT TO     WK-JYOYU-CNT-E
           DISPLAY WK-PGM-NAME " 女優  件数 = " WK-JYOYU-CNT-E
           MOVE    WK-DABURI-CNT TO    WK-DABURI-CNT-E
           DISPLAY WK-PGM-NAME " ダブリ件数 = " WK-DABURI-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
