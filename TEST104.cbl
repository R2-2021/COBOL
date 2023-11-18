      *    *** YouTube 動画サムネイル、自動付加
      *    *** TEST103 => TEST104 TBL から PIO1-F に変更
      *    *** (Walk East.csv) TEST103.PIN1 => TEST103.POT1

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST104.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** YouTube USER 指定 漢字の時，ＵＴＦ８で指定
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** Youtube動画 タイトル、動画urlデータ
      *    *** Youtube動画 CTRL+ A,CTRL+ C でＥｘｃｅｌに貼り付け、
      *    *** ＳＯＲＴ後、不要部分カット、Googleスプレッドに貼り付け、
      *    *** ダウンロード（ｈｔｍｌ）後、展開し、ＵＲＬ抽出後、
      *    *** ＵＲＬをＥｘｃｅｌの別の列に貼り付け、ＣＳＶ（ＵＴＦ８）
      *    *** で出力後、サクラエディターで出力モード（ＢＯＭ）をＯＦＦ
      *    *** で再出力する
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** グルーピング データ
      *    *** EX.
      *    *** 上海,上海,0,
      *    *** 上海,上海,0,C
      *    *** 陸家嘴,上海,1,
      *    *** ＸＸＸ，上海,2,
      *    *** 陸家嘴は上海に集約する、1は、１つ前の場所に集約を意味する
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 中間ファイル
       SELECT PIO1-F           ASSIGN   WK-PIO1-F-NAME
           ORGANIZATION INDEXED
           ACCESS MODE RANDOM
           RECORD KEY PIO1-KEY.

      *    *** サムネイルimg データ
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
           03                  PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  PIO1-F.
       01  PIO1-REC.
           03  PIO1-KEY        PIC  9(006).
           03  PIO1-NO-IDX     PIC  9(001).
           03  PIO1-NO         OCCURS 5
                               PIC  9(004).
           03  PIO1-DATA       PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST104 ".

      *    *** PRM1 でXXXXXXXX を１件目に指定、ＵＴＦ８も可能だが、
      *    *** ＳＪＩＳに変換可能な時のみ
           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST103.PRM1 ".
           03  WK-PIN1-F-NAME  PIC  X(064) VALUE 
               "TEST103.XXXXXXXX.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
      *         "TEST103.Walk East.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(064) VALUE 
               "TEST103.XXXXXXXX.PIN2".
           03  WK-PIO1-F-NAME  PIC  X(032) VALUE "TEST104.PIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST103.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1R-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1W-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1R-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1W-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-WATCH-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-T-LEN        BINARY-LONG SYNC VALUE ZERO.
           03  WK-WATCH        PIC  X(100) VALUE SPACE.
           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-REL          PIC  9(004) VALUE ZERO.
           03  WK-FILE-NAME    PIC  X(032) VALUE SPACE.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  K2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K3              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 500.
             05  TBL01-NO      PIC  9(004) VALUE ZERO.
             05  TBL01-ITEM    PIC  X(100) VALUE SPACE.
             05  TBL01-ITEM-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HEAD    PIC  X(100) VALUE SPACE.
             05  TBL01-HEAD-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN,READ PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL01 SET
                   PERFORM S032-10     THRU    S032-EX

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** WRITE POT1 %ヘッダー出力,1件目
           PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 画像サムネイル付加
                   IF      WK-PIN1-LEN =       ZERO
      *    *** ジャパリ
                        OR PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"

      *    *** ZERO バイト長、ジャパリは出力しない
                           CONTINUE
                   ELSE
      *    *** 画像サムネイル付加
                           PERFORM S100-10     THRU    S100-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX

                   IF      TBL01-CNT (K1) NOT = ZERO
                           MOVE    SPACE       TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
      *    *** PIN1-Fのジャパリは使わない、再作成する
      *    *** ジャパリ
                           MOVE    X"E382B8E383A3E38391E383AA"
                                               TO      POT1-REC
                           MOVE    TBL01-HEAD(K1) (1:TBL01-HEAD-LEN(K1))
                                               TO      POT1-REC 
                                                 (13:TBL01-HEAD-LEN(K1))
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           PERFORM VARYING K2 FROM 1 BY 1
                                   UNTIL K2 > K2-MAX

      *    *** PIO1-F => WRITE POT1 に変更
                                   PERFORM S120-10     THRU    S120-EX
                           END-PERFORM
                   END-IF
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

      *    *** PIO1-F ZERO件に初期化
           OPEN    OUTPUT      PIO1-F
           CLOSE   PIO1-F

           OPEN    INPUT       PRM1-F
           READ    PRM1-F
                   AT  END
                   DISPLAY WK-PGM-NAME " PRM1-F 0ｹﾝ YouTube USER 指定"
                   STOP    RUN
           END-READ
           ADD     1           TO      WK-PRM1-CNT

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
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-PIN2-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (9:)
                                               WK-PIN2-F-NAME (9:)
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
                   MOVE    ".PIN2"     TO      WK-PIN2-F-NAME
                                               (WDE05-BUF2-LEN + 9:5)
           ELSE
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-PIN2-F-NAME (1:8)
                   MOVE    PRM1-REC    TO      WK-PIN1-F-NAME (9:)
                                               WK-PIN2-F-NAME (9:)
                   MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
                                               (WK-PRM1-LEN + 9:5)
                   MOVE    ".PIN2"     TO      WK-PIN2-F-NAME
                                               (WK-PRM1-LEN + 9:5)
           END-IF

           OPEN    INPUT       PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F
                   I-O         PIO1-F

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
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
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
           IF      K1          >       500
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           UNSTRING PIN2-REC
                   DELIMITED BY ","
                   INTO
                   TBL01-ITEM (K1) COUNT TBL01-ITEM-LEN (K1)
                   TBL01-HEAD (K1) COUNT TBL01-HEAD-LEN (K1)
                   WK-REL

           IF      WK-REL      =       ZERO
                   MOVE    K1          TO      TBL01-NO (K1)
           ELSE
                   MOVE    K1          TO      TBL01-NO (K1)
                   COMPUTE TBL01-NO (K1) = TBL01-NO (K1) - WK-REL
           END-IF

           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** 画像サムネイル付加
       S100-10.

      *    *** PIO1-F 上限 999,999件までとする
           ADD     1           TO      K2
           IF      K2          >       999999
                   DISPLAY WK-PGM-NAME " PIO1-F OVER K2=" K2
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      WK-WATCH
           MOVE    ZERO        TO      WK-WATCH-LEN
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:32) =
                         "https://www.youtube.com/watch?v="

                   EVALUATE TRUE
                       WHEN PIN1-REC (J:7) =    ",https:"
                           CONTINUE
                       WHEN PIN1-REC (J:1) =    ","
                           MOVE    "."         TO      PIN1-REC (J:1)
                       WHEN PIN1-REC (J:4) =    '""",' 
                           MOVE    '"  ,'      TO      PIN1-REC (J:4)
                           ADD     3           TO      J
                       WHEN PIN1-REC (J:2) =    '",' 
                           MOVE    ' ,'        TO      PIN1-REC (J:2)
                           ADD     1           TO      J
                       WHEN PIN1-REC (J:3) =    ' ""' 
                           MOVE    '  "'       TO      PIN1-REC (J:3)
                           ADD     2           TO      J
                       WHEN PIN1-REC (J:3) =    '"" ' 
                           MOVE    '"  '       TO      PIN1-REC (J:3)
                           ADD     2           TO      J
                       WHEN PIN1-REC (J:1) =    '"' 
                        AND ( PIN1-REC (J + 1:1) >= X"E0" AND <= X"E9" )
                           MOVE    SPACE       TO      PIN1-REC (J:1)
                           ADD     1           TO      J
      *    *** 、=>．
                       WHEN PIN1-REC (J:3) =    X"E38081" 
                           MOVE    X"EFBC8E"   TO      PIN1-REC (J:3)
                           ADD     2           TO      J
                   END-EVALUATE

                   IF      J + 32      >       WK-PIN1-LEN
                           DISPLAY WK-PGM-NAME " www.youtube.com/watch"
                                   " 無エラー"
                           DISPLAY WK-PGM-NAME " WK-PIN1-CNT="
                                   WK-PIN1-CNT
                           STOP    RUN
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      WK-REC
                                       PIO1-REC
           MOVE    ZERO        TO      PIO1-NO-IDX
                                       PIO1-NO (1)
                                       PIO1-NO (2)
                                       PIO1-NO (3)
                                       PIO1-NO (4)
                                       PIO1-NO (5)
           MOVE    ZERO        TO      WK-T-LEN
           UNSTRING PIN1-REC
                   DELIMITED BY "&t=" OR "     " OR "&amp"
                   INTO
                   WK-REC     COUNT    WK-T-LEN

           MOVE    WK-T-LEN    TO      WK-PIN1-LEN

           MOVE    WK-PIN1-LEN TO      I
           ADD     1           TO      I
           MOVE    " ,"        TO      WK-REC (I:2)
           ADD     2           TO      I

           COMPUTE WK-WATCH-LEN = WK-PIN1-LEN - J - 31
           MOVE    PIN1-REC (J + 32:WK-WATCH-LEN)
                               TO      WK-WATCH

           MOVE    "https://i.ytimg.com/vi/" TO WK-REC (I:23)
           ADD     23          TO      I

           IF      WK-WATCH-LEN NOT =  ZERO
                   MOVE    WK-WATCH    TO      WK-REC (I:WK-WATCH-LEN)
                   ADD     WK-WATCH-LEN TO     I
                   MOVE    "/hqdefault.jpg" TO WK-REC (I:14)
                   ADD     14          TO      I
           END-IF

           MOVE    "N"         TO      SW-YES
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL PIN1-REC (K:6) = ",https"
      *                OR SW-YES = "Y"

                   PERFORM VARYING K1 FROM 1 BY 1
                           UNTIL K1 > K1-MAX

                       IF      TBL01-ITEM-LEN (K1) NOT = ZERO
                           AND PIN1-REC (K:TBL01-ITEM-LEN (K1)) =
                               TBL01-ITEM (K1) (1:TBL01-ITEM-LEN (K1))
                           AND PIO1-NO-IDX <= 4

                           EVALUATE TRUE
                             WHEN PIO1-NO-IDX = ZERO
                               MOVE    1           TO      PIO1-NO-IDX
                               MOVE    TBL01-NO (K1) TO    PIO1-NO (1)
                               MOVE    "Y"         TO      SW-YES
                               ADD     1           TO
                                       TBL01-CNT (TBL01-NO (K1))
                             WHEN PIO1-NO-IDX = 1
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    2           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (2)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 2
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    3           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (3)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 3
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    4           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (4)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                             WHEN PIO1-NO-IDX = 4
                               IF      PIO1-NO (1)     =   TBL01-NO (K1)
                                    OR PIO1-NO (2)     =   TBL01-NO (K1)
                                    OR PIO1-NO (3)     =   TBL01-NO (K1)
                                    OR PIO1-NO (4)     =   TBL01-NO (K1)
                                       CONTINUE
                               ELSE
                                   MOVE    5           TO    PIO1-NO-IDX
                                   MOVE    TBL01-NO (K1) TO  PIO1-NO (5)
                                   ADD     1           TO
                                           TBL01-CNT (TBL01-NO (K1))
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-PERFORM
           END-PERFORM

      *    *** テーブルヒットしない時、テーブルの最後、その他にする
           IF      SW-YES      =       "N"
                   MOVE    1           TO      PIO1-NO-IDX
                   MOVE    K1-MAX      TO      PIO1-NO (1)
                   ADD     1           TO      TBL01-CNT (K1-MAX)
           END-IF

      *    *** K2 は出力したＫＥＹにする
           MOVE    K2          TO      PIO1-KEY
           MOVE    WK-REC      TO      PIO1-DATA
           WRITE   PIO1-REC
                   INVALID KEY 
                   DISPLAY WK-PGM-NAME " PIO1-F WRITE ERROR KEY="
                           PIO1-KEY
                   STOP    RUN
           END-WRITE

           ADD     1           TO      WK-PIO1W-CNT
           MOVE    K2          TO      K2-MAX
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 %ヘッダー出力
       S110-10.

           MOVE    "% "        TO      POT1-REC (1:2)
           MOVE    PRM1-REC    TO      POT1-REC (3:)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

      *    *** PIO1-F => WRITE POT1 に変更
       S120-10.

           MOVE    K2          TO      PIO1-KEY
           READ    PIO1-F
                   INVALID KEY
                   DISPLAY WK-PGM-NAME " PIO1-F RAED ERROR KEY="
                           PIO1-KEY
                   STOP    RUN
           END-READ
           ADD     1           TO      WK-PIO1R-CNT

           PERFORM VARYING K3 FROM 1 BY 1
                   UNTIL K3 > PIO1-NO-IDX
                   IF      K1          =       PIO1-NO (K3)
                           MOVE    PIO1-DATA   TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   PIN2-F
                   PIO1-F
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
           MOVE    WK-PIO1R-CNT TO     WK-PIO1R-CNT-E
           DISPLAY WK-PGM-NAME " PIO1R 件数= " WK-PIO1R-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-PIO1W-CNT TO     WK-PIO1W-CNT-E
           DISPLAY WK-PGM-NAME " PIO1W 件数= " WK-PIO1W-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
