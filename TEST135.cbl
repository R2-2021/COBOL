      *    *** DLNA用 ファイルＩＤ、タイトル、作成日、取り出し
      *    *** 
      *    *** sMedioでタイトル右上、クリック、右下のダウンロードクリック
      *    *** 設定、ダビングで指定したフォルダーにダウンロードする
      *    *** ダウンロード時間、30分で68秒、1時間４５分で３分50秒
      *    *** C:\TVConnectSuite => 
      *    *** L:\User\TVConnectSuite\201406 <=該当の所に移動
      *    *** L: はUGERRN NAS 上のネットワークドライブ
      *    *** smedio 見たいとき、TEST135.POT1 の情報から
      *    *** 
      *    *** L:\User\TVConnectSuite\YYYYMM => F:\TVConnectSuite\YYYYMM
      *    *** F:は外部ドライブ（1TB）、ここにコピーしても見れるので、
      *    *** 見る時はここにした C:は容量少ないため
      *    *** へ該当タイトルコピー（コピーで問題なし）後、SMEDIO の
      *    *** ダビングしたファイル番組を見る
      *    *** sMedio の設定、ダビング設定、ダビング先フォルダー変更でコピー
      *    *** したYYYYMM に変更して番組を見る
      *    *** 
      *    *** >L:
      *    *** >CD TVConnectSuite
      *    *** >CD 201406
      *    *** L:\TVConnectSuite>DIR *.info

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST135.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** XXX.info 情報　(DIR)
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XXX.info 情報読み取り
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** INFO 解析データ　ファイル名、タイトル、作成日
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(100).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(2000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST135 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST135.PIN1".
           03  WK-PIN2-F-NAME.
             05  WK-PIN2-F-NAME1
                               PIC  X(018) VALUE "L:\TVConnectSuite\".
             05  WK-PIN2-F-YYYYMM
                               PIC  X(006) VALUE SPACE.
             05                PIC  X(001) VALUE "\".
             05  WK-PIN2-F-NAME2
                               PIC  X(050) VALUE "XXX.info".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST135.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN2    BINARY-LONG SYNC VALUE ZERO.

      *    *** XXX.info
           03  WK-YYYYMM       PIC  X(006) VALUE SPACE.
           03  WK-TITLE        PIC  X(2000) VALUE SPACE.
           03  WK-DATE         PIC  X(010) VALUE SPACE.
           03  WK-DESC         PIC  X(2000) VALUE SPACE.
           03  WK-GENRE1       PIC  X(100) VALUE SPACE.
           03  WK-GENRE2       PIC  X(100) VALUE SPACE.
           03  WK-DURATION.
             05  WK-DURATION-HH PIC 9(002) VALUE ZERO.
             05                PIC  X(001) VALUE SPACE.
             05  WK-DURATION-MM PIC 9(002) VALUE ZERO.
             05                PIC  X(001) VALUE SPACE.
             05  WK-DURATION-SS PIC 9(002) VALUE ZERO.
           03  WK-SIZE.
             05  WK-SIZE-X     PIC  X(015) VALUE SPACE.
             05  WK-SIZE-9     REDEFINES WK-SIZE-X
                               PIC  9(015).
           03  WK-GB.
             05  WK-GB2        PIC  Z9.9   VALUE ZERO.
             05                PIC  X(002) VALUE "GB".
           03  WK-ACCEPT       PIC  X(001) VALUE SPACE.
           03  WK-PIN2-REC     PIC  X(10000) VALUE SPACE.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DESC-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-GENRE1-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-GENRE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIZE-LEN     BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE 1.
           03  P4              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

      *    *** b3148f00-df1d-11f0-8000-649300004a92.dtcp.info
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 2000.
             05  TBL01-FILE    PIC  X(046) VALUE SPACE.
             05  TBL01-YYYYMM  PIC  X(006) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   IF      PIN1-REC (01:4) IS NUMERIC
                       AND PIN1-REC (78:5) = ".info"
      *    *** TBL01 SET
                           PERFORM S022-10     THRU    S022-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL   K1 > K1-MAX

      *    *** OPEN PIN2
                   MOVE    TBL01-YYYYMM (K1) TO  WK-PIN2-F-YYYYMM
                   MOVE    TBL01-FILE (K1) TO  WK-PIN2-F-NAME2
                   PERFORM S011-10     THRU    S011-EX

      *    *** READ PIN2 
                   PERFORM S030-10     THRU    S030-EX

                   PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** READ PIN2 
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM

                   MOVE    WK-PIN2-LEN2        TO      WK-PIN2-LEN
                   MOVE    WK-PIN2-REC         TO      PIN2-REC

      *    *** XXX.info 解析
                   PERFORM S100-10     THRU    S100-EX

      *    *** CLOSE PIN1
                   PERFORM S012-10     THRU    S012-EX
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY WK-PGM-NAME " 1:L:\TVConnectSuite\"
                   DISPLAY WK-PGM-NAME " 2:L:\TVConnect_Z420\"

                   DISPLAY WK-PGM-NAME " 1 OR 2 入力"
                   ACCEPT  WK-ACCEPT
                   IF      WK-ACCEPT   =       "1" OR "2"
                           MOVE    "Y"         TO      SW-YES
                           EVALUATE TRUE
                               WHEN WK-ACCEPT = "1"
                                   MOVE    "TEST135_1.PIN1" TO
                                           WK-PIN1-F-NAME
                                   MOVE    "L:\TVConnectSuite\" TO
                                           WK-PIN2-F-NAME1
                               WHEN WK-ACCEPT = "2"
      *                             MOVE    "TEST135_2.PIN1" TO
                                   MOVE    "TEST135_2.PIN1" TO
                                           WK-PIN1-F-NAME
                                   MOVE    "L:\TVConnect_Z420\" TO
                                           WK-PIN2-F-NAME1
                           END-EVALUATE
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN PIN2
       S011-10.

           OPEN    INPUT       PIN2-F
           .
       S011-EX.
           EXIT.

      *    *** CLOSE PIN2
       S012-10.

           CLOSE   PIN2-F

           MOVE    LOW-VALUE   TO      WK-PIN2-EOF
           MOVE    SPACE       TO      WK-PIN2-REC
           MOVE    1           TO      P3
           MOVE    ZERO        TO      WK-PIN2-LEN2
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   IF      PIN1-REC (25:4) =   ">DIR" OR ">dir"
                           MOVE    PIN1-REC (19:6) TO  WK-YYYYMM
                   END-IF
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** TBL01 SET
       S022-10.

           ADD     1           TO      K1
           IF      K1          >       2000
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

      *    *** XXX.info FILE名セット
           MOVE    PIN1-REC (37:50) TO TBL01-FILE   (K1)
           MOVE    WK-YYYYMM   TO      TBL01-YYYYMM (K1)
           MOVE    K1          TO      K1-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT

                   MOVE    PIN2-REC    TO
                           WK-PIN2-REC (P3:WK-PIN2-LEN)
                   ADD     WK-PIN2-LEN TO      WK-PIN2-LEN2
                                               P3
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** XXX.info 解析
       S100-10.

           MOVE    SPACE       TO      POT1-REC
                                       WK-TITLE
                                       WK-DATE
                                       WK-DESC
                                       WK-GENRE1
                                       WK-GENRE2
                                       WK-SIZE
                                       WK-DURATION

           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-DESC-LEN
                                       WK-GENRE1-LEN
                                       WK-GENRE2-LEN
                                       WK-SIZE-LEN

           MOVE    1           TO      P
           MOVE    TBL01-YYYYMM (K1) TO POT1-REC (P:6)
           ADD     6           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

      *    *** ファイル名セット
           MOVE    41          TO      L
           MOVE    TBL01-FILE (K1) TO  POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN2-LEN

                   EVALUATE TRUE

      *    *** タイトル
                       WHEN PIN2-REC (I:10) = "<dc:title>"
                           COMPUTE I2 = I + 10
                           MOVE    1           TO      P2
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN2-REC (I:1) = "<"
                                      OR I > WK-PIN2-LEN

                                   IF      P2          >       2000
                                           DISPLAY WK-PGM-NAME
                                   " タイトル 退避エリアオーバー P2=" P2
                                           STOP    RUN
                                   END-IF
                                   MOVE    PIN2-REC (I:1) TO   
                                           WK-TITLE (P2:1)
                                   IF      WK-TITLE (P2:1) = ","
                                           MOVE "." TO WK-TITLE (P2:1)
                                   END-IF
                                   ADD     1          TO      P2
                           END-PERFORM
                           COMPUTE WK-TITLE-LEN = P2 - 1

      *    *** 録画日付
                       WHEN PIN2-REC (I:09) = "<dc:date>"
                           COMPUTE I2 = I + 9
                           MOVE    PIN2-REC (I2:10) TO WK-DATE
      *    *** Tの位置へ
                           ADD     10          TO      I

      *    *** 詳細情報
                       WHEN PIN2-REC (I:16) = "<dc:description>"
                           COMPUTE I2 = I + 16
                           MOVE    1          TO      P2
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN2-REC (I:1) = "<"
                                      OR I > WK-PIN2-LEN

                                   IF      P2          >       2000
                                           DISPLAY WK-PGM-NAME
                                   " 詳細情報 退避エリアオーバー P2=" P2
                                           STOP    RUN
                                   END-IF
                                   MOVE    PIN2-REC (I:1) TO
                                           WK-DESC (P2:1)
                                   IF      WK-DESC (P2:1) = ","
                                           MOVE "." TO WK-DESC (P2:1)
                                   END-IF
                                   ADD     1            TO      P2
                           END-PERFORM
                           COMPUTE WK-DESC-LEN = P2 - 1

      *    *** ジャンル
                       WHEN PIN2-REC (I:29) = 
                            "</upnp:channelNr><upnp:genre>"
                           COMPUTE I2 = I + 29
                           MOVE    1          TO      P2
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN2-REC (I:1) = "<"
                                      OR I > WK-PIN2-LEN

                                   IF      P2          >       100
                                           DISPLAY WK-PGM-NAME
                                 " ジャンル１ 退避エリアオーバー P2=" P2
                                           STOP    RUN
                                   END-IF
                                   MOVE    PIN2-REC (I:1) TO   
                                           WK-GENRE1 (P2:1)
                                   ADD     1           TO      P2
                           END-PERFORM
                           COMPUTE WK-GENRE1-LEN = P2 - 1
      *    *** UNTIL で"<" 判定しているので、次のサーチ一文字戻す
                           ADD     -1          TO      I

      *    *** ジャンル２
                       WHEN PIN2-REC (I:25) =
                            "</upnp:genre><upnp:genre>"
                           COMPUTE I2 = I + 25
                           MOVE    1          TO      P2
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN2-REC (I:1) = "<"
                                      OR I > WK-PIN2-LEN

                                   IF      P2          >       100
                                           DISPLAY WK-PGM-NAME
                                 " ジャンル２ 退避エリアオーバー P2=" P2
                                           STOP    RUN
                                   END-IF
                                   MOVE    PIN2-REC (I:1) TO
                                           WK-GENRE2 (P2:1)
                                   ADD     1           TO      P2
                           END-PERFORM
                           COMPUTE WK-GENRE2-LEN = P2 - 1

      *    *** 保存容量
                       WHEN PIN2-REC (I:15) = 'cleartextSize="'
                           COMPUTE I2 = I + 15
                           MOVE    1          TO      P2
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN2-REC (I:1) = '"'
                                      OR I > WK-PIN2-LEN

                                   IF      P2          >       15
                                           DISPLAY WK-PGM-NAME
                                  " 保存容量 退避エリアオーバー P2=" P2
                                           STOP    RUN
                                   END-IF
                                   MOVE    PIN2-REC (I:1) TO
                                           WK-SIZE (P2:1)
                                   ADD     1           TO      P2
                           END-PERFORM
                           COMPUTE WK-SIZE-LEN = P2 - 1

      *    *** 録画時間
                       WHEN PIN2-REC (I:10) = 'duration="'
                           COMPUTE I2 = I + 10
                           MOVE    PIN2-REC (I2:8) TO WK-DURATION
      *    *** "の次のスペース位置へ
                           ADD     23          TO      I

                   END-EVALUATE
           END-PERFORM

           MOVE    WK-DATE (1:10) TO   POT1-REC (P:10)
           ADD     10          TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           IF      WK-DURATION-SS > 30
                   MOVE    ZERO        TO      WK-DURATION-SS
                   ADD     1           TO      WK-DURATION-MM
                   IF      WK-DURATION-MM = 60
                           MOVE    ZERO        TO      WK-DURATION-MM
                           ADD     1           TO      WK-DURATION-HH
                   ELSE
                           CONTINUE
                   END-IF
           ELSE
                   CONTINUE
           END-IF

           MOVE    WK-DURATION TO      POT1-REC (P:5)
           ADD     5           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

      *     PERFORM VARYING J FROM 15 BY -1
      *             UNTIL J < 1
      *     END-PERFORM

           CALL    "C$JUSTIFY" USING WK-SIZE-X "R"
           INSPECT WK-SIZE-X REPLACING FIRST SPACE BY ZERO
           COMPUTE WK-GB2 = WK-SIZE-9 / 1024 / 1024 / 1024

           MOVE    WK-GB       TO      POT1-REC (P:6)
           ADD     6           TO      P

      *     MOVE    WK-SIZE-LEN TO      L
      *     MOVE    WK-SIZE (1:L) TO    POT1-REC (P:L)
      *     ADD     L           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           MOVE    WK-TITLE-LEN TO     L
           MOVE    WK-TITLE (1:L) TO   POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

      *    *** 情報、その他しか入ってないので、削除
      *     MOVE    WK-GENRE2-LEN TO    L
      *     MOVE    WK-GENRE2 (1:L) TO  POT1-REC (P:L)
      *     ADD     L           TO      P

      *     MOVE    ","         TO      POT1-REC (P:1)
      *     ADD     1           TO      P

           MOVE    WK-DESC-LEN TO      L
           MOVE    WK-DESC (1:L) TO    POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           MOVE    WK-GENRE1-LEN TO    L
           MOVE    WK-GENRE1 (1:L) TO  POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           WRITE   POT1-REC

           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
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
