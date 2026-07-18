      *    *** YOUTUBE CHANNEL データ作成
      *    *** Youtubechannel2.csv はYoutubeのチャンネルをCTRL+A、
      *    *** CTRL+C でGoogleのスプレッドシートのA列に貼り付け、SORT後
      *    *** 不要部分削除して、CVS形式でダウンロードする
      *    *** 
      *    ***  TEST128
      *    ***     |
      *    ***  TEST98

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST128.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** Youtubechannel2.csv
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE CHANNELデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE CHANNEL トピックチャンネルデータ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST128 ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "Youtubechannel2.csv".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "Youtubechannel3.csv".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST128.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST128.POT2".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-REC          PIC  X(200) VALUE SPACE.
           03  WK-REC0         PIC  X(200) VALUE SPACE.
           03  WK-REC1         PIC  X(200) VALUE SPACE.
           03  WK-REC2         PIC  X(200) VALUE SPACE.
           03  WK-UNSTR-PTR    BINARY-LONG SYNC VALUE ZERO.
           03  WK-UNSTR2-PTR   BINARY-LONG SYNC VALUE ZERO.
           03  WK-UNSTR0-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-UNSTR1-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-UNSTR2-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-TOPIC-CNT    BINARY-LONG SYNC VALUE ZERO.
      *    *** トピックチャンネル登録者数
           03  WK-TOPIC1.
             05                PIC  X(020) VALUE
                 X"E38388E38394E38383E382AFE38381E383A3E383".
             05                PIC  X(020) VALUE
                 X"B3E3838DE383ABE799BBE98CB2E88085E695B0".
      *    *** ムービーチャンネル登録者数
           03  WK-TOPIC2.
             05                PIC  X(020) VALUE
                 X"E383A0E383BCE38393E383BCE38381E383A3E383".
             05                PIC  X(019) VALUE
                 X"B3E3838DE383ABE799BBE98CB2E88085E695B0".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-END          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
      *             PERFORM S100-10     THRU    S100-EX
      *             PERFORM S120-10     THRU    S120-EX

                   MOVE    ZERO        TO      WK-TOPIC-CNT
                   INSPECT PIN1-REC TALLYING

      *    *** トピックチャンネル登録者数
                           WK-TOPIC-CNT FOR ALL WK-TOPIC1

      *    *** ムービーチャンネル登録者数
                           WK-TOPIC-CNT FOR ALL WK-TOPIC2

                   IF      WK-TOPIC-CNT =      ZERO
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX
                   ELSE
                           WRITE   POT2-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT2-CNT
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F
                               POT2-F

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

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    "N"         TO      SW-END
           MOVE    ZERO        TO      I2

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL SW-END = "Y"
                      OR I > WK-PIN1-LEN

                   IF      PIN1-REC (I:1) =    "@"
      *    *** @以降
                           PERFORM S110-10     THRU    S110-EX
                           MOVE    "Y"         TO      SW-END
                   ELSE
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1)
                                               TO      POT1-REC (I2:1)
                   END-IF
           END-PERFORM

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** @以降
       S110-10.

           ADD     1           TO      I2
           MOVE
      *    *** チャンネル登録者数
           X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
                               TO      POT1-REC (I2:27)
           ADD     27          TO      I2

           MOVE    ",https://www.youtube.com/@"
                               TO      POT1-REC (I2:26)
           ADD     25          TO      I2

           ADD     1           TO      I
           PERFORM VARYING I3 FROM I BY 1
                   UNTIL PIN1-REC (I3:30) =
      *    *** ・チャンネル登録者数
         X"E280A2E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
                      OR I3 > WK-PIN1-LEN
                   ADD     1           TO      I2
                   MOVE    PIN1-REC (I3:1)
                                       TO      POT1-REC (I2:1)
           END-PERFORM

           ADD     1           TO      I2
           MOVE    " , , , "   TO      POT1-REC (I2:6)
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.

           MOVE    SPACE       TO      POT1-REC
                                       WK-REC
           MOVE    ZERO        TO      I2
           MOVE    1           TO      WK-UNSTR-PTR

      *    *** WK-UNSTR-PTR は @の次の位置がセットされる
      *    *** タイトル名取り出し
           UNSTRING PIN1-REC DELIMITED BY "@"
                   INTO WK-REC
                   WITH POINTER WK-UNSTR-PTR

           COMPUTE I3 = WK-UNSTR-PTR - 2
           MOVE    WK-REC (1:I3) TO     POT1-REC (1:I3)

           COMPUTE I2 = 1 + I3

           MOVE
      *    *** チャンネル登録者数
           X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
                               TO      POT1-REC (I2:27)
           ADD     27          TO      I2

           MOVE    ",https://www.youtube.com/@"
                               TO      POT1-REC (I2:26)
           ADD     26          TO      I2

           MOVE    SPACE       TO      WK-REC
           MOVE    WK-UNSTR-PTR TO     WK-UNSTR2-PTR

      *    *** ユーザーＩＤ取り出し
           UNSTRING PIN1-REC DELIMITED BY X"E280A2"
                   INTO WK-REC
                   WITH POINTER WK-UNSTR2-PTR

           COMPUTE I3 = ( WK-UNSTR2-PTR - 3 ) - WK-UNSTR-PTR 

           MOVE    WK-REC (1:I3) TO    POT1-REC (I2:I3)

           COMPUTE I2 = I2 + I3
           MOVE    " , , ,"    TO      POT1-REC (I2:6)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

      *    *** "佐々木 ゆう子 - トピックチャンネル@佐々木 ゆう子.登録者数 172人
      *    ***                                    *************** <=追加する
           MOVE    SPACE       TO      POT1-REC
                                       WK-REC0
                                       WK-REC1
                                       WK-REC2
           MOVE    ZERO        TO      I2
                                       WK-UNSTR0-CNT
                                       WK-UNSTR1-CNT
                                       WK-UNSTR2-CNT
           MOVE    1           TO      WK-UNSTR-PTR

      *    *** WK-UNSTR-PTR は @の次の位置がセットされる
      *    *** タイトル名取り出し
      *    *** X"E280A2":･チャンネル登録者数の最初の記号
           UNSTRING PIN1-REC DELIMITED BY "@" OR X"E280A2"
                   INTO WK-REC0 COUNT WK-UNSTR0-CNT
                        WK-REC2 COUNT WK-UNSTR2-CNT
                   WITH POINTER WK-UNSTR-PTR

           MOVE    ZERO        TO      J2
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-UNSTR0-CNT
                   IF      WK-REC0 (J:1) = '"'
                           CONTINUE
                   ELSE

                           IF      WK-REC0 (J:1) =     ","
                                   MOVE    "."         TO
                                           WK-REC0 (J:1)
                           END-IF

                           ADD     1           TO      J2
                           MOVE    WK-REC0 (J:1) TO    WK-REC1 (J2:1)
                   END-IF
           END-PERFORM
           MOVE    J2          TO      WK-UNSTR1-CNT

           MOVE    WK-REC1 (1:WK-UNSTR1-CNT) 
                               TO      POT1-REC (1:WK-UNSTR1-CNT)

           COMPUTE I2 = 1 + WK-UNSTR1-CNT

      *     MOVE
      *    *** チャンネル登録者数
      *     X"E38381E383A3E383B3E3838DE383ABE799BBE98CB2E88085E695B0"
      *                         TO      POT1-REC (I2:27)
      *     ADD     27          TO      I2

           MOVE    ",https://www.youtube.com/@"
                               TO      POT1-REC (I2:26)
           ADD     26          TO      I2

           MOVE    WK-REC2 (1:WK-UNSTR2-CNT)
                               TO      POT1-REC (I2:WK-UNSTR2-CNT)

           COMPUTE I2 = I2 + WK-UNSTR2-CNT
           MOVE    " , , , "   TO      POT1-REC (I2:6)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   POT1-F
                   POT2-F

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
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
