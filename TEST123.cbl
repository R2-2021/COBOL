      *    *** YouTube チャンネル チェック
      *    *** TEST97U.POT2 追加・修正の為のデータ作成
      *    *** 
      *    *** TEST75 でマッチング、アンマッチ分振り分け、
      *    *** タイトル名変更の時、TEST97U.POT2を修正
      *    *** 
      *    *** TEST75
      *    *** 
      *    *** TEST53
      *    ***    |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST123.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    *** Youtube_チャンネルデータ
      *    *** Youtube チャンネルで全チャンネルを表示させ、ＣＴＲＬ+Ｃ
      *    *** して、ＥＸＣＥＬに貼りつけ、ＣＳＶで出力して、下記ファイルに
      *    *** 貼り付ける
      *    *** TEST123.YOUTUBE_CHANNEL.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE CHANNEL データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE CHANNEL データ2
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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST123 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST123.YOUTUBE_CHANNEL.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST123.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST123.POT2".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE        PIC  X(100) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1 15件目まで読み飛ばし
      *     PERFORM UNTIL WK-PIN1-CNT = 15
      *                OR WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** タイトルセット
                   MOVE    PIN1-REC    TO      WK-TITLE
                   MOVE    WK-PIN1-LEN TO      WK-TITLE-LEN

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

      *    *** チャンネルアドレス　セット
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM UNTIL
      *    *** 登録済み
                           PIN1-REC (1:12) = X"E799BBE98CB2E6B888E381BF"
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

                   MOVE    SPACE       TO      PIN1-REC
                   PERFORM UNTIL  WK-PIN1-EOF = HIGH-VALUE
                                OR PIN1-REC (1:1) NOT = SPACE
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM
           END-PERFORM

      *    *** CLOSE
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

      *    *** チャンネルアドレス　セット
       S100-10.

      *    *** 佐々木 ゆう子 - トピック PIN1 変更する
      *    *** @https://www.youtube.com/channel/UCGwvz5Iu8DIENsikN1tlarw?チャンネル登録者数 172人?60 本の動画

           IF      PIN1-REC (1:1) NOT = "@"
                   DISPLAY WK-PGM-NAME " @データ無し エラー"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY PIN1-REC (1:80)
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      POT1-REC

           MOVE    1           TO      J2
           MOVE    WK-TITLE    TO      POT1-REC (J2:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     J2
           MOVE    " ,"        TO      POT1-REC (J2:2)

           ADD     2           TO      J2
           MOVE    "https://www.youtube.com/"
                               TO      POT1-REC (J2:24)

           PERFORM TEST AFTER
                   VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
                      OR PIN1-REC (I:3) = X"E280A2"
                   IF      PIN1-REC (I:3) = X"E280A2"
                           COMPUTE WK-PIN1-LEN = I - 1
                   END-IF
           END-PERFORM

           ADD     24          TO      J2
      *     MOVE    PIN1-REC    TO      POT1-REC (J2:WK-PIN1-LEN - 3)
           MOVE    PIN1-REC    TO      POT1-REC (J2:WK-PIN1-LEN)

      *     ADD     WK-PIN1-LEN -3 TO   J2
           ADD     WK-PIN1-LEN TO      J2
           MOVE    " , , ,"    TO      POT1-REC (J2:6)

           ADD     6           TO      J2

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT



           MOVE    SPACE       TO      POT2-REC

           MOVE    1           TO      J2
           MOVE    WK-TITLE    TO      POT2-REC (J2:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     J2
           MOVE    " ,"        TO      POT2-REC (J2:2)

           ADD     2           TO      J2
           MOVE    "https://www.youtube.com/"
                               TO      POT2-REC (J2:24)

           ADD     24          TO      J2
      *     MOVE    PIN1-REC    TO      POT2-REC (J2:WK-PIN1-LEN - 3)
           MOVE    PIN1-REC    TO      POT2-REC (J2:WK-PIN1-LEN)

      *     ADD     WK-PIN1-LEN -3 TO   J2
           ADD     WK-PIN1-LEN TO      J2

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S100-EX.
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
