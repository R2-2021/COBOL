      *    *** Youtube channel TEST49.PIN3 
      *    *** GOOGLE CHROME 用：声優用データ作成
      *    *** 
      *    *** PIN2-F=Youtubechannel.csv の１件目の先頭にX"EFBBBF"が
      *    *** 入っている時、UTF8を判定する、コード（ＢＯＭ）なので、
      *    *** サクラエディタの設定、文字コードの設定ＢＯＭをＯＦＦにする
      *    *** 
      *    *** TEST97.POT1 => TEST49.PIN3
      *    *** TEST97.POT2 => TEST53.PIN1

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST97.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 抽出前データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーワードデータ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後マッチング分データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後アンマッチ分データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(500).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST97  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST96.POT2".
      *    *** 2022/03/19 時点。ＹoutubeＣhannel ALL
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "Youtubechannel_sort.txt".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE
               "Youtubechannel.csv".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST97.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST97.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PNG          PIC  9(004) VALUE ZERO.
           03  WK-SEQ          PIC  9(004) VALUE ZERO.
           03  WK-SEQ-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-NAME PIC  X(200) VALUE SPACE.
           03  WK-CHANNEL-NAME-LEN
                               BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-HTTP PIC  X(200) VALUE SPACE.
           03  WK-CHANNEL-HTTP-LEN
                               BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000.
             05  TBL01-SEQ     PIC  9(004) VALUE ZERO.
             05  TBL01-SEQ-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CHANNEL-NAME 
                               PIC  X(200) VALUE SPACE.
             05  TBL01-CHANNEL-NAME-LEN
                               BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CHANNEL-HTTP
                               PIC  X(200) VALUE SPACE.
             05  TBL01-CHANNEL-HTTP-LEN
                               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 TBL SET
                   PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1,POT2
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
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

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
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-CHANNEL-NAME
                                       WK-CHANNEL-HTTP
           MOVE    ZERO        TO      WK-SEQ
                                       WK-SEQ-LEN
                                       WK-CHANNEL-NAME-LEN
                                       WK-CHANNEL-HTTP-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   MOVE    ","         TO      PIN2-REC 
                                               (WK-PIN2-LEN + 1:1)
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-SEQ COUNT WK-SEQ-LEN
                           WK-CHANNEL-NAME COUNT WK-CHANNEL-NAME-LEN
                           WK-CHANNEL-HTTP COUNT WK-CHANNEL-HTTP-LEN
                   END-UNSTRING
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           ADD     1           TO      I
           IF      I           >       1000
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-SEQ      TO      TBL01-SEQ (I)
           MOVE    WK-SEQ-LEN  TO      TBL01-SEQ-LEN (I)
           MOVE    WK-CHANNEL-NAME     TO TBL01-CHANNEL-NAME     (I)
           MOVE    WK-CHANNEL-NAME-LEN TO TBL01-CHANNEL-NAME-LEN (I)
           MOVE    WK-CHANNEL-HTTP     TO TBL01-CHANNEL-HTTP     (I)
           MOVE    WK-CHANNEL-HTTP-LEN TO TBL01-CHANNEL-HTTP-LEN (I)
           MOVE    I           TO      I-MAX
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1,POT2
       S100-10.

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   MOVE    TBL01-CHANNEL-NAME-LEN (I) TO L
                   IF      TBL01-CHANNEL-NAME (I) (1:L)
                                       =       PIN1-REC (1:WK-PIN1-LEN)
                           MOVE    "Y"         TO      SW-SEARCH

      *    *** WRITE POT1
                           PERFORM S110-10     THRU     S110-EX

      *    *** WRITE POT2
                           PERFORM S120-10     THRU     S120-EX
                           MOVE    I-MAX       TO      I
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "N"
                   DISPLAY WK-PGM-NAME " UNMATCH WK-PIN1-CNT="
                           WK-PIN1-CNT
           END-IF

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           MOVE    1           TO      P
           MOVE    SPACE       TO      POT1-REC

           MOVE    TBL01-CHANNEL-HTTP-LEN (I) TO L
           MOVE    TBL01-CHANNEL-HTTP (I) (1:L)
                               TO      POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    " !"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           MOVE    TBL01-CHANNEL-NAME-LEN (I) TO L
           MOVE    TBL01-CHANNEL-NAME (I) (1:L)
                               TO      POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT2
       S120-10.

           MOVE    1           TO      P
           MOVE    SPACE       TO      POT2-REC

           MOVE    TBL01-CHANNEL-NAME-LEN (I) TO L
           MOVE    TBL01-CHANNEL-NAME (I) (1:L)
                               TO      POT2-REC (P:L)
           ADD     L           TO      P

           MOVE    " ,"        TO      POT2-REC (P:2)
           ADD     2           TO      P

           MOVE    TBL01-CHANNEL-HTTP-LEN (I) TO L
           MOVE    TBL01-CHANNEL-HTTP (I) (1:L)
                               TO      POT2-REC (P:L)
           ADD     L           TO      P

           MOVE    " ,"        TO      POT2-REC (P:2)
           ADD     2           TO      P

           MOVE    "./Youtubechannel.files/image"
                   TO      POT2-REC (P:30)
           ADD     28          TO      P

           MOVE    TBL01-SEQ (I) TO    WK-PNG
      *    *** imagexxxx.png xxxxは奇数で出力
           COMPUTE WK-PNG = WK-PNG * 2 - 1
           MOVE    WK-PNG (1:4) TO     POT2-REC (P:4)
           ADD     4           TO      P

           MOVE    ".png"      TO      POT2-REC (P:4)
           ADD     4           TO      P

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
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
