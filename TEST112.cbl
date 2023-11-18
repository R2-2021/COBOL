      *    *** Youtube channel 追加データチェック
      *    *** 
      *    *** TEST97U.POT2 => TEST97V.POT2
      *    ***     ↑              ↓
      *    ***     ＿＿＿＿＿＿＿＿＿
      *    ***     

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST112.

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

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST112 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "YoutubeChannel.csv".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST97U.POT2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST97V.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

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

           03  WK-CHANNEL      PIC  X(500) VALUE SPACE.
           03  WK-CHANNEL-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS        PIC  X(100) VALUE SPACE.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL2     PIC  X(500) VALUE SPACE.
           03  WK-CHANNEL2-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS2       PIC  X(100) VALUE SPACE.
           03  WK-HTTPS2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-PNG2         PIC  X(100) VALUE SPACE.
           03  WK-PNG2-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** チャンネル登録者数
           03  WK-TOUROKU-CH.
             05                PIC  X(013) VALUE
               X"E38381E383A3E383B3E3838DE3",
             05                PIC  X(014) VALUE
               X"83ABE799BBE98CB2E88085E695B0".

      *    *** ジャパリ
           03  WK-JYAPARI.
             05                PIC  X(012) VALUE
               X"E382B8E383A3E38391E383AA",

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
           03  SW-WRITE        PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 2000.
             05  TBL01-CHANNEL PIC  X(500) VALUE SPACE.
             05  TBL01-CHANNEL-LEN
                               BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HTTPS   PIC  X(100) VALUE SPACE.
             05  TBL01-HTTPS-LEN
                               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** TBL01 SET
                   PERFORM S022-10     THRU    S022-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

                   EVALUATE TRUE
                       WHEN PIN2-REC (1:1) = "%"
                        OR WK-PIN2-LEN = ZERO
                        OR PIN2-REC (1:12) = WK-JYAPARI
                           MOVE    PIN2-REC    TO      POT1-REC
      *    *** WRITE POT1-REC 
                           PERFORM S110-10     THRU    S110-EX
                       WHEN OTHER
      *    *** HTTPS CHECK
                           PERFORM S100-10     THRU    S100-EX
                   END-EVALUATE

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-CHANNEL
                                       WK-HTTPS
           MOVE    ZERO        TO      WK-CHANNEL-LEN
                                       WK-HTTPS-LEN

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC (1:WK-PIN1-LEN)
                           DELIMITED BY ","
                           INTO
                           WK-CHANNEL  COUNT WK-CHANNEL-LEN
                           WK-HTTPS    COUNT WK-HTTPS-LEN
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** TBL01 SET
       S022-10.

           ADD     1           TO      I
           IF      I           >       2000
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-CHANNEL  TO      TBL01-CHANNEL (I)
           MOVE    WK-CHANNEL-LEN TO   TBL01-CHANNEL-LEN (I)
           MOVE    WK-HTTPS    TO      TBL01-HTTPS (I)
           MOVE    WK-HTTPS-LEN TO     TBL01-HTTPS-LEN (I)

           MOVE    I           TO      I-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-CHANNEL2
                                       WK-HTTPS2
                                       WK-PNG2
           MOVE    ZERO        TO      WK-CHANNEL2-LEN
                                       WK-HTTPS2-LEN
                                       WK-PNG2-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC (1:WK-PIN2-LEN)
                           DELIMITED BY ","
                           INTO
                           WK-CHANNEL2 COUNT WK-CHANNEL2-LEN
                           WK-HTTPS2   COUNT WK-HTTPS2-LEN
                           WK-PNG2     COUNT WK-PNG2-LEN
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** HTTPS CHECK
       S100-10.

           MOVE    "N"         TO      SW-WRITE

           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > I-MAX
                      OR SW-WRITE = "Y"

                   UNSTRING TBL01-CHANNEL (I)
                           DELIMITED BY WK-TOUROKU-CH
                           INTO
                           WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   MOVE    WK-CHANNEL-LEN TO    K

      *    *** https://www.youtube.com/channel/@XXXXX
      *    *** 123456789012345678901234567890123
                   IF      WK-CHANNEL (1:K) =  PIN2-REC (1:K)
                       IF      TBL01-HTTPS (I) = WK-HTTPS2
                           MOVE    PIN2-REC   TO      POT1-REC
      *    *** WRITE POT1-REC 
                           PERFORM S110-10    THRU    S110-EX
                           MOVE    "Y"        TO      SW-WRITE
                       ELSE
                           MOVE    WK-CHANNEL2 TO     POT1-REC
                           COMPUTE P = WK-CHANNEL2-LEN + 1

                           MOVE    " ,"       TO      POT1-REC (P:2)
                           ADD     2          TO      P

                           MOVE    TBL01-HTTPS-LEN (I) TO L
                           MOVE    TBL01-HTTPS (I) (1:L) 
                                              TO      POT1-REC (P:L)
                           ADD     L          TO      P

                           MOVE    " ,"       TO      POT1-REC (P:2)
                           ADD     2          TO      P

                           MOVE    WK-PNG2-LEN TO     L
                           MOVE    WK-PNG2    TO      POT1-REC (P:L)
      *    *** WRITE POT1-REC 
                           PERFORM S110-10    THRU    S110-EX
                           MOVE    "Y"        TO      SW-WRITE
                       END-IF
                   ELSE
                       CONTINUE
                   END-IF
           END-PERFORM

           IF      SW-WRITE    =       "N"
                   MOVE    PIN2-REC    TO      POT1-REC
      *    *** WRITE POT1-REC 
                   PERFORM S110-10     THRU    S110-EX
           END-IF
           .
       S100-EX.
           EXIT.


      *    *** WRITE POT1-REC 
       S110-10.

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S110-EX.
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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
