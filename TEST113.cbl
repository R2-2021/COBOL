      *    *** Youtube channel PNG 未使用番号、再採番
      *    *** 
      *    *** TEST97U.POT2 => TEST97W.POT2
      *    ***     ↑              ↓
      *    ***     ＿＿＿＿＿＿＿＿＿
      *    *** コマンドプロンプトでdelとrenを実行する
      *    ***
      *    *** TEST113.PIN2 にTEST98で実行した、CMDイメージをコピーして
      *    *** 未使用IMAGE の番号部分をコピーして作成
      *    *** 
      *    *** 未使用部分のdelコマンドプロンプト自動作成
      *    *** TEST97U.POT2 のIMAGEの大きい方から、PIN2の番号で
      *    *** renコマンドプロンプト自動作成とren後の番号で書き換えした
      *    *** 番号でTEST97W.POT2 を再作成する

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST113.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 抽出前データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用 IMAGE 番号データ (TEST98 実行結果より)
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後マッチング分データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** コマンドプロンプト自動作成
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(016).
           03  PIN2-IMAGE      PIC  X(005).
           03                  PIC  X(005).
           03  PIN2-NO         PIC  9(004).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(080).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST113 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST97U.POT2".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST113.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST97W.POT2".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST113.POT2".

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

           03  WK-CHANNEL      PIC  X(500) VALUE SPACE.
           03  WK-CHANNEL-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS        PIC  X(100) VALUE SPACE.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PNG          PIC  X(100) VALUE ZERO.
           03  WK-PNG-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** ./Youtubechannel.files/image1305.png
           03  WK-PNG2.
             05                PIC  X(028) VALUE SPACE.
             05  WK-PNG2-NO     PIC  9(004) VALUE ZERO.
             05                PIC  X(004) VALUE SPACE.

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
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-END          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL01-NO-OLD
                               INDEXED BY TBL01-IDX.
             05  TBL01-NO-OLD  PIC  9(004) VALUE ZERO.
             05  TBL01-NO-NEW  PIC  9(004) VALUE ZERO.

           03  TBL02-AREA      OCCURS 2000.
             05  TBL02-NO-NEW  PIC  9(004) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   EVALUATE TRUE
                       WHEN PIN1-REC (1:1) = "%"
                           CONTINUE
                       WHEN WK-PIN1-LEN = ZERO
                           CONTINUE
                       WHEN PIN1-REC (1:12) = WK-JYAPARI
                           CONTINUE
                       WHEN OTHER
      *    *** TBL01 SET
                           PERFORM S022-10     THRU    S022-EX
                   END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** TBL01 SORT
      *    *** 昇順に並べるので、
      *    *** SORT 後、(2000)に最大の値が入る、(1)は最小 
      *    *** 内容は該当ない時ZERO
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-NO-OLD

      *    *** CLOSE,OPEN PIN1
           PERFORM S012-10     THRU    S012-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** TBL02 SET
                   PERFORM S032-10     THRU    S032-EX

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** NO CHECK
           PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   EVALUATE TRUE
                       WHEN PIN1-REC (1:1) = "%"
                         OR WK-PIN1-LEN = ZERO
                         OR PIN1-REC (1:12) = WK-JYAPARI
                           MOVE    PIN1-REC    TO      POT1-REC
      *    *** WRITE POT1-REC 
                           PERFORM S110-10     THRU    S110-EX
                       WHEN OTHER
      *    *** WRITE POT1-REC PNG-NO変更
                           PERFORM S130-10     THRU    S130-EX
                   END-EVALUATE

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

      *    *** CLOSE,OPEN PIN1
       S012-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           MOVE    LOW-VALUE   TO      WK-PIN1-EOF
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-CHANNEL
                                       WK-HTTPS
                                       WK-PNG
           MOVE    ZERO        TO      WK-CHANNEL-LEN
                                       WK-HTTPS-LEN
                                       WK-PNG-LEN

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
                           WK-PNG      COUNT WK-PNG-LEN
                   MOVE    WK-PNG      TO      WK-PNG2
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

           MOVE    WK-PNG2-NO  TO      TBL01-NO-OLD (I)

           MOVE    I           TO      I-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** TBL02 SET
       S032-10.

           IF      PIN2-IMAGE  =       "image"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F DATA ERROR image ? "
                           "PIN2-REC=" PIN2-REC
                   STOP    RUN
           END-IF

           ADD     1           TO      J
           IF      J           >       2000
                   DISPLAY WK-PGM-NAME " TBL02 OVER J=" J
                   STOP    RUN
           END-IF

           MOVE    PIN2-NO     TO      TBL02-NO-NEW (J)

           MOVE    J           TO      J-MAX

      *    *** del
           MOVE    SPACE       TO      POT2-REC
           MOVE    "del "      TO      POT2-REC (1:4)
           MOVE    "image"     TO      POT2-REC (5:5)
           MOVE    PIN2-NO     TO      POT2-REC (10:4)
           MOVE    ".png"      TO      POT2-REC (14:4)

           WRITE   POT2-REC

           IF      WK-POT2-STATUS =    ZERO
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           .
       S032-EX.
           EXIT.

      *    *** NO CHECK
       S100-10.

           MOVE    ZERO        TO      J
           MOVE    "N"         TO      SW-END

           PERFORM VARYING I2 FROM 2000 BY -1
                   UNTIL I2 < 1
                      OR SW-END = "Y"
                   ADD     1           TO      J
                   IF      J           >       J-MAX
                           MOVE    "Y"         TO      SW-END
                   ELSE
      *    *** WRITE POT2-REC ren
                           PERFORM S120-10     THRU    S120-EX
                           MOVE    TBL02-NO-NEW (J) TO TBL01-NO-NEW (I2)
                   END-IF
           END-PERFORM
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

      *    *** WRITE POT2-REC ren
       S120-10.

      *    *** ren
           MOVE    SPACE       TO      POT2-REC
           MOVE    "ren "      TO      POT2-REC (1:4)
           MOVE    "image"     TO      POT2-REC (5:5)
           MOVE    TBL01-NO-OLD (I2) TO POT2-REC (10:4)
           MOVE    ".png"      TO      POT2-REC (14:4)

           MOVE    "image"     TO      POT2-REC (19:5)
           MOVE    TBL02-NO-NEW (J) TO POT2-REC (24:4)
           MOVE    ".png"      TO      POT2-REC (28:4)

      *    *** 現在のNOより小さい時のみリネームコマンドプロンプト自動作成する
           IF      TBL01-NO-OLD (I2) > TBL02-NO-NEW (J)
                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1-REC PNG-NO変更
       S130-10.

           SEARCH ALL TBL01-AREA
               AT  END
                   CONTINUE
               WHEN TBL01-NO-OLD (TBL01-IDX) = WK-PNG2-NO

                   IF      TBL01-NO-NEW (TBL01-IDX) NOT = ZERO

      *    *** 現在のNOより小さい時のみリネームコマンドプロンプトと同じ
      *    *** ＮＯに置き換える
                       AND TBL01-NO-OLD (TBL01-IDX) > 
                           TBL01-NO-NEW (TBL01-IDX)

                           MOVE    TBL01-NO-NEW (TBL01-IDX)
                                               TO      WK-PNG2-NO
                           MOVE    WK-PNG2     TO      WK-PNG
                   END-IF
           END-SEARCH

           MOVE    WK-CHANNEL  TO      POT1-REC
           COMPUTE P = WK-CHANNEL-LEN + 1

           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           MOVE    WK-HTTPS-LEN TO     L
           MOVE    WK-HTTPS    TO      POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           MOVE    WK-PNG-LEN  TO      L
           MOVE    WK-PNG      TO      POT1-REC (P:L)
           ADD     L           TO      L

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S130-EX.
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
