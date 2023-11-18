      *    *** マージサンプル ＰＩＮ１、ＰＩＮ２を１ファイルにする
      *    *** 同一キーは項目更新する
      *    *** PIN1,PIN2 同一キー有りは、PIN2の内容で更新し、PIN1出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST02.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** マスター
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** トランザクション
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** マージ後、マスター
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-KEY        PIC  X(003).
           03  PIN1-A          PIC  X(010).
           03  PIN1-B          PIC  X(010).
           03  PIN1-C          PIC  X(010).
           03  FILLER          PIC  X(047).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-KEY        PIC  X(003).
           03  PIN2-A          PIC  X(010).
           03  PIN2-B          PIC  X(010).
           03  PIN2-C          PIC  X(010).
           03  FILLER          PIC  X(047).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST02  ".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST02.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST02.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST02.POT1".

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

           03  WK-PIN1-OKEY    PIC  X(003) VALUE LOW-VALUE.
           03  WK-PIN1-NKEY    PIC  X(003) VALUE LOW-VALUE.

           03  WK-PIN2-OKEY    PIC  X(003) VALUE LOW-VALUE.
           03  WK-PIN2-NKEY    PIC  X(003) VALUE LOW-VALUE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** PIN1,PIN2 KEY 判定 BEFORE は UNTIL 条件先に判定する
           PERFORM TEST BEFORE
                   UNTIL WK-PIN1-EOF = HIGH-VALUE AND
                         WK-PIN2-EOF = HIGH-VALUE
               EVALUATE TRUE

                   WHEN WK-PIN1-NKEY < WK-PIN2-NKEY
                       MOVE    PIN1-REC    TO      POT1-REC
      *    *** WRITE POT1
                       PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                       PERFORM S020-10     THRU    S020-EX

                   WHEN WK-PIN1-NKEY = WK-PIN2-NKEY
      *    *** PIN1 <= PIN2 更新処理
                       PERFORM S110-10     THRU    S110-EX
                       MOVE    PIN1-REC    TO      POT1-REC
      *    *** WRITE POT1
                       PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                       PERFORM S020-10     THRU    S020-EX
      *    *** READ PIN2
                       PERFORM S030-10     THRU    S030-EX

      *             WHEN WK-PIN1-NKEY > WK-PIN2-NKEY
                   WHEN OTHER
                       MOVE    PIN2-REC    TO      POT1-REC
      *    *** WRITE POT1
                       PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN2
                       PERFORM S030-10     THRU    S030-EX
               END-EVALUATE
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
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1（マスター）
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT

                   MOVE    PIN1-KEY    TO      WK-PIN1-NKEY
                   IF      WK-PIN1-OKEY >=     WK-PIN1-NKEY
                           DISPLAY WK-PGM-NAME " PIN1 KEY SEQ ERROR"
                          DISPLAY WK-PGM-NAME " PIN1 OKEY=" WK-PIN1-OKEY
                          DISPLAY WK-PGM-NAME " PIN1 NKEY=" WK-PIN1-NKEY
                           STOP    RUN
                   END-IF
                   MOVE    WK-PIN1-NKEY TO     WK-PIN1-OKEY
           ELSE
      *    *** STATUS = 10 (END OF FILE)
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
                   MOVE    HIGH-VALUE  TO    WK-PIN1-NKEY
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT

                   MOVE    PIN2-KEY    TO      WK-PIN2-NKEY
                   IF      WK-PIN2-OKEY >=     WK-PIN2-NKEY
                           DISPLAY WK-PGM-NAME " PIN2 KEY SEQ ERROR"
                          DISPLAY WK-PGM-NAME " PIN2 OKEY=" WK-PIN2-OKEY
                          DISPLAY WK-PGM-NAME " PIN2 NKEY=" WK-PIN2-NKEY
                           STOP    RUN
                   END-IF
                   MOVE    WK-PIN2-NKEY TO     WK-PIN2-OKEY
           ELSE
      *    *** STATUS = 10  (END OF FILE)
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
                   MOVE    HIGH-VALUE  TO      WK-PIN2-NKEY
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN
           .
       S030-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           WRITE   POT1-REC

           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO        WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** PIN1 = PIN2 更新処理
       S110-10.

           EVALUATE TRUE
      *    *** ** => SPACE クリアー
               WHEN PIN2-A = "**"
                   MOVE    SPACE       TO      PIN1-A
      *    *** SPACE => 項目変更無
               WHEN PIN2-A = SPACE
                   CONTINUE
      *    *** OTHER => 項目内容で変更
               WHEN OTHER
                   MOVE    PIN2-A      TO      PIN1-A
           END-EVALUATE

           EVALUATE TRUE
               WHEN PIN2-B = "**"
                   MOVE    SPACE       TO      PIN1-B
               WHEN PIN2-B = SPACE
                   CONTINUE
               WHEN OTHER
                   MOVE    PIN2-B      TO      PIN1-B
           END-EVALUATE

           EVALUATE TRUE
               WHEN PIN2-C = "**"
                   MOVE    SPACE       TO      PIN1-C
               WHEN PIN2-C = SPACE
                   CONTINUE
               WHEN OTHER
                   MOVE    PIN2-C      TO      PIN1-C
           END-EVALUATE
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
