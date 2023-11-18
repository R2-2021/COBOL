      *    *** XVI PIN4 国選択指示のみ、出力対象

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST60.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 女優名データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 国名・略称、女優名データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 国名・略称、国名データ、未使用
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 出力情報データ（国名・略称で指定）
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 女優名データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(100).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  FILLER          PIC  X(100).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD.
       01  PIN3-REC.
           03  FILLER          PIC  X(100).

       FD  PIN4-F
           LABEL RECORDS ARE STANDARD.
       01  PIN4-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST60  ".
      *    *** %ＸＶＩ
      *    *** ジャパリＡ
      *    *** Aali Kali
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST60.PIN1".
      *    *** af  ,Yasmeena
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST60.PIN2".
      *    *** jp  , Japan
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST60.PIN3".
      *    *** PIN1 出力対象、国名・略称指定
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST60.PIN4".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST60.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-NAME         PIC  X(100) VALUE SPACE.
           03  WK-NAME2        PIC  X(100) VALUE SPACE.
           03  WK-CO-NAME      PIC  X(050) VALUE SPACE.
           03  WK-COUNTRY      PIC  X(004) VALUE SPACE.
           03  WK-PIN4-COUNTRY PIC  X(004) VALUE SPACE.

           03  WK-NAME-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-NAME2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CO-NAME-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNTRY-LEN  BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

      *    *** PIN2 用
       01  TBL02-AREA-G.
           03  TBL02-AREA      OCCURS 20000
                               ASCENDING KEY IS TBL02-NAME
                               INDEXED BY TBL02-IDX.
             05  TBL02-COUNTRY PIC  X(004) VALUE SPACE.
             05  TBL02-NAME    PIC  X(100) VALUE HIGH-VALUE.

      *    *** PIN3 用
       01  TBL03-AREA-G.
           03  TBL03-AREA      OCCURS 200
                               ASCENDING KEY IS TBL03-COUNTRY
                               INDEXED BY TBL03-IDX.
             05  TBL03-COUNTRY PIC  X(004) VALUE SPACE.
             05  TBL03-CO-NAME PIC  X(050) VALUE HIGH-VALUE.

      *    *** PIN4 用
       01  TBL04-AREA-G.
           03  TBL04-AREA      OCCURS 20
                               ASCENDING KEY IS TBL04-COUNTRY
                               INDEXED BY TBL04-IDX.
             05  TBL04-COUNTRY PIC  X(004) VALUE HIGH-VALUE.

       01  SW-AREA.
           03  SW-JP           PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

      *    *** READ PIN4
           PERFORM S050-10     THRU    S050-EX



           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL02 SET
                   PERFORM S031-10     THRU    S031-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** TBL03 SET
                   PERFORM S041-10     THRU    S041-EX
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** TBL04 SET
                   PERFORM S051-10     THRU    S051-EX
      *    *** READ PIN4
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM



      *    *** TBL02 SORT
           SORT    TBL02-AREA
                   ASCENDING KEY TBL02-NAME

      *    *** TBL03 SORT
           SORT    TBL03-AREA
                   ASCENDING KEY TBL03-COUNTRY

      *    *** TBL04 SORT
           SORT    TBL04-AREA
                   ASCENDING KEY TBL04-COUNTRY

           IF      PIN1-REC (1:1) =    "%"
      *    *** タイトル編集
                   PERFORM S060-10     THRU    S060-EX
           END-IF



           PERFORM UNTIL WK-PIN1-EOF   =       HIGH-VALUE
                   EVALUATE TRUE

      *    *** タイトル％はそのまま出力
                       WHEN PIN1-REC (1:1) =   "%"
      *    *** WRITE POT1
                           PERFORM S100-10     THRU    S100-EX

      *    *** Beautifulは、そのまま出力
      *                 WHEN PIN1-REC (1:9) =   "Beautiful"
      *    *** WRITE POT1
      *                     PERFORM S100-10     THRU    S100-EX

      *    *** ゼロバイトはそのまま出力
      *                 WHEN WK-PIN1-LEN =      ZERO
      *    *** WRITE POT1
      *                     PERFORM S100-10     THRU    S100-EX

      *    *** ジャパリはそのまま出力
      *    *** ジャパリ,Ａ...Ｚ
                       WHEN PIN1-REC(1:12) = X"E382B8E383A3E38391E383AA"
      *    *** WRITE POT1
                           PERFORM S100-10     THRU    S100-EX

      *    *** NAME2 <> SPACE 第二名称あるとき、そのまま出力
                       WHEN WK-NAME2 NOT =     SPACE
      *    *** WRITE POT1
                           IF      SW-JP       =       "Y"
                               PERFORM S100-10     THRU    S100-EX
                           END-IF

                       WHEN OTHER
      *    *** NAME で国名・略称取得後TBL02,TBL04 で対象国か チェック
      *    *** SEARCH,WRITE POT1
                           PERFORM S110-10     THRU    S110-EX
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

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F OPEN ERROR STATUS="
                           WK-PIN4-STATUS
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

           SET     TBL02-IDX   TO      1
           SET     TBL03-IDX   TO      1
           SET     TBL04-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-NAME
                                       WK-NAME2
           MOVE    ZERO        TO      WK-NAME-LEN
                                       WK-NAME2-LEN
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-NAME      COUNT WK-NAME-LEN
                           WK-NAME2     COUNT WK-NAME2-LEN
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
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

           MOVE    SPACE       TO      WK-COUNTRY
                                       WK-NAME
           MOVE    ZERO        TO      WK-COUNTRY-LEN
                                       WK-NAME-LEN
           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-COUNTRY  COUNT WK-COUNTRY-LEN
                           WK-NAME     COUNT WK-NAME-LEN
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** TBL02 SET
       S031-10.

           IF      TBL02-IDX   >       20000
                   DISPLAY WK-PGM-NAME
                           " TBL02-TBL OVER TBL02-IDX=" TBL02-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-COUNTRY  TO      TBL02-COUNTRY (TBL02-IDX)
           MOVE    WK-NAME     TO      TBL02-NAME    (TBL02-IDX)

           SET     TBL02-IDX   UP  BY  1
           .
       S031-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           MOVE    SPACE       TO      WK-COUNTRY
                                       WK-CO-NAME
           MOVE    ZERO        TO      WK-COUNTRY-LEN
                                       WK-CO-NAME-LEN
           READ    PIN3-F

           IF      WK-PIN3-STATUS =    ZERO
                   ADD     1           TO      WK-PIN3-CNT
                   UNSTRING PIN3-REC
                           DELIMITED BY ","
                           INTO
                           WK-COUNTRY  COUNT WK-COUNTRY-LEN
                           WK-CO-NAME  COUNT WK-CO-NAME-LEN
           ELSE
               IF  WK-PIN3-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** TBL03 SET
       S041-10.

           IF      TBL03-IDX   >       200
                   DISPLAY WK-PGM-NAME
                           " TBL03-TBL OVER TBL03-IDX=" TBL03-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-COUNTRY  TO      TBL03-COUNTRY (TBL03-IDX)
           MOVE    WK-CO-NAME  TO      TBL03-CO-NAME (TBL03-IDX)

           SET     TBL03-IDX   UP  BY  1
           .
       S041-EX.
           EXIT.

      *    *** READ PIN4
       S050-10.

           READ    PIN4-F

           IF      WK-PIN4-STATUS =    ZERO
                   ADD     1           TO      WK-PIN4-CNT
                   MOVE    PIN4-REC (1:4) TO   WK-PIN4-COUNTRY
           ELSE
               IF  WK-PIN4-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN4-F READ ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S050-EX.
           EXIT.

      *    *** TBL04 SET
       S051-10.

           IF      TBL04-IDX   >       20
                   DISPLAY WK-PGM-NAME
                           " TBL04-TBL OVER TBL04-IDX=" TBL04-IDX
                   STOP    RUN
           END-IF

           MOVE    PIN4-REC (1:4) TO   TBL04-COUNTRY (TBL04-IDX)
           IF      PIN4-REC (1:2) =    "jp"
                   MOVE    "Y"         TO      SW-JP
           END-IF

           SET     TBL04-IDX   UP  BY  1
           .
       S051-EX.
           EXIT.

      *    *** タイトル編集
       S060-10.

      *    *** TBL03 SEARCH し、国情報取得
           SEARCH  ALL TBL03-AREA
               AT END
                   CONTINUE

               WHEN TBL03-COUNTRY (TBL03-IDX) = WK-PIN4-COUNTRY
                   MOVE    SPACE       TO      PIN1-REC (6:1)
                   MOVE    TBL03-CO-NAME (TBL03-IDX) TO PIN1-REC (7:50)
           END-SEARCH
           .
       S060-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           WRITE   POT1-REC    FROM    PIN1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S110-10.

      *    *** TBL02 SEARCH し、国情報取得
           SEARCH  ALL TBL02-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL02-NAME (TBL02-IDX) =   WK-NAME
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH   =       "N"
                   EXIT    PARAGRAPH
           ELSE
                   CONTINUE
           END-IF

      *    *** none は出力対象外にする
           IF      TBL02-COUNTRY (TBL02-IDX) = "none"
                   EXIT    PARAGRAPH
           ELSE
                   CONTINUE
           END-IF

           MOVE    TBL02-COUNTRY (TBL02-IDX) TO WK-COUNTRY

      *    *** TBL04 SEARCH し、出力対象か、チェック
           SEARCH  ALL TBL04-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL04-COUNTRY (TBL04-IDX) = WK-COUNTRY
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH   =       "N"
                   EXIT    PARAGRAPH
           ELSE
                   CONTINUE
           END-IF

           WRITE   POT1-REC    FROM    PIN1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           IF      WK-POT1-CNT =       27
                   DISPLAY WK-PGM-NAME " WK-POT1-CNT=27 "
                           "PIN4 国パラメータ スペル確認"
                   DISPLAY WK-PGM-NAME " % XVI,ジャパリＡ,...,"
                           "ジャパリＺ の２７件しか無かった"
                   DISPLAY WK-PGM-NAME " PIN3 (1:2) の国記号で指定する"
                   STOP    RUN
           END-IF

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

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F CLOSE ERROR STATUS="
                           WK-PIN4-STATUS
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
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 ｹﾝｽｳ = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 ｹﾝｽｳ = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
