      *    *** アニメイト＿アニメ一覧用　声優データで抽出
      *    *** インプット　TEST26.POT1　シフトＪＩＳに変更する

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST27.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST26.POT1 アニメデータ　SHIFT-JIS変更後 
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** animatetimes.女性声優お気に入り.csv　声優データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 声優データにマッチングした　アニメデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 声優データにアンマッチした　アニメデータ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 声優データにマッチングしたタイトルデータ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 声優データにマッチングした、タイトル情報含む、アニメデータ１件目
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
                               STATUS   WK-POT4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  FILLER          PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(1024).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03  FILLER          PIC  X(1024).

       FD  POT4-F
           LABEL RECORDS ARE STANDARD.
       01  POT4-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST27  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST26.POT1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST26_2018ALL.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE
               "COBRND.seiyu.okiniiri.csv".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST27.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST27.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST27.POT3".
           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST27.POT4".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT4-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** 退避エリア
           03  WK-ITEM1        PIC  X(256) VALUE SPACE.
           03  WK-ITEM2        PIC  X(256) VALUE SPACE.
           03  WK-ITEM3        PIC  X(256) VALUE SPACE.
           03  WK-ITEM4        PIC  X(256) VALUE SPACE.
           03  WK-ITEM5        PIC  X(256) VALUE SPACE.
           03  WK-ITEM6        PIC  X(256) VALUE SPACE.
           03  WK-ITEM7        PIC  X(256) VALUE SPACE.
           03  WK-ITEM8        PIC  X(256) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-HIT          PIC  X(001) VALUE ZERO.
           03  SW-HIT2         PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 500.
             05  TBL01-TITLE   PIC  X(256) VALUE SPACE.
           03  TBL02-AREA      OCCURS 500.
             05  TBL02-SEIYU   PIC  X(080) VALUE SPACE.

       PROCEDURE   DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2 声優データ　テーブルセット処理
           PERFORM S030-10     THRU    S030-EX
                   UNTIL   WK-PIN2-EOF   =     HIGH-VALUE

      *    *** READ PIN1 アニメデータ
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** アニメデータＲＥＡＤ　マッチング処理
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1 アニメデータ
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** タイトルデータ　出力
           PERFORM S120-10     THRU    S120-EX

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

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F OPEN ERROR STATUS="
                           WK-POT4-STATUS
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

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT

                   UNSTRING PIN1-REC
                            DELIMITED BY ","
                       INTO
                            WK-ITEM1
                            WK-ITEM2
                            WK-ITEM3
                            WK-ITEM4
                            WK-ITEM5
                            WK-ITEM6
                            WK-ITEM7
                            WK-ITEM8
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

      *    *** READ PIN2 声優データ
       S030-10.

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
                   ADD     1           TO      I
                   IF      I           >       500
                           DISPLAY WK-PGM-NAME " TBL02 OVER I=" I
                           STOP    RUN
                   ELSE
                           MOVE    PIN2-REC    TO      TBL02-SEIYU (I)
                           MOVE    I           TO      I-MAX
                   END-IF
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

      *    *** アニメデータＲＥＡＤ　マッチング処理
       S100-10.

           MOVE    ZERO        TO      SW-HIT
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > I-MAX
                        OR SW-HIT = "1"
      *    *** WK-ITEM8 = 声優名
                   IF      WK-ITEM8    =       TBL02-SEIYU(I) 
                           MOVE    "1"         TO      SW-HIT
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           IF      SW-HIT      =       "1"
      *    *** WRITE POT1 声優名マッチング分
                   PERFORM S110-10     THRU    S110-EX
           ELSE
      *    *** WRITE POT2 声優名アンマッチング分
                   WRITE   POT2-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT2-CNT
           END-IF
           .
       S100-EX.
           EXIT.

       S110-10.

           WRITE   POT1-REC    FROM    PIN1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      SW-HIT2
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL   J > J-MAX
                        OR SW-HIT2 = "1"

      *    *** タイトルマッチングか？
                   IF      WK-ITEM4    =       TBL01-TITLE (J)
                           MOVE    "1"         TO      SW-HIT2
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           IF      SW-HIT2     =       ZERO
                   ADD     1           TO      J-MAX
                   IF      J-MAX       >       500
                          DISPLAY WK-PGM-NAME " TBL02 OVER J-MAX=" J-MAX
                           STOP    RUN
                   ELSE
                           MOVE    WK-ITEM4    TO    TBL01-TITLE (J-MAX)

      *    *** タイトル一致した、アニメデータ１件目のみ　出力処理
                           WRITE   POT4-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT4-CNT
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** タイトルデータのみ　出力処理
       S120-10.

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL   J   >       J-MAX

                   WRITE   POT3-REC    FROM    TBL01-TITLE (J)
                   ADD     1           TO      WK-POT3-CNT
           END-PERFORM
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

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F CLOSE ERROR STATUS="
                           WK-POT4-STATUS
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
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 ｹﾝｽｳ = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4 ｹﾝｽｳ = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
