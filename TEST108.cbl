      *    *** TEST70.PIN5 翻訳（英語＝＞日本語）データ作成

      *    *** JOB=C.TEST108.bat

      *    *** TEST108.POT1 に ふりがなを追加する

      *    *** TEST108
      *    ***   |
      *    *** TEST109
      *    ***   |
      *    *** COBSORT COBSORT.T015.PRM1
      *    ***   |
      *    *** TEST70
      *    ***   |
      *    *** TEST53
      *    ***   |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST108.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST70.PIN5 xvitag_t_s.csv => TEST108.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** POT2 を GOOGLE 翻訳とGOOGLE スプレッドシート(Ｅｘｃｅｌ)で
      *    *** CSVデータにしてインプット
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 翻訳に置き換えたら、xvitag_t_s.csvに置き換える
      *    *** TEST70.CBL PIN5 TEST108.POT1 に変更した
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 翻訳前データ取り出し　翻訳データ付けて、次回PIN2データにする
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST108 ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "xvitag_t_s.csv".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST108.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST108.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST108.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST108.POT2".

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

           03  WK-ID1          PIC  X(050) VALUE SPACE.
           03  WK-ID1-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-ID2          PIC  X(050) VALUE SPACE.
           03  WK-ID2-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HONYAKU      PIC  X(100) VALUE SPACE.
           03  WK-HONYAKU-LEN  BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE 10000.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** テーブルサイズ変更したら、I-MAX も変更する
           03  TBL01-AREA      OCCURS 10000
                               ASCENDING KEY IS TBL01-ID
                               INDEXED BY TBL01-IDX.
             05  TBL01-ID      PIC  X(050) VALUE HIGH-VALUE.
             05  TBL01-ID-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HONYAKU PIC  X(100) VALUE SPACE.
             05  TBL01-HONYAKU-LEN BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** tag-a より前は出力しない
      *    *** WRITE POT1
           PERFORM S110-10     THRU    S110-EX

           PERFORM UNTIL   PIN1-REC (13:5) = "tag-a"
      *    *** ジャパリtag-aまで読み飛ばし
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   IF      PIN1-REC (13:5) NOT = "tag-a"
      *    *** tag-a より前は出力しない
      *    *** WRITE POT1
                           PERFORM S110-10     THRU    S110-EX
      *                     CONTINUE
                   END-IF
           END-PERFORM

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 TBL SET
                   PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM



      *    *** TBL01 SORT
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-ID



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   IF      PIN1-REC (13:4) = "tag-"

      *    *** WRITE POT1
      *    *** tag- は出力しない
                           CONTINUE
      *                     PERFORM S110-10     THRU    S110-EX
                   ELSE

      *    *** 翻訳データチェック
                           PERFORM S100-10     THRU    S100-EX

      *    *** WRITE POT2
                           PERFORM S120-10     THRU    S120-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN 1
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
           SET     TBL01-IDX   TO      1

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
                   UNSTRING PIN1-REC
                           DELIMITED BY "," OR SPACE
                           INTO
                           WK-ID1      COUNT WK-ID1-LEN
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT = " WK-PIN1-CNT
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-ID2
                                       WK-HONYAKU
           MOVE    ZERO        TO      WK-ID2-LEN
                                       WK-HONYAKU-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                           DELIMITED BY "," OR SPACE
                           INTO
                           WK-ID2      COUNT WK-ID2-LEN
                           WK-HONYAKU  COUNT WK-HONYAKU-LEN
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

           IF      TBL01-IDX   >       I-MAX
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-ID2      TO      TBL01-ID      (TBL01-IDX)
           MOVE    WK-ID2-LEN  TO      TBL01-ID-LEN  (TBL01-IDX)
           MOVE    WK-HONYAKU  TO      TBL01-HONYAKU (TBL01-IDX)
           MOVE    WK-HONYAKU-LEN TO   TBL01-HONYAKU-LEN (TBL01-IDX)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** 翻訳データチェック
       S100-10.

           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    PIN1-REC    TO      POT1-REC
                   WRITE   POT1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

               WHEN TBL01-ID (TBL01-IDX)  =    WK-ID1

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    TBL01-HONYAKU-LEN(TBL01-IDX)
                                       TO      L
                   MOVE    TBL01-HONYAKU (TBL01-IDX)
                                       TO      POT1-REC (1:L)

                   COMPUTE P = 1 + L
                   MOVE    ","         TO      POT1-REC (P:1)

                   COMPUTE P = P + 1
                   MOVE    WK-ID1-LEN  TO      L
                   MOVE    WK-ID1      TO      POT1-REC (P:L)

                   COMPUTE P = P + L
                   MOVE    ","         TO      POT1-REC (P:1)

                   WRITE   POT1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-SEARCH
           .
       S100-EX.
           EXIT.

      *    *** tag-a より前のデータ
      *    *** WRITE POT1
      *    *** XXX,XXX, <= item1,item2 同じにする
      *    *** item1は表示用、item2は検索キー用
       S110-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    WK-ID1-LEN  TO      L
           MOVE    WK-ID1      TO      POT1-REC (1:L)

           COMPUTE P = 1 + L
           MOVE    ","         TO      POT1-REC (P:1)

           COMPUTE P = P + 1
           MOVE    WK-ID1      TO      POT1-REC (P:L)

           COMPUTE P = P + L
           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.

           MOVE    WK-ID1      TO      POT2-REC
           WRITE   POT2-REC

           IF      WK-POT2-STATUS =    ZERO
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   DISPLAY WK-PGM-NAME 
                           " POT2-F WRITE ERROR STATUS="
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
