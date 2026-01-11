      *    *** TEST74 単体未登録抽出データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST126.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST74.POT1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 単体未登録抽出データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
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

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST126 ".

           03  WK-PIN1-F-NAME  PIC  X(064) VALUE "TEST74.POT1".
           03  WK-POT1-F-NAME  PIC  X(064) VALUE "TEST126.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ITEM1        PIC  X(100) VALUE SPACE.
           03  WK-ITEM2        PIC  X(100) VALUE SPACE.

           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-2      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-3      BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1-MAX          BINARY-LONG SYNC VALUE 2000.
           03  I2-MAX          BINARY-LONG SYNC VALUE 2000.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL1-AREA.
      *    *** テーブルサイズ変更したら、I1-MAX も変更する
           03  TBL01-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL01-ITEM1
                               INDEXED BY TBL01-IDX.
             05  TBL01-ITEM1   PIC  X(100) VALUE HIGH-VALUE.
             05  TBL01-ITEM2   PIC  X(100) VALUE SPACE.

       01  TBL2-AREA.
      *    *** テーブルサイズ変更したら、I2-MAX も変更する
           03  TBL02-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL02-ITEM1
                               INDEXED BY TBL02-IDX.
             05  TBL02-ITEM1   PIC  X(100) VALUE HIGH-VALUE.
             05  TBL02-ITEM2   PIC  X(100) VALUE SPACE.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** TBL1,2 テーブルセット
                   PERFORM S022-10     THRU    S022-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-ITEM1

           SORT    TBL02-AREA
                   ASCENDING KEY TBL02-ITEM1

      *    *** TBL01-IDX はテーブルにセットした＋１が入っているので、
      *    *** I = TBL01-IDX にしている
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I >= TBL01-IDX
      *    *** 抽出データ出力
                   PERFORM S100-10     THRU    S100-EX
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           SET     TBL01-IDX   TO      1
           SET     TBL02-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.
      *    *** あいださくら ,https://missav.com/dm21/ja/actresses/あいださくら?filters=individual  , ,
      *    *** ICHIKA ,https://missav.com/dm14/ja/actresses/ICHIKA  , ,

           MOVE    SPACE       TO      WK-ITEM1
                                       WK-ITEM2

           MOVE    ZERO        TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN

           MOVE    ZERO        TO      WK-COUNT-1
                                       WK-COUNT-2
                                       WK-COUNT-3

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT

                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-ITEM1    COUNT WK-ITEM1-LEN
                           WK-ITEM2    COUNT WK-ITEM2-LEN

                   INSPECT WK-ITEM2 TALLYING
                           WK-COUNT-1 FOR ALL "https://missav.com"
                           WK-COUNT-2 FOR ALL "actresses"
                           WK-COUNT-3 FOR ALL "filters=individual"

           END-READ
           .
       S020-EX.
           EXIT.

      *    *** PIN1 TBL SET
       S022-10.

           IF      TBL01-IDX   >       I1-MAX
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           IF      TBL02-IDX   >       I2-MAX
                   DISPLAY WK-PGM-NAME
                           " TBL02-TBL OVER TBL02-IDX=" TBL02-IDX
                   STOP    RUN
           END-IF

           IF      WK-COUNT-1  NOT =   ZERO
                   IF      WK-COUNT-2  NOT =   ZERO
                           IF      WK-COUNT-3  NOT =   ZERO
                                   MOVE    WK-ITEM1    TO
                                           TBL02-ITEM1 (TBL02-IDX)
                                   SET     TBL02-IDX   UP  BY  1
                           ELSE
                                   MOVE    WK-ITEM1    TO
                                           TBL01-ITEM1 (TBL01-IDX)
                                   SET     TBL01-IDX   UP  BY  1
                           END-IF
                   ELSE
                           CONTINUE
                   END-IF
           ELSE
                   CONTINUE
           END-IF
           .
       S022-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           SEARCH  ALL TBL02-AREA
                   AT  END 
                           MOVE    "N"         TO      SW-SEARCH

                   WHEN TBL02-ITEM1 (TBL02-IDX) = TBL01-ITEM1 (I)
                           MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

      *    *** アンマッチの時、出力する
           IF      SW-SEARCH   =       "N"
                   MOVE   TBL01-ITEM1 (I) TO   POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
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
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
