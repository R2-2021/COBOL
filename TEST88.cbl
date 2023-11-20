      *    *** ディレクトリ 解析、アンマッチ、マッチング
      *    *** TEST88 => TEST88
      *    *** C:\Users\... DIR データ UTF8 (TEST85.POT1)
      *    *** M:Kachin-PC\...  DIR 解析データ UTF8
      *    ***  (TEST86.POT1=>TEST23.POT1)

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST88.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** C:\Users\... DIR データ UTF8 (TEST85.POT1)
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ここにあるファイルを振り分ける
      *    *** M:Kachin-PC\...  DIR 解析データ UTF8
      *    ***  (TEST86.POT1=>TEST23.POT1)
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION INDEXED
           ACCESS SEQUENTIAL
           RECORD KEY PIN2-KEY.

      *    *** マッチング データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アンマッチ データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(012).
           03  PIN1-KEY        PIC  X(256).
           03                  PIC  X(003).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(012).
           03  PIN2-KEY        PIC  X(256).
           03  PIN2-LEN        PIC  9(003).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(271).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(271).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST88  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST85.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST87.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST88.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST88.POT2".

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

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-NAME         PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** READ PIN2 CHECK
           PERFORM S032-10     THRU    S032-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                     AND WK-PIN2-EOF = HIGH-VALUE
                   EVALUATE TRUE

                      WHEN PIN1-KEY < PIN2-KEY
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX

                      WHEN PIN1-KEY = PIN2-KEY
      *    *** WRITE POT1
      *    *** マッチング分、未使用なのでコメントにする
      *                    PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
      *    *** READ PIN2 CHECK
                           PERFORM S032-10     THRU    S032-EX

      *                WHEN PIN1-KEY > PIN2-KEY
                      WHEN OTHER
      *    *** WRITE POT2
                           PERFORM S110-10     THRU    S110-EX
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
      *    *** READ PIN2 CHECK
                           PERFORM S032-10     THRU    S032-EX
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
           IF      WK-PIN1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT = ZERO
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
                                               PIN1-KEY
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS NOT = ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
                                               PIN2-KEY
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ

           IF      WK-PIN2-STATUS NOT = ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** READ PIN2 CHECK
       S032-10.

           MOVE    "N"         TO      SW-NAME
      *    *** 自動でバックアップされるファイル名は除外
      *    *** 漢字はＵＴＦ８なので、画面上の桁位置はずれる、
      *    *** 漢字文字数＋SAKURA-EDIT 上の画面位置 = PIN2-LEN
           PERFORM UNTIL SW-NAME = "Y"
                      OR WK-PIN2-EOF = HIGH-VALUE
               IF        PIN2-KEY (PIN2-LEN - 08:09) = "Large.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 08:09) = "Small.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 10:11) = "\Folder.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 11:12) = "\desktop.ini"
                     OR  PIN2-KEY (PIN2-LEN - 11:12) = "\Desktop.ini"
                     OR  PIN2-KEY (PIN2-LEN - 06:07) = ".4L.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 05:06) = ".L.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 05:06) = ".M.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 05:06) = ".S.jpg"
                     OR  PIN2-KEY (PIN2-LEN - 09:10) = "\Thumbs.db"
                     OR  PIN2-KEY (PIN2-LEN - 04:05) = ".modd"
                     OR  PIN2-KEY (PIN2-LEN - 04:05) = ".moff"
                     OR  PIN2-KEY (PIN2-LEN - 09:10) = ".search-ms"
                     OR  PIN2-KEY (PIN2-LEN - 03:04) = ".qdl"
                     OR  PIN2-KEY (PIN2-LEN - 11:12) = "_desktop.ini"
                     OR  PIN2-KEY (PIN2-LEN - 06:07) = "\PCM.db"
                     OR  PIN2-KEY (PIN2-LEN - 07:08) = ".onetoc2"
                     OR  PIN2-KEY (PIN2-LEN - 07:08) = "sentinel"
                     OR  PIN2-KEY (PIN2-LEN - 18:19) =
                         "\.webaxs\users\xxxx"
                     OR  PIN2-KEY (PIN2-LEN - 22:23) =
      *    *** C:\ は...cache M:\ は....Cache になってた
                         ".GenerateResource.Cache"
                     OR  PIN2-KEY (PIN2-LEN - 07:08) = ".ide-shm"
                     OR  PIN2-KEY (PIN2-LEN - 07:08) = ".ide-wal"

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
                   MOVE    "N"         TO      SW-NAME
               ELSE
                   MOVE    "Y"         TO      SW-NAME
               END-IF
           END-PERFORM
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1
      *    *** マッチング （バックアッ済）
       S100-10.

           WRITE   POT1-REC    FROM    PIN2-REC
           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT2
      *    *** アンマッチ （削除対象）
       S110-10.

           WRITE   POT2-REC    FROM    PIN2-REC
           IF      WK-POT2-STATUS =    ZERO
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"

           DISPLAY WK-PGM-NAME " PIN2 FILEが PIN1に有る時、"
                               "POT1へ、無い時、POT2へ出力"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 Mｹﾝｽｳ= " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 Uｹﾝｽｳ= " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
