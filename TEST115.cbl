      *    *** mini vs dell ディレクトリチェック
      *    *** OneDrive ディレクトリ
      *    *** 
      *    *** COPYの実行は、PIN1-F より CD XXXフォルダーを実行し、
      *    *** 該当のファイルをCOPYして、出力先は
      *    ***  C:\Users\koko\OneDrive なので、そこから該当のフォルダーに
      *    ***  移動する
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST115.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** Directory of,File(s)  onedrive データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** Directory of,File(s)  onedrive2,3 データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 更新有分 データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ＣＯＰＹ データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 更新無分 データ（PIN1に無、PIN3に有のファイル）
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(1000).

       FD  POT3-F.
       01  POT3-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST115 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "dir.txt".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE
      *         "dir2.txt".
               "dir3.txt".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST115.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST115.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST115.POT3".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-DIR          PIC  X(100) VALUE SPACE.
           03  WK-DIR2         PIC  X(100) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-HIT          PIC  X(001) VALUE "N".
           03  SW-POT3-WRITE   PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 10000.
             05  TBL01-DATE    PIC  X(017) VALUE SPACE.
             05  TBL01-SIZE    PIC  X(010) VALUE SPACE.
             05  TBL01-FILE    PIC  X(100) VALUE SPACE.
             05  TBL01-DIR     PIC  X(100) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   IF      PIN1-REC (5:1) =    "/"
                       AND PIN1-REC (22:5) NOT = "<DIR>"
      *    *** TBL SET
                           PERFORM S022-10     THRU    S022-EX
                   ELSE
                       IF      PIN1-REC (WK-PIN1-LEN - 13:14) =
                               "のディレクトリ"
      *    *** 14 + 24
                               MOVE    PIN1-REC (25:WK-PIN1-LEN - 38)
                                               TO      WK-DIR
                       ELSE
                               CONTINUE
                       END-IF
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

                   IF      PIN2-REC (5:1) =    "/"
                       AND PIN2-REC (22:5) NOT = "<DIR>"
      *    *** データチェック
                           PERFORM S100-10     THRU    S100-EX
                   ELSE
      *                 DISPLAY WK-PIN2-CNT 
      *                         PIN2-REC (WK-PIN2-LEN - 13:14)
                       IF      PIN2-REC (WK-PIN2-LEN - 13:14) =
                               "のディレクトリ"
      *    *** 14:のディレクトリ
      *    *** 25: C:\Users\koko\OneDrive\
      *    *** 14 + 25
                               MOVE    PIN2-REC (26:WK-PIN2-LEN - 39)
                                                   TO      WK-DIR2
      *                         IF WK-PIN2-CNT < 100
      *                           DISPLAY PIN2-REC (26:WK-PIN2-LEN - 39) 
      *                         END-IF
      *    *** WRITE POT1
                               PERFORM S110-10     THRU    S110-EX
      *    *** WRITE POT3
                               PERFORM S130-10     THRU    S130-EX
                      ELSE
                               CONTINUE
                      END-IF
                   END-IF

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

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** TBL SET
       S022-10.

           ADD     1           TO      I
           IF      I           >       10000
                   DISPLAY WK-PGM-NAME " TBL01 OVER I="
                           I
                   STOP    RUN
           END-IF

           MOVE    PIN1-REC (1:17)   TO TBL01-DATE (I)
           MOVE    PIN1-REC (26:10)  TO TBL01-SIZE (I)
           MOVE    PIN1-REC (37:100) TO TBL01-FILE (I)
           MOVE    WK-DIR            TO TBL01-DIR  (I)
           MOVE    I           TO      I-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
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

      *    *** データチェック
       S100-10.

           MOVE    "N"         TO      SW-HIT
           MOVE    "Y"         TO      SW-POT3-WRITE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-HIT = "Y"

      *             IF WK-PIN2-CNT < 100
      *               DISPLAY I
      *               DISPLAY PIN2-REC (37:50)
      *               DISPLAY TBL01-FILE (I) (1:50)
      *               DISPLAY WK-DIR2 (1:50)
      *               DISPLAY TBL01-DIR (I) (1:50)
      *             END-IF

                   IF      PIN2-REC (37:100) = TBL01-FILE (I)
                       AND WK-DIR2 = TBL01-DIR (I)

                       IF  PIN2-REC (37:12) = "eSIMILAR.exe"
                         DISPLAY PIN2-REC (01:17)
                         DISPLAY PIN2-REC (26:10)
                         DISPLAY PIN2-REC (37:30)
                         DISPLAY TBL01-DATE (I)
                         DISPLAY TBL01-SIZE (I)
                         DISPLAY TBL01-FILE (I)
                       END-IF

                       IF      PIN2-REC (1:17) =   TBL01-DATE (I)
      *                     AND PIN2-REC (26:10) =  TBL01-SIZE (I)
                           MOVE    "Y"         TO      SW-HIT
                           MOVE    "N"         TO      SW-POT3-WRITE
                       ELSE
                           IF      PIN2-REC (1:17) >   TBL01-DATE (I)
      *    *** WRITE POT1
                               PERFORM S110-10     THRU    S110-EX
      *    *** WRITE POT1
      *    *** COPY コマンド
                               PERFORM S120-10     THRU    S120-EX
                               MOVE    "N"         TO      SW-HIT
                               MOVE    "N"         TO      SW-POT3-WRITE
                           ELSE
                               MOVE    "Y"         TO      SW-HIT
                               MOVE    "N"         TO      SW-POT3-WRITE
                           END-IF
                       END-IF
                   ELSE
                       CONTINUE
                   END-IF
           END-PERFORM

           IF      SW-POT3-WRITE =     "Y"
      *    *** WRITE POT3
                   PERFORM S130-10     THRU    S130-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           WRITE   POT1-REC    FROM    PIN2-REC

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

      *    *** WRITE POT1
      *    *** COPY コマンド
       S120-10.

           MOVE    "COPY "     TO      POT2-REC
           MOVE    PIN2-REC (37:100) TO POT2-REC (6:100)
           MOVE    " C:\Users\koko\OneDrive"
                               TO      POT2-REC (106:23)
           WRITE   POT2-REC

           IF      WK-POT2-STATUS =    ZERO
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT3
       S130-10.

           WRITE   POT3-REC    FROM    PIN2-REC

           IF      WK-POT3-STATUS =    ZERO
                   ADD     1           TO      WK-POT3-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT3-F WRITE ERROR STATUS="
                           WK-POT3-STATUS
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

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
