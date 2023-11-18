      *    *** 画像ランダム配置、ｈｔｍｌ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST105.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** IMG DIR FILE名 データ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG DIR PATH名 データ
       SELECT PRM2-F           ASSIGN   WK-PRM2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 画像配置ｈｔｍｌサンプルデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 画像ｈｔｍｌ データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(136).

       FD  PRM2-F
           RECORD VARYING DEPENDING ON WK-PRM2-LEN.
       01  PRM2-REC.
           03                  PIC  X(100).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(100).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST105 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST1052.PRM1".
           03  WK-PRM2-F-NAME  PIC  X(032) VALUE "TEST105.PRM2".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST105.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST1052.html".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PRM2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PRM2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PRM2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-NUM          BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-F-NAME  PIC  X(100) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PRM1-EOF = HIGH-VALUE
      *    *** TBL01 SET
                   PERFORM S032-10     THRU    S032-EX

      *    *** READ PRM1
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** READ PRM2
           PERFORM S040-10     THRU    S040-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 画像サムネイル付加
                   EVALUATE TRUE

                       WHEN WK-PIN1-LEN = ZERO
                           MOVE    SPACE       TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                       WHEN PIN1-REC (1:10) = '<img src="'
                           MOVE    PRM2-REC    TO      PIN1-REC (11:)
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT

                       WHEN PIN1-REC (1:3) = "IMG"
                           ADD     1           TO      K
                           COMPUTE WK-NUM ROUNDED = WCR-RND (K)
                                   * WK-PRM1-CNT
                           IF      WK-NUM      =       ZERO
                                   MOVE    1           TO      WK-NUM
                           END-IF
                           MOVE    TBL01-F-NAME (WK-NUM) TO POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                       WHEN OTHER
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PRM1-F
                               PRM2-F
                               PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           MOVE    "RND"       TO      WCR-ID
           MOVE    99          TO      WCR-IDX
           CALL    "COBRND"    USING   WCR-COBRND-AREA

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
           .
       S020-EX.
           EXIT.

      *    *** READ PRM1
       S030-10.

           READ    PRM1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
               NOT AT END
                   ADD     1           TO      WK-PRM1-CNT
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** TBL01 SET
       S032-10.

           ADD     1           TO      K1
           IF      K1          >       100
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           MOVE    PRM1-REC (37:100) TO TBL01-F-NAME (K1)
           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** READ PRM2
       S040-10.

           READ    PRM2-F
               AT END
      *             MOVE    HIGH-VALUE  TO      WK-PRM2-EOF
                   DISPLAY WK-PGM-NAME " PRM2 PATH 無エラー"
                   STOP    RUN
               NOT AT END
                   ADD     1           TO      WK-PRM2-CNT
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PRM2-F
                   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 件数 = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PRM2-CNT TO      WK-PRM2-CNT-E
           DISPLAY WK-PGM-NAME " PRM2 件数 = " WK-PRM2-CNT-E
                   " (" WK-PRM2-F-NAME ")"
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
