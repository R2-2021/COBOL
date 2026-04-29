      *    *** TEST137.POT1.BAT CHECK
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST137.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 抽出パラメータ（タイトルなど）
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XXX.info 情報　(TEST135_N.POT1
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** INFO、DTCP コピー用BAT作成 SJIS作成の必要あり
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(100).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(2000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST137 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST138.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST136.POT1X2.BAT".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST137.POT1".

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

      *    *** XXX.info
           03  WK-YYYYMM       PIC  X(006) VALUE SPACE.
           03  WK-DTCP         PIC  X(041) VALUE SPACE.
           03  WK-DATE         PIC  X(010) VALUE SPACE.
           03  WK-TIME         PIC  X(005) VALUE SPACE.
           03  WK-GB           PIC  X(006) VALUE SPACE.
           03  WK-TITLE        PIC  X(2000) VALUE SPACE.
           03  WK-DESC         PIC  X(2000) VALUE SPACE.
           03  WK-GENRE1       PIC  X(100) VALUE SPACE.

           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-FORDER       PIC  X(100) VALUE SPACE.

           03  WK-TITLE2-SJIS  PIC  X(100) VALUE SPACE.
           03  WK-TITLE2-UTF8  PIC  X(100) VALUE SPACE.

           03  WK-FORDER-SJIS  PIC  X(100) VALUE SPACE.
           03  WK-FORDER-UTF8  PIC  X(100) VALUE SPACE.

           03  WK-ACCEPT       PIC  X(001) VALUE SPACE.
           03  WK-ACCEPT2      PIC  X(001) VALUE SPACE.

           03  WK-YYYYMM-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-DTCP-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-DATE-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-TIME-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-GB-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DESC-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-GENRE1-LEN   BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-FORDER-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (SJIS<=UTF8)
           03  WK-HENKAN       PIC  X(002) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  K3              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-HIT          PIC  X(001) VALUE "N".

      *    *** b3148f00-df1d-11f0-8000-649300004a92.dtcp.info
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-FILE  PIC  X(100) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
                   IF      WK-PIN2-CNT >= 14 AND <= 58
      *    *** TBL01 SET
                           PERFORM S032-10     THRU    S032-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           DISPLAY "K1-MAX=" K1-MAX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   IF      PIN1-REC (73:10) = ".dtcp.info"
      *    *** TBL01 SEARCH
                           PERFORM S100-10     THRU    S100-EX
                   END-IF

      *    *** READ PIN2
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PIN1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA


           OPEN    INPUT       PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F

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
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
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

           MOVE    PIN2-REC (42:46)   TO      TBL01-FILE (K1)
           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** TBL01 SEARCH
       S100-10.

           MOVE    "N"         TO      SW-HIT

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX
                      OR SW-HIT = "Y"
      *             DISPLAY WK-PIN1-CNT " " PIN1-REC (37:46) " "
      *                     TBL01-FILE (K1)
                   IF      PIN1-REC (37:46) = TBL01-FILE (K1)
                           MOVE    "Y"         TO      SW-HIT
                           DISPLAY "HIT " WK-PIN1-CNT
                                   " " PIN1-REC (37:46)
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
