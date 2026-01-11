      *    *** É\Å[ÉX1ï∂éöÇ√Ç¬ï\é¶
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST133.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SJIS Åiä¥Ç∂óLÇËÇÕÅjÇ≈ÉZÅ[ÉuÇ∑ÇÈ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(080).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-CNT        PIC  9(006).
           03                  PIC  X(094).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST133 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST133.PRM1 ".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST133.PIN1".
      *    *** ÉtÉ@ÉCÉãñºÇÕÇrÇiÇhÇrÇ…ïœä∑ÇµÇƒÉZÉbÉg
           03  WK-PIN1-F-NAME  PIC  X(128) VALUE "XXXXXXXX.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST133.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-FILE-NAME    PIC  X(128) VALUE SPACE.
      *    *** 0.1 ïb
           03  WK-NANOSEC01    PIC  9(011) VALUE 100000000.
      *    *** 0.05 ïb
           03  WK-NANOSEC005   PIC  9(011) VALUE 50000000.
      *    *** 0.01 ïb
           03  WK-NANOSEC001   PIC  9(011) VALUE 10000000.
      *    *** 0.001 ïb
           03  WK-NANOSEC0001  PIC  9(011) VALUE 1000000.
      *    *** 0.0001 ïb
           03  WK-NANOSEC00001 PIC  9(011) VALUE 100000.

      *    *** èâä˙íl MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** èâä˙íl HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-END          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN,READ PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** LINE DISPLAY
                   PERFORM S100-10     THRU    S100-EX

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

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           OPEN    INPUT       PRM1-F
           READ    PRM1-F
                   AT  END
                   DISPLAY WK-PGM-NAME " PRM1-F 0π› "
                   STOP    RUN
           END-READ
           ADD     1           TO      WK-PRM1-CNT

      *    *** ÉtÉ@ÉCÉãñºÇÕäøéöÇÃÇ›Ç©ÅAÇPÉoÉCÉgånÇÃÇ›ÇÃÇ«ÇøÇÁÇ©Ç…ï“èWÇ∑ÇÈ
           IF      PRM1-REC (1:1) >=   X"E0" AND <= X"EF"
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    WK-PRM1-LEN TO      WDE05-BUF1-LEN
                   MOVE    WK-PRM1-CNT TO      WDE05-BUF1-CNT
      *    *** ÉtÉ@ÉCÉãñº ÇtÇsÇeÇWÅÅÅÑÇrÇiÇhÇrÇ…ïœä∑
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               PRM1-REC
                                               WK-FILE-NAME
      *             MOVE    "TEST133."  TO      WK-PIN1-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (1:)
      *             MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
      *                                         (WDE05-BUF2-LEN + 9:5)
           ELSE
      *             MOVE    "TEST133."  TO      WK-PIN1-F-NAME (1:8)
                   MOVE    PRM1-REC    TO      WK-PIN1-F-NAME (1:)
      *             MOVE    ".PIN1"     TO      WK-PIN1-F-NAME
      *                                         (WK-PRM1-LEN + 9:5)
           END-IF

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           MOVE    "UTF8"      TO      WFD-KANJI
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

      *             UNSTRING PIN1-REC
      *                     DELIMITED BY SPACE
      *                     INTO
      *                     WK-NNO
      *             END-UNSTRING
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** LINE DISPLAY
       S100-10.

           MOVE    WK-PIN1-CNT TO      PIN1-CNT
           IF      WK-PIN1-LEN =       ZERO
                   MOVE    6           TO      WK-PIN1-LEN
           END-IF
      *    *** DISPLAY
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
                   DISPLAY PIN1-REC (I:1) NO ADVANCING
      *             CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01
      *             CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC005
                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC0001
           END-PERFORM
           DISPLAY " "
           .
       S100-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"

           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 åèêî = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"

           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 åèêî = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"

           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 åèêî = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
