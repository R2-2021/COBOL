      *    *** 通常メインプログラムは-xでコンパイルする XXXX.exeが実行される
      *    *** サブルーチンは-mでコンパイルする XXXX.dllが実行される
      *    *** cobc test01 cbl 大文字でも、小文字でもコンパイル可能
      *    *** -x は小文字でないとだめ
      *    *** cobc -x test01.cbl
      *    *** TEST01
      *    *** 
      *    *** メインプログラムもサブルーチンも-mでコンパイルして、
      *    *** XXXX.dll が作成され、cobcrun で実行出来る
      *    *** cobc test01 cbl cobcrun 大文字でも、小文字でもコンパイル
      *    *** 実行可能
      *    *** -m は小文字でないとだめ
      *    *** cobc -m test01.cbl
      *    *** TEST01 はPROGRAM-IDで指定した文字と同じでないと実行しない
      *    *** 大文字、小文字識別している
      *    *** cobcrun TEST01

      *    *** READ / WRITE TEST

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST01.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *    *** PACKED-DECIMAL の時、この指定が必要、
      *    *** X"10",X"0D"があると行末までカット、文字が削除される
      *    *** BINARY SEQUENTIAL WRITE, BINARY SEQUENTIAL READ でも問題なし 

      *    ORGANIZATION IS RECORD BINARY SEQUENTIAL. 
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
      *     ORGANIZATION IS RECORD BINARY SEQUENTIAL. 
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
      *     03  FILLER          PIC  X(020).
      *     03  PIN1-KANJI      PIC  X(020).
      *     03  FILLER          PIC  X(040).
      *     03  FILLER          PIC  X(3).
           03  FILLER          PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
      *     03  POT1-I1         OCCURS 3
      *                         PIC  X(001).
           03                   PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST01  ".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST01.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM06.POT1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32X.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST01.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** 0-255 WRITE POT1
      *     PERFORM S100-10     THRU    S100-EX

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

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

      *    *** ORGANIZATION IS にすると、
      *    *** AT END でも以下実行しない
      *            AT END
      *            MOVE    HIGH-VALUE    TO    WK-PIN1-EOF
      *    END-READ
           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT

      *             MOVE    "P"         TO      WFD-ID
      *             MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC
           ELSE
      *    *** STATUS = 10 (END OF FILE)
      *    *** ORGANIZATION IS にすると STATUS=4 がAT ENDのとき、入る
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

      *    *** 0-255 WRITE POT1
       S100-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 256
                   compute PIC-Halfword = I - 1
                   MOVE    SPACE       TO      POT1-REC
      *             MOVE    PIC-X       TO      POT1-I1 (1)
      *             MOVE    X"0D"       TO      POT1-I1 (2)
      *             MOVE    X"0A"       TO      POT1-I1 (3)
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "P"         TO      WFD-ID
                   MOVE    I           TO      WFD-SEQ
                   MOVE    2           TO      WFD-SU
                   MOVE    "M"         TO      WFD-TYPE
                   MOVE    "      "    TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC
           END-PERFORM

      *     MOVE    "あいうえお" TO       PIN1-KANJI
      *     WRITE   POT1-REC    FROM      PIN1-REC
      *     IF      WK-POT1-STATUS NOT =  ZERO
      *             DISPLAY "TEST01 POT1-F WRITE ERROR STATUS="
      *                     WK-POT1-STATUS
      *             STOP    RUN
      *     END-IF
      *      WRITE   POT1-REC    FROM      PIN1-REC
      *     ADD     1           TO        WK-POT1-CNT
      *

      *     PERFORM S100        THRU      S100-EX
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           WRITE   POT1-REC

           IF      WK-POT1-STATUS NOT =  ZERO
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
           
           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
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
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
