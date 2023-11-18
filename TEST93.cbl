      *    *** dir ディレクトリ 容量同一データのみ ファイル出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST93.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** dir データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** dir ディレクトリ 容量同一データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST93  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST93.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST93.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-OLD     PIC  X(1024) VALUE SPACE.
           03  WK-BYTE-CNT     BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AK   (ANK=>KANJI)
           03  WK-MODE         PIC  X(002) VALUE "AK".
      *    *** 初期値 HENKAN=UU (UTF8=>UTF8)
           03  WK-HENKAN       PIC  X(006) VALUE "UU".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  KEY-AREA.
             05  KEY-OLD.
               07  KEY-OBYTE   PIC  X(017) VALUE LOW-VALUE.
             05  KEY-NEW.
               07  KEY-NBYTE   PIC  X(017) VALUE LOW-VALUE.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM WITH TEST AFTER
                   UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF    WK-PIN1-EOF   NOT =   HIGH-VALUE
      *    *** 
      *                     PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-IF

      *    *** キーダブっているデータのみ、出力
                   IF      KEY-OLD     =       KEY-NEW
      *    *** WRITE POT1 PIN1-OLD
                           WRITE   POT1-REC    FROM    WK-PIN1-OLD
                           ADD     1           TO      WK-POT1-CNT
                                                       WK-BYTE-CNT
                   ELSE
                           IF      WK-BYTE-CNT >=      1
                               WRITE   POT1-REC    FROM    WK-PIN1-OLD
                               ADD     1           TO      WK-POT1-CNT

                               MOVE    SPACE       TO      POT1-REC
                               WRITE   POT1-REC
                               ADD     1           TO      WK-POT1-CNT
                           END-IF
                           MOVE   ZERO        TO      WK-BYTE-CNT
                   END-IF
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

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** PIN1-F READ
       S020-10.

           MOVE    KEY-NEW     TO      KEY-OLD
           MOVE    PIN1-REC    TO      WK-PIN1-OLD

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT

                   MOVE    PIN1-REC (19:17) TO KEY-NBYTE
                   IF      KEY-OLD     >       KEY-NEW
                           DISPLAY WK-PGM-NAME " PIN1-F SEQ ERROR"
                           DISPLAY WK-PGM-NAME " KEY-OLD=" KEY-OLD
                           DISPLAY WK-PGM-NAME " KEY-NEW=" KEY-NEW
                           STOP    RUN
                   END-IF
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               KEY-NEW
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *    *** 漢字スペース無表示
      *     MOVE    ALL X"EFBEA0" TO    WK-TITLE

      *    *** UTF8 => UTF8 (ANK=>KANJI)
      *     MOVE    "CHANGE"    TO      WDE05-ID
      *     MOVE    WK-HENKAN   TO      WDE05-HENKAN
      *     MOVE    WK-MODE     TO      WDE05-MODE
      *     MOVE    60          TO      WDE05-BUF1-LEN
      *     MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
      *     CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                 WK-TITLE2
      *                                 WK-TITLE
           .
       S020-EX.
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

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

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
