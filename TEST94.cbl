      *    *** random test

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST94.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** XX データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XX データ
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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST94  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST94.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST94.POT1".

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

      *    *** 4.2.81 FUNCTION RANDOM より
       01  random-float        usage float-long.
       01  tally               BINARY-LONG SYNC VALUE ZERO.
       01  random-integer      pic 99.

       01  results.
           05 hits             pic 9(9) occurs 10 times.
       01  first-ten           pic 99.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *    *** RANDOM TEST
           PERFORM S100-10     THRU    S100-EX

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

      *    *** PIN1-F READ
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
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

      *    *** RANDOM TEST
       S100-10.

      *> compute random-float = random(0)
           perform 100000 times
               compute random-float = random() * 10.0
               compute random-integer = random-float + 1.0
               if random-integer < 1 or > 10 then
                  display "anomaly: " random-integer upon syserr
               end-if
               add 1 to hits(random-integer)
               if first-ten < 10 then
                  display random-integer space with no advancing
                  add 1 to first-ten
               end-if
           end-perform
           display "..."

           perform varying tally from 1 by 1 until tally > 10
               display tally ": " hits(tally)
           end-perform
           .
      *    *** random () は 発生率が一様でない
      *    *** 
      * C:\Users\xxxx\OneDrive\ドキュメント\COBOL>test94
      * TEST94   START
      * 05 02 07 03 02 02 02 03 03 03 ...
      * +0000000001: 000000002
      * +0000000002: 000033986
      * +0000000003: 000034104
      * +0000000004: 000011819
      * +0000000005: 000003402
      * +0000000006: 000003394
      * +0000000007: 000003385
      * +0000000008: 000003328
      * +0000000009: 000003307
      * +0000000010: 000003273
      * FILEDUMP POT1 ｹﾝｽｳ =              3 (FILEDUMP.POT1                   )
      * FILEDUMP POT2 ｹﾝｽｳ =              3 (FILEDUMP.POT2                   )
      * TEST94   END
      * TEST94   PIN1 ｹﾝｽｳ =              0 (TEST94.PIN1                     )
      * TEST94   POT1 ｹﾝｽｳ =              0 (TEST94.POT1                     )
      * TEST94   START=21/10/11 20:15:25.40 END=21/10/11 20:15:26.06 月(MON)
      * TEST94   処理時間          0.66秒でした

       S100-EX.
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
