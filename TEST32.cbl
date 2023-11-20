      *    *** SORT サンプル

      *    *** PIN1 ZERO件、内部でデータ70 バイト1,000,000件作った時、
      *    *** 約37.43秒 (SORT 時間 37.43-16.24=21.19)

      *    *** PIN1 同上と同じインプットデータ70バイト1,000,000件、
      *    *** 約51.85秒 (SORT 時間 51.85-39.01=12.84)

      *    *** COBSORTTBL で実行 KEY ２つで同じだが ９タイプ、Ｘタイプ違い有
      *    *** PIN1 同上と同じインプットデータ70バイト1,000,000件、
      *    *** 約62.17秒 9タイプの時、約81.50秒 Xタイプの時

      *    *** TEST03 で実行 KEY ２つで同じだが ９タイプ、Ｘタイプ違い有
      *    *** PIN1 同上と同じインプットデータ70バイト1,000,000件、
      *    *** 約51.60秒 9タイプの時、約42.42秒 Xタイプの時

      *    *** TEST01 で実行 1,000,000件 READ 時間 22.77秒
      *    *** READ,WRITE 時間 39.01秒 (WRITE 時間 16.24秒)

      *    *** C:\Users\xxxx\OneDrive\ドキュメント\COBOL>TEST32
      *    *** 100,000 件 ８０バイト
      *    *** TEST32   処理時間          3.27秒でした

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST32.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT サンプルデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORTWORK
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME.

      *    *** SORT後 サンプルデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
           03  SIO1-KEY1       PIC  9(010).
           03  SIO1-KEY2       PIC  9(010).
           03  SIO1-DATA1      PIC  9(020).
           03  SIO1-DATA2      PIC  X(020).
           03  SIO1-DATA3      PIC  X(020).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  FILLER          PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST32  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32.POT1".
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "TEST32.SIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST32.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-DATA1        PIC S9(1)V9(9)  VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE 0.

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           SORT    SIO1-F
                   ASCENDING KEY SIO1-KEY1
                   ASCENDING KEY SIO1-KEY2
      *    *** READ AND RELEASE
                   INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                   OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-SEC                SECTION.
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *    *** SORT-F はOPEN いらない

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY "TEST32 PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY "TEST32 POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ AND RELEASE
       S100-SEC            SECTION.
       S100-10.
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                           ADD     1           TO      WK-PIN1-CNT
                           RELEASE SIO1-REC    FROM    PIN1-REC
                   ELSE

                       IF  WK-PIN1-STATUS =    10

                           PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > 1000000
                               MOVE    FUNCTION RANDOM TO  WK-DATA1
                               COMPUTE SIO1-KEY1
                                       SIO1-DATA1 = WK-DATA1 * 100000000

                               MOVE    FUNCTION RANDOM TO  WK-DATA1
                               COMPUTE SIO1-KEY2 = WK-DATA1 * 100000000
      *                          COMPUTE WK-DATA2 = WK-DATA1 * 1000000
                               MOVE    SIO1-KEY2   TO    SIO1-DATA2
                               MOVE    SIO1-KEY1   TO    SIO1-DATA3
                               RELEASE SIO1-REC
                           END-PERFORM
                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                       ELSE

                           DISPLAY "TEST32 PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.

           PERFORM UNTIL WK-SIO1-EOF = HIGH-VALUE

                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                       NOT AT END
                           WRITE   POT1-REC    FROM    SIO1-REC
                           IF      WK-POT1-STATUS NOT =  ZERO
                             DISPLAY "TEST32 POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                       END-IF
                       ADD     1           TO      WK-POT1-CNT
                    END-RETURN
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-SEC                SECTION.
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY "TEST32 PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY "TEST32 POT1-F CLOSE ERROR STATUS="
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
