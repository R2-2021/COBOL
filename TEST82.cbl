      *    *** 可変長 ファイル　テスト出力
      *    *** BINARY SEQUENTIAL で POT1-REC 可変長で出力される
      *    *** FILEDUMP では中身８０バイト分入っているが、
      *    *** POT1-REC の中身は、可変長になっている

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST82.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** LINE SEQUENTIAL 
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** BINARY SEQUENTIAL 
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F
           RECORD IS VARYING IN SIZE
           FROM 5 TO 80 CHARACTERS DEPENDING ON WK-POT1-LEN.
      *     LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           05  POT1-KEY        PIC  X(005).
           05  POT1-DATA       PIC  X(75).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST82".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST82.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST82.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PTN          PIC  9(002) VALUE 1.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE07  REPLACING ==:##:== BY ==WDE07==.

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

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
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

           DISPLAY WK-PGM-NAME " PTN INPUT 01-10"
           ACCEPT  WK-PTN
           IF      WK-PTN      IS      NUMERIC
               AND WK-PTN      >=      1
               AND WK-PTN      <=      10
                   CONTINUE
           ELSE
                   MOVE    1           TO      WK-PTN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE07-ID
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
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

      *    *** WRITE POT1
       S100-10.

      *     MOVE    "CHANGE"    TO      WDE07-ID
      *     MOVE    PIN1-REC    TO      WDE07-ASCII
      *     MOVE    WK-PTN      TO      WDE07-PTN
      *     CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *     PERFORM VARYING I FROM 1 BY 1
      *             UNTIL I > 16
      *             WRITE   POT1-REC    FROM    WDE07-LINE (I)
      *             ADD     1           TO      WK-POT1-CNT
      *     END-PERFORM

           EVALUATE TRUE

               WHEN WK-PIN1-CNT = 1
                   MOVE    7           TO      WK-POT1-LEN
                   MOVE    X"07"       TO      PIN1-REC (1:1)
                   MOVE    X"0D0A"     TO      PIN1-REC (6:2)
                   WRITE   POT1-REC    FROM    PIN1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    "P"         TO      WFD-ID
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC

               WHEN WK-PIN1-CNT = 2
                   MOVE    8           TO      WK-POT1-LEN
                   MOVE    X"08"       TO      PIN1-REC (1:1)
                   MOVE    X"0D0A"     TO      PIN1-REC (7:2)
                   WRITE   POT1-REC    FROM    PIN1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC

               WHEN WK-PIN1-CNT = 3
                   MOVE    9           TO      WK-POT1-LEN
                   MOVE    X"09"       TO      PIN1-REC (1:1)
                   MOVE    X"0D0A"     TO      PIN1-REC (8:2)
                   WRITE   POT1-REC    FROM    PIN1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC
           END-EVALUATE

      *     MOVE    80        TO      WK-POT1-LEN
      *     MOVE    SPACE     TO      POT1-REC
      *     WRITE   POT1-REC
      *     ADD     1         TO      WK-POT1-CNT
           .
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

           MOVE    "CLOSE "    TO      WDE07-ID
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

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
