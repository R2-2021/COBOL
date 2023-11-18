      *    *** アイドル横丁 データの出力　AAA/BBB/CCC分割
      *    *** 出力したデータをUTF-8＝＞SJIS変換してＥｘｃｅｌで読み取る

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST12.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      * SELECT PIN1-F           ASSIGN   "TEST12.PIN1"
       SELECT PIN1-F           ASSIGN   "アイドル横丁2018.txt"
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   "TEST12.POT1"
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
      *    RECORD VARYING FROM WK-INT5 TO WK-INT6 DEPENDING WK-PIN1-LEN.
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-I1         PIC  9(0005).
           03  POT1-I2         PIC  X(0001).
           03  POT1-I3         PIC  X(1018).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST12  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST12.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST12.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN         BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  O               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX
      *
      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

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

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
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

      *    *** WRITE POT1
       S100-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L > WK-PIN1-LEN
                   PERFORM VARYING I FROM L BY 1
                           UNTIL PIN1-REC (I:1) = "/" OR
                                 L > WK-PIN1-LEN
                           ADD     +1      TO      O
                           IF      O      =    1 AND
                                   PIN1-REC(I:1) =     SPACE
                                   ADD  -1         TO      O
                           ELSE
                               MOVE    PIN1-REC(I:1) TO   POT1-I3 (O:1)
                               MOVE    I           TO      L
                           END-IF
                   END-PERFORM

                   ADD     1           TO      WK-POT1-CNT
                   MOVE    WK-POT1-CNT TO      POT1-I1
                   MOVE    ","         TO      POT1-I2
                   WRITE   POT1-REC
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    ZERO        TO      O
                   ADD     +1          TO      L
           END-PERFORM
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

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 Mｹﾝｽｳ= " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
