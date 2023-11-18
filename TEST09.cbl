      *    *** http:// 行の出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST09.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      * SELECT PIN1-F           ASSIGN   "TEST09.PIN1"
       SELECT PIN1-F           ASSIGN   "dmm_act_reco.html"
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   "TEST09.POT1"
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
      *    RECORD VARYING FROM WK-INT5 TO WK-INT6 DEPENDING WK-PIN1-LEN.
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST09  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST09.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST09.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-FIND         PIC  X(001) VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

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

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
      *
           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN

           END-IF
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
      *            DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
                   ADD     1           TO        WK-PIN1-CNT
           ELSE
      *    *** STATUS = 10 (END OF FILE)
      *    *** ORGANIZATION IS にすると STATUS=4 がAT ENDのとき、入る
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
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

           MOVE    ZERO        TO      WK-FIND.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-PIN1-LEN - 7
                   IF      PIN1-REC(I:7) =     "http://" 
                       MOVE    ZERO    TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL   PIN1-REC(J:1) =       "'" OR '"' OR 
                                   SPACE
                           ADD     1           TO      L
                       END-PERFORM
                       MOVE    PIN1-REC(I:L) TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO        WK-POT1-CNT
                       MOVE    J           TO        I
                       DISPLAY WK-POT1-CNT
                  END-IF
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

           DISPLAY WK-PGM-NAME " END"
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
          .
       S900-EX.
           EXIT.
