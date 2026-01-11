      *    *** XXX.html <XXX> で分解
      *    *** CBL_READ_FILE 使用

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST131.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(20000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST131 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST131.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST131.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-READ-ONLY    PIC 9(001) VALUE 1.
           03  WK-WRITE-ONLY   PIC 9(001) VALUE 2.
           03  WK-READ-WRITE   PIC 9(001) VALUE 3.

           03  WK-FILEHANDLE   USAGE IS POINTER.
           03  WK-FILENAME     PIC X(040) VALUE SPACE.
           03  WK-CFILE        PIC X(041) VALUE SPACE.
           03  WK-ACCESS-MODE  USAGE BINARY-LONG VALUE ZERO.
           03  WK-FILE-LOCK    PIC X(001) VALUE ZERO.
           03  WK-DEVICE       PIC X(001) VALUE ZERO.
           03  WK-RESULT       USAGE BINARY-LONG VALUE ZERO.

           03  WK-FILE-OFFSET  PIC S9(018) COMP VALUE ZERO.
           03  WK-FILE-OFFSET2 PIC S9(018) COMP VALUE ZERO.
           03  WK-READ-LENGTH  PIC S9(009) COMP VALUE ZERO.
           03  WK-FILE-FLAGS   BINARY-CHAR VALUE ZERO.
           03  WK-READ-BUFFER  PIC X(20000) VALUE SPACE.
           03  WK-MARQUEE      PIC X(20000) VALUE SPACE.
           03  WK-MARQUEE-LIMIT PIC S9(018) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L-MAX           BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX


      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                    PERFORM S100-10    THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE "ANIMATETIMES.202505.HTML" TO WK-FILENAME.

           MOVE WK-READ-ONLY TO WK-ACCESS-MODE.
           STRING WK-FILENAME DELIMITED BY SPACE
                  LOW-VALUE DELIMITED BY SIZE
                  INTO WK-CFILE
           END-STRING

           CALL    "CBL_OPEN_FILE" USING WK-CFILE
                                       WK-ACCESS-MODE
                                       WK-FILE-LOCK
                                       WK-DEVICE
                                       WK-FILEHANDLE
                   RETURNING WK-RESULT
           END-CALL

           MOVE 20000 TO WK-MARQUEE-LIMIT.

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

      *    *** WRITE POT1
       S100-10.

           MOVE 128 TO WK-FILE-FLAGS.

           CALL    "CBL_READ_FILE" USING WK-FILEHANDLE
                                       WK-FILE-OFFSET
                                       WK-READ-LENGTH
                                       WK-FILE-FLAGS
                                       WK-READ-BUFFER
                   RETURNING WK-RESULT
      *    *** 総レコード長
           DISPLAY "WK-FILE-OFFSET =" WK-FILE-OFFSET

           MOVE    ZERO        TO      WK-FILE-FLAGS
                                       WK-FILE-OFFSET
           MOVE    SPACE       TO      WK-READ-BUFFER
                                       POT1-REC

           MOVE    20000       TO      WK-READ-LENGTH

      *     PERFORM VARYING WK-FILE-OFFSET FROM 0 BY 20000
      *     PERFORM FOREVER WITH TEST AFTER
           PERFORM WITH TEST AFTER
      *     PERFORM WITH TEST BEFORE
                   UNTIL (WK-RESULT NOT = 0)
      *        OR (WK-FILE-OFFSET > WK-MARQUEE-LIMIT)
                   MOVE    SPACE       TO      WK-READ-BUFFER
                   CALL    "CBL_READ_FILE" USING WK-FILEHANDLE
                                               WK-FILE-OFFSET
                                               WK-READ-LENGTH
                                               WK-FILE-FLAGS
                                               WK-READ-BUFFER
                           RETURNING WK-RESULT
                   END-CALL
                   ADD     1           TO      WK-PIN1-CNT

      *             MOVE    "P"         TO      WFD-ID
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         WK-READ-BUFFER

      *         MOVE WK-READ-BUFFER TO WK-MARQUEE
               INSPECT WK-READ-BUFFER REPLACING ALL X"0D0A" BY "  "
               INSPECT WK-READ-BUFFER REPLACING ALL X"0A" BY SPACE

                   ADD     20000       TO      WK-FILE-OFFSET
      *    *** 編集
                   PERFORM S110-10     THRU    S110-EX

           END-PERFORM

           IF      POT1-REC    NOT =   SPACE
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
      *     DISPLAY "WK-RESULT=" WK-RESULT
           .
       S100-EX.
           EXIT.

      *    *** 編集
       S110-10.

           IF      L           >=      20000
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       MOVE    ZERO        TO      L
                       MOVE    SPACE       TO      POT1-REC
           END-IF

      *     IF      POT1-REC    NOT =   SPACE
      *             DISPLAY WK-POT1-CNT  " L=" L  
      *                     " POT1-REC=" POT1-REC (1:70)
      *     END-IF

      *     MOVE    ZERO        TO      L
      *     MOVE    SPACE       TO      POT1-REC

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 20000

               IF     WK-READ-BUFFER (I:1) =     "<"

                   IF      L           >       ZERO
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       MOVE    ZERO        TO      L
                       MOVE    SPACE       TO      POT1-REC
                   END-IF

                   PERFORM TEST AFTER
                           VARYING J FROM I BY 1
                           UNTIL   WK-READ-BUFFER (J:1) = ">" 
                                OR J > 20000

                       ADD     1           TO      L
                                                   WK-FILE-OFFSET2
                       IF      L           >       20000
      *                     DISPLAY WK-PGM-NAME "POT1-REC OVER1 L=" L
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
      *                     MOVE    ZERO        TO      L
                           MOVE    SPACE       TO      POT1-REC
                           MOVE    1           TO      L
      *                     STOP    RUN
                       END-IF

                       IF      L          >        L-MAX
                               MOVE    L          TO       L-MAX
                       END-IF

                       MOVE    WK-READ-BUFFER (J:1) TO
                               POT1-REC (L:1)

                       IF      WK-READ-BUFFER (J:1) = ">"
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    ZERO        TO      L
                           MOVE    SPACE       TO      POT1-REC
                           MOVE    J           TO      I
                           EXIT    PERFORM
                       END-IF

                   END-PERFORM

               ELSE

                   ADD     1           TO      L
                                               WK-FILE-OFFSET2
                   IF      L           >       20000
      *                 DISPLAY WK-PGM-NAME "POT1-REC OVER2 L=" L
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
      *                     MOVE    ZERO        TO      L
                           MOVE    SPACE       TO      POT1-REC
                           MOVE    1           TO      L
      *                 STOP    RUN
                   END-IF

                   IF      L          >        L-MAX
                           MOVE    L          TO       L-MAX
                   END-IF

                   IF      L          =        1
                       AND WK-READ-BUFFER (I:1) = SPACE
                           ADD     -1          TO      L
                   ELSE
                           MOVE    WK-READ-BUFFER (I:1) TO
                                   POT1-REC (L:1)
                   END-IF

      *             IF      WK-READ-BUFFER (I:1) = ">"
      *                 DISPLAY POT1-REC (1:80)
      *                 WRITE   POT1-REC
      *                 ADD     1           TO      WK-POT1-CNT
      *                 MOVE    ZERO        TO      L
      *                 MOVE    SPACE       TO      POT1-REC
      *             END-IF

      *             MOVE    "P"         TO      WFD-ID
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         POT1-REC
               END-IF

           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           DISPLAY "WK-FILE-OFFSET2=" WK-FILE-OFFSET2
           DISPLAY "L-MAX=" L-MAX

           CALL    "CBL_CLOSE_FILE" USING WK-FILEHANDLE
                   RETURNING WK-RESULT.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
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
