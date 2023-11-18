      *    *** ＊＊＊　ＳＪＩＳ　漢字コード表　＊＊＊の出力
      *    *** PRINT AREA 2次元でセット　（４０行／１ページ）

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST17.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
             05  PIN1-MOJI     OCCURS 188
                               PIC  X(002).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(136).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST17  ".

           03  WK-PIN1-F-NAME  PIC  X(011) VALUE "TEST17.PIN1".
           03  WK-POT1-F-NAME  PIC  X(011) VALUE "TEST17.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-TIT1.
      *      05  FILLER        PIC  X(010) VALUE SPACE.
             05  FILLER        PIC  X(050) VALUE SPACE.
             05  FILLER        PIC  X(040) VALUE
                 "＊＊＊　ＳＪＩＳ　漢字コード表　＊＊＊　".
             05  FILLER        PIC  X(010) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.
      *    *** 19*7=133 
           03  WK-MID1         PIC  X(133) VALUE ALL "コード ".
           03  WK-HAI          PIC  X(133) VALUE ALL "------ ".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 40
                               PIC  X(136) VALUE SPACE.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE 1.
           03  P2              BINARY-LONG SYNC VALUE 3.
           03  P3              BINARY-LONG SYNC VALUE 4.
           03  P4              BINARY-LONG SYNC VALUE 5.
           03  P5              BINARY-LONG SYNC VALUE 6.
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE ZERO.
           03  SW-AMP          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

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

      *    *** WRITE POT1 END TOTAL
           PERFORM S110-10     THRU    S110-EX

      *    CLOSE
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

           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

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
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       188
      *         IF (PIN1-MOJI(I) >=  X"EE4D" AND
      *             PIN1-MOJI(I) <=  X"EEFC")
      *             CONTINUE
      *         ELSE
                   MOVE    PIN1-MOJI(I) TO  PR-LINE (J) (P1:2)
                   MOVE    PIN1-MOJI(I) (1:1) TO  PIC-X

                   DIVIDE PIC-Halfword BY 16
                          GIVING L REMAINDER R

                   ADD     1        TO      L R

                   MOVE    Hex-Digit (L)   TO  PR-LINE (J) (P2:1)
                   MOVE    Hex-Digit (R)   TO  PR-LINE (J) (P3:1)

                   MOVE    PIN1-MOJI(I) (2:1) TO  PIC-X

                   DIVIDE PIC-Halfword BY 16
                          GIVING L REMAINDER R

                   ADD     1        TO      L R

                   MOVE    Hex-Digit (L)   TO  PR-LINE (J) (P4:1)
                   MOVE    Hex-Digit (R)   TO  PR-LINE (J) (P5:1)
                   ADD     1           TO      J
                   IF      J           >       40
                           MOVE    1           TO       J
                           ADD     7           TO       P1
                           IF      P1          >        130
                               ADD     1           TO      WK-PAGE
                               MOVE    WK-PAGE     TO      WK-TIT1-PAGE
                               WRITE   POT1-REC    FROM    WK-TIT1
                               ADD     1           TO      WK-POT1-CNT
                               WRITE   POT1-REC    FROM    WK-MID1
                               ADD     1           TO      WK-POT1-CNT
                               WRITE   POT1-REC    FROM    WK-HAI
                               ADD     1           TO      WK-POT1-CNT
                               PERFORM VARYING K  FROM 1 BY 1
                                       UNTIL   K   >       40
                                 WRITE   POT1-REC    FROM     PR-LINE(K)
                                 ADD     1           TO      WK-POT1-CNT
                               END-PERFORM
                               MOVE    SPACE       TO      PRINT-AREA
                               MOVE    1           TO      P1
                               MOVE    3           TO      P2
                               MOVE    4           TO      P3
                               MOVE    5           TO      P4
                               MOVE    6           TO      P5
                           ELSE
                               ADD     7           TO      P2
                               ADD     7           TO      P3
                               ADD     7           TO      P4
                      ADD     7           TO      P5
                           END-IF
                   ELSE
                           CONTINUE
                   END-IF
      *         END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 END TOTAL
       S110-10.

      *    *** テーブル出力後、AT ENDの対応
           IF      P1          =       1 AND
                   J           =       1
                   CONTINUE
           ELSE

                   ADD     1           TO      WK-PAGE
                   MOVE    WK-PAGE     TO      WK-TIT1-PAGE
                   WRITE   POT1-REC    FROM    WK-TIT1 
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-MID1
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-HAI
                   ADD     1           TO      WK-POT1-CNT

                   PERFORM VARYING K  FROM 1 BY 1
                           UNTIL   K   >       40
                           WRITE   POT1-REC    FROM    PR-LINE(K)
                           ADD     1           TO      WK-POT1-CNT
                   END-PERFORM
           END-IF
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
