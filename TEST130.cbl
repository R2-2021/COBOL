      *    *** TEST103 データ編集

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST130.

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
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST130 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST103.Random Walker Yu.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST130.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-ITEM1        PIC  X(500) VALUE SPACE.
           03  WK-ITEM2        PIC  X(500) VALUE SPACE.
           03  WK-ITEM3        PIC  X(500) VALUE SPACE.

           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM3-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-2      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-3      BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX


      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                    PERFORM S100-10    THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

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

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-ITEM1
                                       WK-ITEM2

           MOVE    ZERO        TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT

                   IF      PIN1-REC (1:1) =    '"'
                           PERFORM S022-10     THRU    S022-EX
                   END-IF

                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-ITEM1    COUNT WK-ITEM1-LEN
                           WK-ITEM2    COUNT WK-ITEM2-LEN
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** 
       S022-10.

           MOVE    SPACE       TO      PIN1-REC (1:1)

           PERFORM VARYING J FROM 2 BY 1
                   UNTIL PIN1-REC (J:1) = '"'
                   IF      PIN1-REC (J:1) =    ","
                           MOVE    SPACE       TO      PIN1-REC (J:1)
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      PIN1-REC (J:1)
           
           .
       S022-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      L1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-ITEM1-LEN

                   EVALUATE TRUE

                       WHEN WK-ITEM1 (I:1) = SPACE OR "(" OR ")"
                           MOVE    WK-REC (1:L1) TO    POT1-REC
                           MOVE    L1          TO      L2

                           ADD     1           TO      L2
                           MOVE    ","         TO      POT1-REC (L2:1)

                           ADD     1           TO      L2
                           MOVE    WK-REC (1:L1) TO    POT1-REC (L2:L1)

                           ADD     L1          TO      L2
                           MOVE    ","         TO      POT1-REC (L2:1)

                           ADD     1           TO      L2
                           MOVE    "0,"        TO      POT1-REC (L2:2)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    SPACE       TO      WK-REC
                           MOVE    ZERO        TO      L1
      *    *** 】
                       WHEN WK-ITEM1 (I:3) = X"E38091"
      *    *** 、
                         OR WK-ITEM1 (I:3) = X"E38081"
      *    *** の
                         OR WK-ITEM1 (I:3) = X"E381AE"
      *    *** や
                         OR WK-ITEM1 (I:3) = X"E38284"
      *    *** と
                         OR WK-ITEM1 (I:3) = X"E381A8"
      *    *** （
                         OR WK-ITEM1 (I:3) = X"EFBC88"
      *    *** ）
                         OR WK-ITEM1 (I:3) = X"EFBC89"
      *    *** →
                         OR WK-ITEM1 (I:3) = X"E28692"
                           MOVE    WK-REC (1:L1) TO    POT1-REC
                           MOVE    L1          TO      L2

                           ADD     1           TO      L2
                           MOVE    ","         TO      POT1-REC (L2:1)

                           ADD     1           TO      L2
                           MOVE    WK-REC (1:L1) TO    POT1-REC (L2:L1)

                           ADD     L1          TO      L2
                           MOVE    ","         TO      POT1-REC (L2:1)

                           ADD     1           TO      L2
                           MOVE    ",0,"       TO      POT1-REC (L2:3)

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           MOVE    SPACE       TO      WK-REC
                           MOVE    ZERO        TO      L1

                           ADD     2           TO      I

                       WHEN OTHER
                           ADD     1           TO      L1
                           MOVE    WK-ITEM1 (I:1) TO
                                   WK-REC (L1:1)

                   END-EVALUATE
           END-PERFORM

           IF      L1          >       ZERO
                   MOVE    WK-REC (1:L1) TO    POT1-REC
                   MOVE    L1          TO      L2

                   ADD     1           TO      L2
                   MOVE    ","         TO      POT1-REC (L2:1)

                   ADD     1           TO      L2
                   MOVE    WK-REC (1:L1) TO    POT1-REC (L2:L1)

                   ADD     L1          TO      L2
                   MOVE    ","         TO      POT1-REC (L2:1)

                   ADD     1           TO      L2
                   MOVE    ",0,"       TO      POT1-REC (L2:3)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

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
