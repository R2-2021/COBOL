      *    *** TEST53 音韓、女性韓国アイドルグループ データ編集

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST137.

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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST137 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST53_girl_kr3.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST53_girl_kr2.PIN1".

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
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
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

                   IF    ( PIN1-REC (1:1) =    "*" OR "%" OR SPACE )
      *    *** ジャパリ
                        OR PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
      *    *** WRITE POT1
                           PERFORM S100-10     THRU    S100-EX
                   END-IF

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

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      L1

      *    *** 2NE1,https://www.nehannn.com/group-detail/91.html,65
      *    ***  |
      *    ***  v
      *    *** 2NE1,https://www.nehannn.com/img/group/91/91-0.jpg,65


           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   EVALUATE TRUE

                       WHEN PIN1-REC (I:13) =  "group-detail/"

                           ADD     1           TO      L1
                           MOVE    "img/group/" TO     POT1-REC (L1:10)
                           ADD     9           TO      L1

                           COMPUTE I2 = I + 13
                           MOVE    ZERO        TO      I3
                           PERFORM VARYING I FROM I2 BY 1
                                   UNTIL PIN1-REC (I:1) = "."
                                   ADD     1           TO      I3
                                   ADD     1           TO      L1
                                   MOVE    PIN1-REC (I:1) TO
                                           POT1-REC (L1:1)
                           END-PERFORM

      *    *** html の lの位置へ
                           ADD     4           TO      I

                           ADD     1           TO      L1
                           MOVE    "/"         TO      POT1-REC (L1:1)

                           ADD     1           TO      L1
                           MOVE    PIN1-REC (I2:I3) TO POT1-REC (L1:I3)
                           COMPUTE L1 = L1 + I3 - 1

                           ADD     1           TO      L1
                           MOVE    "-0.jpg"    TO      POT1-REC (L1:6)
                           ADD     5           TO      L1

                       WHEN OTHER
                           ADD     1           TO      L1
                           MOVE    PIN1-REC (I:1) TO   POT1-REC (L1:1)

                   END-EVALUATE
           END-PERFORM

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

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
