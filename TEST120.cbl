      *    *** 中国大陸女性アーティスト一覧等、漢字ピンイン抽出
      *    *** 
      *    *** TEST118
      *    ***    |
      *    *** TEST120 <--
      *    ***    |      |
      *    *** TEST119 ---
      *    ***    |
      *    *** TEST53
      *    ***    |
      *    *** TEST54
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST120.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST119.POT4
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST120.POT1
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(10000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST120 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST119.POT4".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST120.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I              BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE 1.
           03  I3-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-HIT          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** 漢字ダブリカット
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** WRITE POT1
           PERFORM S110-10     THRU    S110-EX

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
                   OUTPUT      POT1-F

           MOVE    SPACE       TO      POT1-REC

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

      *    *** 漢字ダブリカット
       S100-10.

           PERFORM VARYING I1 FROM 1 BY 3
                   UNTIL I1 > WK-PIN1-LEN

                   MOVE    "N"         TO      SW-HIT
                   PERFORM VARYING I2 FROM 1 BY 3
                           UNTIL I2 > I3-MAX
                              OR SW-HIT = "Y"
                           IF      PIN1-REC (I1:3) =   POT1-REC (I2:3)
                                   MOVE    "Y"         TO      SW-HIT
                           END-IF
                   END-PERFORM

                   IF      SW-HIT      =       "N"
                           MOVE    PIN1-REC (I1:3) TO  POT1-REC (I3:3)
                           MOVE    I3          TO      I3-MAX

                           ADD     3           TO      I3
                           IF      I3          >       10000
                                   DISPLAY WK-PGM-NAME 
                                           " POT1-REC SET OVER"
                                   STOP RUN
                           END-IF
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
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
