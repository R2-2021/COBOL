      *    *** UTF-8 漢字データ出力
      *    *** 
      *    *** Unicodeのコードポイント                UTF-８
      *    *** 0x0000～0x007f (ASCII)                 0x00～0x7f
      *    *** 0x0080～0x07ff (各国アルファベット)    0xc080～0xdfbf
      *    *** 0x0800～0xffff (インド系諸文字、句読点、
      *    *** 学術記号、絵文字、東アジアの
      *    *** 諸字、全角、半角形)                    0xe08080～0xefbfbf

      *    *** 0xc0=192,0x80=

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST46.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
        SELECT PIN1-F          ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** UTF8 漢字コード
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** UTF8 漢字コード * 16
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-I1L        PIC  X(001).
           03  POT1-I1R        PIC  X(001).
           03  POT1-I2L        PIC  X(001).
           03  POT1-I2R        PIC  X(001).
           03  POT1-I3L        PIC  X(001).
           03  POT1-I3R        PIC  X(001).
           03  POT1-I1         PIC  X(001).
           03  POT1-I2         PIC  X(001).
           03  POT1-I3         PIC  X(001).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-I1L        PIC  X(001).
           03  POT2-I1R        PIC  X(001).
           03  POT2-I2L        PIC  X(001).
           03  POT2-I2R        PIC  X(001).
           03  POT2-I1         PIC  X(001).
           03  POT2-I2         PIC  X(001).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03                  OCCURS 16.
             05                PIC  X(001).
             05  POT3-I1L      PIC  X(001).
             05  POT3-I1R      PIC  X(001).
             05  POT3-I2L      PIC  X(001).
             05  POT3-I2R      PIC  X(001).
             05  POT3-I3L      PIC  X(001).
             05  POT3-I3R      PIC  X(001).
             05  POT3-I1       PIC  X(001).
             05  POT3-I2       PIC  X(001).
             05  POT3-I3       PIC  X(001).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST46  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST46.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST46.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST46.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST46.POT3".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-00-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"00".
           03  WK-00           REDEFINES   WK-00-X
                               PIC  9(004) COMP-X.

           03  WK-08-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"08".
           03  WK-08           REDEFINES   WK-08-X
                               PIC  9(004) COMP-X.

           03  WK-07-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"07".
           03  WK-07           REDEFINES   WK-07-X
                               PIC  9(004) COMP-X.

           03  WK-0C-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"0C".
           03  WK-0C           REDEFINES   WK-0C-X
                               PIC  9(004) COMP-X.

           03  WK-80-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"80".
           03  WK-80           REDEFINES   WK-80-X
                               PIC  9(004) COMP-X.

           03  WK-BF-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"BF".
           03  WK-BF           REDEFINES   WK-BF-X
                               PIC  9(004) COMP-X.

           03  WK-DF-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"DF".
           03  WK-DF           REDEFINES   WK-DF-X
                               PIC  9(004) COMP-X.

           03  WK-E0-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"E0".
           03  WK-E0           REDEFINES   WK-E0-X
                               PIC  9(004) COMP-X.

           03  WK-EF-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"EF".
           03  WK-EF           REDEFINES   WK-EF-X
                               PIC  9(004) COMP-X.

           03  WK-FF-X.
             05                PIC  X(001) VALUE LOW-VALUE.
             05                PIC  X(001) VALUE X"FF".
           03  WK-FF           REDEFINES   WK-FF-X
                               PIC  9(004) COMP-X.

      *    *** 0xe08080～0xefbfbf

           03  WK-PIC.
             05                PIC  X(001) VALUE LOW-VALUE.
             05  WK-PIC-X      PIC  X(001).
           03  WK-PIC-HALF     REDEFINES WK-PIC 
                               PIC  9(004) COMP-X.

           01  WK-HEX-DS       VALUE '0123456789ABCDEF'.
             03  WK-HEX-D      OCCURS 16
                               PIC  X(001).

           03  WK-L            BINARY-LONG SYNC VALUE ZERO.
           03  WK-R            BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-PUT          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** UTF-8 DATA CREATE 3 BYTE
           PERFORM S100-10     THRU    S100-EX

      *    *** UTF-8 DATA CREATE 2 BYTE
      *     PERFORM S200-10     THRU    S200-EX

      *     PERFORM S200-10     THRU    S200-EX
      *             UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** UTF-8 DATA CREATE
       S100-10.

      *    *** 0xe08080～0xefbfbf UTF-8 3バイト系

           MOVE    SPACE       TO      POT3-REC

           PERFORM VARYING I1 FROM WK-E0 BY 1
                   UNTIL I1 > WK-EF

               PERFORM VARYING I2 FROM WK-80 BY 1
                       UNTIL I2 > WK-BF

                   PERFORM VARYING I3 FROM WK-80 BY 1
                           UNTIL I3 > WK-BF

                       ADD     1           TO      J

                       MOVE    SPACE       TO      POT1-REC

                       MOVE    I1          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT1-I1
                                                   POT3-I1 (J)

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT1-I1L
                                                   POT3-I1L (J)
                       MOVE WK-HEX-D(WK-R) TO      POT1-I1R
                                                   POT3-I1R (J)



                       MOVE    I2          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT1-I2
                                                   POT3-I2 (J)

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT1-I2L
                                                   POT3-I2L (J)
                       MOVE WK-HEX-D(WK-R) TO      POT1-I2R
                                                   POT3-I2R (J)


                       MOVE    I3          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT1-I3
                                                   POT3-I3 (J)

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT1-I3L
                                                   POT3-I3L (J)
                       MOVE WK-HEX-D(WK-R) TO      POT1-I3R
                                                   POT3-I3R (J)

                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       IF      J           =       16
                               WRITE   POT3-REC
                               ADD     1           TO      WK-POT3-CNT

                               MOVE    ZERO        TO      J
                               MOVE    SPACE       TO      POT3-REC
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           IF      J           >       16
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF

      *    IF      WK-PIN1-CNT >= 1230 AND <= 1250
      *         DISPLAY " " 
      *         DISPLAY WK-PIN1-CNT " " WK-PIN1-LEN " " WK-POT1-CNT
      *         DISPLAY PIN1-REC(1:80)
      *         DISPLAY POT1-REC(1:80)
      *     END-IF

      *     IF WK-PIN1-CNT >= 0 AND <= 121
      *     DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT " " PIN1-REC (1:20)

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE     WK-PIN1-CNT TO     WFD-SEQ
      *     MOVE    200         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC
      *                                 WFD-LEN
           .
       S100-EX.
           EXIT.

       S200-10.


      *    *** 0x0080～0x07ff (各国アルファベット)    0xc080～0xdfbf

           PERFORM VARYING I1 FROM WK-00 BY 1
                   UNTIL I1 > WK-07

               PERFORM VARYING I2 FROM WK-80 BY 1
                       UNTIL I2 > WK-FF

                       MOVE    SPACE       TO      POT2-REC

                       MOVE    I1          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I1

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I1L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I1R



                       MOVE    I2          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I2

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I2L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I2R

                       WRITE   POT2-REC
                       ADD     1           TO      WK-POT2-CNT
               END-PERFORM
           END-PERFORM

      *    *** 0x0800～0xffff (インド系諸文字、句読点、
           PERFORM VARYING I1 FROM WK-08 BY 1
                   UNTIL I1 > WK-FF

               PERFORM VARYING I2 FROM WK-00 BY 1
                       UNTIL I2 > WK-FF

                       MOVE    SPACE       TO      POT2-REC

                       MOVE    I1          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I1

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I1L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I1R



                       MOVE    I2          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I2

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I2L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I2R

                       WRITE   POT2-REC
                       ADD     1           TO      WK-POT2-CNT
               END-PERFORM
           END-PERFORM

           IF      SW-PUT      =       "N"
      *    ***    0xc080～0xdfbf
             PERFORM VARYING I1 FROM WK-0C BY 1
                   UNTIL I1 > WK-DF

               PERFORM VARYING I2 FROM WK-80 BY 1
                       UNTIL I2 > WK-BF

                       MOVE    SPACE       TO      POT2-REC

                       MOVE    I1          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I1

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I1L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I1R



                       MOVE    I2          TO      WK-PIC-HALF
                       MOVE    WK-PIC-X    TO      POT2-I2

                       DIVIDE  WK-PIC-HALF BY 16
                               GIVING    WK-L
                               REMAINDER WK-R

                       ADD     1           TO      WK-L 
                                                   WK-R

                       MOVE WK-HEX-D(WK-L) TO      POT2-I2L
                       MOVE WK-HEX-D(WK-R) TO      POT2-I2R

                       WRITE   POT2-REC
                       ADD     1           TO      WK-POT2-CNT
               END-PERFORM
             END-PERFORM
           END-IF
           .
       S200-EX.
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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
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
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 ｹﾝｽｳ = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
