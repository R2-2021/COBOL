      *    *** FUNCTION TEST

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST07.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *     ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(007).
           03  POT1-X          PIC  X(001).
           03  POT1-AST        PIC  X(001).
           03  POT1-0D0A       PIC  X(002).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(007).
           03  POT2-X          PIC  X(001).
           03  POT2-AST        PIC  X(001).
           03  POT2-0D0A       PIC  X(002).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST07  ".

           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST07.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST07.POT2".

           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-DATA1
             05 WK-DATA11      PIC  X(001) VALUE SPACE.
             05 WK-DATA12      PIC  X(001) VALUE "*".
           03  WK-DATA2        PIC S9(9)V9(5) VALUE ZERO.
           03  WK-DATA3        PIC S9(009) VALUE 9012.
           03  WK-PAR3         PIC S9(005) VALUE ZERO.
           03  WK-PAR4         PIC S9(005) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** CHECK
           PERFORM S100-10     THRU    S100-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           .
       S010-EX.
           EXIT.

      *    *** CHECK
       S100-10.

           MOVE    FUNCTION RANDOM TO  WK-DATA2 
           DISPLAY WK-DATA2

           MOVE    FUNCTION PI     TO  WK-DATA2 
           DISPLAY WK-DATA2

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 256

                   MOVE    FUNCTION CHAR(I)     TO  WK-DATA11
      *             DISPLAY I " " WK-DATA1
      *    *** >= X"81" AND <= X"9F"
      *    *** >= X"E0" AND <= X"FC"
      *    *** 下記の範囲DISPLAY表示されない、COBDUMPは出る
                  IF ( I >= 130 AND I <= 160 ) OR
                     ( I >= 225 AND I <= 253 )
                      CONTINUE
                  ELSE
                      DISPLAY I " " WK-DATA1
                      CALL    "COBDUMP"   USING WK-DATA1
                  END-IF 
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
                                       POT2-REC
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL   I > 255
                   MOVE    I           TO      PIC-Halfword
                   MOVE    PIC-X       TO      POT1-X
                                               POT2-X
                   MOVE    "*"         TO      POT1-AST
                                               POT2-AST
                   MOVE    X"0D0A"     TO      POT1-0D0A
                                               POT2-0D0A

                   WRITE   POT1-REC
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT1-CNT
                                               WK-POT2-CNT

                   MOVE    "P"         TO      WFD-ID
                   MOVE    1           TO      WFD-SU
                   MOVE    WK-POT1-CNT TO      WFD-SEQ
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC

                   MOVE    "X"         TO      WFD-ID
                   MOVE    2           TO      WFD-SU
                   MOVE    "ASCII"     TO      WFD-ITEM
                   MOVE    WK-POT2-CNT TO      WFD-SEQ
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC
           END-PERFORM

           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           .
       S900-EX.
           EXIT.
