      *    *** COBOL SOURCE LIST,XREF LIST 出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBXREF.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

      *    *** >DIR *.cbl より
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03                  PIC  X(035).
           03  PIN1-COM        PIC  X(001).
           03  PIN1-PGM-ID     PIC  X(050).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBXREF".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBXREF.PIN1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-I-MAX        BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT       PIC  X(001) VALUE SPACE.

       01  WK-LISTING-AREA.
           03  WK-LISTING.
             05  WK-LISTING-SRC  PIC X(001) VALUE " ".
             05  WK-LISTING-XREF PIC X(001) VALUE "X".
             05  WK-LISTING-FILE PIC X(256) VALUE
      *        "C:\Users\xxxx\Documents\COBOL\COBXREF.cbl".
              "COBXREF.CBL".
             05  WK-LISTING-REP-FILE PIC X(256) VALUE SPACE.
      *        "C:\Users\xxxx\Documents\COBOL\COBXREF.lst".
             05  WK-LISTING-POT1-ID.
               07  WK-LISTING-POT1-OPEN  PIC X(001) VALUE "O".
               07  WK-LISTING-POT1-CLOSE PIC X(001) VALUE "C".

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUE.
           05  PIC-X           PIC X VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA.
             05  TBL01-PGM-ID  OCCURS 1000
                               PIC  X(050) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PIN1 36,1=* はコメント扱い、スキップする
                   IF      PIN1-COM (1:1) =    "*"
                           CONTINUE
                   ELSE
      *    *** PIN1 TBL SET
                           PERFORM S030-10     THRU    S030-EX
                   END-IF
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " 1.PROGRAM-ID ACCEPT "
                   DISPLAY WK-PGM-NAME " 2.PIN1 INPUT (COBXREF.PIN1)"
                   DISPLAY WK-PGM-NAME " 1 OR 2 INPUT"
                   
                   ACCEPT  WK-ACCEPT
                   DISPLAY WK-PGM-NAME " INPUT=" WK-ACCEPT
                           " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM

           IF      WK-I-MAX    =       ZERO
                OR WK-ACCEPT   =       "1"

      *    *** PIN1 ない時、パラメータACCEPT
                   MOVE    "OC"        TO      WK-LISTING-POT1-ID
      *    *** XREF PARA INPUT & LISTING
                   PERFORM S100-10     THRU    S100-EX
           ELSE
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-I-MAX
                       IF      I           =       1
                           MOVE    "O"         TO
                                   WK-LISTING-POT1-OPEN
                       ELSE
                           MOVE    SPACE       TO
                                   WK-LISTING-POT1-OPEN
                       END-IF
                       IF      I           =       WK-I-MAX
                           MOVE    "C"         TO
                                   WK-LISTING-POT1-CLOSE
                       ELSE
                           MOVE    SPACE       TO
                                   WK-LISTING-POT1-CLOSE
                       END-IF
      *    *** XREF PARA INPUT & LISTING 2
                       PERFORM S200-10     THRU    S200-EX
                   END-PERFORM
           END-IF

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

      *    *** FILEDUMP OPEN
           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL01-AREA

      *    *** FILEDUMP 項目出力(ID="X") 
      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-DATA2

      *     CALL    "COBDUMP"   USING   TBL01-AREA
      *                                 WFD-LEN
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

      *    *** PIN1 TBL SET
       S030-10.

           ADD     1           TO      I
           IF      I           >       1000
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    PIN1-PGM-ID TO      TBL01-PGM-ID (I)
           MOVE    I           TO      WK-I-MAX
           .
       S030-EX.
           EXIT.

      *    *** XREF PARA INPUT & LISTING
       S100-10.

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT COBOL SOURCE NAME"
                   ACCEPT  WK-LISTING-FILE
                   DISPLAY WK-PGM-NAME " SOURCE NAME=" 
                           WK-LISTING-FILE (1:50)
                           " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM

           MOVE    "X"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y" OR "N"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME
                           " SOURCE LIST N(出力無) OR Y(出力)="
                   ACCEPT  SW-YES
           END-PERFORM
           IF      SW-YES       =      "Y"
                   MOVE    "X"         TO      WK-LISTING-SRC
           ELSE
                   MOVE    SPACE       TO      WK-LISTING-SRC
           END-IF

           MOVE    "X"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y" OR "N"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME
                           " XREF   LIST N(出力無) OR Y(出力)="
                   ACCEPT  SW-YES
           END-PERFORM
           IF      SW-YES       =      "Y"
                   MOVE    "X"         TO      WK-LISTING-XREF
           ELSE
                   MOVE    SPACE       TO      WK-LISTING-XREF
           END-IF

           CALL    "LISTING"   USING   WK-LISTING-SRC
                                       WK-LISTING-XREF
                                       WK-LISTING-FILE
                                       WK-LISTING-REP-FILE
                                       WK-LISTING-POT1-ID
           .
       S100-EX.
           EXIT.

      *    *** XREF PARA INPUT & LISTING 2
       S200-10.

           MOVE    SPACE       TO      WK-LISTING-SRC
           MOVE    "X"         TO      WK-LISTING-XREF
           MOVE    TBL01-PGM-ID (I) TO WK-LISTING-FILE

           DISPLAY "CALL LISTING " WK-LISTING-FILE (1:20)

           CALL    "LISTING"   USING   WK-LISTING-SRC
                                       WK-LISTING-XREF
                                       WK-LISTING-FILE
                                       WK-LISTING-REP-FILE
                                       WK-LISTING-POT1-ID
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

      *    *** FILEDUMP CLOSE
           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL01-AREA

           DISPLAY WK-PGM-NAME
                   " LISTING NAME =" WK-LISTING-REP-FILE(1:50)

           DISPLAY WK-PGM-NAME
                   " DEFREF  NAME =XREF.defref"
           DISPLAY WK-PGM-NAME " END"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
