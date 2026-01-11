      *    *** ÉfÅ[É^ïœä∑ 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST121.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".

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
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(2000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC            PIC  X(2000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST121 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST121.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST121.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC  --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC  --,---,---,--9 VALUE ZERO.

           03  WK-ITEM1        PIC  X(200) VALUE SPACE.
           03  WK-ITEM2        PIC  X(200) VALUE SPACE.
           03  WK-ITEM3        PIC  X(200) VALUE SPACE.

           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM3-LEN    BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

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

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           OPEN    INPUT  PIN1-F
                   OUTPUT POT1-F

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
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
                                       WK-ITEM3
           MOVE    ZERO        TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN
                                       WK-ITEM3-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
      *    *** à 
                           DELIMITED BY X"E4BD8D" 
      *    *** ï]âø
                                     OR X"E8A995E4BEA1"
                           INTO
                           WK-ITEM1    COUNT WK-ITEM1-LEN
                           WK-ITEM2    COUNT WK-ITEM2-LEN
                           WK-ITEM3    COUNT WK-ITEM3-LEN
           END-READ

           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    WK-ITEM2    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 åèêî = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 åèêî = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
