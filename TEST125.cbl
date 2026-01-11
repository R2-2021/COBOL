      *    *** TEST109 用 変換データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST125.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST109.SORT.POT1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST109.PIN3 追加データ
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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST125 ".

           03  WK-PIN1-F-NAME  PIC  X(064) VALUE
               "TEST109.SORT.POT1".
           03  WK-POT1-F-NAME  PIC  X(064) VALUE 
               "TEST125.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-FILE-NAME    PIC  X(064) VALUE SPACE.

           03  WK-ITEM1        PIC  X(030) VALUE SPACE.
           03  WK-ITEM2        PIC  X(030) VALUE SPACE.
           03  WK-ITEM3        PIC  X(030) VALUE SPACE.
           03  WK-ITEM4        PIC  X(030) VALUE SPACE.

           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM3-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM4-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-KEY.
             05  WK-OKEY.
               07  WK-OITEM4   PIC  X(030) VALUE LOW-VALUE.
             05  WK-NKEY.
               07  WK-NITEM4   PIC  X(030) VALUE LOW-VALUE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE


      *    *** 漢字スペースは除外
                   IF      WK-ITEM4 (1:6) NOT = X"E38080E38080"
                       AND WK-OKEY NOT =       WK-NKEY
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

           MOVE    WK-NKEY     TO      WK-OKEY

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-ITEM1    COUNT WK-ITEM1-LEN
                           WK-ITEM2    COUNT WK-ITEM2-LEN
                           WK-ITEM3    COUNT WK-ITEM3-LEN
                           WK-ITEM4    COUNT WK-ITEM4-LEN
           
                   MOVE    WK-ITEM4    TO      WK-NITEM4
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I

           MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO
                   POT1-REC (I:WK-ITEM1-LEN)
           ADD     WK-ITEM1-LEN TO     I

           MOVE    ","         TO      POT1-REC (I:1)
           ADD     1           TO      I

           MOVE    WK-ITEM4 (1:WK-ITEM4-LEN) TO
                   POT1-REC (I:WK-ITEM4-LEN)
           ADD     WK-ITEM4-LEN TO     I

           MOVE    ","         TO      POT1-REC (I:1)
           ADD     1           TO      I

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
