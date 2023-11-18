      *    *** TEST70用 PIN5 検索データ作成

      *    *** JOB=C.TEST108.bat

      *    *** TEST108
      *    ***   |
      *    *** TEST109
      *    ***   |
      *    *** COBSORT COBSORT.T015.PRM1
      *    ***   |
      *    *** TEST110
      *    ***   |
      *    *** TEST70
      *    ***   |
      *    *** TEST53
      *    ***   |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST110.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 検索 CSV データ SORT 済
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 CSV データ #NNN 付加
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST110 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST109.SORT.POT1".
      *    *** YouTube アニメタイトル順データ
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST110.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-ITEM1   PIC  X(100) VALUE SPACE.
           03  WK-PIN1-ITEM2   PIC  X(100) VALUE SPACE.
           03  WK-PIN1-ITEM3   PIC  X(100) VALUE SPACE.
           03  WK-PIN1-ITEM4   PIC  X(100) VALUE SPACE.

           03  WK-PIN1-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-ITEM2-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-ITEM3-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-ITEM4-LEN BINARY-LONG SYNC VALUE ZERO.

           03  WK-NO.
             05                PIC  X(001) VALUE "#".
             05  WK-NO-SEQ     PIC  9(003) VALUE ZERO.
             05                PIC  X(001) VALUE ".".
             05                PIC  X(003) VALUE X"E38080".
             05  WK-NO-ID      PIC  X(003) VALUE SPACE.

           03  WK-NO2.
      *    *** ジャパリ
             05                PIC  X(012) VALUE
                 X"E382B8E383A3E38391E383AA".
             05  WK-NO-ID2     PIC  X(003) VALUE SPACE.

           03  WK-OLD-KEY.
             05  WK-OLD-ID     PIC  X(003) VALUE LOW-VALUE.
           03  WK-NEW-KEY.
             05  WK-NEW-ID     PIC  X(003) VALUE LOW-VALUE.

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

      *    *** PIN1
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      WK-OLD-KEY  NOT =   WK-NEW-KEY

      *    *** WRITE POT1 WK-NO
                           PERFORM S110-10     THRU    S110-EX
                   END-IF

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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    WK-NEW-KEY  TO      WK-OLD-KEY
           MOVE    SPACE       TO      WK-PIN1-ITEM1
                                       WK-PIN1-ITEM2
                                       WK-PIN1-ITEM3
                                       WK-PIN1-ITEM4
           MOVE    ZERO        TO      WK-PIN1-ITEM1-LEN
                                       WK-PIN1-ITEM2-LEN
                                       WK-PIN1-ITEM3-LEN
                                       WK-PIN1-ITEM4-LEN

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-PIN1-ITEM1 COUNT WK-PIN1-ITEM1-LEN
                           WK-PIN1-ITEM2 COUNT WK-PIN1-ITEM2-LEN
                           WK-PIN1-ITEM3 COUNT WK-PIN1-ITEM3-LEN
                           WK-PIN1-ITEM4 COUNT WK-PIN1-ITEM4-LEN
                   MOVE    WK-PIN1-ITEM4 (1:3) TO      WK-NEW-KEY
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               WK-NEW-KEY
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

           WRITE   POT1-REC    FROM    PIN1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 WK-NO 
       S110-10.

      *     ADD     1           TO      WK-NO-SEQ
      *     MOVE    WK-NEW-KEY  TO      WK-NO-ID
           MOVE    WK-NEW-KEY  TO      WK-NO-ID2

           WRITE   POT1-REC    FROM    WK-NO2
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

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
