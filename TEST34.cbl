      *    *** テーブル ロジックでソート
      *    *** パターン２

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST34.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT サンプルデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORT後 サンプルデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-KEY        PIC  X(010).
           03  PIN1-DATA       PIC  X(070).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-KEY        PIC  X(010).
           03  POT1-DATA       PIC  X(070).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST34  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST33.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST34.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-I            BINARY-LONG SYNC VALUE ZERO.
           03  WK-KEY          PIC  X(010) VALUE LOW-VALUE.
           03  WK-KEY1         PIC  X(010) VALUE LOW-VALUE.
           03  WK-KEY2         PIC  X(010) VALUE LOW-VALUE.
           03  WK-DATA         PIC  X(070) VALUE SPACE.
           03  WK-DATA2        PIC  X(070) VALUE SPACE.
           03  WK-TBL-MAX      BINARY-LONG SYNC VALUE 10000.

           03  WK-SU1          PIC  9(004) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  TBL-AREA.
      *    *** SW-SET1 = "0" のやり方で、
      *    ***  10,000件   3.58秒
      *    *** 100,000件 261.53秒 => 145.12秒 TEST33 KEY ランダムのとき
      *    ***                       133.13秒 KEY のみの時
      *    *** 100,000件 261.53秒 => 247.67秒 TEST33 KEY 昇順のとき
      *    *** SW-SET1 = "1" のやり方で、
      *    *** 100,000件 261.53秒 => 133.99秒 TEST33 KEY ランダムのとき
      *    *** 変わらなった
           03  TBL01-AREA.
             05  FILLER        OCCURS 10000.
      *    *** 80 ﾊﾞｲﾄ
               07  TBL01-KEY   PIC X(010) VALUE SPACE.
               07  TBL01-DATA  PIC X(070) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-80          BINARY-LONG VALUE 80.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  J4              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SET2         PIC  X(001) VALUE "0".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** TBL SET
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

      *    *** TBL SET
       S100-10.
           ADD     1           TO      I

           MOVE    I           TO      WK-SU1

           IF      WK-SU1      =       ZERO
               DISPLAY I
               MOVE    "L"         TO      WDT-DATE-TIME-ID
               CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           END-IF

           IF      I           >       WK-TBL-MAX
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           ELSE
               IF      I           =       1
                   MOVE    PIN1-KEY    TO      TBL01-KEY  (I)
                   MOVE    PIN1-DATA   TO      TBL01-DATA (I)
               ELSE
                   MOVE    ZERO        TO      SW-SET2
                   PERFORM VARYING J FROM 1 BY 1
                       UNTIL J > I-MAX
                     IF      TBL01-KEY (I-MAX) <   PIN1-KEY
                             CONTINUE
                     ELSE
                       IF      TBL01-KEY (J) >       PIN1-KEY

      *                     MOVE    TBL01-KEY (J) TO    WK-KEY
      *                     MOVE    TBL01-DATA(J) TO    WK-DATA

      *                     MOVE    PIN1-KEY    TO      TBL01-KEY (J)
      *                     MOVE    PIN1-DATA   TO      TBL01-DATA(J)

      *                     MOVE    WK-KEY      TO      PIN1-KEY
      *                     MOVE    WK-DATA     TO      PIN1-DATA

                           COMPUTE J2 = ((J - 1) * CNS-80) + 1
                           COMPUTE J3 = (I-MAX - J + 1) * CNS-80
                           MOVE    TBL01-AREA (J2:J3) 
                                TO TBL01-AREA (J2+CNS-80 : J3)

                           MOVE    PIN1-KEY    TO      TBL01-KEY (J)
                           MOVE    PIN1-DATA   TO      TBL01-DATA(J)

      *                     MOVE    TBL01-KEY (I-MAX - 1) TO    PIN1-KEY
      *                     MOVE    TBL01-DATA(I-MAX - 1) TO    PIN1-DATA

                           MOVE    I-MAX       TO      J
                           MOVE    "1"         TO      SW-SET2
                       ELSE
                           CONTINUE
                       END-IF
                     END-IF
                   END-PERFORM
                   IF       SW-SET2    =       "1"
                       CONTINUE
                   ELSE
                       MOVE    PIN1-KEY    TO      TBL01-KEY  (I)
                       MOVE    PIN1-DATA   TO      TBL01-DATA (I)
                   END-IF
      *    *** 
      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 TBL01-AREA(1:800)
               END-IF

               MOVE    I            TO      I-MAX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   MOVE    TBL01-KEY  (I) TO   POT1-KEY
                   MOVE    TBL01-DATA (I) TO   POT1-DATA

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-PERFORM
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
