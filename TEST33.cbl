      *    *** テーブル ロジックでソート

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST33.

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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST33  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST33.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST33.POT1".

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
           03  WK-TBL-MAX      BINARY-LONG SYNC VALUE 10000.

           03  WK-SU1          PIC  9(004) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  TBL-AREA.
      *    *** SW-SET = "0" のやり方で、
      *    ***  10,000件   3.58秒
      *    *** 100,000件 261.53秒

      *    *** SW-SET = "1" のやり方は、遅かった
      *    ***   10,000件   5.22秒=>3.60秒(比較テーブル同士からＷＫにしたら 
      *    ***  100,000件 590.00秒位かかる
           03  TBL01-AREA      OCCURS 10000.
             05  TBL01-I       BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-KEY     PIC X(010) VALUE SPACE.
             05  TBL01-DATA    PIC X(070) VALUE SPACE.

           03  TBL02-AREA      OCCURS 10000.
             05  TBL02-I       BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-KEY     PIC X(010) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG VALUE 1.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
      *    *** SW-SET = "0" 早い
      *    ***          "1"　遅い
           03  SW-SET          PIC  X(001) VALUE "0".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           IF      SW-SET      =         "0"

                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE 
      *    *** TBL SET1
                           PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM
      *    *** WRITE POT1 1
                   PERFORM S130-10     THRU    S130-EX
           ELSE

                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE 
      *    *** TBL SET2-1 
                           PERFORM S110-10     THRU    S110-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

                   PERFORM VARYING I1 FROM 1 BY 1
                           UNTIL I1 > I-MAX
      *    *** TBL SET2-2 
                           PERFORM S120-10     THRU    S120-EX
                   END-PERFORM
      *    *** WRITE POT1 2
                   PERFORM S140-10     THRU    S140-EX
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

      *    *** TBL SET1
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
                   PERFORM VARYING J FROM 1 BY 1
                       UNTIL J > I-MAX
      *                 MOVE    TBL01-KEY (J) TO      WK-KEY1
      *                 IF      WK-KEY1       >       PIN1-KEY
      *    *** 下記の方が早い
                       IF      TBL01-KEY (J) >       PIN1-KEY

                           MOVE    TBL01-KEY (J) TO    WK-KEY
                           MOVE    TBL01-DATA(J) TO    WK-DATA

                           MOVE    PIN1-KEY    TO      TBL01-KEY (J)
                           MOVE    PIN1-DATA   TO      TBL01-DATA(J)

                           MOVE    WK-KEY      TO      PIN1-KEY
                           MOVE    WK-DATA     TO      PIN1-DATA
                       ELSE
                           CONTINUE
                       END-IF
                   END-PERFORM

                   MOVE    PIN1-KEY    TO      TBL01-KEY  (I)
                   MOVE    PIN1-DATA   TO      TBL01-DATA (I)
               END-IF

               MOVE    I           TO      I-MAX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** TBL SET2-1
       S110-10.

           ADD     1           TO      I

           IF      I           >       WK-TBL-MAX
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           ELSE
                   MOVE    PIN1-KEY    TO      TBL01-KEY  (I)
                   MOVE    PIN1-DATA   TO      TBL01-DATA (I)
                   MOVE    I           TO      TBL01-I    (I)

                   MOVE    I           TO      I-MAX
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** TBL SET2-2
       S120-10.

           ADD     1           TO      I2

           MOVE    I2          TO      WK-SU1

           IF      WK-SU1      =       ZERO
                   DISPLAY I2
                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           END-IF

           IF      I2          >       WK-TBL-MAX
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           ELSE
               IF      I2          =       1
                   MOVE    TBL01-KEY (I1)  TO  TBL02-KEY (I2)
                   MOVE    TBL01-I   (I1)  TO  TBL02-I   (I2)
               ELSE
                   PERFORM VARYING J FROM 1 BY 1
                       UNTIL J > I-MAX
      *    *** 下記にすることで、１万件　5.45＝＞3.60秒になった
      *                 IF      TBL02-KEY  (J) >        TBL01-KEY (I1)
                       MOVE    TBL02-KEY (J) TO      WK-KEY2
                       MOVE    TBL01-KEY (I1) TO     WK-KEY1

                      IF      WK-KEY2      >        WK-KEY1

                           MOVE    TBL02-KEY (J) TO    WK-KEY
                           MOVE    TBL02-I   (J) TO    WK-I

                           MOVE    TBL01-KEY (I1) TO   TBL02-KEY (J)
                           MOVE    TBL01-I   (I1) TO   TBL02-I   (J)

                           MOVE    WK-KEY      TO      TBL01-KEY (I1)
                           MOVE    WK-I        TO      TBL01-I   (I1)
                       ELSE
                           CONTINUE
                       END-IF
                   END-PERFORM

                   MOVE    TBL01-KEY (I1) TO   TBL02-KEY (I2)
                   MOVE    TBL01-I   (I1) TO   TBL02-I   (I2)
               END-IF
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1 1
       S130-10.

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
       S130-EX.
           EXIT.

      *    *** WRITE POT1 2
       S140-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   MOVE    TBL02-KEY  (I) TO   POT1-KEY
                   MOVE    TBL02-I    (I) TO   I1
                   MOVE    TBL01-DATA (I1) TO  POT1-DATA

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
       S140-EX.
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
