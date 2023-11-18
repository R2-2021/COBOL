      *    *** FILEDTCR で作成したデータチェックする
      *    *** ランダム数均一か？

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST44.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** FILEDTCR データ
      *    *** C.FILEDTCR.T005.bat
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 集計データ１
      *    *** C.FILEITEM.T022.bat
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 集計データ２
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-TBL       OCCURS 10. 
             05  PIN1-I1      PIC S9(009).
             05               PIC  X(001).
      *     03                 PIC  X(9900).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-TBL       OCCURS 11. 
             05  POT1-CNT     PIC  9(009).
             05               PIC  X(001).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-TBL       OCCURS 11. 
             05  POT2-CNT     PIC  9(009).
             05               PIC  X(001).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST44  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "FILEDTCR.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST44.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST44.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-I1      PIC  X(1000) VALUE SPACE.
           03  WK-UNST-PTR     BINARY-LONG SYNC VALUE ZERO.
           03  WK-STO-PTR      BINARY-LONG SYNC VALUE ZERO.
           03  WK-DELI         PIC  X(001) VALUE SPACE.
           03  WK-NUM-ITEM     PIC  X(010) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  TBL-AREA.
      *    *** １１個目は縦、横の平均値
           03  TBL01-AREA      OCCURS 11.
             05  TBL01-AREA2   OCCURS 11.
               07  TBL01-CNT   BINARY-LONG SYNC VALUE ZERO.

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

           PERFORM UNTIL   WK-PIN1-EOF = HIGH-VALUE

      *    *** 集計, (11 NN) (NN 11) は合計値
                  PERFORM S100-10      THRU    S100-EX

      *    *** READ PIN1
                  PERFORM S020-10      THRU    S020-EX
           END-PERFORM

      *    *** 平均値　出力 WRITE POT1,POT2
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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
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

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN

           .
       S020-EX.
           EXIT.

      *    *** 集計, (11 NN) (NN 11) は合計値
       S100-10.
           MOVE    1           TO      WK-UNST-PTR
                                       WK-STO-PTR
                                       J
           MOVE    ZERO        TO      I
           MOVE    SPACE       TO      POT1-REC
                                       WK-PIN1-I1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 10
              IF      I           =       1
                   EVALUATE TRUE 
                       WHEN 
                      (PIN1-I1 (I) >=      0 AND PIN1-I1 (I) < 10000 )
                   OR (PIN1-I1 (I) >= -10000 AND PIN1-I1 (I) < 0 )
                            ADD     1           TO      TBL01-CNT (I 1)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  1)
                                                       TBL01-CNT (11 11)
                       WHEN 
                       (PIN1-I1 (I) >=  10000 AND PIN1-I1 (I) <  20000)
                    OR (PIN1-I1 (I) >= -20000 AND PIN1-I1 (I) < -10000)
                            ADD     1           TO      TBL01-CNT (I 2)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  2)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  20000 AND PIN1-I1 (I) <  30000)
                    OR (PIN1-I1 (I) >= -30000 AND PIN1-I1 (I) < -20000)
                            ADD     1           TO      TBL01-CNT (I 3)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  3)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  30000 AND PIN1-I1 (I) <  40000)
                    OR (PIN1-I1 (I) >= -40000 AND PIN1-I1 (I) < -30000)
                            ADD     1           TO      TBL01-CNT (I 4)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  4)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  40000 AND PIN1-I1 (I) <  50000)
                    OR (PIN1-I1 (I) >= -50000 AND PIN1-I1 (I) < -40000)
                            ADD     1           TO      TBL01-CNT (I 5)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  5)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  50000 AND PIN1-I1 (I) <  60000)
                    OR (PIN1-I1 (I) >= -60000 AND PIN1-I1 (I) < -50000)
                            ADD     1           TO      TBL01-CNT (I 6)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  6)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  60000 AND PIN1-I1 (I) <  70000)
                    OR (PIN1-I1 (I) >= -70000 AND PIN1-I1 (I) < -60000)
                            ADD     1           TO      TBL01-CNT (I 7)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  7)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  70000 AND PIN1-I1 (I) <  80000)
                    OR (PIN1-I1 (I) >= -80000 AND PIN1-I1 (I) < -70000)
                            ADD     1           TO      TBL01-CNT (I 8)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  8)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=  80000 AND PIN1-I1 (I) <  90000)
                    OR (PIN1-I1 (I) >= -90000 AND PIN1-I1 (I) < -80000)
                            ADD     1           TO      TBL01-CNT (I 9)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  9)
                                                       TBL01-CNT (11 11)
                       WHEN
                       (PIN1-I1 (I) >=   90000 AND PIN1-I1(I) <= 100000)
                    OR (PIN1-I1 (I) >= -100000 AND PIN1-I1(I) < -90000)
                            ADD     1           TO      TBL01-CNT (I 10)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11 10)
                                                       TBL01-CNT (11 11)
                   END-EVALUATE
               ELSE
                   EVALUATE TRUE 
                       WHEN
                          (PIN1-I1 (I) >=  1000 AND PIN1-I1 (I) <  1100)
                      OR (PIN1-I1 (I) >= -1100 AND PIN1-I1 (I) <= -1000)
                            ADD     1           TO      TBL01-CNT (I 1)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  1)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1100 AND PIN1-I1 (I) <  1200)
                       OR (PIN1-I1 (I) >= -1200 AND PIN1-I1 (I) < -1100)
                            ADD     1           TO      TBL01-CNT (I 2)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  2)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1200 AND PIN1-I1 (I) <  1300)
                       OR (PIN1-I1 (I) >= -1300 AND PIN1-I1 (I) < -1200)
                            ADD     1           TO      TBL01-CNT (I 3)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  3)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1300 AND PIN1-I1 (I) <  1400)
                       OR (PIN1-I1 (I) >= -1400 AND PIN1-I1 (I) < -1300)
                            ADD     1           TO      TBL01-CNT (I 4)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  4)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1400 AND PIN1-I1 (I) <  1500)
                       OR (PIN1-I1 (I) >= -1500 AND PIN1-I1 (I) < -1400)
                            ADD     1           TO      TBL01-CNT (I 5)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  5)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1500 AND PIN1-I1 (I) <  1600)
                       OR (PIN1-I1 (I) >= -1600 AND PIN1-I1 (I) < -1500)
                            ADD     1           TO      TBL01-CNT (I 6)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  6)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1600 AND PIN1-I1 (I) <  1700)
                       OR (PIN1-I1 (I) >= -1700 AND PIN1-I1 (I) < -1600)
                            ADD     1           TO      TBL01-CNT (I 7)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  7)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1700 AND PIN1-I1 (I) <  1800)
                       OR (PIN1-I1 (I) >= -1800 AND PIN1-I1 (I) < -1700)
                            ADD     1           TO      TBL01-CNT (I 8)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  8)
                                                       TBL01-CNT (11 11)
                       WHEN
                          (PIN1-I1 (I) >=  1800 AND PIN1-I1 (I) <  1900)
                       OR (PIN1-I1 (I) >= -1900 AND PIN1-I1 (I) < -1800)
                            ADD     1           TO      TBL01-CNT (I 9)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11  9)
                                                       TBL01-CNT (11 11)
                       WHEN
                         (PIN1-I1 (I) >=  1900 AND PIN1-I1 (I) <=  2000)
                       OR (PIN1-I1 (I) >= -2000 AND PIN1-I1 (I) < -1900)
                            ADD     1           TO      TBL01-CNT (I 10)
                                                       TBL01-CNT (I  11)
                                                       TBL01-CNT (11 10)
                                                       TBL01-CNT (11 11)
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** 平均値　出力 WRITE POT1,POT2
       S110-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 11

                   MOVE    SPACE       TO      POT1-REC
                                               POT2-REC
                   PERFORM VARYING J FROM 1 BY 1
                       UNTIL J > 11
                           MOVE    TBL01-CNT (I J) TO  POT1-CNT(J)
                           COMPUTE POT2-CNT(J) ROUNDED
                                 = TBL01-CNT (I J) * 100
                                               / TBL01-CNT (I 11)
                   END-PERFORM

                   WRITE   POT1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
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
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
