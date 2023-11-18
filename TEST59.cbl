      *    *** COBRND 発生回数、再チェック
      *    *** C.FILEITEM.T016

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST59.

       ENVIRONMENT             DIVISION.
       CONFIGURATION SECTION.
      *    *** この指定をする事によって、FUNCTION を省略出来る
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** RND 出現回数 DATA
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** RND DATA
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F.
       01  PIN1-REC.
           03  PIN1-I1           PIC  X(005).

       FD  POT1-F.
       01  POT1-REC.
           03  POT1-I1           PIC  9(005).
           03  POT1-TBL          OCCURS 10.
             05  POT1-I2         PIC  9(005).
           03  POT1-MIN          PIC  9(005).
           03  POT1-MAX          PIC  9(005).
           03  POT1-ABE          PIC  9(005).

       FD  POT2-F.
       01  POT2-REC.
           03  POT2-I1           PIC  9(005).
           03  POT2-I2           PIC  9(005).
           03  POT2-RND          PIC  9V9(9).
           03  POT2-NUM          PIC  9(010).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST59  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST59.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST59.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST59.POT2".

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

           03  WK-SEED         BINARY-LONG SYNC VALUE 11111.

       01  WK-REC.
           03  WK-I1           PIC  9(005) VALUE 100.
           03  WK-TBL          OCCURS 10.
             05  WK-I2         PIC  9(005) VALUE ZERO.
           03  WK-MIN          PIC  9(005) VALUE 99999.
           03  WK-MAX          PIC  9(005) VALUE ZERO.
           03  WK-ABE          PIC  9(005) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3-RND          BINARY-LONG SYNC VALUE ZERO.
           03  I3-NUM          BINARY-LONG SYNC VALUE ZERO.
           03  I3-RND-MIN      BINARY-LONG SYNC VALUE ZERO.
           03  I3-NUM-MIN      BINARY-LONG SYNC VALUE ZERO.
           03  I3-RND-MAX      BINARY-LONG SYNC VALUE ZERO.
           03  I3-NUM-MAX      BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 99.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-AREA    OCCURS 100.
               07  TBL01-RND   BINARY-LONG SYNC VALUE ZERO.
               07  TBL01-NUM   BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-AREA2   OCCURS 10.
               07  TBL01-RND-I4 BINARY-LONG SYNC VALUE ZERO.
               07  TBL01-NUM-I4 BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** #NN レコード編集3
      *             PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *      END-PERFORM

      *    *** COBRND CHECK
           PERFORM S100-10     THRU    S100-EX

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
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

      *****     CALL "COBDUMP" USING  WK-DATA
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

      *    *** COBRND CHECK
       S100-10.

           PERFORM VARYING I2 FROM 1 BY 1 
                   UNTIL I2 > 99
                   MOVE    "N"        TO      WCR-ZERO  (I2)

           END-PERFORM

      *     MOVE    "N"        TO      WCR-ZERO  (2)
      *     MOVE    ZERO       TO      WCR-FROM2 (2)
      *     MOVE    2000       TO      WCR-TO2   (2)

      *     MOVE    "Y"        TO      WCR-ZERO  (3)
      *     MOVE    ZERO       TO      WCR-FROM2 (3)
      *     MOVE    100        TO      WCR-TO2   (3)

      *     MOVE    "Y"        TO      WCR-ZERO  (4)
      *     MOVE    50         TO      WCR-FROM2 (4)
      *     MOVE    100        TO      WCR-TO2   (4)

           MOVE    "Y"        TO      WCR-ZERO  (5)
           MOVE    ZERO       TO      WCR-FROM2 (5)
           MOVE    100000     TO      WCR-TO2   (5)

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 1000
                       MOVE    "RND"       TO      WCR-ID
                       MOVE    99          TO      WCR-IDX
                       CALL    "COBRND"    USING   WCR-COBRND-AREA
                   PERFORM VARYING I2 FROM 1 BY 1 
                           UNTIL I2 > 99
      *                     UNTIL I2 > 1

      *                 COMPUTE WCR-RND (I2) = RANDOM (WK-SEED) 
      *                 COMPUTE WCR-RND (I2) = FUNCTION RANDOM (WK-SEED) 

      *    *** WK-SEED がゼロのみだと、同じ乱数しか、返さない。
      *    *** WCR-RND:0.038000
      *                 COMPUTE WCR-RND (I2) = FUNCTION RANDOM (0) 
                       COMPUTE WK-SEED WCR-NUM (I2) 
                              = WCR-RND (I2) * 100000
      *     IF  I2 = 1
      *       DISPLAY "WCR-RND (I2)=" WCR-RND (I2) " WK-SEED=" WK-SEED
      *               " WCR-NUM (I2)=" WCR-NUM (I2)
      *     END-IF
                       MOVE    I1          TO      POT2-I1
                       MOVE    I2          TO      POT2-I2
                       MOVE    WCR-RND (I2) TO     POT2-RND
                       MOVE    WCR-NUM (I2) TO     POT2-NUM

      *    *** WRITE POT2
                       PERFORM S120-10     THRU    S120-EX
                       COMPUTE I ROUNDED = WCR-RND (I2) * 100
                       IF I = ZERO
                         MOVE 1 TO I
                       END-IF
                       IF I > 100
                         MOVE 100 TO I
      *                 DISPLAY "I1=" I1 " I2=" I2  " I=" I
                       END-IF
                       ADD     1           TO      TBL01-RND (I2 I)
                       EVALUATE TRUE
                           WHEN I < 10
                               ADD     1   TO      TBL01-RND-I4 (I2 1)
                           WHEN I < 20
                               ADD     1   TO      TBL01-RND-I4 (I2 2)
                           WHEN I < 30
                               ADD     1   TO      TBL01-RND-I4 (I2 3)
                           WHEN I < 40
                               ADD     1   TO      TBL01-RND-I4 (I2 4)
                           WHEN I < 50
                               ADD     1   TO      TBL01-RND-I4 (I2 5)
                           WHEN I < 60
                               ADD     1   TO      TBL01-RND-I4 (I2 6)
                           WHEN I < 70
                               ADD     1   TO      TBL01-RND-I4 (I2 7)
                           WHEN I < 80
                               ADD     1   TO      TBL01-RND-I4 (I2 8)
                           WHEN I < 90
                               ADD     1   TO      TBL01-RND-I4 (I2 9)
                           WHEN OTHER
                               ADD     1   TO      TBL01-RND-I4 (I2 10)
                       END-EVALUATE

      *             IF  I2 = 1 OR 2 OR 3 OR 4
      *    *** I2=4 NUM=50-100 で作成　1000個中199個表示
                   IF   I2 =  4
                    AND I1 < 200
      *                 DISPLAY "I1=" I1 " I2=" I2
      *                         " RND=" WCR-RND (I2)
      *                         " NUM=" WCR-NUM (I2)
                        CONTINUE
                   END-IF
                       COMPUTE I ROUNDED = WCR-NUM (I2) / 1000
                       IF I = ZERO
                         MOVE 1 TO I
                       END-IF
                       IF I > 100
                         MOVE 100 TO I
      *                 DISPLAY "I1=" I1 " I2=" I2  " I=" I
                       END-IF

      *                 DISPLAY "I1=" I1 " I2=" I2  " I=" I
                       ADD     1           TO      TBL01-NUM (I2 I)
                       EVALUATE TRUE
                           WHEN I < 10
                               ADD     1   TO      TBL01-NUM-I4 (I2 1)
                           WHEN I < 20
                               ADD     1   TO      TBL01-NUM-I4 (I2 2)
                           WHEN I < 30
                               ADD     1   TO      TBL01-NUM-I4 (I2 3)
                           WHEN I < 40
                               ADD     1   TO      TBL01-NUM-I4 (I2 4)
                           WHEN I < 50
                               ADD     1   TO      TBL01-NUM-I4 (I2 5)
                           WHEN I < 60
                               ADD     1   TO      TBL01-NUM-I4 (I2 6)
                           WHEN I < 70
                               ADD     1   TO      TBL01-NUM-I4 (I2 7)
                           WHEN I < 80
                               ADD     1   TO      TBL01-NUM-I4 (I2 8)
                           WHEN I < 90
                               ADD     1   TO      TBL01-NUM-I4 (I2 9)
                           WHEN OTHER
                               ADD     1   TO      TBL01-NUM-I4 (I2 10)
                       END-EVALUATE

      *                 DISPLAY "I1=" I1 " I2=" I2 
      *                         " WCR-RND=" WCR-RND (I2)
      *                         " WCR-NUM(I2)=" WCR-NUM(I2)
                   END-PERFORM
           END-PERFORM

           MOVE    ZERO        TO      I3-RND
           MOVE    ZERO        TO      I3-NUM
           MOVE    9999        TO      I3-RND-MIN
           MOVE    9999        TO      I3-NUM-MIN
           MOVE    ZERO        TO      I3-RND-MAX
           MOVE    ZERO        TO      I3-NUM-MAX
           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 99
                   PERFORM VARYING I2 FROM 1 BY 1 
                           UNTIL I2 > 100

      *               IF ( TBL01-RND (I1 I2) > 15 OR < 5 ) OR
      *                  ( TBL01-NUM (I1 I2) > 15 OR < 5 )
      *                 IF I2 < 50
      *                   DISPLAY "I1=" I1 " I2=" I2 
      *                         " -RND (I1 I2)=" TBL01-RND (I1 I2)
      *                         " -NUM (I1 I2)=" TBL01-NUM (I1 I2)
      *                 END-IF
      *                   ADD 1 TO TBL01-CNT (I1)
      *               END-IF
                      COMPUTE I3-RND = ( TBL01-RND (I1 I2) * I2 ) + 
                                       I3-RND
                      COMPUTE I3-NUM = ( TBL01-NUM (I1 I2) * I2 ) + 
                                       I3-NUM
                   END-PERFORM
                   COMPUTE I3-RND = I3-RND / 100
                   COMPUTE I3-NUM = I3-NUM / 100
      *             DISPLAY "I1=" I1 
      *                     " I3-RND=" I3-RND
      *                     " I3-NUM=" I3-NUM

                   IF      I3-RND      <       I3-RND-MIN
                           MOVE    I3-RND      TO      I3-RND-MIN
                   END-IF

                   IF      I3-NUM      <       I3-NUM-MIN
                           MOVE    I3-NUM      TO      I3-NUM-MIN
                   END-IF

                   IF      I3-RND      >       I3-RND-MAX
                           MOVE    I3-RND      TO      I3-RND-MAX
                   END-IF

                   IF      I3-NUM      >       I3-NUM-MAX
                           MOVE    I3-NUM      TO      I3-NUM-MAX
                   END-IF
                   MOVE    ZERO        TO      I3-RND
                   MOVE    ZERO        TO      I3-NUM
           END-PERFORM

           MOVE    ZERO        TO      POT1-ABE
                                       POT1-MAX
           MOVE    99999       TO      POT1-MIN
           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 99
                   PERFORM VARYING I2 FROM 1 BY 1 
                           UNTIL I2 > 10

      *    *** I1=5 100近くに分散して出力している
      *             IF     I1           <=       5
      *                   DISPLAY "I1=" I1 " I2=" I2 
      *                         " -RND-I4=" TBL01-RND-I4 (I1 I2)
      *                   DISPLAY "I1=" I1 " I2=" I2 
      *                         " -NUM-I4=" TBL01-NUM-I4 (I1 I2)
      *             END-IF
      *               IF    TBL01-CNT (I1 I2) = ZERO OR 1
      *                     CONTINUE
      *               ELSE
      *                  ( TBL01-NUM (I1 I2) > 150 OR < 50 )
      *                  DISPLAY "I1=" I1 " I2=" I2 
      *                         " -CNT (I1)=" TBL01-CNT (I1)
      *               END-IF
      *    *** 80-120 はほぼ均等に出現しているので、表示してない
      *                IF    TBL01-RND-I4 (I1 I2) < 80 OR > 120
      *                   DISPLAY "I1=" I1 " I2=" I2 
      *                         " -RND-I4=" TBL01-RND-I4 (I1 I2)
      *                END-IF
      *                IF    TBL01-NUM-I4 (I1 I2) < 80 OR > 120
      *                   DISPLAY "I1=" I1 " I2=" I2 
      *                         " -NUM-I4=" TBL01-NUM-I4 (I1 I2)
      *                END-IF
                           MOVE    TBL01-RND-I4 (I1 I2) TO POT1-I2 (I2)
                           ADD     TBL01-RND-I4 (I1 I2) TO WK-I2   (I2)
                           ADD     TBL01-RND-I4 (I1 I2) TO POT1-ABE
                                                           WK-ABE
                           IF      TBL01-RND-I4 (I1 I2) <  POT1-MIN
                               MOVE    TBL01-RND-I4 (I1 I2) TO POT1-MIN
                           END-IF
                           IF      TBL01-RND-I4 (I1 I2) >  POT1-MAX
                               MOVE    TBL01-RND-I4 (I1 I2) TO POT1-MAX
                           END-IF

                           IF      TBL01-RND-I4 (I1 I2) <  WK-MIN
                               MOVE    TBL01-RND-I4 (I1 I2) TO WK-MIN
                           END-IF
                           IF      TBL01-RND-I4 (I1 I2) >  WK-MAX
                               MOVE    TBL01-RND-I4 (I1 I2) TO WK-MAX
                           END-IF
                   END-PERFORM

                   MOVE    I1          TO      POT1-I1
                   COMPUTE POT1-ABE ROUNDED = POT1-ABE / 10.0

      *    *** WRITE POT1
                   PERFORM S110-10     THRU    S110-EX

                   MOVE    ZERO        TO      POT1-ABE
                                               POT1-MAX
                   MOVE    99999       TO      POT1-MIN
           END-PERFORM

           PERFORM VARYING I2 FROM 1 BY 1 
                   UNTIL I2 > 10
                   COMPUTE WK-I2 (I2) ROUNDED = WK-I2 (I2) / 99.0
           END-PERFORM
           COMPUTE WK-ABE ROUNDED = WK-ABE / 99.0 / 10.0

           MOVE    WK-REC      TO      POT1-REC

      *    *** WRITE POT1
           PERFORM S110-10     THRU    S110-EX

      *     DISPLAY "RND MIN=" I3-RND-MIN
      *             " MAX="    I3-RND-MAX
      *             " NUM MIN=" I3-NUM-MIN
      *             " MAX="    I3-NUM-MAX
           .
       S100-EX.
           EXIT.

      *    *** COBRND CHECK DATA WRITE
       S110-10.

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT

           .
       S110-EX.
           EXIT.

      *    *** WRITE POT2
       S120-10.

           WRITE   POT2-REC
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT2-CNT

           .
       S120-EX.
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

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

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
