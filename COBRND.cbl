      *    *** ランダム数作成サブルーチン

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBRND.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ＳＪＩＳ、声優氏名データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ＵＴＦ８、声優氏名データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

      *    *** ＳＪＩＳ、声優氏名データ
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-S-NAME     PIC  X(020).
           03  FILLER          PIC  X(060).

      *    *** ＵＴＦ８、声優氏名データ
       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-S-NAME     PIC  X(030).
           03  FILLER          PIC  X(050).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBRND  ".

      *    *** SJIS
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "COBRND.seiyu.okiniiri.csv".

      *    *** UTF8
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE
               "COBRND.seiyuall.utf8.csv".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SEQ          BINARY-LONG SYNC VALUE ZERO.

           03  WK-DATE-TIME.
             05  WK-DATE-HMS.
               07  WK-DATE-HH  PIC  9(002) VALUE ZERO.
               07  WK-DATE-MI  PIC  9(002) VALUE ZERO.
               07  WK-DATE-SS  PIC  9(002) VALUE ZERO.
               07  WK-DATE-SM  PIC  9(002) VALUE ZERO.

           03  WK-SEED         OCCURS 99
                               BINARY-DOUBLE SYNC VALUE ZERO.

           03  WK-DATA         OCCURS 99
                               BINARY PIC V9(9) VALUE ZERO.

       01  TBL-AREA.
      *    *** 未使用
      *     03  TBL01-AREA      OCCURS 99.
      *       05  TBL01-NUM     BINARY-DOUBLE SYNC VALUE ZERO.

           03  TBL02-AREA      OCCURS 44.
             05  TBL02-KANA    PIC  X(001) VALUE SPACE.

           03  TBL03-AREA      OCCURS 26.
             05  TBL03-ALPHA   PIC  X(001) VALUE SPACE.

           03  TBL04-AREA      OCCURS 131.
             05  TBL04-S-NAME  PIC  X(020) VALUE SPACE.

           03  TBL05-AREA      OCCURS 174.
             05  TBL05-S-NAME  PIC  X(030) VALUE SPACE.

       01  INDEX-AREA.
           03  C1              OCCURS 99
                               BINARY-DOUBLE SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       LINKAGE                 SECTION.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==LCR==.

       PROCEDURE               DIVISION  USING
                               LCR-COBRND-AREA.

       M100-10.

           EVALUATE TRUE

               WHEN LCR-ID     =       "STR"
      *    *** OPEN,初期値セット
                   PERFORM S010-10     THRU    S010-EX

               WHEN LCR-ID     =       "RND"
                   ADD     1           TO      WK-SEQ
                   MOVE    WK-SEQ      TO      LCR-SEQ

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > LCR-IDX
                           COMPUTE WK-DATA (I) LCR-RND (I) =
                                 ( WK-SEED (I) * WK-DATA (I) )
                                 + ( ( I / 11.11 )  + 1 ) * .123456
                           COMPUTE WK-SEED (I) = WK-DATA (I) * 100000

      *    *** ランダム値セット
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN LCR-ID       =       "END"
      *    *** CLOSE
                   PERFORM S900-10     THRU    S900-EX

      *    *** ERROR
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LCR-ID PARA ERROR="
                           LCR-ID
                   DISPLAY WK-PGM-NAME " LCR-ID STR,RND,END 指定"
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN,初期値セット
       S010-10.

           OPEN    INPUT       PIN1-F.
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F.
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

      *    *** TBL01-NUM (I) は未使用にする
      *     PERFORM VARYING I FROM 1 BY 1
      *             UNTIL   I > 99
      *    *** I=1  TBL01-NUM (I) <= ZERO
      *    *** I=2  TBL01-NUM (I) <= 1111
      *    *** I=99 TBL01-NUM (I) <= 1111 * 98 (108,878)
      *             COMPUTE TBL01-NUM (I) = ( I - 1 ) * 1111
      *     END-PERFORM

           ACCEPT  WK-DATE-HMS FROM    TIME
           MOVE    WK-DATE-SM  TO      I2
           IF      I2          =       ZERO
                   MOVE    1           TO      I2
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 99
                   COMPUTE WK-DATA (I2) =
                         ( WK-SEED (I2) * WK-DATA (I2) )
                     + ( ( I / 11.11 )  + 1 ) * .123456
                   COMPUTE WK-SEED (I2) = WK-DATA (I2) * 100000
      *     DISPLAY "I=" I " I2=" I2
      *             " WK-DATA (I2)=" WK-DATA (I2) 
      *             " WK-SEED (I2)=" WK-SEED (I2)

                   ADD     1           TO      I2
                   IF      I2          =       100
                           MOVE    1           TO      I2
                   END-IF
           END-PERFORM

           MOVE    ZERO        TO      J
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 255
                  IF   I >= 178 AND I <= 221 
                       ADD     1       TO      J
                       MOVE    FUNCTION CHAR(I) TO TBL02-KANA (J)
           END-PERFORM

           MOVE    ZERO        TO      J
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 256
                  IF   I >= 66 AND I <= 91 
                       ADD     1       TO      J
                       MOVE    FUNCTION CHAR(I) TO TBL03-ALPHA (J)
                  END-IF 
           END-PERFORM

           MOVE    ZERO        TO      J
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 131
                   READ    PIN1-F
                       AT  END
                           CONTINUE
                       NOT AT  END
                           MOVE    PIN1-S-NAME  TO  TBL04-S-NAME (I)
                   END-READ
           END-PERFORM

           MOVE    ZERO        TO      J
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 174
                   READ    PIN2-F
                       AT  END
                           CONTINUE
                       NOT AT  END
                           MOVE    PIN2-S-NAME  TO  TBL05-S-NAME (I)
                   END-READ
           END-PERFORM
           .
       S010-EX.
           EXIT.

      *    *** ランダム値セット
       S020-10.

      *    *** LCR-FROM2(I),LCR-TO2(I)は1-100000範囲でメインで指定している
           IF      LCR-FROM2 (I) =     ZERO AND
                   LCR-TO2   (I) =     ZERO

                   COMPUTE LCR-NUM (I) ROUNDED = 100000 * LCR-RND (I)

                   IF      LCR-ZERO (I) =     "N"
                           IF      LCR-NUM (I) =       ZERO
                                   MOVE    1           TO    LCR-NUM (I)
                           ELSE
                                   CONTINUE
                           END-IF
                   ELSE
                           IF      LCR-RND (I) <       .02
                                   MOVE    ZERO        TO    LCR-NUM (I)
                           ELSE
                                   CONTINUE
                           END-IF
                   END-IF

           ELSE
                   IF      LCR-FROM2 (I) <     LCR-TO2 (I)
                           CONTINUE
                   ELSE
                           MOVE    1000        TO      LCR-FROM2 (I)
                           MOVE    2000        TO      LCR-TO2   (I)
                   END-IF
                   COMPUTE LCR-NUM(I) ROUNDED =
                         ( LCR-TO2(I) - LCR-FROM2 (I) )
                                      * LCR-RND (I) + LCR-FROM2 (I)
                   IF      LCR-ZERO (I) =     "N"
                           IF      LCR-NUM (I) =       ZERO
      *    *** LCR-ZERO (I) = "N"で、LCR-FROM2(I)がゼロの時、
      *    *** ゼロセットを優先にする
                                   MOVE    LCR-FROM2 (I) TO  LCR-NUM (I)
                           ELSE
                                   CONTINUE
                           END-IF
                   ELSE
                           IF      LCR-RND(I)  <       .02
      *    *** LCR-ZERO (I) = "Y"で、LCR-FROM2(I) < .02の時、
      *    *** LCR-FROM2(I)がゼロ以外でも、その値セットを優先にする
                                   MOVE    LCR-FROM2 (I) TO  LCR-NUM (I)
                           ELSE
                                   CONTINUE
                           END-IF
                   END-IF
      *             END-IF
           END-IF

           EVALUATE LCR-SIGN (I)
               WHEN "N"
                   CONTINUE
               WHEN "-"
                   COMPUTE J ROUNDED = LCR-RND (I) * 10
                   IF      J           =       ZERO
                           MOVE    1           TO      J
                   ELSE
                           CONTINUE
                   END-IF
                   IF      J           =       1
                           COMPUTE LCR-NUM (I) = LCR-NUM (I) * -1
                   ELSE
                           CONTINUE
                   END-IF
               WHEN "Y"
                   COMPUTE J ROUNDED = LCR-RND (I) * 10
                   IF      J           =       ZERO
                           MOVE    1           TO      J
                   ELSE
                           CONTINUE
                   END-IF
                   IF      J           =       1
                           COMPUTE LCR-NUM (I) = LCR-NUM (I) * -1
                   ELSE
                           CONTINUE
                   END-IF
               WHEN "1"
                   DIVIDE LCR-SEQ BY 2 GIVING J ROUNDED
                          REMAINDER K
                   IF      K           =       ZERO
                           COMPUTE LCR-NUM (I) = LCR-NUM (I) * -1
                   ELSE
                           CONTINUE
                   END-IF
               WHEN "2"
                   DIVIDE LCR-SEQ BY 10 GIVING J ROUNDED
                          REMAINDER K
                   IF      K           =       ZERO
                           COMPUTE LCR-NUM (I) = LCR-NUM (I) * -1
                   ELSE
                           CONTINUE
                   END-IF
               WHEN "3"
                   DIVIDE LCR-SEQ BY 100 GIVING J ROUNDED
                          REMAINDER K
                   IF      K           =       ZERO
                           COMPUTE LCR-NUM (I) = LCR-NUM (I) * -1
                   ELSE
                           CONTINUE
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE


      *    *** カタカナ　セット
           COMPUTE K ROUNDED = LCR-RND (I) * 44
           IF      K           =       ZERO
                   MOVE    1           TO      K
           END-IF
           MOVE    SPACE       TO      LCR-KANA (I)
           MOVE    FUNCTION SUBSTITUTE
                          ( LCR-KANA (I),SPACE,TBL02-KANA (K) )
                               TO      LCR-KANA (I)


      *    *** アルファベット　セット
           COMPUTE K ROUNDED = LCR-RND (I) * 26
           IF      K           =       ZERO
                   MOVE    1           TO      K
           END-IF
           MOVE    SPACE       TO      LCR-ALPHA (I)
           MOVE    FUNCTION SUBSTITUTE
                          ( LCR-ALPHA (I),SPACE,TBL03-ALPHA (K) )
                               TO      LCR-ALPHA (I)

      *    *** 声優名　ＳＪＩＳ　セット
           COMPUTE K ROUNDED = LCR-RND (I) * 131
           IF      K           =       ZERO
                   MOVE    1           TO      K
           END-IF
           MOVE    TBL04-S-NAME (K) TO      LCR-S-NAME (I)

      *    *** 声優名　ＵＴＦ８　セット
           COMPUTE K ROUNDED = LCR-RND (I) * 174
           IF      K           =       ZERO
                   MOVE    1           TO      K
           END-IF
           MOVE    TBL05-S-NAME (K) TO      LCR-S-NAME8 (I)



           ADD     1           TO      C1 (I)
           IF      LCR-BETWEEN (I) =    ZERO
                   MOVE    1           TO      LCR-BETWEEN (I)
           END-IF

           IF      C1 (I)      >       LCR-TO-CNT (I)
                   MOVE    1           TO      C1 (I)
                   ADD     LCR-BETWEEN (I) TO  LCR-FROM (I)
           ELSE
                   CONTINUE
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F.
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F.
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S900-EX.
           EXIT.
