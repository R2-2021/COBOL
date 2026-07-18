      *    *** CSV データ読み取り

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE09.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE09".

           03  WK-IN-CNT       BINARY-LONG SYNC VALUE ZERO.

      *     COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

      *     COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

      *    *** OUT-TBL SET IDX
           03  K1              BINARY-LONG SYNC VALUE 1.
      *    *** OUT SET 位置
           03  O1              BINARY-LONG SYNC VALUE ZERO.
           03  O1-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

       LINKAGE                 SECTION.

           COPY    CPDECODE09  REPLACING ==:##:== BY ==LDE09==.

       PROCEDURE               DIVISION
                   USING       LDE09-DECODE09-AREA.
       M100-10.

      *    *** 初期処理
           PERFORM S010-10     THRU    S010-EX

      *    *** CSV データ取り出し
           PERFORM S100-10     THRU    S100-EX
          .
       M100-EX.
           EXIT    PROGRAM.

      *    *** 初期処理
       S010-10.

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > 20
                   MOVE    SPACE       TO      LDE09-OUT     (J)
                   MOVE    ZERO        TO      LDE09-OUT-LEN (J)
           END-PERFORM

           MOVE    ZERO        TO      LDE09-OUT-TBL-CNT

           MOVE    1           TO      K1
           MOVE    ZERO        TO      O1
           ADD     1           TO      WK-IN-CNT
           .
       S010-EX.
           EXIT.

      *    *** CSV データ取り出し
       S100-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LDE09-IN-LEN

                   EVALUATE TRUE
                       WHEN LDE09-IN (I:2) = ',"'

                           ADD     1           TO      K1
                           MOVE    ZERO        TO      O1

                           ADD     2           TO      I
                           PERFORM UNTIL LDE09-IN (I:2) = '",'
      *    *** CSV データ取り出し1
                                   PERFORM S110-10     THRU    S110-EX

                                   IF      I           >   LDE09-IN-LEN
                                          DISPLAY WK-PGM-NAME
                                                ' ," とペアの",が無い'
                                          DISPLAY LDE09-IN
                                          STOP    RUN
                                   END-IF

                                   IF      O1          >       2000
                                          DISPLAY WK-PGM-NAME
                                                ' OUT-AREA OVER > 2000'
                                          DISPLAY LDE09-IN
                                          STOP    RUN
                                   END-IF
                           END-PERFORM

                       WHEN LDE09-IN (I:2) = '""'
                           ADD     1           TO      O1
                           MOVE    '"'         TO  
                                   LDE09-OUT (K1) (O1:1)
                           ADD     1           TO      I

                       WHEN LDE09-IN (I:1) = ','
      *    *** 次のTBL SET の為,IDX カウントアップ
                           ADD     1           TO      K1
                           MOVE    ZERO        TO      O1

                       WHEN OTHER
                           ADD     1           TO      O1
                           MOVE    LDE09-IN (I:1) TO
                                   LDE09-OUT (K1) (O1:1)
                   END-EVALUATE

                   IF      I           <       LDE09-IN-LEN
                       AND K1          >       20
                           DISPLAY WK-PGM-NAME
                                    ' OUT-IDX OVER > 20'
                           DISPLAY LDE09-IN
                           STOP    RUN
                   END-IF
           END-PERFORM

           COMPUTE LDE09-OUT-TBL-CNT = K1 - 1
           .
       S100-EX.
           EXIT.

      *    *** CSV データ取り出し1
      *    *** ,"XX,""YY"",ZZ", => XX,"YY",ZZ
       S110-10.

           EVALUATE TRUE
               WHEN LDE09-IN (I:2) = '""'
                   ADD     1           TO      O1
                   MOVE    '"'         TO      LDE09-OUT (K1) (O1:1)
                   ADD     2           TO      I
               WHEN OTHER
                   ADD     1           TO      O1
                   MOVE    LDE09-IN (I:1) TO   LDE09-OUT (K1) (O1:1)
                   ADD     1           TO      I
           END-EVALUATE
           .
       S110-EX.
           EXIT.
