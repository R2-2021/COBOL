      *    *** http:// 行の出力 TEST10でHTML解析できない時、使用
      *    *** 通常<li>の様に、<>１行単位に入力されてるが、
      *    *** <li> １行に無い時、複数行になる時、１行に編集
      *    *** その他の組込み関数、<が>とペアでないため、他の文字に変更

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST45.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** animatetimes.YYYYMM.html データ等
      * SELECT PIN1-F           ASSIGN   "youtube.YUIKAORI2.html"
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST10.PIN2 YYYYMM 指定
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST10.PIN1 入力データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(20000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-YYYYMM     PIC  X(006).
           03                  PIC  X(074).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-FLAG       PIC  X(0004).
           03  POT2-KANMA      PIC  X(0001).
           03  POT2-REC1       PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST45  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST45.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST45.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST45.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-FIND         PIC  X(001) VALUE ZERO.
           03  WK-PIN1-LEN         BINARY-LONG SYNC VALUE ZERO.

           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-2      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-3      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-4      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-5      BINARY-LONG SYNC VALUE ZERO.

           03  WK-STR          PIC  X(003) VALUE "%XX".
           03  WK-VAL          PIC  X(001) VALUE X"20".
           03  WK-FLAG         PIC  X(004) VALUE SPACE.

       01  WK-PIN1-REC         PIC  X(20000) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE 1.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-START        PIC  X(001) VALUE "N".
           03  SW-END          PIC  X(001) VALUE "N".
           03  SW-CONT         PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

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

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF

           IF      PIN2-YYYYMM =       SPACE
                   CONTINUE
           ELSE
               IF    ( PIN2-YYYYMM(1:4) >=     2018 AND <= 2040 ) AND
                     ( PIN2-YYYYMM(5:2) >=     01 AND <= 12 )
                   MOVE    "animatetimes." TO  WK-PIN1-F-NAME (01:13)
                   MOVE    PIN2-YYYYMM     TO  WK-PIN1-F-NAME (14:06)
                   MOVE    ".html"         TO  WK-PIN1-F-NAME (20:05)
               ELSE
                   MOVE    PIN2-REC        TO  WK-PIN1-F-NAME
               END-IF
           END-IF

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

      *    ***READ PIN1
       S020-10.

           MOVE    SPACE       TO      PIN1-REC
           READ    PIN1-F

      *    IF      WK-PIN1-CNT >= 4175 AND <= 4190
      *         DISPLAY " " 
      *         DISPLAY WK-PIN1-CNT " " WK-PIN1-LEN " " WK-POT1-CNT
      *         DISPLAY PIN1-REC(1:80)
      *         DISPLAY POT1-REC(1:80)
      *     END-IF

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT

                   MOVE    ZERO        TO      WK-COUNT-1
                                               WK-COUNT-2
                                               WK-COUNT-3
                                               WK-COUNT-4
                                               WK-COUNT-5
                   INSPECT PIN1-REC
                      TALLYING
                         WK-COUNT-5 FOR ALL "<!--"
      *    *** COUNT-1を先にすると、COUNT-5がセットされない
                         WK-COUNT-3 FOR ALL "<d;k++)"
                         WK-COUNT-4 FOR ALL "<="
                         WK-COUNT-1 FOR ALL "<"
                         WK-COUNT-2 FOR ALL ">"

                       REPLACING ALL "<!--" BY "<!->"
                       REPLACING ALL "<d;k++)" BY "*d;k++)"
                       REPLACING ALL "<=" BY "*="
                       REPLACING ALL "-->" BY "<->"
      *     IF      WK-PIN1-CNT >= 1 AND <= 100
      *       DISPLAY WK-PIN1-CNT " " WK-COUNT-1 " "
      *               WK-COUNT-2 PIN1-REC(1:20)
      *     END-IF
      *             IF      WK-COUNT-1  =        1    AND
      *                     WK-COUNT-2  =        ZERO
                   IF      WK-COUNT-1  >        WK-COUNT-2
                           MOVE    "Y"          TO      SW-CONT
                   END-IF

                   IF      WK-COUNT-3  >        ZERO OR
                           WK-COUNT-4  >        ZERO OR
                           WK-COUNT-5  >        ZERO
                           MOVE    "N"          TO      SW-CONT
                   END-IF

      *             IF      WK-COUNT-1  =        ZERO AND
      *                     WK-COUNT-2  =        1
                   IF      WK-COUNT-1  <        WK-COUNT-2
                           MOVE    "N"          TO      SW-CONT
                   END-IF

                   IF      WK-COUNT-1  =        WK-COUNT-2 AND
                           WK-COUNT-1  NOT =    ZERO
                           MOVE    "N"          TO      SW-CONT
                   END-IF

      *     IF WK-PIN1-CNT >= 0 AND <= 121
      *     DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT " " PIN1-REC (1:20)

      *    MOVE    "P"         TO      WFD-ID
      *    MOVE     WK-PIN1-CNT TO     WFD-SEQ
      *    MOVE    200         TO      WFD-LEN
      *    CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                PIN1-REC
      *                                WFD-LEN
      *     END-IF
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
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

           EVALUATE TRUE

               WHEN SW-CONT = "N"
                   PERFORM VARYING I FROM 1 BY 10
                           UNTIL I > WK-PIN1-LEN OR
                                 I > 20000
                           IF      PIN1-REC(I:10) =    SPACE
                                   CONTINUE
                           ELSE
                               MOVE    PIN1-REC(I:10) TO
                                       WK-PIN1-REC(I2:10)
                               ADD     10          TO      I2
                           END-IF
                   END-PERFORM

                   WRITE   POT1-REC    FROM    WK-PIN1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    SPACE       TO      WK-PIN1-REC
                   MOVE    1           TO      I2
                   MOVE    "N"         TO      SW-CONT

               WHEN SW-CONT = "Y"
                   PERFORM VARYING I FROM 1 BY 10
                           UNTIL I > WK-PIN1-LEN OR
                                 I > 20000
                       IF      PIN1-REC(I:10) =    SPACE
                                   CONTINUE
                           ELSE
                               MOVE    PIN1-REC(I:10) TO
                                       WK-PIN1-REC(I2:10)
                               ADD     10          TO      I2
                           END-IF
                   END-PERFORM

      *             WRITE   POT1-REC    FROM    WK-PIN1-REC
      *             ADD     1           TO      WK-POT1-CNT

      *             MOVE    SPACE       TO      WK-PIN1-REC
      *             MOVE    1           TO      I2
      *             MOVE    "N"         TO      SW-CONT
           END-EVALUATE
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
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
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
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
