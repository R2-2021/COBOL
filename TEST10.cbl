      *    *** http:// 行の出力
      *    *** <>かテキストで１行単位で出力
      *    *** 
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST10.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      * SELECT PIN1-F           ASSIGN   WK-PGM-NAME ".PIN1"
      * SELECT PIN1-F           ASSIGN   "expo_jam_2018.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201801.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201804.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201807.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201810.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201901.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201904.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201907.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.201910.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.202001.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.202004.html"
      * SELECT PIN1-F           ASSIGN   "animatetimes.202007.html"
      * SELECT PIN1-F           ASSIGN   "youtube.石原夏織.html"
      * SELECT PIN1-F           ASSIGN   "youtube.国語中字.html"
      * SELECT PIN1-F           ASSIGN   "kansou.201109"
      * SELECT PIN1-F           ASSIGN   "韓チョア20181115.html"
      * SELECT PIN1-F           ASSIGN   "youtube.ゆいかおり.html"
      * SELECT PIN1-F           ASSIGN   "youtube.YUIKAORI2.html"
      * SELECT PIN1-F           ASSIGN   "TEST45.POT1"
      * SELECT PIN1-F           ASSIGN   "Wikipedia石原夏織.html"
      * SELECT PIN1-F           ASSIGN   "XVI.Country.html"
      * SELECT PIN1-F           ASSIGN   "XVI.Country2.html"
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *    *** XVI 女優名は可変長,XVI.Country.html
      *     ORGANIZATION RECORD BINARY   SEQUENTIAL.
      *    *** XVI 国名は固定長,XVI.Country2.html
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XVI用出力
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.
      *
       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
      *    RECORD VARYING FROM WK-INT5 TO WK-INT6 DEPENDING WK-PIN1-LEN.
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
      *     03  FILLER          PIC  X(65536).
           03  FILLER          PIC  X(1000000).
      *     03  FILLER          PIC  X(100).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-YYYYMM     PIC  X(006).
           03                  PIC  X(250).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-FLAG       PIC  X(0004).
           03  POT2-KANMA      PIC  X(0001).
           03  POT2-REC1       PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST10  ".

           03  WK-PIN1-F-NAME  PIC  X(256) VALUE "TEST45.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST10.POT2".

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

           03  WK-PIN1-LEN-MAX-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN-MAX BINARY-LONG SYNC VALUE ZERO.

           03  WK-FIND         PIC  X(001) VALUE ZERO.
           03  WK-INT5         BINARY-LONG SYNC VALUE ZERO.
           03  WK-INT6         BINARY-LONG SYNC VALUE ZERO.
           03  WK-STR          PIC  X(003) VALUE "%XX".
           03  WK-VAL          PIC  X(001) VALUE X"20".
           03  WK-FLAG         PIC  X(004) VALUE SPACE.
           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.

       01  WK-PIN1-REC         PIC  X(65536) VALUE SPACE.

       01  WK-Buf-L            BINARY-LONG SYNC VALUE 10.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

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
      *
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
      *    *** 毎回ABENDするので、TEST45.POT1 をPIN1として、入力する
               IF    ( PIN2-YYYYMM(1:4) >=     2000 AND <= 2040 ) AND
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
                           " WK-PIN1-F-NAME=" WK-PIN1-F-NAME
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
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

      *     MOVE    20000       TO      WK-PIN1-LEN
           MOVE    SPACE       TO      PIN1-REC
                                       WK-PIN1-REC
           READ    PIN1-F      INTO    WK-PIN1-REC

      *    IF      WK-PIN1-CNT >= 1230 AND <= 1250
      *         DISPLAY " " 
      *         DISPLAY WK-PIN1-CNT " " WK-PIN1-LEN " " WK-POT1-CNT
      *         DISPLAY PIN1-REC(1:80)
      *         DISPLAY POT1-REC(1:80)
      *     END-IF

      *     IF      WK-PIN1-STATUS =    ZERO OR 4
           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   IF      WK-PIN1-LEN >       WK-PIN1-LEN-MAX
                       MOVE    WK-PIN1-LEN TO      WK-PIN1-LEN-MAX
                   END-IF

      *     IF WK-PIN1-CNT >= 0 AND <= 121
      *     DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT " " PIN1-REC (1:20)

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE     WK-PIN1-CNT TO     WFD-SEQ
      *     MOVE    200         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC
      *                                 WFD-LEN
      *     END-IF

                   MOVE    ZERO        TO      WK-COUNT-1
                   IF      WK-PIN1-F-NAME (01:13) = "animatetimes."
      *                     INSPECT WK-PIN1-REC 
      *                             TALLYING  WK-COUNT-1 FOR 
      *                                       ALL '<th width="80%">'
      *    *** PERFORM の方が処理時間早い、最初の<th width="80%">だけ
      *    *** カウントし、カウントあった時だけ、置換する
                       PERFORM VARYING K FROM 1 BY 1
                               UNTIL K > WK-PIN1-LEN
                          IF     WK-PIN1-REC (K:16) = '<th width="80%">'
                              ADD     1           TO      WK-COUNT-1
                              MOVE    WK-PIN1-LEN TO      K
                          END-IF
                       END-PERFORM
                       
                   END-IF

                   IF      WK-COUNT-1 NOT = ZERO
                       INSPECT WK-PIN1-REC 
                               REPLACING ALL '<th width="80%">'
                                          BY '<              >'
                   END-IF

      *    *** この記述はそのまま残す
                   IF      WK-PIN1-REC (1:16) =  '<th width="80%">'
                           MOVE    SPACE       TO     WK-PIN1-REC (1:16)
                           MOVE    17          TO     I2
                   ELSE
                           MOVE    1           TO     I2
                   END-IF

                   MOVE    SPACE       TO      PIN1-REC
                   MOVE    0           TO      J
      *    *** BYTE を １６進数に変換
                   PERFORM VARYING I FROM I2 BY 1
                           UNTIL I > WK-PIN1-LEN
                     IF  WK-PIN1-REC (I:6) = "%0D%0A"
                         ADD     5          TO      I
                     ELSE
                       IF  WK-PIN1-REC (I:1) = "%"
                       AND WK-PIN1-F-NAME (1:9) NOT = "bookmarks"
                           MOVE    WK-PIN1-REC (I:3) TO WK-STR
                           CALL    "DECODE01" USING   WK-STR
                                                      WK-VAL
                           ADD     1          TO      J
                           MOVE    WK-VAL     TO      PIN1-REC (J:1)
                           ADD     2          TO      I
                       ELSE
                           ADD     1          TO      J
                           MOVE    WK-PIN1-REC (I:1) TO PIN1-REC (J:1)
                       END-IF
                     END-IF
                   END-PERFORM
                   MOVE    J           TO      WK-PIN1-LEN
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-PIN1-STATUS=" WK-PIN1-STATUS
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

      *     IF WK-PIN1-CNT <= 100
      *        CALL "COBDUMP" USING  PIN1-REC WK-Buf-L
      *     END-IF
           MOVE    ZERO        TO      WK-FIND
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-PIN1-LEN
      *     IF WK-PIN1-CNT >= 0 AND <= 121
      *       DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *       DISPLAY "I=" I
      *       DISPLAY "PIN1-REC (I:10)=" PIN1-REC (I:10)
      *     END-IF
                   IF     PIN1-REC(I:1) =     "<"
                       MOVE    +1      TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL   PIN1-REC(J:1) =       ">" OR
                                   J           >         WK-PIN1-LEN
                           ADD     1           TO      L
                       END-PERFORM
      *    *** XVI 対応
      *     IF    WK-POT1-CNT >= 17500 AND  <= 17900
      *       DISPLAY PIN1-REC(I:29) " " WK-POT1-CNT
      *     END-IF
                       IF  PIN1-REC(I:29) = 
                           "<span class=""flag-small flag-"
                           MOVE  PIN1-REC(I + 29:4) TO     WK-FLAG
                           IF  WK-FLAG (3:1) = """"
                               MOVE    SPACE    TO     WK-FLAG (3:1)
                           END-IF
                           IF  WK-FLAG (4:1) = ">"
                               MOVE    SPACE    TO     WK-FLAG (4:1)
                           END-IF
                       END-IF
      *    *** キャスト https: 前に　name="times_auto-tag" が
      *    *** 追加されたので削除して、出力　201907より
                       IF  PIN1-REC(I:24) = '<a name="times_auto-tag"'
                           MOVE    PIN1-REC(I:3) TO      POT1-REC(1:3)
                           COMPUTE P  = I + 25
                           COMPUTE L2 = L - 25
                           MOVE    PIN1-REC(P:L2) TO     POT1-REC(4:L2)
                           COMPUTE L3 = 4 + L2
                           IF  POT1-REC (L3:1) NOT = ">"
                               MOVE    WK-FLAG        TO  POT2-FLAG
                               MOVE    ","            TO  POT2-KANMA
                               MOVE    PIN1-REC(1:3 ) TO  POT2-REC1(1:3)
                               MOVE    PIN1-REC(P:L2) TO POT2-REC1(4:L2)
      *                         IF  POT2-REC1(1:1) = SPACE
      *                             WRITE   POT2-REC
      *                             ADD     1         TO      WK-POT2-CNT
      *                         END-IF
                           END-IF
                       ELSE
                           MOVE    PIN1-REC(I:L) TO      POT1-REC
                           IF  POT1-REC (L:1) NOT = ">"
                               MOVE    WK-FLAG        TO  POT2-FLAG
                               MOVE    ","            TO  POT2-KANMA
                               MOVE    PIN1-REC(I:L) TO      POT2-REC1
      *                         IF  POT2-REC1(1:1) = SPACE
      *                             WRITE   POT2-REC
      *                             ADD     1         TO      WK-POT2-CNT
      *                         END-IF
                           END-IF
                       END-IF
                       WRITE   POT1-REC
                       ADD     1           TO        WK-POT1-CNT
                       MOVE    J           TO        I
                   ELSE
      *                    IF WK-PIN1-CNT = 81 OR 82
      *                     CALL "COBDUMP" USING  PIN1-REC WK-Buf-L
      *                    END-IF
      *    *** X"09"はＴａｂ
                       IF      PIN1-REC(I:1) =     X"09" 
      *                        ADD     +1    TO   I
                           CONTINUE
                       ELSE
                           MOVE    +1      TO      L
                           PERFORM VARYING J FROM I BY 1
                               UNTIL   PIN1-REC(J:1) =       "<" OR
                                       J      >     WK-PIN1-LEN      OR
                                     ( L             >      1    AND
                                       PIN1-REC(J:4) IS NUMERIC  AND
      *    *** 年
                                       PIN1-REC(J + 4:3) = X"E5B9B4" )
                               ADD     1           TO      L
                           END-PERFORM
                           ADD     -1            TO      L

      *    *** XVI 対応
                           IF  PIN1-REC(I:29) = 
                               "<span class=""flag-small flag-"
                               MOVE  PIN1-REC(I + 29:4) TO     WK-FLAG
                               IF  WK-FLAG (3:1) = """"
                                   MOVE    SPACE    TO     WK-FLAG (3:1)
                               END-IF
                               IF  WK-FLAG (4:1) = ">"
                                   MOVE    SPACE    TO     WK-FLAG (4:1)
                               END-IF
                           END-IF

                           MOVE    PIN1-REC(I:L) TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1             TO      WK-POT1-CNT
                           MOVE    SPACE         TO      POT1-REC
      *     IF  POT1-REC = " Ashley Storm"
      *         DISPLAY WK-POT1-CNT " i=" I
      *     END-IF
                           IF  POT1-REC (L:1) NOT = ">"
                               MOVE    WK-FLAG       TO      POT2-FLAG
                               MOVE    ","           TO      POT2-KANMA
                               MOVE    PIN1-REC(I:L) TO      POT2-REC1
      *                         IF  POT2-REC1(1:1) = SPACE
      *                             WRITE   POT2-REC
      *                             ADD     1         TO      WK-POT2-CNT
      *                         END-IF
                           END-IF
                           COMPUTE I =     J  - 1
      *                    IF WK-POT1-CNT = 4
      *                     CALL "COBDUMP" USING  POT1-REC WK-Buf-L
      *                    END-IF
                       END-IF
                   END-IF
           END-PERFORM
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

           MOVE    WK-PIN1-LEN-MAX TO  WK-PIN1-LEN-MAX-E
           DISPLAY WK-PGM-NAME " PIN1 ﾚﾝｸﾞｽ= " WK-PIN1-LEN-MAX-E

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
