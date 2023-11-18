      *    *** expo_jam 用
      *    *** http:// 行の出力
      *    *** <img src=と<div class=""externalLink直後の
      *    *** <a href=https
      *    *** アイドルデータの出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST13.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   "TEST10.POT1"
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   "TEST13.POT1"
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(10000)

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST13  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST13.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST13.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HTML1        PIC  X(1024) VALUE SPACE.
           03  WK-HTML2        PIC  X(1024) VALUE SPACE.
           03  WK-HTML3        PIC  X(1024) VALUE SPACE.
           03  WK-DD-DATA      PIC  X(10000) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-LINK         PIC  X(001) VALUE ZERO.
           03  SW-H2           PIC  X(001) VALUE ZERO.
           03  SW-DD           PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-DD-P         BINARY-LONG SYNC VALUE +1.
           03  SV-DD-L         BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** html <> 分解
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

      *    *** html <> 分解
       S100-10.
           
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > WK-PIN1-LEN
                   IF      PIN1-REC(I:1) =     "<" 
                       MOVE    +1      TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL   PIN1-REC(J:1) =     ">"
                           ADD     1           TO      L
                       END-PERFORM
                       MOVE    PIN1-REC(I:L) TO      WK-HTML1
      *                WRITE   POT1-REC
      *                ADD     1           TO        WK-POT1-CNT

      *    *** html 解析
                       PERFORM S110-10     THRU      S110-EX
                       MOVE    WK-PIN1-LEN     TO        I
                  ELSE
                          IF    SW-H2   =     "1"
                                MOVE    ZERO      TO    L
                                PERFORM VARYING J FROM 1 BY 1
                                    UNTIL PIN1-REC(J:2) = SPACE
                                    ADD     1           TO      L
                                END-PERFORM
                                MOVE   PIN1-REC    TO   WK-HTML3
                                MOVE   L      TO    SV-L
                                MOVE   ZERO   TO    SW-H2
                                MOVE   WK-PIN1-LEN TO   I
                          ELSE
                            IF    SW-DD   =     "1"
                                MOVE    ZERO      TO    L
                                PERFORM VARYING J FROM 1 BY 1
                                    UNTIL PIN1-REC(J:2) = SPACE
                                    ADD     1           TO      L
                                END-PERFORM
                                MOVE   PIN1-REC(1:L) TO
                                       WK-DD-DATA (SV-DD-P:L)
                                ADD    L      TO    SV-DD-P
                                ADD    L      TO    SV-DD-L
                                MOVE   WK-PIN1-LEN TO   I
                             ELSE
                                MOVE   WK-PIN1-LEN TO   I
                             END-IF
                          END-IF
                  END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** html 解析
       S110-10.

           EVALUATE TRUE

               WHEN WK-HTML1(1:9) =     "<img src="
                   COMPUTE K = L - 12
                   MOVE    WK-HTML1(11:K) TO   WK-HTML2
                   MOVE    K           TO      SV-K

               WHEN WK-HTML1(1:24) = "<div class=""externalLink"
                   MOVE    "1"         TO      SW-LINK

               WHEN WK-HTML1 (1:8) = "<a href=" AND
                    SW-LINK     =      "1"

      *    ***  POT1 WRITE
                   PERFORM S120-10     THRU    S120-EX
                   MOVE    ZERO        TO      SW-LINK

               WHEN WK-HTML1(1:4) = "<h2>"
                   MOVE    "1"         TO      SW-H2
                   MOVE    SPACE       TO      WK-DD-DATA
                   MOVE    +1          TO      SV-DD-P
                   MOVE    ZERO        TO      SV-DD-L

               WHEN WK-HTML1(1:4) = "<dd>"
                   MOVE    "1"         TO      SW-DD

               WHEN WK-HTML1(1:5) = "</dd>"
                   MOVE    ZERO        TO      SW-DD

               WHEN OTHER 
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.
           MOVE    WK-HTML2 (1:SV-K) TO POT1-REC

           ADD     +1          TO      SV-K
           MOVE    ","         TO      POT1-REC (SV-K:1)

           ADD     +1          TO      SV-K
           COMPUTE K = L - 27
           MOVE    WK-HTML1 (10:K) TO  POT1-REC (SV-K:K)

           ADD     K           TO      SV-K
           MOVE    ","         TO      POT1-REC (SV-K:1)

           ADD     +1          TO      SV-K
           MOVE    WK-HTML3 (1:SV-L) TO  POT1-REC (SV-K:SV-L)

           ADD     SV-L        TO      SV-K
           MOVE    ","         TO      POT1-REC (SV-K:1)

           ADD     +1          TO      SV-K
           MOVE    WK-DD-DATA(1:SV-DD-L) TO POT1-REC (SV-K:SV-DD-L)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      WK-HTML1  WK-HTML2 WK-HTML3
           MOVE    SPACE       TO      WK-DD-DATA
           MOVE    +1          TO      SV-DD-P
           MOVE    ZERO        TO      SV-DD-L
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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 Mｹﾝｽｳ= " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
