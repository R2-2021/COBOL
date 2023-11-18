      *    *** 文字コード表の出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST15.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
       SELECT PIN1-F           ASSIGN   "TEST15.PIN1"
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ＳＪＩＳデータ
       SELECT POT1-F           ASSIGN   "TEST15.POT1"
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ＵＴＦ８データ
       SELECT POT2-F           ASSIGN   "TEST15.POT2"
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100)

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
      *     03  FILLER          PIC  X(376).
           03  FILLER          PIC  X(500).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(500).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST15  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST15.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST15.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST15.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-REC          PIC  X(500) VALUE SPACE.

           03  WK-S.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-S-X        PIC  X(001) VALUE LOW-VALUE.
           03  WK-S-R          REDEFINES   WK-S
                               PIC  9(004) COMP-X.

           03  WK-E.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-E-X        PIC  X(001) VALUE LOW-VALUE.
           03  WK-E-R          REDEFINES   WK-E
                               PIC  9(004) COMP-X.
           03  WK-S2.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-S2-X       PIC  X(001) VALUE LOW-VALUE.
           03  WK-S2-R         REDEFINES   WK-S2
                               PIC  9(004) COMP-X.

           03  WK-E2.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-E2-X       PIC  X(001) VALUE LOW-VALUE.
           03  WK-E2-R         REDEFINES   WK-E2
                               PIC  9(004) COMP-X.

           03  WK-D.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-D-X        PIC  X(001) VALUE LOW-VALUE.
           03  WK-D-R          REDEFINES   WK-D
                               PIC  9(004) COMP-X.

           03  WK-D2.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-D2-X       PIC  X(001) VALUE LOW-VALUE.
           03  WK-D2-R         REDEFINES   WK-D2
                               PIC  9(004) COMP-X.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE 0.
           03  J               BINARY-LONG SYNC VALUE 0.
           03  K               BINARY-LONG SYNC VALUE 0.
           03  L               BINARY-LONG SYNC VALUE 0.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE ZERO.
           03  SW-AMP          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL   WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1,POT2
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

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
                                       WK-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC
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

      *    *** WRITE POT1,POT2
       S100-10.

      *    *** ASCII/JISローマ字　21～7E  (先頭のビットが0)
           MOVE    X"21"       TO      WK-S-X
           MOVE    X"7E"       TO      WK-E-X

           MOVE    0           TO      J
           MOVE    SPACE       TO      WK-REC
           PERFORM VARYING I FROM WK-S-R BY 1
                   UNTIL   I   >       WK-E-R
                   MOVE    I           TO      WK-D-R
                   ADD     1           TO      J
                   MOVE    WK-D-X      TO      WK-REC (J:1)
           END-PERFORM

           MOVE    "P"         TO      WFD-ID
           ADD     1           TO      WFD-SEQ
           MOVE    J           TO      WFD-LEN
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-REC
                                       WFD-LEN

      *
      *    *** JIS X0201片仮名　　A0～DF  (先頭のビットが1)
           MOVE    X"A0"       TO      WK-S-X
           MOVE    X"DF"       TO      WK-E-X
           
           MOVE    0           TO      J
           MOVE    SPACE       TO      WK-REC
           PERFORM VARYING I FROM WK-S-R BY 1
                   UNTIL   I   >       WK-E-R
                   MOVE    I           TO      WK-D-R
                   ADD     1           TO      J
                   MOVE    WK-D-X      TO      WK-REC (J:1)
           END-PERFORM
      *
      *    *** 
           MOVE    "P"         TO      WFD-ID
           ADD     1           TO      WFD-SEQ
           MOVE    J           TO      WFD-LEN
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-REC
                                       WFD-LEN
      *    *** JIS X 208(JIS漢字)
      *    *** 第1バイト　81～9FとE0からEF (A0～DFを避ける)
      *    *** 第2バイト　40～7Eと80からFC 
           MOVE    X"81"       TO      WK-S-X
           MOVE    X"EF"       TO      WK-E-X
           
           MOVE    X"40"       TO      WK-S2-X
           MOVE    X"FC"       TO      WK-E2-X
           
           MOVE    0           TO      J
           MOVE    SPACE       TO      WK-REC
           PERFORM VARYING I FROM WK-S-R BY 1
                   UNTIL   I   >       WK-E-R
               MOVE    I           TO      WK-D-R
               IF      WK-D-X     =       X"A0" 
                   MOVE    X"DF"       TO      WK-S-X
                   MOVE    WK-S-R      TO      I
               ELSE
                   PERFORM VARYING K FROM WK-S2-R BY 1
                       UNTIL   K   >       WK-E2-R
                       MOVE    K           TO      WK-D2-R
                       IF      WK-D2-X     =       X"7F"
                           CONTINUE
                       ELSE
                           ADD     1           TO      J
                           MOVE    WK-D-X      TO      WK-REC (J:1)
                           COMPUTE L = J + 1
                           MOVE    WK-D2-X     TO      WK-REC (L:1)
                           MOVE    L           TO      J
                       END-IF
                   END-PERFORM
      *    *** 
                   MOVE    "P"         TO      WFD-ID
                   ADD     1           TO      WFD-SEQ
      *             MOVE    J           TO      WFD-LEN
                   MOVE    L           TO      WFD-LEN
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-REC
                                               WFD-LEN

      *             WRITE   POT1-REC    FROM    WK-REC
      *             ADD     1           TO      WK-POT1-CNT



      *    *** HENKAN=SU SJIS => UTF8
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "SU"        TO      WDE05-HENKAN
                   MOVE    "AK"        TO      WDE05-MODE
                   MOVE    500         TO      WDE05-BUF1-LEN
                                               WDE05-BUF2-LEN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-REC
                                               POT2-REC

                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT



      *    *** HENKAN=US UTF8 => SJIS
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    "US"        TO      WDE05-HENKAN
                   MOVE    500         TO      WDE05-BUF1-LEN
                                               WDE05-BUF2-LEN
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               POT2-REC
                                               POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT


                   MOVE    0           TO      J
                   MOVE    SPACE       TO      WK-REC

                   MOVE    X"40"       TO      WK-S2-X
                   MOVE    X"FC"       TO      WK-E2-X

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
                                       WK-REC

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       POT1-REC
                                       POT2-REC

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
