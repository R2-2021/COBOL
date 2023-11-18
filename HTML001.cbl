      *    *** HTML TABLE XXnnnn,jpg n件自動作成
      *    *** 
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             HTML001.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
           03  PRM1-ID         PIC  X(002).
           03  FILLER          PIC  X(001).
           03  PRM1-FROM       PIC  9(006).
           03  FILLER          PIC  X(001).
           03  PRM1-TO         PIC  9(006).
           03  FILLER          PIC  X(001).
           03  PRM1-OCCURS     PIC  9(002).
           03  FILLER          PIC  X(001).
           03  PRM1-WIDTH      PIC  9(003).
           03  FILLER          PIC  X(001).
           03  PRM1-HEIGHT     PIC  9(003).
           03  FILLER          PIC  X(100).

      *    *** png 以外のDIR　インプットし、ファイル属性変更
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  FILLER          PIC  X(036).
           03  PIN1-FILENAME   PIC  X(008).
           03  PIN1-FILEZOKU   PIC  X(004).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(128).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "HTML001 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "HTML001.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "HTML001.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "HTML001.POT1".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-ID           PIC  X(002) VALUE SPACE.
           03  WK-FROM         PIC  9(006) VALUE 1.
           03  WK-TO           PIC  9(006) VALUE 1000.
           03  WK-OCCURS       PIC  9(002) VALUE 10.
           03  WK-WIDTH        PIC  9(003) VALUE 200.
           03  WK-HEIGHT       PIC  9(003) VALUE 256.

           03  WK-TD.
             05  FILLER           PIC  X(013) VALUE "<td><a href=""".
             05  WK-TD-A-FILENAME PIC  X(008) VALUE SPACE.
             05  WK-TD-A-FILEZOKU PIC  X(004) VALUE ".jpg".
             05  FILLER           PIC  X(002) VALUE """>".
             05  WK-TD-A-FILENAME2 PIC  X(008) VALUE SPACE.
             05  WK-TD-A-FILEZOKU2 PIC  X(004) VALUE ".jpg".
             05  FILLER           PIC  X(018) VALUE
                 "</a><br><img src=""".
             05  WK-TD-IMG-FILENAME.
               07  WK-TD-IMG-ID   PIC  X(002) VALUE SPACE.
               07  WK-TD-IMG-NUM  PIC  9(006) VALUE ZERO.
             05  WK-TD-IMG-FILEZOKU PIC  X(004) VALUE ".jpg".
             05  FILLER           PIC  X(007) VALUE """ alt=""".
             05  WK-TD-ALT-ID     PIC  X(002) VALUE SPACE.
             05  WK-TD-ALT-NUM    PIC  9(006) VALUE ZERO.
             05  WK-TD-ALT-FILEZOKU PIC  X(004) VALUE ".jpg".
             05  FILLER           PIC  X(009) VALUE """ width=""".
             05  WK-TD-WIDTH      PIC  9(003) VALUE ZERO.
             05  FILLER           PIC  X(010) VALUE """ height=""".
             05  WK-TD-HEIGHT     PIC  9(003) VALUE ZERO.
             05  FILLER           PIC  X(007) VALUE """></td>".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  TBL-AREA,
           03  TBL01-AREA      OCCURS 10000.
             05  TBL01-FILENAME PIC  X(008) VALUE SPACE.
             05  TBL01-FILEZOKU PIC  X(004) VALUE SPACE.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  T1              BINARY-LONG SYNC VALUE ZERO.
           03  T1-MAX          BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1 TBL01 SET
           PERFORM S030-10     THRU    S030-EX

      *    *** HTML TABLE WRITE
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

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F.
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF.

           OPEN    OUTPUT      POT1-F.
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID.
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PRM1
       S020-10.

           READ    PRM1-F

           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
                   IF      WK-PRM1-STATUS =    10
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF      PRM1-ID     =       SPACE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F ID(1:2) SPACE 以外指定する"
                           PRM1-REC
                   STOP    RUN
           ELSE
                   MOVE    PRM1-ID     TO      WK-ID
           END-IF

           IF      PRM1-FROM   IS      NUMERIC
                   MOVE    PRM1-FROM   TO      WK-FROM
           ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F FROM(4:6) 開始Ｎｏ指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF

           IF      PRM1-TO     IS      NUMERIC
                   MOVE    PRM1-TO     TO      WK-FROM
           ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F TO(11:6) 終了Ｎｏ指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF

           IF      PRM1-OCCURS IS      NUMERIC
                   MOVE    PRM1-OCCURS TO      WK-OCCURS
           ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F OCCURS(18:2) 横個数、指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF

           IF      PRM1-WIDTH  IS      NUMERIC
                   MOVE    PRM1-WIDTH  TO      WK-WIDTH
           ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F WIDTH(21:3) width、指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF

           IF      PRM1-HEIGHT IS      NUMERIC
                   MOVE    PRM1-HEIGHT TO      WK-HEIGHT
           ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F HEIGHT(25:3) height、指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF

           .
       S020-EX.
           EXIT.

      *    *** PIN1 READ TBL01 SET
       S030-10.

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   READ    PIN1-F
                   IF      WK-PIN1-STATUS  =       ZERO
                       ADD     1           TO      WK-PIN1-CNT
                   ELSE
                       IF      WK-PIN1-STATUS  =       10
                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                       ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF

                   ADD     1           TO      T1
                   IF      T1          >       10000
                           DISPLAY WK-PGM-NAME 
                                   " TBL01 OVER T1=" T1
                           STOP    RUN
                   ELSE
                       MOVE    PIN1-FILENAME TO      TBL01-FILENAME (T1)
                       MOVE    PIN1-FILEZOKU TO      TBL01-FILEZOKU (T1)
                   END-IF
           END-PERFORM
           MOVE    T1          TO      T1-MAX
           .
       S030-EX.
           EXIT.

      *    *** HTML TABLE WRITE
       S100-10.

           PERFORM VARYING I FROM PRM1-FROM BY 1
                   UNTIL I > PRM1-TO

                   ADD     1           TO      J
                   IF      J           =       1
                       MOVE    "<tr>"      TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                   END-IF

                   MOVE    WK-ID       TO      WK-TD-IMG-ID
                                               WK-TD-ALT-ID
                   MOVE    I           TO      WK-TD-IMG-NUM
                                               WK-TD-ALT-NUM
                   MOVE    ".jpg"      TO      WK-TD-IMG-FILEZOKU
                                               WK-TD-ALT-FILEZOKU
                   MOVE    WK-WIDTH    TO      WK-TD-WIDTH
                   MOVE    WK-HEIGHT   TO      WK-TD-HEIGHT

                   PERFORM VARYING T1 FROM 1 BY 1
                           UNTIL T1 > T1-MAX
                       IF      WK-TD-IMG-FILENAME =  TBL01-FILENAME (T1)
                               MOVE    TBL01-FILEZOKU(T1) TO
                                       WK-TD-IMG-FILEZOKU
                                       WK-TD-ALT-FILEZOKU
                               MOVE    T1-MAX      TO      T1
                       END-IF
                   END-PERFORM
                   MOVE    WK-TD-IMG-FILENAME TO WK-TD-A-FILENAME
                                                 WK-TD-A-FILENAME2
                   MOVE    WK-TD-IMG-FILEZOKU TO WK-TD-A-FILEZOKU
                                                 WK-TD-A-FILEZOKU2

                   WRITE   POT1-REC    FROM    WK-TD
                   ADD     1           TO      WK-POT1-CNT

                   ADD     1           TO      K
                   IF      K           =       WK-OCCURS OR
                           I           =       PRM1-TO
                       MOVE    "</tr>"     TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT
                       MOVE    ZERO        TO      J
                                                   K
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PRM1-F.
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN1-F.
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F.
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
