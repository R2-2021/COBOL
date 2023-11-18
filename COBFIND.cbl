      *    *** CALL,COPY 使用プログラムチェック
      *    *** 
      *    *** 入力はDIR *.CBL 内容をPIN1にコピーしておく
      *    *** 
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBFIND.

       ENVIRONMENT             DIVISION.
       Configuration section.
       repository.
           function all intrinsic.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(036).
           03  PIN1-F-NAME     PIC  X(032).
           03                  PIC  X(012).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-PGID       PIC  X(010) VALUE SPACE.
           03  POT1-DATA.
             05  POT1-DT01     PIC  X(020) VALUE SPACE.
             05  POT1-DT02     PIC  X(020) VALUE SPACE.
             05  POT1-DT03     PIC  X(020) VALUE SPACE.
             05  POT1-DT04     PIC  X(020) VALUE SPACE.
             05  POT1-DT05     PIC  X(020) VALUE SPACE.
             05  POT1-LINENO   PIC  9(006) VALUE ZERO.
             05  POT1-F-NAME   PIC  X(032) VALUE SPACE.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBFIND".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBFIND.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBFIND.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CALL-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-COPY-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PGID-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-OTHER-CNT    BINARY-LONG SYNC VALUE ZERO.

           03  WK-UNST-PTR     BINARY-LONG SYNC VALUE ZERO.
           03  WK-STO-PTR      BINARY-LONG SYNC VALUE ZERO.
           03  WK-LINENO       BINARY-LONG SYNC VALUE ZERO.

           03  WK-DELI1        PIC  X(001) VALUE SPACE.
           03  WK-DELI2        PIC  X(001) VALUE SPACE.
           03  WK-DELI3        PIC  X(001) VALUE SPACE.
           03  WK-DELI4        PIC  X(001) VALUE SPACE.
           03  WK-DELI5        PIC  X(001) VALUE SPACE.

           03  WK-PIN1DT.
             05  WK-PIN1-DT01  PIC  X(020) VALUE SPACE.
             05  WK-PIN1-DT02  PIC  X(020) VALUE SPACE.
             05  WK-PIN1-DT03  PIC  X(020) VALUE SPACE.
             05  WK-PIN1-DT04  PIC  X(020) VALUE SPACE.
             05  WK-PIN1-DT05  PIC  X(020) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  L5              BINARY-LONG SYNC VALUE ZERO.

      *    *** F-NAME にはXXXX.CBLる
       01  TBL-AREA.
           03  TBL01-F-NAME    OCCURS 500
                               PIC  X(032) VALUE SPACE.

       01  SAVE-AREA.
           03  SV-PGID         PIC  X(010) VALUE SPACE.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "0".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN POT1
           PERFORM S010-10     THRU    S010-EX

      *    *** OPEN PIN1
           PERFORM S011-10     THRU    S011-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

      *    *** TBL SET
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE PIN1
           PERFORM S012-10     THRU    S012-EX

      *    *** PIN1を流用する、XXX.CBL 各ファイル入力に使う
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > I-MAX

      *    *** OPEN PIN1
                   MOVE    TBL01-F-NAME (I) TO WK-PIN1-F-NAME
                   PERFORM S011-10     THRU    S011-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

      *    *** FIND CHECK WRITE POT1
                           PERFORM S200-10     THRU    S200-EX

      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

      *    *** CLOSE PIN1
                   PERFORM S012-10     THRU    S012-EX
           END-PERFORM

      *    *** CLOSE POT1
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN POT1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** OPEN PIN1
       S011-10.

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   DISPLAY WK-PGM-NAME " PIN1-F FILE NAME="
                           WK-PIN1-F-NAME
                   STOP    RUN
           END-IF
           MOVE    LOW-VALUE   TO      WK-PIN1-EOF
           MOVE    ZERO        TO      WK-LINENO
           .
       S011-EX.
           EXIT.

      *    *** CLOSE PIN1
       S012-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   DISPLAY WK-PGM-NAME " PIN1-F FILE NAME="
                           WK-PIN1-F-NAME
                   STOP    RUN
           END-IF
           .
       S012-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                                               WK-LINENO
           ELSE
                   IF      WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME " PIN1-F READ ERROR"
                                   " STATUS="  WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** TBL SET
       S100-10.

           ADD     1           TO      I
           IF      I           >       500
                   DISPLAY WK-PGM-NAME " TBL OVER MAX=500 I=" I
                   STOP    RUN
           END-IF

           MOVE    PIN1-F-NAME TO      TBL01-F-NAME (I)
           MOVE    I           TO      I-MAX
           .
       S100-EX.
           EXIT.

      *    *** FIND CHECK WRITE POT1
       S200-10.

           MOVE    ZERO        TO      WK-CALL-CNT
                                       WK-COPY-CNT
                                       WK-PGID-CNT
                                       WK-OTHER-CNT
                                       L1 L2 L3 L4 L5

      *    *** 左寄せ
           CALL    "C$JUSTIFY" USING   PIN1-REC "L"
      *    *** 大文字変換
           CALL    "C$TOUPPER" USING   PIN1-REC , BY VALUE WK-PIN1-LEN

           INSPECT PIN1-REC TALLYING
                   WK-PGID-CNT FOR ALL "PROGRAM-ID"
                   WK-CALL-CNT FOR ALL "CALL"
                   WK-COPY-CNT FOR ALL "COPY"
      *             WK-OTHER-CNT FOR ALL "PIN1"
                   WK-OTHER-CNT FOR ALL "INCLUDE"

      *     IF      WK-OTHER-CNT NOT =  ZERO
      *             DISPLAY "SV-PGID=" SV-PGID
      *     END-IF

      *     IF  WK-CALL-CNT > ZERO OR
      *         WK-COPY-CNT > ZERO
      *         DISPLAY "WK-CALL-CNT=" WK-CALL-CNT
      *                 "WK-COPY-CNT=" WK-COPY-CNT
      *     END-IF

           IF      WK-CALL-CNT >       ZERO OR
                   WK-COPY-CNT >       ZERO OR
      *     IF      WK-OTHER-CNT >      ZERO OR
                   WK-PGID-CNT >       ZERO

      *     DISPLAY "SV-PGID=" SV-PGID
      *             " WK-PGID-CNT=" WK-PGID-CNT
      *             " WK-CALL-CNT=" WK-CALL-CNT

      *            MOVE    "P"         TO      WFD-ID
      *            MOVE    1           TO      WFD-SU
      *            MOVE    "A"         TO      WFD-TYPE
      *            CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        PIN1-REC

                   MOVE    1           TO      WK-UNST-PTR
                                               WK-STO-PTR
      *    *** UNSTRING PIN1-REC
      *    ***     DELIMITED BY ALL SPACE => 複数スペースを１つと数える
      *    *** UNSTRING PIN1-REC
      *    ***     DELIMITED BY SPACE     => １個スペースを１つと数える
                   UNSTRING PIN1-REC
                       DELIMITED BY ALL SPACE OR '"' OR "." OR "'"
                       INTO WK-PIN1-DT01 DELIMITER WK-DELI1 COUNT L1 
                            WK-PIN1-DT02 DELIMITER WK-DELI2 COUNT L2 
                            WK-PIN1-DT03 DELIMITER WK-DELI3 COUNT L3 
                            WK-PIN1-DT04 DELIMITER WK-DELI4 COUNT L4 
                            WK-PIN1-DT05 DELIMITER WK-DELI5 COUNT L5 
                       WITH POINTER WK-UNST-PTR

                   IF      WK-PGID-CNT >       ZERO
                       IF      WK-PIN1-DT01(1:2) = "*>" OR
                               PIN1-REC (7:1) =    "*"
                               CONTINUE
                       ELSE
                           IF      WK-PIN1-DT01 =      "PROGRAM-ID"
                                       MOVE    WK-PIN1-DT03 TO   SV-PGID
                           END-IF
                       END-IF
                   END-IF

                   MOVE    SV-PGID     TO      POT1-PGID
                   MOVE    WK-PIN1-DT01 TO     POT1-DT01
                   MOVE    WK-PIN1-DT02 TO     POT1-DT02
                   MOVE    WK-PIN1-DT03 TO     POT1-DT03
                   MOVE    WK-PIN1-DT04 TO     POT1-DT04
                   MOVE    WK-PIN1-DT05 TO     POT1-DT05
                   MOVE    WK-LINENO   TO      POT1-LINENO
                   MOVE    WK-PIN1-F-NAME TO   POT1-F-NAME

      *            MOVE    "P"         TO      WFD-ID
      *            MOVE    2           TO      WFD-SU
      *            MOVE    "A"         TO      WFD-TYPE
      *            CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        PIN1-REC

                   IF      WK-PIN1-DT01(1:2) = "*>" OR
                           PIN1-REC (7:1) =    "*"  OR
                           WK-PGID-CNT  >      ZERO 
                           CONTINUE
                   ELSE
                       IF      WK-PIN1-DT01(1:4) = "CALL" OR "COPY"
      *                 IF      WK-PIN1-DT01(1:8) = "DATE-RAP"
      *                 IF      WK-PIN1-DT01 = "DISPLAY" AND
      *                         WK-PIN1-DT02 = "WK-PGM-NAME" AND
      *                         WK-PIN1-DT05 = "(PIN1="
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                           IF      WK-POT1-STATUS NOT = ZERO
                                   DISPLAY WK-PGM-NAME
                                           " POT1-F WRITE ERROR"
                                           " STATUS="  WK-POT1-STATUS
                                   STOP    RUN
                           END-IF
                       END-IF
                   END-IF
           END-IF

           .
       S200-EX.
           EXIT.

      *    *** CLOSE POT1
       S900-10.

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

      *    *** PIN1-F-NAME LAST FILE NAME
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
