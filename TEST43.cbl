      *    *** ＣＳＶデータ固定長にする
      *    *** 項目ごとの長さは、プログラムでセットして出力
      *    *** 部分転送 使用
      *    *** TEST42をコピー

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST43.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 任意ＣＳＶファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 項目ごとの長さ設定ファイル
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 固定長ファイル
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(10000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-NO         PIC  X(003).
           03  PIN2-LEN        PIC  9(003).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST43  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST42.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST43.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST43.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-I1      PIC  X(1000) VALUE SPACE.
           03  WK-UNST-PTR     BINARY-LONG SYNC VALUE ZERO.
           03  WK-STO-PTR      BINARY-LONG SYNC VALUE ZERO.
           03  WK-DELI         PIC  X(001) VALUE SPACE.
           03  WK-NUM-ITEM     PIC  X(010) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-LEN     BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-NUM          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** PIN2 READ
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL   WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 CSV 項目長テーブルにセット
                  PERFORM S200-10      THRU    S200-EX

      *    *** PIN2 READ
                  PERFORM S030-10      THRU    S030-EX
           END-PERFORM

      *    *** PIN1 READ
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PIN1 CSV を POT1 WRITE 固定長に変換
                  PERFORM S100-10      THRU    S100-EX

      *    *** PIN1 READ
                  PERFORM S020-10      THRU    S020-EX
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN1-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
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
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

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

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    2           TO      WFD-SU
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC
           .
       S030-EX.
           EXIT.

      *    *** PIN1 CSV を WRITE POT1 固定長に変換
       S100-10.

           MOVE    1           TO      WK-UNST-PTR
                                       WK-STO-PTR
                                       J
           MOVE    ZERO        TO      I
           MOVE    SPACE       TO      POT1-REC
                                       WK-PIN1-I1

           PERFORM UNTIL WK-UNST-PTR > WK-PIN1-LEN
                   UNSTRING PIN1-REC
                       DELIMITED BY ","
                       INTO WK-PIN1-I1 DELIMITER WK-DELI COUNT L
                       WITH POINTER WK-UNST-PTR

                   ADD     1           TO      I
      *    *** １０バイト以下の項目で数字、スペースのみで構成される項目は
      *    *** 右寄せに変換する
                   MOVE    "N"         TO      SW-NUM
                   IF      TBL01-LEN(I) <=     10
                       PERFORM VARYING K FROM 1 BY 1
                               UNTIL K > TBL01-LEN(I) OR
                                     K > 10
                           IF      WK-PIN1-I1 (K:1) =  SPACE OR
                                   WK-PIN1-I1 (K:1) IS  NUMERIC
                               MOVE    "Y"         TO      SW-NUM
                           ELSE
                               MOVE    10          TO      K
                           END-IF
                       END-PERFORM
                   END-IF

                   IF      SW-NUM      =       "Y"
                       MOVE    TBL01-LEN(I)  TO      K2
                       MOVE    WK-PIN1-I1(1:TBL01-LEN(I)) TO WK-NUM-ITEM
                       MOVE    SPACE       TO
                                             WK-PIN1-I1 (1:TBL01-LEN(I))
                       PERFORM VARYING K FROM TBL01-LEN(I) BY -1
                               UNTIL K < 1
                           IF      WK-NUM-ITEM (K:1) NOT = SPACE
                               MOVE    WK-NUM-ITEM (K:1) TO
                                                       WK-PIN1-I1 (K2:1)
                               ADD     -1          TO      K2
                           END-IF
                       END-PERFORM
                    END-IF

                   STRING WK-PIN1-I1 DELIMITED BY ","
                          INTO POT1-REC
                          WITH POINTER WK-STO-PTR
      *    *** Ｉの次の項目のセット位置、WK-STO-PTRセットする
                   COMPUTE J = TBL01-LEN(I) + J
                   MOVE    J           TO      WK-STO-PTR

      *             DISPLAY "WK-PIN1-I1 =" WK-PIN1-I1 (1:70)
      *             DISPLAY "WK-UNST-PTR=" WK-UNST-PTR
      *             DISPLAY "WK-STO-PTR =" WK-STO-PTR
      *             DISPLAY "WK-PIN1-LEN=" WK-PIN1-LEN
      *             DISPLAY "L          =" L
      *             DISPLAY "I          =" I
      *             DISPLAY "TBL01-LEN  =" TBL01-LEN(I)
           END-PERFORM

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PIN2 CSV 項目長テーブルにセット
       S200-10.

           ADD     1           TO      I

           IF      I           >       100
                   DISPLAY WK-PGM-NAME " PIN2-F TBL OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    PIN2-LEN    TO     TBL01-LEN (I)
           .
       S200-EX.
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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
