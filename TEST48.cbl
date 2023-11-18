      *    *** UTF8 <=> SJIS 変換

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST48.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** パラメータデータ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** コード変換前データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** コード変換後データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F.
       01  PRM1-REC.
           03  FILLER          PIC  X(100).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(65535).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(65535).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST48  ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST48.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST48.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST48.POT1".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           03  WK-PRM1DT.
             05  WK-PRM1DT01   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT02   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT03   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT04   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT05   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT06   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT07   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT08   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT09   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT10   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT11   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT12   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT13   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT14   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT15   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT16   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT17   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT18   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT19   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT20   PIC  X(032) VALUE SPACE.
           03  WK-PRM1DT-R     REDEFINES WK-PRM1DT.
             05  WK-PRM1DT-T   OCCURS 20
                               PIC  X(032).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

      *    *** 変換前 が入っているデータ
      * 01  WDE05-BUF1           PIC  X ANY LENGTH.

      *    *** 変換後 が入っているデータ SPACE(X"20") クリアー後セット
      *    *** BUF2 変換後長さ短い時、項目長内でセットする
      *    *** 余ったエリアは SPACE(X"20") が入る 長い時も同様である
      * 01  WDE05-BUF2           PIC  X ANY LENGTH.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-POS     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-TYPE    PIC  X(002) VALUE SPACE.
             05  TBL01-SHIFT   PIC  X(001) VALUE SPACE.

             05  TBL01-LEN     BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "Y".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S050-10     THRU    S050-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF   =         HIGH-VALUE
      *    *** コード変換
                   PERFORM S100-10     THRU      S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU      S020-EX
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

           ACCEPT  WK-ARGUMENT-NUMBER FROM      ARGUMENT-NUMBER

      *    *** PRM1-F 指定無し（ARGUMENT-NUMBER=0）、既定値使用
      *    *** ARGUMENT-NUMBER=1 の時、PRM1-F 指定する
           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-PRM1-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PRM1-F-NAME

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PRM1-F 1個まで指定可"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS NOT = ZERO AND 10
                   DISPLAY WK-PGM-NAME 
                           " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PRM1
       S050-10.

           PERFORM UNTIL WK-PRM1-EOF =   HIGH-VALUE
               READ    PRM1-F
               IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
                   UNSTRING PRM1-REC
                       DELIMITED BY "," OR "=" OR SPACE
                       INTO
                       WK-PRM1DT01
                       WK-PRM1DT02
                       WK-PRM1DT03
                       WK-PRM1DT04
                       WK-PRM1DT05
                       WK-PRM1DT06
                       WK-PRM1DT07
                       WK-PRM1DT08
                       WK-PRM1DT09
                       WK-PRM1DT10
                       WK-PRM1DT11
                       WK-PRM1DT12
                       WK-PRM1DT13
                       WK-PRM1DT14
                       WK-PRM1DT15
                       WK-PRM1DT16
                       WK-PRM1DT17
                       WK-PRM1DT18
                       WK-PRM1DT19
                       WK-PRM1DT20

                   EVALUATE TRUE
      *                 WHEN WK-PRM1DT01(1:3) =    "POS"
      *    *** POS= チェック
      *                     PERFORM S051-10     THRU    S051-EX

                       WHEN WK-PRM1DT01(1:6) =    "HENKAN"
      *    *** HENKAN= チェック
                           PERFORM S052-10     THRU    S052-EX

                       WHEN WK-PRM1DT01(1:4) =    "MODE"
      *    *** MODE= チェック
                           PERFORM S053-10     THRU    S053-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-IN"
      *    *** F-IN= チェック
                           PERFORM S054-10     THRU    S054-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-OT"
      *    *** F-OT= チェック
                           PERFORM S055-10     THRU    S055-EX

                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   IF      WK-PRM1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
               END-IF
           END-PERFORM

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
           .
       S050-EX.
           EXIT.

      *    *** S051-10 現在、未使用
      *    *** POS= CHECK
       S051-10.

      *    *** 指定が無い項目は、そのまま出力
           IF      FUNCTION NUMVAL(WK-PRM1DT02) >=     1 AND
                   FUNCTION NUMVAL(WK-PRM1DT02) <=     100
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT02) TO P
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F POS=N PARA ERROR="
                           PRM1-REC
                 DISPLAY WK-PGM-NAME " POS=N 1-100の範囲で違う数字 指定"
                   STOP    RUN
           END-IF

           ADD     1           TO      TBL01-CNT (P)
           IF      TBL01-CNT (P) >     1
                   DISPLAY WK-PGM-NAME " PRM1-F POS=N PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " POS=N N同じ数字を指定している"
                           " 1-100の範囲で違う数字 指定"
                   STOP    RUN
           END-IF

      *    *** TYPE,SHIFT は未使用
           IF      WK-PRM1DT03(1:4) = "TYPE"
                   IF      WK-PRM1DT04(1:2) = "CH" OR "ZD"
                           MOVE    WK-PRM1DT04 TO      TBL01-TYPE(P)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F POS=N TYPE= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F POS=N"
                                   " TYPE= CH,ZDで指定"
                           STOP    RUN
                   END-IF

           END-IF

           IF      WK-PRM1DT05(1:5) = "SHIFT"
                   IF      WK-PRM1DT06(1:1) = "L" OR "C" OR "R"
                           MOVE    WK-PRM1DT06 TO      TBL01-SHIFT(P)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F POS=N SHIFT= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " POS=N SHIFT= L,C,Rで指定"
                                   " L:左寄せ､C=中央寄せ､R=右寄せ"
                           STOP    RUN
                   END-IF
           END-IF
           .
       S051-EX.
           EXIT.

      *    *** HENKAN= チェック
       S052-10.

           IF      WK-PRM1DT02 (1:2) = "SU" OR "US"
                   MOVE    WK-PRM1DT02 TO      WK-HENKAN
           ELSE
                   DISPLAY WK-PGM-NAME " HENKAN=XX ERROR" 
                           " US(UTF8=>SJIS) OR SU(SJIS=>UTF8) を指定"
                   STOP    RUN
           END-IF
           .
       S052-EX.
           EXIT.

      *    *** MODE= チェック
       S053-10.

           IF      WK-PRM1DT02 (1:2) = "AA" OR "AK"
                   MOVE    WK-PRM1DT02 TO      WK-MODE
           ELSE
                   DISPLAY WK-PGM-NAME " MODE=XX ERROR" 
                           " AA(ANK=>ANK) OR AK(ANK=>KANJI) を指定"
                   STOP    RUN
           END-IF
           .
       S053-EX.
           EXIT.

      *    *** F-IN CHECK
       S054-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                           ACCEPT  WK-PIN1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-PIN1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-PIN1-F-NAME
           END-IF
           .
       S054-EX.
           EXIT.

      *    *** F-OT CHECK
       S055-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " OUTPUT FILE NAME"
                           ACCEPT  WK-POT1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-POT1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-POT1-F-NAME
           END-IF
           .
       S055-EX.
           EXIT.

      *    *** コード変換
       S100-10.

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    WK-PIN1-LEN TO      WDE05-BUF1-LEN
           MOVE    65535       TO      WDE05-BUF2-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

      *    *** 﨑 変換出来ない、崎は出来る ＵＴＦ８＝＞ＳＪＩＳ
      *     IF      WK-PIN1-CNT =       73 OR 229 OR 433 OR 434 OR 448 OR
      *                                452 OR 497 OR 654 OR 737 
      *             MOVE    "X"         TO      WFD-ID
      *             MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC (21:15)
      *     END-IF

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

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

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           DISPLAY WK-PGM-NAME " DECODE05 ERROR 有のとき、"
                   "TEST17.POT1 SJIS CHECK し,KANJI\KANJI1.txt 変更する"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
