      *    *** FILEDUMP パラメータ ACCEPT で　ダンプ出力

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILEDMPP.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** パラメータINPUT
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** FILEDUMP INPUT LINE SEQUENTIAL
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** FILEDUMP INPUT BINARY SEQUENTIAL
      *    *** PACKED-DECIMAL 用 BINARY レコード最後にX"0D0A"必要
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

      *    *** パラメータINPUT
       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
           03  FILLER          PIC  X(080).

      *    *** FILEDUMP INPUT LINE SEQUENTIAL
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(65536).

      *    *** FILEDUMP INPUT BINARY SEQUENTIAL
       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
      *    *** BINARY の時、WK-PIN2-LENは REC定義レコード超えないと
      *    *** セットされない
       01  PIN2-REC.
           03  FILLER          PIC  X(65536).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILEDMPP".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "FILEDMPP.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "FILEDMPP.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "FILEDMPP.PIN2".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE 65536.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-0D0A-CNT
                               BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-0D0A-CNT-E
                               PIC --,---,---,--9 VALUE ZERO.

           03  WK-MODE         PIC  X(001) VALUE SPACE.
           03  WK-JYOKEN       PIC  X(080) VALUE SPACE.
           03  WK-KOMOKU       PIC  X(010) VALUE SPACE.
           03  WK-POS          PIC  X(005) VALUE SPACE.
           03  WK-LEN          PIC  X(005) VALUE SPACE.
           03  WK-YN           PIC  X(001) VALUE SPACE.

           03  WK-ITEM01       PIC  X(032) VALUE SPACE.
           03  WK-ITEM02       PIC  X(032) VALUE SPACE.
           03  WK-ITEM03       PIC  X(032) VALUE SPACE.
           03  WK-ITEM04       PIC  X(032) VALUE SPACE.
           03  WK-ITEM05       PIC  X(032) VALUE SPACE.
           03  WK-ITEM06       PIC  X(032) VALUE SPACE.
           03  WK-ITEM07       PIC  X(032) VALUE SPACE.
           03  WK-ITEM08       PIC  X(032) VALUE SPACE.
           03  WK-ITEM09       PIC  X(032) VALUE SPACE.
           03  WK-ITEM10       PIC  X(032) VALUE SPACE.

       01  WK-BUF2.
           03  WK-BUF2-TBL     OCCURS 65536
                               PIC  X(001) VALUE SPACE.

       01  WDE04-AREA.
           03  WDE04-SHORI     PIC  X(005) VALUE "FIRST".
           03  WDE04-BUF1-LEN  BINARY-LONG SYNC VALUE ZERO.

           03  WDE04-REC-LEN   BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "Y".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 20.
             05  TBL01-ITEM    PIC  X(010) VALUE SPACE.
             05  TBL01-POS     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-LEN     BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN PRM1
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S050-10     THRU    S050-EX

           IF      WK-PIN2-EOF   =     HIGH-VALUE
                OR PRM1-REC (1:1) =    SPACE

      *    *** パラメータ ACCEPT
                   PERFORM S040-10     THRU    S040-EX
           ELSE

                   MOVE    1           TO      I
                   PERFORM UNTIL   WK-PRM1-EOF   =     HIGH-VALUE
                       EVALUATE TRUE
      *    *** (1:1) = SPACE コメント
                           WHEN PRM1-REC (1:1) = SPACE
                               CONTINUE

                           WHEN PRM1-REC (1:4) = "MODE"
      *    *** MODE= 先に指定した方を優先にする
                               IF      WK-MODE     =       SPACE
      *    *** PRM1 ID= 情報セット
                                   PERFORM S060-10     THRU    S060-EX
                               END-IF

                           WHEN OTHER
      *    *** PRM1 TBL SET
                               PERFORM S070-10     THRU    S070-EX
                       END-EVALUATE
      *    *** READ PRM1
                           PERFORM S050-10     THRU    S050-EX
                   END-PERFORM
           END-IF

      *    *** OPEN PIN1,PIN2
           PERFORM S011-10     THRU    S011-EX

           IF      WK-MODE     =       "B"

      *    *** READ PIN2(インプット　バイナリーファイル)
                   PERFORM S030-10     THRU    S030-EX

                   PERFORM UNTIL   WK-PIN2-EOF   =     HIGH-VALUE
      *    *** PIN2 PRINT
                           PERFORM S200-10     THRU    S200-EX
      *    *** READ PIN2(インプット　バイナリーファイル)
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM
           ELSE

      *    *** READ PIN1(インプット　バイナリーファイル)
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE
      *    *** READ PIN1 PRINT
                           PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1(インプット　バイナリーファイル)
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM
           END-IF

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN PRM1
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN PIN1,PIN2
       S011-10.

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
           .
       S011-EX.
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

      *    *** READ PIN2
       S030-10.

           IF      WDE04-SHORI   =       "FIRST" OR "READ " OR "END  "
                   MOVE    HIGH-VALUE  TO      PIN2-REC
                   READ    PIN2-F

                   IF      WK-PIN2-STATUS =    ZERO OR 04
                           ADD     1           TO      WK-PIN2-CNT

      *             DISPLAY "PIN2-STATUS=" WK-PIN2-STATUS 
      *                     " WK-PIN2-CNT=" WK-PIN2-CNT

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     MOVE    WK-PIN2-LEN TO      WFD-LEN
      *     MOVE    WK-PIN2-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN2-REC
      *                                 WFD-LEN
                   END-IF
           ELSE
                   CONTINUE
           END-IF

           IF      WK-PIN2-STATUS =    ZERO OR 04
                   CONTINUE

      *     IF WK-PIN1-CNT < 10
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " LEN=" WK-PIN1-LEN
      *     END-IF

           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** パラメータ ACCEPT
       S040-10.

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE MODE L(SEQ) "
                           "OR B(BINARY) ?  L OR B で入力"
                   ACCEPT  WK-MODE
                   IF      WK-MODE     =       "B"
                           DISPLAY WK-PGM-NAME " BINARY FILE OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                       IF      WK-MODE     =       "L"
                           DISPLAY WK-PGM-NAME 
                                   " LINE SEQUENTIAL OK ? Y/N"
                           ACCEPT  SW-YES
                       ELSE
                           DISPLAY " FILE MODE ERROR MODE=" WK-MODE
                       END-IF
                   END-IF
           END-PERFORM

           IF      WK-MODE     =       "B"
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN2-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN2-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM
           ELSE
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN1-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM
           END-IF



           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT DUMP ID ?"
                           " P(RECORD) OR X(ITEM)"
                   ACCEPT  WFD-ID
                   IF      WFD-ID      =       "P" OR "X"
                       DISPLAY WK-PGM-NAME " ID=" WFD-ID
                               " OK ? Y/N"
                       ACCEPT  SW-YES
                   ELSE
                       DISPLAY WK-PGM-NAME " ID=" WFD-ID
                               " P OR X INPUT"
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT DUMP TYPE ?"
                           " A(ALPHA) OR M(MIX)"
                   ACCEPT  WFD-TYPE
                   IF      WFD-TYPE    =       "A" OR "M"
                       DISPLAY WK-PGM-NAME " TYPE=" WFD-TYPE
                               " OK ? Y/N"
                       ACCEPT  SW-YES
                   ELSE
                       DISPLAY WK-PGM-NAME " TYPE=" WFD-TYPE
                               " A OR M INPUT"
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT DUMP KANJI ?"
                           " SJIS OR UTF8"
                   ACCEPT  WFD-KANJI
                   IF      WFD-KANJI   =       "SJIS" OR "UTF8"
                       DISPLAY WK-PGM-NAME " KANJI=" WFD-KANJI
                               " OK ? Y/N"
                       ACCEPT  SW-YES
                   ELSE
                       DISPLAY WK-PGM-NAME " KANJI=" WFD-KANJI
                               " SJIS OR UTF8 INPUT"
                   END-IF
           END-PERFORM

           MOVE    1           TO      I
           IF      WFD-ID      =       "X"
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " ID=X ITEM,POS,LEN,Y OR N INPUT"
                           " ex. KOMOKU1,11,5,N  OR KOMOKU1,11,5,Y(END)"
                   ACCEPT  WK-JYOKEN
                   UNSTRING WK-JYOKEN
                       DELIMITED BY ","
                       INTO
                       WK-KOMOKU
                       WK-POS
                       WK-LEN
                       WK-YN

                   MOVE    WK-KOMOKU   TO      TBL01-ITEM (I)
                   MOVE    FUNCTION NUMVAL(WK-POS) TO TBL01-POS  (I)
                   MOVE    FUNCTION NUMVAL(WK-LEN) TO TBL01-LEN  (I)
                   MOVE    WK-YN       TO      SW-YES
                   IF      SW-YES      =       "Y"
                       MOVE    I           TO      I-MAX
                   ELSE
                       ADD     1           TO      I
                       IF      I           >       10
                           DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                           STOP    RUN
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** READ PRM1
       S050-10.

           READ    PRM1-F

           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
               IF  WK-PRM1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F READ ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S050-EX.
           EXIT.

      *    *** PRM1 ID= 情報セット
       S060-10.

           UNSTRING PRM1-REC
               DELIMITED BY "," OR "=" OR SPACE
               INTO
                   WK-ITEM01
                   WK-ITEM02
                   WK-ITEM03
                   WK-ITEM04
                   WK-ITEM05
                   WK-ITEM06
                   WK-ITEM07
                   WK-ITEM08
                   WK-ITEM09
                   WK-ITEM10

           IF      WK-ITEM01   =       "MODE"
                   IF      WK-ITEM02   =       "B" OR "L"
                           MOVE    WK-ITEM02   TO      WK-MODE
                   ELSE
                           DISPLAY WK-PGM-NAME " INPUT FILE MODE ERROR"
                                   " MODE=L OR MODE=B"
                                   " L(SEQ), B(BINARY)"
                   END-IF
           END-IF

           IF      WK-ITEM03   =       "FILE"
                   IF      WK-MODE     =       "B"
                           MOVE    WK-ITEM04   TO      WK-PIN2-F-NAME
                   ELSE
                           IF      WK-MODE     =       "L"
                                   MOVE    WK-ITEM04  TO  WK-PIN1-F-NAME
                           ELSE
                                   CONTINUE
                           END-IF
                   END-IF
           END-IF

           IF      WK-ITEM05   =       "ID"
                   IF      WK-ITEM06   =       "P" OR "X"
                           MOVE    WK-ITEM06   TO      WFD-ID
                   ELSE
                           DISPLAY WK-PGM-NAME " INPUT ID ERROR"
                                   " ID=P OR ID=X "
                                   " P(RECORD), X(ITEM)"
                   END-IF
           END-IF

           MOVE    SPACE       TO      WFD-TYPE
           IF      WK-ITEM07   =       "TYPE"
                   IF      WK-ITEM08   =       "A" OR "M"
                           MOVE    WK-ITEM08   TO      WFD-TYPE
                   ELSE
                           DISPLAY WK-PGM-NAME " INPUT TYPE ERROR"
                                   " TYPE=A OR TYPE=M INPUT"
                                   " A(ALPHA), M(MIX)"
                   END-IF
           END-IF

           MOVE    SPACE       TO      WFD-KANJI
           IF      WK-ITEM09   =       "KANJI"
                   IF      WK-ITEM10   =       "SJIS" OR "UTF8"
                           MOVE    WK-ITEM10   TO      WFD-KANJI
                   ELSE
                           DISPLAY WK-PGM-NAME " INPUT KANJI ERROR"
                                   " KANJI=SJIS OR KANJI=UTF8"
                   END-IF
           END-IF

           IF      WK-MODE     =       SPACE
                OR WK-ITEM04   =       SPACE
                OR WK-PIN1-F-NAME =    SPACE
                OR WK-PIN2-F-NAME =    SPACE
                OR WFD-ID      =       SPACE
                OR WFD-TYPE    =       SPACE
                OR WFD-KANJI   =       SPACE
                   DISPLAY WK-PGM-NAME " ID=PARA ERROR =" PRM1-REC
                   STOP    RUN
           END-IF

           .
       S060-EX.
           EXIT.

      *    *** PRM1 ID=X 情報セット
       S070-10.

           UNSTRING PRM1-REC
               DELIMITED BY "," OR SPACE
               INTO
                   WK-KOMOKU
                   WK-POS
                   WK-LEN

           IF      WK-KOMOKU   NOT =   SPACE
               AND FUNCTION NUMVAL(WK-POS) IS NUMERIC
               AND FUNCTION NUMVAL(WK-POS) >= 1 
               AND FUNCTION NUMVAL(WK-POS) <= 65536
               AND FUNCTION NUMVAL(WK-LEN) IS NUMERIC
               AND FUNCTION NUMVAL(WK-LEN) >= 1
               AND FUNCTION NUMVAL(WK-LEN) <= 65536
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " ID=X 項目 パラメータERROR =" 
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " 項目名（スペース以外、１０バイト以内）"
                   DISPLAY WK-PGM-NAME 
                           " 位置（数字、１ー６５５３６以内）"
                   DISPLAY WK-PGM-NAME 
                           " 長さ（数字、１ー６５５３６以内）"
                   STOP    RUN
           END-IF

           IF    ( FUNCTION NUMVAL(WK-POS) +
                   FUNCTION NUMVAL(WK-LEN) ) >= 2
             AND ( FUNCTION NUMVAL(WK-POS) +
                   FUNCTION NUMVAL(WK-LEN) - 1 ) <= 65536
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " ID=X 項目 パラメータERROR =" 
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " 位置＋長さ（２ー６５５３６以内）"
                   STOP    RUN
           END-IF

           MOVE    WK-KOMOKU   TO      TBL01-ITEM (I)
           MOVE    FUNCTION NUMVAL(WK-POS) TO TBL01-POS  (I)
           MOVE    FUNCTION NUMVAL(WK-LEN) TO TBL01-LEN  (I)

           MOVE    I           TO      I-MAX

           ADD     1           TO      I
           IF      I           >       20
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           END-IF

           .
       S070-EX.
           EXIT.

      *    *** PRINT PIN1
       S100-10.

           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           IF      WFD-ID     =        "P"
                   MOVE    1           TO      WFD-SU
                   MOVE    WK-PIN1-LEN TO      WFD-LEN
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               PIN1-REC
                                               WFD-LEN
           ELSE
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

                   MOVE    2           TO      WFD-SU
                   MOVE    TBL01-ITEM (I) TO   WFD-ITEM
                   MOVE    TBL01-POS  (I) TO   P
                   MOVE    TBL01-LEN  (I) TO   L

                   IF    ( P + L )     >=      2
                     AND ( P + L - 1 ) <=      WK-PIN1-LEN
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " ID=X 項目 パラメータERROR " 
                                   " P=" P " L=" L " WK-PIN1-LEN="
                                   WK-PIN1-LEN
                                   " I=" I " ITEM=" TBL01-ITEM (I)
                                   " I-MAX=" I-MAX
                           DISPLAY WK-PGM-NAME 
                                   " 位置＋長さ（レコード長以内）"
                           STOP    RUN
                   END-IF

                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               PIN1-REC (P:L)
               END-PERFORM
           END-IF

           .
       S100-EX.
           EXIT.

      *    *** PRINT PIN2
       S200-10.

      *    *** 可変長は,WK-PIN2-LEN ZEROがセットされている
      *    *** VALUE句で65536セットしておく

           IF      WFD-ID     =        "P"
                   MOVE    1           TO      WFD-SU
                   MOVE    WK-PIN2-LEN TO      WDE04-BUF1-LEN
                   CALL    "DECODE04"  USING   WDE04-AREA
                                               PIN2-REC
                                               WK-BUF2

                   IF      WDE04-SHORI    NOT =   "END  "
                           ADD     1           TO      WK-PIN2-0D0A-CNT
                           MOVE    WK-PIN2-0D0A-CNT TO WFD-SEQ
                           MOVE    WDE04-REC-LEN TO    WFD-LEN
                           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                                       WK-BUF2
                                                       WFD-LEN
                   END-IF
           ELSE
                   MOVE    2           TO      WFD-SU
                   MOVE    WK-PIN2-LEN TO      WDE04-BUF1-LEN
                   CALL    "DECODE04"  USING   WDE04-AREA
                                               PIN2-REC
                                               WK-BUF2

                   IF      WDE04-SHORI    NOT =   "END  "
                           ADD     1           TO      WK-PIN2-0D0A-CNT
                           MOVE    WK-PIN2-0D0A-CNT TO WFD-SEQ
                           PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > I-MAX
                               MOVE    TBL01-ITEM (I) TO   WFD-ITEM
                               MOVE    TBL01-POS  (I) TO   P
                               MOVE    TBL01-LEN  (I) TO   L

                               IF    ( P + L )     >=      2
                                 AND ( P + L - 1 ) <=      WDE04-REC-LEN
                                       CONTINUE
                               ELSE
                                       DISPLAY WK-PGM-NAME 
                                           " ID=X 項目 パラメータERROR " 
                                         " P=" P " L=" L " WK-PIN2-LEN="
                                               WDE04-REC-LEN
                                         " I=" I " ITEM=" TBL01-ITEM (I)
                                         " I-MAX=" I-MAX
                                       DISPLAY WK-PGM-NAME 
                                         " 位置＋長さ（レコード長以内）"
                                       STOP    RUN
                               END-IF

                               CALL   "FILEDUMP" USING WFD-FILEDUMP-AREA
                                                           WK-BUF2 (P:L)
                           END-PERFORM
                   END-IF
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN2-0D0A-CNT TO WK-PIN2-0D0A-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 (0D0A)=" WK-PIN2-0D0A-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
