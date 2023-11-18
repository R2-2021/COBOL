      *    *** INSPECT CHECK, FILEITEM CHECK DATA CREATE

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM06.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** この指定があると、INSPECT CONVERTING がコンパイルエラー
      *    *** になる 項目名LOWER-CASEだと、エラーになる
      *    *** 
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
            ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-I1         PIC  X(200).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-PACK       PIC S9(011) PACKED-DECIMAL.
           03                  PIC  X(005).
           03  POT1-KEY1       PIC  X(001).
           03  POT1-KEY2       PIC  X(001).
           03  POT1-KEY3       PIC  X(001).
           03  POT1-0D0A       PIC  X(002).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-UNPACK     PIC S9(011).
           03  POT2-KEY1       PIC  X(001).
           03  POT2-KEY2       PIC  X(001).
           03  POT2-KEY3       PIC  X(001).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM06".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM06.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM06.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "COBSAM06.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG VALUE ZERO SYNC.
           03  WK-POT1-CNT     BINARY-LONG VALUE ZERO SYNC.
           03  WK-POT2-CNT     BINARY-LONG VALUE ZERO SYNC.

           03  WK-ID1          PIC  X(010) VALUE "ABCDEABCDE".
           03  WK-ID1X         PIC  X(011) VALUE "ABCDEABCDE ".
           03  WK-ID2          PIC  9(005) VALUE ZERO.
           03  WK-ID3          PIC  X(002) VALUE "CD".
           03  WK-ID3X         PIC  X(002) VALUE "AB".
           03  WK-ID8          PIC  X(002) VALUE "DE".
           03  WK-ITEM0        PIC  X(014) VALUE SPACE.
           03  WK-COUNT-0      PIC  9(005) VALUE ZERO.
           03  WK-COUNT-1      PIC  9(005) VALUE ZERO.
           03  WK-COUNT-2      PIC  9(005) VALUE ZERO.
           03  WK-COUNT-3      PIC  9(005) VALUE ZERO.
           03  WK-COUNT-4      PIC  9(005) VALUE ZERO.
           03  WK-COUNTY       PIC  X(005) VALUE "abcde".
           03  HOLD-COUNTY     PIC  X(011) VALUE "fghij".

           03  WK-ZONE         PIC  S9(10)V99 VALUE -12345.67. 
           03  WK-PACK         PIC  S9(10)V99 VALUE -12345.67 
                                           PACKED-DECIMAL.
           03  WK-NUMVAL       PIC  S9(10)V99 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  WK-LOWER-CASE       PIC  X(026) VALUE
           "abcdefghijklmnopqrstuvwxyz".

       01  WK-UPPER-CASE       PIC  X(026) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-I       BINARY-LONG VALUE ZERO SYNC.
             05  TBL01-KEY     PIC  X(010) VALUE SPACE.
             05  TBL01-DATA    PIC  X(070) VALUE SPACE.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG VALUE 1.

       01  INDEX-AREA.
           03  I               BINARY-LONG VALUE ZERO SYNC.
           03  I-MAX           BINARY-LONG VALUE ZERO SYNC.
           03  I1              BINARY-LONG VALUE ZERO SYNC.
           03  I2              BINARY-LONG VALUE ZERO SYNC.
           03  J               BINARY-LONG VALUE ZERO SYNC.
           03  O               BINARY-LONG VALUE ZERO SYNC.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-I            BINARY-LONG VALUE ZERO SYNC.

       01  TBL04-IDX-MAX       BINARY-LONG SYNC VALUE ZERO.
       01  TBL-AREA.

      *    *** 名前ＳＯＲＴ用用
           03  TBL04-AREA      OCCURS 1 TO 3000
                               DEPENDING ON TBL04-IDX-MAX
                               ASCENDING KEY IS TBL04-FURIGANA
                               INDEXED BY TBL04-IDX.
             05  TBL04-FURIGANA PIC  X(030) VALUE HIGH-VALUE.
             05  TBL04-REC     PIC  X(500) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** INSPECT TALLYING CHECK
           PERFORM S100-10     THRU    S100-EX
      *             UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

           MOVE    "AAAA"      TO      TBL04-FURIGANA (2)
           MOVE    "AREC"      TO      TBL04-REC(2)
           MOVE    "BBBB"      TO      TBL04-FURIGANA (1)
           MOVE    "BREC"      TO      TBL04-REC(1)
           DISPLAY "SORT-S"
           DISPLAY "TBL04-IDX    =" TBL04-IDX
           DISPLAY "TBL04-IDX-MAX=" TBL04-IDX-MAX
      *    *** TBL04 SORT
           SORT    TBL04-AREA
                   ASCENDING KEY TBL04-FURIGANA
           DISPLAY "SORT-E"
           DISPLAY TBL04-FURIGANA (1)
           DISPLAY TBL04-REC(1) (1:20)
           DISPLAY TBL04-FURIGANA (2)
           DISPLAY TBL04-REC(2) (1:20)

      *    *** NUMVAL CHECK
           PERFORM S110-10     THRU    S110-EX

      *    ***  INSPECT HOLD-COUNTY CONVERTING
      *    ***          WK-LOWER-CASE TO WK-UPPER-CASE チェック
      *     PERFORM S120-10     THRU    S120-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.

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
                                       POT1-REC

           MOVE    SPACE       TO      POT1-REC
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

           MOVE    "P"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    "PIN1-REC"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC

           MOVE    "P"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           MOVE    "PIN1-REC"  TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
                                       WFD-LEN
           .
       S020-EX.
           EXIT.

      *    *** INSPECT CHECK
       S100-10.


      *    *** INSPECT の文でTALLYING,REPLACING複数行記述すると、
      *    *** 後に記述された、TALLYING,REPLACINGは
      *    *** カウントされた文字、変換された文字は検査対象から外れる

           MOVE "EFABDBCGABCFGG" TO WK-ITEM0
           INSPECT WK-ITEM0 TALLYING
      *       WK-COUNT-0 FOR ALL "AB", ALL "D"
             WK-COUNT-1 FOR ALL "BC"
             WK-COUNT-2 FOR LEADING "EF"
             WK-COUNT-3 FOR LEADING "B"
      *    *** 後にCHARACTERS指定すると、前のカウントされた項目を除く
      *    *** 文字をカウントする
             WK-COUNT-4 FOR CHARACTERS
           DISPLAY WK-ITEM0
           DISPLAY WK-COUNT-0
           DISPLAY WK-COUNT-1
           DISPLAY WK-COUNT-2
           DISPLAY WK-COUNT-3
           DISPLAY WK-COUNT-4

           MOVE "EFABDBCGABCFGG" TO WK-ITEM0
           INSPECT WK-ITEM0 REPLACING
             ALL "AB" BY "XY", "D" BY "X"
             ALL "BC" BY "VW"
             LEADING "EF" BY "TU"
             LEADING "B" BY "S"
             FIRST "G" BY "R"
             FIRST "G" BY "P"
             CHARACTERS BY "Z"
           DISPLAY WK-ITEM0

      *    *** TALLYING
      *    *** 03  WK-ID1          PIC  X(010) VALUE "ABCDEABCDE".

      *    *** 11桁目スペースがあっても数える
           MOVE    ZERO        TO      WK-ID2
      *     INSPECT WK-ID1X TALLYING WK-ID2 FOR CHARACTERS
           INSPECT "BBBC" TALLYING WK-ID2 FOR CHARACTERS
           DISPLAY " "
           DISPLAY "NO.001 "
           DISPLAY "CHARACTERS"
           DISPLAY "WK-ID1X=" WK-ID1X
           DISPLAY "WK-ID2=" WK-ID2

      *    *** 11桁目スペースがあっても数える
           MOVE    ZERO        TO      WK-ID2
      *     INSPECT WK-ID1X TALLYING WK-ID2 FOR CHARACTERS
           INSPECT "BBBC-BGVC" TALLYING WK-ID2 FOR LEADING "-" BEFORE 
           "-"
           DISPLAY " "
           DISPLAY "NO.002 "
           DISPLAY "CHARACTERS"
           DISPLAY "WK-ID1X=" WK-ID1X
           DISPLAY "WK-ID2=" WK-ID2



      *    *** ID3の全ての個数
           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR ALL WK-ID3
           DISPLAY " "
           DISPLAY "NO.002 "
           DISPLAY "TALLYING"
           DISPLAY "ALL"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=" WK-ID3


      *    *** TALLYING 指定の個数計測うまく動作しない時があった、
      *    *** TEST91 にてALL ならうまくいった
      *    *** LEADING の意味は最左端の文字が一致したらカウントする
      *    *** 意味だった　NO.003 はカウントするが、
      *    *** NO.004 はカウントしない

      *    *** 最初の文字"AB"の時のみ、ID2=1になる
           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR LEADING "AB"
           DISPLAY " "
           DISPLAY "NO.003 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=AB" 

           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR LEADING "BC"
           DISPLAY " "
           DISPLAY "NO.004 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=BC" 

           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR LEADING "ABC"
           DISPLAY " "
           DISPLAY "NO.005 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=ABC" 



           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR TRAILING "AB"
           DISPLAY " "
           DISPLAY "NO.007 "
           DISPLAY "TALLYING"
           DISPLAY "TRAILING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=AB"

           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR TRAILING "DE"
           DISPLAY " "
           DISPLAY "NO.008 "
           DISPLAY "TALLYING"
           DISPLAY "TRAILING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=DE"

           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR TRAILING "CDE"
           DISPLAY " "
           DISPLAY "NO.009 "
           DISPLAY "TALLYING"
           DISPLAY "TRAILING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=CDE"

      *    *** 最後の文字"XX"一致の時、ID2=カウントアップになる
           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDE" TALLYING WK-ID2 FOR TRAILING "E"
           DISPLAY " "
           DISPLAY "NO.010 "
           DISPLAY "TALLYING"
           DISPLAY "TRAILING"
           DISPLAY "WK-ID1=ABCDEABCDE"
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=E"



      *    *** 最初に現れた、"E" の前に"CD" いくつあるか
           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDE" TALLYING WK-ID2 FOR ALL "CD" BEFORE "E"
           DISPLAY " "
           DISPLAY "NO.011 "
           DISPLAY "TALLYING"
           DISPLAY "ALL - BEFORE ""E"""
           DISPLAY "WK-ID1=ABCDEABCDF" 
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=CD" 
           DISPLAY "WK-ID8=E"



      *    *** "A" の後に"BC" いくつあるか
           MOVE    ZERO        TO      WK-ID2
           INSPECT WK-ID1 TALLYING WK-ID2 FOR ALL "BC" AFTER "A"
           DISPLAY " "
           DISPLAY "NO.012 "
           DISPLAY "TALLYING"
           DISPLAY "ALL - AFTER ""A"""
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=BC"
           DISPLAY "WK-ID8=A"



      *    *** "F" の前に"CD" いくつあるか
      *    *** BEFORE "F" 指定しても結果同じ
           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDF" TALLYING WK-ID2 FOR LEADING "AB"
                   BEFORE "F"
           DISPLAY " "
           DISPLAY "NO.013 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING - BEFORE"
           DISPLAY "WK-ID1=ABCDEABCDF" 
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=AB" 
           DISPLAY "WK-ID8=F"

           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDF" TALLYING WK-ID2 FOR LEADING "BC"
                   BEFORE "F"
           DISPLAY " "
           DISPLAY "NO.014 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING - BEFORE"
           DISPLAY "WK-ID1=ABCDEABCDF" 
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=BC" 
           DISPLAY "WK-ID8=F"

           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDF" TALLYING WK-ID2 FOR LEADING "ABC"
                   BEFORE "F"
           DISPLAY " "
           DISPLAY "NO.015 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING - BEFORE"
           DISPLAY "WK-ID1=ABCDEABCDF" 
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=ABC" 
           DISPLAY "WK-ID8=F"

           MOVE    ZERO        TO      WK-ID2
           INSPECT "ABCDEABCDF" TALLYING WK-ID2 FOR LEADING "DE"
                   BEFORE "F"
           DISPLAY " "
           DISPLAY "NO.016 "
           DISPLAY "TALLYING"
           DISPLAY "LEADING - BEFORE"
           DISPLAY "WK-ID1=ABCDEABCDF" 
           DISPLAY "WK-ID2=" WK-ID2
           DISPLAY "WK-ID3=DE" 
           DISPLAY "WK-ID8=F"



      *    *** REPLACING

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING CHARACTERS BY "&"
           DISPLAY " "
           DISPLAY "NO.021 "
           DISPLAY "REPLACING  CHARACTERS"
           DISPLAY "WK-ID1=" WK-ID1



           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING ALL "BCE" BY "#CD"
           DISPLAY " "
           DISPLAY "NO.022 "
           DISPLAY "REPLACING  ALL"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID4=#CD" 



           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "ABC" BY "#CD"
           DISPLAY " "
           DISPLAY "NO.023 "
           DISPLAY "REPLACING LEADING"
           DISPLAY "WK-ID1=" WK-ID1
           DISPLAY "WK-ID4=#CD" 

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "BCD" BY "#CD"
           DISPLAY " "
           DISPLAY "NO.024 "
           DISPLAY "REPLACING LEADING "
           DISPLAY "WK-ID1=" WK-ID1

      *    *** AFTER を付けるとその文字以降が対象となるので、
      *    *** 以下のケースは変更されない　２バイト目以降が対象
      *    *** LEADINGは文字の最初のバイトが対象となるので、AFTER指定
      *    *** するとどのパターンでも変更されない
           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "ABC" BY "#CD"
                   AFTER "A"
           DISPLAY " "
           DISPLAY "NO.025 "
           DISPLAY "REPLACING LEADING AFTER A"
           DISPLAY "WK-ID1=" WK-ID1

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "ABC" BY "#CD"
                   AFTER "B"
           DISPLAY " "
           DISPLAY "NO.026 "
           DISPLAY "REPLACING LEADING AFTER B"
           DISPLAY "WK-ID1=" WK-ID1

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "ABC" BY "#CD"
                   BEFORE "C"
           DISPLAY " "
           DISPLAY "NO.027 "
           DISPLAY "REPLACING LEADING BEFORE C"
           DISPLAY "WK-ID1=" WK-ID1

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING LEADING "ABC" BY "#CD"
                   BEFORE "D"
           DISPLAY " "
           DISPLAY "NO.028 "
           DISPLAY "REPLACING LEADING BEFORE D"
           DISPLAY "WK-ID1=" WK-ID1



           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING FIRST "BCD" BY "#CD"
           DISPLAY " "
           DISPLAY "NO.029 "
           DISPLAY "REPLACING FIRST"
           DISPLAY "WK-ID1=" WK-ID1

           MOVE    "ABCDEABCDE" TO WK-ID1
           INSPECT WK-ID1 REPLACING FIRST "EAB" BY "#CD"
           DISPLAY " "
           DISPLAY "NO.030 "
           DISPLAY "REPLACING  FIRST"
           DISPLAY "WK-ID1=" WK-ID1

      *>郡名、小文字＝＞大文字に変換
           DISPLAY " "
           DISPLAY "NO.040"
           DISPLAY "WK-COUNTY前=" HOLD-COUNTY
      *     INSPECT HOLD-COUNTY CONVERTING LOWER-CASE TO UPPER-CASE
           INSPECT HOLD-COUNTY CONVERTING WK-LOWER-CASE TO WK-UPPER-CASE
           DISPLAY "WK-COUNTY後=" HOLD-COUNTY


      *     PERFORM S100-10     THRU    S100-EX
           .
       S100-EX.
           EXIT.

      *    *** NUMVAL CHECK
       S110-10.

           MOVE    NUMVAL("--99.55 ") TO     WK-NUMVAL
           DISPLAY "--99.55 " WK-NUMVAL

           MOVE    NUMVAL("A9..5 ") TO       WK-NUMVAL
           DISPLAY "A9..5 " WK-NUMVAL

           MOVE    NUMVAL("A9..5- ") TO      WK-NUMVAL
           DISPLAY "A9..5- " WK-NUMVAL

           MOVE    NUMVAL("9-9.55 ") TO      WK-NUMVAL
           DISPLAY "9-9.55 " WK-NUMVAL

           MOVE    NUMVAL("99.-55 ") TO      WK-NUMVAL
           DISPLAY "99.-55 " WK-NUMVAL

           MOVE    NUMVAL("9.9.55 ") TO      WK-NUMVAL
           DISPLAY "9.9.55 " WK-NUMVAL

           MOVE    NUMVAL("9.9.55- ") TO      WK-NUMVAL
           DISPLAY "9.9.55- " WK-NUMVAL

           MOVE    NUMVAL(".9-9.55 ") TO      WK-NUMVAL
           DISPLAY ".9-9.55 " WK-NUMVAL

           MOVE    NUMVAL("9-9.55. ") TO      WK-NUMVAL
           DISPLAY "9-9.55. " WK-NUMVAL

           MOVE    NUMVAL("9-9 .55 ") TO      WK-NUMVAL
           DISPLAY "9-9 .55 " WK-NUMVAL

           MOVE    NUMVAL("99 .55 ") TO      WK-NUMVAL
           DISPLAY "99 .55 " WK-NUMVAL

           MOVE    NUMVAL("99 .55. ") TO      WK-NUMVAL
           DISPLAY "99 .55. " WK-NUMVAL


           MOVE    NUMVAL(WK-PACK)    TO      WK-NUMVAL
           DISPLAY "PACK=  " WK-NUMVAL

           MOVE    NUMVAL(WK-ZONE)    TO      WK-NUMVAL
           DISPLAY "ZONE=  " WK-NUMVAL


      *    *** FILEITEM TEST DATA CREATE
           MOVE    SPACE       TO      POT1-REC
                                       POT2-REC
           MOVE    X"0D0A"     TO      POT1-0D0A

      *    *** +


           MOVE    ZERO        TO      POT1-KEY1
                                       POT1-KEY2
                                       POT1-KEY3
                                       POT2-KEY1
                                       POT2-KEY2
                                       POT2-KEY3
           MOVE    ZERO        TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    1           TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    1           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    12          TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    123         TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    2           TO      POT1-KEY2
                                       POT2-KEY2

           MOVE    1           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    1234        TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    12345       TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    1           TO      POT1-KEY1
                                       POT2-KEY1

           MOVE    2           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    123456      TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    1234567     TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    2           TO      POT1-KEY2
                                       POT2-KEY2

           MOVE    3           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    12345678    TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    123456789   TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    3           TO      POT1-KEY1
                                       POT2-KEY1

           MOVE    4           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    1234567890  TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    12345678901 TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** -
           MOVE    4           TO      POT1-KEY1
                                       POT2-KEY1

           MOVE    2           TO      POT1-KEY2
                                       POT2-KEY2

           MOVE    3           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    ZERO        TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -1          TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    2           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    -12         TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -123        TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    3           TO      POT1-KEY2
                                       POT2-KEY2

           MOVE    4           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    -1234       TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -12345      TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    5           TO      POT1-KEY1
                                       POT2-KEY1

           MOVE    -123456     TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -1234567    TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    6           TO      POT1-KEY3
                                       POT2-KEY3

           MOVE    -12345678   TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -123456789  TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    8           TO      POT1-KEY1
                                       POT2-KEY1
           MOVE    7           TO      POT1-KEY2
                                       POT2-KEY2

           MOVE    -1234567890 TO      POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    -12345678901 TO     POT1-PACK
                                       POT2-UNPACK
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

            .
       S110-EX.
           EXIT.

       S120-10.
      *>郡名、小文字＝＞大文字に変換
           DISPLAY " "
           DISPLAY "NO.040"
           MOVE    "abcde" TO HOLD-COUNTY
           DISPLAY "WK-COUNTY前=" HOLD-COUNTY
      *     INSPECT HOLD-COUNTY CONVERTING LOWER-CASE TO UPPER-CASE
           INSPECT HOLD-COUNTY CONVERTING WK-LOWER-CASE TO WK-UPPER-CASE
      
      *    *** この書き方は変換してる
      *     INSPECT HOLD-COUNTY REPLACING ALL "a" BY "A"
      *     INSPECT HOLD-COUNTY REPLACING ALL "b" BY "B"
      *     INSPECT HOLD-COUNTY REPLACING ALL "c" BY "C"
      *     INSPECT HOLD-COUNTY REPLACING ALL "d" BY "D"
      *     INSPECT HOLD-COUNTY REPLACING ALL "e" BY "E"
           DISPLAY "WK-COUNTY後=" HOLD-COUNTY
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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
      *    *** 
           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT
      *
           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
