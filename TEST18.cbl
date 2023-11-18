      *    *** 処理速度の計測
      *    *** PRINT AREA 2次元でセット　（４０行／１ページ）

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST18.

       ENVIRONMENT             DIVISION.
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
             05  PIN1-MOJI     OCCURS 188
                               PIC  X(002).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(136).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST18  ".

           03  WK-PIN1-F-NAME  PIC  X(011) VALUE "TEST18.PIN1".
           03  WK-POT1-F-NAME  PIC  X(011) VALUE "TEST18.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

      *     03  WK-SHORI-CNT    BINARY-LONG SYNC VALUE 10000000.
           03  WK-SHORI-CNT    BINARY-LONG SYNC VALUE 1000000.

      *    *** SYNC 有、無の結果は、あまり変わらない、無しの方が早い時がある

           03  WK-COMPN        PIC  S9(3)V99 COMP   VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP2N       PIC  S9(9)V99 COMP   VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-1N                    COMP-1 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-2N                    COMP-2 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-4N      PIC  S9(3)V99 COMP-4 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-42N     PIC  S9(9)V99 COMP-4 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-5N      PIC  S9(3)V99 COMP-5 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-52N     PIC  S9(9)V99 COMP-5 VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-XN      PIC  S9(3)V99 COMP-X VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-COMP-X2N     PIC  S9(9)V99 COMP-X VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.

           03  WK-BINARYN      BINARY-LONG   VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-BI-SHORTN    BINARY-SHORT  VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.
           03  WK-BI-DOUBLEN   BINARY-DOUBLE VALUE ZERO.
           03                  PIC  X(001)          VALUE ZERO.

           03  WK-COMP         PIC  S9(3)V99 COMP   SYNC VALUE ZERO.
           03  WK-COMP2        PIC  S9(9)V99 COMP   SYNC VALUE ZERO.
           03  WK-COMP-1                     COMP-1 SYNC VALUE ZERO.
           03  WK-COMP-2                     COMP-2 SYNC VALUE ZERO.
           03  WK-COMP-4       PIC  S9(3)V99 COMP-4 SYNC VALUE ZERO.
           03  WK-COMP-42      PIC  S9(9)V99 COMP-4 SYNC VALUE ZERO.
           03  WK-COMP-5       PIC  S9(3)V99 COMP-5 SYNC VALUE ZERO.
           03  WK-COMP-52      PIC  S9(9)V99 COMP-5 SYNC VALUE ZERO.
           03  WK-COMP-X       PIC  S9(3)V99 COMP-X SYNC VALUE ZERO.
           03  WK-COMP-X2      PIC  S9(9)V99 COMP-X SYNC VALUE ZERO.

           03  WK-BINARY-CHAR  BINARY-LONG   SYNC VALUE ZERO.
           03  WK-BINARY       BINARY-LONG   SYNC VALUE ZERO.
           03  WK-BI-SHORT     BINARY-SHORT  SYNC VALUE ZERO.
           03  WK-BI-DOUBLE    BINARY-DOUBLE SYNC VALUE ZERO.

           03  WK-PACK         PIC S9(009)  VALUE ZERO PACKED-DECIMAL.
           03  WK-PACK-V       PIC S9(7)V99 VALUE ZERO PACKED-DECIMAL.

           03  WK-PACK2        PIC S9(009) VALUE 12345 PACKED-DECIMAL.

           03  WK-PACK3        PIC S9(036)  VALUE ZERO PACKED-DECIMAL.
           03  WK-PACK3-V      PIC S9(34)V99 VALUE ZERO PACKED-DECIMAL.

           03  WK-PACK4        PIC S9(009)  VALUE 123  PACKED-DECIMAL.
           03  WK-PACK4-V      PIC S9(7)V99 VALUE 1.23 PACKED-DECIMAL.

           03  WK-UNPACK       PIC S9(009)  VALUE ZERO.
           03  WK-UNPACK-V     PIC S9(7)V99 VALUE ZERO.

           03  WK-UNPACK2      PIC S9(009) VALUE 12345.

           03  WK-BINARY2      BINARY-LONG   SYNC VALUE 12345.
           03  WK-BI-SHORT2    BINARY-SHORT  SYNC VALUE 12345.
           03  WK-BI-DOUBLE2   BINARY-DOUBLE SYNC VALUE 12345.

           03  WK-VALUE-1      PIC  X(001) VALUE "A".
           03  WK-VALUE-10     PIC  X(010) VALUE ALL "ABC".
           03  WK-VALUE-100    PIC  X(100) VALUE ALL "ABC".
           03  WK-VALUE-200    PIC  X(200) VALUE ALL "ABC".

           03  WK-SPACE-1      PIC  X(001) VALUE SPACE.
           03  WK-SPACE-10     PIC  X(010) VALUE SPACE.
           03  WK-SPACE-100    PIC  X(100) VALUE SPACE.
           03  WK-SPACE-200    PIC  X(200) VALUE SPACE.
           03  WK-TESTNO       PIC  X(002) VALUE SPACE.

           03  WK-MSG-G.
             05  WK-NO         PIC  Z9     VALUE ZERO.
             05  WK-MSG        PIC  X(018) VALUE SPACE.

           03  WK-TIT1.
             05  FILLER        PIC  X(050) VALUE SPACE.
             05  FILLER        PIC  X(040) VALUE
                 "＊＊＊　処理速度　百万回で計測＊＊＊　".
             05  FILLER        PIC  X(010) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.
      *    *** 31*4=133 
           03  WK-MID1         PIC  X(124) VALUE
               ALL "　　　　　　　　項目ｌｕｐ ".
           03  WK-HAI          PIC  X(124) VALUE
               ALL "------------------------------ ".
           03  WK-MEI1.
             05  WK-MEI1-I1    PIC  X(020) VALUE SPACE.
             05  WK-MEI1-SSMM  PIC  ZZZ,ZZ9.99 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 40
                               PIC  X(136) VALUE SPACE.

       01  TBL-AREA.
           03  TBL01-AREA.
             05                PIC  X(018) VALUE ".ADD COMP S9(9) SY".
             05                PIC  X(018) VALUE ".ADD COMP S9(5) SY".
             05                PIC  X(018) VALUE ".ADD COMP-1     SY".
             05                PIC  X(018) VALUE ".ADD COMP-2     SY".
             05                PIC  X(018) VALUE ".ADD UNPACK       ".
             05                PIC  X(018) VALUE ".MUL BINARY-DOUBLE".
             05                PIC  X(018) VALUE ".MUL BINARY-SHORT ".
             05                PIC  X(018) VALUE ".MUL BINARY       ".
             05                PIC  X(018) VALUE ".MUL PACK         ".
             05                PIC  X(018) VALUE ".MUL UNPACK       ".
             05                PIC  X(018) VALUE ".MUL PACK-V       ".
             05                PIC  X(018) VALUE ".MUL UNPACK-V     ".
             05                PIC  X(018) VALUE ".SUB BINARY-DOUBLE".
             05                PIC  X(018) VALUE ".SUB BINARY-SHORT ".
             05                PIC  X(018) VALUE ".SUB BINARY       ".
             05                PIC  X(018) VALUE ".SUB PACK".
             05                PIC  X(018) VALUE ".SUB UNPACK".
             05                PIC  X(018) VALUE ".SUB PACK-V".
             05                PIC  X(018) VALUE ".SUB UNPACK-V".
             05                PIC  X(018) VALUE ".MOVE 10 BYTE".
           03  TBL01-AREA-R    REDEFINES TBL01-AREA
                               OCCURS 20
             05  TBL01-MSG     PIC  X(018).

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC  VALUE ZERO.
           03  I2              BINARY-LONG SYNC  VALUE ZERO.
           03  J               BINARY-LONG SYNC  VALUE 1.
           03  K               BINARY-LONG SYNC  VALUE ZERO.
           03  L               BINARY-LONG SYNC  VALUE ZERO.
           03  P1              BINARY-LONG SYNC  VALUE 1.
           03  P2              BINARY-LONG SYNC  VALUE 3.
           03  P3              BINARY-LONG SYNC  VALUE 4.
           03  P4              BINARY-LONG SYNC  VALUE 5.
           03  P5              BINARY-LONG SYNC  VALUE 6.
           03  R               BINARY-LONG SYNC  VALUE ZERO.
           03  M               BINARY-LONG SYNC  VALUE ZERO.

       01  SW-AREA.
           03  SW-LAST         PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

           DISPLAY "TEST NO INPUT"
           DISPLAY "01.ADD"
           DISPLAY "02.SUBTRACT"
           DISPLAY "03.MULTIPLY"
           DISPLAY "04.DIVIDE"
           DISPLAY "05.COMPUTE"
           DISPLAY "06.MOVE"
           DISPLAY "07.IF"
           DISPLAY "08.EVALUATE"

           ACCEPT  WK-TESTNO

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *    *** 処理計測
           EVALUATE WK-TESTNO
               WHEN "01"
                   PERFORM S210-10     THRU    S210-EX
      *             UNTIL   WK-PIN1-EOF   =     HIGH-VALUE
               WHEN "02"
                   PERFORM S220-10     THRU    S220-EX
               WHEN "03"
                   PERFORM S230-10     THRU    S230-EX
               WHEN "04"
                   PERFORM S240-10     THRU    S240-EX
               WHEN "05"
                   PERFORM S250-10     THRU    S250-EX
               WHEN "06"
                   PERFORM S260-10     THRU    S260-EX
               WHEN "07"
                   PERFORM S270-10     THRU    S270-EX
               WHEN "08"
                   PERFORM S280-10     THRU    S280-EX
           END-EVALUATE

      *    *** PRINT
           MOVE    "Y"         TO      SW-LAST
           PERFORM S200-10     THRU    S200-EX

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

           MOVE    WDT-DATE-YY  TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM  TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD  TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH  TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI  TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS  TO      WK-TIT1-SS

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

      *    *** WRITE POT1, PRINT TBL SET
       S200-10.

           IF      SW-LAST     =       "Y"
                   MOVE    40          TO      J
                   MOVE    124         TO      P1
           END-IF

           ADD     1           TO      J
           IF      J           >       40
                   MOVE    1           TO       J
                   ADD     31          TO       P1
                   IF      P1          >        124
                           ADD     1           TO      WK-PAGE
                           MOVE    WK-PAGE     TO      WK-TIT1-PAGE
                           WRITE   POT1-REC    FROM    WK-TIT1
                           ADD     1           TO      WK-POT1-CNT
                           WRITE   POT1-REC    FROM    WK-MID1
                           ADD     1           TO      WK-POT1-CNT
                           WRITE   POT1-REC    FROM    WK-HAI
                           ADD     1           TO      WK-POT1-CNT
                           PERFORM VARYING K  FROM 1 BY 1
                                   UNTIL   K   >       40
                                 WRITE   POT1-REC    FROM    PR-LINE(K)
                                 ADD     1           TO      WK-POT1-CNT
                           END-PERFORM
                           MOVE    SPACE       TO      PRINT-AREA
                           MOVE    1           TO      P1
                   ELSE
                           CONTINUE
                   END-IF
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** 処理計測
      *    *** ADD
       S210-10.

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP2
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP S9(9)V9SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP S9(3)V9SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-1
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-1      SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-2 
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-2      SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-42
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-4 S9(9)SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-4
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-4 S9(3)SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-52
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-5 S9(9)SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-5
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-5 S9(3)SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-X2
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-X S9V99SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-X
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-X S9(3)SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BI-DOUBLE
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BI-DO       SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BI-SHORT
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BI-SH       SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BINARY-CHAR
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BINARY-CHAR SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BINARY
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BINARY      SY" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     123         TO      WK-PACK
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK S9(9)    " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1.23        TO      WK-PACK-V
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK S9(7)V99" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     WK-PACK4     TO      WK-PACK
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK4 S9(9)   " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     WK-PACK4-V  TO      WK-PACK-V
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK4 S9(7)V99" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-PACK3
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK S9(36)   " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1.23        TO      WK-PACK3-V
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD PACK S9(34)V99" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-UNPACK
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD UNPACK S9(9)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1.23        TO      WK-UNPACK-V
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD UNPACK S9(7)V99" TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX




           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP2N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP S9(9)V9  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMPN
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP S9(3)V9  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-1N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-1        " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-2N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-2        " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-42N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-4 S9(9)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-4N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-4 S9(3)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-52N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-5 S9(9)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-5N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-5 S9(3)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-X2N
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-X S9V99  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-COMP-XN
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD COMP-X S9(3)  " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BI-DOUBLEN
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BI-DO         " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BI-SHORTN
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BI-SH         " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   ADD     1           TO      WK-BINARYN
           END-PERFORM
           ADD     1           TO      I
           MOVE    "ADD BINARY        " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           .
       S210-EX.
           EXIT.

       S201-10.

           MOVE    I           TO      WK-NO
           MOVE    WK-MSG-G    TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    WDT-DATE-SSMM TO    WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)

           PERFORM S200-10     THRU    S200-EX
           .
       S201-EX.
           EXIT.

      *    *** SUBTRACT
       S220-10.

           MOVE    12345       TO      WK-BINARY
           MOVE    12345       TO      WK-BI-SHORT
           MOVE    12345       TO      WK-BI-DOUBLE
           MOVE    12345       TO      WK-PACK
           MOVE    12345       TO      WK-UNPACK
           MOVE    123.45      TO      WK-PACK-V
           MOVE    123.45      TO      WK-UNPACK-V

           EVALUATE I

               WHEN 6
                   MULTIPLY 12345      BY      WK-BI-DOUBLE
               WHEN 7
                   MULTIPLY 12345      BY      WK-BI-SHORT
               WHEN 8
                   MULTIPLY 12345      BY      WK-BINARY
               WHEN 9
                   MULTIPLY 12345      BY      WK-PACK
               WHEN 11
                   MULTIPLY 12345.12   BY      WK-PACK-V
               WHEN 10
                   MULTIPLY 12345      BY      WK-UNPACK
               WHEN 12
                   MULTIPLY 12345.12   BY      WK-UNPACK-V

               WHEN 13
                   SUBTRACT 12345      FROM    WK-BI-DOUBLE
               WHEN 14
                   SUBTRACT 12345      FROM    WK-BI-SHORT
               WHEN 15
                   SUBTRACT 12345      FROM    WK-BINARY
               WHEN 16
                   SUBTRACT 12345      FROM    WK-PACK
               WHEN 17
                   SUBTRACT 12345      FROM    WK-UNPACK
               WHEN 18
                   SUBTRACT 12345      FROM    WK-PACK-V
               WHEN 19
                   SUBTRACT 12345      FROM    WK-UNPACK-V

               WHEN 13
                   SUBTRACT 12345      FROM    WK-BI-DOUBLE
               WHEN 14
                   SUBTRACT 12345      FROM    WK-BI-SHORT
               WHEN 15
                   SUBTRACT 12345      FROM    WK-BINARY
               WHEN 16
                   SUBTRACT 12345      FROM    WK-PACK
               WHEN 18
                   SUBTRACT 12345      FROM    WK-PACK-V
               WHEN 17
                   SUBTRACT 12345.12   FROM    WK-UNPACK
               WHEN 19
                   SUBTRACT 12345.12   FROM    WK-UNPACK-V

               WHEN 20
                   MOVE    WK-VALUE-10 TO      WK-SPACE-10
               WHEN 21
                   MOVE    WK-VALUE-100 TO     WK-SPACE-100
           END-EVALUATE
           .
       S220-EX.
           EXIT.

      *    *** MULTIPLY
       S230-10.

           MOVE    12345       TO      WK-BINARY
           MOVE    12345       TO      WK-BI-SHORT
           MOVE    12345       TO      WK-BI-DOUBLE
           MOVE    12345       TO      WK-PACK
           MOVE    12345       TO      WK-UNPACK
           MOVE    123.45      TO      WK-PACK-V
           MOVE    123.45      TO      WK-UNPACK-V
           .
       S230-EX.
           EXIT.

      *    *** DIVIDE
       S240-10.
           .
       S240-EX.
           EXIT.

      *    *** COMPUTE
       S250-10.
           .
       S250-EX.
           EXIT.

      *    *** MOVE
       S260-10.
           .
       S260-EX.
           EXIT.

      *    *** IF
       S270-10.

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   IF      WK-VALUE-1   =       WK-SPACE-1
                           CONTINUE
                   END-IF
           END-PERFORM
           ADD     1           TO      I
           MOVE    "IF   1   BYTE     " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   IF      WK-VALUE-10  =       WK-SPACE-10
                           CONTINUE
                   END-IF
           END-PERFORM
           ADD     1           TO      I
           MOVE    "IF   10  BYTE     " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   IF      WK-VALUE-100  =       WK-SPACE-100
                           CONTINUE
                   END-IF
           END-PERFORM
           ADD     1           TO      I
           MOVE    "IF   100 BYTE     " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL   I2   >       WK-SHORI-CNT
                   IF      WK-VALUE-200 =       WK-SPACE-200
                           CONTINUE
                   END-IF
           END-PERFORM
           ADD     1           TO      I
           MOVE    "IF   200 BYTE     " TO     WK-MSG
           PERFORM S201-10     THRU    S201-EX
           .
       S270-EX.
           EXIT.

      *    *** EVALUATE
       S280-10.
           .
       S280-EX.
           EXIT.

       S300-10.

           MOVE    "21.MOVE 100 BYTE"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.22
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   MOVE    WK-VALUE-200 TO      WK-SPACE-200
           END-PERFORM
           MOVE    "22.MOVE 200 BYTE"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.26
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BI-DOUBLE =       12345
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "26.IF DOUB NUM"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.27
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BI-DOUBLE =      WK-BI-DOUBLE2
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "27.IF DOUB=DOUB"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.28
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BI-SHORT =       12345
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "28.IF SHORT NUM"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.29
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BI-SHORT  =      WK-BI-SHORT2
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "29.IF SHORT=SHORT"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.30
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BINARY   =      12345
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "30.IF BINA NUM"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.31
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-BINARY =         WK-BINARY2
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "31.IF BINA=BINA"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.32
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-PACK     =       12345
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "32.IF PACK NUM"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.33
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-PACK     =       WK-PACK2
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "33.IF PACK=PACK"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.34
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-UNPACK   =       12345
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "34.IF UNPACK NUM"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.35
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-UNPACK   =       WK-UNPACK2
                           CONTINUE
                   END-IF
           END-PERFORM.
           MOVE    "35.IF UNPACK=UNPACK"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           PERFORM S300-10     THRU    S300-EX

      *    *** NO.36
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-SHORI-CNT
                   IF      WK-PACK     =      WK-UNPACK
                           CONTINUE
                   END-IF
           END-PERFORM
           MOVE    "36.IF PACK = UNPACK"
                               TO      WK-MEI1-I1
                                       WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           
           MOVE    WDT-DATE-SSMM TO     WK-MEI1-SSMM
           MOVE    WK-MEI1     TO      PR-LINE (J) (P1:30)
           
           MOVE    40          TO      J
           MOVE    124         TO      P1
           PERFORM S300-10     THRU    S300-EX

      *     PERFORM S100-10     THRU    S100-EX
           .
       S300-EX.
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
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
