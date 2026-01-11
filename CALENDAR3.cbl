       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALENDAR3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *--- System date (YYYYMMDD) from ACCEPT DATE
       01 WS-SYS-DATE            PIC 9(8) VALUE 0.
       01 WS-CUR-YEAR            PIC 9(4) VALUE 0.
       01 WS-CUR-MONTH           PIC 99    VALUE 0.
       01 WS-CUR-DAY             PIC 99    VALUE 0.

       *--- Current displayed year/month
       01 DISP-YEAR              PIC 9(4) VALUE 0.
       01 DISP-MONTH             PIC 99    VALUE 0.

       *--- Table for Sakamoto algorithm (t array)
       01 T-ARRAY.
      *    05 T-ELEM              PIC 99 OCCURS 12
      *         VALUE 0,3,2,5,0,3,5,1,4,6,2,4.
          05 T-ELEM-R PIC X(024)
               VALUE "000302050003050104060204".
          
          05 T-ELEM-X REDEFINES T-ELEM-R.
            07 T-ELEM           PIC 9(2) OCCURS 12.

       *--- General numeric helpers
       01 WS-DAY                 PIC 99    VALUE 1.
       01 DAYS-IN-MONTH          PIC 99    VALUE 31.
       01 DOW                    PIC 9     VALUE 0.  
      *> 0=Sunday .. 6=Saturday

       *--- Input command buffer
       01 CMD-LINE               PIC X(32) VALUE SPACES.
       01 CMD-CHAR               PIC X     VALUE SPACE.
       01 IDX                    PIC 99    VALUE 1.

       *--- temporary numeric values
       01 TMP-Y                  PIC 9(9) COMP-5.
       01 TMP-SUM                PIC 9(9) COMP-5.
       01 TMP-REM                PIC 9(9) COMP-5.
       01 TMP-NUM                PIC 9(9) COMP-5.
       01 YDIV4                  PIC 9(9) COMP-5.
       01 YDIV100                PIC 9(9) COMP-5.
       01 YDIV400                PIC 9(9) COMP-5.
       01 TMP-Q                  PIC 9(9) COMP-5.
       01 TMP-R                  PIC 9(9) COMP-5.
       01 TMP-ANS                PIC 9(9) COMP-5.
       01 TMP-MOD4               PIC 9(9) COMP-5.
       01 TMP-MOD100             PIC 9(9) COMP-5.
       01 TMP-MOD400             PIC 9(9) COMP-5.
       01 TAB-T                  PIC 9(2)  VALUE 0.
       01  WK-A.
         03  PIC X(20) VALUE "コマンド: n(次月) p(".
         03  PIC X(20) VALUE "前月) g(指定: g ->  ".
         03  PIC X(20) VALUE "年 月入力) t(今日) q".
         03  PIC X(07) VALUE "(終了) ".

       *--- month names (Japanese)
       01 MONTH-NAMES.
      *    05 MNAME              PIC X(9) OCCURS 12
      *         VALUE " 1月"," 2月"," 3月"," 4月"," 5月"," 6月",
      *               " 7月"," 8月"," 9月","10月","11月","12月".
          05 MNAME-R PIC X(48)
               VALUE " 1月 2月 3月 4月 5月 6月 7月 8月 9月10月11月12月".
          05 MNAME-X REDEFINES MNAME-X.
            07 MNAME            PIC X(4) OCCURS 12.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *> get system date and initialize display month/year
      *     ACCEPT WS-SYS-DATE FROM DATE
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD

      *> YYYY = WS-SYS-DATE / 10000  (integer division)
           DIVIDE WS-SYS-DATE BY 10000 GIVING WS-CUR-YEAR 
                  REMAINDER TMP-REM

      *> remainder contains MMDD -> month = remainder / 100 , day = remainder MOD 100
           DIVIDE TMP-REM BY 100 GIVING WS-CUR-MONTH
                  REMAINDER WS-CUR-DAY

           MOVE WS-CUR-YEAR TO DISP-YEAR
           MOVE WS-CUR-MONTH TO DISP-MONTH
           .

       MAIN-LOOP.
           PERFORM DISPLAY-MONTH
           DISPLAY WK-A
      * "コマンド: n(次月) p(前月) g(指定: g ->年 月入力) t(今日) q(終了)"
           WITH NO ADVANCING
           ACCEPT CMD-LINE

      *> find first non-space char (simple)
           MOVE 1 TO IDX
           MOVE SPACE TO CMD-CHAR
           PERFORM UNTIL IDX > 32 OR CMD-CHAR NOT = SPACE
               IF CMD-LINE(IDX:1) NOT = SPACE
                   MOVE CMD-LINE(IDX:1) TO CMD-CHAR
               ELSE
                   ADD 1 TO IDX
               END-IF
           END-PERFORM

           IF CMD-CHAR = SPACE
               GO TO MAIN-LOOP
           END-IF

      *> handle commands (check both lower & upper by comparing both cases)
           IF CMD-CHAR = "n" OR CMD-CHAR = "N"
               PERFORM NEXT-MONTH
               GO TO MAIN-LOOP
           ELSE IF CMD-CHAR = "p" OR CMD-CHAR = "P"
               PERFORM PREV-MONTH
               GO TO MAIN-LOOP
           ELSE IF CMD-CHAR = "t" OR CMD-CHAR = "T"
               MOVE WS-CUR-YEAR TO DISP-YEAR
               MOVE WS-CUR-MONTH TO DISP-MONTH
               GO TO MAIN-LOOP
           ELSE IF CMD-CHAR = "g" OR CMD-CHAR = "G"
               DISPLAY "ジャンプ先の年を入力(例: 2026) > "
                       WITH NO ADVANCING
               ACCEPT CMD-LINE
      *         IF CMD-LINE NUMERIC
               IF CMD-LINE (1:4) NUMERIC
                   MOVE CMD-LINE TO DISP-YEAR
               ELSE
                   DISPLAY "無効な年です。"
                   MOVE WS-CUR-YEAR TO DISP-YEAR
               END-IF
      *         DISPLAY "ジャンプ先の月を入力(1-12) > " 
               DISPLAY "ジャンプ先の月を入力(01-12) > " 
                       WITH NO ADVANCING
               ACCEPT CMD-LINE
      *         IF CMD-LINE NUMERIC
               IF CMD-LINE (1:2) NUMERIC
                   MOVE CMD-LINE TO DISP-MONTH
                   IF DISP-MONTH < 1 OR DISP-MONTH > 12
                       DISPLAY "無効な月です。"
                       MOVE WS-CUR-YEAR TO DISP-YEAR
                       MOVE WS-CUR-MONTH TO DISP-MONTH
                   END-IF
               ELSE
                   DISPLAY "無効な月入力。"
                   MOVE WS-CUR-YEAR TO DISP-YEAR
                   MOVE WS-CUR-MONTH TO DISP-MONTH
               END-IF
               GO TO MAIN-LOOP
           ELSE IF CMD-CHAR = "q" OR CMD-CHAR = "Q"
               DISPLAY "終了します。"
               STOP RUN
           ELSE
               DISPLAY "不明なコマンドです。"
               GO TO MAIN-LOOP
           END-IF
           .

       DISPLAY-MONTH.
      *> header
           DISPLAY " "
           DISPLAY "=============================="
      *     DISPLAY MNAME(DISP-MONTH) " " DISP-YEAR
           DISPLAY  DISP-YEAR "年 " MNAME(DISP-MONTH) 
      *     DISPLAY "日 月 火 水 木 金 土"
           DISPLAY " 日  月  火  水  木  金  土"

      *> compute days in month
           PERFORM COMPUTE-DAYS-IN-MONTH

      *> compute day-of-week for first day (Sakamoto without FUNCTION)
           MOVE DISP-YEAR TO TMP-Y
           MOVE DISP-MONTH TO TMP-NUM
           IF TMP-NUM < 3
               SUBTRACT 1 FROM TMP-Y
           END-IF

      *> integer divisions to replace FUNCTION INTEGER(...)
           DIVIDE TMP-Y BY 4   GIVING YDIV4   REMAINDER TMP-REM
           DIVIDE TMP-Y BY 100 GIVING YDIV100 REMAINDER TMP-REM
           DIVIDE TMP-Y BY 400 GIVING YDIV400 REMAINDER TMP-REM

      *> get t-array offset value
           MOVE T-ELEM (DISP-MONTH) TO TAB-T

      *> sum = Y + Y/4 - Y/100 + Y/400 + t[m-1] + 1
           COMPUTE TMP-SUM = TMP-Y + YDIV4 
                           - YDIV100 + YDIV400 + TAB-T + 1

      *> DOW = TMP-SUM MOD 7 (use DIVIDE ... REMAINDER)
           DIVIDE TMP-SUM BY 7 GIVING TMP-Q REMAINDER TMP-R
           MOVE TMP-R TO DOW

      *> print leading spaces for first week
           MOVE 1 TO WS-DAY
           PERFORM VARYING WS-DAY FROM 1 BY 1 UNTIL WS-DAY > DOW
      *         DISPLAY "   " WITH NO ADVANCING
               DISPLAY "    " WITH NO ADVANCING
           END-PERFORM

      *> print all days
           MOVE 1 TO WS-DAY
           PERFORM VARYING WS-DAY FROM 1 BY 1 
                   UNTIL WS-DAY > DAYS-IN-MONTH
      *> print day padded to width 3
               IF WS-DAY < 10
                   DISPLAY " " WS-DAY " " WITH NO ADVANCING
               ELSE
      *             DISPLAY WS-DAY " " WITH NO ADVANCING
                   DISPLAY " " WS-DAY " " WITH NO ADVANCING
               END-IF

      *> update DOW
      *     DISPLAY "TMP-Q=" TMP-Q  " DOW=" DOW
               COMPUTE TMP-Q = (DOW + 1)
      *         COMPUTE DOW = TMP-Q - ( (TMP-Q / 7) * 7 )
           ADD 1 TO DOW
      *     DISPLAY " DOW=" DOW
      *> emulate modulo (but simple)
      *> easier/clear: do small arithmetic
               IF DOW > 6
                   SUBTRACT 7 FROM DOW
               END-IF

      *     DISPLAY "TMP-Q=" TMP-Q  " DOW=" DOW

               IF DOW = 0
                  DISPLAY " "  END-DISPLAY
      *> newline when week rolls
               END-IF
           END-PERFORM
           DISPLAY " "
      *> ensure newline
           DISPLAY "=============================="
           .

       COMPUTE-DAYS-IN-MONTH.
           EVALUATE DISP-MONTH
               WHEN 1  MOVE 31 TO DAYS-IN-MONTH
               WHEN 2
                    DIVIDE DISP-YEAR BY 4   GIVING TMP-ANS 
                           REMAINDER TMP-MOD4
                    DIVIDE DISP-YEAR BY 100 GIVING TMP-ANS 
                           REMAINDER TMP-MOD100
                    DIVIDE DISP-YEAR BY 400 GIVING TMP-ANS 
                           REMAINDER TMP-MOD400
      *> leap check (year divisible by 400 OR divisible by 4 but not 100)
      *              IF ( (DISP-YEAR MOD 400) = 0 ) OR
      *                 ( ( (DISP-YEAR MOD 4) = 0 )
      *                  AND ( (DISP-YEAR MOD 100) NOT = 0 ) )
                    IF ( TMP-MOD400 = 0 ) OR
                       ( ( TMP-MOD4 = 0 )
                        AND ( TMP-MOD100 NOT = 0 ) )
                    THEN
                        MOVE 29 TO DAYS-IN-MONTH
                    ELSE
                        MOVE 28 TO DAYS-IN-MONTH
                    END-IF
               WHEN 3  MOVE 31 TO DAYS-IN-MONTH
               WHEN 4  MOVE 30 TO DAYS-IN-MONTH
               WHEN 5  MOVE 31 TO DAYS-IN-MONTH
               WHEN 6  MOVE 30 TO DAYS-IN-MONTH
               WHEN 7  MOVE 31 TO DAYS-IN-MONTH
               WHEN 8  MOVE 31 TO DAYS-IN-MONTH
               WHEN 9  MOVE 30 TO DAYS-IN-MONTH
               WHEN 10 MOVE 31 TO DAYS-IN-MONTH
               WHEN 11 MOVE 30 TO DAYS-IN-MONTH
               WHEN 12 MOVE 31 TO DAYS-IN-MONTH
               WHEN OTHER MOVE 31 TO DAYS-IN-MONTH
           END-EVALUATE
           .

       NEXT-MONTH.
           ADD 1 TO DISP-MONTH
           IF DISP-MONTH > 12
               MOVE 1 TO DISP-MONTH
               ADD 1 TO DISP-YEAR
           END-IF
           .

       PREV-MONTH.
           SUBTRACT 1 FROM DISP-MONTH
           IF DISP-MONTH < 1
               MOVE 12 TO DISP-MONTH
               SUBTRACT 1 FROM DISP-YEAR
           END-IF
           .
