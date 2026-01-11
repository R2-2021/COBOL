       IDENTIFICATION DIVISION.
       PROGRAM-ID. calendar.
       AUTHOR. Gemini.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  USER-INPUT.
           05 IN-YEAR          PIC 9(04).
           05 IN-MONTH         PIC 9(02).

       01  CALCULATION-VARS.
           05 Y                PIC 9(04).
           05 M                PIC 9(02).
           05 D                PIC 9(02) VALUE 1.
           05 K                PIC 9(02).
           05 J                PIC 9(02).
           05 H                PIC 9(02).
           05 WORK-VAL         PIC S9(04).
           05 REMAINDER-VAL    PIC 9(02).

       01  DATE-VARS.
           05 DAYS-IN-MONTH    PIC 9(02).
           05 START-DAY        PIC 9(01).
           05 CURRENT-DAY      PIC 9(02) VALUE 1.
           05 PRINT-POS        PIC 9(02).
           
       01  FLAGS.
           05 IS-LEAP-YEAR     PIC X(01) VALUE 'N'.

       01  MONTH-DAYS-TABLE.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 28.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 30.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 30.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 30.
           05 FILLER PIC 9(02) VALUE 31.
           05 FILLER PIC 9(02) VALUE 30.
           05 FILLER PIC 9(02) VALUE 31.
       
       01  MONTH-DAYS-ARRAY REDEFINES MONTH-DAYS-TABLE.
           05 M-DAYS OCCURS 12 TIMES PIC 9(02).

       01  OUTPUT-LINE         PIC X(21) VALUE SPACES.
       01  HEADER-LINE         PIC X(21) VALUE "Su Mo Tu We Th Fr Sa".
       01  DISPLAY-DAY         PIC Z9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "----------------------------".
           DISPLAY "   COBOL CALENDAR APP   ".
           DISPLAY "----------------------------".
           
           DISPLAY "Enter Year (e.g. 2025): " WITH NO ADVANCING.
           ACCEPT IN-YEAR.
           
           DISPLAY "Enter Month (1-12): " WITH NO ADVANCING.
           ACCEPT IN-MONTH.

           IF IN-MONTH < 1 OR IN-MONTH > 12
               DISPLAY "Invalid Month. Exiting."
               STOP RUN
           END-IF.

           PERFORM CHECK-LEAP-YEAR.
           PERFORM GET-DAYS-IN-MONTH.
           PERFORM CALCULATE-START-DAY.
           PERFORM PRINT-CALENDAR.

           STOP RUN.

       CHECK-LEAP-YEAR.
      * うるう年判定: 4で割り切れる かつ (100で割り切れない または 400で割り切れる)
           DIVIDE IN-YEAR BY 4 GIVING WORK-VAL REMAINDER REMAINDER-VAL.
           IF REMAINDER-VAL = 0
               DIVIDE IN-YEAR BY 100 GIVING WORK-VAL 
                      REMAINDER REMAINDER-VAL
               IF REMAINDER-VAL NOT = 0
                   MOVE 'Y' TO IS-LEAP-YEAR
               ELSE
                   DIVIDE IN-YEAR BY 400 GIVING WORK-VAL 
                          REMAINDER REMAINDER-VAL
                   IF REMAINDER-VAL = 0
                       MOVE 'Y' TO IS-LEAP-YEAR
                   END-IF
               END-IF
           END-IF.

       GET-DAYS-IN-MONTH.
           MOVE M-DAYS(IN-MONTH) TO DAYS-IN-MONTH.
           IF IN-MONTH = 2 AND IS-LEAP-YEAR = 'Y'
               ADD 1 TO DAYS-IN-MONTH
           END-IF.

       CALCULATE-START-DAY.
      * ツェラーの公式を使用
      * 1月と2月は前年の13月、14月として扱う
           MOVE IN-YEAR TO Y.
           MOVE IN-MONTH TO M.
           
           IF M = 1 OR M = 2
               SUBTRACT 1 FROM Y
               ADD 12 TO M
           END-IF.

           MOVE Y TO J.
           DIVIDE J BY 100 GIVING J.
           
           MOVE Y TO K.
           DIVIDE K BY 100 GIVING WORK-VAL REMAINDER K.

      * h = (q + [(13(m+1))/5] + K + [K/4] + [J/4] - 2J) mod 7
           COMPUTE H = (1 + ((13 * (M + 1)) / 5) + K + (K / 4) + (J / 4) 
                     + (5 * J))
           DIVIDE H BY 7 GIVING WORK-VAL REMAINDER H.
           
      * Hの値: 0=土, 1=日, 2=月 ... 6=金
      * カレンダー表示用に調整 (0=日, 1=月... に変換したいが、
      * ツェラーの標準出力は 0=Sat, 1=Sun.
      * ここでは単純に日曜始まりのカレンダーのためのオフセットを計算
           
           IF H = 0
               MOVE 6 TO START-DAY
           ELSE
               COMPUTE START-DAY = H - 1
           END-IF.

       PRINT-CALENDAR.
           DISPLAY " ".
           DISPLAY "      " IN-YEAR " / " IN-MONTH.
           DISPLAY HEADER-LINE.
           
           MOVE SPACES TO OUTPUT-LINE.
           MOVE 0 TO PRINT-POS.

      * 最初の日の空白を埋める
           PERFORM VARYING WORK-VAL FROM 1 BY 1 UNTIL WORK-VAL > 
                   START-DAY
               MOVE "   " TO OUTPUT-LINE(PRINT-POS * 3 + 1 : 3)
               ADD 1 TO PRINT-POS
           END-PERFORM.

      * 日付を出力するループ
           PERFORM UNTIL CURRENT-DAY > DAYS-IN-MONTH
               MOVE CURRENT-DAY TO DISPLAY-DAY
               MOVE DISPLAY-DAY TO OUTPUT-LINE(PRINT-POS * 3 + 1 : 2)
               
               ADD 1 TO PRINT-POS
               ADD 1 TO CURRENT-DAY
               
               IF PRINT-POS = 7
                   DISPLAY OUTPUT-LINE
                   MOVE SPACES TO OUTPUT-LINE
                   MOVE 0 TO PRINT-POS
               END-IF
           END-PERFORM.

      * 最後の行に残があれば出力
           IF PRINT-POS > 0
               DISPLAY OUTPUT-LINE
           END-IF.
           DISPLAY " ".