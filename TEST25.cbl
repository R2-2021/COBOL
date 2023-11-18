      *    *** 画面テスト、アイドルデータ表示

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST25.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
      *    *** この指定でCOB-CRT-STATUSが定義しなくても、使用可
      *    *** PF01-PF24,SHIFT,CTRL,ALTも可 1001 - 1064
           FUNCTION ALL INTRINSIC. 
      * SPECIAL-NAMES.
      *     CRT STATUS IS COB-CRT-STATUS. 
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION INDEXED
           ACCESS RANDOM
           RECORD KEY PIN1-KEY.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC
           03  PIN1-KEY        PIC  9(004).
           03  PIN1-DATA       PIC  X(2048).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(256).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST25  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST22.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST25.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-SPACE        PIC  X(080) VALUE SPACE.
      *    *** 0.5 SEC = 500,000,000 NANO SEC ナノは９桁
           03  WK-NANOSEC      PIC  9(011) VALUE 500000000.
           03  WK-TESTNO       PIC  9(002) VALUE 0.
           03  WK-ITEM1-R.
             05  WK-ITEM1      PIC  9(003) VALUE 0.
           03  WK-ITEM2        PIC  9(002) VALUE 0.
           03  WK-ITEM3        PIC  X(010) VALUE SPACE.
           03  WK-ITEM4        PIC  X(010) VALUE SPACE.

      *     03  WK-DATA1        PIC  9(004) VALUE ZERO.
      *     03  WK-DATA2        PIC  X(040) VALUE SPACE.
           03  WK-DATA3        PIC  X(010) VALUE SPACE.
           03  WK-DATA4        PIC  X(010) VALUE SPACE.
           03  WK-DATA5        PIC  X(010) VALUE SPACE.
           03  WK-DATA6        PIC  X(010) VALUE SPACE.
           03  WK-URL          PIC  X(078) VALUE SPACE.
           03  WK-L            PIC  9(003) VALUE ZERO.

           03  WK-PIN1-KEY     PIC  9(004) VALUE ZERO.
           03  WK-PIN1-I1      PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I2      PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I3      PIC  X(075) VALUE SPACE.
           03  WK-PIN1-I4-R    PIC  X(1600) VALUE SPACE.
           03  WK-PIN1-I4.
             05  WK-PIN1-I4-01 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-02 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-03 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-04 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-05 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-06 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-07 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-08 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-09 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-10 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-11 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-12 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-13 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-14 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-15 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-16 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-17 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-18 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-19 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-20 PIC  X(080) VALUE SPACE.

      *    *** 画面項目
           03  WK-KEI1         PIC  X(002) VALUE "─".
           03  WK-KEI2         PIC  X(002) VALUE "│".
           03  WK-KEI3         PIC  X(002) VALUE "┌".
           03  WK-KEI4         PIC  X(002) VALUE "┐".
           03  WK-KEI5         PIC  X(002) VALUE "┘".
           03  WK-KEI6         PIC  X(002) VALUE "└".
           03  WK-KEI7         PIC  X(002) VALUE "├".
           03  WK-KEI8         PIC  X(002) VALUE "┬".
           03  WK-KEI9         PIC  X(002) VALUE "┤".
           03  WK-KEI10        PIC  X(002) VALUE "┴".
           03  WK-KEI11        PIC  X(002) VALUE "┼".

           03  WK-HAI1.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(040) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"

           03  WK-HAI2.
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE ALL "┼"
             05  FILLER        PIC  X(040) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
             05  FILLER        PIC  X(001) VALUE X"0A"

           03  WK-HAI3.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(040) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"

           03  WK-MEI2.
             05  FILLER        PIC  X(002) VALUE "│"
             05  WK-DATA1      PIC  9(004) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE "│"
             05  WK-DATA2      PIC  X(040) VALUE SPACE.
             05  FILLER        PIC  X(002) VALUE "│"
      *    *** X"0A" LF LINE FEED
             05  FILLER        PIC  X(001) VALUE X"0A"

           03  WK-TIT1.
             05  FILLER        PIC  X(015) VALUE SPACE.
             05  FILLER        PIC  X(030) VALUE
                 "＊＊＊　アイドルデータ　＊＊＊".
             05  FILLER        PIC  X(005) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "("
             05  WK-TIT1-WEEK-NK PIC X(002) VALUE SPACE.
             05  FILLER        PIC  X(001) VALUE ")".
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.

           03  WK-MID1         PIC  X(004) VALUE
               "項目".
           03  WK-HAI          PIC  X(080) VALUE
               ALL "-".
           03  WK-MEI1.
             05  WK-MEI1-I00   PIC  X(080) VALUE
               "００．初期メニュー".
             05  WK-MEI1-I01   PIC  X(080) VALUE
               "０１．アイドル一覧表示　（ＰＦ７：前頁、ＰＦ８：次頁）".
             05  WK-MEI1-I02   PIC  X(080) VALUE
               "０２．アイドル内容表示　（ＰＦ７：前頁、ＰＦ８：次頁）".
             05  WK-MEI1-I03   PIC  X(080) VALUE
               "０３．アイドル一覧表示２（ＰＦ７：前頁、ＰＦ８：次頁）".
             05  WK-MEI1-I99   PIC  X(080) VALUE
               "９９．終了".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  HEX-DIGITS          VALUE '0123456789ABCDEF'.
           05  HEX-DIGIT       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-HALFWORD        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-DISPLAY      PIC  X(001) VALUE ZERO.
           03  SW-AMP          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

       01  COLOURS-AREA.
           03  BLACK           PIC  9(001) VALUE 0.
           03  BLUE            PIC  9(001) VALUE 1.
           03  GREEN           PIC  9(001) VALUE 2.
           03  CYAN            PIC  9(001) VALUE 3.
           03  RED             PIC  9(001) VALUE 4.
           03  MAGENTA         PIC  9(001) VALUE 5.
           03  BROWN           PIC  9(001) VALUE 6.
      *    *** HIGHLIGHT時は黄色
           03  YELLOW          PIC  9(001) VALUE 6.
           03  GREY            PIC  9(001) VALUE 7.
      *    *** HIGHLIGHT時は白色
           03  WHITE           PIC  9(001) VALUE 7.

       SCREEN                  SECTION.
       01  SCREEN-AREA.

           03  SCR01-AREA.
             05  LINE 25 COL 1             VALUE " "
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR02-AREA.
             05  LINE 25 COL 10            VALUE "Ｎｏ．"
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
      *          HIGHLIGHT
      *          OVERLINE
                 UNDERLINE.
             05  COL PLUS 2 USING WK-TESTNO
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
      *          OVERLINE
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).
      *
      * 不明          JUST
      * 入力＊        SECURE
      * 不明          FULL
      * 不明           PROMPT
      * 反転           REVERSE-VIDEO
      * 画面初期化     ERASE EOL
      * 指定出来ない   EOS
             05  COL PLUS 2 USING WK-ITEM1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(003).

             05  COL PLUS 2 USING WK-ITEM2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM3
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 AUTO-SKIP
                 PIC X(010).

             05  COL PLUS 2 USING WK-ITEM4
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 AUTO-SKIP
                 PIC X(010).

           03  SCR03-AREA.
             05  LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  USING WK-DATA1
                 LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  9(004).
             05  LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  USING WK-DATA2
                 LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(040).
             05  LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".

           03  SCR04-AREA.
             05  LINE 1 COL 1 TO WK-PIN1-I1
                 PIC  X(80)
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
             05  LINE 2 COL 1 TO WK-PIN1-I2
                 PIC  X(80)
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR05-AREA.
             05  USING WK-URL
      *           LINE L
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(078).

           03  SCR07-AREA.
             05  LINE 10 COL 8
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  USING WK-DATA1
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  USING WK-DATA2
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  USING WK-DATA3
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  FROM WK-DATA4
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  FROM WK-DATA5
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".
             05  FROM WK-DATA6
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(010).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "│".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           PERFORM UNTIL   WK-TESTNO   =       99
                   EVALUATE TRUE

                       WHEN WK-TESTNO = 0
      *    *** 初期画面　表示
                            PERFORM S020-10     THRU    S020-EX

                       WHEN WK-TESTNO = 1
      *    *** WK-TESTNO = 1 画面クリアー　表示
                            PERFORM S210-10     THRU    S210-EX
                            MOVE    LOW-VALUE   TO        WK-PIN1-EOF
      *    *** PF7=1007
                            IF      COB-CRT-STATUS  =     1007
                                IF  WK-ITEM1    >=      41
                                    COMPUTE WK-ITEM1 = WK-ITEM1 - 40
                                ELSE
                                    MOVE   1         TO    WK-ITEM1
                                END-IF
                            END-IF

      *    *** PF8=1008 はエンターと同じなので定義しない
                            PERFORM VARYING M FROM 1 BY 1
                                UNTIL M > 20 OR
                                      WK-PIN1-EOF = HIGH-VALUE

      *    *** アイドルデータ　リード
                                PERFORM S100-10     THRU    S100-EX

                                IF    WK-PIN1-EOF NOT = HIGH-VALUE
      *    *** WK-TESTNO = 1 画面　表示
                                    PERFORM S211-10    THRU    S211-EX
                                END-IF
                                ADD     1           TO        WK-ITEM1
                            END-PERFORM 

                            DISPLAY 
                             "ＰＦ７：前頁、ＰＦ８（Ｅｎｔｅｒ）：次頁"
                                    AT LINE 23 COL 10
                                        WITH 
                                          BACKGROUND-COLOR CYAN
                                          FOREGROUND-COLOR WHITE
                            END-DISPLAY


                        WHEN WK-TESTNO = 2
                            IF      COB-CRT-STATUS  =     1007
                                IF  WK-ITEM1    >=      2

                                    COMPUTE WK-ITEM1 = WK-ITEM1 - 2
                                ELSE
                                    MOVE   1         TO    WK-ITEM1
                                END-IF
                            END-IF

      *    *** アイドルデータ　リード
                            PERFORM S100-10     THRU      S100-EX
      *    *** WK-TESTNO = 2 画面　表示
                            PERFORM S220-10     THRU      S220-EX
                            ADD     1           TO        WK-ITEM1

                            DISPLAY 
                             "ＰＦ７：前頁、ＰＦ８（Ｅｎｔｅｒ）：次頁"
                                    AT LINE 23 COL 10
                                        WITH 
                                          BACKGROUND-COLOR CYAN
                                          FOREGROUND-COLOR WHITE
                            END-DISPLAY

                       WHEN WK-TESTNO = 3

      *    *** 枠付き、一覧表示
                            PERFORM S230-10     THRU    S230-EX
                            MOVE    LOW-VALUE   TO      WK-PIN1-EOF
      *    *** PF7=1007
                            IF      COB-CRT-STATUS  =     1007
                                IF  WK-ITEM1    >=      21
                                    COMPUTE WK-ITEM1 = WK-ITEM1 - 20
                                ELSE
                                    MOVE   1         TO    WK-ITEM1
                                END-IF
                            END-IF
      *    *** PF8=1008 はエンターと同じなので定義しない
                            PERFORM VARYING M FROM 1 BY 1
                                UNTIL M > 10 OR
                                      WK-PIN1-EOF = HIGH-VALUE

      *    *** アイドルデータ　リード
                                PERFORM S100-10     THRU    S100-EX

                                IF    WK-PIN1-EOF =    HIGH-VALUE
      *                                ADD -1 TO L
                                      DISPLAY WK-HAI3
                                          AT LINE L COL 1
                                          WITH 
                                            BACKGROUND-COLOR CYAN
                                            FOREGROUND-COLOR WHITE
                                      END-DISPLAY
                                ELSE
      *    *** WK-TESTNO = 3 画面　表示
                                    PERFORM S231-10    THRU    S231-EX
                                END-IF
                                ADD     1           TO        WK-ITEM1
                            END-PERFORM 

                            DISPLAY 
                             "ＰＦ７：前頁、ＰＦ８（Ｅｎｔｅｒ）：次頁"
                                    AT LINE 23 COL 10
                                        WITH 
                                          BACKGROUND-COLOR CYAN
                                          FOREGROUND-COLOR WHITE
                            END-DISPLAY

                   END-EVALUATE

      *    *** 入力待ち
                   PERFORM S030-10    THRU    S030-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.

      *    DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    WDT-DATE-YY  TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM  TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD  TO      WK-TIT1-DD
           MOVE    WDT-DATE-WEEK-NK TO  WK-TIT1-WEEK-NK

           MOVE    WDT-DATE-HH  TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI  TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS  TO      WK-TIT1-SS

           OPEN    I-O         PIN1-F
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
           MOVE    1           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** 初期画面　表示
       S020-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       25
                   DISPLAY WK-SPACE
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           DISPLAY WK-TIT1
                   AT LINE 1 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MID1
                   AT LINE 2 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI
                   AT LINE 3 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MEI1-I00
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MEI1-I01
                   AT LINE 5 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MEI1-I02
                   AT LINE 6 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MEI1-I03
                   AT LINE 7 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-MEI1-I99
                   AT LINE 23 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           .

       S020-20.
           DISPLAY SCR02-AREA
           .
       S020-EX.
           EXIT.

      *    *** 入力待ち
       S030-10.

      *    *** SPACE に出来なかった
      *     MOVE    SPACE        TO      WK-ITEM1-R
           ACCEPT  SCR02-AREA
           .
       S030-EX.
           EXIT.

      *    *** アイドルデータ　リード
       S100-10.

           IF      WK-ITEM1    >=      1 AND
                   WK-ITEM1    <=      120
                   CONTINUE
           ELSE
                   MOVE    1           TO      WK-ITEM1
           END-IF

           MOVE    WK-ITEM1    TO      WK-PIN1-KEY
           MOVE    WK-PIN1-KEY TO      PIN1-KEY

           READ    PIN1-F
                   KEY PIN1-KEY
               INVALID 
      *             DISPLAY "TEST25 PIN1-F READ ERROR KEY=" PIN1-KEY
                   CONTINUE
               NOT INVALID
                   CONTINUE
           END-READ

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT
                   MOVE    WK-PIN1-STATUS TO     WK-ITEM2
                   MOVE    "読込成功"  TO        WK-ITEM3
           ELSE
               IF  WK-PIN1-STATUS =    10 OR 23
                   MOVE    HIGH-VALUE  TO        WK-PIN1-EOF
                   MOVE    "読込失敗"  TO        WK-ITEM3
                   MOVE    SPACE       TO        WK-PIN1-I1
                                                 WK-PIN1-I2
                                                 WK-PIN1-I3
                                                 WK-PIN1-I4
                   MOVE    WK-PIN1-STATUS TO     WK-ITEM2
                   MOVE    0           TO        WK-ITEM1
                   GO  TO  S100-EX
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

           UNSTRING PIN1-DATA
                    DELIMITED BY ","
                    INTO
                    WK-PIN1-I1
                    WK-PIN1-I2
                    WK-PIN1-I3
                    WK-PIN1-I4-R  COUNT L2

           MOVE    SPACE       TO      WK-PIN1-I4
           MOVE    1           TO      J 
           MOVE    ZERO        TO      J2 K

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L2 OR J > 1590
      *    *** 漢字か？一部漢字でない部分あり、
      *    *** 後から指定された部分も指定してない
               IF ( WK-PIN1-I4-R (I:2) >= X"8140" AND 
                    WK-PIN1-I4-R (I:2) <= X"9FFC" )   OR
                  ( WK-PIN1-I4-R (I:2) >= X"E040" AND 
                    WK-PIN1-I4-R (I:2) <= X"EAA4" )
                         MOVE   WK-PIN1-I4-R (I:2) TO   WK-PIN1-I4(J:2)
                         ADD    2    TO     K
      *    *** J 1,3,5...
      *    *** J 81.83,85...
                         ADD    1    TO     I
                         ADD    2    TO     J
                   ELSE
                         MOVE   WK-PIN1-I4-R (I:1) TO   WK-PIN1-I4(J:1)
                         ADD    1    TO     K
      *    *** J 1,2,3...
      *    *** J 81,82,83...
      *    *** J 161.162,163...
                         ADD    1    TO     J
                   END-IF
                   
      *    ***     IF    K    >   73
                   IF    K    >   77
      *            IF    K    >   75
                         ADD   80    TO     J2
      *    *** J 81,161...
                         ADD   J2 1  GIVING J
                         MOVE  ZERO  TO     K
                   END-IF
           END-PERFORM

           GO  TO  S100-EX

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WK-SU
      *     ADD     1           TO      WK-SEQ
      *     MOVE    100         TO      WK-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I3
      *                                 WK-LEN
           .
       S100-EX.
           EXIT.

      *    *** WK-TESTNO = 1 画面クリアー　表示
       S210-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S210-EX.
           EXIT.

      *    *** WK-TESTNO = 1 画面　表示
       S211-10.

           DISPLAY WK-PIN1-KEY
                   AT LINE M COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   UNDERLINE
           END-DISPLAY

           DISPLAY WK-PIN1-I3
      *    *** DISPLAY ではPLUS 指定不可
      *             AT LINE M COL PLUS 2
                   AT LINE M COL 6
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   UNDERLINE
           END-DISPLAY
           .
       S211-EX.
           EXIT.

      *    *** WK-TESTNO = 2 画面　表示
       S220-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           GO  TO  S220-20

           DISPLAY SCR04-AREA
      *             AT LINE 1 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           .
       S220-20.

           DISPLAY WK-PIN1-I1
      *     MOVE    WK-PIN1-I1  TO      WK-URL
      *     DISPLAY SCR05-AREA
                   AT LINE 1 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I2
      *     MOVE    WK-PIN1-I2  TO      WK-URL
      *     DISPLAY SCR05-AREA
                   AT LINE 2 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-ITEM1
                   AT LINE 3 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I3
      *      MOVE    WK-PIN1-I3  TO      WK-URL
      *      DISPLAY SCR05-AREA
                   AT LINE 4 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-01
                   AT LINE 6 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-02
                   AT LINE 7 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-03
                   AT LINE 8 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-04
                   AT LINE 9 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-05
                   AT LINE 10 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-06
                   AT LINE 11 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-07
                   AT LINE 12 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-08
                   AT LINE 13 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-09
                   AT LINE 14 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-10
                   AT LINE 15 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-11
                   AT LINE 16 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-12
                   AT LINE 17 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-13
                   AT LINE 18 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-14
                   AT LINE 19 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-15
                   AT LINE 20 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-16
                   AT LINE 21 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-17
                   AT LINE 22 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-18
                   AT LINE 23 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-19
                   AT LINE 24 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *     DISPLAY WK-PIN1-I4-20
      *             AT LINE 25 COL 1
      *             WITH 
      *             BACKGROUND-COLOR CYAN
      *             FOREGROUND-COLOR WHITE
      *     END-DISPLAY
           .
       S220-EX.
           EXIT.

      *    *** WK-TESTNO = 3 画面クリアー　表示
       S230-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S230-EX.
           EXIT.

      *    *** WK-TESTNO = 3 画面　表示
       S231-10.

           IF      M           =       1
                   MOVE    1           TO      L
                   DISPLAY WK-HAI1
                           AT LINE L COL 1
                           WITH 
                           BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-IF

           MOVE    WK-PIN1-KEY TO      WK-DATA1
           MOVE    WK-PIN1-I3  TO      WK-DATA2

           ADD     1           TO      L
      *     DISPLAY SCR03-AREA
      *             AT LINE L COL C
      *             WITH 
      *             BACKGROUND-COLOR CYAN
      *             FOREGROUND-COLOR WHITE
      *     END-DISPLAY

      *    *** 行末にＬＦ無いと文字ずれる
           DISPLAY WK-MEI2
                   AT LINE L COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           IF      M           <      10
                   ADD     1          TO      L
                   DISPLAY WK-HAI2
                           AT LINE L COL 1
                           WITH 
                           BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY
           ELSE
                   ADD     1          TO      L
                   DISPLAY WK-HAI3
                           AT LINE L COL 1
                           WITH 
                           BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-IF
           .
       S231-EX.
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

      *    DISPLAY WK-PGM-NAME " END"
      *    DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
      *    DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT
      *
           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
