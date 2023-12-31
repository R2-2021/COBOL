      *    *** YYYYMMDDから曜日計算 サブルーチン

       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         DATEWEEK.

       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.

       DATA                DIVISION.
       WORKING-STORAGE     SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DATEWEEK".

           03  WK-URUU         PIC  X(001) VALUE SPACE.
           03  WK-R-NISUU      BINARY-LONG SYNC VALUE ZERO.
           03  WK-NISUU2       BINARY-LONG SYNC VALUE ZERO.
           03  WK-SHOU         BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY         BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY-2       BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-400    BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-100    BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-4      BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI        BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** 0000年(1)にセット,9999年(10000)にセット
           03  TBL01-AREA      OCCURS 10000.
             05  TBL01-YYYY    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-R-NISUU BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-URUU    PIC  X(001) VALUE SPACE.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "N".

       LINKAGE                 SECTION.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==LDW==.

       PROCEDURE   DIVISION    USING   LDW-DATEWEEK-AREA
           .
       M100-10.

           IF      SW-FIRST    =       "N"
      *    *** TBL01 SET,累積日数セット
                   PERFORM S010-10     THRU    S010-EX
           END-IF

           EVALUATE TRUE

      *    *** LDW-DATE2-YMD から曜日 LDW-DATE2-WEEK 求める
               WHEN LDW-DATE2-ID = "A"
      *    *** LDW-DATE2-YMD 迄の日数セット
                   PERFORM S200-10     THRU    S200-EX

      *    *** 閏年の日数等、計算する
      *             PERFORM S100-10     THRU    S100-EX

      *    *** 曜日計算する
      *    *** 日曜：1、月曜：２、…、土曜：７
      *    *** LDW-DATE2-WEEK SET
                   PERFORM S210-10     THRU    S210-EX

      *    *** LDW-NISUU から LDW-DATE2-YMD 求める
               WHEN LDW-DATE2-ID = "R"
      *    *** LDW-DATE2-YMD 迄の日数セット
                   PERFORM S300-10     THRU    S300-EX

      *    *** 曜日計算する
      *    *** 日曜：1、月曜：２、…、土曜：７
      *    *** LDW-DATE2-WEEK SET
                   PERFORM S210-10     THRU    S210-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID ERROR" 
                          " LDW-DATE2-ID=" LDW-DATE2-ID
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** TBL01 SET,累積日数セット
       S010-10.
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 9999

                   MOVE    I           TO      WK-YYYY
      *    *** 閏年判定
                   PERFORM S011-10     THRU    S011-EX
                   ADD     I 1         GIVING  I2
                   MOVE    I           TO      TBL01-YYYY (I2)
                   MOVE    WK-URUU     TO      TBL01-URUU (I2)
                   IF      WK-URUU     =       "Y"
                           ADD     366         TO      WK-R-NISUU
                   ELSE
                           ADD     365         TO      WK-R-NISUU
                   END-IF
      *    *** その年の12月31日までの日数、0000年12月31日を366として、
                   MOVE    WK-R-NISUU    TO   TBL01-R-NISUU (I2)
           END-PERFORM
           MOVE    "Y"         TO      SW-FIRST
           .
       S010-EX.
           EXIT.

      *    *** 閏年判定
       S011-10.

      *    *** うるう年判定、４００年で割り切れる年は、２月は２９日
      *    *** うるう年判定、１００年で割り切れる年は、２月は２８日
      *    *** うるう年判定、　　４年で割り切れる年は、２月は２９日
           DIVIDE  WK-YYYY BY 400 GIVING WK-YYYY-2
                   REMAINDER WK-AMARI-400

           IF      WK-AMARI-400 =      ZERO
                   MOVE    "Y"         TO      WK-URUU
           ELSE
               DIVIDE  WK-YYYY BY 100 GIVING WK-YYYY-2
                       REMAINDER WK-AMARI-100

               IF      WK-AMARI-100 =      ZERO
                       MOVE    "N"         TO      WK-URUU
               ELSE
                   DIVIDE  WK-YYYY BY 4   GIVING WK-YYYY-2
                           REMAINDER WK-AMARI-4

                   IF      WK-AMARI-4 =    ZERO
                       MOVE    "Y"         TO      WK-URUU
                   ELSE
                       MOVE    "N"         TO      WK-URUU
                   END-IF
               END-IF
           END-IF
           .
       S011-EX.
           EXIT.

      *    *** 算出方法変更、S100-10 未使用
      *    *** 閏年の日数等、計算する
       S100-10.

      *    *** LW-NISUU の基準日 1582.1.1 => 0000.1.1 とする
      *    *** 1582年以前は、暦がグレゴリオ暦でないので、目安である
           MOVE    ZERO        TO      LDW-NISUU
      *     PERFORM VARYING I FROM 1582 BY 1
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > LDW-DATE2-YYYY

                   MOVE    I           TO      WK-YYYY
      *    *** 閏年判定
                   PERFORM S011-10     THRU    S011-EX

                   IF      I           =       LDW-DATE2-YYYY
                       PERFORM VARYING J FROM 1 BY 1
                              UNTIL J > LDW-DATE2-MM
                           IF      J           =       LDW-DATE2-MM
                               ADD     LDW-DATE2-DD TO      LDW-NISUU
                           ELSE
                               ADD     LDW-DATE2-DD2(J) TO  LDW-NISUU
                           END-IF
                       END-PERFORM
                   ELSE
                       IF      LDW-URUU         =       "Y"
                           ADD     366         TO      LDW-NISUU
                       ELSE
                           ADD     365         TO      LDW-NISUU
                       END-IF
                   END-IF
           END-PERFORM
           .

       S100-EX.
           EXIT.

      *    *** LDW-DATE2-YMD 迄の日数セット
       S200-10.

           IF      LDW-DATE2-YMD IS    NUMERIC
               AND LDW-DATE2-MM >=     01
               AND LDW-DATE2-MM <=     12
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = A"
                           " LDW-DATE2-YMD NOT NUMERIC ERROR OR"
                           " LDW-DATE2-MM < 01 OR > 12 ERROR"
                           " LDW-DATE2-YMD=" LDW-DATE2-YMD
                   STOP    RUN
           END-IF

           MOVE    LDW-DATE2-YYYY TO   I
      *    *** 前年までの累積日数セット
           IF      LDW-DATE2-YYYY =    ZERO
                   MOVE    ZERO        TO      LDW-NISUU
           ELSE
                   MOVE    TBL01-R-NISUU (I) TO  LDW-NISUU
           END-IF

           ADD     I 1         GIVING  I2

           IF      TBL01-URUU (I2) =   "Y"
                   MOVE    29          TO      LDW-DATE2-DD2 (2)
           ELSE
                   MOVE    28          TO      LDW-DATE2-DD2 (2)
           END-IF
           MOVE    TBL01-URUU (I2) TO  LDW-URUU

           IF      LDW-DATE2-DD >=     01
               AND LDW-DATE2-DD <=     LDW-DATE2-DD2 (LDW-DATE2-MM)
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = A"
                           " LDW-DATE2-DD < 01 OR > 28,29,30,31 ERROR"
                           " LDW-DATE2-YMD=" LDW-DATE2-YMD
                   STOP    RUN
           END-IF

      *    *** 当年の月日までの日数セット
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > LDW-DATE2-MM
                   IF      J           =       LDW-DATE2-MM
                           ADD     LDW-DATE2-DD TO      LDW-NISUU
                   ELSE
                           ADD     LDW-DATE2-DD2 (J) TO LDW-NISUU
                   END-IF
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** 1900.1.1 は日曜；1、曜日計算する
      *    *** 日曜：1、月曜：２、…、土曜：７
       S210-10.

           DIVIDE  LDW-NISUU BY 7 GIVING WK-SHOU
                   REMAINDER LDW-DATE2-WEEK

      *    *** 補正する 0000年1月1日＝＞土曜日（7）
      *    *** LDW-DATE2-WEEK => 1(日) + 6 => 7 土

           ADD      6          TO      LDW-DATE2-WEEK

           EVALUATE TRUE
               WHEN LDW-DATE2-WEEK = 8
                   MOVE     1           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 9
                   MOVE     2           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 10
                   MOVE     3           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 11
                   MOVE     4           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 12
                   MOVE     5           TO      LDW-DATE2-WEEK
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

      *    ACCEPT DAY-OF-WEEK に合わせる
           IF      LDW-DATE2-WEEK =    1
                   MOVE    7           TO      LDW-DATE2-WEEK
           ELSE
                   ADD     -1          TO      LDW-DATE2-WEEK
           END-IF
      *    *** DATETIME の WEEK 数字とは判断違う
           EVALUATE LDW-DATE2-WEEK
               WHEN 1
                   MOVE    "月"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "MON"       TO      LDW-DATE2-WEEK-NA
               WHEN 2
                   MOVE    "火"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "TUE"       TO      LDW-DATE2-WEEK-NA
               WHEN 3
                   MOVE    "水"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "WED"       TO      LDW-DATE2-WEEK-NA
               WHEN 4
                   MOVE    "木"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "THU"       TO      LDW-DATE2-WEEK-NA
               WHEN 5
                   MOVE    "金"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "FRI"       TO      LDW-DATE2-WEEK-NA
               WHEN 6
                   MOVE    "土"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "SAT"       TO      LDW-DATE2-WEEK-NA
               WHEN 7
                   MOVE    "日"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "SUN"       TO      LDW-DATE2-WEEK-NA
           END-EVALUATE
           .
       S210-EX.
           EXIT.

      *    *** LDW-NISUU から LDW-DATE2-YMD をセット
       S300-10.

           MOVE    ZERO        TO      WK-NISUU2
      *    *** 0000年=>(1)
      *    *** 9999年=>(10000)
           PERFORM TEST AFTER
                   VARYING I2 FROM 1 BY 1
                   UNTIL I2 > 10000
                    OR TBL01-R-NISUU (I2) >= LDW-NISUU
               IF      TBL01-R-NISUU (I2) >= LDW-NISUU
      *    *** I = 受け渡されたDW-NISUUの1年前の年数
                   ADD     I2 -1       GIVING  I
                   MOVE    TBL01-YYYY (I2) TO  LDW-DATE2-YYYY

                   IF      TBL01-URUU (I2) =   "Y"
                        MOVE    29          TO      LDW-DATE2-DD2 (2)
                   ELSE
                        MOVE    28          TO      LDW-DATE2-DD2 (2)
                   END-IF

      *    *** WK-NISSU2 は受け渡されたDW-NISUUより、当年1月１日からの日数
                   IF      I           =       ZERO
                       MOVE    LDW-NISUU   TO      WK-NISUU2
                   ELSE

      *    *** TBL01-R-NISUU (I) 1年前の累積日数を引き、当年の日数求める
                       COMPUTE WK-NISUU2 = LDW-NISUU - TBL01-R-NISUU (I)
                   END-IF

      *    *** WK-NISUU2、当年、当月の日数求める
                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL J > 12
                              OR WK-NISUU2 <= LDW-DATE2-DD2 (J)
                           COMPUTE WK-NISUU2 = WK-NISUU2 
                                             - LDW-DATE2-DD2 (J)
                   END-PERFORM

                   IF      J          >       12
                           DISPLAY WK-PGM-NAME " LDW-DATE2-ID = R"
                                   " LDW-NISUU OVER ERROR LDW-NISUU="
                                   LDW-NISUU " J=" J
                           STOP    RUN
                   END-IF

      *    *** 月、日求める
                   MOVE    J           TO      LDW-DATE2-MM
                   MOVE    WK-NISUU2   TO      LDW-DATE2-DD
               END-IF
           END-PERFORM

      *    *** 受け渡された日数が9999年より大きい時
      *    *** S011-10 で9999までセット
           IF      I2          >       10000
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = R"
                           " LDW-NISUU OVER ERROR LDW-NISUU=" LDW-NISUU
                           " I2=" I2
                   STOP    RUN
           END-IF
           .
       S300-EX.
           EXIT.
