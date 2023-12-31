      *    *** 処理時間、ＬＵＰ時間 計算 サブルーチン

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DATETIME.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DATETIME".

           03  WK-DATE-TIME-S.
             05  WK-DATE-YMD-S.
               07  WK-DATE-YY-S PIC  9(002) VALUE ZERO.
               07  WK-DATE-MM-S PIC  9(002) VALUE ZERO.
               07  WK-DATE-DD-S PIC  9(002) VALUE ZERO.
             05  WK-DATE-HMS-S.
               07  WK-DATE-HH-S PIC  9(002) VALUE ZERO.
               07  WK-DATE-MI-S PIC  9(002) VALUE ZERO.
               07  WK-DATE-SS-S PIC  9(002) VALUE ZERO.
               07  WK-DATE-SM-S PIC  9(002) VALUE ZERO.

           03  WK-DATE-TIME-S-R  REDEFINES WK-DATE-TIME-S.
             05  WK-DATE-YMD-SU-S OCCURS 14
                               PIC  9(001).

           03  WK-SEC-1        PIC  9(009)V99 VALUE ZERO.
           03  WK-SEC-2        PIC  9(009)V99 VALUE ZERO.

           03  WK-SEC-D        PIC  ZZZ,ZZZ,ZZ9.99 VALUE ZERO.

           03  WK-SEC          PIC  9(009)V99 VALUE ZERO.
           03  WK-SEC-R        REDEFINES WK-SEC.
             05  WK-SEC-SS     PIC  9(009).
             05  WK-SEC-SM     PIC  9(002).

           03  WK-DAY-S.
             05  WK-DAY-YYYY-S PIC  9(004) VALUE ZERO.
             05  WK-DAY-DDD-S  PIC  9(003) VALUE ZERO.

           03  WK-DAY-S-R      REDEFINES WK-DAY-S.
             05  WK-DAY-SU-S   OCCURS 7
                               PIC  9(001).
           03  WK-DAY-E.
             05  WK-DAY-YYYY-E PIC  9(004) VALUE ZERO.
             05  WK-DAY-DDD-E  PIC  9(003) VALUE ZERO.

           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-AMARI        PIC  9(004) VALUE ZERO.

           03  WK-DATE-TIME-S2.
             05  WK-DATE-YMD-S2.
               07  WK-DATE-YY-S2 PIC  9(002) VALUE ZERO.
               07  WK-DATE-MM-S2 PIC  9(002) VALUE ZERO.
               07  WK-DATE-DD-S2 PIC  9(002) VALUE ZERO.
             05  WK-DATE-HMS-S2.
               07  WK-DATE-HH-S2 PIC  9(002) VALUE ZERO.
               07  WK-DATE-MI-S2 PIC  9(002) VALUE ZERO.
               07  WK-DATE-SS-S2 PIC  9(002) VALUE ZERO.
               07  WK-DATE-SM-S2 PIC  9(002) VALUE ZERO.
           03  WK-SEC-21       PIC  9(009)V99 VALUE ZERO.
           03  WK-SEC-22       PIC  9(009)V99 VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA.
             05  FILLER        PIC  X(018) VALUE "January  February ".
             05  FILLER        PIC  X(018) VALUE "March    April    ".
             05  FILLER        PIC  X(018) VALUE "May      June     ".
             05  FILLER        PIC  X(018) VALUE "July     August   ".
             05  FILLER        PIC  X(018) VALUE "SeptemberOctober  ".
             05  FILLER        PIC  X(018) VALUE "November December ".
           03  TBL01-AREA-R    REDEFINES TBL01-AREA.
             05  TBL01-MM-NA   OCCURS 12
                               PIC X(009).
           03  TBL02-AREA.
             05  FILLER        PIC  N(009) VALUE NC"Ｊａｎｕａｒｙ　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｆｅｂｒｕａｒｙ　".
             05  FILLER        PIC  N(009) VALUE NC"Ｍａｒｃｈ　　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ａｐｒｉｌ　　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｍａｙ　　　　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｊｕｎｅ　　　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｊｕｌｙ　　　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ａｕｇｕｓｔ　　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｓｅｐｔｅｍｂｅｒ".
             05  FILLER        PIC  N(009) VALUE NC"Ｏｃｔｏｂｅｒ　　".
             05  FILLER        PIC  N(009) VALUE NC"Ｎｏｖｅｍｂｅｒ　".
             05  FILLER        PIC  N(009) VALUE NC"Ｄｅｃｅｍｂｅｒ　".
           03  TBL02-AREA-R    REDEFINES TBL02-AREA.
             05  TBL02-MM-NK1  OCCURS 12
                               PIC N(009).
           03  TBL03-AREA.
             05  FILLER        PIC  N(003) VALUE "　１月".
             05  FILLER        PIC  N(003) VALUE "　２月".
             05  FILLER        PIC  N(003) VALUE "　３月".
             05  FILLER        PIC  N(003) VALUE "　４月".
             05  FILLER        PIC  N(003) VALUE "　５月".
             05  FILLER        PIC  N(003) VALUE "　６月".
             05  FILLER        PIC  N(003) VALUE "　７月".
             05  FILLER        PIC  N(003) VALUE "　８月".
             05  FILLER        PIC  N(003) VALUE "　９月".
             05  FILLER        PIC  N(003) VALUE "１０月".
             05  FILLER        PIC  N(003) VALUE "１１月".
             05  FILLER        PIC  N(003) VALUE "１２月".
           03  TBL03-AREA-R    REDEFINES TBL03-AREA.
             05  TBL03-MM-NK2  OCCURS 12
                               PIC N(003).
           03  TBL04-AREA.
             05  FILLER        PIC  N(001) VALUE NC"０".
             05  FILLER        PIC  N(001) VALUE NC"１".
             05  FILLER        PIC  N(001) VALUE NC"２".
             05  FILLER        PIC  N(001) VALUE NC"３".
             05  FILLER        PIC  N(001) VALUE NC"４".
             05  FILLER        PIC  N(001) VALUE NC"５".
             05  FILLER        PIC  N(001) VALUE NC"６".
             05  FILLER        PIC  N(001) VALUE NC"７".
             05  FILLER        PIC  N(001) VALUE NC"８".
             05  FILLER        PIC  N(001) VALUE NC"９".
           03  TBL04-AREA-R    REDEFINES TBL04-AREA.
             05  TBL04-SU-NK   OCCURS 10
                               PIC N(001).

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.

       LINKAGE SECTION.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==LDT==.

       PROCEDURE   DIVISION    USING   LDT-DATETIME-AREA
           .
       M100-10.

           EVALUATE TRUE

      *    *** 開始処理
      *    *** LDT-DATE-TIME-ID=S,START
               WHEN LDT-DATE-TIME-ID =   "S" 
                   ACCEPT  LDT-DATE-YMD FROM   DATE
                   ACCEPT  LDT-DATE-HMS FROM   TIME
                   MOVE    LDT-DATE-TIME TO    WK-DATE-TIME-S
                   ACCEPT  LDT-DATE-WEEK FROM  DAY-OF-WEEK
                   ACCEPT  LDT-DATE-DAY FROM   DAY YYYYDDD
                   ACCEPT  WK-DAY-S    FROM    DAY YYYYDDD

                   EVALUATE LDT-DATE-WEEK
                       WHEN 1
                           MOVE    "月"        TO      LDT-DATE-WEEK-NK
                           MOVE    "MON"       TO      LDT-DATE-WEEK-NA
                       WHEN 2
                           MOVE    "火"        TO      LDT-DATE-WEEK-NK
                           MOVE    "TUE"       TO      LDT-DATE-WEEK-NA
                       WHEN 3
                           MOVE    "水"        TO      LDT-DATE-WEEK-NK
                           MOVE    "WED"       TO      LDT-DATE-WEEK-NA
                       WHEN 4
                           MOVE    "木"        TO      LDT-DATE-WEEK-NK
                           MOVE    "THU"       TO      LDT-DATE-WEEK-NA
                       WHEN 5
                           MOVE    "金"        TO      LDT-DATE-WEEK-NK
                           MOVE    "FRI"       TO      LDT-DATE-WEEK-NA
                       WHEN 6
                           MOVE    "土"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SAT"       TO      LDT-DATE-WEEK-NA
                       WHEN 7
                           MOVE    "日"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SUN"       TO      LDT-DATE-WEEK-NA
                   END-EVALUATE

                   MOVE     TBL01-MM-NA (LDT-DATE-MM) TO LDT-DATE-MM-NA
                   MOVE     TBL02-MM-NK1(LDT-DATE-MM) TO LDT-DATE-MM-NK1
                   MOVE     TBL03-MM-NK2(LDT-DATE-MM) TO LDT-DATE-MM-NK2

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 14
                       COMPUTE J = WK-DATE-YMD-SU-S (I) + 1
                       MOVE     TBL04-SU-NK(J) TO LDT-DATE-YMD-HMS-N(I)
                   END-PERFORM

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 7
                       COMPUTE J = WK-DAY-SU-S (I) + 1
                       MOVE    TBL04-SU-NK(J) TO LDT-DATE-DAY-YYDDD-N(I)
                   END-PERFORM

      *     DISPLAY LDT-DATE-MM-NA
      *     DISPLAY LDT-DATE-MM-NK1
      *     DISPLAY LDT-DATE-MM-NK2
      *     DISPLAY LDT-DATE-TIME-N
      *     DISPLAY LDT-DATE-DAY-N

      *    *** ラップ（経過）処理
      *    *** LDT-DATE-TIME-ID=L,LUP
               WHEN LDT-DATE-TIME-ID =  "L"
                   ACCEPT  LDT-DATE-YMD FROM   DATE
                   ACCEPT  LDT-DATE-HMS FROM   TIME
                   ACCEPT  WK-DAY-E     FROM   DAY YYYYDDD
                   ACCEPT  LDT-DATE-WEEK FROM  DAY-OF-WEEK
                   ACCEPT  LDT-DATE-DAY FROM   DAY YYYYDDD

                   EVALUATE LDT-DATE-WEEK
                       WHEN 1
                           MOVE    "月"        TO      LDT-DATE-WEEK-NK
                           MOVE    "MON"       TO      LDT-DATE-WEEK-NA
                       WHEN 2
                           MOVE    "火"        TO      LDT-DATE-WEEK-NK
                           MOVE    "TUE"       TO      LDT-DATE-WEEK-NA
                       WHEN 3
                           MOVE    "水"        TO      LDT-DATE-WEEK-NK
                           MOVE    "WED"       TO      LDT-DATE-WEEK-NA
                       WHEN 4
                           MOVE    "木"        TO      LDT-DATE-WEEK-NK
                           MOVE    "THU"       TO      LDT-DATE-WEEK-NA
                       WHEN 5
                           MOVE    "金"        TO      LDT-DATE-WEEK-NK
                           MOVE    "FRI"       TO      LDT-DATE-WEEK-NA
                       WHEN 6
                           MOVE    "土"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SAT"       TO      LDT-DATE-WEEK-NA
                       WHEN 7
                           MOVE    "日"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SUN"       TO      LDT-DATE-WEEK-NA
                   END-EVALUATE

                   MOVE     TBL01-MM-NA (LDT-DATE-MM) TO LDT-DATE-MM-NA
                   MOVE     TBL02-MM-NK1(LDT-DATE-MM) TO LDT-DATE-MM-NK1
                   MOVE     TBL03-MM-NK2(LDT-DATE-MM) TO LDT-DATE-MM-NK2

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 14
                       COMPUTE J = WK-DATE-YMD-SU-S (I) + 1
                       MOVE     TBL04-SU-NK(J) TO LDT-DATE-YMD-HMS-N(I)
                   END-PERFORM

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 7
                       COMPUTE J = WK-DAY-SU-S (I) + 1
                       MOVE    TBL04-SU-NK(J) TO LDT-DATE-DAY-YYDDD-N(I)
                   END-PERFORM

      *    *** ＬＵＰ時間 計算
                   PERFORM S200-10     THRU    S200-EX

                   DISPLAY LDT-DATE-TIME-PGM
                           " START=" WK-DATE-YY-S2
                           "/"      WK-DATE-MM-S2
                           "/"      WK-DATE-DD-S2
                           " "      WK-DATE-HH-S2
                           ":"      WK-DATE-MI-S2
                           ":"      WK-DATE-SS-S2
                           "."      WK-DATE-SM-S2
                           " END="  LDT-DATE-YY
                           "/"      LDT-DATE-MM
                           "/"      LDT-DATE-DD
                           " "      LDT-DATE-HH
                           ":"      LDT-DATE-MI
                           ":"      LDT-DATE-SS
                           "."      LDT-DATE-SM " "
      *    *** 曜日は終了日付の曜日表示
                           LDT-DATE-WEEK-NK "(" 
                           LDT-DATE-WEEK-NA ")"
                   DISPLAY LDT-DATE-TIME-PGM " " 
                           LDT-DATE-LUP-COM " LUP 処理時間"
                            WK-SEC-D "秒でした"
                   MOVE    LDT-DATE-TIME TO    WK-DATE-TIME-S2

      *    *** 終了処理
      *    *** LDT-DATE-TIME-ID=E,END
               WHEN LDT-DATE-TIME-ID =  "E" 
                   ACCEPT  LDT-DATE-YMD FROM   DATE
                   ACCEPT  LDT-DATE-HMS FROM   TIME
                   ACCEPT  LDT-DATE-WEEK FROM  DAY-OF-WEEK
                   ACCEPT  LDT-DATE-DAY  FROM  DAY YYYYDDD
                   ACCEPT  WK-DAY-E    FROM    DAY YYYYDDD

                   EVALUATE LDT-DATE-WEEK
                       WHEN 1
                           MOVE    "月"        TO      LDT-DATE-WEEK-NK
                           MOVE    "MON"       TO      LDT-DATE-WEEK-NA
                       WHEN 2
                           MOVE    "火"        TO      LDT-DATE-WEEK-NK
                           MOVE    "TUE"       TO      LDT-DATE-WEEK-NA
                       WHEN 3
                           MOVE    "水"        TO      LDT-DATE-WEEK-NK
                           MOVE    "WED"       TO      LDT-DATE-WEEK-NA
                       WHEN 4
                           MOVE    "木"        TO      LDT-DATE-WEEK-NK
                           MOVE    "THU"       TO      LDT-DATE-WEEK-NA
                       WHEN 5
                           MOVE    "金"        TO      LDT-DATE-WEEK-NK
                           MOVE    "FRI"       TO      LDT-DATE-WEEK-NA
                       WHEN 6
                           MOVE    "土"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SAT"       TO      LDT-DATE-WEEK-NA
                       WHEN 7
                           MOVE    "日"        TO      LDT-DATE-WEEK-NK
                           MOVE    "SUN"       TO      LDT-DATE-WEEK-NA
                   END-EVALUATE

                   MOVE     TBL01-MM-NA (LDT-DATE-MM) TO LDT-DATE-MM-NA
                   MOVE     TBL02-MM-NK1(LDT-DATE-MM) TO LDT-DATE-MM-NK1
                   MOVE     TBL03-MM-NK2(LDT-DATE-MM) TO LDT-DATE-MM-NK2

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 14
                       COMPUTE J = WK-DATE-YMD-SU-S (I) + 1
                       MOVE     TBL04-SU-NK(J) TO LDT-DATE-YMD-HMS-N(I)
                   END-PERFORM

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 7
                       COMPUTE J = WK-DAY-SU-S (I) + 1
                       MOVE    TBL04-SU-NK(J) TO LDT-DATE-DAY-YYDDD-N(I)
                   END-PERFORM

      *    *** 処理時間 計算
                   PERFORM S100-10     THRU    S100-EX

                   DISPLAY LDT-DATE-TIME-PGM
                           " START=" WK-DATE-YY-S
                           "/"      WK-DATE-MM-S
                           "/"      WK-DATE-DD-S
                           " "      WK-DATE-HH-S
                           ":"      WK-DATE-MI-S
                           ":"      WK-DATE-SS-S
                           "."      WK-DATE-SM-S
                           " END="  LDT-DATE-YY
                           "/"      LDT-DATE-MM
                           "/"      LDT-DATE-DD
                           " "      LDT-DATE-HH
                           ":"      LDT-DATE-MI
                           ":"      LDT-DATE-SS
                           "."      LDT-DATE-SM " "
                           LDT-DATE-WEEK-NK "(" 
                           LDT-DATE-WEEK-NA ")"
                   DISPLAY LDT-DATE-TIME-PGM 
                           " 処理時間" WK-SEC-D "秒でした"
      *    *** ERROR 処理
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDT-DATE-TIME-ID ERROR"
                           " LDT-DATE-TIME-ID=" LDT-DATE-TIME-ID 
                   STOP    RUN
           END-EVALUATE.
       M100-EX.
           EXIT     PROGRAM.

      *    *** 処理時間 計算
       S100-10.

           MOVE    WK-DATE-SM-S TO     WK-SEC-SM.
           COMPUTE WK-SEC-SS =   WK-DATE-SS-S
                             + ( WK-DATE-MI-S * 60 )
                             + ( WK-DATE-HH-S * 3600 ).
           MOVE    WK-SEC      TO      WK-SEC-1.

           MOVE    LDT-DATE-SM TO      WK-SEC-SM.
           COMPUTE WK-SEC-SS =   LDT-DATE-SS
                             + ( LDT-DATE-MI * 60 )
                             + ( LDT-DATE-HH * 3600 ).
           MOVE    WK-SEC      TO      WK-SEC-2.

      *    *** YYYY と DDD < は無いが、＝と同じにする
           IF      WK-DAY-YYYY-E <=     WK-DAY-YYYY-S
               IF      WK-DAY-DDD-E <=      WK-DAY-DDD-S
                       COMPUTE WK-SEC-D = WK-SEC-2 - WK-SEC-1
               ELSE
                       COMPUTE WK-SEC-D = WK-SEC-2 - WK-SEC-1 
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E - WK-DAY-DDD-S ))
               END-IF
           ELSE

      *    *** WK-DAY-YYYY-E - WK-DAY-YYYY-S は１年のみとする
      *    *** YYYY が違う時、ＤＤＤも違うので、うるう年判定する
      *    *** うるう年判定、１００年、４００年の判定は考慮しない
               DIVIDE  WK-DAY-YYYY-S BY 4 GIVING WK-YYYY
                       REMAINDER WK-AMARI
               IF      WK-AMARI   =    ZERO
                       COMPUTE WK-SEC-D = WK-SEC-2 - WK-SEC-1
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E + (366 - WK-DAY-DDD-S)))
               ELSE
                       COMPUTE WK-SEC-D = WK-SEC-2 - WK-SEC-1 
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E + (365 - WK-DAY-DDD-S)))
               END-IF
           END-IF.
       S100-EX.
           EXIT.

      *    *** ＬＵＰ時間 計算
       S200-10.
           IF      WK-DATE-TIME-S2 =      ZERO
                   MOVE    WK-DATE-TIME-S TO      WK-DATE-TIME-S2
           END-IF.

           MOVE    WK-DATE-SM-S2 TO    WK-SEC-SM.
           COMPUTE WK-SEC-SS =   WK-DATE-SS-S2
                             + ( WK-DATE-MI-S2 * 60 )
                             + ( WK-DATE-HH-S2 * 3600 ).
           MOVE    WK-SEC      TO      WK-SEC-1.

           MOVE    LDT-DATE-SM TO      WK-SEC-SM.
           COMPUTE WK-SEC-SS =   LDT-DATE-SS
                             + ( LDT-DATE-MI * 60 )
                             + ( LDT-DATE-HH * 3600 ).
           MOVE    WK-SEC      TO      WK-SEC-2.

      *    *** YYYY と DDD < は無いが、＝と同じにする
           IF      WK-DAY-YYYY-E <=     WK-DAY-YYYY-S
               IF      WK-DAY-DDD-E <=      WK-DAY-DDD-S
                       COMPUTE WK-SEC-D 
                               LDT-DATE-SSMM = WK-SEC-2 - WK-SEC-1
               ELSE
                       COMPUTE WK-SEC-D
                               LDT-DATE-SSMM = WK-SEC-2 - WK-SEC-1 
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E - WK-DAY-DDD-S ))
               END-IF
           ELSE
      *    *** WK-DAY-YYYY-E - WK-DAY-YYYY-S は１年のみとする
      *    *** YYYY が違う時、ＤＤＤも違うので、うるう年判定する
      *    *** うるう年判定、１００年、４００年の判定は考慮しない
               DIVIDE  WK-DAY-YYYY-S BY 4 GIVING WK-YYYY
                       REMAINDER WK-AMARI
               IF      WK-AMARI   =    ZERO
                       COMPUTE WK-SEC-D
                               LDT-DATE-SSMM = WK-SEC-2 - WK-SEC-1
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E + (366 - WK-DAY-DDD-S)))
               ELSE
                       COMPUTE WK-SEC-D
                               LDT-DATE-SSMM = WK-SEC-2 - WK-SEC-1 
                              + ( 24 * 3600 
                              * ( WK-DAY-DDD-E + (365 - WK-DAY-DDD-S)))
               END-IF
           END-IF.
       S200-EX.
           EXIT.
