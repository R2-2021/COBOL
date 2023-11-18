      *    *** YYYYMMDD‚©‚ç—j“úŒvŽZ ƒTƒuƒ‹[ƒ`ƒ“

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
      *    *** 0000”N(1)‚ÉƒZƒbƒg,9999”N(10000)‚ÉƒZƒbƒg
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
      *    *** TBL01 SET,—ÝÏ“ú”ƒZƒbƒg
                   PERFORM S010-10     THRU    S010-EX
           END-IF

           EVALUATE TRUE

      *    *** LDW-DATE2-YMD ‚©‚ç—j“ú LDW-DATE2-WEEK ‹‚ß‚é
               WHEN LDW-DATE2-ID = "A"
      *    *** LDW-DATE2-YMD –˜‚Ì“ú”ƒZƒbƒg
                   PERFORM S200-10     THRU    S200-EX

      *    *** ‰[”N‚Ì“ú”“™AŒvŽZ‚·‚é
      *             PERFORM S100-10     THRU    S100-EX

      *    *** —j“úŒvŽZ‚·‚é
      *    *** “ú—jF1AŒŽ—jF‚QAcA“y—jF‚V
      *    *** LDW-DATE2-WEEK SET
                   PERFORM S210-10     THRU    S210-EX

      *    *** LDW-NISUU ‚©‚ç LDW-DATE2-YMD ‹‚ß‚é
               WHEN LDW-DATE2-ID = "R"
      *    *** LDW-DATE2-YMD –˜‚Ì“ú”ƒZƒbƒg
                   PERFORM S300-10     THRU    S300-EX

      *    *** —j“úŒvŽZ‚·‚é
      *    *** “ú—jF1AŒŽ—jF‚QAcA“y—jF‚V
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

      *    *** TBL01 SET,—ÝÏ“ú”ƒZƒbƒg
       S010-10.
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 9999

                   MOVE    I           TO      WK-YYYY
      *    *** ‰[”N”»’è
                   PERFORM S011-10     THRU    S011-EX
                   ADD     I 1         GIVING  I2
                   MOVE    I           TO      TBL01-YYYY (I2)
                   MOVE    WK-URUU     TO      TBL01-URUU (I2)
                   IF      WK-URUU     =       "Y"
                           ADD     366         TO      WK-R-NISUU
                   ELSE
                           ADD     365         TO      WK-R-NISUU
                   END-IF
      *    *** ‚»‚Ì”N‚Ì12ŒŽ31“ú‚Ü‚Å‚Ì“ú”A0000”N12ŒŽ31“ú‚ð366‚Æ‚µ‚ÄA
                   MOVE    WK-R-NISUU    TO   TBL01-R-NISUU (I2)
           END-PERFORM
           MOVE    "Y"         TO      SW-FIRST
           .
       S010-EX.
           EXIT.

      *    *** ‰[”N”»’è
       S011-10.

      *    *** ‚¤‚é‚¤”N”»’èA‚S‚O‚O”N‚ÅŠ„‚èØ‚ê‚é”N‚ÍA‚QŒŽ‚Í‚Q‚X“ú
      *    *** ‚¤‚é‚¤”N”»’èA‚P‚O‚O”N‚ÅŠ„‚èØ‚ê‚é”N‚ÍA‚QŒŽ‚Í‚Q‚W“ú
      *    *** ‚¤‚é‚¤”N”»’èA@@‚S”N‚ÅŠ„‚èØ‚ê‚é”N‚ÍA‚QŒŽ‚Í‚Q‚X“ú
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

      *    *** ŽZo•û–@•ÏXAS100-10 –¢Žg—p
      *    *** ‰[”N‚Ì“ú”“™AŒvŽZ‚·‚é
       S100-10.

      *    *** LW-NISUU ‚ÌŠî€“ú 1582.1.1 => 0000.1.1 ‚Æ‚·‚é
      *    *** 1582”NˆÈ‘O‚ÍA—ï‚ªƒOƒŒƒSƒŠƒI—ï‚Å‚È‚¢‚Ì‚ÅA–ÚˆÀ‚Å‚ ‚é
           MOVE    ZERO        TO      LDW-NISUU
      *     PERFORM VARYING I FROM 1582 BY 1
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > LDW-DATE2-YYYY

                   MOVE    I           TO      WK-YYYY
      *    *** ‰[”N”»’è
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

      *    *** LDW-DATE2-YMD –˜‚Ì“ú”ƒZƒbƒg
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
      *    *** ‘O”N‚Ü‚Å‚Ì—ÝÏ“ú”ƒZƒbƒg
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

      *    *** “–”N‚ÌŒŽ“ú‚Ü‚Å‚Ì“ú”ƒZƒbƒg
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

      *    *** 1900.1.1 ‚Í“ú—jG1A—j“úŒvŽZ‚·‚é
      *    *** “ú—jF1AŒŽ—jF‚QAcA“y—jF‚V
       S210-10.

           DIVIDE  LDW-NISUU BY 7 GIVING WK-SHOU
                   REMAINDER LDW-DATE2-WEEK

      *    *** •â³‚·‚é 0000”N1ŒŽ1“ú„“y—j“úi7j
      *    *** LDW-DATE2-WEEK => 1(“ú) + 6 => 7 “y

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

      *    ACCEPT DAY-OF-WEEK ‚É‡‚í‚¹‚é
           IF      LDW-DATE2-WEEK =    1
                   MOVE    7           TO      LDW-DATE2-WEEK
           ELSE
                   ADD     -1          TO      LDW-DATE2-WEEK
           END-IF
      *    *** DATETIME ‚Ì WEEK ”Žš‚Æ‚Í”»’fˆá‚¤
           EVALUATE LDW-DATE2-WEEK
               WHEN 1
                   MOVE    "ŒŽ"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "MON"       TO      LDW-DATE2-WEEK-NA
               WHEN 2
                   MOVE    "‰Î"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "TUE"       TO      LDW-DATE2-WEEK-NA
               WHEN 3
                   MOVE    "…"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "WED"       TO      LDW-DATE2-WEEK-NA
               WHEN 4
                   MOVE    "–Ø"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "THU"       TO      LDW-DATE2-WEEK-NA
               WHEN 5
                   MOVE    "‹à"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "FRI"       TO      LDW-DATE2-WEEK-NA
               WHEN 6
                   MOVE    "“y"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "SAT"       TO      LDW-DATE2-WEEK-NA
               WHEN 7
                   MOVE    "“ú"        TO      LDW-DATE2-WEEK-NK
                   MOVE    "SUN"       TO      LDW-DATE2-WEEK-NA
           END-EVALUATE
           .
       S210-EX.
           EXIT.

      *    *** LDW-NISUU ‚©‚ç LDW-DATE2-YMD ‚ðƒZƒbƒg
       S300-10.

           MOVE    ZERO        TO      WK-NISUU2
      *    *** 0000”N=>(1)
      *    *** 9999”N=>(10000)
           PERFORM TEST AFTER
                   VARYING I2 FROM 1 BY 1
                   UNTIL I2 > 10000
                    OR TBL01-R-NISUU (I2) >= LDW-NISUU
               IF      TBL01-R-NISUU (I2) >= LDW-NISUU
      *    *** I = Žó‚¯“n‚³‚ê‚½DW-NISUU‚Ì1”N‘O‚Ì”N”
                   ADD     I2 -1       GIVING  I
                   MOVE    TBL01-YYYY (I2) TO  LDW-DATE2-YYYY

                   IF      TBL01-URUU (I2) =   "Y"
                        MOVE    29          TO      LDW-DATE2-DD2 (2)
                   ELSE
                        MOVE    28          TO      LDW-DATE2-DD2 (2)
                   END-IF

      *    *** WK-NISSU2 ‚ÍŽó‚¯“n‚³‚ê‚½DW-NISUU‚æ‚èA“–”N1ŒŽ‚P“ú‚©‚ç‚Ì“ú”
                   IF      I           =       ZERO
                       MOVE    LDW-NISUU   TO      WK-NISUU2
                   ELSE

      *    *** TBL01-R-NISUU (I) 1”N‘O‚Ì—ÝÏ“ú”‚ðˆø‚«A“–”N‚Ì“ú”‹‚ß‚é
                       COMPUTE WK-NISUU2 = LDW-NISUU - TBL01-R-NISUU (I)
                   END-IF

      *    *** WK-NISUU2A“–”NA“–ŒŽ‚Ì“ú”‹‚ß‚é
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

      *    *** ŒŽA“ú‹‚ß‚é
                   MOVE    J           TO      LDW-DATE2-MM
                   MOVE    WK-NISUU2   TO      LDW-DATE2-DD
               END-IF
           END-PERFORM

      *    *** Žó‚¯“n‚³‚ê‚½“ú”‚ª9999”N‚æ‚è‘å‚«‚¢Žž
      *    *** S011-10 ‚Å9999‚Ü‚ÅƒZƒbƒg
           IF      I2          >       10000
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = R"
                           " LDW-NISUU OVER ERROR LDW-NISUU=" LDW-NISUU
                           " I2=" I2
                   STOP    RUN
           END-IF
           .
       S300-EX.
           EXIT.
