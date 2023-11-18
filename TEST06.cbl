      *    *** DISPLAY TEST
      *    LINE ãLì¸ÇµÇ»Ç¢Ç∆ÅAÉAÉ{Å[ÉgÇ∑ÇÈ
      *    SCROLL UP,DOWN ìÆçÏÇµÇ»Ç¢
      *    SCREEN SECTION Ç≈íËã`ÇµÇΩÅ@LINE,COLÇÕìÆçÏÇµÇ»Ç¢Ç≈ÇÕÇ»Ç≠ÅA
      *    äÓñ{çÄñ⁄Ç≈éwíËÇµÇ»Ç¢Ç∆ÅAìÆçÏÇµÇ»Ç¢

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST06.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       SPECIAL-NAMES.
           CURSOR         WK-CURSOL
           CRT STATUS     WK-CRT-STATUS
      *    SCREEN CONTROL WK-SCREEN-CONTROL
      *     EVENT STATUS   WK-EVENT-STATUS.

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
           03  PIN1-DATA       PIC  X(100).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DEG        PIC  ---9.
           03                  PIC  X(001).
           03  POT1-SIN        PIC  ---9.999.
           03                  PIC  X(001).
           03  POT1-COS        PIC  ---9.999.
           03                  PIC  X(001).
           03  POT1-TAN        PIC  ---9.999.
           03                  PIC  X(001).
           03  POT1-ASIN       PIC  ---9.999.
           03                  PIC  X(001).
           03  POT1-I          PIC  ZZ,ZZ9.
           03                  PIC  X(001).
           03  POT1-NUM        PIC  -ZZZ,ZZZ,ZZ9.
           03                  PIC  X(001).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST06".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST06.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST06.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SHORI        PIC  X(002) VALUE "00".
           03  WK-DATA1        PIC  X(005) VALUE "ABCDE".
           03  WK-DATA2        PIC S9(009) VALUE 5678.
           03  WK-DATA3        PIC S9(009) VALUE 9012.
           03  WK-PAR3         PIC S9(005) VALUE ZERO.
           03  WK-PAR4         PIC S9(005) VALUE ZERO.
           03  WK-BIT          PIC  X(001) VALUE SPACE.
           03  WK-NAME-XX      PIC  X(008) VALUE "ÇlÇlÇlÇl".
           03  WK-NAME-XN      PIC  N(004) VALUE "ÇmÇmÇmÇm".
           03  WK-NAME-NX      PIC  X(008) VALUE "ÇwÇwÇwÇw".
           03  WK-NAME-NN      PIC  N(004) VALUE "ÇxÇxÇxÇx".
           03  WK-NAME         PIC  N(005) VALUE "êŒå¥Å@âƒêD".
           03  WK-NAME01       PIC  N(005) VALUE "ÇPêŒå¥âƒêD".
           03  WK-NAME02       PIC  N(005) VALUE "ÇQêŒå¥âƒêD".
           03  WK-NAME03       PIC  N(005) VALUE "ÇRêŒå¥âƒêD".
           03  WK-NAME04       PIC  N(005) VALUE "ÇSêŒå¥âƒêD".
           03  WK-NAME05       PIC  N(005) VALUE "ÇTêŒå¥âƒêD".
           03  WK-NAME2        PIC  N(010) VALUE SPACE.
           03  WK-SPACE-N. 
             05  FILLER        PIC  N(040) VALUE SPACE.
           03  WK-L1.
             05  FILLER        PIC  X(001) VALUE "+".
             05  FILLER        PIC  X(078) VALUE ALL "-". 
             05  FILLER        PIC  X(001) VALUE "+".
      *    *** 0.5 SEC = 500,000,000 NANO SEC
           03  WK-NANOSEC      PIC  9(011) VALUE 500000000.
           03  WK-NANOSEC1     PIC  9(011) VALUE 100000000.
           03  WK-NANOSEC01    PIC  9(011) VALUE 10000000.
           03  WK-PI           PIC  9V9(5) VALUE 0.
           03  WK-DEG          PIC  9(003) VALUE 0.
           03  WK-RAD          PIC  9V9(3) VALUE 0.
           03  WK-SIN          PIC  S9V9(3) VALUE ZERO.
           03  WK-ASIN         PIC  S999V9(3) VALUE ZERO.
           03  WK-TAN          PIC  S999V9(3) VALUE ZERO.
           03  WK-C            PIC  9(002) VALUE ZERO.
           03  WK-NUM          PIC  9(009) VALUE ZERO.
           03  WK-ZERO         PIC  9(002) VALUE ZERO.
           03  WK-CURSOL       PIC  9(004) VALUE ZERO.
           03  WK-CRT-STATUS   PIC  9(004) VALUE ZERO.
           03  WK-SCREEN-CONTROL PIC  9(004) VALUE ZERO.
           03  WK-EVENT-STATUS PIC  9(004) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  KEI-AREA.
           03  KEI-CMD         PIC  9(002) VALUE 5.
           03  KEI-LINE        PIC  9(002) VALUE 19.
           03  KEI-COL         PIC  9(002) VALUE 5.
           03  KEI-LEN1        PIC  9(002) VALUE 12.
           03  KEI-LEN2        PIC  9(002) VALUE 3.
           03  KEI-COLOR       PIC  9(002) VALUE ZERO.
           03  KEI-PRN         PIC  9(002) VALUE ZERO.

       01  Horizontal-Line     PIC  X(080).

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C1              BINARY-LONG SYNC VALUE ZERO.
           03  C2              BINARY-LONG SYNC VALUE ZERO.
           03  C3              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  H               BINARY-LONG SYNC VALUE 1.
           03  V               BINARY-LONG SYNC VALUE 2.
           03  L               BINARY-LONG SYNC VALUE ZERO..

       01  COLOR-AREA.
           03  BK              BINARY-LONG SYNC VALUE 0.
           03  BL              BINARY-LONG SYNC VALUE 1.
           03  GR              BINARY-LONG SYNC VALUE 2.
           03  CY              BINARY-LONG SYNC VALUE 3.
           03  RE              BINARY-LONG SYNC VALUE 4.
           03  MA              BINARY-LONG SYNC VALUE 5.
           03  YE              BINARY-LONG SYNC VALUE 6.
           03  WH              BINARY-LONG SYNC VALUE 7.

       SCREEN                  SECTION.

       78  LD-UL-Corner        VALUE '+'.
       78  LD-LL-Corner        VALUE '+'.
       78  LD-UR-Corner        VALUE '+'.
       78  LD-LR-Corner        VALUE '+'.
       78  LD-Upper-T          VALUE '+'.
       78  LD-Lower-T          VALUE '+'.
       78  LD-Horiz-Line       VALUE '-'.
       78  LD-Vert-Line        VALUE '|'.

      *    *** èWícçÄñ⁄ÅAäÓñ{çÄñ⁄óºï˚Ç… BACKGROUND-COLOR,FOREGROUND-COLOR
      *    *** éwíËÇµÇΩéûÅAäÓñ{çÄñ⁄ÇÃéwíËÇ™óLå¯Ç…Ç»ÇÈ
      *    *** èWícçÄñ⁄ÇµÇ©éwíËÇ™ñ≥Ç¢éûÅAèWícçÄñ⁄ÇÃéwíËÇ™óLå¯Ç…Ç»ÇÈ
       01  SCR-I1 LINE 11 COL 1
             BACKGROUND-COLOR BL FOREGROUND-COLOR CY.

      *    *** BACKGROUND-COLOR GR FOREGROUND-COLOR WH Ç…Ç»ÇÈ
           03  SCR-I1-1        PIC  X(005) VALUE "ABC"
               LINE 12 COL 1   BACKGROUND-COLOR GR FOREGROUND-COLOR WH.

      *    *** BACKGROUND-COLOR BL FOREGROUND-COLOR RE Ç…Ç»ÇÈ
           03  SCR-I1-2        PIC  X(005) VALUE "DEF"
               LINE 12 COL 6   FOREGROUND-COLOR RE.

      *    *** BACKGROUND-COLOR MA FOREGROUND-COLOR WH Ç…Ç»ÇÈ
           03  SCR-I1-3        PIC  X(005) VALUE "GHI"
               LINE 12 COL 10  BACKGROUND-COLOR MA FOREGROUND-COLOR WH.

      *    *** BACKGROUND-COLOR BL FOREGROUND-COLOR WH Ç…Ç»ÇÈ
           03  SCR-I1-4        PIC  X(005) VALUE "JKL"
               LINE 12 COL 15  FOREGROUND-COLOR WH.

      *    *** ERROR âπ
       01  SCR-I2 BEEP.

      *    *** COLUMN 1 ï\é¶èoóàÇ»Ç¢ÅAâ∫ê¸ï\é¶
      * 01  SCR-I3 LINE 11 COLUMN 2 UNDERLINE.
      *    *** LINE,COL ÇÕäÓñ{çÄñ⁄Ç≈éwíËÇµÇ»Ç¢Ç∆ÅAìÆçÏÇµÇ»Ç¢Å@èWícçÄñ⁄ÇÕÉ_ÉÅ
      *    ***  
      * 01  SCR-I3 UNDERLINE AUTO.
       01  SCR-I3 UNDERLINE.
           03  LINE 11 COLUMN 2 PIC X(40) VALUE "ABCDE".
      *    *** PIC N(010) éÛéÊë§Ç‡N(010)Ç…Ç∑ÇÍÇŒÅAäøéöì¸óÕèoóàÇÈÇ™ÅA
      *    *** ìÒâÒñ⁄Ç≈ÇµÇ©ì¸óÕÇ≥ÇÍÇ»Ç¢ÅAÉoÉOóLÅH
      *    *** SECURE ÇÕì¸óÕéûÇÃÇ›ÅAÅñÇÃÉ}ÉXÉNÇ™Ç©Ç©ÇÈ
           03  SCR-I3-1 LINE 14 COL 02 USING WK-NAME-XX PIC X(08) AUTO
               SECURE.
           03  SCR-I3-2 LINE 14 COL 12 USING WK-NAME-XN PIC X(08) AUTO.
           03  SCR-I3-3 LINE 14 COL 22 USING WK-NAME-NX PIC N(04) AUTO.
           03  SCR-I3-4 LINE 14 COL 32 USING WK-NAME-NN PIC N(04) AUTO.

      *    *** OVERLINEÇÕ COL 1 ÇÕï\é¶èoóàÇ»Ç¢
       01  SCR-I4 OVERLINE.
           03   LINE 16 COL 1 PIC X(40) VALUE "FGHIJ".

      *    *** X=>X,X=>N,N=>X,N=>N Ç¢Ç∏ÇÍÇ‡èoóÕÇ≥ÇÍÇÈ
      *    *** PIC Ç≈éwíËÇµÇΩåÖêîÇ…Ç»ÇÈ
           03           COL PLUS 05 PIC X(08) FROM WK-NAME-XX.
           03           COL PLUS 05 PIC X(06) FROM WK-NAME-XN.
           03           COL PLUS 05 PIC N(04) FROM WK-NAME-NX.
      *    *** SECURE ÇÕèoóÕéûÇÕÅAÅñÇÃÉ}ÉXÉNÇ™Ç©Ç©ÇÁÇ»Ç¢
           03           COL PLUS 05 PIC N(03) FROM WK-NAME-NN
                        SECURE.

      *    *** SECURE
      *    *** SECURE ÇÕå„Ç©ÇÁÅAèoóÕÇÃèÍèäÇ…éwíËÇµÇƒÇ‡ÅAÅñÇÃÉ}ÉXÉNÇÕÇ©Ç©ÇÁÇ»Ç¢
       01  SCR-I5 LINE 16 COLUMN 10 PIC N(008) SECURE.

      *    *** 16,10 X(011)
       01  SCR-I6 LINE 16 COLUMN 1 PIC X(005).

      *    *** LINE 1 è¡ãé 14çsñ⁄è¡Ç¶ÇΩ
      *    *** 
       01  SCR-I7 LINE 14 COLUMN 1 ERASE EOL.

      *    *** 1,1 X(080) BACKGROUND-COLOR CY FOREGROUND-COLOR GR
       01  SCR-I8 LINE 1 COLUMN 1
           BACKGROUND-COLOR CY
           FOREGROUND-COLOR GR.
           03  FILLER          PIC  X(080).

      *    *** 1,2 X(080) "ABCÇ†Ç¢Ç§Ç¶Ç®"
      *    *** èWícçÄñ⁄Ç…ÇµÇ©ÅALINE,COL ãLèqñ≥Ç¢éûÅAÇPÇ¬ëOÇÃDISPLAY,ACCEPT
      *    *** ÇÃà íuÇ©ÇÁèoóÕÇ≥ÇÍÇÈ
      * 01  SCR-I9 LINE 1 COLUMN 2.
       01  SCR-I9.
           03  LINE 2 COL 1 PIC  X(080) VALUE "ABCÇ†Ç¢Ç§Ç¶Ç®".

      *    *** âÊñ 
      *    *** è¡ãé

      *    *** 1,1 BLANK SCREEN BACKGROUND-COLOR BK FOREGROUND-COLOR WH
       01  SCR-I10 LINE 1 COLUMN 1 BLANK SCREEN
      *    *** BLANK SCREEN ÇÃêFÅABACKGROUND-COLOR Ç≈éwíËÇµÇΩêFÇ…Ç»ÇÈ
      *    *** CALL årê¸ÇÃêFÇ‡Ç±Ç±Ç≈éwíËÇµÇΩ FOREGROUND-COLOR ÇÃêFÇ…Ç»ÇÁÇ»Ç¢
      *    *** â∫ê¸ïîï™ÇÃÇ›ÅAêFÇ™ïœÇÌÇÈ
      *       BACKGROUND-COLOR WH FOREGROUND-COLOR BK.
             BACKGROUND-COLOR BK FOREGROUND-COLOR CY.

      *    *** èWícçÄñ⁄ÅAäÓñ{çÄñ⁄óºï˚Ç…LINE,COL éwíËÇµÇΩéûÅAäÓñ{çÄñ⁄Ç™óDêÊ
       01  SCR-I11 LINE 1 COLUMN 1
              BACKGROUND-COLOR BK FOREGROUND-COLOR WH.
           05  SCR-I11-I5 LINE 24 COL 20 PIC N(010) FROM WK-NAME.

      *    ÇuÇ`ÇkÇtÇdÇÕäøéöÇ≈èoÇÈ
      *      05  LINE 24 COL 20 VALUE "ÉnÉbÉJÉhÅ[Éã". 
       01  SCR-I12 LINE 1 COLUMN 1 BLANK SCREEN.

       01  SCR-I13 LINE 1 COLUMN 1 BLANK SCREEN.

      * 01  Blank-Screen LINE 1 COLUMN 1 BLANK SCREEN. 
       01  SCR-I14 LINE 1 COLUMN 1 BLANK SCREEN. 

      * 01  Sw-Screen BACKGROUND-COLOR CY
       01  SCR-I15 BACKGROUND-COLOR CY
              FOREGROUND-COLOR BL AUTO.

           03  BACKGROUND-COLOR BK
               FOREGROUND-COLOR WH
               HIGHLIGHT.
             05  LINE 02 COL 02 VALUE LD-UL-Corner.
             05                 PIC  X(077) FROM Horizontal-Line.
             05          COL 80 VALUE LD-UR-Corner.

             05  LINE 03 COL 02 VALUE LD-Vert-Line.
             05          COL 80 VALUE LD-Vert-Line.

             05  LINE 04 COL 02 VALUE LD-Vert-Line.
             05          COL 80 VALUE LD-Vert-Line.

             05  LINE 05 COL 02 VALUE LD-Vert-Line.
             05          COL 80 VALUE LD-Vert-Line.

             05  LINE 06 COL 02 VALUE LD-LL-Corner.
             05                 PIC  X(077) FROM Horizontal-Line.
             05          COL 80 VALUE LD-LR-Corner.

           03  BACKGROUND-COLOR BK
               FOREGROUND-COLOR WH
               HIGHLIGHT.
             05  LINE 09 COL 04 VALUE 'F1'.
             05  LINE 10 COL 04 VALUE 'F2'.

       01  SCR-I16.

           03  HIGHLIGHT
               BACKGROUND-COLOR BK
               FOREGROUND-COLOR C1
               .
             05  LINE 01 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".

           03  HIGHLIGHT
               BACKGROUND-COLOR BK
               FOREGROUND-COLOR C1
               .
             05  LINE 08 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".
             05  LINE PLUS 1 COL C VALUE "Å°".

       01  SCR-I17.

           03  HIGHLIGHT
               BACKGROUND-COLOR BK
               FOREGROUND-COLOR C1
               .
             05  LINE 06 COL 23 VALUE "Å°".
             05  LINE 07 COL 23 VALUE "Å°".
             05  LINE 08 COL 23 VALUE "Å°".

           03  HIGHLIGHT
               BACKGROUND-COLOR BK
               FOREGROUND-COLOR C2
               .
             05  LINE 06 COL 25 VALUE "Åú".
             05  LINE 07 COL 25 VALUE "Åú".
             05  LINE 08 COL 25 VALUE "Åú".

       PROCEDURE               DIVISION.

       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** óºï˚éwíËÇ∑ÇÈÇ∆ÅAESC(2005),PGUP(2001),PGDN(2002) égópâ¬Ç…Ç»ÇÈ
      *    *** TAB(2007)
      *    *** PF01(1001)ÇÕéwíËÇµÇ»Ç≠ÇƒÇ‡ÅAéwíËâ¬
      *    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
      *    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.

      *
           DISPLAY "èàóùî‘çÜÅ@êîéö2åÖì¸óÕ"         LINE 08 COL 10
           DISPLAY " 00.âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇP"    LINE 10 COL 10
           DISPLAY " 01.âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇQ"    LINE 11 COL 10
           DISPLAY " 02.êŒå¥âƒêD"                  LINE 12 COL 10
           DISPLAY " 03.COLOR TEST"                LINE 13 COL 10
           DISPLAY " 04.årê¸ÉeÉXÉg"                LINE 14 COL 10
           DISPLAY " 05.Å°,ÅZ"                     LINE 15 COL 10
           DISPLAY " 06.SIN "                      LINE 16 COL 10
           DISPLAY " 07.TAN "                      LINE 17 COL 10
           DISPLAY " 08.ASIN "                     LINE 18 COL 10
           DISPLAY " 09.X**2 â°"                   LINE 19 COL 10

           DISPLAY " 10.X**2 èc"                   LINE 10 COL 40
           DISPLAY " 11.ècñ_      "                LINE 11 COL 40
           DISPLAY " 12.ñ_ÉOÉâÉt"                  LINE 12 COL 40
           DISPLAY " 13.          "                LINE 13 COL 40
           DISPLAY " 14.          "                LINE 14 COL 40
           DISPLAY " 15.          "                LINE 15 COL 40
           DISPLAY " 16.          "                LINE 16 COL 40
           DISPLAY " 17.          "                LINE 17 COL 40
           DISPLAY " 18.          "                LINE 18 COL 40
           DISPLAY " 19.          "                LINE 19 COL 40

           ACCEPT WK-SHORI LINE 25 COL 1

      *    *** 1,1 BLANK SCREEN BACKGROUND-COLOR BK FOREGROUND-COLOR WH
           DISPLAY SCR-I10


           EVALUATE TRUE

      *    *** âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇP
               WHEN WK-SHORI   =       "00"
                   PERFORM S100-10     THRU    S100-EX

      *    *** âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇQ
               WHEN WK-SHORI   =       "01"
                   PERFORM S110-10     THRU    S110-EX

      *    *** êŒå¥âƒêD
               WHEN WK-SHORI   =       "02"
                   PERFORM S120-10     THRU    S120-EX

      *    *** COLOR TEST
               WHEN WK-SHORI   =       "03"
                   PERFORM S130-10     THRU    S130-EX

      *    *** årê¸ÉeÉXÉg
               WHEN WK-SHORI   =       "04"
                   PERFORM S140-10     THRU    S140-EX

      *    *** Å°,ÅZ
               WHEN WK-SHORI   =       "05"
                   PERFORM S150-10     THRU    S150-EX

      *    *** SIN
               WHEN WK-SHORI   =       "06"
                   PERFORM S160-10     THRU    S160-EX

      *    *** TAN
               WHEN WK-SHORI   =       "07"
                   PERFORM S170-10     THRU    S170-EX

      *    *** ASIN
               WHEN WK-SHORI   =       "08"
                   PERFORM S180-10     THRU    S180-EX

      *    *** X**2 â°
               WHEN WK-SHORI   =       "09"
                   PERFORM S190-10     THRU    S190-EX

      *    *** X**2 èc
               WHEN WK-SHORI   =       "10"
                   PERFORM S200-10     THRU    S200-EX

      *    *** ècñ_
               WHEN WK-SHORI   =       "11"
                   PERFORM S210-10     THRU    S210-EX

      *    *** ñ_ÉOÉâÉt
               WHEN WK-SHORI   =       "12"
                   PERFORM S220-10     THRU    S220-EX

           END-EVALUATE

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

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇP
       S100-10.

           DISPLAY SCR-I1

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I2
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I3
           ACCEPT  SCR-I3-1
           ACCEPT  SCR-I3-2
           ACCEPT  SCR-I3-3
           ACCEPT  SCR-I3-4
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I4 LINE 10 COL 5
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I5
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I6
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I7
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I8
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I9
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I10
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I11
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I12
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I13
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I14
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY SCR-I15
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY WK-DATA1 LINE 1 COL 1
           DISPLAY WK-DATA2 LINE 1 COL 10
           DISPLAY WK-DATA3 LINE 1 COL 20
           DISPLAY WK-CURSOL     LINE 24 COL 1
           DISPLAY WK-CRT-STATUS LINE 24 COL 10

           ACCEPT  WK-BIT LINE 25 COL 1 
           .
       S100-EX.
           EXIT.

      *    *** âÊñ ì¸óÕÅ@çÄñ⁄ÉeÉXÉgÇQ
       S110-10.

      *     DISPLAY BLANK-SCREEN
           DISPLAY SCR-I14

           DISPLAY WK-DATA1 LINE 1 COL 2
           DISPLAY WK-DATA2 LINE 1 COL 10
           DISPLAY WK-DATA3 LINE 1 COL 20
           DISPLAY "ÇPçsñ⁄Å@WK-DATA1,2,3 èoóÕ"
                   LINE 25 COL 01 WITH
                   BACKGROUND-COLOR BK FOREGROUND-COLOR WH



           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY WK-DATA1 AT LINE 2 COL 2 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR BL
                   UNDERLINE

           DISPLAY WK-DATA2 LINE 2 COL 10 WITH
                   BACKGROUND-COLOR RE FOREGROUND-COLOR BL
                   OVERLINE

           DISPLAY WK-DATA3 LINE 2 COL 20 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR WH
                   OVERLINE
           DISPLAY "ÇQçsñ⁄Å@BACKGROUND-COLOR§FOREGROUND-COLOR " &
                   "ïœÇ¶Çƒ èoóÕ"
                   LINE 25 COL 01 WITH
                   BACKGROUND-COLOR BK FOREGROUND-COLOR WH



           ACCEPT  WK-BIT LINE 25 COL 1
           DISPLAY WK-DATA1 LINE 3 COL 2 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR BL
                   HIGHLIGHT
                   OVERLINE

           DISPLAY WK-DATA2 LINE 3 COL 10 WITH
                   BACKGROUND-COLOR RE FOREGROUND-COLOR BL
                   UNDERLINE

           DISPLAY WK-DATA3 LINE 3 COL 20 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR WH
                   UNDERLINE
           DISPLAY "ÇRçsñ⁄Å@HIGHLIGHT§OVERLINE§UNDERLINE ïœÇ¶Çƒ èoóÕ"
                   LINE 25 COL 01 WITH
                   BACKGROUND-COLOR BK FOREGROUND-COLOR WH

           ACCEPT  WK-BIT LINE 25 COL 1


           DISPLAY WK-DATA1 LINE 4 COL 2 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR BL
                   HIGHLIGHT
                   OVERLINE

           DISPLAY WK-DATA2 LINE 4 COL 10 WITH
                   BACKGROUND-COLOR RE FOREGROUND-COLOR BL
                   HIGHLIGHT
                   UNDERLINE

           DISPLAY WK-DATA3 LINE 4 COL 20 WITH
                   BACKGROUND-COLOR CY FOREGROUND-COLOR WH
                   HIGHLIGHT
                   UNDERLINE
      *            SCROLL DOWN BY 1
           DISPLAY "ÇSçsñ⁄Å@HIGHLIGHT§OVERLINE§UNDERLINE ïœÇ¶Çƒ èoóÕ"
                   LINE 25 COL 01 WITH
                   BACKGROUND-COLOR BK FOREGROUND-COLOR WH

           ACCEPT  WK-BIT LINE 25 COL 1



           DISPLAY "êŒå¥âƒêD"  LINE 5 COL 20 WITH
                   HIGHLIGHT
                   UNDERLINE
           DISPLAY "ÇTçsñ⁄Å@èÌêîÇ…HIGHLIGHT§OVERLINE§UNDERLINE"
                   " ïœÇ¶Çƒ èoóÕ"
                   LINE 25 COL 01 WITH
                   BACKGROUND-COLOR BK FOREGROUND-COLOR WH

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S110-EX.
           EXIT.

       S120-10.

           MOVE    1          TO      L
      *     DISPLAY SCR-I10
      *    *** LINE 1,COL 1 ÇÃÅ{ÇÕï\é¶Ç≥ÇÍÇ»Ç¢
           DISPLAY WK-L1 LINE 1 COL 1 WITH
      *            BACKGROUND-COLOR BK
      *            FOREGROUND-COLOR WH
                   BACKGROUND-COLOR WH
                   FOREGROUND-COLOR BK
                   LOWLIGHT

           PERFORM VARYING I  FROM 1 BY 1
                   UNTIL   I   >     7
                   ADD     I 1      GIVING L 
                   DISPLAY "ÇPêŒå¥âƒêD" LINE L COL 1 WITH
                   BACKGROUND-COLOR WH
                   FOREGROUND-COLOR I
                   HIGHLIGHT UNDERLINE
      *              DISPLAY WK-SPACE WITH SCROLL DOWN 1 
           END-PERFORM

           PERFORM VARYING I  FROM 1 BY 1
                   UNTIL   I   >     7
                   ADD     I 1      GIVING L 
                   DISPLAY "ÇQêŒå¥âƒêD" LINE L COL 11 WITH
                   BACKGROUND-COLOR I
                   FOREGROUND-COLOR WH
                   HIGHLIGHT UNDERLINE
           END-PERFORM

           PERFORM VARYING I  FROM 1 BY 1
                   UNTIL   I   >     7
                   ADD     I 1      GIVING L 
                   DISPLAY "ÇRêŒå¥âƒêD" LINE L COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR I
                   LOWLIGHT UNDERLINE
           END-PERFORM

           PERFORM VARYING I  FROM 1 BY 1
                   UNTIL   I   >     7
                   ADD     I 1      GIVING L 
                   DISPLAY "ÇSêŒå¥âƒêD" LINE L COL 31 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR I
                   HIGHLIGHT UNDERLINE
           END-PERFORM

      *     MOVE    15         TO      L
      *    DISPLAY SCR-I8 LINE L COL 1 WITH
      *            BACKGROUND-COLOR BK
      *            FOREGROUND-COLOR WH
      *            HIGHLIGHT UNDERLINE

      *    *** +----+
           DISPLAY WK-L1 LINE 9 COL 1 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR WH
                   LOWLIGHT

      *     DISPLAY SCR-I1

           DISPLAY SCR-I5
           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S120-EX.
           EXIT.

      *    *** COLOR TEST
       S130-10.

           MOVE    ZERO       TO       C
      *    *** ÇQÇPçsâ∫ê¸ÅiÇtÇmÇcÇdÇqÇkÇhÇmÇdÅjï\é¶
           PERFORM VARYING J  FROM 2 BY 1
                   UNTIL   J   >     22
                       DISPLAY WK-SPACE-N LINE J COL 1 WITH
                           BACKGROUND-COLOR BK
                           FOREGROUND-COLOR WH
                           UNDERLINE
           END-PERFORM

           DISPLAY WK-L1 LINE 1 COL 1 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR WH
                   LOWLIGHT

      *    MOVEÇ≈Ç‡èoÇÈ
      *    MOVE "êŒå¥âƒêD" TO WK-NAME
      *    ACCEPT WORK-AREA Ç≈Ç»Ç¢Ç∆í‚é~ÅAÇ`ÇbÇbÇdÇoÇsÇµÇ»Ç¢
      *     ACCEPT  WK-NAME LINE 24 COL 20
      *     MOVE    WK-NAME  TO  SCR-I11-I5

      *    DISPLAY SCR-I11  LINE 24 COL 1 WITH
      *     DISPLAY SCR-I11  LINE 24 COL 1 WITH
      *             BACKGROUND-COLOR BK
      *             FOREGROUND-COLOR WH
      *             HIGHLIGHT UNDERLINE

           MOVE    1           TO      H
           MOVE    2           TO      V
           MOVE    ZERO        TO      C

           PERFORM VARYING J  FROM 1 BY 1
                   UNTIL   J   >     20
                   
               PERFORM VARYING I  FROM 1 BY 1
                       UNTIL   I   >     8
      *    äøéöÇÕì¸óÕèoóàÇ»Ç¢
      *                ACCEPT WK-NAME LINE 21 COL 1 WITH AUTO
      *                DISPLAY WK-NAME LINE V COL H WITH

                       DISPLAY "êŒå¥âƒêD" LINE V COL H WITH
      *                 BACKGROUND-COLOR BK
      *                 BACKGROUND-COLOR WH
                       BACKGROUND-COLOR BL
                       FOREGROUND-COLOR C
                       HIGHLIGHT UNDERLINE

      *
      *    FOREGROUND-COLOR C=0 Ç»ÇÃÇ…ÅAçïèoÇ»Ç¢ÅAê¬Ç…Ç»ÇÈ
      *    1çsñ⁄ÅA2çsñ⁄ÇÕÇnÇjÇ…Ç»ÇÈ
      *    
      *             IF  V = 2 AND H = 1
      *                 DISPLAY "êŒå¥âƒêD" LINE 23 COL 10 WITH
      *                 BACKGROUND-COLOR BK
      *                 FOREGROUND-COLOR 0
      *                  LOWLIGHT UNDERLINE
      *                 MOVE    C        TO     WK-DATA2
      *                 DISPLAY WK-DATA2 LINE 23 COL 1 WITH
      *                 BACKGROUND-COLOR BK
      *                 FOREGROUND-COLOR WH
      *                 HIGHLIGHT UNDERLINE
      *             END-IF
                       ADD     1        TO     C
                       IF      C        =      8
                           MOVE    0        TO     C 
                       END-IF
                       IF      I        =      8
                           MOVE    1        TO     H 
                       ELSE
                           ADD     10       TO     H
                       END-IF
                
               END-PERFORM
               ADD     1        TO     V
           END-PERFORM

           DISPLAY WK-L1 LINE V COL 1 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR WH
                   LOWLIGHT

           ACCEPT  WK-BIT LINE 25
            COL 1
           .
       S130-EX.
           EXIT.

      *    *** årê¸ÉeÉXÉg
       S140-10.

      *    MOVEÇ≈Ç‡èoÇÈ
      *    MOVE "êŒå¥âƒêD" TO WK-NAME
      *    ACCEPT WORK-AREA Ç≈Ç»Ç¢Ç∆í‚é~ÅAÇ`ÇbÇbÇdÇoÇsÇµÇ»Ç¢
      *     DISPLAY SCR-I11-I5 LINE 24 COL 20
      *     ACCEPT  WK-NAME LINE 24 COL 20
      *     MOVE    WK-NAME  TO  SCR-I11-I5
      
      *    DISPLAY SCR-I11  LINE 24 COL 1 WITH
      *    *** DISPLAY Ç≈LINE,COLéwíËÇµÇƒÇ‡ÅAäÓñ{çÄñ⁄Ç≈ÇµÇΩï˚Ç™óDêÊÇ∑ÇÈ
           MOVE    WK-NAME05   TO      WK-NAME
           DISPLAY SCR-I11 LINE 23 COL 2 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR WH
                   HIGHLIGHT UNDERLINE OVERLINE

      *    *** CMD=1 â∫ê¸
           MOVE    1           TO      KEI-CMD
           MOVE    2           TO      KEI-LINE
           MOVE    1           TO      KEI-COL
           MOVE    4           TO      KEI-LEN1
           MOVE    0           TO      KEI-LEN2
           MOVE    1           TO      KEI-COLOR

           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

      *    *** CMD=2 è„ê¸
           MOVE    2           TO      KEI-CMD
           MOVE    2           TO      KEI-LINE
           MOVE    11          TO      KEI-COL
           MOVE    06          TO      KEI-LEN1
           MOVE    0           TO      KEI-LEN2
      *    *** COLOR ïœçXÇµÇƒÇ‡êFÅAïœÇÌÇÁÇ»Ç¢
           MOVE    2           TO      KEI-COLOR

           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

      *    *** CMD=3 ç∂èc
           MOVE    3           TO      KEI-CMD
           MOVE    2           TO      KEI-LINE
           MOVE    21          TO      KEI-COL
           MOVE    03          TO      KEI-LEN1
           MOVE    0           TO      KEI-LEN2
           MOVE    3           TO      KEI-COLOR

           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

      *    *** CMD=4 âEèc
           MOVE    4           TO      KEI-CMD
           MOVE    2           TO      KEI-LINE
           MOVE    31          TO      KEI-COL
           MOVE    05          TO      KEI-LEN1
           MOVE    0           TO      KEI-LEN2
           MOVE    4           TO      KEI-COLOR

           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

      *    *** CMD=5 éläpÇÃòg
           MOVE    5           TO      KEI-CMD
           MOVE    2           TO      KEI-LINE
           MOVE    41          TO      KEI-COL
           MOVE    06          TO      KEI-LEN1
           MOVE    3           TO      KEI-LEN2
           MOVE    5           TO      KEI-COLOR

           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN



           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           DISPLAY WK-NAME01  LINE 20 COL 6



           DISPLAY "Ñ°ÑüÑüÑüÑüÑüÑ¢"  LINE 16 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 17 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 17 COL 33 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY WK-NAME02         LINE 17 COL 23
           DISPLAY "Ñ§ÑüÑüÑüÑüÑüÑ£"  LINE 18 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY



           DISPLAY WK-NAME03 LINE 10 COL 1 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
                   LOWLIGHT UNDERLINE OVERLINE

           DISPLAY "Å@Å@Å@Å@Å@" LINE 10 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR WH
                   LOWLIGHT UNDERLINE



           DISPLAY WK-NAME04      LINE 10 COL 21 WITH
                   BLINK
                   BACKGROUND-COLOR CY
                   FOREGROUND-COLOR BK
                   LOWLIGHT 

      *    *** éöÇ™èoóÕÇ≥ÇÍÇΩèäÇ…â∫ãLéwíËÇÇ∑ÇÈÇ∆ÅAéöÇ™écÇËêFÇ™àÍÇ¬ëOÇ≈éwíË
      *    *** Ç≥ÇÍÇΩBACKGROUND-COLOR CY,FOREGROUND-COLOR BK Ç≈êFÇ™ïœÇÌÇÈ
                   
      *    *** CMD=1 â∫ê¸
           MOVE    1           TO      KEI-CMD
           MOVE    24          TO      KEI-LINE
           MOVE    20          TO      KEI-COL
           MOVE    4           TO      KEI-LEN1
           MOVE    0           TO      KEI-LEN2
           MOVE    1           TO      KEI-COLOR

      *    *** 24,20 4BYTE ÇTêŒ Ç™ïœÇÌÇÈ
           CALL    "CBL_OC_KEISEN" USING
           
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S140-EX.
           EXIT.

      *    *** Å°,ÅZ
       S150-10.

           MOVE    ZERO        TO      I2
           MOVE    ZERO        TO      C
           MOVE    21          TO      C1
           MOVE    41          TO      C2
           MOVE    01          TO      C3
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 80

                   ADD     1           TO      I2
                   IF      I2          >       10
                           MOVE    1           TO      I2
                           ADD     1           TO      C
                           ADD     2           TO      C1
                                                       C2
                                                       C3
                   END-IF

      *    *** ÇPçsÇ…ï°êîÅAêFÇÃéwíËÇ∑ÇÈÇ∆ÅAéwíËí ÇËÇÃêFÅAèoÇ»Ç¢éûÇ™Ç†ÇÈ
                   DISPLAY "ÅZ"  LINE I2 COL C3
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C

      *    *** C <= ZERO,FOREGROUND-COLOR C ê¬Ç≈ï\é¶Ç≥ÇÍÇÈÅAçïèoÇ»Ç¢ÉoÉOóL
                   DISPLAY "Å°"  LINE I2 COL C1
                       WITH
                       BACKGROUND-COLOR BK
      *                 FOREGROUND-COLOR C
                       FOREGROUND-COLOR WK-ZERO

                   DISPLAY "ÅZ"  LINE I2 COL C2
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C

                   MOVE    C           TO      WK-C
                   DISPLAY WK-C LINE 25 COL C1
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR WH

      *             CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01
                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S150-EX.
           EXIT.

      *    *** SIN
       S160-10.

           MOVE    FUNCTION PI TO      WK-PI
           MOVE    0         TO        L
           MOVE    1         TO        C
                                       C1
           MOVE    ZERO      TO        WK-DEG
           PERFORM VARYING I FROM 1 BY 1 
      *             UNTIL I > 72
                   UNTIL I > 1000
                   ADD     1           TO      WK-DEG
                   IF      WK-DEG      >       360
                           MOVE    ZERO        TO      WK-DEG
                   END-IF
      *             COMPUTE WK-DEG WK-DEG2 = I * 5
                   COMPUTE WK-RAD = WK-DEG * WK-PI / 180
                   COMPUTE WK-SIN ROUNDED = 
                           FUNCTION SIN(WK-RAD)

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "DEG"       TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-DEG

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "SIN"       TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-SIN

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    WK-DEG      TO      POT1-DEG
                   MOVE    WK-SIN      TO      POT1-SIN
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-SIN      <       ZERO
                           COMPUTE WK-SIN = WK-SIN * -1
                   END-IF
                   COMPUTE C ROUNDED = WK-SIN * 80
                   ADD     1           TO      L 
                   IF      L           >       24
                           MOVE    1           TO      L
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S160-EX.
           EXIT.

      *    *** TAN
       S170-10.

           MOVE    FUNCTION PI TO      WK-PI
           MOVE    0         TO        L
           MOVE    1         TO        C
                                       C1
           MOVE    ZERO      TO        WK-DEG
           PERFORM VARYING I FROM 1 BY 1 
      *             UNTIL I > 72
                   UNTIL I > 1000
                   ADD     1           TO      WK-DEG
                   IF      WK-DEG      >       360
                           MOVE    ZERO        TO      WK-DEG
                   END-IF
      *             COMPUTE WK-DEG WK-DEG2 = I * 5
                   COMPUTE WK-RAD = WK-DEG * WK-PI / 180
                   COMPUTE WK-TAN ROUNDED = 
                           FUNCTION TAN(WK-RAD)

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "DEG"       TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-DEG

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "TAN"       TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-TAN

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    WK-DEG      TO      POT1-DEG
                   MOVE    WK-TAN      TO      POT1-TAN
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-TAN      <       ZERO
                           COMPUTE WK-TAN = WK-TAN * -1
                   END-IF
                   IF      WK-TAN      <       0.01
                           COMPUTE WK-TAN = WK-TAN * 100
                   ELSE
                       IF      WK-TAN      <       0.1
                               COMPUTE WK-TAN = WK-TAN * 10
                       ELSE
                               CONTINUE
                       END-IF
                   END-IF
                   COMPUTE C ROUNDED = WK-TAN * 80
                   ADD     1           TO      L 
                   IF      L           >       24
                           MOVE    1           TO      L
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                  CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S170-EX.
           EXIT.

      *    *** ASIN
       S180-10.

           MOVE    FUNCTION PI TO      WK-PI
           MOVE    0         TO        L
           MOVE    1         TO        C
                                       C1
           MOVE    ZERO      TO        WK-DEG
           PERFORM VARYING I FROM 1 BY 1 
      *             UNTIL I > 72
                   UNTIL I > 1000
                   ADD     1           TO      WK-DEG
                   IF      WK-DEG      >       360
                           MOVE    ZERO        TO      WK-DEG
                   END-IF
      *             COMPUTE WK-DEG WK-DEG2 = I * 5
                   COMPUTE WK-RAD = WK-DEG * WK-PI / 180
                   COMPUTE WK-ASIN ROUNDED = 
                           FUNCTION ASIN(WK-RAD)

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "DEG"       TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-DEG

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "ASIN"      TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-ASIN

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    WK-DEG      TO      POT1-DEG
                   MOVE    WK-ASIN     TO      POT1-ASIN
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-ASIN     >       ZERO
                           COMPUTE WK-ASIN = WK-ASIN * -1
                           COMPUTE C ROUNDED = WK-ASIN * 80
                           ADD     1           TO      L 
                           IF      L           >       24
                                   MOVE    1           TO      L
                           END-IF
                           DISPLAY "Å°"  LINE L COL C
                               WITH
                               BACKGROUND-COLOR BK
                               FOREGROUND-COLOR C1
                           ADD     1           TO      C1 
                           IF      C1          >       7
                                   MOVE    1           TO      C1
                           END-IF
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S180-EX.
           EXIT.

      *    *** X**2 â°
       S190-10.

           MOVE    0         TO        L
           MOVE    1         TO        C
                                       C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   COMPUTE WK-NUM = I * I

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    I           TO      POT1-I
                   MOVE    WK-NUM      TO      POT1-NUM
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   COMPUTE C ROUNDED = ( WK-NUM / 100 ) * 80
                   ADD     1           TO      L 
                   IF      L           >       24
                           MOVE    1           TO      L
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S190-EX.
           EXIT.

      *    *** X**2 èc
       S200-10.

           MOVE    0         TO        C
           MOVE    1         TO        C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   COMPUTE WK-NUM = I * I

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    I           TO      POT1-I
                   MOVE    WK-NUM      TO      POT1-NUM
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   COMPUTE L ROUNDED = ( WK-NUM / 100 ) * 24
                   ADD     1           TO      C
                   IF      C           >       80
                           MOVE    1           TO      C
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           MOVE    20        TO        C
           MOVE    1         TO        C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   COMPUTE WK-NUM = I * I

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    I           TO      POT1-I
                   MOVE    WK-NUM      TO      POT1-NUM
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   COMPUTE L ROUNDED = ( WK-NUM / 100 ) * 24 / 2 + 12
                   ADD     1           TO      C
                   IF      C           >       80
                           MOVE    1           TO      C
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           MOVE    40        TO        C
           MOVE    1         TO        C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   COMPUTE WK-NUM = I * I

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    I           TO      POT1-I
                   MOVE    WK-NUM      TO      POT1-NUM
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   COMPUTE L ROUNDED = ( WK-NUM / 100 ) * 24 / 4 + 18
                   ADD     1           TO      C
                   IF      C           >       80
                           MOVE    1           TO      C
                   END-IF
                   DISPLAY "Å°"  LINE L COL C
                       WITH
                       BACKGROUND-COLOR BK
                       FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S200-EX.
           EXIT.

      *    *** ècñ_
       S210-10.

           MOVE    40        TO        C
           MOVE    1         TO        C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   ADD     1           TO      C
                   IF      C           >       80
                           MOVE    1           TO      C
                   END-IF

                   DISPLAY SCR-I16 LINE L COL C
      *    *** Ç±Ç±Ç≈ÅAFOREGROUND-COLOR êFéwíËïœçXÇµÇƒÇ‡ÅAïœÇÌÇÁÇ»Ç¢
      *    *** íËã`ãÂÇ≈éwíËÇµÇƒÅAêFïœçXÇ∑ÇÈÇ∆ÅAïœçXÇ≥ÇÍÇÈÇ™ÅAÇøÇ·ÇÒÇ∆èoÇ»Ç¢
      *                 WITH
      *                 BACKGROUND-COLOR BK
      *                 FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM

           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S210-EX.
           EXIT.

      *    *** ñ_ÉOÉâÉt
       S220-10.

           DISPLAY "Ñ°ÑüÑüÑüÑüÑüÑ¢"  LINE 05 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 06 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 06 COL 33 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 07 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 07 COL 33 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 08 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 08 COL 33 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
      *    DISPLAY WK-NAME02         LINE 08 COL 23
           DISPLAY "Ñ§ÑüÑüÑüÑüÑüÑ£"  LINE 09 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY

           MOVE    1         TO        C1
           MOVE    2         TO        C2
           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > 10
                   IF  I  >   100
                       EXIT    PERFORM
                   END-IF
      *             ADD     1           TO      C
      *             IF      C           >       80
      *                     MOVE    1           TO      C
      *             END-IF

           DISPLAY "Ñ†"              LINE 06 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 07 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY
           DISPLAY "Ñ†"              LINE 08 COL 21 WITH
                   BACKGROUND-COLOR BK
                   FOREGROUND-COLOR CY

           DISPLAY SCR-I17

                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   ADD     1           TO      C2 
                   IF      C2          >       7
                           MOVE    1           TO      C2
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM
           .
           GO  TO  S220-20
           
           MOVE    40        TO        C
           MOVE    1         TO        C1
           PERFORM VARYING I FROM -10 BY 1 
                   UNTIL I > 10
                   ADD     1           TO      C
                   IF      C           >       80
                           MOVE    1           TO      C
                   END-IF

                   DISPLAY SCR-I16 LINE L COL C
      *    *** Ç±Ç±Ç≈ÅAFOREGROUND-COLOR êFéwíËïœçXÇµÇƒÇ‡ÅAïœÇÌÇÁÇ»Ç¢
      *    *** íËã`ãÂÇ≈éwíËÇµÇƒÅAêFïœçXÇ∑ÇÈÇ∆ÅAïœçXÇ≥ÇÍÇÈÇ™ÅAÇøÇ·ÇÒÇ∆èoÇ»Ç¢
      *                 WITH
      *                 BACKGROUND-COLOR BK
      *                 FOREGROUND-COLOR C1
                   ADD     1           TO      C1 
                   IF      C1          >       7
                           MOVE    1           TO      C1
                   END-IF

                   CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM
           .

       S220-20.
           ACCEPT  WK-BIT LINE 25 COL 1
           .
       S220-EX.
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
           DISPLAY WK-PGM-NAME " PIN1 π›Ω≥ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 π›Ω≥ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
