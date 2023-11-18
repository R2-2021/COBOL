      *    *** –‚•ûw‚S–‚S—p
      *    *** TEST40.CBL ‚æ‚èƒRƒs[
      *    *** ‘å‘O’ñ@SCREEN SECTION ‚Å@‚k‚h‚m‚dC‚b‚n‚kŽw’è‚·‚é
      *    *** ACCEPT,DISPLAY ‚Å‚k‚h‚m‚dC‚b‚n‚kŽw’è‚·‚é‚Æ‚«AWORK-AREA
      *    *** €–Ú‚Å‚È‚¢‚ÆAŽw’è‚µ‚½êŠ‚É•\Ž¦‚³‚ê‚È‚¢

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST41.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
      *    *** ‚±‚ÌŽw’è‚ÅCOB-CRT-STATUS‚ª’è‹`‚µ‚È‚­‚Ä‚àAŽg—p‰Â
      *    *** PF01-PF24,SHIFT,CTRL,ALT‚à‰Â 1001 - 1064
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
           CRT STATUS IS WK-CRT-STATUS
           CURRENCY SIGN IS "\".
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
           03  PIN1-MOJI      OCCURS 188
                               PIC  X(002).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(136).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST41  ".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST16.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST41.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-CRT-STATUS   PIC  9(004) VALUE ZERO.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-SPACE1       PIC  X(001) VALUE SPACE.
           03  WK-SPACE80.
             05  FILLER        PIC  X(080) VALUE SPACE.
           03  WK-SPACE132.
             05  FILLER        PIC  X(132) VALUE SPACE.
           03  WK-TESTNO-OLD   PIC  9(002) VALUE 0.
           03  WK-TESTNO       PIC  9(002) VALUE 0.
           03  WK-ITEM1        PIC  9(002) VALUE 0.
           03  WK-ITEM2        PIC  9(002) VALUE 0.
           03  WK-ITEM3        PIC  X(010) VALUE SPACE.
           03  WK-ITEM4        PIC  X(010) VALUE SPACE.
           03  WK-ERR-COM      PIC  X(080) VALUE SPACE.
           03  WK-KETA         BINARY-LONG SYNC VALUE ZERO.
           03  WK-SYOKI        BINARY-LONG SYNC VALUE ZERO.
           03  WK-PU-SU1       BINARY-LONG SYNC VALUE ZERO.
           03  WK-PU-SU2       BINARY-LONG SYNC VALUE ZERO.
           03  WK-PU-SU3       BINARY-LONG SYNC VALUE ZERO.
           03  WK-PU-SU4       BINARY-LONG SYNC VALUE ZERO.

      *    *** ‰æ–Êãƒ[ƒ„‚r‚o‚`‚b‚d•ÏŠ·‚µ‚È‚¢
      *    *** ‰Ý•¼‹L†‚ào‚È‚¢
      *    *** ƒsƒŠƒIƒh‚à•ÒW‚³‚ê‚È‚¢
      *    *** 

           03  WK-KEI1         PIC  X(002) VALUE "„Ÿ".
           03  WK-KEI2         PIC  X(002) VALUE "„ ".
           03  WK-KEI3         PIC  X(002) VALUE "„¡".
           03  WK-KEI4         PIC  X(002) VALUE "„¢".
           03  WK-KEI5         PIC  X(002) VALUE "„£".
           03  WK-KEI6         PIC  X(002) VALUE "„¤".
           03  WK-KEI7         PIC  X(002) VALUE "„¥".
           03  WK-KEI8         PIC  X(002) VALUE "„¦".
           03  WK-KEI9         PIC  X(002) VALUE "„§".
           03  WK-KEI10        PIC  X(002) VALUE "„¨".
           03  WK-KEI11        PIC  X(002) VALUE "„©".

           03  WK-KEI21        PIC  X(002) VALUE "„ª".
           03  WK-KEI22        PIC  X(002) VALUE "„«".
           03  WK-KEI23        PIC  X(002) VALUE "„¬".
           03  WK-KEI24        PIC  X(002) VALUE "„­".
           03  WK-KEI25        PIC  X(002) VALUE "„®".
           03  WK-KEI26        PIC  X(002) VALUE "„¯".
           03  WK-KEI27        PIC  X(002) VALUE "„°".
           03  WK-KEI28        PIC  X(002) VALUE "„±".
           03  WK-KEI29        PIC  X(002) VALUE "„²".
           03  WK-KEI30        PIC  X(002) VALUE "„³".
           03  WK-KEI31        PIC  X(002) VALUE "„´".
           03  WK-KEI32        PIC  X(002) VALUE "„µ".
           03  WK-KEI33        PIC  X(002) VALUE "„¶".
           03  WK-KEI34        PIC  X(002) VALUE "„·".
           03  WK-KEI35        PIC  X(002) VALUE "„¸".
           03  WK-KEI36        PIC  X(002) VALUE "„¹".
           03  WK-KEI37        PIC  X(002) VALUE "„º".
           03  WK-KEI38        PIC  X(002) VALUE "„»".
           03  WK-KEI39        PIC  X(002) VALUE "„¼".
           03  WK-KEI40        PIC  X(002) VALUE "„½".
           03  WK-KEI41        PIC  X(002) VALUE "„¾".

      *    *** HAI10,11,12@‚Í–‚•ûw—p
           03  WK-HAI10.
             05  FILLER        PIC  X(002) VALUE "„¡".
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ".
             05  FILLER        PIC  X(002) VALUE "„¦".
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ".
             05  FILLER        PIC  X(002) VALUE "„¦".
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ".
             05  FILLER        PIC  X(002) VALUE "„¢".
             05  FILLER        PIC  X(004) VALUE SPACE.
             05  FILLER        PIC  X(002) VALUE "„¡".
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ".
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¡"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"
           03  WK-HAI10S.
             05  FILLER        PIC  X(002) VALUE "„¡"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¡"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"
           03  WK-HAI11.
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
           03  WK-HAI11S.
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
             05  FILLER        PIC  X(004) VALUE ALL "@"
             05  FILLER        PIC  X(002) VALUE "„ "
           03  WK-HAI12.
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"
           03  WK-HAI12S.
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"
           03  WK-HAI13.
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"
           03  WK-HAI13S.
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"

           03  WK-TIT1.
             05  FILLER        PIC  X(020) VALUE SPACE.
             05  FILLER        PIC  X(028) VALUE
                 "–––@‰Šúƒƒjƒ…[@–––".
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
      *    ***
           03  WK-MID1         PIC  X(012) VALUE
               "ƒƒjƒ…[€–Ú".
           03  WK-HAI          PIC  X(080) VALUE
               ALL "-".

           03  WK-TIT2.
      *      05  FILLER        PIC  X(010) VALUE SPACE.
             05  FILLER        PIC  X(050) VALUE "TEST41".
             05  FILLER        PIC  X(040) VALUE
                 "–––@‚r‚i‚h‚r@Š¿ŽšƒR[ƒh•\@–––@".
             05  FILLER        PIC  X(010) VALUE SPACE.
             05  WK-TIT2-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT2-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT2-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT2-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT2-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT2-SS    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT2-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.
      *    *** 29*7=133 
           03  WK-MID2         PIC  X(203) VALUE ALL "ƒR[ƒh ".
           03  WK-HAI2         PIC  X(203) VALUE ALL "------ ".

           03  WK-MEI1.
             05  WK-MEI1-I00   PIC  X(080) VALUE
               "‚O‚OD‰Šúƒƒjƒ…[".
             05  WK-MEI1-I01   PIC  X(080) VALUE
               "‚O‚PDŠ¿ŽšƒR[ƒhˆê——".
             05  WK-MEI1-I02   PIC  X(080) VALUE
               "‚O‚QD".
             05  WK-MEI1-I03   PIC  X(080) VALUE
               "‚O‚RD".
             05  WK-MEI1-I04   PIC  X(080) VALUE
               "‚O‚SD".
             05  WK-MEI1-I05   PIC  X(080) VALUE
               "‚O‚TD".
             05  WK-MEI1-I06   PIC  X(080) VALUE
               "‚O‚UD".
             05  WK-MEI1-I07   PIC  X(080) VALUE
               "‚O‚VD".
             05  WK-MEI1-I08   PIC  X(080) VALUE
               "‚O‚WD".
             05  WK-MEI1-I09   PIC  X(080) VALUE
               "‚O‚XD".
             05  WK-MEI1-I10   PIC  X(080) VALUE
               "‚P‚OD".
             05  WK-MEI1-I11   PIC  X(080) VALUE
               "‚P‚PD".
             05  WK-MEI1-I99   PIC  X(080) VALUE
               "‚X‚XDI—¹".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 52
                               PIC  X(203) VALUE SPACE.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE 1.
           03  P2              BINARY-LONG SYNC VALUE 3.
           03  P3              BINARY-LONG SYNC VALUE 4.
           03  P4              BINARY-LONG SYNC VALUE 5.
           03  P5              BINARY-LONG SYNC VALUE 6.
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-DISPLAY      PIC  X(001) VALUE ZERO.
           03  SW-ERROR        PIC  X(001) VALUE ZERO.
           03  SW-AMP          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

       01  COLORS-AREA.
           03  BLACK           PIC  9(001) VALUE 0.
           03  BLUE            PIC  9(001) VALUE 1.
           03  GREEN           PIC  9(001) VALUE 2.
           03  CYAN            PIC  9(001) VALUE 3.
           03  RED             PIC  9(001) VALUE 4.
           03  MAGENTA         PIC  9(001) VALUE 5.
           03  BROWN           PIC  9(001) VALUE 6.
      *    *** HIGHLIGHTŽž‚Í‰©F
           03  YELLOW          PIC  9(001) VALUE 6.
           03  GREY            PIC  9(001) VALUE 7.
      *    *** HIGHLIGHTŽž‚Í”’F
           03  WHITE           PIC  9(001) VALUE 7.

           03  COLORS-NAME.
             05  COLOR-0L      PIC  X(010) VALUE "BLACK     ".
             05  COLOR-0H      PIC  X(010) VALUE "DARK GREY ".
             05  COLOR-1       PIC  X(010) VALUE "BLUE      ".
             05  COLOR-2       PIC  X(010) VALUE "GREEN     ".
             05  COLOR-3       PIC  X(010) VALUE "CYAN      ".
             05  COLOR-4       PIC  X(010) VALUE "RED       ".
             05  COLOR-5       PIC  X(010) VALUE "MAGENTA   ".
             05  COLOR-6L      PIC  X(010) VALUE "BROWN     ".
             05  COLOR-6H      PIC  X(010) VALUE "YELLOW    ".
             05  COLOR-7L      PIC  X(010) VALUE "LIGHT GREY".
             05  COLOR-7H      PIC  X(010) VALUE "WHITE     ".



       SCREEN                  SECTION.
       01  SCREEN-AREA.

           03  SCR00-AREA.
             05  LINE 25 COL 8 VALUE "ƒƒjƒ…[‚m‚"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
      *          HIGHLIGHT
      *          OVERLINE
                 UNDERLINE
                 .
      *    *** TO ‚Í“ü—Í€–Ú‚Æ‚µ‚Ä’è‹`‚·‚é
             05  COL PLUS 2 TO WK-TESTNO
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BROWN
      *          OVERLINE
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).
      *
      * •s–¾          JUST
      * “ü—Í–        SECURE
      * •s–¾          FULL
      * •s–¾           PROMPT
      * ”½“]           REVERSE-VIDEO
      * ‰æ–Ê‰Šú‰»     ERASE EOL
      * Žw’èo—ˆ‚È‚¢   EOS
             05  COL PLUS 2 USING WK-ITEM1
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BROWN
                 HIGHLIGHT
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM2
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BROWN
                 HIGHLIGHT
                 UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM3
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BROWN
                 HIGHLIGHT
                 UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

             05  COL PLUS 2 USING WK-ITEM4
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BROWN
                 HIGHLIGHT
                 UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

           03  SCR01-AREA
      *    *** W’c€–Ú‚ÅFŽw’èo—ˆ‚é
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR01-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR CYAN
                 HIGHLIGHT
                 VALUE "–––@–‚•ûw‚S–‚S@–––".

           03  SCR01-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "”ŽšŒ…@“ü—Í".

      *    *** USING‚Í“ü—ÍAo—Í—¼•û‚ÉŽg‚¦‚é@Å‰‚É‰ºü‚ª•\Ž¦‚³‚ê‚Ä‚é
             05  USING WK-KETA
                 LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                 AUTO
                       PIC  9(001).

             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "‰Šú’l@“ü—Í".

             05  USING WK-SYOKI
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                       PIC  9(004).

             05  FROM WK-ERR-COM
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR YELLOW
                 HIGHLIGHT
                 PIC X(050).

           03  SCR01-3-AREA.
             05  BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR GREEN
                 HIGHLIGHT.

      *    *** FROM ‚Ío—Í€–Ú‚Æ‚µ‚Ä’è‹`‚·‚é
               07  FROM WK-PU-SU1
                   LINE L COL C
                   PIC  ZZZ9.
               07  FROM WK-PU-SU2
                   COL PLUS 2
                   PIC  ZZZ9.
               07  FROM WK-PU-SU3
                   COL PLUS 2
                   PIC  ZZZ9.
               07  FROM WK-PU-SU4
                   COL PLUS 2
                   PIC  ZZZ9.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** ‚O‚OD‰Šúƒƒjƒ…[
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX

           PERFORM UNTIL WK-TESTNO = 99
                   EVALUATE TRUE
                       WHEN WK-TESTNO = 0
      *    *** ‚O‚OD‰Šúƒƒjƒ…[
                            PERFORM S020-10     THRU    S020-EX
                       WHEN WK-TESTNO = 1
      *    *** ‚O‚PDŠ¿ŽšƒR[ƒhˆê——
                            PERFORM S210-10     THRU    S210-EX
                       WHEN WK-TESTNO = 2
                            PERFORM S220-10     THRU    S220-EX
                       WHEN WK-TESTNO = 3
                            PERFORM S230-10     THRU    S230-EX
                       WHEN WK-TESTNO = 4
                            PERFORM S240-10     THRU    S240-EX
                       WHEN WK-TESTNO = 5
                            PERFORM S250-10     THRU    S250-EX
                       WHEN WK-TESTNO = 6
                            PERFORM S260-10     THRU    S260-EX
                       WHEN WK-TESTNO = 7
                            PERFORM S270-10     THRU    S270-EX
                       WHEN WK-TESTNO = 8 
                            PERFORM S280-10     THRU    S280-EX
                       WHEN WK-TESTNO = 9  
                            PERFORM S290-10     THRU    S290-EX
                       WHEN WK-TESTNO = 10
                            PERFORM S300-10     THRU    S300-EX
                       WHEN WK-TESTNO = 11
                            PERFORM S310-10     THRU    S310-EX
                   END-EVALUATE

      *    *** ‰Šúƒƒjƒ…[•\Ž¦@“ü—Í‘Ò‚¿
                   PERFORM S030-10    THRU    S030-EX
           END-PERFORM

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

           MOVE    WDT-DATE-YY  TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM  TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD  TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH  TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI  TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS  TO      WK-TIT1-SS

           MOVE    WDT-DATE-YY  TO      WK-TIT2-YY
           MOVE    WDT-DATE-MM  TO      WK-TIT2-MM
           MOVE    WDT-DATE-DD  TO      WK-TIT2-DD

           MOVE    WDT-DATE-HH  TO      WK-TIT2-HH
           MOVE    WDT-DATE-MI  TO      WK-TIT2-MI
           MOVE    WDT-DATE-SS  TO      WK-TIT2-SS

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

      *    *** ‚O‚OD‰Šúƒƒjƒ…[
       S020-10.
           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       25
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           DISPLAY WK-TIT1
                   AT LINE 1 COL 1
      *    *** COLOR Žw’è‚µ‚È‚¢‚ÆA”wŒiF•AˆóŽšF”’‚É‚È‚é
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR CYAN
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MID1
                   AT LINE 2 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-HAI
                   AT LINE 3 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I00
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I01
                   AT LINE 5 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I02
                   AT LINE 6 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I03
                   AT LINE 7 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I04
                   AT LINE 8 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I05
                   AT LINE 9 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I06
                   AT LINE 10 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I07
                   AT LINE 11 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I08
                   AT LINE 12 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I09
                   AT LINE 13 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I10
                   AT LINE 14 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I11
                   AT LINE 15 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I99
                   AT LINE 23 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           MOVE    WK-CRT-STATUS TO     WK-ITEM4
           DISPLAY SCR00-AREA
           .
       S020-EX.
           EXIT.

      *    *** ‰Šúƒƒjƒ…[•\Ž¦@“ü—Í‘Ò‚¿
       S030-10.
           MOVE    WK-TESTNO   TO      WK-TESTNO-OLD
           ACCEPT  SCR00-AREA

           IF      WK-TESTNO   NOT =   WK-TESTNO-OLD
      *    *** ƒJƒŒƒ“ƒ_[@ˆê“xƒƒjƒ…[•ÏX‚µ‚½‚çAƒŠƒZƒbƒg‚Ì‚½‚ßA
      *    *** WK-YYYY,WK-MM ‚y‚d‚q‚n‚É‚·‚é
      *             MOVE    ZERO        TO      WK-YYYY
      *             MOVE    ZERO        TO      WK-MM
                   CONTINUE
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** READ PIN1
       S100-10.
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
               ELSE

                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN
           .
       S100-EX.
           EXIT.
      *
      *    *** ‚O‚PD
       S210-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
      *             DISPLAY WK-SPACE80
                   DISPLAY WK-SPACE132
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** ŒrüA“ü—Í€–Ú
           MOVE    1           TO      WK-KETA
           MOVE    1           TO      WK-SYOKI
      *     PERFORM VARYING I2 FROM 1 BY 1
      *             UNTIL I2 >  24
      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
           IF      WK-PIN1-EOF NOT = HIGH-VALUE
                   PERFORM S211-10     THRU    S211-EX
           END-IF
      *     END-PERFORM

      *             UNTIL   WK-PIN1-EOF   =     HIGH-VALUE
      *     END-PERFORM
           .
       S210-EX.
           EXIT.

      *    *** Š¿ŽšƒR[ƒhˆê——
       S211-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       188
               IF (PIN1-MOJI(I) >=  X"81AD" AND
                   PIN1-MOJI(I) <=  X"81B7")   OR
                  (PIN1-MOJI(I) >=  X"81C0" AND
                   PIN1-MOJI(I) <=  X"81C7")   OR
                  (PIN1-MOJI(I) >=  X"81CF" AND
                   PIN1-MOJI(I) <=  X"81D9")   OR
                  (PIN1-MOJI(I) >=  X"81E9" AND
                   PIN1-MOJI(I) <=  X"81EF")   OR
                  (PIN1-MOJI(I) >=  X"81F8" AND
                   PIN1-MOJI(I) <=  X"81FB")   OR
                  (PIN1-MOJI(I) >=  X"8240" AND
                   PIN1-MOJI(I) <=  X"824E")   OR
                  (PIN1-MOJI(I) >=  X"8259" AND
                   PIN1-MOJI(I) <=  X"825F")   OR
                  (PIN1-MOJI(I) >=  X"827A" AND
                   PIN1-MOJI(I) <=  X"8280")   OR
                  (PIN1-MOJI(I) >=  X"829B" AND
                   PIN1-MOJI(I) <=  X"829E")   OR
                  (PIN1-MOJI(I) >=  X"82F2" AND
                   PIN1-MOJI(I) <=  X"82FC")   OR
                  (PIN1-MOJI(I) >=  X"8397" AND
                   PIN1-MOJI(I) <=  X"839E")   OR
                  (PIN1-MOJI(I) >=  X"83B7" AND
                   PIN1-MOJI(I) <=  X"83BE")   OR
                  (PIN1-MOJI(I) >=  X"83D7" AND
                   PIN1-MOJI(I) <=  X"83FC")   OR
                  (PIN1-MOJI(I) >=  X"8461" AND
                   PIN1-MOJI(I) <=  X"846F")   OR
                  (PIN1-MOJI(I) >=  X"8492" AND
                   PIN1-MOJI(I) <=  X"849E")   OR
                  (PIN1-MOJI(I) >=  X"84BF" AND
                   PIN1-MOJI(I) <=  X"84FC")   OR
                  (PIN1-MOJI(I) >=  X"8540" AND
                   PIN1-MOJI(I) <=  X"86FC")   OR
                  (PIN1-MOJI(I) =   X"875E")   OR
                  (PIN1-MOJI(I) >=  X"8776" AND
                   PIN1-MOJI(I) <=  X"877D")   OR
                  (PIN1-MOJI(I) >=  X"8790" AND
                   PIN1-MOJI(I) <=  X"8792")   OR
                  (PIN1-MOJI(I) >=  X"8795" AND
                   PIN1-MOJI(I) <=  X"8797")   OR
                  (PIN1-MOJI(I) >=  X"879A" AND
                   PIN1-MOJI(I) <=  X"889E")   OR
                  (PIN1-MOJI(I) >=  X"9873" AND
                   PIN1-MOJI(I) <=  X"989E")   OR
                  (PIN1-MOJI(I) >=  X"EAA5" AND
                   PIN1-MOJI(I) <=  X"EFFC")
                   CONTINUE
               ELSE
                   MOVE    PIN1-MOJI(I) TO  PR-LINE (J) (P1:2)
                   MOVE    PIN1-MOJI(I) (1:1) TO  PIC-X

                   DIVIDE PIC-Halfword BY 16
                          GIVING L REMAINDER R

                   ADD     1        TO      L R

                   MOVE    Hex-Digit (L)   TO  PR-LINE (J) (P2:1)
                   MOVE    Hex-Digit (R)   TO  PR-LINE (J) (P3:1)

                   MOVE    PIN1-MOJI(I) (2:1) TO  PIC-X

                   DIVIDE PIC-Halfword BY 16
                          GIVING L REMAINDER R

                   ADD     1        TO      L R

                   MOVE    Hex-Digit (L)   TO  PR-LINE (J) (P4:1)
                   MOVE    Hex-Digit (R)   TO  PR-LINE (J) (P5:1)
                   ADD     1           TO      J
                   IF      J           >       20
                           MOVE    1           TO       J
                           ADD     7           TO       P1
      *                     IF      P1          >        7 * 29
                           IF      P1          >        7 * 8
                               ADD     1           TO      WK-PAGE
                               MOVE    WK-PAGE     TO      WK-TIT2-PAGE
                               DISPLAY WK-TIT2
                                       AT LINE 4 COL 1
                                  WITH BACKGROUND-COLOR CYAN
                                       FOREGROUND-COLOR WHITE
                               END-DISPLAY

                               DISPLAY WK-MID2
                                       AT LINE 4 COL 1
                                  WITH BACKGROUND-COLOR CYAN
                                       FOREGROUND-COLOR WHITE
                               END-DISPLAY

                               DISPLAY WK-HAI2
                                       AT LINE 4 COL 1
                                  WITH BACKGROUND-COLOR CYAN
                                       FOREGROUND-COLOR WHITE
                               END-DISPLAY

                               PERFORM VARYING K  FROM 1 BY 1
                                       UNTIL   K   >       20
                                   COMPUTE L = K + 3
                                   DISPLAY PR-LINE(K)
                                           AT LINE K COL 1
                                      WITH BACKGROUND-COLOR CYAN
                                           FOREGROUND-COLOR WHITE
                                   END-DISPLAY
                               END-PERFORM

                               MOVE    SPACE       TO      PRINT-AREA
                               MOVE    1           TO      P1
                               MOVE    3           TO      P2
                               MOVE    4           TO      P3
                               MOVE    5           TO      P4
                               MOVE    6           TO      P5
                           ELSE
                               ADD     7           TO      P2
                               ADD     7           TO      P3
                               ADD     7           TO      P4
                               ADD     7           TO      P5
                           END-IF
                   ELSE
                           CONTINUE
                   END-IF
               END-IF
           END-PERFORM

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX

           IF      WK-PIN1-EOF =       HIGH-VALUE
                   DISPLAY WK-TIT2
                           AT LINE 1 COL 1
                      WITH BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY
                   GO  TO  S211-EX

                   ADD     1           TO      WK-PAGE
                   MOVE    WK-PAGE     TO      WK-TIT2-PAGE
                   ADD     1           TO      WK-PAGE
                   DISPLAY WK-TIT2
                           AT LINE 4 COL 1
                      WITH BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY

                   WRITE   POT1-REC    FROM    WK-MID2
                   DISPLAY WK-MID2
                           AT LINE 4 COL 1
                      WITH BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY

                   DISPLAY WK-HAI2
                           AT LINE 4 COL 1
                      WITH BACKGROUND-COLOR CYAN
                           FOREGROUND-COLOR WHITE
                   END-DISPLAY

                   PERFORM VARYING K  FROM 1 BY 1
                           UNTIL   K   >       52
                           COMPUTE L = K + 3
                           DISPLAY PR-LINE(K) 
                                   AT LINE L COL 1
                              WITH BACKGROUND-COLOR CYAN
                                   FOREGROUND-COLOR WHITE
                              END-DISPLAY
                   END-PERFORM
           END-IF
           .
       S211-EX.
           EXIT.

      *    *** ‚O‚QD

       S220-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           .
       S220-EX.
           EXIT.

      *    *** ‚O‚RD
       S230-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S230-EX.
           EXIT.

      *    *** ‚O‚SD
       S240-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S240-EX.
           EXIT.

      *    *** ‚O‚TD
       S250-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S250-EX.
           EXIT.

      *    *** ‚O‚UD
       S260-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S260-EX.
           EXIT.

      *    *** ‚O‚VD
       S270-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S270-EX.
           EXIT.

      *    *** ‚O‚WD
       S280-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S280-EX.
           EXIT.

      *    *** ‚O‚XD
       S290-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .
       S290-EX.
           EXIT.

      *    *** ‚P‚OD
       S300-10.

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM
           .
       S300-EX.
           EXIT.

      *    *** ‚P‚PD
       S310-10.

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM
           .
       S310-EX.
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
      *    DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT
      *    DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
