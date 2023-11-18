      *    *** ƒAƒjƒˆê——‚Ìo—Í
      *    *** PRINT AREA 2ŽŸŒ³‚ÅƒZƒbƒg
      *    *** Œrü˜gƒZƒbƒgA‚`‚ScA‚`‚S‰¡@ƒvƒƒOƒ‰ƒ€‚ÅÝ’è


       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST30.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ƒAƒjƒ@ƒ^ƒCƒgƒ‹ƒf[ƒ^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ƒAƒjƒˆê——
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
             05  FILLER        PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(250).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST30  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST28_201110_2018XX.CSV".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST30.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.
           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-SEQNO        PIC  X(004) VALUE SPACE.
           03  WK-YYYY         PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  X(002) VALUE SPACE.
           03  WK-KISETU       PIC  X(002) VALUE SPACE.
           03  WK-TITLE        PIC  X(020) VALUE SPACE.
           03  WK-TITLE2       PIC  X(020) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.

           03  WK-TIT1.
             05  FILLER        PIC  X(040) VALUE
                 "–––@ƒAƒjƒƒ^ƒCƒgƒ‹ˆê——•\@–––@@".
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

           03  WK-TIT1-A4T.
             05  FILLER        PIC  X(040) VALUE "TEST30-T".
             05  WK-TIT1-A4T-1 PIC  X(076) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05  FILLER        PIC  X(070) VALUE "TEST30-Y".
             05  WK-TIT1-A4Y-1 PIC  X(076) VALUE SPACE.

      *    *** ˆóüƒy[ƒWÝ’è‚Åƒy[ƒW‚ ‚½‚è‚Ìs•¶Žš”Acs”‚ª
      *    *** •Ï‚í‚é‚Ì‚ÅAˆÈ‰ºÝ’è‚Åˆóü‚·‚é

      *    *** ˆóüƒy[ƒWÝ’è
      *    *** ”¼ŠpƒtƒHƒ“ƒg@‚l‚rƒSƒVƒbƒN
      *    *** ‘SŠpƒtƒHƒ“ƒg@‚l‚rƒSƒVƒbƒN
      *    *** ƒtƒHƒ“ƒg‚@‚Q‚U‚‚A‚VD‚R‚‚”
      *    *** s‘—‚è‚O“
      *    *** —]”’@ã‚P‚OA‰º‚P‚OA‰E‚P‚OA¶‚P‚O‚‚
      *    *** 
      *    *** s‚ ‚½‚è‚Ì•¶Žš”F‰¡Žž‚Q‚P‚R  cŽž‚P‚S‚U
      *    *** c•ûŒü‚Ìs”F    ‰¡Žž‚V‚P    cŽž‚P‚O‚S

      *    *** MAX=146,A4c—p
      *    *** 2+40*3=122
           03  WK-MID1-A4T.
             05  FILLER        PIC  X(002) VALUE "„ ".
             05  FILLER        PIC  X(120) VALUE ALL
                 " SEQ„   ”N„ ‹G„ ƒ^ƒCƒgƒ‹@@@@@@@„ ".

      *    *** MAX=213,A4‰¡—p
      *    *** 2+40*5=202
           03  WK-MID1-A4Y.
             05  FILLER        PIC  X(002) VALUE "„ ".
             05  FILLER        PIC  X(200) VALUE ALL
                 " SEQ„   ”N„ ‹G„ ƒ^ƒCƒgƒ‹@@@@@@@„ ".

      *    *** ‰æ–Ê€–Ú
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

           03  WK-KEI1-A4T.
             05  FILLER        PIC  X(002) VALUE "„¡"
             05  FILLER        OCCURS 2.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"

           03  WK-KEI2-A4T.
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        OCCURS 2.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"

           03  WK-KEI3-A4T.
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        OCCURS 2.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"

           03  WK-KEI1-A4Y.
             05  FILLER        PIC  X(002) VALUE "„¡"
             05  FILLER        OCCURS 4.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¦"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¦"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¢"

           03  WK-KEI2-A4Y.
             05  FILLER        PIC  X(002) VALUE "„¥"
             05  FILLER        OCCURS 4.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„©"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„©"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„§"

           03  WK-KEI3-A4Y.
             05  FILLER        PIC  X(002) VALUE "„¤"
             05  FILLER        OCCURS 4.  
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(004) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(002) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"
               07  FILLER      PIC  X(022) VALUE ALL "„Ÿ"
               07  FILLER      PIC  X(002) VALUE "„¨"

             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(004) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(002) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„¨"
             05  FILLER        PIC  X(022) VALUE ALL "„Ÿ"
             05  FILLER        PIC  X(002) VALUE "„£"

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 50
                               PIC  X(213) VALUE SPACE.

       01  CNS-AREA.
      *    *** PX ‚ÌˆóŽšˆÊ’u
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 3.
           03  CNS-P3          BINARY-LONG SYNC VALUE 7.
           03  CNS-P4          BINARY-LONG SYNC VALUE 9.
           03  CNS-P5          BINARY-LONG SYNC VALUE 13.
           03  CNS-P6          BINARY-LONG SYNC VALUE 15.
           03  CNS-P7          BINARY-LONG SYNC VALUE 17.
           03  CNS-P8          BINARY-LONG SYNC VALUE 19.
           03  CNS-P9          BINARY-LONG SYNC VALUE 41.
      *    *** PX ‚ÌŒ…”
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P5-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P6-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P7-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P8-L        BINARY-LONG SYNC VALUE 22.
           03  CNS-P9-L        BINARY-LONG SYNC VALUE 2.
      *    *** P1-PX ‚ÌˆóŽš‡ŒvŒ…”@ƒXƒy[ƒXŠÜ‚Þ
           03  CNS-L-SIZE      BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C2              BINARY-LONG SYNC VALUE 1.
           03  C3              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  J2              BINARY-LONG SYNC VALUE 1.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO
           03  P5              BINARY-LONG SYNC VALUE ZERO
           03  P6              BINARY-LONG SYNC VALUE ZERO
           03  P7              BINARY-LONG SYNC VALUE ZERO
           03  P8              BINARY-LONG SYNC VALUE ZERO
           03  P9              BINARY-LONG SYNC VALUE ZERO
           03  PX              BINARY-LONG SYNC VALUE ZERO

      *    *** s‚ ‚½‚è‚Ì•¶Žš”F‰¡Žž‚Q‚P‚R  cŽž‚P‚S‚U
      *    *** c•ûŒü‚Ìs”F    ‰¡Žž‚V‚P    cŽž‚P‚O‚S
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
      *    *** "1" = A4c,
      *    *** "0" = A4‰¡
           03  SW-A4TATE       PIC  X(001) VALUE "1".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PRINT TABLE SET
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** AT END Žžˆ—
           PERFORM S120-10     THRU    S120-EX

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

           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

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

           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + CNS-P5-L + CNS-P6-L
                              + CNS-P7-L + CNS-P8-L + CNS-P9-L

           IF      SW-A4TATE   =       "1"
      *    *** Š„ŽZ‚Å¤‚ð‹‚ßA¤ * CNS-L-SIZE‚ð‹‚ß‚é
                   COMPUTE C3 = 146 / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
      *    *** 50 = ( 104 - 4 ) / 2
      *    *** - 4 ‚Íƒwƒbƒ_[A/ 2 ‚Í–¾×sŽŸ‚ÌŒrü•ªœ‚­
                   MOVE    50          TO      R
           ELSE
                   COMPUTE C3 = 213 / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
      *    *** 33 = ( 71 - 5 ) / 2
      *    *** - 5 ‚Íƒwƒbƒ_[A/ 2 ‚Í–¾×sŽŸ‚ÌŒrü•ªœ‚­
                   MOVE    33          TO      R
           END-IF

      *    *** ƒe[ƒuƒ‹i2jˆÈ~@‚’¼üA’u‚«Š·‚¦‚é
           COMPUTE CNS-L-SIZE = CNS-L-SIZE - 2

           MOVE    CNS-P1      TO      P1
           MOVE    CNS-P2      TO      P2
           MOVE    CNS-P3      TO      P3
           MOVE    CNS-P4      TO      P4
           MOVE    CNS-P5      TO      P5
           MOVE    CNS-P6      TO      P6
           MOVE    CNS-P7      TO      P7
           MOVE    CNS-P8      TO      P8
           MOVE    CNS-P9      TO      P9
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                            DELIMITED BY ","
                       INTO
                            WK-SEQNO
                            WK-YYYY
                            WK-MM
                            WK-KISETU
                            WK-TITLE2
                            WK-SITE
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *    *** 19,1 ‚©‚çŠ¿ŽšŽn‚Ü‚éŽžAƒZƒbƒg‚µ‚È‚¢
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      M 

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 19
      *    *** Š¿Žš‚©Hˆê•”Š¿Žš‚Å‚È‚¢•”•ª‚ ‚èA
      *    *** Œã‚©‚çŽw’è‚³‚ê‚½•”•ª‚àŽw’è‚µ‚Ä‚È‚¢
               IF ( WK-TITLE2 (I:2) >= X"8140" AND 
                    WK-TITLE2 (I:2) <= X"9FFC" )   OR
                  ( WK-TITLE2 (I:2) >= X"E040" AND 
                    WK-TITLE2 (I:2) <= X"EAA4" )
                         MOVE   WK-TITLE2 (I:2) TO   WK-TITLE (M:2)
      *    *** J 1,3,5...
                         ADD    1    TO     I
                         ADD    2    TO     M
                   ELSE
                         MOVE   WK-TITLE2 (I:1) TO   WK-TITLE (M:1)
      *    *** J 1,2,3...
                         ADD    1    TO     M
                   END-IF
           END-PERFORM
           .
       S020-EX.
           EXIT.

      *    *** PRINT TABLE SET
       S100-10.

           MOVE    WK-KEI2     TO      PR-LINE (J) (P1:CNS-P1-L)
           CALL    "C$JUSTIFY" USING   WK-SEQNO "R"
           MOVE    WK-SEQNO    TO      PR-LINE (J) (P2:CNS-P2-L)
           MOVE    WK-KEI2     TO      PR-LINE (J) (P3:CNS-P3-L)
           MOVE    WK-YYYY     TO      PR-LINE (J) (P4:CNS-P4-L)
           MOVE    WK-KEI2     TO      PR-LINE (J) (P5:CNS-P5-L)
           MOVE    WK-KISETU   TO      PR-LINE (J) (P6:CNS-P6-L)
           MOVE    WK-KEI2     TO      PR-LINE (J) (P7:CNS-P7-L)
           MOVE    WK-TITLE    TO      PR-LINE (J) (P8:CNS-P8-L)
           MOVE    WK-KEI2     TO      PR-LINE (J) (P9:CNS-P9-L)

           ADD     1           TO      J
           IF      J           >       R
                   MOVE    1           TO      J
                   ADD     1           TO      C2
                   IF      C2          >       C3
      *    *** PRINT TBL WRITE
                           PERFORM S110-10     THRU    S110-EX

                           MOVE    SPACE       TO      PRINT-AREA
                           MOVE    CNS-P1      TO      P1
                           MOVE    CNS-P2      TO      P2
                           MOVE    CNS-P3      TO      P3
                           MOVE    CNS-P4      TO      P4
                           MOVE    CNS-P5      TO      P5
                           MOVE    CNS-P6      TO      P6
                           MOVE    CNS-P7      TO      P7
                           MOVE    CNS-P8      TO      P8
                           MOVE    CNS-P9      TO      P9
                           MOVE    1           TO      C2
                   ELSE
                           ADD     CNS-L-SIZE  TO      P1
                           ADD     CNS-L-SIZE  TO      P2
                           ADD     CNS-L-SIZE  TO      P3
                           ADD     CNS-L-SIZE  TO      P4
                           ADD     CNS-L-SIZE  TO      P5
                           ADD     CNS-L-SIZE  TO      P6
                           ADD     CNS-L-SIZE  TO      P7
                           ADD     CNS-L-SIZE  TO      P8
                           ADD     CNS-L-SIZE  TO      P9
                   END-IF
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PRINT TBL WRITE
       S110-10.

           ADD     1           TO      WK-PAGE
           MOVE    WK-PAGE     TO      WK-TIT1-PAGE
           IF      SW-A4TATE   =       "1"
                   MOVE    WK-TIT1     TO      WK-TIT1-A4T-1
                   MOVE    WK-TIT1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI2-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

           ELSE
                   MOVE    WK-TIT1     TO      WK-TIT1-A4Y-1
                   MOVE    WK-TIT1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    SPACE       TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI2-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

           END-IF

           PERFORM VARYING K FROM 1 BY 1
                   UNTIL   K > R
                   MOVE    PR-LINE (K) TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

      *    *** R=ÅIs”
                   IF      K           =       R
                       IF      SW-A4TATE   =       "1"
                               MOVE    WK-KEI3-A4T TO      POT1-REC
      *    *** WRITE POT1
                               PERFORM S130-10     THRU    S130-EX

                       ELSE
                               MOVE    WK-KEI3-A4Y TO      POT1-REC
      *    *** WRITE POT1
                               PERFORM S130-10     THRU    S130-EX

                       END-IF
                   ELSE
                       IF      SW-A4TATE   =       "1"
                               MOVE    WK-KEI2-A4T TO      POT1-REC
      *    *** WRITE POT1
                               PERFORM S130-10     THRU    S130-EX

                       ELSE
                               MOVE    WK-KEI2-A4Y TO      POT1-REC
      *    *** WRITE POT1
                               PERFORM S130-10     THRU    S130-EX

                       END-IF
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** AT END Žžˆ—
      *    *** 0Œ‚Å‚àA‚`‚s@‚d‚m‚cŽžAŒ”o—Í
       S120-10.

           COMPUTE PX = P1 + 18
           MOVE    "*** "      TO      PR-LINE (J) (PX:4)
           MOVE    WK-PIN1-CNT TO      WK-CNT
           COMPUTE PX = PX + 4
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:4)
           COMPUTE PX = PX + 4
           MOVE    " Œ ***"   TO      PR-LINE (J) (PX:7)

           MOVE    J           TO      J2
           PERFORM UNTIL C2 > C3
                   MOVE    WK-KEI2     TO  PR-LINE (J2) (P1:CNS-P1-L)
                   MOVE    WK-KEI2     TO  PR-LINE (J2) (P3:CNS-P3-L)
                   MOVE    WK-KEI2     TO  PR-LINE (J2) (P5:CNS-P5-L)
                   MOVE    WK-KEI2     TO  PR-LINE (J2) (P7:CNS-P7-L)
                   MOVE    WK-KEI2     TO  PR-LINE (J2) (P9:CNS-P9-L)

                   ADD     1           TO      J2
                   IF      J2          >       R
                           MOVE    1           TO      J2
                           ADD     1           TO      C2
                           IF      C2          >       C3
                                   CONTINUE
                           ELSE
                                   ADD     CNS-L-SIZE  TO      P1
                                   ADD     CNS-L-SIZE  TO      P3
                                   ADD     CNS-L-SIZE  TO      P5
                                   ADD     CNS-L-SIZE  TO      P7
                                   ADD     CNS-L-SIZE  TO      P9
                           END-IF
                   END-IF
           END-PERFORM

      *    *** PRINT TBL WRITE
           PERFORM S110-10     THRU    S110-EX
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

           WRITE   POT1-REC
           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS 
                           " WK-POT1-CNT=" WK-POT1-CNT
                   STOP    RUN
           END-IF
           .
       S130-EX.
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
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 Íß°¼Þ= " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
