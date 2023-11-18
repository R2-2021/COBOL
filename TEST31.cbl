      *    *** ÉAÉjÉÅàÍóóÇÃèoóÕ
      *    *** PRINT AREA 2éüå≥Ç≈ÉZÉbÉg
      *    *** â∫ê¸ÉZÉbÉgÅAÇ`ÇSècÅAÇ`ÇSâ°Å@ÉvÉçÉOÉâÉÄÇ≈ê›íË

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST31.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ÉAÉjÉÅÅ@É^ÉCÉgÉãÉfÅ[É^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ÉAÉjÉÅàÍóó
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
           03  FILLER          PIC  X(300).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST31  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST28_201110_2018XX.CSV".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST31.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.

           03  WK-SEQNO        PIC  X(004) VALUE SPACE.
           03  WK-YYYY         PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  X(002) VALUE SPACE.
           03  WK-KISETU       PIC  X(002) VALUE SPACE.
           03  WK-TITLE        PIC  X(020) VALUE SPACE.
           03  WK-TITLE2       PIC  X(022) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.

           03  WK-TIT1.
             05  FILLER        PIC  X(040) VALUE
                 "ÅñÅñÅñÅ@ÉAÉjÉÅÉ^ÉCÉgÉãàÍóóï\Å@ÅñÅñÅñÅ@Å@".
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
             05  FILLER        PIC  X(040) VALUE "TEST31-T".
             05  WK-TIT1-A4T-1 PIC  X(076) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05  FILLER        PIC  X(070) VALUE "TEST31-Y".
             05  WK-TIT1-A4Y-1 PIC  X(076) VALUE SPACE.

      *    *** àÛç¸ÉyÅ[ÉWê›íËÇ≈ÉyÅ[ÉWÇ†ÇΩÇËÇÃçsï∂éöêîÅAècçsêîÇ™
      *    *** ïœÇÌÇÈÇÃÇ≈ÅAà»â∫ê›íËÇ≈àÛç¸Ç∑ÇÈ

      *    *** àÛç¸ÉyÅ[ÉWê›íË
      *    *** îºäpÉtÉHÉìÉgÅ@ÇlÇrÉSÉVÉbÉN
      *    *** ëSäpÉtÉHÉìÉgÅ@ÇlÇrÉSÉVÉbÉN
      *    *** ÉtÉHÉìÉgçÇÅ@ÇQÇUÇçÇçÅAÇVÅDÇRÇêÇî
      *    *** çsëóÇËÇOÅì
      *    *** ó]îíÅ@è„ÇPÇOÅAâ∫ÇPÇOÅAâEÇPÇOÅAç∂ÇPÇOÇçÇç
      *    *** 
      *    *** çsÇ†ÇΩÇËÇÃï∂éöêîÅFâ°éûÇQÇPÇR  ècéûÇPÇSÇU
      *    *** ècï˚å¸ÇÃçsêîÅF    â°éûÇVÇP    ècéûÇPÇOÇS

      *    *** MAX=146,A4ècóp
      *    *** 35*4=140
           03  WK-MID1-A4T.
             05  FILLER        PIC  X(140) VALUE ALL
                 " SEQ    îN ãG É^ÉCÉgÉã             ".
      *           1234**7890*23*56789012345678901234*

      *    *** MAX=213,A4â°óp
      *    *** 35*6=210
           03  WK-MID1-A4Y.
             05  FILLER        PIC  X(210) VALUE ALL
                 " SEQ    îN ãG É^ÉCÉgÉã             ".

      *    *** âÊñ çÄñ⁄
           03  WK-KEI1         PIC  X(002) VALUE "Ñü".
           03  WK-KEI2         PIC  X(002) VALUE "Ñ†".
           03  WK-KEI3         PIC  X(002) VALUE "Ñ°".
           03  WK-KEI4         PIC  X(002) VALUE "Ñ¢".
           03  WK-KEI5         PIC  X(002) VALUE "Ñ£".
           03  WK-KEI6         PIC  X(002) VALUE "Ñ§".
           03  WK-KEI7         PIC  X(002) VALUE "Ñ•".
           03  WK-KEI8         PIC  X(002) VALUE "Ñ¶".
           03  WK-KEI9         PIC  X(002) VALUE "Ñß".
           03  WK-KEI10        PIC  X(002) VALUE "Ñ®".
           03  WK-KEI11        PIC  X(002) VALUE "Ñ©".

           03  WK-KEI1-A4T.
             05  FILLER        OCCURS 4.
               07  FILLER      PIC  X(034) VALUE ALL "Ñü".
               07  FILLER      PIC  X(001) VALUE SPACE.

           03  WK-KEI1-A4Y.
             05  FILLER        OCCURS 6.
               07  FILLER      PIC  X(034) VALUE ALL "Ñü".
               07  FILLER      PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 50
                               PIC  X(213) VALUE SPACE.

       01  CNS-AREA.
      *    *** PX ÇÃàÛéöà íu
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 7.
           03  CNS-P3          BINARY-LONG SYNC VALUE 12.
           03  CNS-P4          BINARY-LONG SYNC VALUE 15.

      *    *** PX ÇÃåÖêî
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 20.

      *    *** P1-PX ÇÃàÛéöçáåvåÖêîÅ@ÉXÉyÅ[ÉXä‹Çﬁ
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
           03  PX              BINARY-LONG SYNC VALUE ZERO

      *    *** çsÇ†ÇΩÇËÇÃï∂éöêîÅFâ°éûÇQÇPÇR  ècéûÇPÇSÇU
      *    *** ècï˚å¸ÇÃçsêîÅF    â°éûÇVÇP    ècéûÇPÇOÇS

           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
      *    *** "1" = A4èc,
      *    *** "0" = A4â°
           03  SW-A4TATE       PIC  X(001) VALUE "1".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU      S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PRINT TABLE SET
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** AT END éûèàóù
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

      *    *** 5ÇÕíËã`ÇÃ*ÇÃêîÅAàÛéöÇµÇ»Ç¢èä
           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + 5

           IF      SW-A4TATE   =       "1"
      *    *** äÑéZÇ≈è§ÇãÅÇﬂÅAè§ * CNS-L-SIZEÇãÅÇﬂÇÈ
                   COMPUTE C3 = 146 / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
      *    *** 50 = ( 104 - 4 ) / 2
      *    *** - 4 ÇÕÉwÉbÉ_Å[ÅA/ 2 ÇÕñæç◊çséüÇÃårê¸ï™èúÇ≠
                   MOVE    50          TO      R
           ELSE
                   COMPUTE C3 = 213 / CNS-L-SIZE
                   COMPUTE C = C3   * CNS-L-SIZE
      *    *** 33 = ( 71 - 5 ) / 2
      *    *** - 5 ÇÕÉwÉbÉ_Å[ÅA/ 2 ÇÕñæç◊çséüÇÃårê¸ï™èúÇ≠
                   MOVE    33          TO      R
           END-IF

           MOVE    CNS-P1      TO      P1
           MOVE    CNS-P2      TO      P2
           MOVE    CNS-P3      TO      P3
           MOVE    CNS-P4      TO      P4
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT
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
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *    *** 19,1 Ç©ÇÁäøéöénÇ‹ÇÈéûÅAÉZÉbÉgÇµÇ»Ç¢
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      M 

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 19
      *    *** äøéöÇ©ÅHàÍïîäøéöÇ≈Ç»Ç¢ïîï™Ç†ÇËÅA
      *    *** å„Ç©ÇÁéwíËÇ≥ÇÍÇΩïîï™Ç‡éwíËÇµÇƒÇ»Ç¢
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

           CALL    "C$JUSTIFY" USING   WK-SEQNO "R"
           MOVE    WK-SEQNO    TO      PR-LINE (J) (P1:CNS-P1-L)
           MOVE    WK-YYYY     TO      PR-LINE (J) (P2:CNS-P2-L)
           MOVE    WK-KISETU   TO      PR-LINE (J) (P3:CNS-P3-L)
           MOVE    WK-TITLE    TO      PR-LINE (J) (P4:CNS-P4-L)

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
                           MOVE    1           TO      C2
                   ELSE
                           ADD     CNS-L-SIZE  TO      P1
                           ADD     CNS-L-SIZE  TO      P2
                           ADD     CNS-L-SIZE  TO      P3
                           ADD     CNS-L-SIZE  TO      P4
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

                   MOVE    SPACE       TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4T TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4T TO      POT1-REC
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

                   MOVE    SPACE       TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-MID1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   MOVE    WK-KEI1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

           END-IF

           PERFORM VARYING K FROM 1 BY 1
                   UNTIL   K > R
                   MOVE    PR-LINE(K)  TO      POT1-REC
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX

                   IF      SW-A4TATE   =       "1"
                           MOVE    WK-KEI1-A4T TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX

                   ELSE
                           MOVE    WK-KEI1-A4Y TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX

                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** AT END éûèàóù
      *    *** 0åèÇ≈Ç‡ÅAÇ`ÇsÅ@ÇdÇmÇcéûÅAåèêîèoóÕ
       S120-10.

           COMPUTE PX = P1 + 6
           MOVE    "*** "      TO      PR-LINE (J) (PX:4)
           MOVE    WK-PIN1-CNT TO      WK-CNT
           COMPUTE PX = PX + 4
           MOVE    WK-CNT      TO      PR-LINE (J) (PX:4)
           COMPUTE PX = PX + 4
           MOVE    " åè ***"   TO      PR-LINE (J) (PX:7)

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
           DISPLAY WK-PGM-NAME " PIN1 π›Ω≥ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 π›Ω≥ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 Õﬂ∞ºﬁ= " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
