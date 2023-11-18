      *    *** ÉJÉìÉ}ÅAÉ\Å[ÉgÉLÅ[(êîéöÅAÉJÉiÅAÇ©Ç»ÅAäøéöì«Ç›)ïtâ¡
      *    *** TEST70.PIN5 ÉfÅ[É^çÏê¨

      *    *** JOB=C.TEST108.bat

      *    *** TEST108.POT1 Ç… Ç”ÇËÇ™Ç»Çí«â¡Ç∑ÇÈ

      *    *** TEST108
      *    ***   |
      *    *** TEST109
      *    ***   |
      *    *** COBSORT COBSORT.T015.PRM1
      *    ***   |
      *    *** TEST70
      *    ***   |
      *    *** TEST53
      *    ***   |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST109.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ãå TEST70.PIN5 åüçıÉfÅ[É^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ASCII=>UTF8 ïœä∑ÉfÅ[É^
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** äøéöì«Ç›ÉfÅ[É^
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** åüçıÉfÅ[É^
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** åüçıÉfÅ[É^ äøéöÉTÅ[É`ÉGÉâÅ[ï™
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  FILLER          PIC  X(100).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD.
       01  PIN3-REC.
           03  FILLER          PIC  X(120).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST109 ".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST108.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST61.POT1".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST109.PIN3".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST109.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST109.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-ITEM1   PIC  X(100) VALUE SPACE.
           03  WK-PIN1-ITEM2   PIC  X(100) VALUE SPACE.

           03  WK-PIN1-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-ITEM2-LEN BINARY-LONG SYNC VALUE ZERO.

           03  WK-TBL03-IDX-MAX BINARY-LONG SYNC VALUE ZERO.
           03  WK-SU           PIC  9(009) VALUE ZERO.

      *    *** ASCII
           03  WK-ASCII        PIC  X(001) VALUE SPACE.
      *    *** UTF8
           03  WK-KATAKANA     PIC  X(003) VALUE SPACE.
      *    *** UTF8
           03  WK-HIRAGANA     PIC  X(003) VALUE SPACE.

           03  WK-ASCII-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-KATA-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-HIRA-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-KANJI        PIC  X(060) VALUE SPACE.
           03  WK-YOMI         PIC  X(060) VALUE SPACE.
           03  WK-ITEM         PIC  X(050) VALUE SPACE.

           03  WK-KANJI-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-YOMI-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-HEX2.
               07              PIC  X(001) VALUE LOW-VALUE.
               07  WK-HEX3     PIC  X(002) VALUE LOW-VALUE.
           03  WK-HEX-SU       REDEFINES WK-HEX
                               PIC  9(009) COMP-X.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.

      *    *** PIN2 óp
       01  TBL02-AREA-G.
           03  TBL02-AREA      OCCURS 158
                               ASCENDING KEY IS TBL02-ASCII
                               INDEXED BY TBL02-IDX.
             05  TBL02-ASCII   PIC  X(003) VALUE HIGH-VALUE.
             05  TBL02-KATAKANA PIC X(003) VALUE SPACE.
             05  TBL02-HIRAGANA PIC X(003) VALUE SPACE.

      *    *** PIN3 óp
       01  TBL03-AREA-G.
           03  TBL03-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL03-KANJI
                               INDEXED BY TBL03-IDX.
      *    *** äøéöÇlÇ`ÇwÇSï∂éö
      *    *** ÇPï∂éöÇÃéûÅ@äøéöÇPï∂éöÅ{ÇgÇhÇfÇgÅ|ÇuÇ`ÇkÇtÇdÇ≈ÉZÉbÉg
      *    *** ÇQÅ|ÇRÇ‡ìØÇ∂
      *    *** ÇrÇdÇ`ÇqÇbÇgÇÕÇSï∂éöÇêÊÇ…Ç∑ÇÈÇΩÇﬂ
             05  TBL03-KANJI   PIC  X(060) VALUE HIGH-VALUE.
             05  TBL03-KANJI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL03-YOMI    PIC  X(060) VALUE SPACE
             05  TBL03-YOMI-LEN BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX



           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL02 SET
                   PERFORM S031-10     THRU    S031-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** TBL03 SET
                   PERFORM S041-10     THRU    S041-EX
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM



      *    *** TBL02 SORT SORTèáÇ…Ç»Ç¡ÇƒÇ¢ÇÈÇÃÇ≈ÅAÉ\Å[ÉgÇµÇ»Ç¢
      *     SORT    TBL02-AREA
      *             ASCENDING KEY TBL02-ASCII

      *    *** TBL03 SORT
           SORT    TBL03-AREA
                   ASCENDING KEY TBL03-KANJI



           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   MOVE    PIN1-REC    TO      POT1-REC
                   ADD     1 WK-PIN1-LEN GIVING P
                   MOVE    ","         TO      POT1-REC (P:1)
                   ADD     1           TO      P
                   MOVE    1           TO      P2

      *    *** Åg(E2809C) Åu(E3808C) Åy(E38090) Åw(E3808E)
      *    *** ÇæÇ¡ÇΩÇÁÅAéüÇÃï∂éöêÊì™Ç…Ç∑ÇÈ
                   IF      WK-PIN1-ITEM1 (1:3) =       X"E2809C"
                          OR X"E3808C" OR X"E38090" OR X"E3808E"
      *                     MOVE    WK-PIN1-ITEM1 (4:50) TO WK-ITEM
      *                     MOVE    WK-ITEM TO WK-PIN1-ITEM1 (1:50)
                            MOVE    WK-PIN1-ITEM1 (4:50)
                                            TO WK-PIN1-ITEM1 (1:50)

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-ITEM1 (1:20)
                   END-IF

      *    *** ÇPï∂éöñ⁄Å@äøéö
      *    *** Ç®åZÇ∆Ç©ÅAÇPï∂éöñ⁄Ç™Ç–ÇÁÇ™Ç»Ç≈ÅAäøéöçÄñ⁄óLÇÈÇΩÇﬂÅA
      *    *** ÇgÇhÇsÇÃéûÇÕÅAäøéöÉeÅ[ÉuÉãÉTÅ[É`Ç∑ÇÈÇÊÇ§Ç…Ç∑ÇÈ
                   MOVE    "N"         TO      SW-SEARCH
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-TBL03-IDX-MAX
                              OR SW-SEARCH = "Y"
                           MOVE    TBL03-KANJI-LEN (I) TO L
                           IF      WK-PIN1-ITEM1 (1:L) 
                                 = TBL03-KANJI (I) (1:L)
                               MOVE    "Y"         TO      SW-SEARCH
                               MOVE    TBL03-YOMI-LEN (I) TO L
                               MOVE    TBL03-YOMI (I) TO  POT1-REC (P:L)
                           END-IF
                   END-PERFORM

                   EVALUATE TRUE

                       WHEN PIN1-REC (1:1) = "#"
      *    *** # ÇÕ ÉXÉLÉbÉv
                           CONTINUE

      *    *** àÍï∂éöñ⁄îªíË

      *    *** 0-9,A-Z,a-z
      *    *** UTF8 ±-›ÅiîºäpÅjÇÕÇRÉoÉCÉgÇ»ÇÃÇ≈ÅA0-z(SPACE-•)Ç‹Ç≈Ç…Ç∑ÇÈ
                       WHEN ( WK-PIN1-ITEM1 (1:1) >= SPACE AND <= "•" )
      *    *** WRITE POT1
                           PERFORM S100-10     THRU    S100-EX

                       WHEN
      *    *** ÅIÅ|ÇXÅAÇ†Å[ÇÒÅAÅFÅ[ÇyÅAÅEÅAÅ[ÅAÅ`ÅAÅfÅAÅôÅA•
                (( WK-PIN1-ITEM1 (1:3) >= X"EFBC81" AND <= X"EFBC99" )
              OR ( WK-PIN1-ITEM1 (1:3) >= X"E38181" AND <= X"E38296" )
              OR ( WK-PIN1-ITEM1 (1:3) >= X"EFBC9A" AND <= X"EFBCBA" )
              OR ( WK-PIN1-ITEM1 (1:3)  = X"E383BB" OR X"E383BC")
              OR ( WK-PIN1-ITEM1 (1:3)  = X"E28099" OR X"EFBD9E" 
                          OR X"E29886" OR X"EFBDA5" OR X"EFBDA5" ))
              AND  SW-SEARCH = "N"
                           PERFORM S130-10     THRU    S130-EX

      *    *** ÇÅÅ|Çö
                       WHEN ( WK-PIN1-ITEM1 (1:3) >= X"EFBD81"
                                              AND <= X"EFBD9A" )
                         AND  SW-SEARCH = "N"
      *    *** WRITE POT1
                           PERFORM S140-10     THRU    S140-EX

      *    *** ¶Å|ﬂÅiUTF8 îºäpÅjÇÕÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ
                       WHEN ( WK-PIN1-ITEM1 (1:3) >= X"EFBDA6"
                                              AND <= X"EFBE9F" )
                         AND  SW-SEARCH = "N"
      *    *** WRITE POT1
                           PERFORM S150-10     THRU    S150-EX

      *    *** É@-Éñ,ÉJÉ^ÉJÉiÇ¶ÇÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ
                       WHEN ( WK-PIN1-ITEM1 (1:3) >= X"E382A1"
                                              AND <= X"E383B6" )
                         AND  SW-SEARCH = "N"
      *    *** WRITE POT1
                           PERFORM S160-10     THRU    S160-EX

      *    *** äøéöÅAÇ†-ÇÒÅ@ä‹Çﬁ
                       WHEN WK-PIN1-ITEM1 (1:1) >= X"E3" AND <= X"E9"
      *    *** WRITE POT1
                           PERFORM S170-10     THRU    S170-EX

                       WHEN OTHER
      *    *** ERROR ?
                           DISPLAY WK-PGM-NAME 
                               " ÇPï∂éöñ⁄ PIN1 ¿≤ƒŸ ¥◊∞ FILEDUMP Ç…èoóÕ"
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                                  " WK-PIN1-ITEM1=" WK-PIN1-ITEM1 (1:10)
                           MOVE    "X"         TO      WFD-ID
                           MOVE    WK-PIN1-CNT TO      WFD-SEQ
                           MOVE    "PIN1-ITEM1" TO     WFD-ITEM
                           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-PIN1-ITEM1
                           STOP    RUN
                   END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

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

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           SET     TBL02-IDX   TO      1
           SET     TBL03-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-PIN1-ITEM1
                                       WK-PIN1-ITEM2
           MOVE    ZERO        TO      WK-PIN1-ITEM1-LEN
                                       WK-PIN1-ITEM2-LEN
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-PIN1-ITEM1 COUNT WK-PIN1-ITEM1-LEN
                           WK-PIN1-ITEM2 COUNT WK-PIN1-ITEM2-LEN
      *             CALL "COBDUMP" USING  WK-PIN1-ITEM1 (1:6)
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-ASCII
                                       WK-KATAKANA
                                       WK-HIRAGANA
           MOVE    ZERO        TO      WK-ASCII-LEN
                                       WK-KATA-LEN
                                       WK-HIRA-LEN
           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
      *             UNSTRING PIN2-REC
      *                     DELIMITED BY ","
      *                     INTO
      *                    WK-ASCII    COUNT WK-ASCII-LEN
      *                     WK-KATAKANA COUNT WK-KATA-LEN
      *                     WK-HIRAGANA COUNT WK-HIRA-LEN
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** TBL02 SET
       S031-10.

           IF      TBL02-IDX   >       158
                   DISPLAY WK-PGM-NAME
                           " TBL02-TBL OVER TBL02-IDX=" TBL02-IDX
                   STOP    RUN
           END-IF

      *    *** ÉfÅ[É^Ç…ÅCóLÇÈÇΩÇﬂ
           IF      PIN2-REC (2:1) =   ","
               MOVE    PIN2-REC (1:1) TO   TBL02-ASCII    (TBL02-IDX)
               MOVE    PIN2-REC (3:3) TO   TBL02-KATAKANA (TBL02-IDX)
               MOVE    PIN2-REC (7:3) TO   TBL02-HIRAGANA (TBL02-IDX)
           ELSE
               MOVE    PIN2-REC (1:3) TO   TBL02-ASCII    (TBL02-IDX)
               MOVE    PIN2-REC (5:3) TO   TBL02-KATAKANA (TBL02-IDX)
               MOVE    PIN2-REC (9:3) TO   TBL02-HIRAGANA (TBL02-IDX)
           END-IF

           SET     TBL02-IDX   UP  BY  1
           .
       S031-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           MOVE    SPACE       TO      WK-KANJI
                                       WK-YOMI
           MOVE    ZERO        TO      WK-KANJI-LEN
                                       WK-YOMI-LEN
           READ    PIN3-F

           IF      WK-PIN3-STATUS =    ZERO
                   ADD     1           TO      WK-PIN3-CNT
                   UNSTRING PIN3-REC
                           DELIMITED BY ","
                           INTO
                           WK-KANJI    COUNT WK-KANJI-LEN
                           WK-YOMI     COUNT WK-YOMI-LEN
           ELSE
               IF  WK-PIN3-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** TBL03 SET
       S041-10.

           IF      TBL03-IDX   >       1000
                   DISPLAY WK-PGM-NAME
                           " TBL03-TBL OVER TBL03-IDX=" TBL03-IDX
                   STOP    RUN
           END-IF

           MOVE    HIGH-VALUE  TO      TBL03-KANJI     (TBL03-IDX)
           MOVE    WK-KANJI-LEN TO     TBL03-KANJI-LEN (TBL03-IDX)
                                       L
           MOVE    WK-KANJI    TO      TBL03-KANJI     (TBL03-IDX) (1:L)
           MOVE    WK-YOMI     TO      TBL03-YOMI      (TBL03-IDX)
           MOVE    WK-YOMI-LEN TO      TBL03-YOMI-LEN  (TBL03-IDX)
           MOVE    TBL03-IDX   TO      WK-TBL03-IDX-MAX

           SET     TBL03-IDX   UP  BY  1
           .
       S041-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

      *    *** ÇPï∂éöñ⁄ WK-PIN1-ITEM1 (P2:1) >= "0" AND <= "z"
      *    ***     OR ( WK-PIN1-ITEM1 (P2:1) = "."  OR SPACE)
           PERFORM S110-10     THRU    S110-EX

      *    *** ÇQï∂éöñ⁄
           PERFORM S120-10     THRU    S120-EX

      *    *** ÇRï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

      *    *** ÇSï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** ÇPï∂éöñ⁄ÅCÇQï∂éöñ⁄ WK-PIN1-ITEM1 (P2:1) >= "0" AND <= "z"
       S110-10.

      *    *** TBL02 ASCII SEARCH
           SEARCH  ALL TBL02-AREA
               AT END
      *    *** UTF8 SPACE
                   MOVE    X"E38080"   TO      POT1-REC (P:3)

              WHEN TBL02-ASCII (TBL02-IDX) (1:1) =  WK-PIN1-ITEM1 (P2:1)
                   MOVE    TBL02-HIRAGANA (TBL02-IDX) TO POT1-REC (P:3)
           END-SEARCH

           ADD     3           TO      P
           ADD     1           TO      P2
           .
       S110-EX.
           EXIT.

      *    *** 2,3,4 ÉoÉCÉgñ⁄
       S120-10.

           EVALUATE TRUE

      *    *** Å~ÇÕÇªÇÃÇ‹Ç‹èoóÕ
               WHEN ( WK-PIN1-ITEM1 (P2:2) = X"C397" )

                   ADD     2           TO      P
                   ADD     2           TO      P2

               WHEN ( WK-PIN1-ITEM1 (P2:1) >= SPACE AND <= "•" )
      *    *** 2ÉoÉCÉgñ⁄ WK-PIN1-ITEM1 (2:1) >= "0" AND <= "z" (SPACE-•)
                   PERFORM S110-10     THRU    S110-EX
                   MOVE    ","         TO      POT1-REC (P:1)

               WHEN 

      *    *** ÅIÅ|ÇXÅAÇ†Å[ÇÒÅAÅFÅ[ÇyÅAÅEÅAÅ[ÅAÅ`ÅAÅfÅAÅôÅA•ÇÕÇªÇÃÇ‹Ç‹èoóÕ
                 ( WK-PIN1-ITEM1 (P2:3) >= X"EFBC81" AND <= X"EFBC99" )
              OR ( WK-PIN1-ITEM1 (P2:3) >= X"E38181" AND <= X"E38296" )
              OR ( WK-PIN1-ITEM1 (P2:3) >= X"EFBC9A" AND <= X"EFBCBA" )
              OR ( WK-PIN1-ITEM1 (P2:3)  = X"E383BB" OR X"E383BC" )
              OR ( WK-PIN1-ITEM1 (P2:3)  = X"E28099" OR X"EFBD9E" 
                                        OR X"E29886" OR X"EFBDA5" 
      *    *** çïêØ
                                        OR X"E29885"
      *    *** ñ≥å¿ëÂ
                                        OR X"E2889E"
                                        )

      *             MOVE    WK-PIN1-ITEM1 (3:2) TO WK-HEX3
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *             DISPLAY WK-HEX-SU
                   MOVE    WK-PIN1-ITEM1 (P2:3) TO POT1-REC (P:3)

                   ADD     3           TO      P
                   ADD     3           TO      P2

      *    *** ÇÅÅ|ÇöÇÕÇ`Å[ÇyÇ…ïœä∑ÇµÇƒèoóÕ
               WHEN WK-PIN1-ITEM1 (P2:3) >= X"EFBD81" AND <= X"EFBD9A" 
      *             MOVE    WK-PIN1-ITEM1 (3:2) TO WK-HEX3
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *             DISPLAY WK-HEX-SU
      *             MOVE    WK-PIN1-ITEM1 (2:3) TO POT1-REC (P:3)

                   MOVE    WK-PIN1-ITEM1 (P2:3) TO   WK-HEX2
                   COMPUTE WK-HEX-SU = WK-HEX-SU - 224
                   MOVE    WK-HEX2     TO      POT1-REC (P:3)

                   ADD     3           TO      P
                   ADD     3           TO      P2

      *    *** ¶Å|ﬂÅiUTF8 îºäpÅjÇÕÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ
               WHEN WK-PIN1-ITEM1 (P2:3) >= X"EFBDA6" AND <= X"EFBE9F" 

      *    *** TBL02 ASCII SEARCH
                   SEARCH  ALL TBL02-AREA
                           AT END
      *    *** UTF8 SPACE
                           MOVE    X"E38080"   TO      POT1-REC (P:3)

                   WHEN TBL02-ASCII (TBL02-IDX) =  WK-PIN1-ITEM1 (P2:3)
                           MOVE    TBL02-HIRAGANA (TBL02-IDX) 
                                               TO      POT1-REC (P:3)
                   END-SEARCH

                   ADD     3           TO      P
                   ADD     3           TO      P2

      *    *** É@-Éñ,ÉJÉ^ÉJÉiÇ¶ÇÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ
               WHEN WK-PIN1-ITEM1 (P2:3) >= X"E382A1" AND <= X"E383B6" 
      *             MOVE    WK-PIN1-ITEM1 (3:2) TO WK-HEX3
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *             DISPLAY WK-HEX-SU
      *             MOVE    WK-PIN1-ITEM1 (2:3) TO POT1-REC (P:3)

                   MOVE    WK-PIN1-ITEM1 (P2:3) TO   WK-HEX2
                   IF   ( WK-PIN1-ITEM1 (P2:3) >= X"E38380" 
                          AND <= X"E3838F" )
                     OR
                        ( WK-PIN1-ITEM1 (P2:3) >= X"E38390"
                          AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      POT1-REC (P:3)

                   ADD     3           TO      P
                   ADD     3           TO      P2

      *    *** äøéö
               WHEN WK-PIN1-ITEM1 (P2:1) >= X"E3" AND <= X"E9"

                   MOVE    "N"         TO      SW-SEARCH
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-TBL03-IDX-MAX
                                  OR SW-SEARCH = "Y"
                       MOVE    TBL03-KANJI-LEN (I) TO L
                       IF      WK-PIN1-ITEM1 (P2:L) 
                             = TBL03-KANJI (I) (1:L)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE    TBL03-YOMI-LEN (I) TO L
                           MOVE    TBL03-YOMI (I) TO   POT1-REC (P:L)
                       END-IF
                   END-PERFORM

                   IF      SW-SEARCH   =       "Y"
                           ADD     L           TO      P
                           MOVE    ","         TO      POT1-REC (P:1)
                   ELSE
                           MOVE    X"E38080"   TO      POT1-REC (P:3)

                           ADD     3           TO      P
                           MOVE    ","         TO      POT1-REC (P:1)
                   END-IF

               WHEN OTHER
      *    *** Ç±ÇÃÉGÉâÅ[Ç™èoÇΩÇÁÅAêÊì™ÇÃX"XXXXXX" Ç
      *    *** í«â¡Ç∑ÇÈÅ@çïêØÅAñ≥å¿ëÂìô
                   DISPLAY WK-PGM-NAME
                           " ÇQï∂éöñ⁄ PIN1 ¿≤ƒŸ ¥◊∞ FILEDUMP Ç…èoóÕ"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-PIN1-ITEM1=" WK-PIN1-ITEM1 (1:10)
                   MOVE    "X"         TO      WFD-ID
                   MOVE    WK-PIN1-CNT TO      WFD-SEQ
                   move    "UTF8"      TO      WFD-KANJI
                   MOVE    "PIN1-ITEM1" TO     WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-PIN1-ITEM1
                   STOP    RUN
           END-EVALUATE

           .
       S120-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S130-10.

      *    *** ÇPï∂éöñ⁄ÅAÇOÅ|ÇXÅAÇ†Å[ÇÒÅAÇ`Å[ÇyÅAÅ` ÇPï∂éöñ⁄ÇÕÇªÇÃÇ‹Ç‹èoóÕ

           MOVE    WK-PIN1-ITEM1 (1:3) TO POT1-REC (P:3)
           ADD     3           TO      P
           ADD     3           TO      P2

      *    *** ÇQï∂éöñ⁄
           PERFORM S120-10     THRU    S120-EX

      *    *** ÇRï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

      *    *** ÇSï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S130-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S140-10.

      *    *** ÇPï∂éöñ⁄ÅAÇÅÅ|ÇöÇÕÇ`Å[ÇyÇ…ïœä∑ÇµÇƒèoóÕ
           MOVE    WK-PIN1-ITEM1 (1:3) TO   WK-HEX2
           COMPUTE WK-HEX-SU = WK-HEX-SU - 224
           MOVE    WK-HEX2     TO      POT1-REC (P:3)
           ADD     3           TO      P
           ADD     3           TO      P2

      *    *** ÇQï∂éöñ⁄
           PERFORM S120-10     THRU    S120-EX

      *    *** ÇRï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

      *    *** ÇSï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S140-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S150-10.

      *    *** ÇPï∂éöñ⁄ ¶Å|ﬂÅiUTF8 îºäpÅjÇÕÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ

      *    *** TBL02 ASCII SEARCH
           SEARCH  ALL TBL02-AREA
                   AT END
      *    *** UTF8 SPACE
                   MOVE    X"E38080"   TO      POT1-REC (P:3)
              WHEN TBL02-ASCII (TBL02-IDX) =  WK-PIN1-ITEM1 (1:3)
                   MOVE    TBL02-HIRAGANA (TBL02-IDX) 
                                       TO      POT1-REC (P:3)
           END-SEARCH
           ADD     3           TO      P
           ADD     3           TO      P2

      *    *** ÇQï∂éöñ⁄
           PERFORM S120-10     THRU    S120-EX

      *    *** ÇRï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

      *    *** ÇSï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S150-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S160-10.
      *    *** É@-Éñ,ÉJÉ^ÉJÉiÇ¶ÇÇ†Å[ÇÒÇ…ïœä∑ÇµÇƒèoóÕ

           MOVE    WK-PIN1-ITEM1 (1:3) TO   WK-HEX2
           IF    ( WK-PIN1-ITEM1 (1:3) >= X"E38380" AND <= X"E3838F" )
              OR ( WK-PIN1-ITEM1 (1:3) >= X"E38390" AND <= X"E3839F" )
                   COMPUTE WK-HEX-SU = WK-HEX-SU - 480
           ELSE
                   COMPUTE WK-HEX-SU = WK-HEX-SU - 288
           END-IF
           MOVE    WK-HEX2     TO      POT1-REC (P:3)
           ADD     3           TO      P
           ADD     3           TO      P2

      *    *** ÇQï∂éöñ⁄
           PERFORM S120-10     THRU    S120-EX

      *    *** ÇRï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

      *    *** ÇSï∂éöñ⁄
           IF      SW-SEARCH   =       "N"
                   PERFORM S120-10     THRU    S120-EX
           END-IF

           MOVE    ","         TO      POT1-REC (P:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S160-EX.
           EXIT.

      *    *** SEARCH,WRITE POT1
       S170-10.

      *    *** ÇPï∂éöñ⁄Å@äøéö
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-TBL03-IDX-MAX
                      OR SW-SEARCH = "Y"
                   MOVE    TBL03-KANJI-LEN (I) TO L
                   IF      WK-PIN1-ITEM1 (P2:L)  = TBL03-KANJI (I) (1:L)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE    TBL03-YOMI-LEN (I) TO L
                           MOVE    TBL03-YOMI (I) TO   POT1-REC (P:L)
                   END-IF
           END-PERFORM

      *    *** äøéöÅ@ÇgÇhÇsÇµÇΩÇÁÅAèIóπ
           IF      SW-SEARCH   =       "Y"
                   ADD     L           TO      P
                   MOVE    ","         TO      POT1-REC (P:1)
           ELSE
                   MOVE    X"E38080E38080" TO  POT1-REC (P:6)
                   ADD     6           TO      P
                   MOVE    ","         TO      POT1-REC (P:1)

      *    *** äøéöÉTÅ[É`ÇgÇhÇsÇµÇ»Ç¢éûÅAPOT2-REC Ç…èoóÕ
                   WRITE   POT2-REC    FROM    PIN1-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT2-CNT
           END-IF

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S170-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 π›Ω≥ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 π›Ω≥ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 π›Ω≥ = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 π›Ω≥ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 π›Ω≥ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
