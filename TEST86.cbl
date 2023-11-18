      *    *** BAT ÇÃé¿çs
      *    *** TEST85.POT2 (XCOPY.BAT)

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST86.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST85Ç≈çÏê¨ÇµÇΩÇwÇbÇnÇoÇx ÇaÇ`ÇsÉtÉ@ÉCÉã
      *    *** CALL "SYSTEM" Ç≈BATé¿çsÇ∑ÇÈÇ∆ÅAUSING ÇÃì‡óeÇ™ÅA
      *    *** ÇrÇiÇhÇrèoÇ»Ç¢Ç∆ÉGÉâÅ[Ç…Ç»ÇÈ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ñ¢égóp
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST86  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST85.POT2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST86.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** èâä˙íl MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** èâä˙íl HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

      *    *** UTF8 Ç…ïœçX
           03  WK-CHCP         PIC  X(010) VALUE "CHCP 65001".
           03  WK-ENTER        PIC  X(001) VALUE SPACE.
      *    *** 0.5 SEC = 500,000,000 NANO SEC
           03  WK-NANOSEC      PIC  9(011) VALUE 2000000000.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** BAT é¿çs
                   PERFORM S100-10     THRU    S100-EX
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

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
 
      *    *** ÉRÉ}ÉìÉh ÇtÇsÇeÇWÇ…ïœçX
           CALL    "SYSTEM"    USING   WK-CHCP
           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT CHCP SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
               IF      WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME
                           " PIN1-F READ ERROR STATUS=" WK-PIN1-STATUS
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** BAT é¿çs
       S100-10.

      *    *** PIN1-REC äøéöä‹ÇﬁéûÅAäøéöÇ™ÇtÇsÇeÇWÇæÇ∆,
      *    *** RETURN-CODE=1 Ç…Ç»Ç¡ÇƒÇµÇ‹Ç§
      *    *** äøéöÇÇrÇiÇhÇrÇ…ïœçXÇµÇΩÇÁÅARETURN-CODE=ZEROÇ…Ç»Ç¡ÇΩ

      *     GO  TO  S100-20

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    WK-PIN1-LEN TO      WDE05-BUF1-LEN
      *    *** WDE05-BUF2-LEN ïKÇ∏ç≈ëÂí∑ÇÉZÉbÉgÇ∑ÇÈ
           MOVE    1000        TO      WDE05-BUF2-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

      *    *** éûä‘íxÇÁÇπÇÈ
      *     CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

      *    *** SJIS
           CALL    "SYSTEM"    USING   POT1-REC
           .
       S100-20.

      *    *** UTF8
      *     CALL    "SYSTEM"    USING   PIN1-REC

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT XCOPY SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                           " WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " PIN1-REC=" PIN1-REC (1:200)
                   DISPLAY " POT1-REC=" POT1-REC (1:200)
                   DISPLAY WK-PGM-NAME " Any Enter "
                   ACCEPT  WK-ENTER
                   STOP    RUN
           END-IF
           .
       S100-EX.
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

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 åèêî = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 åèêî = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
