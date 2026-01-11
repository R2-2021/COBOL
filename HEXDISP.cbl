      *    *** YouTube 動画サムネイル、自動付加

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST104.

       DATA                    DIVISION.
       FILE                    SECTION.

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "HEXDISP".

           03  WK-ACCEPT       PIC  X(100) VALUE SPACE.
           03  WK-ACCEPT-LEN   BINARY-LONG SYNC VALUE 100.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

       01  WK-BUF2.
           03  WK-BUF2-L-TBL.
             05  WK-BUF2-L     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-R-TBL.
             05  WK-BUF2-R     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-LR-TBL.
             05  WK-BUF2-LR-TBL2 OCCURS 65536.
               07  WK-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WK-BUF2-R2  PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY
           PERFORM S010-10     THRU    S010-EX

      *    *** HEX DISPLAY
           PERFORM S100-10     THRU    S100-EX

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-ACCEPT
                                       WK-ACCEPT

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-ACCEPT

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** 画像サムネイル付加
       S100-10.

           DISPLAY WK-PGM-NAME " 100文字まで"
           ACCEPT  WK-ACCEPT

           CALL    "DECODE03" USING    WK-ACCEPT
                                       WK-ACCEPT-LEN
                                       WK-BUF2

           DISPLAY WK-BUF2-L-TBL (1:100)
           DISPLAY WK-BUF2-R-TBL (1:100)

          .
       S100-EX.
           EXIT.

      *    *** END DISPLAY
       S900-10.

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-ACCEPT
                                       WK-ACCEPT

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-ACCEPT

           DISPLAY WK-PGM-NAME " END"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
