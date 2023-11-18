      *    *** HACKADOLL 画像ファイル名抽出 サブルーチン
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE06.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** この指定が無いとコンパイルエラーになる
      *    *** MOVE LENGTH(LI-BUF2) TO WK-BUF-LEN2 の所
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** HACKADOLL DIR データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE06".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "DECODE06.PIN1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1144.
             05  TBL01-FILE    PIC  X(012) VALUE SPACE.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "N".

       LINKAGE                 SECTION.

       01  LI-AREA.
      *    *** ID=O (OPEN)
      *    *** ID=S (SEARCH)
      *    *** ID=C (CLOSE)
           03  LI-ID           PIC  X(001).
           03  LI-NUM          PIC  9(004).
           03  LI-FILE         PIC  X(012).

       PROCEDURE   DIVISION    USING   LI-AREA
           .
       M100-10.

           EVALUATE LI-ID

               WHEN "O"
      *    *** OPEN
                   PERFORM S010-10     THRU    S010-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** TBL SET PIN1
                           PERFORM S030-10     THRU    S030-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN "S"
      *    *** SEARCH
                   MOVE    TBL01-FILE (LI-NUM) TO LI-FILE

               WHEN "C"
      *    *** CLOSE
                   PERFORM S900-10     THRU    S900-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LI-ID ERROR LI=ID=" LI-ID
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

      *     MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
      *     MOVE    "S"         TO      WDT-DATE-TIME-ID
      *     CALL    "DATETIME"  USING   WDT-DATETIME-AREA


           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
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

      *    *** TBL SET PIN1
       S030-10.

      *    *** SJIS DATA
           ADD     1           TO      I
           IF      I           >       1144
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    PIN1-REC (37:12) TO TBL01-FILE (I)
      *
      *    *** 圧縮画像ファイル使用するようにしたので、全部ｊｐｇになった
      *    *** YouTube声優\Hackadoll\ の画像ファイル使う
           MOVE    ".jpg"      TO      TBL01-FILE (I) (9:4)
           .
       S030-EX.
           EXIT.

      *    *** 
       S100-10.

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

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           .
       S900-EX.
           EXIT.
