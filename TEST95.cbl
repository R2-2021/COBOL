      *    *** FILE NAME BYTE CHECK
      *    *** JOB = C.TEST95.bat

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST95.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *REPOSITORY.
      *     FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ダウンロードにある、ファイル名レングスチェック
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST95  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST95.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST95.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-DATA         PIC  X(1000) VALUE SPACE.
           03  WK-BYTE         PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-A            PIC  9(002) VALUE 11.
           03  WK-B            PIC  9(002) VALUE 22.
           03  WK-C            PIC  9(002) VALUE 33.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-MP4          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL PIN1-REC (1:1) IS NUMERIC
                     AND ( PIN1-REC (37:11) NOT = "videos2.lnk"
                     AND   PIN1-REC (37:1)  NOT = "."
                     AND   PIN1-REC (37:2)  NOT = ".." )
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
                   DISPLAY WK-PIN1-CNT
           END-PERFORM


      *    *** SHORI
           PERFORM S100-10     THRU    S100-EX

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

           MOVE    "O"         TO      WFD-ID
           MOVE    "UTF8"      TO      WFD-KANJI
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** PIN1-F READ
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

      *    *** SHORI
       S100-10.

      *     DISPLAY "FILE NAME INPUT"
      *     ACCEPT WK-DATA

           MOVE    PIN1-REC    TO      WK-DATA
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL SW-MP4 = "Y"
                      OR I > 1000
                   IF      WK-DATA (I + 1:4) = ".mp4"
                           MOVE    "Y"         TO      SW-MP4
                           MOVE    I           TO      WK-BYTE
                   END-IF
           END-PERFORM

           DISPLAY "FILE NAME BYTE = " WK-BYTE
           MOVE    "P"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA
           ADD     WK-A WK-B   TO      WK-C
           add     WK-C        TO      WK-C                    
           DISPLAY WK-C
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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
