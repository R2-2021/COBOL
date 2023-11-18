      *    *** Qosmio dir から html データ作成

      *    *** C.TEST74

      *    *** TEST74
      *    ***   |
      *    *** TEST53
      *    ***   |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST74.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** アニメhtml CSV データ SORT 済
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アニメhtml CSV データ #NNN 付加
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST74  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST74_China.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST74.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** HEAD 
           03  WK-HEAD.
             05                PIC  X(002) VALUE "%".
             05                PIC  X(001) VALUE SPACE.
             05                PIC  X(011) VALUE
                 "Qosmio_G50,".

      *    *** ID=ミュージシャン名（ＵＴＦ８）
           03  WK-NO.
             05                PIC  X(012) VALUE 
      *    *** ジャパリ
                 X"E382B8E383A3E38391E383AA".
             05  WK-NO-ID      PIC  X(100) VALUE SPACE.

           03  WK-OLD-KEY.
             05  WK-OLD-ID     PIC  X(100) VALUE LOW-VALUE.
           03  WK-NEW-KEY.
             05  WK-NEW-ID     PIC  X(100) VALUE LOW-VALUE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** WRITE HEAD
           PERFORM S120-10     THRU    S120-EX

      *    *** PIN1
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      WK-OLD-KEY  NOT =   WK-NEW-KEY

      *    *** WRITE POT1 WK-NO
                           PERFORM S110-10     THRU    S110-EX
                   END-IF

      *    *** WRITE POT1
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    WK-NEW-KEY  TO      WK-OLD-KEY

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
      *    *** KEY SET,アーティスト名抽出
                   PERFORM S021-10     THRU    S021-EX

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "UTF8"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               WK-NEW-KEY
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.
           .

      *    *** KEY SET,アーティスト名抽出
       S021-10.

           MOVE    "N"         TO      SW-SET
           MOVE    SPACE       TO      WK-NEW-KEY
           MOVE    ZERO        TO      I2

           PERFORM VARYING I FROM 37 BY 1
                   UNTIL I > WK-PIN1-LEN
                   IF      PIN1-REC (I:4) =    ".mp3" OR ".wma"
                       MOVE    ",   "      TO      PIN1-REC (I:4)
                       MOVE    WK-PIN1-LEN TO      I
                   END-IF

                   IF    ( PIN1-REC (I:1) =    SPACE OR "-" )
      *    *** －
                      OR ( PIN1-REC (I:3) =    X"EFBC8D" )
                       MOVE    "Y"         TO      SW-SET
                   END-IF

                   IF      SW-SET      =       "N"
                       ADD     1           TO      I2
                       IF      I2          >       100
                           DISPLAY WK-PGM-NAME " ID SET OVER I2=" I2
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           STOP    RUN
                       END-IF
                       MOVE    PIN1-REC (I:1) TO   WK-NEW-KEY (I2:1)
                   END-IF
           END-PERFORM
           .
       S021-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    PIN1-REC (37:) TO   POT1-REC
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

      *    *** WRITE POT1 WK-NO 
       S110-10.

           MOVE    WK-NEW-KEY  TO      WK-NO-ID

           WRITE   POT1-REC    FROM    WK-NO
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT


           MOVE    WK-NEW-KEY  TO      POT1-REC
           MOVE    ","         TO      POT1-REC (I2 + 1:1)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 
       S120-10.

           WRITE   POT1-REC    FROM    WK-HEAD
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           ADD     1           TO      WK-POT1-CNT

           .
       S120-EX.
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
