      *    *** EXPO_JAM_2018 一覧
      *    *** TEST53 入力データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST83.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** EXPO_JAM_2018 データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** EXPO_JAM_2018 データ TEST53 入力データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(2000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           05  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST83".

      *    *** SJIS
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "expo_jam_2018.csv".
      *    *** UTF8 （）等[]に変更した
      *    ***
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST83.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST83.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-TITLE1.
             05                PIC  X(015) VALUE "% expo_jam_2018".
      *    *** 一覧
             05                PIC  X(006) VALUE X"E4B880E8A6A7".
             05                PIC  X(001) VALUE ",".

           03  WK-TITLE2.
      *    *** ジャパリアイドル
             05                PIC  X(024) VALUE 
               X"E382B8E383A3E38391E383AAE382A2E382A4E38389E383AB".

           03  WK-I1           PIC  X(200) VALUE SPACE.
           03  WK-I2           PIC  X(200) VALUE SPACE.
           03  WK-I3           PIC  X(200) VALUE SPACE.
           03  WK-I4           PIC  X(2000) VALUE SPACE.

           03  WK-I1-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I2-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I3-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I4-LEN       BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
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

           WRITE   POT1-REC    FROM    WK-TITLE1

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           WRITE   POT1-REC    FROM    WK-TITLE2

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
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

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-I1
                                       WK-I2
                                       WK-I3
                                       WK-I4
           MOVE    ZERO        TO      WK-I1-LEN
                                       WK-I2-LEN
                                       WK-I3-LEN
                                       WK-I4-LEN
           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-I1    COUNT WK-I1-LEN
                           WK-I2    COUNT WK-I2-LEN
                           WK-I3    COUNT WK-I3-LEN
                           WK-I4    COUNT WK-I4-LEN
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND "10"
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** アイドル名
           MOVE    WK-I3 (1:WK-I3-LEN) 
                               TO      POT1-REC (1:WK-I3-LEN)
           ADD     WK-I3-LEN 1 GIVING  I

           MOVE    ","         TO      POT1-REC (I:1)
           ADD     1           TO      I

      *    *** アイドル ホームページ
           MOVE    WK-I2 (1:WK-I2-LEN) 
                               TO      POT1-REC (I:WK-I2-LEN)
           ADD     WK-I2-LEN   TO      I

           MOVE    ","         TO      POT1-REC (I:1)
           ADD     1           TO      I

      *    *** IMG 画像
           MOVE    WK-I1 (1:WK-I1-LEN) 
                               TO      POT1-REC (I:WK-I1-LEN)
           ADD     WK-I1-LEN   TO      I

           MOVE    ","         TO      POT1-REC (I:1)
           ADD     1           TO      I

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
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
