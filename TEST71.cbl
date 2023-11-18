      *    *** キーダブリカット　データ作成
      *    *** KEY1,2,3 ACCEPT で入力、KEY長32バイトまで

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST71.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT 済 データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーダブリカットデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーダブリカット 件数追加　データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-KEY        PIC  X(096).
           03                  PIC  X(004).
           03  POT2-CNT        PIC  9(009).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST71  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSORT.POT1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST84.POT3S".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST71.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST71.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2B-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-POS1         PIC  9(005) VALUE ZERO.
           03  WK-POS2         PIC  9(005) VALUE ZERO.
           03  WK-POS3         PIC  9(005) VALUE ZERO.
           03  WK-LEN1         PIC  9(005) VALUE ZERO.
           03  WK-LEN2         PIC  9(005) VALUE ZERO.
           03  WK-LEN3         PIC  9(005) VALUE ZERO.
           03  WK-OLD-REC      PIC  X(10000) VALUE LOW-VALUE.

           03  WK-OLD-KEY.
             05  WK-OLD-KEY1   PIC  X(032) VALUE LOW-VALUE.
             05  WK-OLD-KEY2   PIC  X(032) VALUE LOW-VALUE.
             05  WK-OLD-KEY3   PIC  X(032) VALUE LOW-VALUE.
           03  WK-NEW-KEY.
             05  WK-NEW-KEY1   PIC  X(032) VALUE LOW-VALUE.
             05  WK-NEW-KEY2   PIC  X(032) VALUE LOW-VALUE.
             05  WK-NEW-KEY3   PIC  X(032) VALUE LOW-VALUE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX
           ADD     1           TO      WK-POT2B-CNT

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   IF      WK-OLD-KEY  =       WK-NEW-KEY
                           ADD     1           TO      WK-POT2B-CNT
                   ELSE
      *    *** WRITE POT1,POT2
                           PERFORM S100-10     THRU    S100-EX
                           MOVE     1          TO      WK-POT2B-CNT
                   END-IF
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"

                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " KEY1 POS 入力"
                   ACCEPT  WK-POS1 
                   DISPLAY WK-PGM-NAME " KEY1 LEN 入力"
                   ACCEPT  WK-LEN1 

                   DISPLAY WK-PGM-NAME " KEY2 POS 入力 KEY2無はZERO入力"
                   ACCEPT  WK-POS2 
                   DISPLAY WK-PGM-NAME " KEY2 LEN 入力 KEY2無はZERO入力"
                   ACCEPT  WK-LEN2 

                   DISPLAY WK-PGM-NAME " KEY3 POS 入力 KEY3無はZERO入力"
                   ACCEPT  WK-POS3 
                   DISPLAY WK-PGM-NAME " KEY3 LEN 入力 KEY3無はZERO入力"
                   ACCEPT  WK-LEN3 

                   DISPLAY "KEY1=(" WK-POS1 ":" WK-LEN1 ")"
                   DISPLAY "KEY2=(" WK-POS2 ":" WK-LEN2 ")"
                   DISPLAY "KEY3=(" WK-POS3 ":" WK-LEN3 ")"

                   IF      WK-POS1     NOT NUMERIC
                        OR WK-POS2     NOT NUMERIC
                        OR WK-POS3     NOT NUMERIC
                        OR WK-LEN1     NOT NUMERIC
                        OR WK-LEN2     NOT NUMERIC
                        OR WK-LEN3     NOT NUMERIC
                        OR WK-POS1     =       ZERO
                        OR WK-LEN1     =       ZERO
                      OR ( WK-POS2     =       ZERO
                       AND WK-LEN2     NOT =   ZERO )
                      OR ( WK-POS2     NOT =   ZERO
                       AND WK-LEN2     =       ZERO )
                      OR ( WK-POS3     =       ZERO
                       AND WK-LEN3     NOT =   ZERO )
                      OR ( WK-POS3     NOT =   ZERO
                       AND WK-LEN3     =       ZERO )
                        OR WK-LEN1     >       32
                        OR WK-LEN2     >       32
                        OR WK-LEN3     >       32
                        OR WK-POS1 + WK-LEN1 > 10000
                        OR WK-POS2 + WK-LEN2 > 10000
                        OR WK-POS3 + WK-LEN3 > 10000
                           DISPLAY WK-PGM-NAME " POS,LEN 数字で指定"
                                   " LENは32 まで　POS1,LEN1 は必須"
                                   " WK-POS1 + WK-LEN1 <= 10000 "
                           MOVE    "N"         TO      SW-YES
                   ELSE
                           DISPLAY WK-PGM-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-IF
           END-PERFORM

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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF
           MOVE    SPACE       TO      POT2-REC

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA

           MOVE    LOW-VALUE   TO      PIN1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    WK-NEW-KEY  TO      WK-OLD-KEY
           MOVE    PIN1-REC    TO      WK-OLD-REC

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               WK-NEW-KEY
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   MOVE    PIN1-REC (WK-POS1:WK-LEN1)
                                       TO      WK-NEW-KEY1 (1:WK-LEN1)
                   IF      WK-POS2     NOT =   ZERO
                           MOVE    PIN1-REC (WK-POS2:WK-LEN2)
                                       TO      WK-NEW-KEY2 (1:WK-LEN2)
                   END-IF
                   IF      WK-POS3     NOT =   ZERO
                           MOVE    PIN1-REC (WK-POS3:WK-LEN3)
                                       TO      WK-NEW-KEY3 (1:WK-LEN3)
                   END-IF
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

      *    *** キーダブリカット
           WRITE   POT1-REC    FROM    WK-OLD-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

      *    *** 件数
           IF      WK-POT2B-CNT >      50
                   MOVE    WK-OLD-KEY  TO      POT2-KEY
                   MOVE    WK-POT2B-CNT TO     POT2-CNT
                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
