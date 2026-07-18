      *    *** キーダブリカット　データ作成
      *    *** KEY1,2,3 ACCEPT で入力、KEY長32バイトまで

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST71.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT 済 データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーダブリ　１件目データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーダブリ　２件目以降データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーダブリ　キーデータ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
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
           03                  PIC  X(10000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03                  PIC  X(10000).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03                  PIC  X(300).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST71  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSORT.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST84.POT3S".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST75.SORT.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST127.SORT.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST130.POT1.SORT".
      *         "TEST26_202607SJIS2.POT1.SORT".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST71.PIN1.SORT".

           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST71.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST71.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST71.POT3".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2B-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-POS1         PIC  9(005) VALUE ZERO.
           03  WK-POS2         PIC  9(005) VALUE ZERO.
           03  WK-POS3         PIC  9(005) VALUE ZERO.
           03  WK-LEN1         PIC  9(005) VALUE ZERO.
           03  WK-LEN2         PIC  9(005) VALUE ZERO.
           03  WK-LEN3         PIC  9(005) VALUE ZERO.
           03  WK-SJIS         PIC  X(100) VALUE SPACE.
           03  WK-OLD-REC      PIC  X(10000) VALUE LOW-VALUE.

           03  WK-OLD-KEY.
             05  WK-OLD-KEY1   PIC  X(100) VALUE LOW-VALUE.
             05  WK-OLD-KEY2   PIC  X(100) VALUE LOW-VALUE.
             05  WK-OLD-KEY3   PIC  X(100) VALUE LOW-VALUE.
           03  WK-NEW-KEY.
             05  WK-NEW-KEY1   PIC  X(100) VALUE LOW-VALUE.
             05  WK-NEW-KEY2   PIC  X(100) VALUE LOW-VALUE.
             05  WK-NEW-KEY3   PIC  X(100) VALUE LOW-VALUE.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

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

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
               EVALUATE TRUE

                   WHEN WK-OLD-KEY  >  WK-NEW-KEY
                           DISPLAY WK-PGM-NAME " PIN1-F 未ＳＯＲＴです"
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           DISPLAY "OLD-KEY=" WK-OLD-KEY
                           CALL "COBDUMP" USING  WK-OLD-KEY

                           IF      WK-OLD-KEY (1:1) >= X"E0"
                               AND WK-OLD-KEY (1:1) <= X"E9"
                               MOVE    "CHANGE"    TO      WDE05-ID
                               MOVE    WK-HENKAN   TO      WDE05-HENKAN
                               MOVE    WK-MODE     TO      WDE05-MODE
                               MOVE    100         TO     WDE05-BUF1-LEN
                               MOVE    WK-PIN1-CNT TO     WDE05-BUF1-CNT
      *    *** 漢字 ＵＴＦ８＝＞ＳＪＩＳに変換
                               CALL "DECODE05" USING WDE05-DECODE05-AREA
                                                           WK-OLD-KEY
                                                           WK-SJIS
                               DISPLAY "OLD-KEY=" WK-SJIS 
                           END-IF

                           DISPLAY "NEW-KEY=" WK-NEW-KEY
                           CALL "COBDUMP" USING  WK-NEW-KEY

                           IF      WK-NEW-KEY (1:1) >= X"E0"
                               AND WK-NEW-KEY (1:1) <= X"E9"
                               MOVE    "CHANGE"    TO      WDE05-ID
                               MOVE    WK-HENKAN   TO      WDE05-HENKAN
                               MOVE    WK-MODE     TO      WDE05-MODE
                               MOVE    100         TO     WDE05-BUF1-LEN
                               MOVE    WK-PIN1-CNT TO     WDE05-BUF1-CNT
      *    *** 漢字 ＵＴＦ８＝＞ＳＪＩＳに変換
                               CALL "DECODE05" USING WDE05-DECODE05-AREA
                                                           WK-NEW-KEY
                                                           WK-SJIS
                               DISPLAY "NEW-KEY=" WK-SJIS 
                           END-IF

                           STOP    RUN

                   WHEN WK-OLD-KEY  NOT =   WK-NEW-KEY

                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-NEW-KEY  TO      POT3-REC
                           INSPECT POT3-REC REPLACING ALL 
                                   LOW-VALUE BY SPACE
                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT

                   WHEN OTHER
                           WRITE   POT2-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT2-CNT
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

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-PIN1-F-NAME FROM ARGUMENT-VALUE
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " ARGUMENT-VALUE 指定無しか１個指定"
                           " TEST71 PIN1.SORT <=例 SORT済ファイルを指定"
                   STOP    RUN
           END-EVALUATE

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

                      OR ( WK-POS1     =       ZERO
                       AND WK-LEN1     NOT =   ZERO )

                      OR ( WK-POS1     NOT =   ZERO
                       AND WK-LEN1     =       ZERO )

                      OR ( WK-POS2     =       ZERO
                       AND WK-LEN2     NOT =   ZERO )

                      OR ( WK-POS2     NOT =   ZERO
                       AND WK-LEN2     =       ZERO )

                      OR ( WK-POS3     =       ZERO
                       AND WK-LEN3     NOT =   ZERO )

                      OR ( WK-POS3     NOT =   ZERO
                       AND WK-LEN3     =       ZERO )

                        OR WK-LEN1     >       100
                        OR WK-LEN2     >       100
                        OR WK-LEN3     >       100

                        OR WK-POS1 + WK-LEN1 > 10000
                        OR WK-POS2 + WK-LEN2 > 10000
                        OR WK-POS3 + WK-LEN3 > 10000

                           DISPLAY WK-PGM-NAME " POS,LEN 数字で指定"
                                   " LENは100 まで　POS1,LEN1 は必須"
                                   " WK-POS1 + WK-LEN1 <= 10000 "
                           MOVE    "N"         TO      SW-YES
                   ELSE
                           DISPLAY WK-PGM-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-IF
           END-PERFORM

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F
                               POT2-F
                               POT3-F

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
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               WK-NEW-KEY
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                                               WK-POT2B-CNT
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
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

      *    *** キーダブリカット
      *     WRITE   POT1-REC    FROM    WK-OLD-REC
           WRITE   POT1-REC    FROM    PIN1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 件数
      *     IF      WK-POT2B-CNT >      50
      *     IF      WK-POT2B-CNT >      2
      *     IF      WK-POT2B-CNT >      1
      *     IF      WK-POT2B-CNT =      1
      *     IF      WK-POT2B-CNT >      ZERO
      *             MOVE    WK-OLD-KEY  TO      POT2-KEY
      *             MOVE    WK-POT2B-CNT TO     POT2-CNT
      *             INSPECT POT2-KEY REPLACING ALL LOW-VALUE BY SPACE
      *             WRITE   POT2-REC
      *             ADD     1           TO      WK-POT2-CNT
      *     END-IF

      *      MOVE    WK-OLD-KEY  TO      POT2-KEY
      *     MOVE    WK-NEW-KEY  TO      POT2-KEY
      *     INSPECT POT2-KEY REPLACING ALL LOW-VALUE BY SPACE

      *     WRITE   POT2-REC
      *     ADD     1           TO      WK-POT2-CNT
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F
                   POT2-F
                   POT3-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
