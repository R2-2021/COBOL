      *    *** 通常メインプログラムは-xでコンパイルする XXXX.exeが実行される
      *    *** サブルーチンは-mでコンパイルする XXXX.dllが実行される
      *    *** cobc test01 cbl 大文字でも、小文字でもコンパイル可能
      *    *** -x は小文字でないとだめ
      *    *** cobc -x test01.cbl
      *    *** TEST01
      *    *** 
      *    *** メインプログラムもサブルーチンも-mでコンパイルして、
      *    *** XXXX.dll が作成され、cobcrun で実行出来る
      *    *** cobc test01 cbl cobcrun 大文字でも、小文字でもコンパイル
      *    *** 実行可能
      *    *** -m は小文字でないとだめ
      *    *** cobc -m test01.cbl
      *    *** TEST01 はPROGRAM-IDで指定した文字と同じでないと実行しない
      *    *** 大文字、小文字識別している
      *    *** cobcrun TEST01

      *    *** READ / WRITE TEST

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST01.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
      *    *** PACKED-DECIMAL の時、この指定が必要、
      *    *** X"10",X"0D"があると行末までカット、文字が削除される
      *    *** BINARY SEQUENTIAL WRITE, BINARY SEQUENTIAL READ でも問題なし 

      *    ORGANIZATION IS RECORD BINARY SEQUENTIAL. 
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
      *     ORGANIZATION IS RECORD BINARY SEQUENTIAL. 
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST01  ".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST01.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST01X.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "kanji/TEST46_UTF8.POT1".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM06.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST54.PIN2".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST01.PIN2.SORT".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "KANJI1.txt".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST75.POT1".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST71X.POT2".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST01.PIN2".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST139.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST01.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST01.POT2".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.

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

           03  WK-ITEM1X       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM1X-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM1        PIC  X(1000) VALUE SPACE.
           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2        PIC  X(1000) VALUE SPACE.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG          PIC  X(1000) VALUE SPACE.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-I1           PIC  X(006) VALUE SPACE.
           03  WK-I2           PIC  X(006) VALUE SPACE.
           03  WK-I3           PIC  X(006) VALUE SPACE.
           03  WK-I4           PIC  X(006) VALUE SPACE.
           03  WK-I5           PIC  X(006) VALUE SPACE.
           03  WK-I6           PIC  X(006) VALUE SPACE.
           03  WK-I7           PIC  X(006) VALUE SPACE.
           03  WK-I8           PIC  X(006) VALUE SPACE.
           03  WK-I9           PIC  X(006) VALUE SPACE.
           03  WK-I1-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I2-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I3-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I4-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I5-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I6-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I7-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I8-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I9-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-HIT          BINARY-LONG SYNC VALUE ZERO.
           03  WK-KANMA        BINARY-LONG SYNC VALUE ZERO.
           03  WK-CNT          PIC ----9 VALUE ZERO.
           03  WK-PACK         PIC S9(009) PACKED-DECIMAL VALUE 1.
           03  WK-BINARY-LONG  BINARY-LONG SYNC VALUE 2.
           03  WK-DISPLAY1     PIC  X(008) VALUE X"E28093E28690".
           03  WK-DISPLAY2     PIC  X(100) VALUE SPACE.
      *    *** ＡＢＣ
           03  WK-DISPLAY3.
      *       05     PIC X(004) VALUE "ABCD".
             05     PIC X(201) VALUE ALL "三上悠亜".
      *         ALL X"EFBCA1EFBCA2EFBCA3".

           03  WK-DISPLAY3-41.
             05     PIC X(041) VALUE ALL "三上悠亜".
      *         ALL X"212223EFBCA1EFBCA2EFBCA3".

           03  WK-DISPLAY3-43.
             05     PIC X(043) VALUE ALL "三上悠亜".
      *         ALL X"212223EFBCA1EFBCA2EFBCA3".

           03  WK-DISPLAY4.
      *       05                PIC  X(005) VALUE "ABCDE".
             05                PIC  X(005) VALUE "ABCDE".
             05                PIC  X(200) VALUE ALL "≠ａ＝ＢＣ".

           03  WK-SEIYU.
               05  PIC  X(023) VALUE 
                   "C:/Users/koko/OneDrive/".
               05  PIC  X(035) VALUE 
                   "ドキュメント/html/YouTube声優/image".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".
           03  WK-PIN2-REC     PIC  X(1000) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  PIC-XX.
           05                  PIC X VALUE LOW-VALUE.
           05  PIC-X           PIC X VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  SW-AREA..
           05  SW-SEARCH       PIC  X(001) VALUE "N".
           05  SW-SET          PIC  X(001) VALUE "N".

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  TABLE-AREA,
           03  TBL01-AREA      OCCURS 200.
             05  TBL01-ITEM1   PIC  X(100) VALUE SPACE.
             05  TBL01-WRITE   BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-REC     PIC  X(100) VALUE SPACE.

           03  TBL02-AREA      OCCURS 15000.
             05  TBL02-ITEM1   PIC  X(100) VALUE SPACE.
             05  TBL02-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-IMG     PIC  X(100) VALUE SPACE.
             05  TBL02-IMG-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-SET     PIC  X(001) VALUE SPACE.

           03  TBL03-AREA      OCCURS 100.
             05  TBL03-ITEM1   PIC  X(011) VALUE SPACE.
             05  TBL03-SET     BINARY-LONG SYNC VALUE ZERO.

           03  TBL04-AREA      OCCURS 10000.
             05  TBL04-SJIS    PIC  X(004) VALUE SPACE.
             05  TBL04-UTF8    PIC  X(006) VALUE SPACE.
             05  TBL04-UTF8-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL04-IMG     PIC  X(003) VALUE SPACE.
             05  TBL04-SET     BINARY-LONG SYNC VALUE ZERO.

           03  TBL05-AREA      OCCURS 65536
             05  TBL05-UTF8    PIC  X(006) VALUE SPACE.
             05  TBL05-IMG     PIC  X(003) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** TBL05 SET
      *             PERFORM S022-10     THRU    S022-EX

      *    *** WRITE POT1
      *             PERFORM S220-10     THRU    S220-EX

      *    *** WRITE POT1
      *             PERFORM S230-10     THRU    S230-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** READ PIN2
      *     PERFORM S030-10     THRU    S030-EX

      *     PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** TBL01 SET
      *             PERFORM S032-10     THRU    S032-EX

      *    *** TBL04 SET
      *             PERFORM S033-10     THRU    S033-EX

      *    *** READ PIN2
      *             PERFORM S030-10     THRU    S030-EX
      *     END-PERFORM

      *    *** READ PIN3
      *     PERFORM S040-10     THRU    S040-EX

      *     PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE

      *    *** ジャパリ
      *             IF      PIN3-REC (1:12) = X"E382B8E383A3E38391E383AA"
      *                  OR PIN3-REC (1:01) = "%"
      *                     CONTINUE
      *             ELSE
      *    *** TBL02 SET
      *                   PERFORM S042-10     THRU    S042-EX
      *             END-IF

      *    *** READ PIN3
      *             PERFORM S040-10     THRU    S040-EX
      *     END-PERFORM

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
      *             PERFORM S110-10     THRU    S110-EX

      *    *** WRITE POT1
      *             PERFORM S120-10     THRU    S120-EX

      *    *** WRITE POT1
      *             PERFORM S130-10     THRU    S130-EX

      *    *** WRITE POT1
      *             PERFORM S140-10     THRU    S140-EX

      *    *** WRITE POT1
      *             PERFORM S160-10     THRU    S160-EX

      *    *** WRITE POT1
      *             PERFORM S200-10     THRU    S200-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

      *    *** WRITE IMG 追加分
      *     PERFORM S150-10     THRU    S150-EX

      *    *** 0-255 WRITE POT1
      *     PERFORM S100-10     THRU    S100-EX

      *    *** DISPLAY CHECK
      *     PERFORM S170-10     THRU    S170-EX

      *    *** DISPLAY CHECK
      *     PERFORM S180-10     THRU    S180-EX

      *    *** DISPLAY CHECK
      *     PERFORM S190-10     THRU    S190-EX

      *    *** DISPLAY CHECK
      *     PERFORM S210-10     THRU    S210-EX

      *    *** CALL "SYSTEM"
           PERFORM S240-10     THRU    S240-EX

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

           OPEN    INPUT       PIN1-F
                               PIN2-F
                               PIN3-F
           OPEN    OUTPUT      POT1-F
                               POT2-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
      *             INSPECT PIN1-REC REPLACING ALL 
      *                     X'09' BY SPACE

      *             MOVE    "P"         TO      WFD-ID
      *             MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** TBL01 SET
       S022-10.

           ADD     1           TO      I1
           IF      I1          >       65536
                   DISPLAY WK-PGM-NAME " TBL05 OVER I1=" I1
                   STOP    RUN
           END-IF

           MOVE    PIN1-REC (1:6) TO   TBL05-UTF8 (I1)
           MOVE    PIN1-REC (7:3) TO   TBL05-IMG  (I1)
           MOVE    I1          TO      I1-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   MOVE    PIN2-REC    TO   WK-PIN2-REC
                   MOVE    ","         TO   
                           WK-PIN2-REC (WK-PIN2-LEN + 1:1)

                   INSPECT WK-PIN2-REC REPLACING ALL 
                           X'09' BY ","
                   MOVE    SPACE       TO      WK-I1
                                               WK-I2
                                               WK-I3
                                               WK-I4
                                               WK-I5
                                               WK-I6
                                               WK-I7
                                               WK-I8
                                               WK-I9
                   MOVE    ZERO        TO      WK-I1-LEN
                                               WK-I2-LEN
                                               WK-I3-LEN
                                               WK-I4-LEN
                                               WK-I5-LEN
                                               WK-I6-LEN
                                               WK-I7-LEN
                                               WK-I8-LEN
                                               WK-I9-LEN

                    UNSTRING WK-PIN2-REC
                        DELIMITED BY ","
                        INTO
                            WK-I1 COUNT WK-I1-LEN
                            WK-I2 COUNT WK-I2-LEN
                            WK-I3 COUNT WK-I3-LEN
                            WK-I4 COUNT WK-I4-LEN
                            WK-I5 COUNT WK-I5-LEN
                            WK-I6 COUNT WK-I6-LEN
                            WK-I7 COUNT WK-I7-LEN
                            WK-I8 COUNT WK-I8-LEN
                            WK-I9 COUNT WK-I9-LEN
                    END-UNSTRING

           IF WK-PIN2-CNT < 10
                   MOVE    "X"         TO      WFD-ID
                   MOVE    "UTF8"      TO      WFD-KANJI
                   MOVE    WK-PIN2-CNT TO      WFD-SEQ
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-PIN2-REC (1:40)
                   MOVE    "X"         TO      WFD-ID
                   MOVE    "UTF8"      TO      WFD-KANJI
                   MOVE    WK-PIN2-CNT TO      WFD-SEQ
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               WK-PIN2-REC (1:100)
           END-IF

                   ADD     1           TO      WK-PIN2-CNT
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** TBL01 SET
       S032-10.

           ADD     1           TO      I
           IF      I           >       100
                   DISPLAY WK-PGM-NAME " TBL03 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    PIN2-REC    TO      TBL03-ITEM1 (I)
           MOVE    I           TO      I-MAX
           .
       S032-EX.
           EXIT.

      *    *** TBL04 SET
       S033-10.

           ADD     1           TO      I
           IF      I           >       10000
                   DISPLAY WK-PGM-NAME " TBL04 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-I4       TO      TBL04-SJIS (I)
           MOVE    WK-I6       TO      TBL04-UTF8 (I)
           MOVE    WK-I8       TO      TBL04-IMG  (I)
           MOVE    I           TO      I-MAX

      *    *** EFBFBD:?
           IF      WK-I8 (1:3) =       X"EFBFBD"
                OR WK-I6       NOT =   SPACE
                   WRITE   POT1-REC    FROM    PIN2-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
      *    *** 追加
                   PERFORM S034-10     THRU    S034-EX
           END-IF
           .
       S033-EX.
           EXIT.

       S034-10.

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL I1 > I1-MAX
                   IF      WK-I8 (1:3)    =        TBL05-IMG (I1)
                           MOVE    TBL05-UTF8 (I1) TO  WK-I6
                           MOVE    6           TO      WK-I6-LEN
                           MOVE    "Y"         TO      SW-SET
                   END-IF
           END-PERFORM

           IF WK-PIN2-CNT = 1
              DISPLAY SW-SET
           END-IF

           IF      SW-SET      =       "N"
                   WRITE   POT1-REC    FROM    PIN2-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
      *     IF WK-PIN2-CNT < 10
      *     DISPLAY "WK-PIN2-CNT=" WK-PIN2-CNT
      *     DISPLAY "WK-I1=" WK-I1
      *     DISPLAY "WK-I2=" WK-I2
      *     DISPLAY "WK-I3=" WK-I3
      *     DISPLAY "WK-I4=" WK-I4
      *     DISPLAY "WK-I5=" WK-I5
      *     DISPLAY "WK-I6=" WK-I6
      *     DISPLAY "WK-I7=" WK-I7
      *     DISPLAY "WK-I8=" WK-I8
      *     DISPLAY "WK-I9=" WK-I9-LEN

      *     DISPLAY "WK-I1-LEN=" WK-I1-LEN
      *     DISPLAY "WK-I2-LEN=" WK-I2-LEN
      *     DISPLAY "WK-I3-LEN=" WK-I3-LEN
      *     DISPLAY "WK-I4-LEN=" WK-I4-LEN
      *     DISPLAY "WK-I5-LEN=" WK-I5-LEN
      *     DISPLAY "WK-I6-LEN=" WK-I6-LEN
      *     DISPLAY "WK-I7-LEN=" WK-I7-LEN
      *     DISPLAY "WK-I8-LEN=" WK-I8-LEN
      *     DISPLAY "WK-I9-LEN=" WK-I9-LEN
      *     END-IF
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    1           TO      P

                   MOVE    WK-I1 (1:WK-I1-LEN) TO POT1-REC (P:WK-I1-LEN)
                   ADD     WK-I1-LEN   TO      P
      *    *** X"09":TAB
                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    WK-I2 (1:WK-I2-LEN) TO POT1-REC (P:WK-I2-LEN)
                   ADD     WK-I2-LEN   TO      P

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    WK-I3 (1:WK-I3-LEN) TO POT1-REC (P:WK-I3-LEN)
                   ADD     WK-I3-LEN   TO      P

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    WK-I4 (1:WK-I4-LEN) TO POT1-REC (P:WK-I4-LEN)
                   ADD     WK-I4-LEN   TO      P

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    WK-I5 (1:WK-I5-LEN) TO POT1-REC (P:WK-I5-LEN)
                   ADD     WK-I5-LEN   TO      P

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   IF      WK-I6-LEN   =       ZERO
                           CONTINUE
                   ELSE
                           MOVE    WK-I6 (1:WK-I6-LEN) TO 
                                   POT1-REC (P:WK-I6-LEN)
                           ADD     WK-I6-LEN   TO      P
                   END-IF

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   IF      WK-I7-LEN   =       ZERO
                           CONTINUE
                   ELSE
                           MOVE    WK-I7 (1:WK-I7-LEN) TO 
                                   POT1-REC (P:WK-I7-LEN)
                           ADD     WK-I7-LEN   TO      P
                   END-IF

                   MOVE    X"09"       TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    WK-I8 (1:WK-I8-LEN) TO POT1-REC (P:WK-I8-LEN)
                   ADD     WK-I8-LEN   TO      P

      *             MOVE    X"09"       TO      POT1-REC (P:1)
      *             ADD     1           TO      P

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S034-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           READ    PIN3-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** TBL02 SET
       S042-10.

           ADD     1           TO      J
           IF      J           >       15000
                   DISPLAY WK-PGM-NAME " TBL02 OVER J=" J
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      WK-ITEM1
                                       WK-ITEM2
                                       WK-IMG
           MOVE    ZERO        TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN
                                       WK-IMG-LEN

           UNSTRING PIN3-REC
                    DELIMITED BY " ,"
                    INTO
                    WK-ITEM1     COUNT WK-ITEM1-LEN
                    WK-ITEM2     COUNT WK-ITEM2-LEN
                    WK-IMG       COUNT WK-IMG-LEN

           PERFORM VARYING J3 FROM 1 BY 1
                   UNTIL J3 > J

               IF      WK-ITEM1 =          TBL02-ITEM1     (J3)
                   ADD     -1          TO      J
                   EXIT    PERFORM
               END-IF

               IF      J3  =  J
                   MOVE    WK-ITEM1    TO      TBL02-ITEM1     (J)
                   MOVE    WK-ITEM1-LEN TO     TBL02-ITEM1-LEN (J)
                   MOVE    WK-IMG      TO      TBL02-IMG       (J)
                   MOVE    WK-IMG-LEN  TO      TBL02-IMG-LEN   (J)
               END-IF
           END-PERFORM

           MOVE    J           TO      J-MAX
           .
       S042-EX.
           EXIT.

      *    *** 0-255 WRITE POT1
       S100-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 256
                   compute PIC-Halfword = I - 1
                   MOVE    SPACE       TO      POT1-REC
      *             MOVE    PIC-X       TO      POT1-I1 (1)
      *             MOVE    X"0D"       TO      POT1-I1 (2)
      *             MOVE    X"0A"       TO      POT1-I1 (3)
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "P"         TO      WFD-ID
                   MOVE    I           TO      WFD-SEQ
                   MOVE    2           TO      WFD-SU
                   MOVE    "M"         TO      WFD-TYPE
                   MOVE    "      "    TO      WFD-ITEM
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC
           END-PERFORM

      *     MOVE    "あいうえお" TO       PIN1-KANJI
      *     WRITE   POT1-REC    FROM      PIN1-REC
      *     WRITE   POT1-REC    FROM      PIN1-REC
      *     ADD     1           TO        WK-POT1-CNT
      *

      *     PERFORM S100        THRU      S100-EX
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.

           MOVE    SPACE       TO      WK-ITEM1

           MOVE    ZERO        TO      WK-ITEM1-LEN

           UNSTRING PIN1-REC
                    DELIMITED BY ","
                    INTO
                    WK-ITEM1     COUNT WK-ITEM1-LEN

           WRITE   POT1-REC    FROM    WK-ITEM1
           ADD     1           TO      WK-POT1-CNT
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

           MOVE    SPACE       TO      WK-ITEM1
           MOVE    ZERO        TO      WK-ITEM1-LEN

           UNSTRING PIN1-REC
                    DELIMITED BY ","
                    INTO
                    WK-ITEM1     COUNT WK-ITEM1-LEN

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

                   IF      TBL01-ITEM1 (I)  (1:WK-ITEM1-LEN)
                       =   WK-ITEM1 (1:WK-ITEM1-LEN)
                           MOVE    "Y"         TO      SW-SEARCH
                       IF      TBL01-WRITE (I) =   ZERO

                               WRITE   POT1-REC    FROM    PIN1-REC
                               ADD     1           TO      WK-POT1-CNT
                               MOVE    1           TO    TBL01-WRITE (I)
                               MOVE    PIN1-REC    TO      TBL01-REC (I)
                       ELSE
                           IF      PIN1-REC (1:WK-PIN1-LEN)
                                 = TBL01-REC (I) (1:WK-PIN1-LEN)
                                   CONTINUE
                           ELSE
                               WRITE   POT2-REC    FROM    PIN1-REC
                               ADD     1           TO      WK-POT2-CNT
                           END-IF
                       END-IF
                       EXIT    PERFORM
                   END-IF

                   IF      I           =       I-MAX
      *                 IF      TBL01-WRITE (I) =   ZERO

                               WRITE   POT1-REC    FROM    PIN1-REC
                               ADD     1           TO      WK-POT1-CNT
      *                 END-IF
                   END-IF
           END-PERFORM

           .
       S130-EX.
           EXIT.

      *    *** WRITE POT1
       S140-10.

           MOVE    SPACE       TO      WK-ITEM1X
           MOVE    ZERO        TO      WK-ITEM1X-LEN

           UNSTRING PIN1-REC
                    DELIMITED BY ","
                    INTO
                    WK-ITEM1X     COUNT WK-ITEM1X-LEN

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > J-MAX

                   IF      TBL02-ITEM1 (J)  (1:WK-ITEM1X-LEN)
                       =   WK-ITEM1X (1:WK-ITEM1X-LEN)

                           MOVE    SPACE       TO      POT1-REC

                           MOVE    "1"         TO      TBL02-SET (J)
                           MOVE    1           TO      P
                           MOVE    WK-ITEM1X (1:WK-ITEM1X-LEN) TO
                                   POT1-REC (P:WK-ITEM1X-LEN)
                           ADD     WK-ITEM1X-LEN TO    P

                           MOVE    ","         TO      POT1-REC (P:1)
                           ADD     1           TO      P

                           MOVE    TBL02-IMG-LEN (J) TO    L
                           MOVE    TBL02-IMG (J) (1:L) TO
                                   POT1-REC (P:L)
                           ADD     L           TO      P

                           MOVE    ZERO        TO      WK-KANMA
                           PERFORM VARYING J2 FROM 1 BY 1
                                   UNTIL J2 > WK-PIN1-LEN
                               IF      PIN1-REC (J2:1) = ","
                                   ADD     1           TO      WK-KANMA
                               END-IF

                               IF      WK-KANMA > 1
                                       MOVE PIN1-REC (J2:1) TO
                                            POT1-REC (P:1)
                                       ADD     1       TO      P
                               END-IF
                           END-PERFORM

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                                                       WK-HIT
                           EXIT    PERFORM
                   ELSE
                           CONTINUE
                   END-IF

                   IF      J           =       J-MAX
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-PERFORM

           .
       S140-EX.
           EXIT.

      *    *** WRITE IMG 追加分
       S150-10.

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > J-MAX
                   IF      TBL02-SET (J) NOT = "1"
                           MOVE    SPACE       TO      POT2-REC

                           MOVE    1           TO      P
                           MOVE    TBL02-ITEM1-LEN (J) TO L
                           MOVE    TBL02-ITEM1 (J) TO  POT2-REC (P:L)
                           ADD     L           TO      P

                           MOVE    ","         TO      POT2-REC (P:1)
                           ADD     1           TO      P

                           MOVE    TBL02-IMG-LEN (J) TO L
                           MOVE    TBL02-IMG (J) TO    POT2-REC (P:L)
                           ADD     L           TO      P

                           MOVE    ","         TO      POT2-REC (P:1)
                           ADD     1           TO      P

                           WRITE   POT2-REC
                           ADD     1           TO      WK-POT2-CNT
                   END-IF
           END-PERFORM
           .
       S150-EX.
           EXIT.

       S160-10.

           IF      PIN1-REC (22:2) = " 0"
                OR PIN1-REC (22:2) = "05"
                OR PIN1-REC (22:2) = "20"
                OR PIN1-REC (22:2) = "5C"
                OR PIN1-REC (24:2) = " 0"
                OR PIN1-REC (24:2) = "05"
                OR PIN1-REC (24:2) = "20"
                OR PIN1-REC (24:2) = "5C"
                OR PIN1-REC (26:2) = " 0"
                OR PIN1-REC (26:2) = "05"
                OR PIN1-REC (26:2) = "20"
                OR PIN1-REC (26:2) = "5C"
                   DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT 
                           " PIN1-REC (22:6)=" PIN1-REC (22:6)
           END-IF

           MOVE    PIN1-REC (22:2) TO  POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (24:2) TO  POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (26:2) TO  POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S160-EX.
           EXIT.

       S170-10.

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    100         TO      WDE05-BUF1-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
      *    *** 漢字 ＵＴＦ８＝＞ＳＪＩＳに変換
      *     CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                 WK-DISPLAY1
      *                                 WK-DISPLAY2
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       WK-DISPLAY2

           CALL    "COBDUMP"   USING   WK-DISPLAY2
           .
       S170-EX.
           EXIT.

       S180-10.

           MOVE    "X"         TO      WFD-ID
      *     MOVE    WK-PACK     TO      WFD-SEQ
           MOVE    1           TO      WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    SPACE       TO      WFD-ITEM
      *     MOVE    X"E9A085E79BAEEFBC91"      TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:30)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
      *     MOVE    X"E9A085E79BAEEFBC92"      TO      WFD-ITEM2
           MOVE    SPACE       TO      WFD-ITEM
           MOVE    SPACE       TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:20)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    "WKDISPLAY3"      TO      WFD-ITEM
           MOVE    SPACE       TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:39)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
      *     MOVE    X"E9A085E79BAEEFBC94"      TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:40)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    "WK-DIS-41 "    TO      WFD-ITEM
           MOVE    SPACE       TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3-41 (1:41)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    SPACE       TO      WFD-ITEM
      *     MOVE    X"E9A085E79BAEEFBC96"      TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:42)

           MOVE    "X"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    SPACE       TO      WFD-ITEM
      *     MOVE    X"E9A085E79BAEEFBC97"      TO      WFD-ITEM2
           MOVE    SPACE       TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3-43 (1:43)

           MOVE    "P"         TO      WFD-ID
           MOVE    WK-BINARY-LONG TO   WFD-SEQ
           MOVE    "SJIS"      TO      WFD-KANJI
           MOVE    SPACE       TO      WFD-ITEM
      *     MOVE    X"E9A085E79BAEEFBC98"      TO      WFD-ITEM2
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DISPLAY3 (1:201)

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "SJIS"      TO      WFD-KANJI
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-DISPLAY4

      *     MOVE    "AあいうえおKBかきくけこさしすせそ"
      *                         TO      WK-DISPLAY2

      *     CALL    "COBDUMP"   USING   WK-DISPLAY2
           .
       S180-EX.
           EXIT.

       S190-10.

           UNSTRING PIN1-REC
                    DELIMITED BY " url : /watch?v=" OR " ," OR "&pp"
                               OR "&list" OR "&start"
                    INTO
                    WK-ITEM1     COUNT WK-ITEM1-LEN
                    WK-ITEM2     COUNT WK-ITEM2-LEN
                    WK-IMG       COUNT WK-IMG-LEN

      *     DISPLAY PIN1-REC (1:80)
      *     DISPLAY WK-ITEM1 (1:80)
           WRITE   POT1-REC    FROM    WK-ITEM2
           ADD     1           TO      WK-POT1-CNT
           .
       S190-EX.
           EXIT.

       S200-10.

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

               PERFORM VARYING I2 FROM 1 BY 1
                       UNTIL I2 > WK-PIN1-LEN
                          OR SW-SET = "Y"

                   IF      TBL03-ITEM1 (I) (1:11)  = PIN1-REC (I2:11)
                           ADD     1           TO      TBL03-SET (I)
                           MOVE    "Y"         TO      SW-SET
                           ADD     1           TO      J
                           DISPLAY J  " " TBL03-ITEM1 (I) (1:11)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       S200-EX.
           EXIT.

       S210-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   IF      TBL03-SET (I) = ZERO
                           WRITE   POT1-REC    FROM    TBL03-ITEM1 (I)
                           ADD     1           TO      WK-POT1-CNT
                   END-IF
           END-PERFORM
           .
       S210-EX.
           EXIT.

       S220-10.

           EVALUATE TRUE
               WHEN WK-PIN1-LEN = ZERO
                   MOVE    SPACE       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** ジャパリ
      *         WHEN PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"
      *             MOVE    SPACE       TO      POT1-REC
      *             WRITE   POT1-REC
      *             ADD     1           TO      WK-POT1-CNT

               WHEN OTHER
                   MOVE    ","         TO   PIN1-REC (WK-PIN1-LEN + 1:1)
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT

           END-EVALUATE
           .
       S220-EX.
           EXIT.

       S230-10.

           UNSTRING PIN1-REC
                    DELIMITED BY "," OR SPACE
                    INTO
                    WK-ITEM1     COUNT WK-ITEM1-LEN
                    WK-ITEM2     COUNT WK-ITEM2-LEN
           IF WK-PIN1-CNT = 1
              DISPLAY "WK-ITEM2=" WK-ITEM2 " WK-ITEM2-LEN=" WK-ITEM2-LEN
           END-IF

           MOVE    WK-ITEM1    TO      POT1-REC
           COMPUTE P = WK-ITEM1-LEN + 1

           MOVE    ","         TO      POT1-REC (P:1)
           ADD     1           TO      P

           MOVE    WK-ITEM2    TO      POT1-REC (P:WK-ITEM2-LEN)
           ADD     WK-ITEM2-LEN TO     P

           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           MOVE    WK-ITEM2    TO      POT1-REC (P:32)
           ADD     32          TO      P

           MOVE    "_img"      TO      POT1-REC (P:4)
           ADD     4           TO      P

           COMPUTE L = WK-ITEM2-LEN - 32
           MOVE    WK-ITEM2 (33:L) TO  POT1-REC (P:L)
           ADD     L           TO      P

           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S230-EX.
           EXIT.

       S240-10.

           CALL   "CBL_CHANGE_DIR" USING WK-SEIYU

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " CALL SYSTEM RETURN-CODE ERROR="
           END-IF

           CALL    "SYSTEM"    USING   "DIR *.jpg > TEST01.DIR.txt"

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME 
                           " CALL SYSTEM DIR RETURN-CODE ERROR="
           END-IF

           .
       S240-EX.
           EXIT.

      *    *** CLOSE
       S900-10.
           
           CLOSE   PIN1-F
                   PIN2-F
                   PIN3-F
                   POT1-F
                   POT2-F

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
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 件数 = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           DISPLAY WK-HIT
           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
