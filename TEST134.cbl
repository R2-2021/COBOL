      *    *** MissAV用 TEST103.ＭｉｓｓＡＶ　ＸＸＸＸ.PIN1
      *    *** 同一ＩＤは無修正を先に出力
      *    *** 

      *    *** PYTHON　MissAV_赤名いと.py を実行
      *    *** import requests
      *    *** res = requests.get('https://missav.ai/ja/actresses/%E8%B5%A4%E5%90%8D%E3%81%84%E3%81%A8?filters=individual&page=1')
      *    *** with open('../../cobol/MissAV.赤名いと.html','w',encoding='utf8') as file:
      *    ***     file.write(res.text)

      *    *** res = requests.get('https://missav.ai/ja/actresses/%E8%B5%A4%E5%90%8D%E3%81%84%E3%81%A8?filters=individual&page=2')
      *    *** with open('../../cobol/MissAV.赤名いと.html','a',encoding='utf8') as file:
      *    ***     file.write(res.text)

      *    *** res = requests.get('https://missav.ai/ja/actresses/%E8%B5%A4%E5%90%8D%E3%81%84%E3%81%A8?filters=individual&page=3')
      *    *** with open('../../cobol/MissAV.赤名いと.html','a',encoding='utf8') as file:
      *    ***     file.write(res.text)

      *    *** res = requests.get('https://missav.ai/ja/actresses/%E8%B5%A4%E5%90%8D%E3%81%84%E3%81%A8?filters=individual&page=4')
      *    *** with open('../../cobol/MissAV.赤名いと.html','a',encoding='utf8') as file:
      *    ***     file.write(res.text)
      *    ***
      *    *** 白城リサは単体は２ページだったので、ページのソースを表示（右クリック）して、
      *    *** CTRL+A 全体選択後、CTRL+C コピーした内容で下記ファイル作成し、
      *    *** TEST10のインプット（PYTHON　未使用）にしても結果同じだった
      *    *** MissAV.白城リサ.html
      *    *** 

      *    ***    |
      *    *** TEST10 PIN2　で1件目、MissAV.赤名いと.html を指定
      *    ***    |
      *    *** TEST134
      *    ***    |
      *    *** TEST104 A015
      *    *** TEST103.PRM1 にA015ＭｉｓｓＡＶ　赤名いと を追加
      *    ***    |
      *    *** TEST53 032 05 または
      *    *** TEST53 032 02
      *    ***    |
      *    *** TEST54
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST134.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10,PIN2 データ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST10,POT1 データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.ＭｉｓｓＡＶ　ＸＸＸＸ.PIN1
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(100).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST134 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-POT1-F-NAME  PIC  X(064) VALUE 
      *         "TEST103.ＭｉｓｓＡＶ　ＸＸＸＸ.PIN1".
               "TEST103.ＭｉｓｓＡＶ　".
           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.


           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT1       PIC  X(100) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  SAVE-AREA.
           03  SV-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  SV-PIN1-REC     PIC  X(1000) VALUE SPACE.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-VIDEO        PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING  KEY IS TBL01-TITLE
                               DESCENDING KEY IS TBL01-UNCEN.
             05  TBL01-TITLE   PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL01-TITLE-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HTTPS   PIC  X(100) VALUE SPACE.
             05  TBL01-HTTPS-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-UNCEN   PIC  X(001) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** 
                   IF      PIN1-REC (1:09) = '<a href="'
                           MOVE     PIN1-REC    TO      SV-PIN1-REC
                   END-IF

      *    *** 
                   IF    ( PIN1-REC (1:11) = '<video     '
                        OR PIN1-REC (1:15) = '<video :class="' )
                       AND SV-PIN1-REC (1:1) NOT = SPACE
      *    *** HTTPS 抽出
                           PERFORM S100-10     THRU    S100-EX
                           MOVE    "Y"         TO      SW-VIDEO
                   END-IF

      *    *** 女優の時
                   IF    ( PIN1-REC (1:07) = '<a     '
      *    *** 女優の時python MissAV_pythonTest3.py
      *    *** import time
      *    *** import os
      *    *** from selenium import webdriver
      *    *** 使用の時

                        OR PIN1-REC (1:24) = '<a class="text-secondary')
                       AND SW-VIDEO    =       "Y"

                           PERFORM UNTIL PIN1-REC (WK-PIN1-LEN:1) = ">"

      *    *** READ PIN1
                                   PERFORM S020-10     THRU    S020-EX

           
                           END-PERFORM
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX

      *    *** タイトル 抽出
                           PERFORM S110-10     THRU    S110-EX
                           MOVE    "N"         TO      SW-VIDEO
                           MOVE    SPACE       TO      SV-PIN1-REC
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** WRITE POT1
           PERFORM S120-10     THRU    S120-EX

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           ACCEPT  WK-ARGUMENT-NUMBER FROM ARGUMENT-NUMBER

           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-ACCEPT1 FROM ARGUMENT-VALUE
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " ARGUMENT-VALUE 指定無しか１個指定"
                           " TEST10 XXX.html <=例"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PRM1-F
                               PIN1-F

           IF      WK-ARGUMENT-NUMBER = 1
                   MOVE    22          TO      P1
      *    *** 女優名等セット
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL WK-ACCEPT1 (J:1) = "."
                           ADD      1          TO       P1
                           MOVE     WK-ACCEPT1 (J:1) TO 
                                    WK-POT1-F-NAME (P1:1)
                   END-PERFORM
                   ADD     1           TO      P1
                   IF      P1          >       64
                           DISPLAY WK-PGM-NAME " WK-POT1-F-NAME"
                                   " OVER > 64"
                           STOP    RUN
                   END-IF
                   MOVE     ".PIN1"    TO       WK-POT1-F-NAME (P1:5)
           ELSE
             READ    PRM1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
               NOT AT END
                   ADD     1           TO      WK-PRM1-CNT
                   MOVE    22          TO      P1
      *    *** 女優名等セット
                   PERFORM VARYING J FROM 8 BY 1
                           UNTIL PRM1-REC (J:1) = "."
                           ADD      1          TO       P1
                           MOVE     PRM1-REC (J:1) TO 
                                    WK-POT1-F-NAME (P1:1)
                   END-PERFORM
                   ADD     1           TO      P1
                   IF      P1          >       64
                           DISPLAY WK-PGM-NAME " WK-POT1-F-NAME"
                                   " OVER > 64"
                           STOP    RUN
                   END-IF
                   MOVE     ".PIN1"    TO       WK-POT1-F-NAME (P1:5)
             END-READ
           END-IF

           OPEN    OUTPUT      POT1-F

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
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** HTTPS 抽出
       S100-10.
           ADD     1           TO      K1
           MOVE    SPACE       TO      TBL01-HTTPS (K1)
           MOVE    ZERO        TO      P2

           PERFORM VARYING I FROM 10 BY 1
                   UNTIL SV-PIN1-REC (I:1) = '"'
                   ADD     1           TO      P2
                   IF      P2          >       100
                           DISPLAY WK-PGM-NAME " TBL01-HTTPS-LEN"
                                   " OVER > 100"
                           DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
                           DISPLAY "SV-PIN1-REC=" SV-PIN1-REC (1:150)
                           STOP    RUN
                   END-IF
                   MOVE    SV-PIN1-REC (I:1) TO  TBL01-HTTPS (K1) (P2:1)
           END-PERFORM

           MOVE    P2          TO      TBL01-HTTPS-LEN (K1)
           IF      SV-PIN1-REC (I - 15:15) = "uncensored-leak"
                   MOVE    "Y"         TO      TBL01-UNCEN (K1)
           END-IF

           MOVE    K1          TO      K1-MAX
           .
       S100-EX.
           EXIT.

      *    *** タイトル 抽出
       S110-10.

           MOVE    SPACE       TO      TBL01-TITLE (K1)

           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL L1 > WK-PIN1-LEN
                   IF      PIN1-REC (L1:1) NOT = SPACE
                           COMPUTE L2 = WK-PIN1-LEN - L1 + 1
                           IF      L2          >       1000
                                  DISPLAY WK-PGM-NAME " TBL01-TITLE-LEN"
                                           " OVER > 1000"
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC (L1:L2) TO 
                                   TBL01-TITLE (K1) (1:L2)
                           MOVE    L2          TO   TBL01-TITLE-LEN (K1)
                           MOVE    WK-PIN1-LEN TO   L1
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.

           SORT    TBL01-AREA
                   ASCENDING  KEY TBL01-TITLE
                   DESCENDING KEY TBL01-UNCEN

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    1           TO      P3

                   MOVE    TBL01-TITLE-LEN (K1) TO L2
                   MOVE    TBL01-TITLE (K1) (1:L2) TO POT1-REC (P3:L2)

                   ADD     L2          TO      P3
                   MOVE    ","         TO      POT1-REC (P3:1)

                   ADD     1           TO      P3
                   MOVE    TBL01-HTTPS-LEN (K1) TO L2
                   MOVE    TBL01-HTTPS (K1) (1:L2) TO POT1-REC (P3:L2)

                   ADD     L2          TO      P3
                   MOVE    ","         TO      POT1-REC (P3:1)

                   ADD     1           TO      P3
                   MOVE    TBL01-UNCEN (K1) TO POT1-REC (P3:1)

                   ADD     1           TO      P3
                   MOVE    ","         TO      POT1-REC (P3:1)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 件数 = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
