      *    *** YouTube 検索 サムネイル再編集
      *    *** このプログラムで 
      *    *** 全部実行(Python=>TEST69=>TEST142=>TEST68)
      *    *** 
      *    *** このプログラムでPython プログラムを作成（TEST141.POT1.py）
      *    *** 実行して、Youtube htmlソース解析して、
      *    *** 
      *    *** YouTube Top List
      *    *** YouTube Mix List
      *    *** YouTube Play List
      *    *** YouTube Short List
      *    *** YouTube Watch List
      *    *** YouTube Search List
      *    *** 
      *    *** Python
      *    ***   ↓
      *    *** TEST69 youtube.石原夏織.html
      *    ***   ↓
      *    *** TEST142
      *    ***   ↓
      *    *** TEST68 石原夏織
      *    *** 
      *    *** 
      *    *** 
      *    *** TEST141 で全部実行する
      *    *** 
      *    *** TEST141 "https://www.youtube.com/results?search_query=%%E5%%B0%%8F%%E5%%80%%89%%E5%%94%%AF" 小倉唯
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST141.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** python 実行ファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** python 変更後、実行ファイル
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
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
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST141 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "youtube_XXXX.py".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST141.POT1.py".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT1      PIC  X(300) VALUE SPACE.
           03  WK-ACCEPT2      PIC  X(100) VALUE SPACE.
           03  WK-UTF8         PIC  X(100) VALUE SPACE.
           03  WK-ITEM         PIC  X(100) VALUE SPACE.
           03  WK-UTF8-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PYTHON-EXEC  PIC  X(022) VALUE 
               "python TEST141.POT1.py".

           03  WK-TEST69-EXEC  PIC  X(100) VALUE 
               "TEST69  youtube.XXXX.html".

           03  WK-TEST142-EXEC PIC  X(007) VALUE 
               "TEST142".

           03  WK-TEST68-EXEC  PIC  X(100) VALUE 
               "TEST68  XXXX".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=SU (SJIS=>UTF8)
           03  WK-HENKAN       PIC  X(006) VALUE "SU".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  SAVE-AREA.
           03  SV-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

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

      *    *** python 実行
           PERFORM S110-10     THRU    S110-EX

      *     PERFORM UNTIL SW-YES = "Y"
      *             DISPLAY WK-PGM-NAME " Python 終わったら、Y 入力"
      *             ACCEPT SW-YES
      *     END-PERFORM



      *    *** TEST69,TEST142,TEST68 実行
           PERFORM S120-10     THRU    S120-EX

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

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
               WHEN 2
                   ACCEPT  WK-ACCEPT1 FROM ARGUMENT-VALUE
                   ACCEPT  WK-ACCEPT2 FROM ARGUMENT-VALUE
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " ARGUMENT-VALUE ２個指定"
                    " >TEST141"
                   ' "https://www.youtube.com/results?search_query='
                   '%%E7%%9F%%B3%%E5%%8E%%9F%%E5%%A4%%8F%%E7%%B9%%94"'
                    " 女優名"
                   STOP    RUN
           END-EVALUATE

           MOVE     SPACE      TO      WK-ITEM
           MOVE     ZERO       TO      WK-ITEM-LEN
           UNSTRING WK-ACCEPT2
                    DELIMITED BY SPACE
                    INTO
                    WK-ITEM COUNT WK-ITEM-LEN
           END-UNSTRING

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

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
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           IF      WK-PIN1-CNT =       4
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    100         TO      WDE05-BUF1-LEN
                   MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
      *    *** 女優名 ＳＪＩＳ＝＞ＵＴＦ８に変換
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-ACCEPT2
                                               WK-UTF8

                   PERFORM VARYING I FROM 1 BY 1
      *                     UNTIL WK-ACCEPT2 (I:1) = SPACE
                           UNTIL I > WK-ITEM-LEN
      *    *** BAT SJIS
                           MOVE    WK-ACCEPT2 (I:1) TO
                                              WK-TEST69-EXEC  (I + 16:1)
                                              WK-TEST68-EXEC  (I +  8:1)
                   END-PERFORM
                   MOVE   '.html'      TO     WK-TEST69-EXEC  (I + 16:5)

                   MOVE    35          TO      P
                   PERFORM VARYING I FROM 1 BY 1
      *                     UNTIL WK-UTF8 (I:1) = SPACE
                           UNTIL I > WDE05-BUF2-LEN
      *    *** PYTHON UTF8
                           MOVE    WK-UTF8 (I:1) TO    PIN1-REC  (P:1)
                           ADD     1           TO      WK-UTF8-LEN
                           ADD     1           TO      P
                   END-PERFORM
                   MOVE   '.html")'    TO      PIN1-REC (P:7)

           END-IF

           IF      WK-PIN1-CNT =       5
                   MOVE    21          TO      P
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL WK-ACCEPT1 (I:1) = SPACE
                           MOVE    WK-ACCEPT1 (I:1) TO PIN1-REC (P:1)
                           ADD     1           TO      P
                   END-PERFORM
                   MOVE    "')"        TO      PIN1-REC (P:2)
           END-IF

           WRITE   POT1-REC    FROM    PIN1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** python 実行
       S110-10.

           CLOSE   POT1-F

           CALL    "SYSTEM"    USING   WK-PYTHON-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " python SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** TEST69,TEST142,TEST68 実行
       S120-10.

           CALL    "SYSTEM"    USING   WK-TEST69-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST69 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           CALL    "SYSTEM"    USING   WK-TEST142-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST142 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           CALL    "SYSTEM"    USING   WK-TEST68-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST68 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F

      *    *** 別の場所でCLOSE
      *             POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1  件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1  件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
