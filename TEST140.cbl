      *    *** MISSAV 検索 検索結果 画像拡大表示 横５ OR ２個
      *    *** このプログラムで 
      *    *** 全部実行(Python=>TEST10=>TEST134=>TEST104=>TEST53=>TEST54)
      *    *** 
      *    *** 私は叡智を極める修行中の身であるため普段から複数の
      *    *** 無料アダルトサイトで修行を重ねていますので、＜＝他からの流用
      *    *** アクセスしやすいように、htmlを作っています。
      *    *** このプログラムは、このような内容の為、
      *    *** １８禁（１８歳未満使用不可）です。
      *    *** 使用については、参照者のモラルに任せます。
      *    *** 
      *    *** 
      *    *** このプログラムでPython プログラムを作成（TEST140.POT1.py）
      *    *** 実行して、AV女優リストで、ページのソースを
      *    *** 
      *    *** または、Pythonで該当ページのhtmlを取得してファイル作成する
      *    *** MissAV_XXXX.htmlに出力
      *    *** 下記プログラムもこのプログラムで自動実行する
      *    *** 
      *    *** TEST10  (PIN2 1件目にファイル名 MISSAV.XXXX.html を指定)
      *    ***   ↓
      *    *** TEST104 A001
      *    ***   ↓
      *    *** TEST53 032 02
      *    ***   ↓
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST140.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** python 実行ファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST54.PIN2 データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.PRM1 データ
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** C.TEST104_1A.bat データ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** python 変更後、実行ファイル
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST54.PIN2 変更データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.PRM1 変更データ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** C.TEST104_1A.bat 変更データ
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(10000).

       FD  PIN3-F
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(100).

       FD  PIN4-F
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(10000).

       FD  POT3-F.
       01  POT3-REC.
           03  FILLER          PIC  X(100).

       FD  POT4-F.
       01  POT4-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST140 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "MissAV_XXXX.py".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST54.PIN2".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST103.PRM1".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "C.TEST104_1A.bat".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST140.POT1.py".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST140.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST140.POT3".
           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST140.POT4".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT2    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-ACCEPT1      PIC  X(300) VALUE SPACE.
           03  WK-ACCEPT2      PIC  ZZZ9   VALUE ZERO.
           03  WK-ACCEPT3      PIC  X(100) VALUE SPACE.
           03  WK-ACCEPT4      PIC  X(001) VALUE SPACE.
           03  WK-UTF8         PIC  X(100) VALUE SPACE.
           03  WK-CHCP-932     PIC  X(010) VALUE "CHCP 932".
           03  WK-CHCP-65001   PIC  X(010) VALUE "CHCP 65001".
           03  WK-ITEM1        PIC  X(100) VALUE SPACE.
           03  WK-ITEM2        PIC  X(100) VALUE SPACE.
           03  WK-ITEM3        PIC  X(100) VALUE SPACE.
           03  WK-UTF8-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM3-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-NO.
             05  WK-NO-9       PIC  9(003) VALUE ZERO.
           03  WK-PYTHON-EXEC  PIC  X(022) VALUE 
               "python TEST140.POT1.py".

           03  WK-TEST10-EXEC  PIC  X(100) VALUE 
               "TEST10  MissAV.XXXX.html".

           03  WK-TEST134-EXEC PIC  X(100) VALUE 
               "TEST134 MissAV.XXXX.html".

           03  WK-TEST104-EXEC PIC  X(100) VALUE 
               "TEST104 XXXX".

           03  WK-TEST53-EXEC  PIC  X(100) VALUE 
               "TEST53 032 XX".

           03  WK-TEST54-EXEC  PIC  X(100) VALUE 
               "TEST54".

      *    *** 置換えCOPY 実行ファイル
           03  WK-COPY1-EXEC.
             05                 PIC  X(032) VALUE
                 "copy /y TEST140.POT2 TEST54.PIN2".
           03  WK-COPY2-EXEC.
             05                 PIC  X(033) VALUE
                 "copy /y TEST140.POT3 TEST103.PRM1".
           03  WK-COPY3-EXEC.
             05                 PIC  X(037) VALUE
                 "copy /y TEST140.POT4 C.TEST104_1A.bat".

      *    *** 女優名,https://fourhoi.com/actress/1079452-t.jpg 
           03  WK-TEST54-PIN2-1.
             05                 PIC  X(032) VALUE
                 ",file:///C:/Users/koko/OneDrive/".
      *    *** ドキュメント
             05                 PIC  X(018) VALUE
                 X"E38389E382ADE383A5E383A1E383B3E38388".
             05                 PIC  X(013) VALUE "/html/YouTube".
      *    *** ＡＸ
             05                 PIC  X(006) VALUE
                 X"EFBCA1EFBCB8".
             05                 PIC  X(006) VALUE "/index".
      *    *** ＭｉｓｓＡＶ　
             05                 PIC  X(021) VALUE
                 X"EFBCADEFBD89EFBD93EFBD93EFBCA1EFBCB6E38080".

      *    *** 女優名
           03  WK-TEST54-PIN2-2.
             05                 PIC  X(012) VALUE SPACE.

           03  WK-TEST54-PIN2-3.
             05                 PIC  X(010) VALUE " 02.html !".

      *    *** 女優名
           03  WK-TEST54-PIN2-4.
             05                 PIC  X(012) VALUE SPACE.

           03  WK-TEST54-PIN2-5.
             05                 PIC  X(035) VALUE
                 " 02,file:///C:/Users/koko/OneDrive/".
      *    *** ドキュメント
             05                 PIC  X(018) VALUE
                 X"E38389E382ADE383A5E383A1E383B3E38388".
             05                 PIC  X(013) VALUE "/html/YouTube".
      *    *** ＡＸ
             05                 PIC  X(006) VALUE
                 X"EFBCA1EFBCB8".
             05                 PIC  X(006) VALUE "/index".
      *    *** ＭｉｓｓＡＶ　
             05                 PIC  X(021) VALUE
                 X"EFBCADEFBD89EFBD93EFBD93EFBCA1EFBCB6E38080".

      *    *** 女優名
           03  WK-TEST54-PIN2-6.
             05                 PIC  X(012) VALUE SPACE.

           03  WK-TEST54-PIN2-7.
             05                 PIC  X(010) VALUE " 05.html !".

      *    *** 女優名
           03  WK-TEST54-PIN2-8.
             05                 PIC  X(012) VALUE SPACE.

           03  WK-TEST54-PIN2-9.
             05                 PIC  X(004) VALUE " 05,".

           03  WK-TEST104-1.
             05                 PIC  X(004) VALUE "REM ".
      *    *** SJIS
             05                 PIC  X(014) VALUE "ＭｉｓｓＡＶ　".

           03  WK-TEST104-2.
      *    *** 女優名
             05                 PIC  X(012) VALUE SPACE.

           03  WK-TEST104-3.
             05                 PIC  X(003) VALUE " 02".

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=SU (SJIS=>UTF8)
           03  WK-HENKAN       PIC  X(006) VALUE "SU".
           03  WK-POT2-REC     PIC  X(1000) VALUE SPACE.
           03  WK-POT3-REC.
             05  WK-POT3-ID    PIC  X(001) VALUE SPACE.
             05  WK-POT3-NO    PIC  9(003) VALUE ZERO.
      *    *** ＭｉｓｓＡＶ　
             05                PIC  X(021) VALUE
                 X"EFBCADEFBD89EFBD93EFBD93EFBCA1EFBCB6E38080".
             05  WK-POT3-NAME  PIC  X(076) VALUE SPACE.

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
           03  SW-EXPANSION    PIC  X(001) VALUE "N".
           03  SW-ACCEPT4      PIC  X(001) VALUE "N".
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



      *    *** TEST10,TEST134 実行
           PERFORM S120-10     THRU    S120-EX



      *    *** PIN2 : TEST54.PIN2 の変更　A:女優の時のみ行う
           IF      WK-ACCEPT4  =       "A"
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX

                   PERFORM UNTIL SW-EXPANSION = "Y"
                              OR WK-PIN2-EOF = HIGH-VALUE

      *    *** TEST54.PIN2 データ変更
                           PERFORM S130-10     THRU    S130-EX
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM

      *    *** CLOSE PIN2 OPEN PIN2
                   PERFORM S012-10     THRU    S012-EX

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX

                   PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
                           IF      WK-PIN2-CNT2 =      SV-PIN2-CNT
                                   CONTINUE
                           ELSE
      *    *** TEST54.PIN2 変更 => POT2-REC
                                   PERFORM S132-10     THRU    S132-EX
                           END-IF
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM
           END-IF



      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** TEST103.PRM1 変更
                   PERFORM S140-10     THRU    S140-EX
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM



      *    *** READ PIN4
           PERFORM S050-10     THRU    S050-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** C.TEST104_1A.bat 既存分
                   PERFORM S150-10     THRU    S150-EX
      *    *** READ PIN4
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM

      *    *** C.TEST104_1A.bat 追加分
           PERFORM S152-10     THRU    S152-EX



      *    *** COPY 実行
           PERFORM S160-10     THRU    S160-EX



      *    *** TEST103,TEST53,TEST54 実行
           PERFORM S170-10     THRU    S170-EX

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
               WHEN 4
                   ACCEPT  WK-ACCEPT1 FROM ARGUMENT-VALUE
                   ACCEPT  WK-ACCEPT2 FROM ARGUMENT-VALUE
                   ACCEPT  WK-ACCEPT3 FROM ARGUMENT-VALUE
                   ACCEPT  WK-ACCEPT4 FROM ARGUMENT-VALUE
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME 
                           " ARGUMENT-VALUE ４個指定"
                    " >TEST140"
                   ' "https://missav.ai/ja/actresses/女優名&page= PPPP"'
                    " 女優名 A  <=例 PPPP=ページＮｏ＋１"
                   STOP    RUN
           END-EVALUATE

           MOVE     SPACE      TO      WK-ITEM3
           MOVE     ZERO       TO      WK-ITEM3-LEN
           UNSTRING WK-ACCEPT3
                    DELIMITED BY SPACE
                    INTO
                    WK-ITEM3 COUNT WK-ITEM3-LEN
           END-UNSTRING

      *    *** TEST103.PRM1 MISSAV 用 IDチェック
           IF      WK-ACCEPT4  =       "A" OR "J" OR "K" OR "L"
                                    OR "M" OR "S" OR "T"
                   MOVE    WK-ACCEPT4  TO      WK-PIN4-F-NAME (12:1)
           ELSE
                   DISPLAY WK-PGM-NAME 
                           " TEST103.PRM1 MISSAV 用 ID エラー"
                           " WK-ACCEPT4=" WK-ACCEPT4
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
                               PIN2-F
                               PIN3-F
                               PIN4-F
                   OUTPUT      POT1-F
                               POT2-F
                               POT3-F
                               POT4-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** CLOSE PIN2 OPEN PIN2
       S012-10.

           CLOSE   PIN2-F
           OPEN    INPUT       PIN2-F

           MOVE    LOW-VALUE   TO      WK-PIN2-EOF
           MOVE    ZERO        TO      WK-PIN2-CNT2
           .
       S012-EX.
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

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT  AT  END
                   IF     SW-EXPANSION =     "Y"
                          ADD     1          TO      WK-PIN2-CNT2
                   ELSE
                          ADD     1          TO      WK-PIN2-CNT
                   END-IF
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           READ    PIN3-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** READ PIN4
       S050-10.

           READ    PIN4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ
           .
       S050-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           IF      WK-PIN1-CNT =       10
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    100         TO      WDE05-BUF1-LEN
                   MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
      *    *** 女優名 ＳＪＩＳ＝＞ＵＴＦ８に変換
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               WK-ACCEPT3
                                               WK-UTF8

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL WK-ACCEPT3 (I:1) = SPACE
      *    *** BAT SJIS
                           MOVE    WK-ACCEPT3 (I:1) TO
                                              WK-TEST10-EXEC  (I + 15:1)
                                              WK-TEST134-EXEC (I + 15:1)
                   END-PERFORM
                   MOVE   '.html'      TO     WK-TEST10-EXEC  (I + 15:5)
                                              WK-TEST134-EXEC (I + 15:5)

                   MOVE    34          TO      P
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL WK-UTF8 (I:1) = SPACE
      *    *** PYTHON UTF8
                           MOVE    WK-UTF8 (I:1) TO PIN1-REC  (P:1)
                           ADD     1           TO      WK-UTF8-LEN
                           ADD     1           TO      P
                   END-PERFORM
                   MOVE   '.html")'    TO      PIN1-REC (P:7)

           END-IF

           IF      WK-PIN1-CNT =       11
                   MOVE    13          TO      P
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL WK-ACCEPT1 (I:1) = SPACE
                           MOVE    WK-ACCEPT1 (I:1) TO PIN1-REC (P:1)
                           ADD     1           TO      P
                   END-PERFORM
                   MOVE    "'"         TO      PIN1-REC (P:1)
           END-IF

           IF      WK-PIN1-CNT =       21
                   MOVE    WK-ACCEPT2  TO      PIN1-REC (26:4)
           END-IF

           WRITE   POT1-REC    FROM    PIN1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** python 実行
       S110-10.

           CLOSE   POT1-F

      *    *** コマンド ＵＴＦ８に変更
      *     CALL    "SYSTEM"    USING   WK-CHCP-65001

      *     IF      RETURN-CODE NOT =   ZERO
      *             DISPLAY WK-PGM-NAME " BAT CHCP SYSTEM ERROR"
      *                     " RETURN-CODE=" RETURN-CODE
      *             STOP    RUN
      *     END-IF

           CALL    "SYSTEM"    USING   WK-PYTHON-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " python SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** TEST10,TEST134 実行
       S120-10.

           CALL    "SYSTEM"    USING   WK-TEST10-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST10 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           CALL    "SYSTEM"    USING   WK-TEST134-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST134 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** TEST54.PIN2 変更
       S130-10.

      *     MOVE    "CHANGE"    TO      WDE05-ID
      *     MOVE    WK-HENKAN   TO      WDE05-HENKAN
      *     MOVE    WK-MODE     TO      WDE05-MODE
      *     MOVE    100         TO      WDE05-BUF1-LEN
      *     MOVE    WK-PIN2-CNT TO      WDE05-BUF1-CNT
      *    *** 女優名 ＳＪＩＳ＝＞ＵＴＦ８に変換
      *     CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                 WK-ACCEPT3
      *                                 WK-UTF8

           MOVE     SPACE      TO      WK-ITEM1
                                       WK-ITEM2
           MOVE     ZERO       TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN
           UNSTRING PIN2-REC
                    DELIMITED BY ","
                    INTO
                    WK-ITEM1 COUNT WK-ITEM1-LEN
                    WK-ITEM2 COUNT WK-ITEM2-LEN
           END-UNSTRING

           IF       WK-ITEM2 (1:1) =   SPACE
                    MOVE     1          TO      WK-ITEM2-LEN
           END-IF

           IF      WK-ITEM1-LEN NOT =  ZERO
               AND WK-UTF8 (1:WK-ITEM1-LEN) = WK-ITEM1 (1:WK-ITEM1-LEN)

                   MOVE    SPACE       TO      WK-POT2-REC
                   MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO
                           WK-POT2-REC (1:WK-ITEM1-LEN)
                   COMPUTE P2 = WK-ITEM1-LEN + 1

                   MOVE    ","        TO      WK-POT2-REC (P2:1)
                   ADD     1          TO      P2

                   MOVE    WK-ITEM2 ( 1:WK-ITEM2-LEN) TO 
                           WK-POT2-REC (P2:WK-ITEM2-LEN)
                   ADD     WK-ITEM2-LEN TO     P2

                   MOVE    WK-TEST54-PIN2-1 TO 
                           WK-POT2-REC (P2:96)
                   ADD     96          TO      P2

                   MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO 
                           WK-POT2-REC (P2:WDE05-BUF2-LEN)
                   ADD     WDE05-BUF2-LEN TO   P2

                   MOVE    WK-TEST54-PIN2-3 TO 
                           WK-POT2-REC (P2:10)
                   ADD     10          TO      P2

                   MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO 
                           WK-POT2-REC (P2:WDE05-BUF2-LEN)
                   ADD     WDE05-BUF2-LEN TO   P2

                   MOVE    WK-TEST54-PIN2-5 TO 
                           WK-POT2-REC (P2:99)
                   ADD     99          TO      P2

                   MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO 
                           WK-POT2-REC (P2:WDE05-BUF2-LEN)
                   ADD     WDE05-BUF2-LEN TO   P2

                   MOVE    WK-TEST54-PIN2-7 TO 
                           WK-POT2-REC (P2:10)
                   ADD     10          TO      P2

                   MOVE    WK-ITEM1 (1:WK-ITEM1-LEN) TO 
                           WK-POT2-REC (P2:WDE05-BUF2-LEN)
                   ADD     WDE05-BUF2-LEN TO   P2

                   MOVE    WK-TEST54-PIN2-9 TO 
                           WK-POT2-REC (P2:4)
                   ADD     4           TO      P2

                   MOVE    WK-PIN2-CNT TO      SV-PIN2-CNT
                   MOVE    "Y"         TO      SW-EXPANSION

           ELSE
                   CONTINUE
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** TEST54.PIN2 変更 => POT2-REC
       S132-10.

           IF      PIN2-REC (1:9) =    "EXPANSION"

                   WRITE   POT2-REC    FROM    PIN2-REC
                   ADD     1           TO      WK-POT2-CNT

      *    *** 変更後、EXPANSION の次のレコードに出力
                   WRITE   POT2-REC    FROM    WK-POT2-REC
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   WRITE   POT2-REC    FROM    PIN2-REC
                   ADD     1           TO      WK-POT2-CNT
           .
       S132-EX.
           EXIT.

      *    *** TEST103.PRM1 変更
       S140-10.

           IF      PIN3-REC (26:WK-UTF8-LEN) = WK-UTF8 (1:WK-UTF8-LEN)
                   DISPLAY WK-PGM-NAME " WK-ACCEPT3 既に作成済です"
                           " WK-ACCEPT3=" WK-ACCEPT3
                  STOP    RUN
           END-IF

           IF      WK-ACCEPT4  =       PIN3-REC (1:1)
                   MOVE    "Y"         TO      SW-ACCEPT4
                   MOVE    PIN3-REC (2:3) TO   WK-NO

                   WRITE   POT3-REC    FROM    PIN3-REC
                   ADD     1           TO      WK-POT3-CNT
           ELSE

      *    *** 同じIDの最後に今回分追加
                   IF      SW-ACCEPT4  =       "Y"
                       AND WK-PIN3-LEN =       ZERO
                           MOVE    WK-ACCEPT4  TO      WK-POT3-ID
                           COMPUTE WK-POT3-NO = WK-NO-9 + 1
                           MOVE    WK-UTF8     TO      WK-POT3-NAME

                           WRITE   POT3-REC    FROM    WK-POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                           MOVE    "N"         TO      SW-ACCEPT4
                   END-IF

                   WRITE   POT3-REC    FROM    PIN3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-IF
           .
       S140-EX.
           EXIT.

      *    *** C.TEST104_1A.bat 既存分
       S150-10.

           WRITE   POT4-REC    FROM    PIN4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S150-EX.
           EXIT.

      *    *** C.TEST104_1A.bat 追加分
       S152-10.

      *    *** 02

           MOVE    SPACE       TO      POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    WK-TEST104-1 TO     POT4-REC
           MOVE    19          TO      P3

      *    *** SJIS
           MOVE    WK-ITEM3 (1:WK-ITEM3-LEN) TO
                   POT4-REC (P3:WK-ITEM3-LEN)
           ADD     WK-ITEM3-LEN TO   P3

           MOVE    " 02"       TO      POT4-REC (P3:4)
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST104 "  TO      POT4-REC
           MOVE    WK-POT3-REC (1:4) TO POT4-REC (9:4)
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST53 032 02" TO  POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST54  "  TO      POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

      *    *** 05

           MOVE    SPACE       TO      POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    WK-TEST104-1 TO     POT4-REC
           MOVE    19          TO      P3

      *    *** SJIS
           MOVE    WK-ITEM3 (1:WK-ITEM3-LEN) TO
                   POT4-REC (P3:WK-ITEM3-LEN)
           ADD     WK-ITEM3-LEN TO   P3

           MOVE    " 05"       TO      POT4-REC (P3:4)
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST104 "  TO      POT4-REC
           MOVE    WK-POT3-REC (1:4) TO POT4-REC (9:4)
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST53 032 05" TO  POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    "TEST54  "  TO      POT4-REC
           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S152-EX.
           EXIT.

      *    *** COPY 実行
       S160-10.

      *    *** PIN2 : TEST54.PIN2 の変更　A:女優の時のみ行う
           IF      WK-ACCEPT4  =       "A"
                   CLOSE   POT2-F

                   CALL    "SYSTEM"    USING   WK-COPY1-EXEC

                   IF      RETURN-CODE NOT =   ZERO
                           DISPLAY WK-PGM-NAME " BAT COPY1 SYSTEM ERROR"
                                   " RETURN-CODE=" RETURN-CODE
                           STOP    RUN
                   END-IF
           ELSE
                   CLOSE   POT2-F
           END-IF

           CLOSE   POT3-F
                   POT4-F

           CALL    "SYSTEM"    USING   WK-COPY2-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT COPY2 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           MOVE    WK-ACCEPT4  TO      WK-COPY3-EXEC (33:1)
           DISPLAY WK-COPY3-EXEC
           CALL    "SYSTEM"    USING   WK-COPY3-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT COPY3 SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S160-EX.
           EXIT.

      *    *** TEST103,TEST53,TEST54 実行
       S170-10.

      *    *** 02

      *    *** TEST104 は1回実行
           MOVE    WK-POT3-REC (1:4) TO WK-TEST104-EXEC (9:4)
           CALL    "SYSTEM"    USING   WK-TEST104-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST104"
                           " SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           MOVE    "02"        TO      WK-TEST53-EXEC (12:2)
           CALL    "SYSTEM"    USING   WK-TEST53-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST53 "
                           " SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           CALL    "SYSTEM"    USING   WK-TEST54-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST54"
                           " SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF



      *    *** 05

 
           MOVE    "05"        TO      WK-TEST53-EXEC (12:2)
           CALL    "SYSTEM"    USING   WK-TEST53-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST53 "
                           " SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF

           CALL    "SYSTEM"    USING   WK-TEST54-EXEC

           IF      RETURN-CODE NOT =   ZERO
                   DISPLAY WK-PGM-NAME " BAT TEST54"
                           " SYSTEM ERROR"
                           " RETURN-CODE=" RETURN-CODE
                   STOP    RUN
           END-IF
           .
       S170-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   PIN3-F
                   PIN4-F

      *    *** 別の場所でCLOSE
      *             POT1-F
      *             POT2-F
      *             POT3-F
      *             POT4-F

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
           MOVE    WK-PIN2-CNT2 TO     WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2  件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3  件数 = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4  件数 = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1  件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2  件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3  件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4  件数 = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
