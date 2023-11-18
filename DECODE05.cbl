      *    *** SJIS <=> UTF8 変換 サブルーチン
      *    *** 
      *    *** https://www.seiai.ed.jp/sys/text/java/utf8table.html から引用
      *    *** 00-7x	１バイト文字	US-ASCIIにおなじ
      *    *** 8x,9x,Ax,Bx	多バイト文字の２バイト目以降	
      *    *** Cx,Dx	2バイト文字の開始バイト	
      *    *** Ex	3バイト文字の開始バイト	漢字はおおむねこれで開始
      *    *** Fx	4バイト以上の文字の開始バイト	
      *    *** F0-F7は4バイト、F8-FBは5バイト、FC-FDは６バイト
      *    *** 
      *    *** SJIS=>UTF8 の時、下記範囲にある時、変換、
      *    *** 対応するコード無は"？"をセット
      *    *** MODE=AA 指定時、1バイトづつセット
      *    *** MODE=AK 指定時、X"2020"の時は、X"E38080"セット
      *    *** その他のコードは1バイトづつセット

      *    *** LDE05-BUF2 (L2:CNS-3) 等 L2の位置、項目範囲内でないと
      *    *** 他の項目の内容破壊する LDE05-BUF1 の内容等

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE05.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** この指定が無いとコンパイルエラーになる
      *    *** MOVE LENGTH(LDE05-BUF2) TO WK-BUF-LEN2 の所
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SJIS,UTF8 データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** UTF8 1 BYTE => 3 BYTE データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE05".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "KANJI\KANJI1.txt".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST61.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *     03  WK-SJIS         PIC  X(002) VALUE SPACE.
           03  WK-UTF8         PIC  X(003) VALUE SPACE.
           03  WK-UTF8-A       PIC  X(001) VALUE SPACE.
      *    *** SPACE
           03  WK-20           PIC  X(001) VALUE X"20".
      *    *** SPACE
           03  WK-2020         PIC  X(002) VALUE X"2020".
      *    *** ？ SJIS
           03  WK-8148         PIC  X(002) VALUE X"8148".
      *    *** SPACE UTF8
           03  WK-E38080       PIC  X(003) VALUE X"E38080".
      *    *** EFBEA0 = 漢字スペース（空白表示）
           03  WK-EFBEA0       PIC  X(003) VALUE X"EFBEA0".
      *    *** ？ UTF8
           03  WK-EFBC9F       PIC  X(003) VALUE X"EFBC9F".
           03  WK-UTF8-BYTE    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DATA.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L1    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R1    PIC  X(001) VALUE ZERO.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L2    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R2    PIC  X(001) VALUE ZERO.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L3    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R3    PIC  X(001) VALUE ZERO.
           03  WK-BUF1-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BUF2-L       BINARY-LONG SYNC VALUE ZERO.

      *    *** 変換前のデータの長さ
      *     03  LDE05-BUF1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** 変換後のデータの長さ
      *     03  WK-BUF2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX-DIGITS   VALUE '0123456789ABCDEF'.
             05  WK-HEX-DIGIT  OCCURS 16
                               PIC  X(001).

           03  WK-PIC.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-PIC-X      PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIC-HALF     REDEFINES WK-PIC 
                               PIC  9(004) COMP-X.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA01-G.
      *    *** SJIS で昇順
           03  TBL01-AREA      OCCURS 10000
                               ASCENDING KEY IS TBL01-SJIS
                               INDEXED BY TBL01-IDX.
      *    ***
             05  TBL01-SJIS    PIC  X(002) VALUE HIGH-VALUE.
      *    ***
             05  TBL01-UTF8-BYTE BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-UTF8    PIC  X(003) VALUE SPACE.

       01  TBL-AREA02-G.
      *    *** UTF8 で昇順
           03  TBL02-AREA      OCCURS 10000
                               ASCENDING KEY IS TBL02-UTF8
                               INDEXED BY TBL02-IDX.
      *    ***
             05  TBL02-SJIS    PIC  X(002) VALUE SPACE.
      *    ***
             05  TBL02-UTF8-BYTE BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-UTF8    PIC  X(003) VALUE HIGH-VALUE.

       01  TBL-AREA03-G.
      *    *** UTF8 追加分 1バイト系UTF8=>SJIS
           03  TBL03-AREA.
             05  TBL03-AREA2.
               07              PIC  X(003) VALUE X"208140".
               07              PIC  X(003) VALUE X"218149".
               07              PIC  X(003) VALUE X"22814E".
               07              PIC  X(003) VALUE X"238194".
               07              PIC  X(003) VALUE X"248190".
               07              PIC  X(003) VALUE X"258193".
               07              PIC  X(003) VALUE X"268195".
               07              PIC  X(003) VALUE X"27818C".
               07              PIC  X(003) VALUE X"288169".
               07              PIC  X(003) VALUE X"29816A".
               07              PIC  X(003) VALUE X"2A8196".
               07              PIC  X(003) VALUE X"2B817B".
               07              PIC  X(003) VALUE X"2C8143".
               07              PIC  X(003) VALUE X"2D817C".
               07              PIC  X(003) VALUE X"2E8144".
               07              PIC  X(003) VALUE X"2F815E".

               07              PIC  X(003) VALUE X"30824F".
               07              PIC  X(003) VALUE X"318250".
               07              PIC  X(003) VALUE X"328251".
               07              PIC  X(003) VALUE X"338252".
               07              PIC  X(003) VALUE X"348253".
               07              PIC  X(003) VALUE X"358254".
               07              PIC  X(003) VALUE X"368255".
               07              PIC  X(003) VALUE X"378256".
               07              PIC  X(003) VALUE X"388257".
               07              PIC  X(003) VALUE X"398258".
               07              PIC  X(003) VALUE X"3A8146".
               07              PIC  X(003) VALUE X"3B8147".
               07              PIC  X(003) VALUE X"3C8183".
               07              PIC  X(003) VALUE X"3D8181".
               07              PIC  X(003) VALUE X"3E8184".
               07              PIC  X(003) VALUE X"3F8148".

               07              PIC  X(003) VALUE X"408197".
               07              PIC  X(003) VALUE X"418260".
               07              PIC  X(003) VALUE X"428261".
               07              PIC  X(003) VALUE X"438262".
               07              PIC  X(003) VALUE X"448263".
               07              PIC  X(003) VALUE X"458264".
               07              PIC  X(003) VALUE X"468265".
               07              PIC  X(003) VALUE X"478266".
               07              PIC  X(003) VALUE X"488267".
               07              PIC  X(003) VALUE X"498268".
               07              PIC  X(003) VALUE X"4A8269".
               07              PIC  X(003) VALUE X"4B826A".
               07              PIC  X(003) VALUE X"4C826B".
               07              PIC  X(003) VALUE X"4D826C".
               07              PIC  X(003) VALUE X"4E826D".
               07              PIC  X(003) VALUE X"4F826E".

               07              PIC  X(003) VALUE X"50826F".
               07              PIC  X(003) VALUE X"518270".
               07              PIC  X(003) VALUE X"528271".
               07              PIC  X(003) VALUE X"538272".
               07              PIC  X(003) VALUE X"548273".
               07              PIC  X(003) VALUE X"558274".
               07              PIC  X(003) VALUE X"568275".
               07              PIC  X(003) VALUE X"578276".
               07              PIC  X(003) VALUE X"588277".
               07              PIC  X(003) VALUE X"598278".
               07              PIC  X(003) VALUE X"5A8279".
               07              PIC  X(003) VALUE X"5B816D".
               07              PIC  X(003) VALUE X"5C815F".
               07              PIC  X(003) VALUE X"5D816E".
               07              PIC  X(003) VALUE X"5E814F".
               07              PIC  X(003) VALUE X"5F8151".

               07              PIC  X(003) VALUE X"60814D".
               07              PIC  X(003) VALUE X"618281".
               07              PIC  X(003) VALUE X"628282".
               07              PIC  X(003) VALUE X"638283".
               07              PIC  X(003) VALUE X"648284".
               07              PIC  X(003) VALUE X"658285".
               07              PIC  X(003) VALUE X"668286".
               07              PIC  X(003) VALUE X"678287".
               07              PIC  X(003) VALUE X"688288".
               07              PIC  X(003) VALUE X"698289".
               07              PIC  X(003) VALUE X"6A828A".
               07              PIC  X(003) VALUE X"6B828B".
               07              PIC  X(003) VALUE X"6C828C".
               07              PIC  X(003) VALUE X"6D828D".
               07              PIC  X(003) VALUE X"6E828E".
               07              PIC  X(003) VALUE X"6F828F".

               07              PIC  X(003) VALUE X"708290".
               07              PIC  X(003) VALUE X"718291".
               07              PIC  X(003) VALUE X"728292".
               07              PIC  X(003) VALUE X"738293".
               07              PIC  X(003) VALUE X"748294".
               07              PIC  X(003) VALUE X"758295".
               07              PIC  X(003) VALUE X"768296".
               07              PIC  X(003) VALUE X"778297".
               07              PIC  X(003) VALUE X"788298".
               07              PIC  X(003) VALUE X"798299".
               07              PIC  X(003) VALUE X"7A829A".
               07              PIC  X(003) VALUE X"7B816F".
               07              PIC  X(003) VALUE X"7C8162".
               07              PIC  X(003) VALUE X"7D8170".
               07              PIC  X(003) VALUE X"7E8160".

             05  TBL03-AREA2-R REDEFINES TBL03-AREA2
                               OCCURS 95
                               INDEXED BY TBL03-IDX.
               07  TBL03-UTF8  PIC  X(001).
               07  TBL03-SJIS  PIC  X(002).

       01  TBL-AREA04-G.
      *    *** UTF8=>SJIS 変換不可データ
           03  TBL04-AREA      OCCURS 1000.
             05  TBL04-UTF8    PIC  X(003) VALUE SPACE.

       01  TBL-AREA05-G.
      *    *** UTF8 1 BYTE => UTF8 3 BYTE
           03  TBL05-AREA      OCCURS 158
                               ASCENDING KEY IS TBL05-UTF8-A
                               INDEXED BY TBL05-IDX.
             05  TBL05-UTF8-A  PIC  X(001) VALUE SPACE.
             05  TBL05-UTF8-1  PIC  X(003) VALUE SPACE.
             05  TBL05-UTF8-2  PIC  X(003) VALUE SPACE.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K-MAX           BINARY-LONG SYNC VALUE ZERO.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG SYNC VALUE 1.
           03  CNS-2           BINARY-LONG SYNC VALUE 2.
           03  CNS-3           BINARY-LONG SYNC VALUE 3.
           03  CNS-20          BINARY-LONG SYNC VALUE 20.
           03  CNS-40          BINARY-LONG SYNC VALUE 40.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".

       LINKAGE                 SECTION.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==LDE05==.

       01  LDE05-BUF1          PIC  X(001) ANY LENGTH.

       01  LDE05-BUF2          PIC  X(001) ANY LENGTH.

       PROCEDURE   DIVISION    USING   LDE05-DECODE05-AREA
                                       LDE05-BUF1
                                       LDE05-BUF2
           .
       M100-10.

           EVALUATE LDE05-ID

               WHEN "OPEN  "
      *    *** OPEN,READ PIN1,TBL01 SET
                   PERFORM S010-10     THRU    S010-EX

               WHEN "CHANGE"

                   IF      LDE05-BUF1-LEN =    ZERO
                       MOVE    LENGTH(LDE05-BUF1) TO     LDE05-BUF1-LEN
                   END-IF
                   IF      LDE05-BUF2-LEN =    ZERO
                       MOVE    LENGTH(LDE05-BUF2) TO     LDE05-BUF2-LEN
                   END-IF
                   MOVE    SPACE
                           TO LDE05-BUF2 (1:LENGTH(LDE05-BUF2))

                   EVALUATE LDE05-HENKAN
                       WHEN "SU"
      *    *** SJIS => UTF8
                           PERFORM S100-10     THRU    S100-EX

                       WHEN "US"
      *    *** UTF8 => SJIS
                           PERFORM S200-10     THRU    S200-EX

                       WHEN "UU"
      *    *** UTF8 => UTF8
                           PERFORM S300-10     THRU    S300-EX

                       WHEN OTHER
                            DISPLAY WK-PGM-NAME 
                                    " LDE05-HENKAN PARA ERROR="
                                    LDE05-HENKAN
                            DISPLAY WK-PGM-NAME
                                    " LDE05-HENKAN SU(SJIS=>"
                                    "UTF8) US(UTF8=>SJIS) 指定"
                            STOP    RUN
                   END-EVALUATE

                   PERFORM VARYING L4 FROM LDE05-BUF2-LEN BY -1
                           UNTIL L4 < 1
                              OR LDE05-BUF2 (L4:1) NOT = SPACE
                           CONTINUE
                   END-PERFORM
      *    *** 変換後長さセット
                   MOVE    L4          TO      LDE05-BUF2-LEN

               WHEN "CLOSE "
                   PERFORM S900-10     THRU    S900-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDE05-ID PARA ERROR="
                           LDE05-ID
                   DISPLAY WK-PGM-NAME 
                           " LDE05-ID OPEN,CHANGE,CLOSE 指定"
                   STOP    RUN
           END-EVALUATE

           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN,READ PIN1,TBL01 SET
       S010-10.

      *    *** OPEN
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** PIN1 DATA ｽﾄｱｰ
      *    *** 区
                   IF      PIN1-REC (1:3) =    X"E58CBA"
                        OR PIN1-REC (22:1) =   X"09"
                        OR WK-PIN1-CNT <       15
                        OR WK-PIN1-LEN =       ZERO
                           CONTINUE
                   ELSE

      *    *** PIN1 DATA ｽﾄｱｰ
                           PERFORM S032-10     THRU    S032-EX
                   END-IF
      *    *** READ PIN1
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** UTF8 追加ストアー
           PERFORM S050-10     THRU    S050-EX

      *    *** READ PIN2
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** PIN2 DATA ｽﾄｱｰ
                   PERFORM S042-10     THRU    S042-EX

      *    *** READ PIN2
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

           SORT    TBL01-AREA
                   ASCENDING  KEY TBL01-SJIS

      *     MOVE    "P"         TO    WFD-ID
      *     CALL    "FILEDUMP" USING  WFD-FILEDUMP-AREA
      *                               TBL-AREA01-G

           SORT    TBL02-AREA
                   ASCENDING KEY TBL02-UTF8

           SORT    TBL05-AREA
                   ASCENDING KEY TBL05-UTF8-A
           .
       S010-EX.
           EXIT.

      *    *** OPEN
       S020-10.

           DISPLAY WK-PGM-NAME " START"

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           SET     TBL01-IDX   TO      1
           SET     TBL02-IDX   TO      1
           SET     TBL05-IDX   TO      1
           .
       S020-EX.
           EXIT.

      *    *** READ PIN1
       S030-10.
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
       S030-EX.
           EXIT.

      *    *** PIN1 DATA ｽﾄｱｰ
       S032-10.

           IF      TBL01-IDX   >       10000
                   DISPLAY WK-PGM-NAME " TBL01 OVER TBL01-IDX="
                           TBL01-IDX
                   STOP    RUN
           END-IF

           IF      TBL02-IDX   >       10000
                   DISPLAY WK-PGM-NAME " TBL02 OVER TBL02-IDX="
                           TBL02-IDX
                   STOP    RUN
           END-IF

      *    *** MAINでOPEN,CLOSE していれば,FILEDUMP 使用可能
      *    *** やっぱり、使えない？FILEDUMP 終了件数表示されるが、
      *    *** ファイル内容出力されていない
      *     IF (WK-PIN1-CNT >= 50   AND <= 50 ) 
      *     IF (WK-PIN1-CNT >= 8000 AND <= 8500 )
      *     IF (WK-PIN1-CNT >= 8000)
      *        MOVE    "P"         TO      WFD-ID
      *        MOVE    2           TO      WFD-SU
      *        CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                    PIN1-REC
      *        CALL    "COBDUMP"   USING   PIN1-REC (1:80)
      *     END-IF

      *    *** SJIS DATA
           MOVE    PIN1-REC (12:1) TO  WK-DATA-L1
           MOVE    PIN1-REC (13:1) TO  WK-DATA-R1
           MOVE    PIN1-REC (14:1) TO  WK-DATA-L2
           MOVE    PIN1-REC (15:1) TO  WK-DATA-R2
           MOVE    "2"         TO      WK-DATA-L3
           MOVE    "0"         TO      WK-DATA-R3
      *     MOVE    9           TO      WK-BUF1-L
      *     MOVE    2           TO      WK-BUF2-L
      *    *** %LR%LR... 16進変換サブルーチン流用する
           CALL    "DECODE02"  USING   WK-DATA
                                       WK-BUF1-L
                                       TBL01-SJIS (TBL01-IDX)
                                       WK-BUF2-L
           MOVE    TBL01-SJIS (TBL01-IDX) TO
                                       TBL02-SJIS (TBL02-IDX)

      *    *** UTF8 DATA
           MOVE    PIN1-REC (22:1) TO  WK-DATA-L1
           MOVE    PIN1-REC (23:1) TO  WK-DATA-R1

           IF      PIN1-REC (24:1) =   X"09"
                   MOVE    "2"         TO     WK-DATA-L2
                   MOVE    "0"         TO     WK-DATA-R2
                   MOVE    "2"         TO     WK-DATA-L3
                   MOVE    "0"         TO     WK-DATA-R3
                   MOVE    1           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
           ELSE
               MOVE    PIN1-REC (24:1) TO  WK-DATA-L2
               MOVE    PIN1-REC (25:1) TO  WK-DATA-R2

               IF      PIN1-REC (26:1) =   X"09"
                   MOVE    "2"         TO     WK-DATA-L3
                   MOVE    "0"         TO     WK-DATA-R3
                   MOVE    2           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
               ELSE
                   MOVE    PIN1-REC (26:1) TO  WK-DATA-L3
                   MOVE    PIN1-REC (27:1) TO  WK-DATA-R3
                   MOVE    3           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
               END-IF
           END-IF

      *    *** %LR%LR... 16進変換サブルーチン流用する
      *     MOVE    9           TO      WK-BUF1-L
      *     MOVE    3           TO      WK-BUF2-L
           CALL    "DECODE02"  USING   WK-DATA
                                       WK-BUF1-L
                                       TBL01-UTF8 (TBL01-IDX)
                                       WK-BUF2-L
           MOVE    TBL01-UTF8 (TBL01-IDX) TO
                                       TBL02-UTF8 (TBL02-IDX)

      *    *** 次回 １加算しておく
           SET     TBL01-IDX   UP BY   1
           SET     TBL02-IDX   UP BY   1
           .
       S032-EX.
           EXIT.

      *    *** READ PIN2
       S040-10.

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** PIN2 DATA ｽﾄｱｰ
       S042-10.

           IF      TBL05-IDX   >       158
                   DISPLAY WK-PGM-NAME " TBL05 OVER TBL05-IDX="
                           TBL05-IDX
                   STOP    RUN
           END-IF

           MOVE    PIN2-REC (01:1) TO  TBL05-UTF8-A (TBL05-IDX)
           MOVE    PIN2-REC (03:3) TO  TBL05-UTF8-1 (TBL05-IDX)
           MOVE    PIN2-REC (07:3) TO  TBL05-UTF8-2 (TBL05-IDX)

      *    *** 次回 １加算しておく
           SET     TBL05-IDX   UP BY   1
           .
       S042-EX.
           EXIT.

      *    *** UTF8 1バイト系追加
       S050-10.

           SET     TBL03-IDX   TO      1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 95
                   MOVE    TBL03-UTF8 (TBL03-IDX) TO
                           TBL02-UTF8 (TBL02-IDX)
                   MOVE    1           TO      
                           TBL02-UTF8-BYTE (TBL02-IDX)
                   MOVE    TBL03-SJIS (TBL03-IDX) TO
                           TBL02-SJIS (TBL02-IDX)

      *    *** 次回 １加算しておく
                   SET     TBL03-IDX   UP BY   1
                   SET     TBL02-IDX   UP BY   1
           END-PERFORM
           .
       S050-EX.
           EXIT.

      *    *** SJIS => UTF8
       S100-10.

           MOVE    1           TO      L2
      *    *** SJIS 2バイト系のみ対応、2バイトおきにチェックする
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL L1 > LDE05-BUF1-LEN - 1
                   EVALUATE TRUE
                       WHEN ( LDE05-BUF1 (L1:2) >= X"8140" AND 
                              LDE05-BUF1 (L1:2) <= X"9FFC" ) OR
                            ( LDE05-BUF1 (L1:2) >= X"E040" AND 
                              LDE05-BUF1 (L1:2) <= X"EAA4" )
      *    *** CHANGE SJIS => UTF8
                            PERFORM S110-10     THRU    S110-EX
                            ADD     1           TO      L1
                       WHEN OTHER
                           IF      LDE05-MODE  =       "AA"
                                   MOVE    L2          TO      L3
                               IF      L3          <=     LDE05-BUF2-LEN
                                       MOVE    LDE05-BUF1 (L1:1) TO
                                               LDE05-BUF2 (L2:1)
                               END-IF
                               ADD     1           TO      L2
                           ELSE
                               IF      LDE05-BUF1 (L1:CNS-2) =  WK-2020
                                       ADD     L2 2        GIVING L3
                                   IF      L3     <=  LDE05-BUF2-LEN
      *                              MOVE    X"E38080"      TO
      *    *** X"E38080" だとMOVE内容が別の常数になってしまう
      *                                     MOVE    WK-E38080     TO
                                       MOVE    WK-EFBEA0     TO
                                               LDE05-BUF2 (L2:CNS-3)
      *                           CALL "COBDUMP" USING LDE05-BUF2 (L2:CNS-3)
                                   END-IF
                                   ADD     1           TO      L1
                                   ADD     3           TO      L2
                               ELSE
                                   MOVE    L2          TO      L3
                                   IF      L3          <= LDE05-BUF2-LEN
                                       MOVE    LDE05-BUF1 (L1:1) TO
                                               LDE05-BUF2 (L2:1)
                                   END-IF
                                   ADD     1           TO      L2
                               END-IF
                           END-IF
                   END-EVALUATE
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** CHANGE SJIS => UTF8
       S110-10.

           SEARCH  ALL TBL01-AREA
               AT END
      *    *** ？ セット
                   ADD     L2 2        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    WK-EFBC9F TO    LDE05-BUF2 (L2:CNS-3)
                   END-IF
                   ADD     3           TO      L2

      *    *** 部分参照でAND指定で指定すると、内容違ってもＨＩＴしてしまう
               WHEN TBL01-SJIS (TBL01-IDX) =   LDE05-BUF1 (L1:CNS-2)
                   EVALUATE TBL01-UTF8-BYTE (TBL01-IDX)
                       WHEN 3
                           ADD     L2 2        GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-3)
                           END-IF
                           ADD     3           TO      L2
                       WHEN 2
                           ADD     L2 1        GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-2)
                           END-IF
                           ADD     2           TO      L2
                       WHEN 1
                           MOVE    L2          TO      L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-1)
                           END-IF
                           ADD     1           TO      L2
                   END-EVALUATE

           END-SEARCH

           .
       S110-EX.
           EXIT.

      *    *** UTF8 => SJIS ４，５，６バイト系は考慮しない
      *    *** UTF8 => SJIS
       S200-10.

           MOVE    1           TO      L2
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL L1 > LDE05-BUF1-LEN
                   EVALUATE TRUE

      *    *** UTF8 ３バイト系
                       WHEN LDE05-BUF1 (L1:1) >= X"E0" AND <= X"EF"
                           IF      L1          >      LDE05-BUF1-LEN - 2
                               CONTINUE
                           ELSE
                               MOVE    3           TO      WK-UTF8-BYTE
      *    *** CHANGE UTF8 => SJIS ３バイト系 ＵＴＦ８
                               PERFORM S210-10     THRU    S210-EX
                           END-IF
                           ADD     2           TO      L1

      *    *** UTF8 ２バイト系
                       WHEN LDE05-BUF1 (L1:1) >= X"C0" AND <= X"DF"
                           IF      L1          >      LDE05-BUF1-LEN - 1
                               CONTINUE
                           ELSE
                               MOVE    2           TO      WK-UTF8-BYTE
      *    *** CHANGE UTF8 => SJIS ３バイト系 ＵＴＦ８
                               PERFORM S210-10     THRU    S210-EX
                           END-IF
                           ADD     1           TO      L1

      *    *** UTF8 １バイト系
      *                WHEN LDE05-BUF1 (L1:1) >= X"00"  AND <= X"7F"
                       WHEN OTHER
                           IF      LDE05-MODE  =       "AA"
                               MOVE    L2          TO      L3
                               IF      L3          <=     LDE05-BUF2-LEN
                                   MOVE    LDE05-BUF1 (L1:1) TO
                                           LDE05-BUF2 (L2:1)
                               END-IF
                               ADD     1           TO      L2
                           ELSE
                               MOVE    1           TO      WK-UTF8-BYTE
      *    *** CHANGE UTF8 => SJIS ３バイト系 ＵＴＦ８
                               PERFORM S210-10 THRU    S210-EX
                           END-IF
                   END-EVALUATE
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CHANGE UTF8 => SJIS ３バイト系 ＵＴＦ８
       S210-10.

           EVALUATE WK-UTF8-BYTE
               WHEN 3
                   MOVE    LDE05-BUF1 (L1:CNS-3) TO   WK-UTF8
               WHEN 2
                   MOVE    LDE05-BUF1 (L1:CNS-2) TO   WK-UTF8 (1:2)
                   MOVE    WK-20              TO   WK-UTF8 (3:1)
               WHEN 1
                   MOVE    LDE05-BUF1 (L1:CNS-1) TO   WK-UTF8 (1:1)
                   MOVE    WK-2020            TO   WK-UTF8 (2:2)
           END-EVALUATE

           SEARCH  ALL TBL02-AREA
               AT END
      *    *** ？ セット
                   ADD     L2 1        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    WK-8148 TO      LDE05-BUF2 (L2:CNS-2)
                   END-IF
      *    *** debug の為、display 残しておく
                   DISPLAY WK-PGM-NAME
                           " S210-10 " " BUF1-CNT=" LDE05-BUF1-CNT
                           " L1=" L1 " L2=" L2
                   CALL    "COBDUMP"   USING   WK-UTF8
                   ADD     2           TO      L2

      *             MOVE    "P"         TO      WFD-ID
      *             MOVE    "UTF8"      TO      WFD-KANJI
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         LDE05-BUF1
      *                                           (1:LDE05-BUF1-LEN)
                   MOVE    "N"         TO      SW-SET
                   PERFORM VARYING K FROM 1 BY 1
                           UNTIL K > K-MAX
                           IF      TBL04-UTF8 (K) = WK-UTF8
                               MOVE    "Y"         TO      SW-SET
                           END-IF
                   END-PERFORM
                   IF      SW-SET      =       "N"

                           IF      K           >       1000
                                   DISPLAY WK-PGM-NAME 
                                           " TBL04 OVER K=" K
                                   STOP    RUN
                           END-IF

                           MOVE    WK-UTF8     TO      TBL04-UTF8 (K)
                           IF      K           >       K-MAX
                               MOVE    K           TO      K-MAX
                           END-IF
                   END-IF

      *    *** 部分参照でAND指定で指定すると、内容違ってもＨＩＴしてしまう
               WHEN TBL02-UTF8 (TBL02-IDX) = WK-UTF8

                   ADD     L2 1        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    TBL02-SJIS (TBL02-IDX) TO
                               LDE05-BUF2 (L2:CNS-2)
                   END-IF
                   ADD     2           TO      L2
           END-SEARCH
           .
       S210-EX.
           EXIT.

      *    *** UTF8 => UTF8
       S300-10.

           MOVE    1           TO      L2
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL L1 > LDE05-BUF1-LEN
                   EVALUATE TRUE

      *    *** UTF8 ３バイト系
                       WHEN LDE05-BUF1 (L1:1) >= X"E0" AND <= X"EF"
      *                                  IF      L1  > LDE05-BUF1-LEN - 2
      *                                      CONTINUE
      *                                  ELSE
      *                                      MOVE    3    TO WK-UTF8-BYTE
      *                                      PERFORM S310-10 THRU S310-EX
      *                                  END-IF

      *    *** UTF8 漢字コードの時そのままセット
                           ADD     L2 2        GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               IF      LDE05-BUF1 (L1:CNS-3) =
                                       WK-E38080
      *    *** 漢字スペース、無表示に変更
                                   MOVE    WK-EFBEA0   TO
                                           LDE05-BUF2 (L2:CNS-3)
                               ELSE
                                   MOVE    LDE05-BUF1 (L1:CNS-3) TO
                                           LDE05-BUF2 (L2:CNS-3)
                               END-IF
                           END-IF
                           ADD     3           TO      L2

                           ADD     2           TO      L1

      *    *** UTF8 ２バイト系
      *                              WHEN LDE05-BUF1 (L1:1) >= X"C0"
      *                                                 AND <= X"DF"
      *                                  IF      L1  > LDE05-BUF1-LEN - 1
      *                                      CONTINUE
      *                                  ELSE
      *                                      MOVE    2    TO WK-UTF8-BYTE
      *                                      PERFORM S310-10 THRU S310-EX
      *                                  END-IF
      *                                  ADD     1           TO      L1

      *    *** UTF8 １バイト系
      *                              WHEN LDE05-BUF1 (L1:1) >= X"00" 
      *                                                 AND <= X"7F"
                       WHEN OTHER
                           IF      LDE05-MODE  =       "AA"
                               MOVE    L2          TO      L3
                               IF      L3          <=     LDE05-BUF2-LEN
                                   MOVE    LDE05-BUF1 (L1:1) TO
                                           LDE05-BUF2 (L2:1)
                               END-IF
                               ADD     1           TO      L2
                           ELSE
                               MOVE    1           TO      WK-UTF8-BYTE
      *    *** CHANGE UTF8 => UTF8
                               PERFORM S310-10     THRU    S310-EX
                           END-IF
                   END-EVALUATE
           END-PERFORM
           .
       S300-EX.
           EXIT.

      *    *** CHANGE UTF8 => UTF8
       S310-10.

           EVALUATE WK-UTF8-BYTE
      *         WHEN 3
      *             MOVE    LDE05-BUF1 (L1:CNS-3) TO   WK-UTF8
      *         WHEN 2
      *             MOVE    LDE05-BUF1 (L1:CNS-2) TO   WK-UTF8 (1:2)
      *             MOVE    WK-20              TO   WK-UTF8 (3:1)
               WHEN 1
                   MOVE    LDE05-BUF1 (L1:CNS-1) TO   WK-UTF8-A
      *             MOVE    WK-2020            TO   WK-UTF8 (2:2)
           END-EVALUATE

           SEARCH  ALL TBL05-AREA
               AT END
      *    *** ？ セット
                   ADD     L2 2        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    WK-EFBC9F TO    LDE05-BUF2 (L2:CNS-3)
                   END-IF
                   ADD     3           TO      L2

      *    *** 部分参照でAND指定で指定すると、内容違ってもＨＩＴしてしまう
               WHEN TBL05-UTF8-A (TBL05-IDX) = WK-UTF8-A

                   ADD     L2 2        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    TBL05-UTF8-1 (TBL05-IDX) TO
                               LDE05-BUF2 (L2:CNS-3)
                   END-IF
                   ADD     3           TO      L2
           END-SEARCH
           .
       S310-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

      *    *** UTF8 => SJIS 変換アンマッチ分
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > K-MAX
                   MOVE    "X"         TO      WFD-ID
                   MOVE    2           TO      WFD-SU
                   MOVE    "UTF8"      TO      WFD-KANJI
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               TBL04-UTF8 (K)
           END-PERFORM

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           .
       S900-EX.
           EXIT.
