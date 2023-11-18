      *    *** FILEDUMP サブルーチン
      *    *** OPEN COBOL SOURCE 改良した　小文字部分はほほオリジナル

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILEDUMP.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** LFD-SU = 1の時、ダンプデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** LFD-SU = 2の時、ダンプデータ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC            PIC  X(200).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC            PIC  X(200).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILEDUMP".

           03  WK-POT1-F-NAME  PIC  X(032) VALUE "FILEDUMP.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "FILEDUMP.POT2".
           03  WK-POTN-F-NAME  PIC  X(032) VALUE SPACE.

           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-CNT.
             05 FILLER         PIC  X(001) VALUE LOW-VALUE.
             05 WK-CNT-X       PIC  X(001) VALUE LOW-VALUE.
           03  WK-CNT-R        REDEFINES   WK-CNT
                               PIC  9(004) COMP-X.

      *    *** スペースクリアーしない,INPUT 可変長があるので、MAINでは項目長の
      *    *** 場所のみ参照とする、スペースクリアーに時間が掛かるため

       01  WK-BUF2.
           03  WK-BUF2-L-TBL.
             05  WK-BUF2-L     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-R-TBL.
             05  WK-BUF2-R     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-LR-TBL.
             05  WK-BUF2-LR-TBL2 OCCURS 65536.
               07  WK-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WK-BUF2-R2  PIC  X(001) VALUE SPACE.

       01  Addr-Pointer        POINTER.

       01  Addr-Sub            BINARY-CHAR.

       01  Addr-Value          BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Length       BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES
                               PIC  X(001).

       01  Left-Nibble         COMP-5
                               PIC  9(002).
       01  Nibble              REDEFINES Left-Nibble 
                               BINARY-CHAR.

       01  Right-Nibble        COMP-5 
                               PIC  9(002).

       01  Output-Detail1.
           05  OD-ASCII        OCCURS 101 TIMES
                               PIC  X(001).
           05                  PIC  X(002).
           05  OD-I12          PIC  X(024).

       01  Output-Detail12.
           05  OD-ASCII-X.
             07  OD-ASCII2     OCCURS 180 TIMES
                               PIC  X(001).
           05  OD-I122         REDEFINES OD-ASCII-X
                               PIC  X(180).

       01  OD-I11.
           05  OD-Byte         PIC  ZZ,ZZ9
           05  OD-SURA         PIC  X(001)
           05  OD-LENG         PIC  ZZ,ZZ9
           05  OD-SEQ          PIC  ZZZ,ZZZ,ZZ9.

       01  Output-Detail2.
           05  OD-Hex          OCCURS 100 TIMES.
             10  OD-Hex-1      PIC  X(001).

       01  Output-Detail3.
           05  OD-Hex          OCCURS 100 TIMES.
             10  OD-Hex-2      PIC  X(001).

       01  Output-Detail4.
           05  OD4-SEQ         PIC  ZZZ,ZZZ,ZZ9.
           05  FILLER          PIC  X(001).
           05  OD4-ITEM        PIC  X(010).
           05  FILLER          PIC  X(001).
           05  OD4-ASCII-X.
             07  OD4-ASCII     OCCURS 40 TIMES
                               PIC  X(001).
           05  FILLER          PIC  X(001).
           05  OD4-I12         PIC  X(080).
           05  FILLER          PIC  X(001).
           05  OD4-I13         PIC  X(024).

       01  Output-Detail42.
           05  OD4-SEQ2        PIC ZZZ,ZZZ,ZZ9.
           05  FILLER          PIC  X(001).
           05  OD4-ITEM2       PIC  X(010).
           05  FILLER          PIC  X(001).
           05  OD4-ASCII2-X.
             07  OD4-ASCII2    OCCURS 180 TIMES 
                               PIC X(001).
           05  OD4-I22         REDEFINES OD4-ASCII2-X
                               PIC X(180).

       01  OD4-I11.
           05  OD4-Hex         OCCURS 40 TIMES.
             10  OD4-Hex-1     PIC  X(001).
             10  OD4-Hex-2     PIC  X(001).

       01  Output-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Output-Header-1.
           05  FILLER          PIC  X(010) VALUE "         1".
           05  FILLER          PIC  X(010) VALUE "         2".
           05  FILLER          PIC  X(010) VALUE "         3".
           05  FILLER          PIC  X(010) VALUE "         4".
           05  FILLER          PIC  X(010) VALUE "         5".
           05  FILLER          PIC  X(010) VALUE "         6".
           05  FILLER          PIC  X(010) VALUE "         7".
           05  FILLER          PIC  X(010) VALUE "         8".
           05  FILLER          PIC  X(010) VALUE "         9".
           05  FILLER          PIC  X(010) VALUE "         0".

       01  Output-Header-2.
           05  FILLER          PIC  X(100) VALUE ALL "1234567890".
           05  FILLER          PIC  X(003) VALUE SPACE.
           05  FILLER          PIC  X(020) VALUE "  Byte/   LEN       ".
           05  FILLER          PIC  X(004) VALUE " SEQ".

       01  Output-Header-3.
           05  FILLER          PIC  X(023) VALUE SPACE.
           05  FILLER          PIC  X(040) VALUE ALL "1234567890".
           05  FILLER          PIC  X(001) VALUE SPACE.
           05  FILLER          PIC  X(010) VALUE " 1 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "11 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "21 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "31 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(001) VALUE SPACE.
           05  FILLER          PIC  X(020) VALUE "  Byte/   LEN       ".
           05  FILLER          PIC  X(004) VALUE " SEQ".

       01  PIC-XX.
           05  FILLER          PIC  X(001) VALUE LOW-VALUES.
           05  PIC-X           PIC  X(001).
       01  PIC-Halfword        REDEFINES PIC-XX 
                               PIC  9(004) COMP-X.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P1-2            BINARY-LONG SYNC VALUE ZERO.
           03  P2-2            BINARY-LONG SYNC VALUE ZERO.
           03  CNS-50          BINARY-LONG SYNC VALUE 50.

       01  SW-AREA.
           05  SW-KANJI        PIC  X(001) VALUE ZERO.
           05  SW-KANJI2       PIC  X(001) VALUE ZERO.
           05  SW-UTF8         PIC  X(001) VALUE "N"..

       LINKAGE                 SECTION.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==LFD==.

       01  Buffer              PIC X ANY LENGTH.

      *01  Buffer-Len          BINARY-LONG.

      *    *** Buffer-Len 指定無しは、そのレコード、項目の長さで出力
       PROCEDURE   DIVISION    USING   LFD-FILEDUMP-AREA,
                                       Buffer,
                                       OPTIONAL LFD-LEN
           .
       M100-10.

      *    *** OPEN
           EVALUATE TRUE
               WHEN LFD-ID      =       "O"
                   PERFORM S010-10      THRU    S010-EX
      *    *** RECORDの出力
               WHEN LFD-ID      =       "P"
                   PERFORM S100-10      THRU    S100-EX
      *    *** 項目の出力
               WHEN LFD-ID      =       "X"
                   PERFORM S200-10      THRU    S200-EX
      *    *** CLOSE
               WHEN LFD-ID      =       "C"
                   PERFORM S900-10      THRU    S900-EX
      *    *** ERROR
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LFD-ID ERROR LFD-ID=" LFD-ID
                   DISPLAY WK-PGM-NAME " ID=O(OPEN),P(RECORD-PUT),"
                           "X(ITEM-PUT),C(CLOSE) 指定"
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN
       S010-10.

           IF      LFD-PGM     NOT = SPACE
                   MOVE    WK-POT1-F-NAME TO   WK-POTN-F-NAME
                   STRING
                      LFD-PGM DELIMITED BY SPACE
                      "." DELIMITED BY SIZE
                      WK-POTN-F-NAME
                      INTO WK-POT1-F-NAME
                   END-STRING 

                   MOVE    WK-POT2-F-NAME TO   WK-POTN-F-NAME
                   STRING
                      LFD-PGM DELIMITED BY SPACE
                      "." DELIMITED BY SIZE
                      WK-POTN-F-NAME
                      INTO WK-POT2-F-NAME
                   END-STRING 
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

      *    *** 1バイト目から出力の為、サクラEDITでは目盛り表示あるため
      *    *** 規定値　先頭に１件だけヘッダ－出力
           IF      LFD-HED     =       "Y"
                   WRITE   POT1-REC    FROM    Output-Header-1
                   WRITE   POT1-REC    FROM    Output-Header-2
                   WRITE   POT1-REC    FROM    Output-Header-3
                   ADD     3            TO     WK-POT1-CNT

                   WRITE   POT2-REC    FROM    Output-Header-1
                   WRITE   POT2-REC    FROM    Output-Header-2
                   WRITE   POT2-REC    FROM    Output-Header-3
                   ADD     3            TO     WK-POT2-CNT
           END-IF
           .
       S010-EX.
           EXIT.

      *    *** RECORDの出力
      *    *** LFD-SUでPOT1,POT2どちらに出力するか指定
       S100-10.
           IF NUMBER-OF-CALL-PARAMETERS = 3
      *        MOVE Buffer-Len     TO Buffer-Length
              MOVE LFD-LEN     TO Buffer-Length
      *    *** 指定された長さが、項目長を超えてる時、項目長にする
      *        IF   Buffer-Len >   LENGTH(Buffer)  OR
      *             Buffer-Len =   0
              IF   LFD-LEN >   LENGTH(Buffer)
      *          OR
      *             LFD-LEN =   0
      *    *** 項目長ゼロも有効とする、出力対象外とする
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           ELSE
      *    *** 第3パラメータ(LFD-LEN)無い時、引き渡された項目の長さとする
              MOVE LENGTH(Buffer) TO Buffer-Length
           END-IF

           MOVE SPACE             TO Output-Detail1
                                     Output-Detail12
                                     Output-Detail2
                                     Output-Detail3
                                     OD-I11 OD-I12

           SET Addr-Pointer       TO ADDRESS OF Buffer

           MOVE    ZERO        TO      I
           MOVE    ZERO        TO      SW-KANJI SW-KANJI2

           MOVE    104         TO      P1
           MOVE    "N"         TO      SW-UTF8

           MOVE    ZERO        TO      Output-Sub
                                       P2

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length

                   ADD 1 TO Output-Sub
                            P2
      *    *** 漢字判定したとき、１回判定スキップ 
                   IF  LFD-KANJI = "SJIS"
                     IF  SW-KANJI = "1"
      *    *** SW-KANJI=1のとき、２バイト目
                       IF  SW-KANJI2 =  "1"
                           MOVE   ZERO   TO  SW-KANJI2
                           ADD    1      TO  I
                       ELSE
                           ADD    1      TO  I
                           MOVE   ZERO   TO  SW-KANJI
                           PERFORM  S210-10 THRU S210-EX
                       END-IF
                     ELSE
      *    *** 漢字チェック
                       ADD   1   TO   I
                       PERFORM  S210-10 THRU S210-EX
                     END-IF
                   END-IF

                   IF Output-Sub = 1
                      MOVE Buffer-Sub    TO OD-Byte
                                            L2
                      MOVE "/"           TO OD-SURA
                      MOVE Buffer-Length TO OD-LENG
                   END-IF

                   IF Buffer-Sub = 1
                      CALL    "DECODE03" USING Buffer
                                               Buffer-Length
                                               WK-BUF2
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** UTF-8 と思われる漢字の時、パディング文字をスペースに変更する
                   IF  LFD-KANJI = "UTF8"
                     IF  ( PIC-X >= X"E0" AND PIC-X <= X"EF" ) AND
                         ( Buffer (Buffer-Sub + 1: 1) >= X"80" AND
                           Buffer (Buffer-Sub + 1: 1) <= X"BF" )

                         MOVE    "Y"       TO     SW-UTF8
                         ADD     1         TO     P1
                         ADD     1         TO     P2

      *    *** 最終バイト コード３バイト無い時、その先の漢字バイトセット対応
                         IF  Output-Sub = 99
                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF

                         IF  Output-Sub = 100
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 1)

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF
                     END-IF
                   END-IF

      *    *** X"20"=SPACE ,X"7E"=~
      *    *** X"7E" ではカタカナ出ないので、X"FD"に変更
                   IF   (PIC-X < X"20")
                     OR (PIC-X = X"7F")
      *               OR (PIC-X > X"7E" AND PIC-X < X"A1")
      *               OR (PIC-X > X"DF")) AND
      *****             SW-KANJI = ZERO
                        MOVE SPACE TO OD-ASCII (Output-Sub)
                        MOVE SPACE TO OD-ASCII2 (P2)
                   ELSE
                        MOVE PIC-X TO OD-ASCII (Output-Sub)
                        MOVE PIC-X TO OD-ASCII2 (P2)
                   END-IF

      *             IF  LFD-TYPE = "M"

      *                 DIVIDE PIC-Halfword BY 16
      *                        GIVING Left-Nibble
      *                        REMAINDER Right-Nibble
      *                 ADD 1 TO Left-Nibble Right-Nibble
      *                 MOVE Hex-Digit (Left-Nibble)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE Hex-Digit (Right-Nibble)
      *                         TO OD-Hex-2 (Output-Sub)

      *    *** CALL "DECODE03" に変更する
      *                 MOVE WK-BUF2-L (Buffer-Sub)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE WK-BUF2-R (Buffer-Sub)
      *                         TO OD-Hex-2 (Output-Sub)
      *             END-IF

                   IF  Output-Sub = 100
                       IF  SW-KANJI = "1"
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII(101)
                       END-IF

                       MOVE  LFD-SEQ  TO OD-SEQ

                       MOVE  OD-I11   TO OD-I12
                       MOVE  OD-I11   TO OD-I122 (P1:24)

                       IF  LFD-SU = 1
                           MOVE  SPACE    TO   POT1-REC
                           WRITE POT1-REC
                           ADD   1        TO   WK-POT1-CNT

                           IF  LFD-HED  =  "A"
                             WRITE   POT1-REC    FROM    Output-Header-1
                             WRITE   POT1-REC    FROM    Output-Header-2
                             ADD   2        TO   WK-POT1-CNT
                           END-IF

                           IF  SW-UTF8 = "Y"
                               WRITE POT1-REC FROM Output-Detail12
      *                         CALL "COBDUMP" USING Output-Detail12
                           ELSE
                               WRITE POT1-REC FROM Output-Detail1
                           END-IF
                           ADD   1        TO   WK-POT1-CNT
                       ELSE
                           MOVE  SPACE    TO   POT2-REC
                           WRITE POT2-REC
                           ADD   1        TO   WK-POT2-CNT

                           IF  LFD-HED  =  "A"
                             WRITE   POT2-REC    FROM    Output-Header-1
                             WRITE   POT2-REC    FROM    Output-Header-2
                             ADD   2        TO   WK-POT2-CNT
                           END-IF
                           IF  SW-UTF8 = "Y"
                               WRITE POT2-REC FROM Output-Detail12
                           ELSE
                               WRITE POT2-REC FROM Output-Detail1
                           END-IF
                           ADD   1        TO   WK-POT2-CNT
                       END-IF

                       IF  LFD-TYPE = "M"

      *    *** CALL "DECODE03" に変更する
                           MOVE WK-BUF2-L-TBL (L2:100)
                                   TO Output-Detail2
                           MOVE WK-BUF2-R-TBL (L2:100)
                                   TO Output-Detail3
                           IF  LFD-SU  =  1
                               WRITE POT1-REC FROM Output-Detail2
                               WRITE POT1-REC FROM Output-Detail3
                               ADD   2        TO   WK-POT1-CNT
                           ELSE
                               WRITE POT2-REC FROM Output-Detail2
                               WRITE POT2-REC FROM Output-Detail3
                               ADD   2        TO   WK-POT2-CNT
                           END-IF
                       END-IF

      
                       MOVE SPACE   TO Output-Detail1
                                       Output-Detail12
                                       Output-Detail2
                                       Output-Detail3
                                       OD-I11 OD-I12

                       MOVE    ZERO        TO      I
                       MOVE    ZERO        TO      SW-KANJI SW-KANJI2

                       MOVE    104         TO      P1
                       MOVE    "N"         TO      SW-UTF8
                       MOVE    0           TO      Output-Sub
                                                   P2

      *                 SET Addr-Pointer UP BY 100
                   END-IF
           END-PERFORM

           IF  Output-Sub > 0
               MOVE  LFD-SEQ  TO OD-SEQ

               MOVE  OD-I11   TO OD-I12
                                 OD-I122 (P1:24)

               IF  LFD-SU = 1
                   MOVE  SPACE    TO   POT1-REC
                   WRITE POT1-REC
                   ADD   1        TO   WK-POT1-CNT

                   IF  LFD-HED  =  "A"
                       WRITE   POT1-REC    FROM    Output-Header-1
                       WRITE   POT1-REC    FROM    Output-Header-2
                       ADD   2        TO   WK-POT1-CNT
                   END-IF

                   IF  SW-UTF8 = "Y"
                       WRITE POT1-REC FROM Output-Detail12
                   ELSE
                       WRITE POT1-REC FROM Output-Detail1
                   END-IF
                   ADD   1        TO   WK-POT1-CNT
               ELSE
                   MOVE  SPACE    TO   POT2-REC
                   WRITE POT2-REC
                   ADD   1        TO   WK-POT2-CNT

                   IF  LFD-HED  =  "A"
                       WRITE   POT2-REC    FROM    Output-Header-1
                       WRITE   POT2-REC    FROM    Output-Header-2
                       ADD   2        TO   WK-POT2-CNT
                   END-IF

                   IF  SW-UTF8 = "Y"
                       WRITE POT2-REC FROM Output-Detail12
                   ELSE
                       WRITE POT2-REC FROM Output-Detail1
                   END-IF
                   ADD   1        TO   WK-POT2-CNT
               END-IF

               IF  LFD-TYPE = "M"
                   MOVE WK-BUF2-L-TBL (L2:Output-Sub)
                           TO Output-Detail2
                   MOVE WK-BUF2-R-TBL (L2:Output-Sub)
                           TO Output-Detail3
                   IF  LFD-SU  =  1
                       WRITE POT1-REC FROM Output-Detail2
                       WRITE POT1-REC FROM Output-Detail3
                       ADD   2        TO   WK-POT1-CNT
                   ELSE
                       WRITE POT2-REC FROM Output-Detail2
                       WRITE POT2-REC FROM Output-Detail3
                       ADD   2        TO   WK-POT2-CNT
                   END-IF
               END-IF
           END-IF
           . 
       S100-EX.
           EXIT.

      *    *** ID=X用 最初の100バイトか、項目ごとを出力
      *    *** LFD-TYPE=A(ASCII文字のみ）、=M(hex)も出力
      *    *** LFD-SEQ=RECORD.NO MAINで指定する
      *    *** LFD-ITEM　項目名10バイト以内で指定
       S200-10.
           IF NUMBER-OF-CALL-PARAMETERS = 3
      *        MOVE Buffer-Len     TO Buffer-Length
              MOVE LFD-LEN     TO Buffer-Length
      *    *** 指定された長さが、項目長を超えてる時、項目長にする
      *        IF   Buffer-Len >   LENGTH(Buffer)  OR
      *             Buffer-Len =   0
              IF   LFD-LEN >   LENGTH(Buffer) 
      *    *** 項目長ゼロも有効とする、出力対象外とする
      *         OR
      *             LFD-LEN =   0
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           ELSE
              MOVE LENGTH(Buffer) TO Buffer-Length
           END-IF

           MOVE SPACE             TO Output-Detail1
                                     Output-Detail12
                                     Output-Detail2
                                     Output-Detail3
                                     Output-Detail4
                                     Output-Detail42
                                     OD-I11 OD-I12
                                     OD4-I12
                                     OD4-I13
                                     OD4-I11

      *     SET Addr-Pointer       TO ADDRESS OF Buffer

           MOVE    ZERO        TO      I
           MOVE    ZERO        TO      SW-KANJI SW-KANJI2

           MOVE    104         TO      P1
           MOVE    42          TO      P1-2

           MOVE    "N"         TO      SW-UTF8

           MOVE    ZERO        TO      Output-Sub
                                       P2
                                       P2-2

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length
      *    *** ID=X の時、ＭＡＸ＝１００バイトまで
                        OR Output-Sub = 100

                   ADD 1 TO Output-Sub
                            P2
                            P2-2
      *    *** 漢字判定したとき、１回判定スキップ 
                   IF  LFD-KANJI = "SJIS"
                     IF  SW-KANJI = "1"
      *    *** SW-KANJI=1のとき、２バイト目
                       IF  SW-KANJI2 =  "1"
                           MOVE   ZERO   TO  SW-KANJI2
                           ADD    1      TO  I
                       ELSE
                           ADD    1      TO  I
                           MOVE   ZERO   TO  SW-KANJI
                           PERFORM  S210-10 THRU S210-EX
                       END-IF
                     ELSE
      *    *** 漢字チェック
                       ADD   1   TO   I
                       PERFORM  S210-10 THRU S210-EX
                     END-IF
                   END-IF

                   IF Output-Sub = 1
                      MOVE Buffer-Sub    TO OD-Byte
                                            L2
                      MOVE "/"           TO OD-SURA
                      MOVE Buffer-Length TO OD-LENG
                   END-IF

                   IF Buffer-Sub = 1
      *    *** ID=X は １００バイトまでしか表示しない為
                      IF  Buffer-Length > 100
                          MOVE    100  TO  L4
                          CALL    "DECODE03" USING Buffer
                                                   L4
                                                   WK-BUF2
                      ELSE
                          CALL    "DECODE03" USING Buffer
                                                   Buffer-Length
                                                   WK-BUF2
                      END-IF
                   END-IF

                   IF  Output-Sub = 100
                       IF  SW-KANJI = "1"
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII(101)
                       END-IF
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** UTF-8 と思われる漢字の時、パディング文字をスペースに変更する
                   IF  LFD-KANJI = "UTF8"
                     IF  ( PIC-X >= X"E0" AND PIC-X <= X"EF" ) AND
                         ( Buffer (Buffer-Sub + 1: 1) >= X"80" AND
                                                      <= X"BF" )
                         MOVE    "Y"       TO     SW-UTF8
                         ADD     1         TO     P1
                                                  P2
                                                  P2-2
      *    *** 最後の文字漢字 でない時、ずらさない
                         IF ( Buffer-Length     = Output-Sub ) OR
                            ( Buffer-Length - 1 = Output-Sub )
                             CONTINUE
                         ELSE
                             ADD     1         TO     P1-2
                         END-IF

                         IF  Output-Sub = 99

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF

                         IF  Output-Sub = 100

                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 1)

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF
                     END-IF
                   END-IF

      *    *** X"20"=SPACE ?,X"7E"=~
      *    *** X"7E" ではカタカナ出ないので、X"FD"に変更
                   IF    (PIC-X < X"20")
                      OR (PIC-X = X"7F")
      *               OR (PIC-X > X"7E" AND PIC-X < X"A1")
      *               OR (PIC-X > X"DF")) AND
      *****               SW-KANJI = ZERO
                        MOVE SPACE TO OD-ASCII (Output-Sub)
                                      OD-ASCII2 (P2)

                        IF Buffer-Length <= 40
                           MOVE SPACE TO OD4-ASCII (Output-Sub)
                                         OD4-ASCII2 (P2-2)
                        END-IF
                   ELSE
                        MOVE PIC-X TO OD-ASCII (Output-Sub)
                                      OD-ASCII2 (P2)

                        IF Buffer-Length <= 40
                           MOVE PIC-X TO OD4-ASCII (Output-Sub)
                                         OD4-ASCII2 (P2-2)
                        END-IF
                   END-IF

      *             IF  LFD-TYPE = "M" OR 
      *             IF  Buffer-Length <= 40

      *                 DIVIDE PIC-Halfword BY 16
      *                        GIVING Left-Nibble
      *                        REMAINDER Right-Nibble
      *                ADD 1 TO Left-Nibble Right-Nibble
      *                 MOVE Hex-Digit (Left-Nibble)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE Hex-Digit (Right-Nibble)
      *                         TO OD-Hex-2 (Output-Sub)

      *    *** CALL "DECODE03" に変更する
      *                 MOVE WK-BUF2-L (Buffer-Sub)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE WK-BUF2-R (Buffer-Sub)
      *                         TO OD-Hex-2 (Output-Sub)

      *                 IF Buffer-Length <= 40
      *                     MOVE Hex-Digit (Left-Nibble)
      *                         TO OD4-Hex-1 (Output-Sub)
      *                     MOVE Hex-Digit (Right-Nibble)
      *                         TO OD4-Hex-2 (Output-Sub)

      *                     MOVE WK-BUF2-L (Buffer-Sub)
      *                             TO OD4-Hex-1 (Output-Sub)
      *                     MOVE WK-BUF2-R (Buffer-Sub)
      *                             TO OD4-Hex-2 (Output-Sub)
      *                 END-IF
      *             END-IF
           END-PERFORM

           IF  Output-Sub > 0
               MOVE  LFD-SEQ  TO OD4-SEQ
                                 OD4-SEQ2
                                 OD-SEQ
               MOVE  LFD-ITEM TO OD4-ITEM
                                 OD4-ITEM2
               MOVE  OD-I11   TO OD-I12
                                 OD4-I13
                                 OD-I122 (P1:24)
                                 OD4-I22 (P1-2 + 81:24)

      *    *** ID=X で　40バイト以下の時、TYPE=A でも16進数 出力する
               IF Buffer-Length <= 40
                   COMPUTE L3 = OUTPUT-SUB * 2
                   MOVE  WK-BUF2-LR-TBL(1:L3) TO OD4-I11 (1:L3)
                   MOVE  OD4-I11  TO OD4-I12
                                     OD4-I22 (P1-2:80)
                   IF  LFD-SU = 1
                       MOVE  SPACE    TO   POT1-REC
                       WRITE POT1-REC
                       ADD   1        TO   WK-POT1-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT1-REC    FROM    Output-Header-3
                           ADD   1        TO   WK-POT1-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
      *     IF ( LFD-SEQ = 10 ) OR
      *        ( LFD-SEQ >=49 AND <= 57 ) 
      *       CALL "COBDUMP" USING Output-Detail42
      *     END-IF
                           WRITE POT1-REC FROM Output-Detail42
                       ELSE
                           WRITE POT1-REC FROM Output-Detail4
                       ADD   1        TO   WK-POT1-CNT
                       END-IF
                   ELSE
                       MOVE  SPACE    TO   POT2-REC
                       WRITE POT2-REC
                       ADD   1        TO   WK-POT2-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT2-REC   FROM    Output-Header-3
                           ADD   1        TO   WK-POT2-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT2-REC FROM Output-Detail42
                       ELSE
                           WRITE POT2-REC FROM Output-Detail4
                       END-IF

                       ADD   1        TO   WK-POT2-CNT
                   END-IF
               ELSE
                   MOVE  SPACE    TO   OD4-I12
                                       OD4-I13
                                       OD4-I11
                                       OD4-ASCII-X
                                       OD4-ASCII2-X

                   IF  LFD-SU = 1
                       MOVE  SPACE    TO   POT1-REC
                       WRITE POT1-REC
                       WRITE POT1-REC FROM Output-Detail4
                       ADD   2        TO   WK-POT1-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT1-REC    FROM    Output-Header-1
                           WRITE   POT1-REC    FROM    Output-Header-2
                           ADD   2        TO   WK-POT1-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT1-REC FROM Output-Detail12
                       ELSE
                           WRITE POT1-REC FROM Output-Detail1
                       END-IF
                       ADD   1        TO   WK-POT1-CNT
                   ELSE
                       MOVE  SPACE    TO   POT2-REC
                       WRITE POT2-REC
                       WRITE POT2-REC FROM Output-Detail4
                       ADD   2        TO   WK-POT2-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT2-REC    FROM    Output-Header-1
                           WRITE   POT2-REC    FROM    Output-Header-2
                           ADD   2        TO   WK-POT2-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT2-REC FROM Output-Detail12
                       ELSE
                           WRITE POT2-REC FROM Output-Detail1
                       END-IF
                       ADD   1        TO   WK-POT2-CNT
                   END-IF
                   IF  LFD-TYPE = "M"
                       MOVE WK-BUF2-L-TBL (L2:Output-Sub)
                               TO Output-Detail2
                       MOVE WK-BUF2-R-TBL (L2:Output-Sub)
                               TO Output-Detail3
                       IF  LFD-SU  =  1
                           WRITE POT1-REC FROM Output-Detail2
                           WRITE POT1-REC FROM Output-Detail3
                           ADD   2        TO   WK-POT1-CNT
                       ELSE
                           WRITE POT2-REC FROM Output-Detail2
                           WRITE POT2-REC FROM Output-Detail3
                           ADD   2        TO   WK-POT2-CNT
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** SJIS 用
       S210-10.
      *    *** SJIS の時、１００バイト目の漢字を表示させるため
           IF    ( Buffer (I:2) >= X"8140" AND 
                   Buffer (I:2) <= X"9FFC" )   OR
                 ( Buffer (I:2) >= X"E040" AND 
                   Buffer (I:2) <= X"EAA4" )
                   MOVE   "1"  TO     SW-KANJI
                                      SW-KANJI2
           ELSE
                   MOVE   ZERO TO     SW-KANJI
           END-IF
           .
       S210-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

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

           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           .
       S900-EX.
           EXIT.
