      *    *** DECODE01 改良 サブルーチン
      *    *** １文字ずつでなく、Buffer全部を変換
      *    *** インターネット HTML %XX UTF-8 文字変換
      *    *** 
      *    *** MAIN で２カ所以上で、使用の時、LI-BUF2-L 未設定だと、
      *    *** 最初のCALLで長さを自動でセットするため、長さの違うCALL
      *    *** の時、予期せぬ結果になるので、注意する

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE02.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** この指定が無いとコンパイルエラーになる
      *    *** MOVE LENGTH(LI-BUF2) TO WK-BUF-LEN2 の所
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-L            BINARY-LONG SYNC VALUE ZERO.
           03  WK-R            BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX-DIGITS   VALUE '0123456789ABCDEF'.
             05  WK-HEX-DIGIT  OCCURS 16
                               PIC  X(001).

           03  WK-PIC.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-PIC-X      PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIC-HALF     REDEFINES WK-PIC 
                               PIC  9(004) COMP-X.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 256
                               ASCENDING KEY IS TBL01-LR
      *    *** TBL01-IDX このINDEXED BY を指定した INDEX は定義不要
                               INDEXED BY TBL01-IDX.

      *    *** 0-255 の16進数が入っている
             05  TBL01-PIC-X   PIC  X(001) VALUE ZERO.
      *    *** 0-255 の16進数、ASCII 文字が入っている
             05  TBL01-LR.
               07  TBL01-L     PIC  X(001) VALUE ZERO.
               07  TBL01-R     PIC  X(001) VALUE ZERO.

       01  IDX-AREA.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  CNS-AREA.
           03  CNS-9           BINARY-LONG SYNC VALUE 9.
           03  CNS-3           BINARY-LONG SYNC VALUE 3.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       LINKAGE                 SECTION.

      *    *** %XX%XX... が入っているデータ
       01  LI-BUF1             PIC  X(001) ANY LENGTH.
      *    *** MAIN でセットすること、変換したい長さセット
       01  LI-BUF1-L           BINARY-LONG SYNC.
      *    *** 変換後 が入っているデータ
       01  LI-BUF2             PIC  X(001) ANY LENGTH.
      *    *** MAIN でセットすること、RETURN後は変換後長さセット
       01  LI-BUF2-L           BINARY-LONG SYNC.

       PROCEDURE   DIVISION    USING   LI-BUF1
                                       LI-BUF1-L
                                       LI-BUF2
                                       LI-BUF2-L
           .
       M100-10.

           IF      SW-FIRST    =       "Y"
      *    *** X"00" => X"FF" TBL01 作成
                   PERFORM S010-10     THRU    S010-EX
           END-IF

      *     MOVE    LENGTH(LI-BUF1) TO  WK-BUF-LEN1
      *     MOVE    LENGTH(LI-BUF2) TO  WK-BUF-LEN2
      *     IF      LI-BUF1-L   =       ZERO
                   MOVE    LENGTH(LI-BUF1) TO  LI-BUF1-L
      *     END-IF
      *     IF      LI-BUF2-L   =       ZERO
                   MOVE    LENGTH(LI-BUF2) TO  LI-BUF2-L
      *     END-IF

      *    *** UTF-8  0xe08080～0xefbfbf
      *    *** Egg%XX%YY => EggXY に対応 ANK,%XX 混在に対応
           MOVE    ZERO        TO      L2
                                       L3
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL   L1 >  LI-BUF1-L
                        OR L2 >= LI-BUF2-L

               ADD     1           TO      L2
               IF      LI-BUF1 (L1:1) =    "%"

      *    *** SEARCH ALL INDEX に SET 不要
      *             SET     TBL01-IDX   TO      1
                   SEARCH  ALL TBL01-AREA
                       AT END

      *    *** 256文字TBLに全部あるので、AT END はありえない
                           MOVE    SPACE       TO      LI-BUF2 (L2:1)
                       WHEN TBL01-LR (TBL01-IDX) = LI-BUF1 (L1 + 1:2)
                           MOVE    TBL01-PIC-X (TBL01-IDX)
                                               TO      LI-BUF2 (L2:1)
                   END-SEARCH

                   ADD     2           TO      L1
                   ADD     1           TO      L3
               ELSE
      *    *** % 以外はANKと思われるので、そのままセット
      *    *** Eggたまご+[22/7] +以降が不要の時、MAINでカットする
      *    *** "/results?search_query=Egg%E3%81%9F%E3%81%BE%E3%81%94+
      *    *** %E3%80%9022%2F7%E5%85%AC%E5%BC%8F%E3%80%91"
      *    *** なので、X"20"右スペース長さをカットする
                   MOVE    LI-BUF1 (L1:1) TO   LI-BUF2 (L2:1)
                   IF      LI-BUF1 (L1:1) NOT = SPACE
                       ADD     1           TO      L3
                   END-IF
               END-IF
           END-PERFORM

      *    *** L3 は変換後長さ
           MOVE    L3          TO      LI-BUF2-L

           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** X"00" => X"FF" TBL01 作成
       S010-10.

           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 255
                   MOVE    I           TO      WK-PIC-HALF
                   MOVE    WK-PIC-X    TO      TBL01-PIC-X (I + 1)

                   DIVIDE  WK-PIC-HALF BY 16
                           GIVING    WK-L
                           REMAINDER WK-R

                   ADD     1           TO      WK-L
                                               WK-R

                   MOVE    WK-HEX-DIGIT (WK-L)
                                       TO      TBL01-L (I + 1)

                   MOVE    WK-HEX-DIGIT (WK-R)
                                       TO      TBL01-R (I + 1)
           END-PERFORM

           MOVE    "N"         TO      SW-FIRST
           .
       S010-EX.
           EXIT.
