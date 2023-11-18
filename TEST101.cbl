      *    *** 楽天検索データ自動作成
      *    *** 声優データから９９件ランダム作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST101.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 日本の女優一覧 データ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 楽天検索データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(3000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(050).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST101 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST58_jyoyuall.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST101.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST101.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST101.POT2".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SEIYU1       PIC  X(050) VALUE SPACE.
           03  WK-NUM          PIC  9(004) VALUE ZERO.
           03  WK-DELI-ITEM    PIC  X(001) VALUE SPACE.
           03  WK-UNSTR-PTR    BINARY-LONG SYNC VALUE ZERO.

      *    *** 変換前 が入っているデータ
      * 01  WDE03-BUF1             PIC  X(001) ANY LENGTH.

      *    *** 変換前のデータの長さ
       01  WDE03-BUF1-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** 16進数 変換後 が入っているデータ
      *    *** 富士通のNETCOBOLの資料によると、項目最大長は64770バイトである
       01  WDE03-BUF2.
      *    *** LLL...
           03  WDE03-BUF2-L-TBL.
             05  WDE03-BUF2-L  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** RRR...
           03  WDE03-BUF2-R-TBL.
             05  WDE03-BUF2-R  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** LRLR...
           03  WDE03-BUF2-LR-TBL.
             05  WDE03-BUF2-LR-TBL2 OCCURS 65536.
               07  WDE03-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WDE03-BUF2-R2  PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I5-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  M2              BINARY-LONG SYNC VALUE ZERO.
           03  M2-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       01  TBL-AREA.
      *    *** 声優用一意性チェック
           03  TBL01-AREA      OCCURS 174.
             05  TBL01-CHECK   BINARY-LONG SYNC VALUE ZERO.

      *    *** 女優用
           03  TBL05-AREA      OCCURS 5000.
             05  TBL05-SHIMEI  PIC  X(50) VALUE SPACE.
             05  TBL05-SHIMEI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL05-CHECK   BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** WEEK 1=月曜日、..、7=日曜日
           IF      WDT-DATE-WEEK =     1 OR 3 OR 5 OR 7

      *    *** データ作成・声優
                   PERFORM S100-10     THRU    S100-EX
           ELSE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                       EVALUATE TRUE
                           WHEN WK-PIN1-LEN = ZERO
                               CONTINUE
                           WHEN PIN1-REC (1:1) = "%"
      *    *** ヘッダー出力
      *    *** 出力しない、読み飛ばし
                               CONTINUE
      *    *** ジャパリ
                           WHEN PIN1-REC (1:12) =
                                X"E382B8E383A3E38391E383AA"
      *    *** 行
                            AND PIN1-REC (16:3) = X"E8A18C"
      *    *** 出力しない、読み飛ばし
                               CONTINUE
      *    *** ジャパリ
                           WHEN PIN1-REC (1:12) =
                                X"E382B8E383A3E38391E383AA"
      *    *** 出力しない、読み飛ばし
                               CONTINUE

                           WHEN OTHER
      *    *** TBL SET PIN1 女優名１
                               PERFORM S140-10     THRU    S140-EX
      *    *** TBL SET PIN1 女優名２
      *                        PERFORM S150-10     THRU    S150-EX
                       END-EVALUATE

      *    *** READ PIN1
                       PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

      *    *** データ作成２
                   PERFORM S110-10     THRU    S110-EX
           END-IF

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F
                               POT2-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

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

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** データ作成・声優
       S100-10.

           MOVE    "N"         TO      WCR-SIGN  (1)
           MOVE    "N"         TO      WCR-ZERO  (1)
           MOVE    ZERO        TO      WCR-FROM2 (1)
           MOVE    100000      TO      WCR-TO2   (1)

           MOVE    "RND"       TO      WCR-ID
           MOVE    99          TO      WCR-IDX
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 99

                   MOVE    SPACE       TO      WK-SEIYU1
                   MOVE    ZERO        TO      WDE03-BUF1-LEN
                   UNSTRING WCR-S-NAME8 (I1)
                           DELIMITED BY SPACE
                           INTO
                           WK-SEIYU1   COUNT   WDE03-BUF1-LEN
                   MOVE    WK-SEIYU1 (1:WDE03-BUF1-LEN)
                                       TO      POT1-REC (1:)
      *    *** J2:POT1-REC (J2:NNN) SET 開始一
                   ADD     1 WDE03-BUF1-LEN GIVING J2
                   MOVE    " ,"        TO      POT1-REC (J2:2)

                   ADD     2           TO      J2
                   MOVE    "https://websearch.rakuten.co.jp/Web?qt="
                                       TO      POT1-REC (J2:39)
                   ADD     39          TO      J2

                   CALL    "DECODE03"  USING   WK-SEIYU1
                                               WDE03-BUF1-LEN
                                               WDE03-BUF2

                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL J > WDE03-BUF1-LEN
                       IF    ( WK-SEIYU1 (J:1) >= "A" AND
                               WK-SEIYU1 (J:1) <= "Z" )  OR
                             ( WK-SEIYU1 (J:1) >= "a" AND
                               WK-SEIYU1 (J:1) <= "z" )
                           MOVE    WK-SEIYU1 (J:1) TO  POT1-REC (J2:1)
                           ADD     1           TO      J2
                       ELSE
                           MOVE    "%"         TO      POT1-REC (J2:1)
                           ADD     1           TO      J2
                           MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                           ADD     1           TO      J2
                           MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
                           ADD     1           TO      J2
                       END-IF
                   END-PERFORM

      *             MOVE     "&ref=chext_tb&tool_id=1&col=OW"
      *                                 TO      POT1-REC (J2:30)

                   MOVE     "&ref=chext_tb_r&col=OW"
                                       TO      POT1-REC (J2:22)
                   ADD     1           TO      J2

                   COMPUTE K ROUNDED = WCR-RND (I1) * 174
                   IF      K           =       ZERO
                           MOVE    1           TO      K
                   END-IF

                   IF      TBL01-CHECK (K) =  ZERO
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           WRITE   POT2-REC    FROM    WK-SEIYU1
                           ADD     1           TO      WK-POT2-CNT
                                                       TBL01-CHECK (K)
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** データ作成２
       S110-10.

           MOVE    "N"         TO      WCR-SIGN  (1)
           MOVE    "N"         TO      WCR-ZERO  (1)
           MOVE    ZERO        TO      WCR-FROM2 (1)
           MOVE    100000      TO      WCR-TO2   (1)

           MOVE    "RND"       TO      WCR-ID
           MOVE    99          TO      WCR-IDX
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 99

      *    *** 2386 は女優数
                   COMPUTE WK-NUM ROUNDED = WCR-RND (I1) * 2386
                   IF      WK-NUM      =       ZERO
                           MOVE    1           TO      WK-NUM
                   END-IF

                   MOVE    SPACE       TO      WK-SEIYU1
                   MOVE    ZERO        TO      WDE03-BUF1-LEN
                   UNSTRING TBL05-SHIMEI (WK-NUM)
                           DELIMITED BY SPACE
                           INTO
                           WK-SEIYU1   COUNT   WDE03-BUF1-LEN
                   MOVE    WK-SEIYU1 (1:WDE03-BUF1-LEN)
                                       TO      POT1-REC (1:)
      *    *** J2:POT1-REC (J2:NNN) SET 開始一
                   ADD     1 WDE03-BUF1-LEN GIVING J2
                   MOVE    " ,"        TO      POT1-REC (J2:2)

                   ADD     2           TO      J2
                   MOVE    "https://websearch.rakuten.co.jp/Web?qt="
                                       TO      POT1-REC (J2:39)
                   ADD     39          TO      J2

                   CALL    "DECODE03"  USING   WK-SEIYU1
                                               WDE03-BUF1-LEN
                                               WDE03-BUF2

                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL J > WDE03-BUF1-LEN
                       IF    ( WK-SEIYU1 (J:1) >= "A" AND
                               WK-SEIYU1 (J:1) <= "Z" )  OR
                             ( WK-SEIYU1 (J:1) >= "a" AND
                               WK-SEIYU1 (J:1) <= "z" )
                           MOVE    WK-SEIYU1 (J:1) TO  POT1-REC (J2:1)
                           ADD     1           TO      J2
                       ELSE
                           MOVE    "%"         TO      POT1-REC (J2:1)
                           ADD     1           TO      J2
                           MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                           ADD     1           TO      J2
                           MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
                           ADD     1           TO      J2
                       END-IF
                   END-PERFORM

      *             MOVE     "&ref=chext_tb&tool_id=1&col=OW"
      *                                 TO      POT1-REC (J2:30)

                   MOVE     "&ref=chext_tb_r&col=OW"
                                       TO      POT1-REC (J2:22)
                   ADD     1           TO      J2

                   IF      TBL05-CHECK (WK-NUM) =  ZERO
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                                               TBL05-CHECK (WK-NUM)

                           WRITE   POT2-REC    FROM
                                               TBL05-SHIMEI (WK-NUM)
                           ADD     1           TO      WK-POT2-CNT
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** TBL SET PIN1 女優名１
      *    *** UNSTRINGで女優名分離
      *    *** 実行時間 0.10秒
       S140-10.

           ADD     1           TO      I5
           IF      I5          >       5000
                   DISPLAY WK-PGM-NAME 
                           " TBL05 OVER I5=" I5
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      TBL05-SHIMEI     (I5)
           MOVE    ZERO        TO      TBL05-SHIMEI-LEN (I5)

           MOVE    1           TO      WK-UNSTR-PTR
           MOVE    SPACE       TO      WK-DELI-ITEM

           PERFORM UNTIL WK-UNSTR-PTR >= WK-PIN1-LEN
                   UNSTRING PIN1-REC DELIMITED BY SPACE OR ","
                          INTO TBL05-SHIMEI (I5) 
                          DELIMITER WK-DELI-ITEM
                          COUNT TBL05-SHIMEI-LEN (I5)
                          WITH POINTER WK-UNSTR-PTR

      *    *** UNSTR-PTR = WK-PIN1-LEN + 2 になるのかは、不明
      *    *** DELIMITED BY ALL SPACE にすると UNSTR-PTR = 3001 になる
      *    *** 項目 PIN1-REC 項目長 3000 + 1 になる
      *             IF I5 >= 1 AND <= 300
      *             DISPLAY "I5=" I5 
      *                             " UNSTR-PTR=" WK-UNSTR-PTR
      *                             " WK-PIN1-LEN=" WK-PIN1-LEN
      *                             " SHIMEI-LEN (I5)="
      *                             TBL05-SHIMEI-LEN (I5)
      *                             " DELI=" WK-DELI-ITEM
      *             END-IF

                   IF      TBL05-SHIMEI-LEN (I5) > 50
                           DISPLAY WK-PGM-NAME 
                                   " TBL05 SHIMEI OVER "
                           DISPLAY WK-PGM-NAME 
                                   " WK-PIN1-CNT="WK-PIN1-CNT 
                                   " WK-PIN1-LEN="WK-PIN1-LEN 
                                   " TBL05-SHIMEI-LEN (I5)=" 
                                   TBL05-SHIMEI-LEN (I5)
                           CALL    "COBDUMP" USING 
                                   TBL05-SHIMEI (I5)
                           STOP    RUN
                   END-IF

                   IF      WK-DELI-ITEM =  ","
                           ADD     1           TO      I5
                           IF      I5          >       5000
                                   DISPLAY WK-PGM-NAME 
                                           " TBL05 OVER I5=" I5
                                   STOP    RUN
                           END-IF
                           MOVE    SPACE       TO
                                               TBL05-SHIMEI  (I5)
                           MOVE    ZERO        TO
                                               TBL05-SHIMEI-LEN (I5)
                    END-IF
           END-PERFORM

           IF       I5         >       I5-MAX
                    MOVE    I5          TO      I5-MAX
      *              DISPLAY "I5-MAX=" I5-MAX
           END-IF
           .
       S140-EX.
           EXIT.

      *    *** TBL SET PIN1 女優名２
      *    *** 1バイトづつ、”,”チェックして女優名分離
      *    *** 実行時間 0.11秒 UNSTRING とあまり変わらない
       S150-10.

           ADD     1           TO      I5
           IF      I5          >       5000
                   DISPLAY WK-PGM-NAME 
                           " TBL05 OVER I5=" I5
                   STOP    RUN
           END-IF

           MOVE    ZERO        TO      M2
           MOVE    SPACE       TO      TBL05-SHIMEI     (I5)
           MOVE    ZERO        TO      TBL05-SHIMEI-LEN (I5)

           PERFORM VARYING M FROM 1 BY 1
                   UNTIL M > WK-PIN1-LEN 
                   IF      PIN1-REC (M:1) =    ","
      *                 OR  WK-PIN1-LEN =       M 
      *     DISPLAY I5 " SHIMEI=" TBL05-SHIMEI (I5) " " WK-PIN1-CNT
      *    *** ジャパリの所で先頭をクリアーする、
      *    *** ジャパリい は２行あるので残す

                           ADD     1           TO      I5
                           IF      I5          >       5000
                                   DISPLAY WK-PGM-NAME 
                                           " TBL05 OVER I5=" I5
                                   STOP    RUN
                           END-IF

                           MOVE    ZERO        TO      M2
                           MOVE    SPACE       TO
                                   TBL05-SHIMEI  (I5)
                           MOVE    ZERO        TO
                                   TBL05-SHIMEI-LEN (I5)

                   ELSE
                           ADD     1           TO      M2
                           IF      M2          >       50
                                   DISPLAY WK-PGM-NAME 
                                           " TBL05 SHIMEI OVER M2=" M2
                                   DISPLAY WK-PGM-NAME 
                                           " WK-PIN1-CNT="WK-PIN1-CNT 
                                           " WK-PIN1-LEN="WK-PIN1-LEN 
                                           " M=" M
                                   CALL    "COBDUMP" USING 
                                           TBL05-SHIMEI (I5)
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC (M:1) TO   
                                   TBL05-SHIMEI (I5) (M2:1)
                           ADD     1           TO      
                                   TBL05-SHIMEI-LEN (I5)
                   END-IF
           END-PERFORM

           IF       I5         >       I5-MAX
                    MOVE    I5          TO      I5-MAX
      *              DISPLAY "I5-MAX=" I5-MAX
           END-IF
           .
       S150-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   POT1-F
                   POT2-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
