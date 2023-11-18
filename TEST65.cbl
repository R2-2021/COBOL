      *    *** 仮想　コンバージョン
      *    *** PIN1-CODE でソート順のこと

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST65.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** コンバージョンインプットデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** コンバージョン後ＯＫデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** コンバージョンエラーリスト
       SELECT LST1-F           ASSIGN   WK-LST1-F-NAME
                               STATUS   WK-LST1-STATUS.
      *    *** 実際のプリンターで試してないと、分からない。
      *    *** 
      *    *** この指定をしないと、ＨＯＳＴと同じ仕様になる。
      *    *** ＡＦＴＥＲ　１は１行改行
      *    *** 1ページ目ＡＦＴＥＲ　ＰＡＧＥ必要かどうかは、
      *    *** プリンターではチェックしていない、
      *    *** （不要なブランクページが出るか？）
      *     ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F.
       01  PIN1-REC.
           03  PIN1-ID         PIC  X(002).
           03  PIN1-CODE       PIC  X(006).
           03  PIN1-CARDID     PIC  X(002).
           03  PIN1-YMD        PIC  X(008).
           03  PIN1-USERCD     PIC  X(004).
           03  PIN1-SHOHIN     PIC  X(004).
           03  PIN1-SIGN       PIC  X(001).
           03  PIN1-SURYO      PIC  9(007)V99.
           03  PIN1-TANKA      PIC  9(007)V99.
           03  PIN1-KINGAKU    PIC  9(009).
           03  PIN1-NISUGATA   PIC  X(002).

       FD  POT1-F.
       01  POT1-REC.
           03  POT1-ID         PIC  X(002).
           03  POT1-CODE       PIC  X(006).
           03  POT1-CARDID     PIC  X(002).
           03  POT1-YMD        PIC  X(008).
           03  POT1-USERCD     PIC  X(004).
           03  POT1-SHOHIN     PIC  X(001).
           03  POT1-SURYO      PIC  9(001)V99.
           03  POT1-TANKA      PIC  9(009)V99.
           03  POT1-KINGAKU    PIC  9(009).
           03  POT1-NISUGATA   PIC  X(002).

       FD  LST1-F.
       01  LST1-REC            PIC  X(180).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST65  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST65.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST65.POT1".
           03  WK-LST1-F-NAME  PIC  X(032) VALUE "TEST65.LST1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-LST1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-LST1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-LST1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-T-ER-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-T-OK-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.

           03  WK-C-ER-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-C-OK-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-T-ER-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-T-OK-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-LINE         BINARY-LONG SYNC VALUE 99.
           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.
           03  WK-SPACE        PIC  X(001) VALUE SPACE.

       01  PRINT-AREA.
           03  PR-TIT1.
             05  PR-LF         PIC  X(001) VALUE X"0A".
             05                PIC  X(030) VALUE "TEST65".
             05  PR-TIT1-TIT   PIC  X(042) VALUE
                 "＊＊＊　ＸＸコンバージョン　エラー　＊＊＊".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(006) VALUE "日付：".
             05  PR-TIT1-YY    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE "/".
             05  PR-TIT1-MM    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE "/".
             05  PR-TIT1-DD    PIC  9(002) VALUE ZERO.
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(006) VALUE "時刻：".
             05  PR-TIT1-HH    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE ":".
             05  PR-TIT1-MI    PIC  9(002) VALUE ZERO.
             05                PIC  X(001) VALUE ":".
             05  PR-TIT1-SS    PIC  9(002) VALUE ZERO.
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(004) VALUE "頁：".
             05  PR-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.

           03  PR-MID1
             05                PIC  X(002) VALUE "ID".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(006) VALUE "CODE".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(006) VALUE "CARDCD".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(008) VALUE "YMD".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(004) VALUE "USER".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(004) VALUE "商品".
             05                PIC  X(002) VALUE SPACE.
             05                PIC  X(020) VALUE "エラーメッセージ".

           03  PR-MID2.
             05  PR-MID2-COM1  PIC  X(010) VALUE SPACE.
             05                PIC  X(002) VALUE SPACE.
             05  PR-MID2-CODE  PIC  X(006) VALUE SPACE.
 
           03  PR-HAI1.
             05                PIC  X(180) VALUE ALL "-".

           03  PR-MEI1.
             05  PR-MEI1-ID     PIC  X(002) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-CODE   PIC  X(006) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-CARDID PIC  X(006) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-YMD    PIC  X(008) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-USERCD PIC  X(004) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-SHOHIN PIC  X(004) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI1-COMENT PIC  X(020) VALUE SPACE.

           03  PR-MEI2.
             05  PR-MEI2-ID     PIC  X(002) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI2-CODE   PIC  X(006) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI2-CARDID PIC  X(002) VALUE SPACE.
             05                 PIC  X(006) VALUE SPACE.
             05  PR-MEI2-YMD    PIC  X(008) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI2-USERCD PIC  X(004) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI2-SHOHIN PIC  X(004) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI2-COMENT PIC  X(020) VALUE SPACE.

           03  PR-MEI3.
             05  PR-MEI3-COM1   PIC  X(010) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI3-CODE   PIC  X(006) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI3-COM2   PIC  X(010) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI3-OK-CNT PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI3-COM3   PIC  X(010) VALUE SPACE.
             05                 PIC  X(002) VALUE SPACE.
             05  PR-MEI3-ER-CNT PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.

       01  KEY-AREA.
           03  KEY-OLD.
             05  KEY-O-CODE    PIC  X(006) VALUE LOW-VALUE.
           03  KEY-NEW.
             05  KEY-N-CODE    PIC  X(006) VALUE LOW-VALUE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA-G.
             05  TBL01-AREA    OCCURS 07.
               07  TBL01-ERR   PIC  X(001) VALUE SPACE.
           03  TBL02-AREA-G.
             05  TBL02-AREA.
               07              PIC  X(020) VALUE "AAAAAAAAAAAAAAAAAAAA".
               07              PIC  X(020) VALUE "BBBBBBBBBBBBBBBBBBBB".
               07              PIC  X(020) VALUE "CCCCCCCCCCCCCCCCCCCC".
               07              PIC  X(020) VALUE "DDDDDDDDDDDDDDDDDDDD".
               07              PIC  X(020) VALUE "EEEEEEEEEEEEEEEEEEEE".
               07              PIC  X(020) VALUE "FFFFFFFFFFFFFFFFFFFF".
             05  TBL02-AREA-R  REDEFINES TBL02-AREA
                               OCCURS 06.
               07  TBL02-COMENT PIC  X(020).

       01  SW-AREA.
           03  SW-ERROR        PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM TEST AFTER
                   UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** ０件の時、総合計
               IF      KEY-NEW     =       HIGH-VALUE
      *    *** KEY ﾌﾞﾚｲｸ WRITE 総合計
                   PERFORM S140-10     THRU    S140-EX

               ELSE

      *    *** FORMAT CHECK
                   PERFORM S100-10     THRU    S100-EX

                   IF      SW-ERROR    =       "Y"
      *    *** ERROR LIST WRITE
                           PERFORM S110-10     THRU    S110-EX
                   ELSE
      *    *** OK DATA WRITE
                           PERFORM S120-10     THRU    S120-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   IF      KEY-OLD     NOT =   KEY-NEW
                       IF      WK-C-ER-CNT NOT =   ZERO
      *    *** KEY ﾌﾞﾚｲｸ WRITE
                               PERFORM S130-10     THRU    S130-EX
                       END-IF

                       IF      KEY-NEW     =       HIGH-VALUE
      *    *** KEY ﾌﾞﾚｲｸ WRITE 総合計
                               PERFORM S140-10     THRU    S140-EX
                       END-IF
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

           MOVE    WDT-DATE-YY  TO      PR-TIT1-YY
           MOVE    WDT-DATE-MM  TO      PR-TIT1-MM
           MOVE    WDT-DATE-DD  TO      PR-TIT1-DD

           MOVE    WDT-DATE-HH  TO      PR-TIT1-HH
           MOVE    WDT-DATE-MI  TO      PR-TIT1-MI
           MOVE    WDT-DATE-SS  TO      PR-TIT1-SS

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

           OPEN    OUTPUT      LST1-F
           IF      WK-LST1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " LST1-F OPEN ERROR STATUS="
                           WK-LST1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    KEY-NEW     TO      KEY-OLD
           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               KEY-NEW
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   MOVE    PIN1-CODE   TO      KEY-N-CODE
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** FORMAT CHECK
       S100-10.

           MOVE    SPACE       TO      PR-MEI1
                                       PR-MEI2
                                       POT1-REC
                                       TBL01-AREA-G
                                       SW-ERROR

           MOVE    PIN1-ID     TO      PR-MEI1-ID
           EVALUATE TRUE

               WHEN PIN1-ID = "11"
                   MOVE    "A1"        TO      POT1-ID
               WHEN PIN1-ID = "12"
                   MOVE    "A2"        TO      POT1-ID
               WHEN PIN1-ID = "13"
                   MOVE    "A3"        TO      POT1-ID
               WHEN PIN1-ID = "14"
                   MOVE    "A4"        TO      POT1-ID

               WHEN PIN1-ID = "21"
                   MOVE    "B1"        TO      POT1-ID
               WHEN PIN1-ID = "22"
                   MOVE    "B2"        TO      POT1-ID
               WHEN PIN1-ID = "23"
                   MOVE    "B3"        TO      POT1-ID
               WHEN PIN1-ID = "24"
                   MOVE    "B4"        TO      POT1-ID

               WHEN OTHER
                   MOVE    "?"         TO      TBL01-ERR (1)
                   MOVE    ALL "?"     TO      PR-MEI2-ID
           END-EVALUATE

           MOVE    PIN1-CODE   TO      POT1-CODE
                                       PR-MEI1-CODE
           IF      PIN1-CODE   NOT NUMERIC
                   MOVE    "?"         TO      TBL01-ERR (2)
                   MOVE    ALL "?"     TO      PR-MEI2-CODE
           END-IF

           MOVE    PIN1-CARDID TO      POT1-CARDID
                                       PR-MEI1-CARDID
           IF      PIN1-CARDID NOT NUMERIC
                   MOVE    "?"         TO      TBL01-ERR (3)
                   MOVE    ALL "?"     TO      PR-MEI2-CARDID
           END-IF

           MOVE    PIN1-YMD  TO      POT1-YMD
                                     PR-MEI1-YMD
           IF      PIN1-YMD  NOT NUMERIC
                   MOVE    "?"         TO      TBL01-ERR (4)
                   MOVE    ALL "?"     TO      PR-MEI2-YMD
           END-IF

           MOVE    PIN1-USERCD TO      POT1-USERCD
                                       PR-MEI1-USERCD
           IF      PIN1-USERCD NOT NUMERIC
                   MOVE    "?"         TO      TBL01-ERR (5)
                   MOVE    ALL "?"     TO      PR-MEI2-USERCD
           END-IF

           MOVE    PIN1-SHOHIN TO      POT1-SHOHIN
                                       PR-MEI1-SHOHIN
           IF      PIN1-SHOHIN NOT NUMERIC
                   MOVE    "?"         TO      TBL01-ERR (6)
                   MOVE    ALL "?"     TO      PR-MEI2-SHOHIN
           END-IF

           IF      TBL01-AREA-G NOT =  SPACE
               MOVE    "Y"         TO      SW-ERROR
      *    *** PR-MEI1-COMENT,PR-MEI2-COMENT SPACE の時、
      *    *** エラーコメント先にセット
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > 6
                   IF      TBL01-ERR(I) NOT =  SPACE
                       IF      PR-MEI1-COMENT =    SPACE
                           MOVE    TBL02-COMENT (I) TO PR-MEI1-COMENT
                       ELSE
                           IF      PR-MEI2-COMENT =    SPACE
                               MOVE    TBL02-COMENT(I) TO PR-MEI2-COMENT
                               ADD     I 1         GIVING  I2
                               EXIT    PERFORM
                           END-IF
                       END-IF
                   END-IF
                   ADD     I 1         GIVING  I2
               END-PERFORM
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** エラー明細出力
       S110-10.

      *    *** WRITE AFTER,BEFORE
      *    *** HOST と違う、PCではWRITE後、行末にCRLFが挿入されるため、
      *    *** AFTER 1にすると、見た目，２行改行になる
      *    *** AFTER 0にすると、見た目，１行改行になる
      *    *** AFTER PAGE も１ページ目は不要と思われる
      *    *** COBXREF の サブルーチンLISTING.CBL では、WRITE BEFORE
      *    *** を使用している

           MOVE    PIN1-CODE   TO      PR-MID2-CODE
      *    *** エラーリスト、ヘッダー出力
           PERFORM S111-10     THRU    S111-EX

      *     WRITE   LST1-REC    FROM    PR-MEI1 AFTER 1
           WRITE   LST1-REC    FROM    PR-MEI1 AFTER 2
           ADD     2           TO      WK-LINE
                                       WK-LST1-CNT

      *    *** エラーリスト、ヘッダー出力
           PERFORM S111-10     THRU    S111-EX

      *     WRITE   LST1-REC    FROM    PR-MEI2 AFTER 0
           WRITE   LST1-REC    FROM    PR-MEI2 AFTER 1
           ADD     1           TO      WK-LINE
                                       WK-LST1-CNT

      *    *** エラーコメント出力
      *    *** エラーコメント３つ以上の時、ここでセット
      *    *** I2 に３つ目以降の？がセットされている
           PERFORM VARYING I FROM I2 BY 1
                   UNTIL I > 6
                   IF      TBL01-ERR(I) NOT =  SPACE
                       MOVE    SPACE       TO      PR-MEI1
                       MOVE    TBL02-COMENT (I) TO PR-MEI1-COMENT

      *    *** エラーリスト、ヘッダー出力
                       PERFORM S111-10     THRU    S111-EX
      *                 WRITE   LST1-REC    FROM    PR-MEI1 AFTER 0
                       WRITE   LST1-REC    FROM    PR-MEI1 AFTER 1
                       ADD     1           TO      WK-LINE
                                                   WK-LST1-CNT
                   END-IF
           END-PERFORM

           ADD     1           TO      WK-C-ER-CNT
                                       WK-T-ER-CNT
          .
       S110-EX.
           EXIT.

      *    *** エラーリスト、ヘッダー出力
       S111-10.

           IF      WK-LINE     >       60
                   ADD     1           TO      WK-PAGE
                   MOVE    WK-PAGE     TO      PR-TIT1-PAGE
                   IF      SW-FIRST    =       "Y"
      *                 WRITE   LST1-REC    FROM    PR-TIT1 AFTER 0
      *                 MOVE    SPACE       TO      LST1-REC
      *                 WRITE   LST1-REC    AFTER   ADVANCING PAGE
      *                 WRITE   LST1-REC    FROM    PR-TIT1 AFTER PAGE
                       WRITE   LST1-REC    FROM    PR-TIT1 AFTER 1
                       MOVE    "N"         TO      SW-FIRST
                   ELSE
      *                 WRITE   LST1-REC    FROM    PR-TIT1 AFTER PAGE
      *    *** X"0A"=LF
      *                 WRITE   LST1-REC    AFTER   PAGE
                       WRITE   LST1-REC    FROM    PR-TIT1 AFTER 1
                   END-IF
                   MOVE    "ｺｰﾄﾞ:"     TO      PR-MID2-COM1
      *             WRITE   LST1-REC    FROM    PR-MID2 AFTER 1
      *             WRITE   LST1-REC    FROM    PR-MID1 AFTER 0
      *             WRITE   LST1-REC    FROM    PR-HAI1 AFTER 0
                   WRITE   LST1-REC    FROM    PR-MID2 AFTER 2
                   WRITE   LST1-REC    FROM    PR-MID1 AFTER 1
                   WRITE   LST1-REC    FROM    PR-HAI1 AFTER 1
                   MOVE    5           TO      WK-LINE
                   ADD     5           TO      WK-LST1-CNT
           END-IF
           .
       S111-EX.
           EXIT.

      *    *** ＯＫデータ出力
       S120-10.

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
                                       WK-C-OK-CNT
                                       WK-T-OK-CNT
           .
       S120-EX.
           EXIT.

      *    *** コード合計 エラー件数、１件以上の時、出力する
       S130-10.

           MOVE    KEY-O-CODE  TO      PR-MID2-CODE
      *    *** エラーリスト、ヘッダー出力
           PERFORM S111-10     THRU    S111-EX

           MOVE    SPACE       TO      PR-MEI3
           MOVE    "ｺｰﾄﾞ:"     TO      PR-MEI3-COM1
           MOVE    KEY-O-CODE  TO      PR-MEI3-CODE
           MOVE    "OK:"       TO      PR-MEI3-COM2
           MOVE    WK-C-OK-CNT TO      PR-MEI3-OK-CNT
           MOVE    "ERROR:"    TO      PR-MEI3-COM3
           MOVE    WK-C-ER-CNT TO      PR-MEI3-ER-CNT
      *     WRITE   LST1-REC    FROM    PR-MEI3 AFTER 1
           WRITE   LST1-REC    FROM    PR-MEI3 AFTER 2
           ADD     2           TO      WK-LINE
                                       WK-LST1-CNT

           MOVE    ZERO        TO      WK-C-ER-CNT
                                       WK-C-OK-CNT
           MOVE    99          TO      WK-LINE
           .
       S130-EX.
           EXIT.

      *    *** 総合計 PIN1 ０件でも出力する
       S140-10.

           MOVE    SPACE       TO      PR-MID2-CODE
      *    *** エラーリスト、ヘッダー出力
           PERFORM S111-10     THRU    S111-EX

           MOVE    SPACE       TO      PR-MEI3
           MOVE    "ｿｳｺﾞｳｹｲ:"  TO      PR-MEI3-COM1
           MOVE    SPACE       TO      PR-MEI3-CODE
           MOVE    "OK:"       TO      PR-MEI3-COM2
           MOVE    WK-T-OK-CNT TO      PR-MEI3-OK-CNT
           MOVE    "ERROR:"    TO      PR-MEI3-COM3
           MOVE    WK-T-ER-CNT TO      PR-MEI3-ER-CNT
      *     WRITE   LST1-REC    FROM    PR-MEI3 AFTER 1
           WRITE   LST1-REC    FROM    PR-MEI3 AFTER 2
           ADD     2           TO      WK-LINE
                                       WK-LST1-CNT
           .
       S140-EX.
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

           CLOSE   LST1-F
           IF      WK-LST1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " LST1-F CLOSE ERROR STATUS="
                           WK-LST1-STATUS
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
      *     MOVE    WK-LST1-CNT TO      WK-LST1-CNT-E
      *     DISPLAY WK-PGM-NAME " LST1 ｹﾝｽｳ = " WK-LST1-CNT-E
      *             " (" WK-LST1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " LST1 ﾍﾟｰｼﾞ= " WK-PAGE-E
                   " (" WK-LST1-F-NAME ")"
           MOVE    WK-T-OK-CNT TO      WK-T-OK-CNT-E
           DISPLAY WK-PGM-NAME " OK   ｹﾝｽｳ = " WK-T-OK-CNT-E
                   " (" WK-LST1-F-NAME ")"
           MOVE    WK-T-ER-CNT TO      WK-T-ER-CNT-E
           DISPLAY WK-PGM-NAME " ER   ｹﾝｽｳ = " WK-T-ER-CNT-E
                   " (" WK-LST1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
