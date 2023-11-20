      *    *** YouTube html 声優 作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST49.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 声優データ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アニメイト声優データ　ＵＴＦ８
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** twiiter,instagram
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** HTMLデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(500).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(20000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST49  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST49.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST47.POT1".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST49.PIN3".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST49.POT1".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "index.html".
           03  WK-POT1-F-NAME.
             05 PIC X(023) VALUE "C:\Users\xxxx\OneDrive\".
             05 PIC X(012) VALUE "ドキュメント".
             05 PIC X(013) VALUE "\HTML\YouTube".
             05 PIC X(004) VALUE "声優".
             05 PIC X(011) VALUE "\index.html".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

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

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** YouTube 声優
           03  WK-TITLE.
             05                PIC  X(008) VALUE "YouTube ".
             05                PIC  X(006) VALUE X"E5A3B0E584AA".

           03  WK-SEIYU1       PIC  X(100) VALUE SPACE.
           03  WK-SEIYU2       PIC  X(100) VALUE SPACE.
           03  WK-SITE-TBL.
             05  WK-SITE       OCCURS 50
                               PIC  X(500) VALUE SPACE.
           03  WK-IMGLINK      PIC  X(500) VALUE SPACE.
           03  WK-SITE1        PIC  X(500) VALUE SPACE.
           03  WK-SITE2        PIC  X(500) VALUE SPACE.
           03  WK-KENSAKU      PIC  X(200) VALUE SPACE.
           03  WK-NUM          PIC  9(004) VALUE ZERO.
           03  WK-NO           PIC  9(002) VALUE ZERO.
           03  WK-SITE-NAME    PIC  X(200) VALUE SPACE.
           03  WK-DD-SHO       BINARY-LONG SYNC VALUE ZERO.
           03  WK-DD-AMARI     BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEIYU1-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMGLINK-LEN  BINARY-LONG SYNC VALUE ZERO.

           03  TBL01-IDX-MAX   BINARY-LONG SYNC VALUE ZERO.
           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-REC2         PIC  X(1000) VALUE SPACE.

      *     *** "%E7%9F%B3%E5%8E%9F%E5%A4%8F%E7%B9%94".
           03  WK-UTF8-1.
             05  FILLER        PIC  X(003) VALUE SPACE. 
           03  WK-UTF8-2.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
           03  WK-BUF1-L       USAGE BINARY-LONG VALUE 3.
           03  WK-BUF2-L       USAGE BINARY-LONG VALUE 1.

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

      *    *** HACKADOLL FILE
       01  WDE06-AREA.
           03  WDE06-ID        PIC  X(001)  VALUE SPACE.
           03  WDE06-NUM       PIC  9(004)  VALUE ZERO.
           03  WDE06-FILE      PIC  X(012)  VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  Z               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-SEIYU
                               INDEXED BY TBL01-IDX.
             05  TBL01-SEIYU   PIC  X(100) VALUE HIGH-VALUE.
      *    *** PIN2
             05  TBL01-SITE1   PIC  X(200) VALUE SPACE.
             05  TBL01-SITE2   PIC  X(200) VALUE SPACE.
      *    *** PIN3 twiiter,instagram 他
             05  TBL01-SITE    OCCURS 50
                               PIC  X(500) VALUE SPACE.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** WRITE POT1 (HTML 前データ出力)
           PERFORM S050-10     THRU    S050-EX



      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 TBL SET
                   PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM



      *    *** TBL01 SORT
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-SEIYU

      *    *** SORT 2個あると、（異常終了する）
      *    *** 01 レベルで分ければ問題なさそう



      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** PIN3 TBL SET
                   PERFORM S042-10     THRU    S042-EX
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

      *    *** データ追加したので、再度ＳＯＲＴする
      *    *** TBL01 SORT
           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-SEIYU

      *    *** #NN link 出力
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   EVALUATE PIN1-REC (1:1)
                       WHEN "#"
      *    *** #NN レコード編集3
                           PERFORM S130-10     THRU    S130-EX
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,OPEN PIN1
           PERFORM S060-10     THRU    S060-EX



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   EVALUATE PIN1-REC (1:1)
                       WHEN "#"
                           IF      PIN1-REC (1:3) =    "#01"
      *    *** #NN レコード編集1
                                   PERFORM S110-10     THRU    S110-EX
                           ELSE
      *    *** #NN レコード編集2
                                   PERFORM S120-10     THRU    S120-EX
                           END-IF
      *    *** 1,1 = SPACE はコメント
                       WHEN " "
                           CONTINUE
                       WHEN OTHER
      *    *** <td> データ出力
                           PERFORM S100-10     THRU    S100-EX
                   END-EVALUATE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** WRITE POT1 (HTML 後データ出力)
           PERFORM S070-10     THRU    S070-EX

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

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "O"         TO      WDE06-ID
           CALL    "DECODE06"  USING   WDE06-AREA

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           SET     TBL01-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           MOVE    SPACE       TO      WK-SEIYU1
                                       WK-IMGLINK
           MOVE    ZERO        TO      WK-SEIYU1-LEN
                                       WK-IMGLINK-LEN

           IF      WK-PIN1-STATUS =    ZERO
               IF      SW-FIRST    =       "N"
                   ADD     1           TO      WK-PIN1-CNT
               END-IF

      *    *** 256バイトまでしか入らない
                   UNSTRING PIN1-REC
      *                     DELIMITED BY "," OR SPACE
                           DELIMITED BY ","
                           INTO
                           WK-SEIYU1  COUNT WK-SEIYU1-LEN
                           WK-IMGLINK COUNT WK-IMGLINK-LEN
                   IF      WK-IMGLINK (1:1) =  SPACE
                           MOVE    ZERO        TO      WK-IMGLINK-LEN
                   END-IF
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
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F

           MOVE    SPACE       TO      WK-SEIYU2
                                       WK-SITE1
                                       WK-SITE2
           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT

      *    *** 256バイトまでしか入らない
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-SEIYU2
                           WK-SITE1
                           WK-SITE2
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
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           IF      TBL01-IDX   >       1000
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER 1 TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-SEIYU2   TO      TBL01-SEIYU (TBL01-IDX)
           MOVE    WK-SITE1    TO      TBL01-SITE1 (TBL01-IDX)
           MOVE    WK-SITE2    TO      TBL01-SITE2 (TBL01-IDX)
           SET     TBL01-IDX-MAX TO    TBL01-IDX

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           READ    PIN3-F

           MOVE    SPACE       TO      WK-SEIYU2
                                       WK-SITE-TBL
           IF      WK-PIN3-STATUS =    ZERO
                   ADD     1           TO      WK-PIN3-CNT

      *    *** 256バイトまでしか入らない
                   UNSTRING PIN3-REC
                           DELIMITED BY ","
                           INTO
                           WK-SEIYU2
                           WK-SITE (001)
                           WK-SITE (002)
                           WK-SITE (003)
                           WK-SITE (004)
                           WK-SITE (005)
                           WK-SITE (006)
                           WK-SITE (007)
                           WK-SITE (008)
                           WK-SITE (009)
                           WK-SITE (010)
                           WK-SITE (011)
                           WK-SITE (012)
                           WK-SITE (013)
                           WK-SITE (014)
                           WK-SITE (015)
                           WK-SITE (016)
                           WK-SITE (017)
                           WK-SITE (018)
                           WK-SITE (019)
                           WK-SITE (020)
                           WK-SITE (021)
                           WK-SITE (022)
                           WK-SITE (023)
                           WK-SITE (024)
                           WK-SITE (025)
                           WK-SITE (026)
                           WK-SITE (027)
                           WK-SITE (028)
                           WK-SITE (029)
                           WK-SITE (030)
                           WK-SITE (031)
                           WK-SITE (032)
                           WK-SITE (033)
                           WK-SITE (034)
                           WK-SITE (035)
                           WK-SITE (036)
                           WK-SITE (037)
                           WK-SITE (038)
                           WK-SITE (039)
                           WK-SITE (040)
                           WK-SITE (041)
                           WK-SITE (042)
                           WK-SITE (043)
                           WK-SITE (044)
                           WK-SITE (045)
                           WK-SITE (046)
                           WK-SITE (047)
                           WK-SITE (048)
                           WK-SITE (049)
                           WK-SITE (050)

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "PIN3-REC"  TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN3-REC

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "WK-SEIYU2"  TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-SEIYU2
           ELSE
               IF  WK-PIN3-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S040-EX.
           EXIT.

      *    *** PIN3 TBL SET
       S042-10.

           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL01-SEIYU (TBL01-IDX) =  WK-SEIYU2
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "WK-SEIYU2"  TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-SEIYU2

           IF      SW-SEARCH   =       "Y"

                   MOVE    WK-SITE (01) TO     TBL01-SITE (TBL01-IDX 1)
                   MOVE    WK-SITE (02) TO     TBL01-SITE (TBL01-IDX 2)
                   MOVE    WK-SITE (03) TO     TBL01-SITE (TBL01-IDX 3)
                   MOVE    WK-SITE (04) TO     TBL01-SITE (TBL01-IDX 4)
                   MOVE    WK-SITE (05) TO     TBL01-SITE (TBL01-IDX 5)
                   MOVE    WK-SITE (06) TO     TBL01-SITE (TBL01-IDX 6)
                   MOVE    WK-SITE (07) TO     TBL01-SITE (TBL01-IDX 7)
                   MOVE    WK-SITE (08) TO     TBL01-SITE (TBL01-IDX 8)
                   MOVE    WK-SITE (09) TO     TBL01-SITE (TBL01-IDX 9)
                   MOVE    WK-SITE (10) TO     TBL01-SITE (TBL01-IDX 10)
                   MOVE    WK-SITE (11) TO     TBL01-SITE (TBL01-IDX 11)
                   MOVE    WK-SITE (12) TO     TBL01-SITE (TBL01-IDX 12)
                   MOVE    WK-SITE (13) TO     TBL01-SITE (TBL01-IDX 13)
                   MOVE    WK-SITE (14) TO     TBL01-SITE (TBL01-IDX 14)
                   MOVE    WK-SITE (15) TO     TBL01-SITE (TBL01-IDX 15)
                   MOVE    WK-SITE (16) TO     TBL01-SITE (TBL01-IDX 16)
                   MOVE    WK-SITE (17) TO     TBL01-SITE (TBL01-IDX 17)
                   MOVE    WK-SITE (18) TO     TBL01-SITE (TBL01-IDX 18)
                   MOVE    WK-SITE (19) TO     TBL01-SITE (TBL01-IDX 19)
                   MOVE    WK-SITE (20) TO     TBL01-SITE (TBL01-IDX 20)
                   MOVE    WK-SITE (21) TO     TBL01-SITE (TBL01-IDX 21)
                   MOVE    WK-SITE (22) TO     TBL01-SITE (TBL01-IDX 22)
                   MOVE    WK-SITE (23) TO     TBL01-SITE (TBL01-IDX 23)
                   MOVE    WK-SITE (24) TO     TBL01-SITE (TBL01-IDX 24)
                   MOVE    WK-SITE (25) TO     TBL01-SITE (TBL01-IDX 25)
                   MOVE    WK-SITE (26) TO     TBL01-SITE (TBL01-IDX 26)
                   MOVE    WK-SITE (27) TO     TBL01-SITE (TBL01-IDX 27)
                   MOVE    WK-SITE (28) TO     TBL01-SITE (TBL01-IDX 28)
                   MOVE    WK-SITE (29) TO     TBL01-SITE (TBL01-IDX 29)
                   MOVE    WK-SITE (30) TO     TBL01-SITE (TBL01-IDX 30)
                   MOVE    WK-SITE (31) TO     TBL01-SITE (TBL01-IDX 31)
                   MOVE    WK-SITE (32) TO     TBL01-SITE (TBL01-IDX 32)
                   MOVE    WK-SITE (33) TO     TBL01-SITE (TBL01-IDX 33)
                   MOVE    WK-SITE (34) TO     TBL01-SITE (TBL01-IDX 34)
                   MOVE    WK-SITE (35) TO     TBL01-SITE (TBL01-IDX 35)
                   MOVE    WK-SITE (36) TO     TBL01-SITE (TBL01-IDX 36)
                   MOVE    WK-SITE (37) TO     TBL01-SITE (TBL01-IDX 37)
                   MOVE    WK-SITE (38) TO     TBL01-SITE (TBL01-IDX 38)
                   MOVE    WK-SITE (39) TO     TBL01-SITE (TBL01-IDX 39)
                   MOVE    WK-SITE (40) TO     TBL01-SITE (TBL01-IDX 40)
                   MOVE    WK-SITE (41) TO     TBL01-SITE (TBL01-IDX 41)
                   MOVE    WK-SITE (42) TO     TBL01-SITE (TBL01-IDX 42)
                   MOVE    WK-SITE (43) TO     TBL01-SITE (TBL01-IDX 43)
                   MOVE    WK-SITE (44) TO     TBL01-SITE (TBL01-IDX 44)
                   MOVE    WK-SITE (45) TO     TBL01-SITE (TBL01-IDX 45)
                   MOVE    WK-SITE (46) TO     TBL01-SITE (TBL01-IDX 46)
                   MOVE    WK-SITE (47) TO     TBL01-SITE (TBL01-IDX 47)
                   MOVE    WK-SITE (48) TO     TBL01-SITE (TBL01-IDX 48)
                   MOVE    WK-SITE (49) TO     TBL01-SITE (TBL01-IDX 49)
                   MOVE    WK-SITE (50) TO     TBL01-SITE (TBL01-IDX 50)
           ELSE
                   SET     TBL01-IDX   TO      TBL01-IDX-MAX
                   SET     TBL01-IDX   UP  BY  1

                   IF      TBL01-IDX   >       1000
                           DISPLAY WK-PGM-NAME
                                   " TBL01-TBL OVER 2 TBL01-IDX="
                                   TBL01-IDX
                           STOP    RUN
                   END-IF

      *    *** PIN2 に声優名無い時自動で追加する
                   MOVE    WK-SEIYU2    TO     TBL01-SEIYU (TBL01-IDX)
                   MOVE    WK-SITE (01) TO     TBL01-SITE (TBL01-IDX 1)
                   MOVE    WK-SITE (02) TO     TBL01-SITE (TBL01-IDX 2)
                   MOVE    WK-SITE (03) TO     TBL01-SITE (TBL01-IDX 3)
                   MOVE    WK-SITE (04) TO     TBL01-SITE (TBL01-IDX 4)
                   MOVE    WK-SITE (05) TO     TBL01-SITE (TBL01-IDX 5)
                   MOVE    WK-SITE (06) TO     TBL01-SITE (TBL01-IDX 6)
                   MOVE    WK-SITE (07) TO     TBL01-SITE (TBL01-IDX 7)
                   MOVE    WK-SITE (08) TO     TBL01-SITE (TBL01-IDX 8)
                   MOVE    WK-SITE (09) TO     TBL01-SITE (TBL01-IDX 9)
                   MOVE    WK-SITE (10) TO     TBL01-SITE (TBL01-IDX 10)
                   MOVE    WK-SITE (11) TO     TBL01-SITE (TBL01-IDX 11)
                   MOVE    WK-SITE (12) TO     TBL01-SITE (TBL01-IDX 12)
                   MOVE    WK-SITE (13) TO     TBL01-SITE (TBL01-IDX 13)
                   MOVE    WK-SITE (14) TO     TBL01-SITE (TBL01-IDX 14)
                   MOVE    WK-SITE (15) TO     TBL01-SITE (TBL01-IDX 15)
                   MOVE    WK-SITE (16) TO     TBL01-SITE (TBL01-IDX 16)
                   MOVE    WK-SITE (17) TO     TBL01-SITE (TBL01-IDX 17)
                   MOVE    WK-SITE (18) TO     TBL01-SITE (TBL01-IDX 18)
                   MOVE    WK-SITE (19) TO     TBL01-SITE (TBL01-IDX 19)
                   MOVE    WK-SITE (20) TO     TBL01-SITE (TBL01-IDX 20)
                   MOVE    WK-SITE (21) TO     TBL01-SITE (TBL01-IDX 21)
                   MOVE    WK-SITE (22) TO     TBL01-SITE (TBL01-IDX 22)
                   MOVE    WK-SITE (23) TO     TBL01-SITE (TBL01-IDX 23)
                   MOVE    WK-SITE (24) TO     TBL01-SITE (TBL01-IDX 24)
                   MOVE    WK-SITE (25) TO     TBL01-SITE (TBL01-IDX 25)
                   MOVE    WK-SITE (26) TO     TBL01-SITE (TBL01-IDX 26)
                   MOVE    WK-SITE (27) TO     TBL01-SITE (TBL01-IDX 27)
                   MOVE    WK-SITE (28) TO     TBL01-SITE (TBL01-IDX 28)
                   MOVE    WK-SITE (29) TO     TBL01-SITE (TBL01-IDX 29)
                   MOVE    WK-SITE (30) TO     TBL01-SITE (TBL01-IDX 30)
                   MOVE    WK-SITE (31) TO     TBL01-SITE (TBL01-IDX 31)
                   MOVE    WK-SITE (32) TO     TBL01-SITE (TBL01-IDX 32)
                   MOVE    WK-SITE (33) TO     TBL01-SITE (TBL01-IDX 33)
                   MOVE    WK-SITE (34) TO     TBL01-SITE (TBL01-IDX 34)
                   MOVE    WK-SITE (35) TO     TBL01-SITE (TBL01-IDX 35)
                   MOVE    WK-SITE (36) TO     TBL01-SITE (TBL01-IDX 36)
                   MOVE    WK-SITE (37) TO     TBL01-SITE (TBL01-IDX 37)
                   MOVE    WK-SITE (38) TO     TBL01-SITE (TBL01-IDX 38)
                   MOVE    WK-SITE (39) TO     TBL01-SITE (TBL01-IDX 39)
                   MOVE    WK-SITE (40) TO     TBL01-SITE (TBL01-IDX 40)
                   MOVE    WK-SITE (41) TO     TBL01-SITE (TBL01-IDX 41)
                   MOVE    WK-SITE (42) TO     TBL01-SITE (TBL01-IDX 42)
                   MOVE    WK-SITE (43) TO     TBL01-SITE (TBL01-IDX 43)
                   MOVE    WK-SITE (44) TO     TBL01-SITE (TBL01-IDX 44)
                   MOVE    WK-SITE (45) TO     TBL01-SITE (TBL01-IDX 45)
                   MOVE    WK-SITE (46) TO     TBL01-SITE (TBL01-IDX 46)
                   MOVE    WK-SITE (47) TO     TBL01-SITE (TBL01-IDX 47)
                   MOVE    WK-SITE (48) TO     TBL01-SITE (TBL01-IDX 48)
                   MOVE    WK-SITE (49) TO     TBL01-SITE (TBL01-IDX 49)
                   MOVE    WK-SITE (50) TO     TBL01-SITE (TBL01-IDX 50)
                   SET     TBL01-IDX-MAX TO    TBL01-IDX

           END-IF
           .
       S042-EX.
           EXIT.

      *    *** WRITE POT1 (HTML 前データ出力)
       S050-10.

           MOVE    "<DOCTYPE html>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<html>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<head>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<meta charset="utf-8">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<title>"   TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-TITLE    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</title>"  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE
           '<link rel="stylesheet" type="text/css" href="mystyle.css">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</head>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<body>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<h1>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-TITLE    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</h1>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE
      *     '<img src="C:\Users\xxxx\OneDrive\Hackadoll\'
           '<img src="Hackadoll\'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "RND"       TO      WCR-ID
           MOVE    99          TO      WCR-IDX
      *     MOVE    1           TO      WCR-FROM   (1)
      *     MOVE    100         TO      WCR-TO-CNT (1)
      *     MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    "N"         TO      WCR-SIGN   (1)
           MOVE    "N"         TO      WCR-ZERO   (1)
      *     MOVE    1           TO      WCR-FROM2  (1)
      *     MOVE    528         TO      WCR-TO2    (1)

           CALL    "COBRND"    USING   WCR-COBRND-AREA
           IF      WDT-DATE-SM =       ZERO
                   MOVE    1           TO      WK-NUM
           ELSE
                   COMPUTE WK-NUM ROUNDED = WCR-RND (WDT-DATE-SM) * 1143
           END-IF
           DIVIDE  WDT-DATE-DD BY 6 GIVING WK-DD-SHO
                       REMAINDER WK-DD-AMARI
      *    *** WK-NUM に０－６の値を加える
           ADD     WK-DD-AMARI TO      WK-NUM
           IF      WK-NUM      >       1143
                   ADD     -1143        TO      WK-NUM
           END-IF
      *     DISPLAY "WK-NUM=" WK-NUM
      *     DISPLAY "WCR-RND (1)=" WCR-RND (1)
      *     MOVE    WK-NUM      TO      POT1-REC
      *     MOVE    509         TO      POT1-REC

           MOVE    "S"         TO      WDE06-ID
           MOVE    WK-NUM      TO      WDE06-NUM
           CALL    "DECODE06"  USING   WDE06-AREA
           MOVE    WDE06-FILE  TO      POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '" alt=""'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *     MOVE    '" style="position:relative; left:400px;'
           MOVE    '" style="float:right; '
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ' width:400; height:auto; " ><br>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S050-EX.
           EXIT.

      *    *** CLOSE,OPEN PIN1
       S060-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           MOVE    LOW-VALUE   TO      WK-PIN1-EOF
           MOVE    "N"         TO      SW-FIRST
           .
       S060-EX.
           EXIT.

      *    *** WRITE POT1 (HTML 後データ出力)
       S070-10.

           MOVE    '</tr></table><a href="#top">TOP</a></body></html>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S070-EX.
           EXIT.

      *    *** <td> データ出力
       S100-10.

           ADD     1           TO      I
           IF      I           >       8
                   MOVE    1           TO      I
                   MOVE    "</tr>"     TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "<tr>"      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    '<td>'      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    '<p class="welcome">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** YouTube %23=#
           MOVE
           '<a href="https://www.youtube.com/results?search_query=%23'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *     MOVE    WK-PIN1-LEN TO      WDE03-BUF1-LEN
           MOVE    WK-SEIYU1-LEN TO    WDE03-BUF1-LEN
           IF      WK-SEIYU1-LEN =     1000
                   DISPLAY WK-PGM-NAME " PIN1-REC カンマ無しエラー"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-IF
      *     CALL    "DECODE03"  USING   PIN1-REC
           CALL    "DECODE03"  USING   WK-SEIYU1
                                       WDE03-BUF1-LEN
                                       WDE03-BUF2

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      J2
           PERFORM VARYING J FROM 1 BY 1
      *             UNTIL J > WK-PIN1-LEN
                   UNTIL J > WK-SEIYU1-LEN
               IF (PIN1-REC (J:1) >= "A" AND
                   PIN1-REC (J:1) <= "Z" )  OR
                  (PIN1-REC (J:1) >= "a" AND
                   PIN1-REC (J:1) <= "z" )
                   ADD     1           TO      J2
                   MOVE    PIN1-REC (J:1) TO   POT1-REC (J2:1)
               ELSE
                   ADD     1           TO      J2
                   MOVE    "%"         TO      POT1-REC (J2:1)
                   ADD     1           TO      J2
                   MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                   ADD     1           TO      J2
                   MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
               END-IF
           END-PERFORM

      *    *** ささきのぞみ
           IF      PIN1-REC    = X"E38195E38195E3818DE381AEE3819EE381BF"
      *    *** 伊藤美紀
                OR PIN1-REC    = X"E4BC8AE897A4E7BE8EE7B480"
                   ADD     1           TO      J2
      *    *** 　声優
                   MOVE    "%E3%80%80%E5%A3%B0%E5%84%AA"
                                       TO      POT1-REC (J2:27)
           END-IF

           IF      PIN1-REC    =       "Lynn" OR
                   PIN1-REC    =       "Poppin'PartyPile" OR
                   PIN1-REC    =       "Afterglow" OR
                   PIN1-REC    =       "Pastel*Palettes" OR
                   PIN1-REC    =       "Roselia" OR
                   PIN1-REC    =       "RAISE A SUILEN" OR
                   PIN1-REC    =       "Molfonica" OR
                   PIN1-REC    =       "Machico" OR
                 ( PIN1-REC (1:1) =    "M" AND 
                   PIN1-REC (5:1) =    "A" AND
                   PIN1-REC (9:1) =    "O" ) OR
                   PIN1-REC    =       "Egg"
                   MOVE    PIN1-REC    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           MOVE    POT1-REC    TO      WK-KENSAKU

           IF      WK-IMGLINK-LEN =    ZERO

                   MOVE    '"><img src="image\'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
      *    *** 声優名、ＩＭＧＬＩＮＫ無
      *             MOVE    PIN1-REC    TO      POT1-REC
                   MOVE    WK-SEIYU1   TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.jpg" loading="lazy" alt="'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-SEIYU1   TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *     MOVE    '.jpg" width="85" height="128">'
      *     MOVE    '.jpg" width="100%">'
      *     MOVE    '.jpg" width="125" height="auto">'
                   MOVE    '.jpg" class="welcome"><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE

                   MOVE    '"><img src="' TO   POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
      *    *** 声優名、ＩＭＧＬＩＮＫ有

                   MOVE    WK-IMGLINK  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" loading="lazy" alt="'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-SEIYU1   TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" class="welcome"><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

      *     MOVE    PIN1-REC    TO      POT1-REC
           MOVE    WK-SEIYU1   TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a><br>"  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** google
           MOVE
                   '<a href="https://www.google.co.jp/search?q='
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '"><br>google</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** wiki
           MOVE
                   '<a href="https://ja.wikipedia.org/wiki/'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-KENSAKU  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '"><br>wiki</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

      *         WHEN TBL01-SEIYU (TBL01-IDX) =  PIN1-REC (1:100)
      *         WHEN TBL01-SEIYU (TBL01-IDX) (1:WK-SEIYU1-LEN)
      *             = WK-SEIYU1 (1:WK-SEIYU1-LEN)
      *    *** 中国　前２桁下と同じなので、別のテーブルサーチしてしまうので
      *    *** １００バイト分で一致するかでチェックする
      *    *** 中国音楽
               WHEN TBL01-SEIYU (TBL01-IDX) (1:100)
                   = WK-SEIYU1 (1:100)
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH   =       "Y"
               AND TBL01-SITE1 (TBL01-IDX) NOT = SPACE

                   MOVE    '<a href="' TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    TBL01-SITE1 (TBL01-IDX)
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '"><br><br>animate_info</a>'
                                       TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="' TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    TBL01-SITE2 (TBL01-IDX)
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '"><br>animate_Photo</a><br>'
                                       TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           IF      SW-SEARCH   =       "Y"
               AND TBL01-SITE1 (TBL01-IDX) = SPACE

                   MOVE    '<br>'      TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           IF      SW-SEARCH   =       "Y" 
               PERFORM VARYING K FROM 1 BY 1
                       UNTIL K > 50
                 IF      TBL01-SITE (TBL01-IDX K) (1:1) NOT = SPACE

                   MOVE    '<br><br>'  TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '<a href="' TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    SPACE       TO      POT1-REC
                                               WK-SITE-NAME

                   UNSTRING TBL01-SITE (TBL01-IDX K)
                           DELIMITED BY " !"
                           INTO
                           POT1-REC
                           WK-SITE-NAME

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '">'        TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-SITE-NAME = SPACE
                     IF      TBL01-SITE (TBL01-IDX K) (1:20) = 
                           "https://twitter.com/"
                            MOVE    "twitter"  TO      POT1-REC
                       IF      TBL01-SITE (TBL01-IDX K) (21:7) = 
                               "hashtag"
                               MOVE "/hashtag" TO      POT1-REC (8:8)
                       END-IF
                     ELSE
                       IF      TBL01-SITE (TBL01-IDX K) (1:26) = 
                           "https://www.instagram.com/"
                           MOVE    "instagram" TO      POT1-REC
                       ELSE
      *                     MOVE    "other"     TO      POT1-REC
                           EVALUATE TRUE
                               WHEN TBL01-SITE (TBL01-IDX K) (1:11) =
                                    "http://www."
                                   MOVE TBL01-SITE (TBL01-IDX K) (12:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:7) =
                                    "http://"
                                   MOVE TBL01-SITE (TBL01-IDX K) (8:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:12) =
                                    "https://www."
                                   MOVE TBL01-SITE (TBL01-IDX K) (13:18)
                                               TO      POT1-REC
                               WHEN TBL01-SITE (TBL01-IDX K) (1:8) =
                                    "https://"
                                   MOVE TBL01-SITE (TBL01-IDX K) (9:18)
                                               TO      POT1-REC
                               WHEN OTHER
                                   MOVE TBL01-SITE (TBL01-IDX K) (1:18)
                                               TO      POT1-REC
                           END-EVALUATE
                       END-IF
                   ELSE
                     MOVE    WK-SITE-NAME TO     POT1-REC
                   END-IF

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>'      TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                 END-IF
               END-PERFORM
           END-IF

           MOVE    "</p></td>" TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S100-EX.
           EXIT.

      *    *** #NN レコード編集1
       S110-10.

           MOVE    '<h2><br><a name="  ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:2)
                               TO      POT1-REC (18:2)
           WRITE   POT1-REC
           MOVE    1           TO      WK-NO
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (5:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a></h2><table border='1'><tr>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I
           .
       S110-EX.
           EXIT.

      *    *** #NN レコード編集2
       S120-10.

           MOVE    '</tr></table><a href="#top">TOP</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** <br> １つだと、自動的に連番が段落に入る 入らない時もある
      *    *** <br> ２つだと、自動的に連番が段落に入らない
           MOVE    '<h2><br><a name="  ">'
                               TO      POT1-REC
      *     MOVE    PIN1-REC (2:2) TO   POT1-REC (18:2)
           ADD     1           TO      WK-NO
           MOVE    WK-NO       TO      POT1-REC (18:2)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (5:)
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</a></h2><table border='1'><tr>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    ZERO        TO      I

           .
       S120-EX.
           EXIT.

      *    *** #NN レコード編集3
       S130-10.

           IF      PIN1-REC (1:3) =    "#01"
                   MOVE    ZERO        TO      WK-NO



                   MOVE '<a href="https://twitter.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** Twitter.com
                   MOVE    "X"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.instagram.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** Instagram.com
                   MOVE    "Instagram"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.tiktok.com/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** TikTok.com
                   MOVE    "TikTok"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.ganjingworld.com/ja-JP">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** Ganjingworld.com
                   MOVE    "GanJingWorld"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE '<a href="https://www.animatetimes.com/seiyu/">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 声優ニュース
                   MOVE    X"E5A3B0E584AAE3838BE383A5E383BCE382B9"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE    '<a href="index.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE
              '<img src="image\icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 声優
                   MOVE    X'E5A3B0E584AA'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'  TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE    '<a href="indexanime.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE
              '<img src="image\icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** アニメ年代順
                   MOVE    X'E382A2E3838BE383A1E5B9B4E4BBA3E9A086'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE    '<a href="indexanimesort.html">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE
              '<img src="image\icon136.gif" alt="icon136.gif" width=20>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** アニメタイトル順
                   MOVE    
                   X'E382A2E3838BE383A1E382BFE382A4E38388E383ABE9A086'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

           END-IF

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NN
           MOVE    PIN1-REC (1:1)
                               TO      POT1-REC
           ADD     1           TO      WK-NO
           MOVE    WK-NO       TO      POT1-REC (2:2)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '"><br>'    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NN ＸＸＸＸＸ => ＸＸＸＸＸ
           MOVE    PIN1-REC (5:)
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '</a>'      TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.
           .

      *    *** 現在未使用
      *    *** index%XX 16進=> UTF8 漢字にする
       S140-10.

           MOVE    ZERO        TO      L2
           MOVE    SPACE       TO      WK-REC2
      *    *** index%E7%9F%B3%E5%8E%9F%E5%A4%8F%E7%B9%94youtube.html =>
      *    *** index石原夏織youtube.html
      *    *** 123,1 はindexの次のバイト
           PERFORM VARYING L FROM 123 BY 1
                   UNTIL WK-REC (L:1) = SPACE
                   IF      WK-REC (L:1) =     "%"
                       MOVE    WK-REC (L:3) TO     WK-UTF8-1
      *    *** %E7%9F%B3 => 石
                       CALL    "DECODE02"  USING   WK-UTF8-1
                                                   WK-BUF1-L
                                                   WK-UTF8-2
                                                   WK-BUF2-L
                       ADD     1           TO      L2
                       MOVE    WK-UTF8-2   TO      WK-REC2 (L2:1)
                       ADD     2           TO      L
                   ELSE
                       ADD     1           TO      L2
                       MOVE    WK-REC (L:1) TO     WK-REC2 (L2:1)
                   END-IF
           END-PERFORM
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

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "C"         TO      WDE06-ID
           CALL    "DECODE06"  USING   WDE06-AREA

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
