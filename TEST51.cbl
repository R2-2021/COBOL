      *    *** YouTibe html アニメ作品 （タイトル順）作成

      *    *** JOB=C.TEST61

      *    *** TEST61
      *    ***   |
      *    *** TEST62
      *    ***   |
      *    *** COBSORT COBSORT.T003.PRM1
      *    ***   |
      *    *** TEST63
      *    ***   |
      *    *** TEST51 アニメ 名前順 ｈｔｍｌ 作成
      *    ***   |
      *    *** COBSORT COBSORT.T009.PRM1
      *    ***   |
      *    *** TEST73
      *    ***   |
      *    *** TEST50 アニメ 年代順 ｈｔｍｌ 作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST51.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** アニメ作品データ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** twiiter,instagram データ　未使用
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** HTML データ
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
           03  FILLER          PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST51  ".

      *    *** COBSORT でタイトルでソートしておく
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
      *         "TEST28_201110_202007SORT.csv".
      *          "TEST51.PIN1".
                "TEST63.POT1".

           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST51.PIN2".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST51.POT1".
      *     03  WK-POT1-F-NAME  PIC  X(032) VALUE "indexanimesort.html".
           03  WK-POT1-F-NAME.
             05 PIC X(023) VALUE "C:\Users\koko\OneDrive\".
             05 PIC X(012) VALUE "ドキュメント".
             05 PIC X(013) VALUE "\HTML\YouTube".
             05 PIC X(004) VALUE "声優".
             05 PIC X(020) VALUE "\indexanimesort.html".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** YouTube アニメタイトル順
           03  WK-TITLE-ANIME.
             05                PIC  X(008) VALUE "YouTube ".
             05                PIC  X(009) VALUE X"E382A2E3838BE383A1".
             05                PIC  X(015) VALUE 
               X"E382BFE382A4E38388E383ABE9A086".

           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-SITE1        PIC  X(200) VALUE SPACE.
           03  WK-SITE2        PIC  X(256) VALUE SPACE.
           03  WK-SITE3        PIC  X(256) VALUE SPACE.
           03  WK-SITE4        PIC  X(256) VALUE SPACE.
           03  WK-SITE5        PIC  X(256) VALUE SPACE.
           03  WK-KENSAKU      PIC  X(256) VALUE SPACE.
           03  WK-NUM          PIC  9(004) VALUE ZERO.

           03  WK-YYYYMM.
             05  WK-YYYY2      PIC  X(004) VALUE SPACE.
             05                PIC  X(001) VALUE ".".
             05  WK-MM2        PIC  X(002) VALUE SPACE.

           03  WK-YYYY         PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  X(002) VALUE SPACE.
           03  WK-SEASON       PIC  X(003) VALUE SPACE.
           03  WK-TITLE        PIC  X(256) VALUE SPACE.
           03  WK-SITE         PIC  X(256) VALUE SPACE.
           03  WK-IMG          PIC  X(256) VALUE SPACE.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-SITE-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-LEFT-POS     PIC  9(004) VALUE ZERO.
           03  WK-DD-SHO       BINARY-LONG SYNC VALUE ZERO.
           03  WK-DD-AMARI     BINARY-LONG SYNC VALUE ZERO.

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

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-TITLE
                               INDEXED BY TBL01-IDX.
             05  TBL01-TITLE   PIC  X(100) VALUE HIGH-VALUE.
      *    *** PIN2 twiiter,instagram
             05  TBL01-SITE    OCCURS 5
                               PIC  X(256) VALUE SPACE.

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
                   ASCENDING KEY TBL01-TITLE



      *    *** #NN link 出力
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   EVALUATE WK-YYYY (1:1)
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
                   EVALUATE WK-YYYY (1:1)
                       WHEN "#"
                           IF      WK-YYYY (1:4) =     "#001"
      *    *** #NN レコード編集1
                                   PERFORM S110-10     THRU    S110-EX
                           ELSE
      *    *** #NN レコード編集2
                                   PERFORM S120-10     THRU    S120-EX
                           END-IF
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

           SET     TBL01-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           MOVE    SPACE       TO      WK-YYYY
                                       WK-MM
                                       WK-SEASON
                                       WK-TITLE
                                       WK-SITE
                                       WK-IMG

           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-SITE-LEN
                                       WK-IMG-LEN

           IF      WK-PIN1-STATUS =    ZERO
                   IF      SW-FIRST    =       "N"
                       ADD     1           TO      WK-PIN1-CNT
                   END-IF
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-YYYY
                           WK-MM
                           WK-SEASON
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-SITE     COUNT WK-SITE-LEN
                           WK-IMG      COUNT WK-IMG-LEN
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

           MOVE    SPACE       TO      WK-TITLE2
                                       WK-SITE1
                                       WK-SITE2
                                       WK-SITE3
                                       WK-SITE4
                                       WK-SITE5

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT

      *    *** 256バイトまでしか入らない
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE2
                           WK-SITE1
                           WK-SITE2
                           WK-SITE3
                           WK-SITE4
                           WK-SITE5
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
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2   TO      TBL01-TITLE (TBL01-IDX)
           MOVE    WK-SITE1    TO      TBL01-SITE  (TBL01-IDX 1)
           MOVE    WK-SITE2    TO      TBL01-SITE  (TBL01-IDX 2)
           MOVE    WK-SITE3    TO      TBL01-SITE  (TBL01-IDX 3)
           MOVE    WK-SITE4    TO      TBL01-SITE  (TBL01-IDX 4)
           MOVE    WK-SITE5    TO      TBL01-SITE  (TBL01-IDX 5)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
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

           MOVE    WK-TITLE-ANIME TO   POT1-REC
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

           MOVE    WK-TITLE-ANIME TO   POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</h1>"
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE
      *     '<img src="C:\Users\koko\OneDrive\Hackadoll\'
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
                   ADD     -1143       TO      WK-NUM
           END-IF

      *     DISPLAY "WDT-DATE-SM=" WDT-DATE-SM
      *     DISPLAY "WK-NUM     =" WK-NUM
      *     DISPLAY "WDT-DATE-DD=" WDT-DATE-DD
      *     DISPLAY "WK-DD-SHO  =" WK-DD-SHO
      *     DISPLAY "WK-DD-AMARI=" WK-DD-AMARI
      *     PERFORM VARYING L FROM 1 BY 1
      *             UNTIL L > 99
      *             DISPLAY "L=" L " WCR-RND (L)=" WCR-RND (L)
      *     END-PERFORM

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

           MOVE    "<td>"     TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<p class="welcome">'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-SITE     TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    '">'        TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
      *     MOVE    '<img src=image\"' TO    POT1-REC
           IF      WK-IMG (1:1) =      SPACE
                   MOVE 
                   '<img src="C:\Users\koko\Pictures\anime\animate'
                               TO      POT1-REC
                   MOVE    PIN1-REC (1:4) TO   POT1-REC (47:4)
                   MOVE    PIN1-REC (6:2) TO   POT1-REC (51:2)
                   MOVE    "\"         TO      POT1-REC (53:1)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.jpg" alt="'        TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.jpg" class="welcome"><br><br>'
                               TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   MOVE    '<img src="' TO     POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-IMG      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '" loading="lazy" alt="'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-TITLE    TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '.jpg" class="welcome"><br><br>'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    WK-TITLE    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "<br></a>"  TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** YouTube
           MOVE
           '<a href="https://www.youtube.com/results?search_query='
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-TITLE-LEN TO     WDE03-BUF1-LEN
           CALL    "DECODE03"  USING   WK-TITLE
                                       WDE03-BUF1-LEN
                                       WDE03-BUF2

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      J2
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-TITLE-LEN
                   ADD     1           TO      J2
                   MOVE    "%"         TO      POT1-REC (J2:1)
                   ADD     1           TO      J2
                   MOVE    WDE03-BUF2-L (J) TO POT1-REC (J2:1)
                   ADD     1           TO      J2
                   MOVE    WDE03-BUF2-R (J) TO POT1-REC (J2:1)
           END-PERFORM

           WRITE   POT1-REC
           MOVE    POT1-REC    TO      WK-KENSAKU
           ADD     1           TO      WK-POT1-CNT

           MOVE    '"><br>YouTube</a>' 
                               TO      POT1-REC
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

               WHEN TBL01-TITLE (TBL01-IDX) =  WK-TITLE (1:100)
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH   =       "Y"

               PERFORM VARYING K FROM 1 BY 1
                       UNTIL K > 5
                 IF      TBL01-SITE (TBL01-IDX K) (1:1) NOT = SPACE
                   MOVE    '<a href="' TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    TBL01-SITE (TBL01-IDX K) TO POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '"><br>'    TO      POT1-REC

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      TBL01-SITE (TBL01-IDX K) (1:20) = 
                           "https://twitter.com/"
                           MOVE    "twitter"   TO      POT1-REC
                   ELSE
                       IF      TBL01-SITE (TBL01-IDX K) (1:26) = 
                           "https://www.instagram.com/"
                           MOVE    "instagram" TO      POT1-REC
                       ELSE
      *                     MOVE    "other"     TO      POT1-REC
                           EVALUATE TRUE
                               WHEN TBL01-SITE (TBL01-IDX K) (1:7) =
                                    "http://"
                                   MOVE TBL01-SITE (TBL01-IDX K) (8:18)
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
                   END-IF

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>'      TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                 END-IF
               END-PERFORM
           END-IF

           MOVE    '<br>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    WK-YYYY     TO      WK-YYYY2
           MOVE    WK-MM       TO      WK-MM2
           MOVE    WK-YYYYMM   TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "</p></td>" TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** #NNN レコード編集1
       S110-10.

           MOVE    '<h2><br><a name="   ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:3) TO   POT1-REC (18:3)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (9:) TO    POT1-REC
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

      *    *** #NNN レコード編集2
       S120-10.

           MOVE    '</tr></table><a href="#top">TOP</a>'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** <br> １つだと、自動的に連番が段落に入る 入らない時もある
      *    *** <br> ２つだと、自動的に連番が段落に入らない
           MOVE    '<h2><br><a name="   ">'
                               TO      POT1-REC
           MOVE    PIN1-REC (2:3) TO   POT1-REC (18:3)
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    PIN1-REC (9:) TO    POT1-REC
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

           IF      PIN1-REC (1:4) =    "#001"



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

                   MOVE    '</a><br>'
                                       TO      POT1-REC
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



                   MOVE 
       '<a href="https://www.animatetimes.com/tag/details.php?id=6212">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 冬アニメ
                   MOVE    X"E586ACE382A2E3838BE383A1"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>&nbsp;&nbsp;&nbsp;' TO POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE 
       '<a href="https://www.animatetimes.com/tag/details.php?id=5228">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 春アニメ
                   MOVE    X"E698A5E382A2E3838BE383A1"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>&nbsp;&nbsp;&nbsp;' 
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE 
       '<a href="https://www.animatetimes.com/tag/details.php?id=5806">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 夏アニメ
                   MOVE    X"E5A48FE382A2E3838BE383A1"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a>&nbsp;&nbsp;&nbsp;' TO POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT



                   MOVE 
       '<a href="https://www.animatetimes.com/tag/details.php?id=5947">'
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

      *    *** 秋アニメ
                   MOVE    X"E7A78BE382A2E3838BE383A1"
                                       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    '</a><br><br>' TO   POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    '<a href="'
                               TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NNN
           MOVE    PIN1-REC (1:4) TO   POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           EVALUATE TRUE

               WHEN PIN1-REC (9:3) =
      *    *** あ、１、Ａ、＋
                       X"E38182" OR X"EFBC91" OR X"EFBCA1" OR X"EFBC8B"
      *    *** か、さ、た、な、
      *    *** は、ま、や、ら、
      *    *** わ
      *    *** 文字の直前に、">を入れたい、文字を指定
                    OR X"E3818B" OR X"E38195" OR X"E3819F" OR X"E381AA"
                    OR X"E381AF" OR X"E381BE" OR X"E38284" OR X"E38289"
                    OR X"E3828F"
                   MOVE    '">'    TO      POT1-REC

      *    *** 
               WHEN OTHER
                   MOVE    '" style="position:relative; left:0000px;">'
                                   TO      POT1-REC
                   ADD     16      TO      WK-LEFT-POS
                   MOVE    WK-LEFT-POS TO  POT1-REC (34:4)
           END-EVALUATE

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** #NNN ＸＸＸＸＸ => ＸＸＸＸＸ
           MOVE    PIN1-REC (9:) TO    POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** う”、９、Ｚ
      *    *** 文字の後に、<br><br>を入れたい、文字を指定
           IF    ( PIN1-REC (9:3) = X"E38294" OR X"EFBC99" OR X"EFBCBA")
                   MOVE    '</a><br><br>' TO   POT1-REC
                   MOVE    ZERO        TO      WK-LEFT-POS
           ELSE
      *    *** お、ご、ぞ、ど、
      *    *** の、ぽ、も、よ
      *    *** ろ、
      *    *** ＋
      *    *** 文字の後に、<br>を入れたい、文字を指定
               IF      PIN1-REC (9:3) =
                       X"E3818A" OR X"E38194" OR X"E3819E" OR X"E381A9"
                    OR X"E381AE" OR X"E381BD" OR X"E38282" OR X"E38288"
                    OR X"E3828D" OR X"EFBC8B"
                   MOVE    '</a><br>'  TO      POT1-REC
                   MOVE    ZERO        TO      WK-LEFT-POS
               ELSE
                   MOVE    '</a>'      TO      POT1-REC
               END-IF
           END-IF
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
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

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
