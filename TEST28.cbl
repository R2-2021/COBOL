      *    *** http://www.kansou.me/archive/2011_autumn.html
      *    *** アニメ新番組一覧編集 2011.10-2016.10 分
      *    *** https://www.animatetimes.com/tag/details.php?id=6212 等
      *    *** 2019.01-

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST28.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST10.PIN2 YYYYMM データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アニメ公式サイトデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F.
       01  PIN2-REC.
           03  PIN2-YYYYMM     PIC  X(006).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST28  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST28.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** 退避エリア
           03  WK-HTML0        PIC  X(256) VALUE SPACE.
      *    *** タイトル名
           03  WK-HTML1        PIC  X(256) VALUE SPACE.
      *    *** サイト
           03  WK-HTML2        PIC  X(256) VALUE SPACE.
      *    *** ｉｍｇ
           03  WK-HTML3        PIC  X(256) VALUE SPACE.

           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML3-L      BINARY-LONG SYNC VALUE ZERO.

           03  WK-HED1.
             05  WK-HED1-YYYY  PIC  X(004) VALUE SPACE.
             05  FILLER        PIC  X(001) VALUE ",".
             05  WK-HED1-MM    PIC  X(002) VALUE SPACE..
             05  FILLER        PIC  X(001) VALUE ",".
             05  WK-HED1-KISETU PIC X(003) VALUE SPACE.
             05  FILLER        PIC  X(001) VALUE ",".

           03  WK-HED2.
             05  WK-FUYU       PIC  X(002) VALUE "冬".
             05  WK-HARU       PIC  X(002) VALUE "春".
             05  WK-NATU       PIC  X(002) VALUE "夏".
             05  WK-AKI        PIC  X(002) VALUE "秋".

      *    *** 公式サイト UTF-8
           03  WK-SITE.
             05  FILLER        PIC  X(003) VALUE X"E585AC".
             05  FILLER        PIC  X(003) VALUE X"E5BC8F".
             05  FILLER        PIC  X(003) VALUE X"E382B5".
             05  FILLER        PIC  X(003) VALUE X"E382A4".
             05  FILLER        PIC  X(003) VALUE X"E38388".
      *    *** 公式HP
           03  WK-SITE2.
             05  FILLER        PIC  X(003) VALUE X"E585AC".
             05  FILLER        PIC  X(003) VALUE X"E5BC8F".
             05  FILLER        PIC  X(002) VALUE "HP".
             05  FILLER        PIC  X(001) VALUE X"09".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SPAN-CLASS   PIC  X(001) VALUE ZERO.
           03  SW-CENTER       PIC  X(001) VALUE ZERO.
           03  SW-KOSHIKI      PIC  X(001) VALUE ZERO.
           03  SW-TD           PIC  X(001) VALUE ZERO.
           03  SW-TITLE        PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN,READ PIN2
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 編集
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
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

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ

      *    *** 1件必須
           IF      WK-PIN2-STATUS =    ZERO
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS " PIN2 1件は必須 REC="
                           PIN2-REC
                   STOP    RUN
           END-IF

           IF      PIN2-YYYYMM (1:4) IS NUMERIC
               AND PIN2-YYYYMM (5:2) >= "01" 
               AND PIN2-YYYYMM (5:2) <= "12" 
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F YYYY NTO NUMERIC OR"
                           " MM < 01 OR > 12 ?"
                   STOP    RUN
           END-IF

           IF      PIN2-YYYYMM =       SPACE
                   CONTINUE
           ELSE
                   MOVE    "_"             TO  WK-POT1-F-NAME (07:01)
                   MOVE    PIN2-YYYYMM     TO  WK-POT1-F-NAME (08:06)
                   MOVE    ".POT1"         TO  WK-POT1-F-NAME (14:05)
           END-IF

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
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           IF      WK-PIN1-STATUS =    ZERO OR 10
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** 編集
       S100-10.

           IF      PIN1-REC(1:1) =     "<"
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-PIN1-LEN
                   IF      PIN1-REC(I:1) =     "<" 
                       MOVE    +1      TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL   PIN1-REC(J:1) =       ">" OR
                                   PIN1-REC(J:2) =       """ "
      *    *** <img src="xxxxxx" alt="xxxxx">
      *    *** altの前がX"C2A0" UTF-8 の　スペース？
                                                             OR
                                   PIN1-REC(J:3) =       X"22C2A0"
                           ADD     1           TO      L
                       END-PERFORM
                       ADD     1           TO      L
                       MOVE    PIN1-REC(I:L) TO    WK-HTML0
                       MOVE    L           TO      WK-HTML0-L
      *    *** "<" の後、">"等の の時
                       PERFORM S110-10     THRU    S110-EX
                       MOVE    WK-PIN1-LEN     TO      I
                  ELSE
                       MOVE    WK-PIN1-LEN     TO      I
                  END-IF
               END-PERFORM
           ELSE
      *    *** 1,1 "<" 以外の時
                  PERFORM S130-10     THRU    S130-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** "<" の後、">"等の の時
       S110-10.

           EVALUATE TRUE
               WHEN PIN1-REC(1:7) =    "<title>"
                   MOVE    "1"         TO      SW-TITLE

               WHEN PIN1-REC(1:8) =    "</title>"
                   MOVE    "0"         TO      SW-TITLE

      *    *** 2019.04 から<spanから<h2の次にタイトル来るようになった
               WHEN WK-HTML0(1:12) =   "<span class=" OR
                    WK-HTML0(1:09) =   "<h2 class"
                   MOVE    "1"         TO      SW-SPAN-CLASS

               WHEN WK-HTML0(1:7)  =   "</span>" OR
                    WK-HTML0(1:5)  =   "</h2>"
                   MOVE    "0"         TO      SW-SPAN-CLASS

               WHEN WK-HTML0(1:8)  =   "<CENTER>" OR "<center>"
                   MOVE    "1"         TO      SW-CENTER

               WHEN WK-HTML0(1:9)  =   "</CENTER>" OR "</center>"
                   MOVE    "0"         TO      SW-CENTER

               WHEN WK-HTML0(1:9) =    "<img src="
                   IF    SW-CENTER =   "1"
                         COMPUTE K = L - 9
      *    *** タイトルＩＭＧ
                         ADD     -3            TO      K
                         MOVE    WK-HTML0(11:K) TO     WK-HTML3
                         MOVE    K             TO      WK-HTML3-L
      *                   DISPLAY "HTML3=" WK-HTML3 (1:70)
                   END-IF

               WHEN WK-HTML0 (1:8) = "<a href="

      *    *** <a href="" ?
                   IF    WK-HTML0(10:1) = """"
                         DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
                         DISPLAY WK-HTML0 (1:10)
                         STOP    RUN
                   END-IF

      *    *** 公式サイト ?
                   COMPUTE K = L - 9
      *             ADD     -3            TO      K
      *    *** サイトアドレス、最後の／まで入れる
                   ADD     -2            TO      K
                   MOVE    WK-HTML0(10:K) TO     WK-HTML2
                   MOVE    K             TO      WK-HTML2-L

               WHEN OTHER 
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.

      *    *** タイトルデータ出力
       S120-10.

           MOVE    SPACE       TO      POT1-REC

           MOVE    12          TO      K
           MOVE    WK-HED1     TO      POT1-REC (1:12)

           ADD     +1          TO      K
           MOVE    WK-HTML1 (1:WK-HTML1-L) TO  POT1-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML2    TO      POT1-REC (K:WK-HTML2-L)

           ADD     WK-HTML2-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML3    TO      POT1-REC (K:WK-HTML3-L)

           ADD     WK-HTML3-L  TO      K

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      WK-HTML1
                                       WK-HTML2
                                       WK-HTML3

           MOVE    1           TO      WK-HTML1-L
                                       WK-HTML2-L
                                       WK-HTML3-L
           .
       S120-EX.
           EXIT.

      *    *** 1,1 "<" 以外の時
       S130-10.

           IF      SW-TITLE    =       "1"

                   MOVE    PIN1-REC(1:4) TO    WK-HED1-YYYY
                   EVALUATE TRUE
      *    *** 冬？（ＵＴＦ－８)
                       WHEN PIN1-REC(5:3) =    X"E586AC"
                            MOVE   PIN1-REC(5:3) TO    WK-HED1-KISETU
                            MOVE   "01"        TO      WK-HED1-MM
      *    *** 春？（ＵＴＦ－８)
                       WHEN PIN1-REC(5:3) =    X"E698A5" 
                            MOVE   PIN1-REC(5:3) TO    WK-HED1-KISETU
                            MOVE   "04"        TO      WK-HED1-MM
      *    *** 夏？（ＵＴＦ－８)
                       WHEN PIN1-REC(5:3) =    X"E5A48F" 
                            MOVE   PIN1-REC(5:3) TO    WK-HED1-KISETU
                            MOVE   "07"        TO      WK-HED1-MM
      *    *** 秋？（ＵＴＦ－８)
                       WHEN PIN1-REC(5:3) =    X"E7A78B"
                            MOVE   PIN1-REC(5:3) TO    WK-HED1-KISETU
                            MOVE   "10"        TO      WK-HED1-MM
                       WHEN OTHER
                            MOVE   SPACE       TO      WK-HED1-KISETU
                            MOVE   SPACE       TO      WK-HED1-MM
                   END-EVALUATE
                   MOVE    "0"         TO      SW-TITLE
           END-IF

           IF      SW-SPAN-CLASS =     "1"
      *    *** 公式サイト無しや対応
                   IF      WK-HTML1 (1:1) NOT = SPACE
                       IF      WK-HTML1 (1:12) =   "animateTimes"
      *    *** アニメ一覧
                            OR WK-HTML1 (8:15) =  
                           X"E382A2E3838BE383A1E4B880E8A6A7"
      *    *** 関連記事
                            OR WK-HTML1 (1:12) =  
                           X"E996A2E980A3E8A898E4BA8B"
      *    *** アニメ一覧
                            OR WK-HTML1 (1:15) =  
                           X"E382A2E3838BE383A1E4B880E8A6A7"
      *    *** 最新記事
                            OR WK-HTML1 (1:12) =  
                           X"E69C80E696B0E8A898E4BA8B"
      *    *** 放送のアニメ
                            OR WK-HTML1 (8:18) =  
                           X"E694BEE98081E381AEE382A2E3838BE383A1"
      *    *** おすすめアニメ
                            OR WK-HTML1 (1:21) =  
                           X"E3818AE38199E38199E38281E382A2E3838BE383A1"
      *    *** 1 
                            OR WK-HTML1 (1:2)  =  "1 "
      *    *** &times;
                            OR WK-HTML1 (1:7)  =  "&times;"
                               CONTINUE
                       ELSE
      *    *** タイトルデータ出力
                           PERFORM S120-10   THRU    S120-EX
                       END-IF
                   END-IF

                   MOVE    SPACE       TO      WK-HTML1  
                                               WK-HTML2
                                               WK-HTML3
                   MOVE    1           TO      WK-HTML1-L
                                               WK-HTML2-L
                                               WK-HTML3-L

      *    *** タイトル名
      *    *** 2017 頃、タイトルにX"09" = TAB 取る
                   IF    PIN1-REC (WK-PIN1-LEN:1) = X"09"
                         ADD     -1          TO      WK-PIN1-LEN
                   END-IF
      *    *** タイトル名
                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO WK-HTML1
                   MOVE    WK-PIN1-LEN     TO      WK-HTML1-L
           ELSE
                   IF  PIN1-REC (1:2) = "TV"  OR
                       PIN1-REC (1:3) = " TV" OR
      *    ***『 か？
                       PIN1-REC (1:3) = X"E3808E" OR
      *    *** 「か?
                       PIN1-REC (1:3) = X"E3808C" OR
      *    *** アニメか？
                       PIN1-REC (1:9) = X"E382A2E3838BE383A1"
      *                 PERFORM TEST BEFORE
                       PERFORM TEST AFTER
                               VARYING J FROM 3 BY 1
                               UNTIL   J        >       WK-PIN1-LEN - 15
      *    *** UTF8 公式サイトか？
                               IF      PIN1-REC(J:15)   =  WK-SITE
                                   AND WK-HTML1 (1:1) NOT = SPACE
      *    *** タイトルデータ出力
                                   PERFORM S120-10   THRU    S120-EX
                               END-IF
                       END-PERFORM
                   ELSE 
      *    *** 公式サイト
                       IF      PIN1-REC(1:15)   =  WK-SITE
                               AND WK-HTML1 (1:1) NOT = SPACE
      *    *** タイトルデータ出力
                               PERFORM S120-10   THRU    S120-EX
                       END-IF

      *    *** 公式HP
                       IF      PIN1-REC(WK-PIN1-LEN - 8:09)  =  WK-SITE2
                           AND WK-HTML1 (1:1) NOT = SPACE
      *    *** タイトルデータ出力
                               PERFORM S120-10   THRU    S120-EX
                       END-IF
                   END-IF
                   
           END-IF
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

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
