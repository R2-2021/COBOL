      *    *** Youtube html ゆいかおり 解析
      *    *** htmlの構成が変わったため、解析出来なくなった
      *    *** TEST69.cbl で再解析

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST66.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ ＵＴＦ８
      *    *** TEST10.POT1 => TEST66.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE WATCH データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** YOUTUBE IMG データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST66  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST66.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.石原夏織.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST66.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST66.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** <title>
           03  WK-TITLE        PIC  X(1024) VALUE SPACE.
      *    *** 退避エリア
           03  WK-HTML0        PIC  X(1024) VALUE SPACE.
      *    *** タイトル名
           03  WK-HTML1        PIC  X(1024) VALUE SPACE.
      *    *** channel,watch サイト
           03  WK-HTML2        PIC  X(1024) VALUE SPACE.
      *    *** img= src=XXX",data-thumb="XXX" XXX
           03  WK-HTML3        PIC  X(1024) VALUE SPACE.

           03  WK-TITLE-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML3-L      BINARY-LONG SYNC VALUE ZERO.

      *    *** 公式サイト UTF-8
           03  WK-SITE.
             05  FILLER        PIC  X(003) VALUE X"E585AC".
             05  FILLER        PIC  X(003) VALUE X"E5BC8F".
             05  FILLER        PIC  X(003) VALUE X"E382B5".
             05  FILLER        PIC  X(003) VALUE X"E382A4".
             05  FILLER        PIC  X(003) VALUE X"E38388".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
           03  SW-TITLE        PIC  X(001) VALUE "N".
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL   WK-PIN1-EOF = HIGH-VALUE

      *    *** html 解析
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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
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

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** html 解析
       S100-10.

           IF      PIN1-REC(1:1) =     "<"
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   IF      PIN1-REC(I:1) =     "<" 
                       MOVE    +1      TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL PIN1-REC(J:1) = ">"
                              OR PIN1-REC(J:2) = """ "
                           ADD     1           TO      L
                       END-PERFORM

      *    *** > は >を含む長さセット
                       IF      PIN1-REC(J:1) = ">"
                           ADD     1           TO      L
                       ELSE
      *    *** ="" " の位置は、最初の"の位置なので、－１する
                           ADD     -1          TO      L
                       END-IF
                       MOVE    PIN1-REC(I:L) TO    WK-HTML0
                       MOVE    L           TO      WK-HTML0-L

      *    *** 1,1 = "<" の時、
                       PERFORM S110-10     THRU    S110-EX
                       MOVE    WK-PIN1-LEN TO      I
                  ELSE
                       MOVE    WK-PIN1-LEN TO      I
                  END-IF
               END-PERFORM
           ELSE

      *    *** 1,1 "<" 以外の時
                  PERFORM S130-10     THRU    S130-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** 1,1 = "<" の時、
       S110-10.

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     MOVE    "HTML0"     TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-HTML0 (1:40)

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "S110-10"   TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC (1:40)

           EVALUATE TRUE

               WHEN WK-HTML0 (1:07) = "<title>"
                   MOVE    "Y"         TO      SW-TITLE

               WHEN WK-HTML0 (1:08) = "</title>"
                   MOVE    "N"         TO      SW-TITLE

      *    *** 2019.04 から<spanから<h2の次にタイトル来るようになった
               WHEN WK-HTML0 (1:04) = "<h3 "
                   MOVE    "Y"         TO      SW-H3

               WHEN WK-HTML0 (1:05) =  "</h3>"
                   MOVE    "N"         TO      SW-H3

               WHEN WK-HTML0 (1:05) = "<img "
      *    *** img CHECK
      *    *** scr="XXX",data-thumb="XXX" XXX 取り出し
                   PERFORM S140-10     THRU    S140-EX
                   WRITE   POT2-REC    FROM    WK-HTML3
                   ADD     1           TO      WK-POT2-CNT

      *             MOVE    SPACE       TO      WK-HTML3
      *             MOVE    ZERO        TO      WK-HTML3-L

               WHEN WK-HTML0 (1:08) = "<a href="
      *    *** <a href="" ?
                   IF  WK-HTML0 (10:01) = """"
                       DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
                       DISPLAY WK-HTML0 (1:10)
                       STOP    RUN
                   END-IF

      *    *** /channel,/watch?
      *    *** <a href="=9 を引いて、"XXX",
      *    *** XXXの長さを求める
      *    *** 
                   COMPUTE K = L - 9
                   MOVE    WK-HTML0 (10:K) TO  WK-HTML2
                   MOVE    K           TO      WK-HTML2-L

               WHEN WK-HTML0(1:04) = "</a>"
                   IF      SW-H3       =     "Y"
      *    *** watch DATA WRITE
                       PERFORM S120-10   THRU    S120-EX
                       MOVE    "N"       TO      SW-H3
                   END-IF
               WHEN OTHER 
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.
      *
      *    *** watch データ出力
       S120-10.

           MOVE    SPACE       TO      POT1-REC

           IF      SW-FIRST    =       "Y"
                   MOVE    "%"         TO      POT1-REC (1:1)
                   MOVE    WK-TITLE    TO      POT1-REC (2:WK-TITLE-L)
                   MOVE    "N"         TO      SW-FIRST

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    "#01 Watch List" TO POT1-REC (1:14)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC

           MOVE    1           TO      K
           MOVE    WK-HTML1 (1:WK-HTML1-L)
                               TO      POT1-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    "https://www.youtube.com"
                               TO      POT1-REC (K:23)

           ADD     +23         TO      K
           MOVE    WK-HTML2    TO      POT1-REC (K:WK-HTML2-L)

           ADD     WK-HTML2-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML3 (1:WK-HTML3-L)
                               TO      POT1-REC (K:WK-HTML3-L)

           ADD     WK-HTML3-L  TO      K

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      WK-HTML1
                                       WK-HTML2
           MOVE    1           TO      WK-HTML1-L
                                       WK-HTML2-L
           .
       S120-EX.
           EXIT.

      *    *** 1,1 "<" 以外の時
       S130-10.

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    "S130-10"   TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC (1:40)

           IF      SW-TITLE    =       "Y"

                   MOVE    PIN1-REC    TO      WK-TITLE
                   MOVE    WK-PIN1-LEN TO      WK-TITLE-L
           END-IF

           IF      SW-H3       =       "Y"
      *    *** タイトル名
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      WK-HTML1
                   MOVE    WK-PIN1-LEN TO      WK-HTML1-L
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** img CHECK
       S140-10.

           PERFORM VARYING I FROM 5 BY 1
                   UNTIL I > WK-PIN1-LEN

               EVALUATE TRUE

                   WHEN PIN1-REC(I:12) = "data-thumb=""" 
                       MOVE    1       TO      L
                       ADD     12      TO      I
                       PERFORM VARYING J FROM I BY 1
                           UNTIL PIN1-REC(J:2) = """ "
                           ADD     1           TO      L
                       END-PERFORM
      *    *** " を除く
                       ADD     -1          TO      L
                       MOVE    PIN1-REC(I:L) TO    WK-HTML3
                       MOVE    L           TO      WK-HTML3-L
                       MOVE    WK-PIN1-LEN TO      I

                   WHEN PIN1-REC(I:05) = "src=""" 
                       MOVE    1           TO      L
                       ADD     5           TO      I
                       PERFORM VARYING J FROM I BY 1
                           UNTIL PIN1-REC(J:2) = """ "
                           ADD     1           TO      L
                       END-PERFORM
      *    *** " を除く
                       ADD     -1          TO      L
                       MOVE    PIN1-REC(I:L) TO    WK-HTML3
                       MOVE    L           TO      WK-HTML3-L
                       MOVE    WK-PIN1-LEN TO      I

                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
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
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
