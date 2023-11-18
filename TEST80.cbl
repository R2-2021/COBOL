      *    *** お菓子系アイドル　データ抽出
      *    *** 
      *    *** https://okashik.atype.jp/index.html?act=1&cap=%E3%81%82
      *    *** お菓子系アイドル　出演者一覧
      *    *** Pythonでは、あーおでやっても、桁足りない為、
      *    *** CALL "CBL_READ_FILE" 利用
      *    *** 右クリック、ページのソースの表示でhtml表示後、
      *    *** 名前部分のみにカットし、インプットデータ作成

      *    *** TEST80
      *    ***   ↓
      *    *** TEST53
      *    ***   ↓
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST80.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** お菓子系データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST80  ".

      *    *** html だが、通常READだとエラーになる為、SYSTEM READ使う
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "TEST80_okashik_a_wo.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST80.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-HREF         PIC  X(1000) VALUE SPACE.
           03  WK-IMG          PIC  X(1000) VALUE SPACE.

           03  WK-APT          BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE.
             05                PIC  X(002) VALUE "% ".
             05                PIC  X(015) VALUE
      *    *** お菓子系２
               X"E3818AE88F93E5AD90E7B3BBEFBC92".
             05                PIC  X(001) VALUE ",".
           03  WK-READ-ONLY    PIC  9(001) VALUE 1.
           03  WK-WRITE-ONLY   PIC  9(001) VALUE 2.
           03  WK-READ-WRITE   PIC  9(001) VALUE 3.

       01  FILEHANDLE          USAGE IS POINTER.
       01  FILENAME            PIC X(256) VALUE SPACE.
       01  CFILE               PIC X(257) VALUE SPACE.
      *    *** 1:READ-ONLY
       01  ACCESS-MODE         USAGE BINARY-LONG VALUE 1.
       01  FILE-LOCK           PIC X.
       01  DEVICE              PIC X.
       01  RESULT              USAGE BINARY-LONG.

       01  FILE-OFFSET         PIC 9(18) COMP.
       01  FILE-OFFSET2        PIC 9(18) COMP VALUE ZERO.
       01  READ-LENGTH         PIC 9(8)  COMP.
       01  FILE-FLAGS          BINARY-CHAR.
      *    *** MAX= 2147483647
       01  READ-BUFFER         PIC X(2000000).
      * 01  READ-BUFFER          PIC X(2147483647).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  L5              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-NAME         PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 編集
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
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

           MOVE    WK-PIN1-F-NAME TO   FILENAME
           STRING  FILENAME  DELIMITED BY SPACE
                   LOW-VALUE DELIMITED BY SIZE
                   INTO CFILE
           END-STRING

           CALL    "CBL_OPEN_FILE"
                               USING   CFILE
                                       ACCESS-MODE
                                       FILE-LOCK
                                       DEVICE
                                       FILEHANDLE
                   RETURNING           RESULT
           END-CALL

           DISPLAY WK-PGM-NAME
                   " OPEN-RESULT=" RESULT

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           WRITE   POT1-REC    FROM    WK-TITLE

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
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

           MOVE    ZERO        TO      FILE-OFFSET
           MOVE    ZERO        TO      READ-LENGTH
           MOVE    128         TO      FILE-FLAGS

           CALL    "CBL_READ_FILE"
                               USING   FILEHANDLE
                                       FILE-OFFSET
                                       READ-LENGTH
                                       FILE-FLAGS
                                       READ-BUFFER
                   RETURNING           RESULT
           END-CALL

           DISPLAY WK-PGM-NAME
                   " READ-RESULT=" RESULT 
                   " FILE-OFFSET=" FILE-OFFSET
           MOVE    FILE-OFFSET TO      WK-PIN1-LEN

           MOVE    ZERO        TO      FILE-OFFSET
           MOVE    WK-PIN1-LEN TO      READ-LENGTH
           MOVE    ZERO        TO      FILE-FLAGS

           CALL    "CBL_READ_FILE" 
                               USING   FILEHANDLE
                                       FILE-OFFSET
                                       READ-LENGTH
                                       FILE-FLAGS
                                       READ-BUFFER
                   RETURNING           RESULT
           END-CALL

           IF      RESULT      =       ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** 編集
       S100-10.

      *         inspect marquee replacing all x"0d0a" by "  "
      *         inspect marquee replacing all x"0a" by space


           MOVE    SPACE       TO      WK-REC
                                       WK-HREF
                                       WK-IMG

           MOVE    1           TO      L
                                       L2
                                       L3
                                       L4
                                       L5

           MOVE    ZERO        TO      WK-APT
           MOVE    "N"         TO      SW-NAME

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-PIN1-LEN

      *             IF      READ-BUFFER (I:1) = ";"
                   IF      READ-BUFFER (I:1) = ">"
                       MOVE    READ-BUFFER (I:1) TO WK-REC (L:1)
                       ADD     1           TO      L
                       IF      READ-BUFFER (I + 4:1) = "<"
                           AND READ-BUFFER (I + 1:1) NOT NUMERIC
      *    *** ジャパリ あ-ん
      *                     MOVE    X"E382B8E383A3E38391E383AA" 
      *                                         TO       POT1-REC
                           MOVE    READ-BUFFER (I + 1:3) 
                                               TO       POT1-REC (1:)
                           WRITE   POT1-REC
      *                     ADD     1           TO       L

                           IF      WK-POT1-STATUS =    ZERO
                               ADD     1           TO      WK-POT1-CNT
                           ELSE
                               DISPLAY WK-PGM-NAME 
                                       " POT1-F WRITE ERROR STATUS="
                                       WK-POT1-STATUS
                               STOP    RUN
                           END-IF
                       END-IF

                       MOVE    SPACE       TO      WK-REC
                       MOVE    1           TO      L

      *                 PERFORM S110-10     THRU     S110-EX
      *                 WRITE   POT1-REC        
      *                 ADD     1           TO      WK-POT1-CNT
                  ELSE
                       EVALUATE TRUE
      *    *** L3 = NAMEのバイト数
                           WHEN READ-BUFFER (I:1) = "("
                               MOVE    "Y"         TO      SW-NAME
                               ADD     L -1        GIVING  L3
      *    *** （
      *    *** L2 = HREFのバイト数
                           WHEN WK-REC (1:7) = "<a href"
                            AND READ-BUFFER (I:2) = '">'
                               ADD     L -10       GIVING  L2
                               MOVE    WK-REC (10:L2) TO   WK-HREF
      *    *** （
      *    *** L4 = IMGのバイト数
                           WHEN WK-REC (1:4) = "<img"
                            AND READ-BUFFER (I:1) = '"'
                               ADD     1           TO      WK-APT
                               IF      WK-APT      =       2
                                   ADD     L -11       GIVING   L4
                                   MOVE    "https://okashik.atype.jp"
                                                   TO      WK-IMG
                                   IF  WK-REC (11:33) =
                                    "/images//https://okashik.atype.jp"
      *    *** IMG ERROR
                                       DISPLAY "IMG-ERROR HTTPS"
                                       DISPLAY "WK-HREF=" WK-HREF (1:60)
                                       DISPLAY "WK-REC =" WK-REC (1:60)
                                       ADD     -33        TO   L4
                                       MOVE    WK-REC (11 + 33:L4)
                                                   TO     WK-IMG (25:L4)
                                   ELSE
                                       MOVE    WK-REC (11:L4)
                                                   TO     WK-IMG (25:L4)
                                   END-IF
                               END-IF
                       END-EVALUATE

                       IF      SW-NAME     =       "Y"
      *    *** WRITE POT1-REC
                           PERFORM S110-10     THRU     S110-EX

      *     WRITE   POT1-REC    FROM    WK-HREF
      *     ADD     1           TO      WK-POT1-CNT

      *     WRITE   POT1-REC    FROM    WK-IMG
      *     ADD     1           TO      WK-POT1-CNT

      *     WRITE   POT1-REC    FROM    WK-REC
      *     ADD     1           TO      WK-POT1-CNT

                       ELSE
                           MOVE    READ-BUFFER (I:1) TO WK-REC (L:1)
                           ADD     1           TO       L
                       END-IF
                   END-IF
           END-PERFORM

      *     IF      L           >        1
      *    *** 
      *             PERFORM S110-10     THRU     S110-EX
      *             WRITE   POT1-REC        
      *             ADD     1           TO      WK-POT1-CNT
      *     END-IF

           MOVE     HIGH-VALUE  TO      WK-PIN1-EOF
      *     DISPLAY WK-POT1-CNT

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "M"         TO      WK-TYPE
      *     MOVE    "PIN1"      TO      WK-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC (1:100)

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1-REC
       S110-10.

           MOVE    WK-REC (1:L3) TO    POT1-REC (1:)
           ADD     L3 1        GIVING  L5

           MOVE    " ,"        TO      POT1-REC (L5:2)
           ADD     2           TO      L5

           MOVE    WK-HREF (1:L2) TO   POT1-REC (L5:L2)
           ADD     L2          TO      L5

           MOVE    " ,"        TO      POT1-REC (L5:2)
           ADD     2           TO      L5

      *    *** 24 = "https://okashik.atype.jp"
           MOVE    WK-IMG (1:L4 + 24) TO POT1-REC (L5:L4 + 24)
           ADD     L4 24       TO      L5

           MOVE    " ,"        TO      POT1-REC (L5:2)
           ADD     2           TO      L5

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    SPACE       TO      WK-REC
                                       WK-HREF
                                       WK-IMG
           MOVE    1           TO      L
                                       L2
                                       L3
                                       L4
                                       L5
           MOVE    ZERO        TO      WK-APT
           MOVE    "N"         TO      SW-NAME
           .
       S110-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CALL    "CBL_CLOSE_FILE" 
                               USING   FILEHANDLE
                   RETURNING           RESULT.

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
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
