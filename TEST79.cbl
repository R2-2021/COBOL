      *    *** お菓子系アイドル　データ抽出
      *    *** 
      *    *** https://okashik.atype.jp/index.html?act=1&cap=%E3%81%82
      *    *** お菓子系アイドル　出演者一覧
      *    *** Pythonでは、あーおでやっても、”あ”しか抽出出来ない為、
      *    *** 右クリック、ページのソースの表示でhtml表示後、TEST10 実行

      *    *** TEST10
      *    ***   ↓
      *    *** TEST79
      *    ***   ↓
      *    *** TEST53
      *    ***   ↓
      *    *** TEST54
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST79.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** お菓子系データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST80.POT1 https:,img お菓子系データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** お菓子系データ ｉｍｇセット後
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST54用 IMGデータ作成
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST79  ".

      *    *** TEST80.POT1 => TEST79_okashik_idol.PIN1 コピー
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
               "TEST79_okashik_idol.PIN1".

           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST80.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST79.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST79.POT2".

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

           03  WK-NAME         PIC  X(100) VALUE SPACE.
           03  WK-HTTPS        PIC  X(200) VALUE SPACE.
           03  WK-IMG          PIC  X(200) VALUE SPACE.
           03  WK-NAME-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.

           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL01-NAME
                               INDEXED BY TBL01-IDX.
             05  TBL01-NAME    PIC  X(100) VALUE HIGH-VALUE.
             05  TBL01-HTTPS   PIC  X(200) VALUE HIGH-VALUE.
             05  TBL01-IMG     PIC  X(200) VALUE HIGH-VALUE.
             05  TBL01-NAME-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HTTPS-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-IMG-LEN  BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.
      *
       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX



      *    *** READ PIN2
      *    *** からのセットやめる
      *     PERFORM S030-10     THRU    S030-EX

      *     PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** PIN2 TBL SET
      *             PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
      *             PERFORM S030-10     THRU    S030-EX
      *     END-PERFORM



      *    *** TBL01 SORT
      *     SORT    TBL01-AREA
      *             ASCENDING KEY TBL01-NAME



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      PIN1-REC (1:1) =    "%"
      *                  OR PIN1-REC (1:24) =
      *    *** ジャパリアイドル
      *             X"E382B8E383A3E38391E383AAE382A2E382A4E38389E383AB"
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                       IF      WK-PIN1-LEN =   3
      *    *** ジャパリ
                           MOVE    X"E382B8E383A3E38391E383AA" TO
                                   POT1-REC
                           MOVE    PIN1-REC (1:3) TO   POT1-REC (13:3)
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                       ELSE
      *    *** WRITE POT1
      *                     PERFORM S100-10     THRU    S100-EX
                           WRITE   POT1-REC    FROM    PIN1-REC
                           ADD     1           TO      WK-POT1-CNT
      *    *** WRITE POT2
                           PERFORM S120-10     THRU    S120-EX
                       END-IF
                   END-IF
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
                               PIN2-F
                   OUTPUT      POT1-F
                               POT2-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           SET     TBL01-IDX   TO      1

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
          .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-NAME
                                       WK-HTTPS
                                       WK-IMG
           MOVE    ZERO        TO      WK-NAME-LEN
                                       WK-HTTPS-LEN
                                       WK-IMG-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN2-CNT

                   UNSTRING PIN2-REC DELIMITED BY ","
                       INTO
                           WK-NAME  COUNT WK-NAME-LEN
                           WK-HTTPS COUNT WK-HTTPS-LEN
                           WK-IMG   COUNT WK-IMG-LEN
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           IF      TBL01-IDX   >       2000
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER TBL01-IDX=" TBL01-IDX
                   STOP    RUN
           END-IF

           MOVE    WK-NAME     TO      TBL01-NAME     (TBL01-IDX)
           MOVE    WK-NAME-LEN TO      TBL01-NAME-LEN (TBL01-IDX)

           MOVE    WK-HTTPS    TO      TBL01-HTTPS     (TBL01-IDX)
           MOVE    WK-HTTPS-LEN TO     TBL01-HTTPS-LEN (TBL01-IDX)

           MOVE    WK-IMG      TO      TBL01-IMG     (TBL01-IDX)
           MOVE    WK-IMG-LEN  TO      TBL01-IMG-LEN (TBL01-IDX)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      WK-NAME
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
                      OR PIN1-REC (I:1) = ","
                   MOVE    PIN1-REC (I:1) TO   WK-NAME (I:1)
                   MOVE    I           TO      L
           END-PERFORM

      *    *** SEARCH  ALL TBL01-AREA
      *     SET     TBL01-IDX    TO     1
           SEARCH  ALL TBL01-AREA
               AT END
                   MOVE    "N"         TO      SW-SEARCH

               WHEN TBL01-NAME (TBL01-IDX) (1:L) =  WK-NAME (1:L)
                   MOVE    "Y"         TO      SW-SEARCH
           END-SEARCH

           IF      SW-SEARCH    =       "Y"
      *    *** WRITE POT1 編集
                   PERFORM S110-10     THRU    S110-EX
           ELSE
                   MOVE    WK-NAME     TO      POT1-REC
                   MOVE    " , , ,"    TO      POT1-REC (I:6)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 編集
       S110-10.

           MOVE   WK-NAME      TO      POT1-REC
           ADD    L 1          GIVING  P

           MOVE   " ,"         TO      POT1-REC (P:2)
           ADD    2            TO      P

           MOVE   TBL01-HTTPS-LEN (TBL01-IDX)
                               TO      L
           MOVE   TBL01-HTTPS (TBL01-IDX) (1:L) 
                               TO      POT1-REC (P:L)
           ADD    L            TO      P

           MOVE   " ,"         TO      POT1-REC (P:2)
           ADD    2            TO      P

           MOVE   TBL01-IMG-LEN (TBL01-IDX)
                               TO      L
           MOVE   TBL01-IMG (TBL01-IDX) (1:L) 
                               TO      POT1-REC (P:L)
           ADD    L            TO      P

           MOVE   " ,"         TO      POT1-REC (P:2)
           ADD    2            TO      P

           WRITE  POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

       S120-10.

           MOVE    SPACE       TO      WK-NAME
                                       WK-HTTPS
                                       WK-IMG
           MOVE    ZERO        TO      WK-NAME-LEN
                                       WK-HTTPS-LEN
                                       WK-IMG-LEN

           UNSTRING PIN1-REC
                   DELIMITED BY ","
                   INTO
                   WK-NAME  COUNT WK-NAME-LEN
                   WK-HTTPS COUNT WK-HTTPS-LEN
                   WK-IMG   COUNT WK-IMG-LEN

           MOVE    1           TO      P
           MOVE    WK-NAME (1:WK-NAME-LEN - 1) TO POT2-REC
           ADD     WK-NAME-LEN -1 TO      P

           MOVE    ","         TO      POT2-REC (P:1)
           ADD     1           TO      P

           MOVE    WK-IMG (1:WK-IMG-LEN) TO POT2-REC (P:WK-IMG-LEN)
           ADD     WK-IMG-LEN  TO      P

           MOVE    ","         TO      POT2-REC (P:1)
           ADD     1           TO      P

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S120-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   POT1-F
                   POT2-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

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
