      *    *** bookmarks.html DMMデータ抽出
      *    ***
      *    *** JOB TEST10   TEST71
      *    ***        |        |
      *    ***        |--------
      *    ***     TEST72
      *    ***        |
      *    ***     TEST53
      *    ***        |
      *    ***     TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST72.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML 解析データ ＵＴＦ８
      *    *** TEST10.POT1 => TEST72.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 キーワード追加データ
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 女優名追加データ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 検索 NUM,ALPHA追加データ
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
                               STATUS   WK-PIN5-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** DMM データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** IMG アンマッチデータ　次回 IMG HTML 追加してIMG データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN3-F
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(100).

       FD  PIN4-F
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03  FILLER          PIC  X(100).

       FD  PIN5-F
           RECORD VARYING DEPENDING ON WK-PIN5-LEN.
       01  PIN5-REC.
           03  FILLER          PIC  X(100).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST72  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
      *    *** TEST10 で
      *    "dmm.xxx.html" => TEST10.POT1
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST72.PIN2".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST70.PIN3".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST70.PIN4".
      *     03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST70.PIN5".
           03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST110.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST72.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST72.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN5-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN5-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-A            PIC  X(1000) VALUE SPACE.
           03  WK-ID           PIC  X(012) VALUE SPACE.
           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-IMG          PIC  X(500) VALUE SPACE.

           03  WK-A-LEN        BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEARCH.
             05                PIC  X(050) VALUE
             "/analyze=V1ECCVcEUAM_/limit=120/n1=FgRCTw9VBA4GFlB".
             05                PIC  X(050) VALUE
             "VQ1oD/n2=Aw1fVhQKX0FZCEFUVmkKXhUAQF9UXAs_/sort=ran".
             05                PIC  X(005) VALUE
             "king/".

           03  WK-PIN5-ITEM1   PIC  X(100) VALUE SPACE.
           03  WK-PIN5-ITEM2   PIC  X(100) VALUE SPACE.

           03  WK-PIN5-ITEM1-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-ITEM2-LEN BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-ID
                               INDEXED BY TBL01-IDX.
             05  TBL01-ID      PIC  X(012) VALUE HIGH-VALUE.
             05  TBL01-IMG     PIC  X(500) VALUE SPACE.
             05  TBL01-IMG-LEN BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-H3           PIC  X(001) VALUE "N".
           03  SW-DMM          PIC  X(001) VALUE "N".
           03  SW-A            PIC  X(001) VALUE "N".
           03  SW-CID          PIC  X(001) VALUE "N".
           03  SW-SKIP         PIC  X(001) VALUE "Y".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

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
                   ASCENDING KEY TBL01-ID



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** DMM まで読み飛ばし
           PERFORM UNTIL   SW-DMM = "Y"
                        OR WK-PIN1-EOF   =     HIGH-VALUE
                   IF      PIN1-REC (1:3) = "DMM"
                       AND WK-PIN1-LEN =    3

      *    *** WRITE POT1 HEAD
                       PERFORM S040-10     THRU    S040-EX
                       MOVE    "Y"         TO      SW-DMM
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** READ PIN1 </H3>
           PERFORM S020-10     THRU    S020-EX

      *    *** PIN3 => PIN5 に変更した為、READ 止める
      *    *** READ PIN3
      *     PERFORM S050-10     THRU    S050-EX
           MOVE    HIGH-VALUE  TO      WK-PIN3-EOF

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE

      *    *** 検索 キーワード追加データ出力
      *    *** WRITE POT1 HEAD
                   PERFORM S052-10     THRU    S052-EX

      *    *** READ PIN3
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM



      *    *** WRITE POT1 HEAD 2
           PERFORM S042-10     THRU    S042-EX

      *    *** READ PIN4
           PERFORM S060-10     THRU    S060-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE

      *    *** 検索 女優名追加データ出力
      *    *** WRITE POT1 HEAD
                   PERFORM S062-10     THRU    S062-EX

      *    *** READ PIN4
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM



      *    *** ジャパリaduxvi-Search-Num-Alpha WRITE 止める
      *    *** WRITE POT1 HEAD 3
      *     PERFORM S044-10     THRU    S044-EX

      *    *** READ PIN5
           PERFORM S070-10     THRU    S070-EX

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE
      *    *** ジャパリあ まで読み飛ばし
                      OR PIN5-REC (13:3) = X"E38182"

      *    *** READ PIN5
                   PERFORM S070-10     THRU    S070-EX

           END-PERFORM

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE

      *    *** ジャパリ
                   IF      PIN5-REC (1:12) = X"E382B8E383A3E38391E383AA"
                           MOVE    PIN5-REC    TO      POT1-REC
      *    *** WRITE POT1
                           PERFORM S080-10     THRU    S080-EX
                   ELSE

      *    *** 検索 NUM,APLHA追加データ出力
      *    *** WRITE POT1 HEAD
                           PERFORM S072-10     THRU    S072-EX
                   END-IF

      *    *** READ PIN5
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM



           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                        OR PIN1-REC (1:6) =    "aduFC2"

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

      *    *** OPEN 1
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

           OPEN    INPUT       PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F OPEN ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F OPEN ERROR STATUS="
                           WK-PIN5-STATUS
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

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-IMG
           MOVE    ZERO        TO      WK-IMG-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE  COUNT WK-TITLE-LEN
                           WK-IMG    COUNT WK-IMG-LEN
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
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

           MOVE    SPACE       TO      TBL01-ID (TBL01-IDX)
           MOVE    ZERO        TO      I5
           MOVE    "N"         TO      SW-CID

      *    *** cid=値をTBL01-ID (TBL01-IDX)にセットする
           PERFORM VARYING I FROM 20 BY 1
                   UNTIL I > WK-PIN2-LEN
                   IF      PIN2-REC (I:4) = "cid="
                           MOVE    "Y"         TO      SW-CID
                           ADD     4           TO      I
                   END-IF

                   IF      SW-CID      =       "Y"
                       IF      PIN2-REC (I:1) = "/"
                           MOVE    WK-PIN2-LEN TO      I
                       ELSE
                           ADD     1           TO      I5
                           MOVE    PIN2-REC (I:1) TO   
                                   TBL01-ID (TBL01-IDX) (I5:1)
                       END-IF
                   END-IF
           END-PERFORM

           MOVE    WK-IMG      TO      TBL01-IMG (TBL01-IDX)
           MOVE    WK-IMG-LEN  TO      TBL01-IMG-LEN (TBL01-IDX)

           SET     TBL01-IDX   UP  BY  1
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1 HEAD
       S040-10.

           MOVE    "% DMM,"    TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** 下記、WRITE 止める
           IF      SW-SKIP     NOT =   "Y"

                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduDMM-Search" TO  POT1-REC (13:13)

      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
           END-IF
            .
       S040-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 2
       S042-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    "aduDMM-Search-Actress-name"
                               TO      POT1-REC (13:26)

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
            .
       S042-EX.
           EXIT.

      *    *** WRITE POT1 HEAD 3
       S044-10.

           MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
           MOVE    "aduDMM-Search-Num-Alpha"
                               TO      POT1-REC (13:23)
 
      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
            .
       S044-EX.
           EXIT.

      *    *** READ PIN3
       S050-10.

           READ    PIN3-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ

           IF      WK-PIN3-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF
           .
       S050-EX.
           EXIT.

      *    *** 検索キーワード追加データ出力
      *    *** WRITE POT1 HEAD
       S052-10.

           MOVE    PIN3-REC    TO      POT1-REC

           ADD     WK-PIN3-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

           MOVE    "https://www.dmm.co.jp/search/=/searchstr="
                               TO      POT1-REC (J:41)
           ADD     41          TO      J

           MOVE    PIN3-REC (1:WK-PIN3-LEN)  
                               TO      POT1-REC (J:WK-PIN3-LEN)
           ADD     WK-PIN3-LEN TO      J

           MOVE    WK-SEARCH   TO      POT1-REC (J:105)
           ADD     105         TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
           .
       S052-EX.
           EXIT.

      *    *** READ PIN4
       S060-10.

           READ    PIN4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ

           IF      WK-PIN4-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN4-F READ ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF
           .
       S060-EX.
           EXIT.

      *    *** 検索女優名追加データ出力
      *    *** WRITE POT1 HEAD
       S062-10.

           MOVE    PIN4-REC    TO      POT1-REC

           ADD     WK-PIN4-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

           MOVE    "https://www.dmm.co.jp/search/=/searchstr="
                               TO      POT1-REC (J:41)
           ADD     41          TO      J

           MOVE    PIN4-REC (1:WK-PIN4-LEN)  
                               TO      POT1-REC (J:WK-PIN4-LEN)
           ADD     WK-PIN4-LEN TO      J

           MOVE    WK-SEARCH   TO      POT1-REC (J:105)
           ADD     105         TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
           .
       S062-EX.
           EXIT.

      *    *** READ PIN5
       S070-10.

           MOVE    SPACE       TO      WK-PIN5-ITEM1
                                       WK-PIN5-ITEM2
           MOVE    ZERO        TO      WK-PIN5-ITEM1-LEN
                                       WK-PIN5-ITEM2-LEN

           READ    PIN5-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
                   UNSTRING PIN5-REC
                           DELIMITED BY ","
                           INTO
                           WK-PIN5-ITEM1 COUNT WK-PIN5-ITEM1-LEN
                           WK-PIN5-ITEM2 COUNT WK-PIN5-ITEM2-LEN
           END-READ

           IF      WK-PIN5-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN5-F READ ERROR STATUS="
                           WK-PIN5-STATUS
                   STOP    RUN
           END-IF
           .
       S070-EX.
           EXIT.

      *    *** 検索NUM,ALPHA追加データ出力
      *    *** WRITE POT1 HEAD
       S072-10.

      *    MOVE    PIN5-REC    TO      POT1-REC
           MOVE    WK-PIN5-ITEM1 TO    POT1-REC

      *     ADD     WK-PIN5-LEN 1 GIVING J
           ADD     WK-PIN5-ITEM1-LEN 1 GIVING J
      *    *** を検索する
      *     MOVE    X"E38292E6A49CE7B4A2E38199E3828B" TO POT1-REC (J:15)
      *     ADD     15           TO      J

           MOVE    " ,"        TO      POT1-REC (J:2)
           ADD     2           TO      J

           MOVE    "https://www.dmm.co.jp/search/=/searchstr="
                               TO      POT1-REC (J:41)
           ADD     41          TO      J

      *     MOVE    PIN5-REC (1:WK-PIN5-LEN)  
      *                         TO      POT1-REC (J:WK-PIN5-LEN)
      *     ADD     WK-PIN5-LEN TO      J
      *    *** XVI と共通の為、ITEM1(日本語)で検索する
           MOVE    PIN5-REC (1:WK-PIN5-ITEM1-LEN)  
                               TO      POT1-REC (J:WK-PIN5-ITEM1-LEN)
           ADD     WK-PIN5-ITEM1-LEN TO J

           MOVE    WK-SEARCH   TO      POT1-REC (J:105)
           ADD     105         TO      J

           MOVE    " ,OF ,"    TO      POT1-REC (J:6)
           ADD     6           TO      J

      *    *** WRITE POT1
           PERFORM S080-10     THRU    S080-EX
           .
       S072-EX.
           EXIT.

      *    *** WRITE POT1
       S080-10.

           WRITE   POT1-REC

           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S080-EX.
           EXIT.

      *    *** html 解析
       S100-10.

           EVALUATE TRUE

               WHEN PIN1-REC (1:3) = "<H3"
                   MOVE    "Y"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   AND
      *    *** H3 の #aduDMM は残す
                    PIN1-REC (1:10) = "#aduDMM-br"
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      POT1-REC (13:WK-PIN1-LEN)
      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   AND
      *    *** H3 の #XXX はカット
                    PIN1-REC (1:1) = "#"
                   MOVE    "N"         TO      SW-H3

               WHEN SW-H3 = "Y"
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    PIN1-REC (1:WK-PIN1-LEN)
                                       TO      POT1-REC (13:WK-PIN1-LEN)

      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-H3

               WHEN PIN1-REC (1:9) = "<A HREF="""
                   MOVE    "Y"         TO      SW-A
                   MOVE    "N"         TO      SW-CID

      *    *** 動画のアドレスをWK-Aにセット
                   MOVE    SPACE       TO      WK-A
                   MOVE    ZERO        TO      I2
                   PERFORM VARYING I FROM 10 BY 1
                           UNTIL I > WK-PIN1-LEN
                           OR PIN1-REC (I:1) = '"'
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1) TO   WK-A (I2:1)
                           IF      PIN1-REC (I:4) =    "cid="
                               MOVE    "Y"         TO      SW-CID
                           END-IF
                   END-PERFORM
                   MOVE    I2          TO      WK-A-LEN

               WHEN SW-A  = "Y"
                   MOVE    SPACE       TO      POT1-REC
                   MOVE    ZERO        TO      I3
      *    *** 動画のタイトルをPOT1-REC 先頭にセット
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-PIN1-LEN
      *    *** - エロ動画・
                              OR PIN1-REC (I:18) = 
                                 X"202D20E382A8E383ADE58B95E794BBE383BB"
      *    *** - 無料エロ
                              OR PIN1-REC (I:15) = 
                                 X"202D20E784A1E69699E382A8E383AD"
      *    *** - FANZA
                              OR PIN1-REC (I:08) = 
                                 " - FANZA"
      *    ***  - ソフト・
                              OR PIN1-REC (I:15) = 
                                 X"202D20E382BDE38395E38388E383BB"
                       EVALUATE TRUE
                           WHEN PIN1-REC (I:08) = " - FANZA"
                               ADD     08          TO      I
                           WHEN OTHER
                               ADD     1           TO      I3
                               IF      PIN1-REC (I:1) = ","
                                   MOVE    "."    TO    PIN1-REC (I:1)
                               END-IF
                               MOVE    PIN1-REC (I:1) TO POT1-REC (I3:1)
                       END-EVALUATE
                   END-PERFORM
                   
                   ADD     2           TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

      *    *** 動画のアドレスをPOT1-REC ２番目にセット
                   ADD     1           TO      I3
                   MOVE    WK-A (1:WK-A-LEN) TO   POT1-REC (I3:WK-A-LEN)

                   ADD     2 WK-A-LEN  TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   IF      SW-CID      =       "Y"
      *    *** IMG SET
                       PERFORM S110-10     THRU    S110-EX
                   ELSE
                       ADD     2           TO      I3
                       MOVE    ","         TO      POT1-REC (I3:1)
                   END-IF

      *    *** WRITE POT1
                   PERFORM S080-10     THRU    S080-EX
                   MOVE    "N"         TO      SW-A

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S100-EX.
           EXIT.

      *    *** IMG SET
       S110-10.

           MOVE    SPACE       TO      WK-ID
           MOVE    ZERO        TO      I5
           MOVE    "N"         TO      SW-CID

      *    *** cid=値をWK-IDにセットする
           PERFORM VARYING I FROM 20 BY 1
                   UNTIL I > WK-A-LEN
                   IF      WK-A (I:4) = "cid="
                           MOVE    "Y"         TO      SW-CID
                           ADD     4           TO      I
                   END-IF

                   IF      SW-CID      =       "Y"
                       IF      WK-A (I:1) = "/"
                           MOVE    WK-A-LEN    TO      I
                       ELSE
                           ADD     1           TO      I5
                           MOVE    WK-A (I:1)  TO      WK-ID (I5:1)
                       END-IF
                   END-IF
           END-PERFORM

           SEARCH  ALL TBL01-AREA
               AT END
      *    *** IMG ない時、POT2へ出力
                   ADD     2           TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)

                   MOVE    WK-A (1:WK-A-LEN) TO POT2-REC
                   MOVE    " , , ,"    TO      POT2-REC (WK-A-LEN + 1:6)
                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF

               WHEN TBL01-ID (TBL01-IDX)  =  WK-ID

                   ADD     1           TO      I3
                   MOVE    TBL01-IMG-LEN (TBL01-IDX)
                                       TO      I4
                   MOVE    TBL01-IMG (TBL01-IDX) (1:I4)
                                       TO      POT1-REC (I3:I4)

                   ADD     2 I4        TO      I3
                   MOVE    ","         TO      POT1-REC (I3:1)
           END-SEARCH
           .
       S110-EX.
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

           CLOSE   PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F CLOSE ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F CLOSE ERROR STATUS="
                           WK-PIN5-STATUS
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
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 ｹﾝｽｳ = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 ｹﾝｽｳ = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-PIN5-CNT TO      WK-PIN5-CNT-E
           DISPLAY WK-PGM-NAME " PIN5 ｹﾝｽｳ = " WK-PIN5-CNT-E
                   " (" WK-PIN5-F-NAME ")"
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
