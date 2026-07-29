      *    *** YouTube 検索 サムネイル(TEST69.POT)再編集
      *    *** このプログラムで WATCHデータ再編集
      *    *** 実行順 TEST141 で全て実行 (Python=>TEST69=>TEST142=>TEST68)
      *    *** 
      *    *** 
      *    *** YouTube Top List
      *    *** YouTube Mix List
      *    *** YouTube Play List
      *    *** YouTube Short List
      *    *** YouTube Watch List
      *    *** YouTube Search List
      *    *** 
      *    *** 個別に実行するか
      *    *** 
      *    *** Python
      *    ***   ↓
      *    *** TEST69 youtube.石原夏織.html
      *    ***   ↓
      *    *** TEST142
      *    ***   ↓
      *    *** TEST68 石原夏織
      *    *** 
      *    *** 
      *    *** 
      *    *** TEST141 で全部実行する
      *    *** 
      *    *** TEST141 "https://www.youtube.com/results?search_query=%%E5%%B0%%8F%%E5%%80%%89%%E5%%94%%AF" 小倉唯


       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST142.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** python 実行ファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** python 変更後、実行ファイル
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST142 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST69.POT3".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST142.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ITEM01       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM02       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM03       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM04       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM05       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM06       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM07       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM08       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM09       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM10       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM11       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM12       PIC  X(1000) VALUE SPACE.

           03  WK-ITEM05X      PIC  X(1000) VALUE SPACE.
           03  WK-TIME         PIC  X(010) VALUE SPACE.
           03  WK-HTTPS        PIC  X(200) VALUE SPACE.
           03  WK-NUM-E        PIC  ZZZZZ9 VALUE ZERO.
           03  WK-ITEM-1       PIC  X(1000) VALUE SPACE.
           03  WK-ITEM-2       PIC  X(1000) VALUE SPACE.

           03  WK-ITEM01-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM02-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM03-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM04-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM05-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM06-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM07-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM08-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM09-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM10-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM11-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM12-LEN   BINARY-LONG SYNC VALUE ZERO.

           03  WK-ITEM05X-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-TIME-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  SAVE-AREA.
           03  SV-XX           BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-HIT1         PIC  X(001) VALUE "N".
           03  SW-HIT2         PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 300.
             05  TBL01-ITEM01  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM02  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM03  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM04  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM05  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM06  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM07  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM08  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM09  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM10  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM11  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM12  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM13  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM14  PIC  X(1000) VALUE SPACE.
             05  TBL01-ITEM15  PIC  X(1000) VALUE SPACE.

             05  TBL01-ITEM05X PIC  X(1000) VALUE SPACE.
             05  TBL01-TIME    PIC  X(010) VALUE SPACE.

             05  TBL01-ITEM01-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM02-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM03-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM04-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM05-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM06-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM07-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM08-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM09-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM10-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM11-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM12-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM13-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM14-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ITEM15-LEN BINARY-LONG SYNC VALUE ZERO.

             05  TBL01-ITEM05X-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-TIME-LEN   BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** YouTube Watch List 直前まで、そのまま出力
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                    OR ( PIN1-REC (1:1) = "#"
                     AND PIN1-REC (5:18) = "YouTube Watch List" )

      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** YouTube Watch List は、そのまま出力
           IF    ( PIN1-REC (1:1) = "#"
               AND PIN1-REC (5:18) = "YouTube Watch List" )

      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-IF

      *    *** 次の#までWatch データ編集
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                     OR  PIN1-REC (1:1) = "#"

      *    *** PIN1 UNSTRING
                   PERFORM S110-10     THRU    S110-EX

      *    *** TBL01 SET
                   PERFORM S120-10     THRU    S120-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** WK-ITEM CHECK
           PERFORM S130-10     THRU    S130-EX

      *    *** TBL01 => WRITE POT1
           PERFORM S140-10     THRU    S140-EX

      *    *** AT END まで、そのまま出力
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

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
               NOT  AT  END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           WRITE   POT1-REC    FROM    PIN1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** PIN1-REC UNSTRING
       S110-10.

           MOVE     SPACE      TO      WK-ITEM01
                                       WK-ITEM02
                                       WK-ITEM03
                                       WK-ITEM04
                                       WK-ITEM05
                                       WK-ITEM06
                                       WK-ITEM07
                                       WK-ITEM08
                                       WK-ITEM09
                                       WK-ITEM10
                                       WK-ITEM11
                                       WK-ITEM12

           MOVE     ZERO       TO      WK-ITEM01-LEN
                                       WK-ITEM02-LEN
                                       WK-ITEM03-LEN
                                       WK-ITEM04-LEN
                                       WK-ITEM05-LEN
                                       WK-ITEM06-LEN
                                       WK-ITEM07-LEN
                                       WK-ITEM08-LEN
                                       WK-ITEM09-LEN
                                       WK-ITEM10-LEN
                                       WK-ITEM11-LEN
                                       WK-ITEM12-LEN
                                       
      *    *** 区切り" ," にする
           UNSTRING PIN1-REC
                    DELIMITED BY " ,"
                    INTO
                    WK-ITEM01 COUNT WK-ITEM01-LEN
                    WK-ITEM02 COUNT WK-ITEM02-LEN
                    WK-ITEM03 COUNT WK-ITEM03-LEN
                    WK-ITEM04 COUNT WK-ITEM04-LEN
                    WK-ITEM05 COUNT WK-ITEM05-LEN
                    WK-ITEM06 COUNT WK-ITEM06-LEN
                    WK-ITEM07 COUNT WK-ITEM07-LEN
                    WK-ITEM08 COUNT WK-ITEM08-LEN
                    WK-ITEM09 COUNT WK-ITEM09-LEN
                    WK-ITEM10 COUNT WK-ITEM10-LEN
                    WK-ITEM11 COUNT WK-ITEM11-LEN
                    WK-ITEM12 COUNT WK-ITEM12-LEN
           END-UNSTRING

           IF      WK-ITEM01-LEN > 1000
                OR WK-ITEM02-LEN > 1000
                OR WK-ITEM03-LEN > 1000
                OR WK-ITEM04-LEN > 1000
                OR WK-ITEM05-LEN > 1000
                OR WK-ITEM06-LEN > 1000
                OR WK-ITEM07-LEN > 1000
                OR WK-ITEM08-LEN > 1000
                OR WK-ITEM09-LEN > 1000
                OR WK-ITEM10-LEN > 1000
                OR WK-ITEM11-LEN > 1000
                OR WK-ITEM12-LEN > 1000

                   DISPLAY WK-PGM-NAME " WK-ITEMXX-LEN OVER"
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME "WK-ITEM01-LEN=" WK-ITEM01-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM02-LEN=" WK-ITEM02-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM03-LEN=" WK-ITEM03-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM04-LEN=" WK-ITEM04-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM05-LEN=" WK-ITEM05-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM06-LEN=" WK-ITEM06-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM07-LEN=" WK-ITEM07-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM08-LEN=" WK-ITEM08-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM09-LEN=" WK-ITEM09-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM10-LEN=" WK-ITEM10-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM11-LEN=" WK-ITEM11-LEN
                   DISPLAY WK-PGM-NAME "WK-ITEM12-LEN=" WK-ITEM12-LEN
                   STOP RUN
           END-IF

           MOVE     SPACE      TO      WK-ITEM05X
           MOVE     ZERO       TO      WK-ITEM05X-LEN

           UNSTRING WK-ITEM05
                    DELIMITED BY "&list" OR "     " OR "&pp"
                    INTO
                    WK-ITEM05X COUNT WK-ITEM05X-LEN
           END-UNSTRING

           IF       WK-ITEM05X-LEN > 2000
                 OR WK-ITEM05X-LEN = ZERO
                    DISPLAY WK-PGM-NAME " WK-ITEM05X-LEN OVER"
                            " OR ZERO WK-PIN1-CNT=" WK-PIN1-CNT
                            " WK-ITEM05X-LEN=" WK-ITEM05X-LEN
           END-IF

           MOVE     SPACE      TO      WK-HTTPS
                                       WK-TIME

           MOVE     ZERO       TO      WK-HTTPS-LEN
                                       WK-TIME-LEN

           UNSTRING WK-ITEM05
                    DELIMITED BY "&t=" OR "     "
                    INTO
                    WK-HTTPS COUNT WK-HTTPS-LEN
                    WK-TIME  COUNT WK-TIME-LEN
           END-UNSTRING
           .
       S110-EX.
           EXIT.

      *    *** TBL01 SET
       S120-10.

           MOVE    "N"         TO      SW-SET
      *    *** TIME 有りは、WATCH 別に登録する
           IF      WK-TIME-LEN NOT =   ZERO
                   CONTINUE
           ELSE
               PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I-MAX
                      OR SW-SET = "Y"

                       MOVE    TBL01-ITEM05X-LEN (I2) TO L
                       IF      TBL01-ITEM05X (I2) (1:L)
                             = WK-ITEM05X (1:L)
      *    *** WATCH 有り、TBL01 SET
                           PERFORM S122-10     THRU    S122-EX
                           MOVE    "Y"         TO      SW-SET
                       END-IF
               END-PERFORM
           END-IF

           IF      SW-SET      =       "Y"
                   CONTINUE
           ELSE

                   ADD     1           TO      I
                   IF      I           >       300
                           DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                           STOP    RUN
                   END-IF
      *    *** WATCH 無し、TBL01 SET
                   PERFORM S124-10     THRU    S124-EX

                   MOVE    I           TO      I-MAX
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WATCH 有り、TBL01 SET
       S122-10.

      *    *** 13,14,15 予備

           IF      WK-ITEM01 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF  TBL01-ITEM01 (I2) = SPACE
                   MOVE    WK-ITEM01   TO      TBL01-ITEM01     (I2)
                   MOVE    WK-ITEM01-LEN TO    TBL01-ITEM01-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM01 = TBL01-ITEM01 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM01   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM01-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF


           IF      WK-ITEM02 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM02 (I2) = SPACE
                   MOVE    WK-ITEM02   TO      TBL01-ITEM02     (I2)
                   MOVE    WK-ITEM02-LEN TO    TBL01-ITEM02-LEN (I2)
               ELSE
                   IF      WK-ITEM02-LEN >     TBL01-ITEM02-LEN (I2)
                           MOVE    WK-ITEM02   TO      TBL01-ITEM02 (I2)
                           MOVE    WK-ITEM02-LEN TO   
                                   TBL01-ITEM02-LEN (I2)
                   END-IF
               END-IF
           END-IF

           IF      WK-ITEM03 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM03 (I2) = SPACE
                   MOVE    WK-ITEM03   TO      TBL01-ITEM03     (I2)
                   MOVE    WK-ITEM03-LEN TO    TBL01-ITEM03-LEN (I2)
               ELSE
                   IF      WK-ITEM03-LEN >     TBL01-ITEM03-LEN (I2)
                           MOVE    WK-ITEM03   TO      TBL01-ITEM03 (I2)
                           MOVE    WK-ITEM03-LEN TO   
                                   TBL01-ITEM03-LEN (I2)
                   END-IF
               END-IF
           END-IF

           IF      WK-ITEM04 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM04 (I2) = SPACE
                   MOVE    WK-ITEM04   TO      TBL01-ITEM04     (I2)
                   MOVE    WK-ITEM04-LEN TO    TBL01-ITEM04-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM04 = TBL01-ITEM04 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM04   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM04-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF

      *    *** ITEM05 はセットしない
      *     IF      TBL01-ITEM05 (I2) = SPACE
      *             MOVE    WK-ITEM05   TO      TBL01-ITEM05     (I2)
      *             MOVE    WK-ITEM05-LEN TO    TBL01-ITEM05-LEN (I2)
      *     ELSE
      *             IF      WK-ITEM05-LEN >     TBL01-ITEM05-LEN (I2)
      *                     MOVE    WK-ITEM05   TO      TBL01-ITEM05 (I2)
      *                     MOVE    WK-ITEM05-LEN TO   
      *                             TBL01-ITEM05-LEN (I2)
      *             END-IF
      *     END-IF

           IF      WK-ITEM06 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM06 (I2) = SPACE
                   MOVE    WK-ITEM06   TO      TBL01-ITEM06     (I2)
                   MOVE    WK-ITEM06-LEN TO    TBL01-ITEM06-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM06 = TBL01-ITEM06 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM06   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM06-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF

           IF      WK-ITEM07 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM07 (I2) = SPACE
                   MOVE    WK-ITEM07   TO      TBL01-ITEM07     (I2)
                   MOVE    WK-ITEM07-LEN TO    TBL01-ITEM07-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM07 = TBL01-ITEM07 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM07   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM07-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF


           IF      WK-ITEM08 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM08 (I2) = SPACE
                   MOVE    WK-ITEM08   TO      TBL01-ITEM08     (I2)
                   MOVE    WK-ITEM08-LEN TO    TBL01-ITEM08-LEN (I2)
               ELSE
                   IF      WK-ITEM08-LEN >     TBL01-ITEM08-LEN (I2)
                           MOVE    WK-ITEM08   TO      TBL01-ITEM08 (I2)
                           MOVE    WK-ITEM08-LEN TO   
                                   TBL01-ITEM08-LEN (I2)
                   END-IF
               END-IF
           END-IF

           IF      WK-ITEM09 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM09 (I2) = SPACE
                   MOVE    WK-ITEM09   TO      TBL01-ITEM09     (I2)
                   MOVE    WK-ITEM09-LEN TO    TBL01-ITEM09-LEN (I2)
               ELSE
                   IF      WK-ITEM09-LEN >     TBL01-ITEM09-LEN (I2)
                           MOVE    WK-ITEM09   TO      TBL01-ITEM09 (I2)
                           MOVE    WK-ITEM09-LEN TO   
                                   TBL01-ITEM09-LEN (I2)
                   END-IF
               END-IF
           END-IF

           IF      WK-ITEM10 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM10 (I2) = SPACE
                   MOVE    WK-ITEM10   TO      TBL01-ITEM10     (I2)
                   MOVE    WK-ITEM10-LEN TO    TBL01-ITEM10-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM10 = TBL01-ITEM10 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM10   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM10-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF

           IF      WK-ITEM11 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM11 (I2) = SPACE
                   MOVE    WK-ITEM11   TO      TBL01-ITEM11     (I2)
                   MOVE    WK-ITEM11-LEN TO    TBL01-ITEM11-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM11 = TBL01-ITEM11 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM12 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM12     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM12-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM11   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM11-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF

           IF      WK-ITEM12 (1:5) =   SPACE
                   CONTINUE
           ELSE
               IF      TBL01-ITEM12 (I2) = SPACE
                   MOVE    WK-ITEM12   TO      TBL01-ITEM12     (I2)
                   MOVE    WK-ITEM12-LEN TO    TBL01-ITEM12-LEN (I2)
               ELSE
      *    *** 02,03,05,08,09 はセットしない
                   EVALUATE TRUE
                       WHEN WK-ITEM12 = TBL01-ITEM12 (I2)
                           CONTINUE
                       WHEN TBL01-ITEM01 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM01     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM01-LEN (I2)
                       WHEN TBL01-ITEM04 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM04     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM04-LEN (I2)
                       WHEN TBL01-ITEM06 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM06     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM06-LEN (I2)
                       WHEN TBL01-ITEM07 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM07     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM07-LEN (I2)
                       WHEN TBL01-ITEM10 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM10     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM10-LEN (I2)
                       WHEN TBL01-ITEM11 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM11     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM11-LEN (I2)
                       WHEN TBL01-ITEM13 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM13     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM13-LEN (I2)
                       WHEN TBL01-ITEM14 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM14     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM14-LEN (I2)
                       WHEN TBL01-ITEM15 (I2) (1:1) = SPACE
                           MOVE   WK-ITEM12   TO   TBL01-ITEM15     (I2)
                           MOVE   WK-ITEM12-LEN TO TBL01-ITEM15-LEN (I2)
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
           END-IF
           .
       S122-EX.
           EXIT.

      *    *** WATCH 無し、TBL01 SET
       S124-10.

           MOVE    WK-ITEM01   TO      TBL01-ITEM01 (I)
           MOVE    WK-ITEM02   TO      TBL01-ITEM02 (I)
           MOVE    WK-ITEM03   TO      TBL01-ITEM03 (I)
           MOVE    WK-ITEM04   TO      TBL01-ITEM04 (I)
           MOVE    WK-ITEM05   TO      TBL01-ITEM05 (I)
           MOVE    WK-ITEM06   TO      TBL01-ITEM06 (I)
           MOVE    WK-ITEM07   TO      TBL01-ITEM07 (I)
           MOVE    WK-ITEM08   TO      TBL01-ITEM08 (I)
           MOVE    WK-ITEM09   TO      TBL01-ITEM09 (I)
           MOVE    WK-ITEM10   TO      TBL01-ITEM10 (I)
           MOVE    WK-ITEM11   TO      TBL01-ITEM11 (I)
           MOVE    WK-ITEM12   TO      TBL01-ITEM12 (I)

           MOVE    WK-ITEM05X  TO      TBL01-ITEM05X (I)
           MOVE    WK-TIME     TO      TBL01-TIME   (I)

           MOVE    WK-ITEM01-LEN TO    TBL01-ITEM01-LEN  (I)
           MOVE    WK-ITEM02-LEN TO    TBL01-ITEM02-LEN  (I)
           MOVE    WK-ITEM03-LEN TO    TBL01-ITEM03-LEN  (I)
           MOVE    WK-ITEM04-LEN TO    TBL01-ITEM04-LEN  (I)
           MOVE    WK-ITEM05-LEN TO    TBL01-ITEM05-LEN  (I)
           MOVE    WK-ITEM06-LEN TO    TBL01-ITEM06-LEN  (I)
           MOVE    WK-ITEM07-LEN TO    TBL01-ITEM07-LEN  (I)
           MOVE    WK-ITEM08-LEN TO    TBL01-ITEM08-LEN  (I)
           MOVE    WK-ITEM09-LEN TO    TBL01-ITEM09-LEN  (I)
           MOVE    WK-ITEM10-LEN TO    TBL01-ITEM10-LEN  (I)
           MOVE    WK-ITEM11-LEN TO    TBL01-ITEM11-LEN  (I)
           MOVE    WK-ITEM12-LEN TO    TBL01-ITEM12-LEN  (I)

           MOVE    WK-ITEM05X-LEN TO   TBL01-ITEM05X-LEN (I)
           MOVE    WK-TIME-LEN   TO    TBL01-TIME-LEN    (I)
           .
       S124-EX.
           EXIT.

      *    *** TBL01 CHECK 02,03,05 URL CHECK しない
       S130-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

               MOVE    TBL01-ITEM01-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM04-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM06-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM07-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM08-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM09-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM01 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM01     (I)
                           MOVE    ZERO        TO   TBL01-ITEM01-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM04-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM06-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM07-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM08-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM09-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM04 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM04     (I)
                           MOVE    ZERO        TO   TBL01-ITEM04-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM06-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM07-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM08-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM09-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM06 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM06     (I)
                           MOVE    ZERO        TO   TBL01-ITEM06-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM07-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM08-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM09-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM07 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM07     (I)
                           MOVE    ZERO        TO   TBL01-ITEM07-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM08-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM09-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM08 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM08     (I)
                           MOVE    ZERO        TO   TBL01-ITEM08-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM09-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM10-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM09 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM09     (I)
                           MOVE    ZERO        TO   TBL01-ITEM09-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM10-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM11-LEN (I) TO L2
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM10 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM10     (I)
                           MOVE    ZERO        TO   TBL01-ITEM10-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM11-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM12-LEN (I) TO L2
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM11 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM11     (I)
                           MOVE    ZERO        TO   TBL01-ITEM11-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM12-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM13-LEN (I) TO L2
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM12 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM12     (I)
                           MOVE    ZERO        TO   TBL01-ITEM12-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM13-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM14-LEN (I) TO L2
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                       END-IF
                   END-IF

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM13 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM13     (I)
                           MOVE    ZERO        TO   TBL01-ITEM13-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF



               MOVE    TBL01-ITEM14-LEN (I) TO L
               IF      L       NOT = ZERO

                   MOVE    TBL01-ITEM15-LEN (I) TO L2
                   MOVE    TBL01-ITEM14 (I) TO  WK-ITEM-1
                   MOVE    TBL01-ITEM15 (I) TO  WK-ITEM-2
      *    *** ITEM CHECK
                   PERFORM S132-10     THRU     S132-EX

                   IF      SW-HIT1 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM14     (I)
                           MOVE    ZERO        TO   TBL01-ITEM14-LEN (I)
                   ELSE
                       IF      SW-HIT2 = "Y"
                           MOVE    SPACE       TO   TBL01-ITEM15     (I)
                           MOVE    ZERO        TO   TBL01-ITEM15-LEN (I)
                       END-IF
                   END-IF
               END-IF

           END-PERFORM
           .
       S130-EX.
           EXIT.

      *    *** ITEM CHECK
      *    *** WK-ITEM-1,WK-ITEM-2 の長さ小さい方で大きい方をチェックし、
      *    *** 内容同じなら、SW-HIT1、SW-HIT2をセットし、S130-10でクリアー
      *    *** する
       S132-10.

           MOVE    "N"         TO      SW-HIT1
                                       SW-HIT2
           IF      L           >       L2
                   PERFORM VARYING I3 FROM 1 BY 1
                           UNTIL I3 > L - L2 + 1
                              OR SW-HIT2 = "Y"
                           IF      L2 > 2
                               AND WK-ITEM-1 (I3:L2) = WK-ITEM-2 (1:L2)
                                   MOVE    "Y"         TO      SW-HIT2
                           END-IF
                   END-PERFORM
           ELSE
                   PERFORM VARYING I3 FROM 1 BY 1
                           UNTIL I3 > L2 - L + 1
                              OR SW-HIT1 = "Y"
                           IF      L > 2
                               AND WK-ITEM-1 (1:L) = WK-ITEM-2 (I3:L)
                                   MOVE    "Y"         TO      SW-HIT1
                           END-IF
                   END-PERFORM
           END-IF
           .
       S132-EX.
           EXIT.

      *    *** TBL01 => WRITE POT1
       S140-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX

                   MOVE    SPACE       TO      POT1-REC
                   MOVE    1           TO      P

                   MOVE    TBL01-ITEM01-LEN (I) TO L
                   MOVE    TBL01-ITEM01 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM02-LEN (I) TO L
                   MOVE    TBL01-ITEM02 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM03-LEN (I) TO L
                   MOVE    TBL01-ITEM03 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM04-LEN (I) TO L
                   MOVE    TBL01-ITEM04 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM05-LEN (I) TO L
                   MOVE    TBL01-ITEM05 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM06-LEN (I) TO L
                   MOVE    TBL01-ITEM06 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM07-LEN (I) TO L
                   MOVE    TBL01-ITEM07 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM08-LEN (I) TO L
                   MOVE    TBL01-ITEM08 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM09-LEN (I) TO L
                   MOVE    TBL01-ITEM09 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM10-LEN (I) TO L
                   MOVE    TBL01-ITEM10 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM11-LEN (I) TO L
                   MOVE    TBL01-ITEM11 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM12-LEN (I) TO L
                   MOVE    TBL01-ITEM12 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM13-LEN (I) TO L
                   MOVE    TBL01-ITEM13 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM14-LEN (I) TO L
                   MOVE    TBL01-ITEM14 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL01-ITEM15-LEN (I) TO L
                   MOVE    TBL01-ITEM15 (I) TO POT1-REC (P:L)
                   ADD     L           TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S140-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
