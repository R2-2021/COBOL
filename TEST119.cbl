      *    *** 中国系アーティスト一覧
      *    *** (タイトル括弧内の漢字をジャパリをして、下記６ファイルを
      *    *** １つにして出力する、１件目％はジャパリタイトルとして出力)
      *    *** 
      *    *** TEST53.中国系アーティスト一覧.PIN1  自動作成
      *    *** 
      *    *** TEST118
      *    ***    |
      *    *** TEST120 <--
      *    ***    |      |
      *    *** TEST119 ---
      *    ***    |
      *    *** TEST53
      *    ***    |
      *    *** TEST54
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST119.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST53_中国大陸女性アーティスト一覧.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国大陸男性アーティスト一覧.PIN1
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国大陸グループアーティスト一覧.PIN1
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾女性アーティスト一覧.PIN1
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾男性アーティスト一覧.PIN1
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾グループアーティスト一覧.PIN1
       SELECT PIN6-F           ASSIGN   WK-PIN6-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST120.PIN2 ピンインデータ
       SELECT PIN7-F           ASSIGN   WK-PIN7-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.



      *    *** TEST53_中国系アーティスト一覧.PIN1
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国系アーティスト２一覧.PIN1
      *    *** ジャパリＸ 一文字版
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国系アーティスト３一覧.PIN
      *    *** CHANNEL 登録有り版
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国系アーティスト２一覧.PIN
      *    *** タイトル一文字目データ
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  PIN3-F
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03                  PIC  X(1000).

       FD  PIN4-F
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03                  PIC  X(1000).

       FD  PIN5-F
           RECORD VARYING DEPENDING ON WK-PIN5-LEN.
       01  PIN5-REC.
           03                  PIC  X(1000).

       FD  PIN6-F
           RECORD VARYING DEPENDING ON WK-PIN6-LEN.
       01  PIN6-REC.
           03                  PIC  X(1000).

       FD  PIN7-F
           RECORD VARYING DEPENDING ON WK-PIN7-LEN.
       01  PIN7-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03                  PIC  X(1000).

       FD  POT3-F.
       01  POT3-REC.
           03                  PIC  X(1000).

       FD  POT4-F.
       01  POT4-REC.
           03                  PIC  X(10000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST119 ".

           03  WK-PIN1-F-NAME  PIC  X(064) VALUE 
               "TEST53_中国大陸女性アーティスト一覧.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(064) VALUE 
               "TEST53_中国大陸男性アーティスト一覧.PIN1".
           03  WK-PIN3-F-NAME  PIC  X(064) VALUE 
               "TEST53_中国大陸グループアーティスト一覧.PIN1".
           03  WK-PIN4-F-NAME  PIC  X(064) VALUE 
               "TEST53_香港台湾女性アーティスト一覧.PIN1".
           03  WK-PIN5-F-NAME  PIC  X(064) VALUE 
               "TEST53_香港台湾男性アーティスト一覧.PIN1".
           03  WK-PIN6-F-NAME  PIC  X(064) VALUE 
               "TEST53_香港台湾グループアーティスト一覧.PIN1".
           03  WK-PIN7-F-NAME  PIC  X(032) VALUE "TEST120.PIN2".

           03  WK-POT1-F-NAME  PIC  X(064) VALUE
               "TEST53_中国系アーティスト一覧.PIN1".
           03  WK-POT2-F-NAME  PIC  X(064) VALUE
               "TEST53_中国系アーティスト２一覧.PIN1".
           03  WK-POT3-F-NAME  PIC  X(064) VALUE
               "TEST53_中国系アーティスト３一覧.PIN1".

           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST119.POT4".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN5-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN6-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN7-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN6-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN7-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN6-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN7-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN6-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN7-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE        PIC  X(100) VALUE SPACE.
           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-IMG          PIC  X(200) VALUE SPACE.
           03  WK-CHANNEL      PIC  X(200) VALUE SPACE.
           03  WK-POT4-REC     PIC  X(10000) VALUE SPACE.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** 中国系アーティスト一覧,
           03  WK-HEAD.
             05                PIC  X(017) VALUE 
               X"E4B8ADE59BBDE7B3BBE382A2E383BCE383".
             05                PIC  X(017) VALUE 
               X"86E382A3E382B9E38388E4B880E8A6A72C".

      *    *** 中国系アーティスト２一覧,
           03  WK-HEAD2.
             05                PIC  X(017) VALUE 
               X"E4B8ADE59BBDE7B3BBE382A2E383BCE383".
             05                PIC  X(020) VALUE 
               X"86E382A3E382B9E38388EFBC92E4B880E8A6A72C".

      *    *** 中国系アーティスト３一覧,
           03  WK-HEAD3.
             05                PIC  X(017) VALUE 
               X"E4B8ADE59BBDE7B3BBE382A2E383BCE383".
             05                PIC  X(020) VALUE 
               X"86E382A3E382B9E38388EFBC93E4B880E8A6A72C".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  SAVE-AREA.
           03  SV-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  SV-TITLE        PIC  X(100) VALUE SPACE.

       01  KEY-AREA.
           03  KEY11-AREA.
             05  KEY11-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY11-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY21-AREA.
             05  KEY21-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY21-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY31-AREA.
             05  KEY31-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY31-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY41-AREA.
             05  KEY41-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY41-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY51-AREA.
             05  KEY51-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY51-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY61-AREA.
             05  KEY61-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY61-NEW     PIC  X(003) VALUE LOW-VALUE.

           03  KEY12-AREA.
             05  KEY12-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY12-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY22-AREA.
             05  KEY22-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY22-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY32-AREA.
             05  KEY32-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY32-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY42-AREA.
             05  KEY42-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY42-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY52-AREA.
             05  KEY52-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY52-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY62-AREA.
             05  KEY62-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY62-NEW     PIC  X(003) VALUE LOW-VALUE.

           03  KEY13-AREA.
             05  KEY13-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY13-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY23-AREA.
             05  KEY23-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY23-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY33-AREA.
             05  KEY33-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY33-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY43-AREA.
             05  KEY43-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY43-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY53-AREA.
             05  KEY53-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY53-NEW     PIC  X(003) VALUE LOW-VALUE.
           03  KEY63-AREA.
             05  KEY63-OLD     PIC  X(003) VALUE LOW-VALUE.
             05  KEY63-NEW     PIC  X(003) VALUE LOW-VALUE.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I6              BINARY-LONG SYNC VALUE ZERO.
           03  I7              BINARY-LONG SYNC VALUE ZERO.
           03  I1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I3-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I4-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I5-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I6-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I7-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-REC          PIC  X(001) VALUE "1".

       01  TBL-AREA1.
           03  TBL01-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL01-PININ
                               ASCENDING KEY IS TBL01-TITLE2
                               INDEXED BY TBL01-IDX.
             05  TBL01-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL01-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL01-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL01-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA2.
           03  TBL02-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL02-PININ
                               ASCENDING KEY IS TBL02-TITLE2
                               INDEXED BY TBL02-IDX.
             05  TBL02-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL02-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL02-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL02-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA3.
           03  TBL03-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL03-PININ
                               ASCENDING KEY IS TBL03-TITLE2
                               INDEXED BY TBL03-IDX.
             05  TBL03-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL03-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL03-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL03-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA4.
           03  TBL04-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL04-PININ
                               ASCENDING KEY IS TBL04-TITLE2
                               INDEXED BY TBL04-IDX.
             05  TBL04-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL04-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL04-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL04-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA5.
           03  TBL05-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL05-PININ
                               ASCENDING KEY IS TBL05-TITLE2
                               INDEXED BY TBL05-IDX.
             05  TBL05-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL05-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL05-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL05-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA6.
           03  TBL06-AREA      OCCURS 1000
                               ASCENDING KEY IS TBL06-PININ
                               ASCENDING KEY IS TBL06-TITLE2
                               INDEXED BY TBL06-IDX.
             05  TBL06-TITLE2  PIC  X(100) VALUE HIGH-VALUE.
             05  TBL06-REC     PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL06-CH      PIC  X(001) VALUE HIGH-VALUE.
             05  TBL06-PININ   PIC  X(030) VALUE HIGH-VALUE.

       01  TBL-AREA7.
           03  TBL07-AREA      OCCURS 2000
                               ASCENDING KEY IS TBL07-KANJI
                               INDEXED BY TBL07-IDX.
             05  TBL07-PININ   PIC  X(030) VALUE HIGH-VALUE.
             05  TBL07-KANJI   PIC  X(003) VALUE HIGH-VALUE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** POT1 % 1件目 WRITE
           PERFORM S190-10     THRU    S190-EX

      *    *** PININ データ テーブルストアー
           PERFORM S700-10     THRU    S700-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 中国大陸女性アーティスト一覧
                   PERFORM S100-10     THRU    S100-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** 中国大陸男性アーティスト一覧
                   PERFORM S200-10     THRU    S200-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** 中国大陸グループアーティスト一覧
                   PERFORM S300-10     THRU    S300-EX
           END-PERFORM



           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** 香港台湾女性アーティスト一覧
                   PERFORM S400-10     THRU    S400-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE
      *    *** 香港台湾男性アーティスト一覧
                   PERFORM S500-10     THRU    S500-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN6-EOF = HIGH-VALUE
      *    *** 香港台湾グループアーティスト一覧
                   PERFORM S600-10     THRU    S600-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                               PIN2-F
                               PIN3-F
                               PIN4-F
                               PIN5-F
                               PIN6-F
                               PIN7-F
                   OUTPUT      POT1-F
                               POT2-F
                               POT3-F
                               POT4-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN1-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN1-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL01 SET
                           PERFORM S022-10     THRU    S022-EX
                   END-IF
           END-READ
           .
       S020-EX.
           EXIT.

       S022-10.

           ADD     1           TO      I1
           IF      I1          >       1000
                   DISPLAY WK-PGM-NAME " TBL01 OVER I1=" I1
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL01-TITLE2 (I1)
           MOVE    PIN1-REC     TO     TBL01-REC (I1)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL01-CH (I1)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL01-PININ (I1)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL01-TITLE2 (I1) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL01-PININ (I1)
           END-SEARCH

           MOVE    I1           TO     I1-MAX
           .
       S022-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN2-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN2-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL02 SET
                           PERFORM S032-10     THRU    S032-EX
                   END-IF
           END-READ
           .
       S030-EX.
           EXIT.

       S032-10.

           ADD     1           TO      I2
           IF      I2          >       1000
                   DISPLAY WK-PGM-NAME " TBL02 OVER I2=" I2
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL02-TITLE2 (I2)
           MOVE    PIN2-REC     TO     TBL02-REC (I2)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL02-CH (I2)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL02-PININ (I2)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL02-TITLE2 (I2) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL02-PININ (I2)
           END-SEARCH

           MOVE    I2           TO     I2-MAX
           .
       S032-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN3-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
                   UNSTRING PIN3-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN3-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN3-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL03 SET
                           PERFORM S042-10     THRU    S042-EX
                   END-IF
           END-READ
           .
       S040-EX.
           EXIT.

       S042-10.

           ADD     1           TO      I3
           IF      I3          >       1000
                   DISPLAY WK-PGM-NAME " TBL03 OVER I3=" I3
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL03-TITLE2 (I3)
           MOVE    PIN3-REC     TO     TBL03-REC (I3)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL03-CH (I3)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL03-PININ (I3)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL03-TITLE2 (I3) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL03-PININ (I3)
           END-SEARCH

           MOVE    I3           TO     I3-MAX
           .
       S042-EX.
           EXIT.

      *    *** READ PIN4
       S050-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN4-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
                   UNSTRING PIN4-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN4-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN4-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL04 SET
                           PERFORM S052-10     THRU    S052-EX
                   END-IF
           END-READ
           .
       S050-EX.
           EXIT.

       S052-10.

           ADD     1           TO      I4
           IF      I4          >       1000
                   DISPLAY WK-PGM-NAME " TBL04 OVER I4=" I4
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL04-TITLE2 (I4)
           MOVE    PIN4-REC     TO     TBL04-REC (I4)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL04-CH (I4)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL04-PININ (I4)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL04-TITLE2 (I4) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL04-PININ (I4)
           END-SEARCH

           MOVE    I4           TO     I4-MAX
           .
       S052-EX.
           EXIT.

      *    *** READ PIN5
       S060-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN5-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
                   UNSTRING PIN5-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN5-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN5-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL05 SET
                           PERFORM S062-10     THRU    S062-EX
                   END-IF
           END-READ
           .
       S060-EX.
           EXIT.

       S062-10.

           ADD     1           TO      I5
           IF      I5          >       1000
                   DISPLAY WK-PGM-NAME " TBL05 OVER I5=" I5
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL05-TITLE2 (I5)
           MOVE    PIN5-REC     TO     TBL05-REC (I5)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL05-CH (I5)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL05-PININ (I5)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL05-TITLE2 (I5) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL05-PININ (I5)
           END-SEARCH

           MOVE    I5           TO     I5-MAX
           .
       S062-EX.
           EXIT.

      *    *** READ PIN6
       S070-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       WK-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       WK-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN6-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN6-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN6-CNT
                   UNSTRING PIN6-REC
                       DELIMITED BY "(" OR ")" OR ","
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                               WK-IMG      COUNT WK-IMG-LEN
                               WK-CHANNEL  COUNT WK-CHANNEL-LEN
                   END-UNSTRING

      *    *** ジャパリ
                   IF      PIN6-REC (1:12) = X"E382B8E383A3E38391E383AA"
                        OR PIN6-REC (1:1) =    "%"
                           CONTINUE
                   ELSE
      *    *** TBL06 SET
                           PERFORM S072-10     THRU    S072-EX
                   END-IF
           END-READ
           .
       S070-EX.
           EXIT.

       S072-10.

           ADD     1           TO      I6
           IF      I6          >       1000
                   DISPLAY WK-PGM-NAME " TBL06 OVER I6=" I6
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2    TO     TBL06-TITLE2 (I6)
           MOVE    PIN6-REC     TO     TBL06-REC (I6)
           IF      WK-CHANNEL   NOT=   SPACE
                   MOVE    "*"          TO     TBL06-CH (I6)
           END-IF

           SEARCH ALL TBL07-AREA
               AT END 
                  MOVE    SPACE         TO     TBL06-PININ (I6)
               WHEN TBL07-KANJI (TBL07-IDX) = TBL06-TITLE2 (I6) (1:3)
                  MOVE    TBL07-PININ (TBL07-IDX) TO
                          TBL06-PININ (I6)
           END-SEARCH

           MOVE    I6           TO     I6-MAX
           .
       S072-EX.
           EXIT.

      *    *** READ PIN7
       S080-10.

      *    *** 1件目 ゼロバイト区切り
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
                   GO  TO  S080-EX
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
           END-READ

      *    *** 2件目 カナ読み OR AT END
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
           END-READ

      *    *** 3件目 ゼロバイト区切り
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
           END-READ

      *    *** 4件目 PININ読み
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
      *    *** TBL07 SET
                   PERFORM S082-10     THRU    S082-EX
           END-READ

      *    *** 5件目  ゼロバイト区切り
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
           END-READ

      *    *** 6件目 漢字
           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
                   MOVE    PIN7-REC     TO     TBL07-KANJI (I7)
           END-READ
           .
       S080-EX.
           EXIT.

      *    *** TBL07 SET
       S082-10.

           ADD     1           TO      I7
           IF      I7          >       2000
                   DISPLAY WK-PGM-NAME " TBL07 OVER I7=" I7
                   STOP    RUN
           END-IF

           MOVE    PIN7-REC     TO     TBL07-PININ (I7)
           MOVE    I7           TO     I7-MAX
           .
       S082-EX.
           EXIT.

      *    *** 中国大陸女性アーティスト一覧
       S100-10.

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S110-10     THRU    S110-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-PININ
                   ASCENDING KEY TBL01-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL I1 > I1-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S130-10     THRU    S130-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S150-10     THRU    S150-EX

                   IF      TBL01-CH (I1) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL01-REC (I1) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL I1 > I1-MAX
                   IF    ( TBL01-TITLE2 (I1) (1:1) >= "0" AND <= "9" )
                      OR ( TBL01-TITLE2 (I1) (1:1) >= "a" AND <= "z" )
                      OR ( TBL01-TITLE2 (I1) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL01-TITLE2 (I1) (1:1) TO
                                                        KEY11-NEW
                   ELSE
                           MOVE    TBL01-TITLE2 (I1) TO KEY11-NEW
                   END-IF
                   IF      KEY11-OLD   NOT =   KEY11-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S140-10     THRU    S140-EX
                   END-IF
                   MOVE    KEY11-NEW   TO      KEY11-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S120-10     THRU    S120-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S130-10     THRU    S130-EX

                   IF      TBL01-CH (I1) =     "*"
                           IF    ( TBL01-TITLE2 (I1) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL01-TITLE2 (I1) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL01-TITLE2 (I1) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL01-TITLE2 (I1) (1:1) TO
                                                               KEY13-NEW
                           ELSE
                                   MOVE    TBL01-TITLE2(I1) TO KEY13-NEW
                           END-IF
                           IF      KEY13-OLD   NOT =   KEY13-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S140-10     THRU    S140-EX
                           END-IF
                           MOVE    KEY13-NEW   TO      KEY13-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S160-10     THRU    S160-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL I1 > I1-MAX
                   IF    ( TBL01-TITLE2 (I1) (1:1) >= "0" AND <= "9" )
                      OR ( TBL01-TITLE2 (I1) (1:1) >= "a" AND <= "z" )
                      OR ( TBL01-TITLE2 (I1) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL01-TITLE2 (I1) (1:1) TO
                                                        KEY12-NEW
                   ELSE
                           MOVE    TBL01-TITLE2 (I1) TO KEY12-NEW
                   END-IF
                   IF      KEY12-OLD    NOT =   KEY12-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S140-10     THRU    S140-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S150-10     THRU    S150-EX
                   MOVE    KEY12-NEW    TO      KEY12-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S100-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S110-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN1-REC (3:42) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN1-REC (3:42) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN1-REC (3:42) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S110-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S120-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL01-TITLE2 (I1) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

           MOVE    TBL01-REC (I1) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S130-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S140-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL01-TITLE2 (I1) (1:3) TO POT1-REC (13:)
                   MOVE    KEY11-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL01-TITLE2 (I1) (1:3) TO POT2-REC (13:)
                   MOVE    KEY12-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL01-TITLE2 (I1) (1:3) TO POT3-REC (13:)
                   MOVE    KEY13-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL01-TITLE2 (I1) (1:1) >= "A"
               AND TBL01-TITLE2 (I1) (1:1) <= "Z" )
              OR ( TBL01-TITLE2 (I1) (1:1) >= "a"
               AND TBL01-TITLE2 (I1) (1:1) <= "z" )
              OR ( TBL01-TITLE2 (I1) (1:1) >= "0"
               AND TBL01-TITLE2 (I1) (1:1) <= "9" )
              OR   TBL01-TITLE2 (I1) (1:1) =  "-"
                   IF    ( TBL01-TITLE2 (I1) (2:1) >= "A"
                       AND TBL01-TITLE2 (I1) (2:1) <= "Z" )
                      OR ( TBL01-TITLE2 (I1) (2:1) >= "a"
                       AND TBL01-TITLE2 (I1) (2:1) <= "z" )
                      OR ( TBL01-TITLE2 (I1) (2:1) >= "0"
                       AND TBL01-TITLE2 (I1) (2:1) <= "9" )
                      OR   TBL01-TITLE2 (I1) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL01-TITLE2 (I1) (3:1) >= "A"
                       AND TBL01-TITLE2 (I1) (3:1) <= "Z" )
                      OR ( TBL01-TITLE2 (I1) (3:1) >= "a"
                       AND TBL01-TITLE2 (I1) (3:1) <= "z" )
                      OR ( TBL01-TITLE2 (I1) (3:1) >= "0"
                       AND TBL01-TITLE2 (I1) (3:1) <= "9" )
                      OR   TBL01-TITLE2 (I1) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL01-PININ (I1) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL01-PININ (I1) TO POT2-REC (17:)

                               MOVE    TBL01-TITLE2 (I1) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL01-PININ (I1) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S140-EX.
           EXIT.

      *    *** WRITE POT2
       S150-10.

           MOVE    TBL01-REC (I1) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S150-EX.
           EXIT.

      *    *** WRITE POT3
       S160-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL01-TITLE2 (I1) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL01-REC (I1) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S160-EX.
           EXIT.

      *    *** WRITE POT4
       S170-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S170-EX.
           EXIT.

      *    *** クリアー
       S180-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S180-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S190-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    "% "        TO      POT1-REC (1:2)
           MOVE    WK-HEAD     TO      POT1-REC (3:34)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      POT2-REC
           MOVE    "% "        TO      POT2-REC (1:2)
           MOVE    WK-HEAD2     TO     POT2-REC (3:37)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    SPACE       TO      POT3-REC
           MOVE    "% "        TO      POT3-REC (1:2)
           MOVE    WK-HEAD3     TO     POT3-REC (3:37)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S190-EX.
           EXIT.

      *    *** 中国大陸男性アーティスト一覧
       S200-10.

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S210-10     THRU    S210-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           SORT    TBL02-AREA
                   ASCENDING KEY TBL02-PININ
                   ASCENDING KEY TBL02-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I2-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S230-10     THRU    S230-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S250-10     THRU    S250-EX

                   IF      TBL02-CH (I2) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL02-REC (I2) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I2-MAX
                   IF    ( TBL02-TITLE2 (I2) (1:1) >= "0" AND <= "9" )
                      OR ( TBL02-TITLE2 (I2) (1:1) >= "a" AND <= "z" )
                      OR ( TBL02-TITLE2 (I2) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL02-TITLE2 (I2) (1:1) TO
                                                        KEY21-NEW
                   ELSE
                           MOVE    TBL02-TITLE2 (I2) TO KEY21-NEW
                   END-IF
                   IF      KEY21-OLD   NOT =   KEY21-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S240-10     THRU    S240-EX
                   END-IF
                   MOVE    KEY21-NEW   TO      KEY21-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S220-10     THRU    S220-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S230-10     THRU    S230-EX

                   IF      TBL02-CH (I2) =     "*"
                           IF    ( TBL02-TITLE2 (I2) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL02-TITLE2 (I2) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL02-TITLE2 (I2) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL02-TITLE2 (I2) (1:1) TO
                                                               KEY23-NEW
                           ELSE
                                   MOVE    TBL02-TITLE2(I2) TO KEY23-NEW
                           END-IF
                           IF      KEY23-OLD   NOT =   KEY23-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S240-10     THRU    S240-EX
                           END-IF
                           MOVE    KEY23-NEW   TO      KEY23-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S260-10     THRU    S260-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I2-MAX
                   IF    ( TBL02-TITLE2 (I2) (1:1) >= "0" AND <= "9" )
                      OR ( TBL02-TITLE2 (I2) (1:1) >= "a" AND <= "z" )
                      OR ( TBL02-TITLE2 (I2) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL02-TITLE2 (I2) (1:1) TO
                                                        KEY22-NEW
                   ELSE
                           MOVE    TBL02-TITLE2 (I2) TO KEY22-NEW
                   END-IF
                   IF      KEY22-OLD    NOT =   KEY22-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S240-10     THRU    S240-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S250-10     THRU    S250-EX
                   MOVE    KEY22-NEW    TO      KEY22-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S200-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S210-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN2-REC (3:42) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN2-REC (3:42) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN2-REC (3:42) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S210-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S220-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL02-TITLE2 (I2) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S220-EX.
           EXIT.

      *    *** WRITE POT1
       S230-10.

           MOVE    TBL02-REC (I2) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S230-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S240-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL02-TITLE2 (I2) (1:3) TO POT1-REC (13:)
                   MOVE    KEY21-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL02-TITLE2 (I2) (1:3) TO POT2-REC (13:)
                   MOVE    KEY22-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL02-TITLE2 (I2) (1:3) TO POT3-REC (13:)
                   MOVE    KEY23-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL02-TITLE2 (I2) (1:1) >= "A"
               AND TBL02-TITLE2 (I2) (1:1) <= "Z" )
              OR ( TBL02-TITLE2 (I2) (1:1) >= "a"
               AND TBL02-TITLE2 (I2) (1:1) <= "z" )
              OR ( TBL02-TITLE2 (I2) (1:1) >= "0"
               AND TBL02-TITLE2 (I2) (1:1) <= "9" )
              OR   TBL02-TITLE2 (I2) (1:1) =  "-"
                   IF    ( TBL02-TITLE2 (I2) (2:1) >= "A"
                       AND TBL02-TITLE2 (I2) (2:1) <= "Z" )
                      OR ( TBL02-TITLE2 (I2) (2:1) >= "a"
                       AND TBL02-TITLE2 (I2) (2:1) <= "z" )
                      OR ( TBL02-TITLE2 (I2) (2:1) >= "0"
                       AND TBL02-TITLE2 (I2) (2:1) <= "9" )
                      OR   TBL02-TITLE2 (I2) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL02-TITLE2 (I2) (3:1) >= "A"
                       AND TBL02-TITLE2 (I2) (3:1) <= "Z" )
                      OR ( TBL02-TITLE2 (I2) (3:1) >= "a"
                       AND TBL02-TITLE2 (I2) (3:1) <= "z" )
                      OR ( TBL02-TITLE2 (I2) (3:1) >= "0"
                       AND TBL02-TITLE2 (I2) (3:1) <= "9" )
                      OR   TBL02-TITLE2 (I2) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL02-PININ (I2) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL02-PININ (I2) TO POT2-REC (17:)

                               MOVE    TBL02-TITLE2 (I2) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL02-PININ (I2) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S240-EX.
           EXIT.

      *    *** WRITE POT2
       S250-10.

           MOVE    TBL02-REC (I2) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S250-EX.
           EXIT.

      *    *** WRITE POT3
       S260-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL02-TITLE2 (I2) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL02-REC (I2) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S260-EX.
           EXIT.

      *    *** WRITE POT4
       S270-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S270-EX.
           EXIT.

      *    *** クリアー
       S280-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S280-EX.
           EXIT.

      *    *** 中国大陸グループアーティスト一覧
       S300-10.

      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S310-10     THRU    S310-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

           SORT    TBL03-AREA
                   ASCENDING KEY TBL03-PININ
                   ASCENDING KEY TBL03-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I3 FROM 1 BY 1
                   UNTIL I3 > I3-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S330-10     THRU    S330-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S350-10     THRU    S350-EX

                   IF      TBL03-CH (I3) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL03-REC (I3) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I3 FROM 1 BY 1
                   UNTIL I3 > I3-MAX
                   IF    ( TBL03-TITLE2 (I3) (1:1) >= "0" AND <= "9" )
                      OR ( TBL03-TITLE2 (I3) (1:1) >= "a" AND <= "z" )
                      OR ( TBL03-TITLE2 (I3) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL03-TITLE2 (I3) (1:1) TO
                                                        KEY31-NEW
                   ELSE
                           MOVE    TBL03-TITLE2 (I3) TO KEY31-NEW
                   END-IF
                   IF      KEY31-OLD   NOT =   KEY31-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S340-10     THRU    S340-EX
                   END-IF
                   MOVE    KEY31-NEW   TO      KEY31-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S320-10     THRU    S320-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S330-10     THRU    S330-EX

                   IF      TBL03-CH (I3) =     "*"
                           IF    ( TBL03-TITLE2 (I3) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL03-TITLE2 (I3) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL03-TITLE2 (I3) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL03-TITLE2 (I3) (1:1) TO
                                                               KEY33-NEW
                           ELSE
                                   MOVE    TBL03-TITLE2(I3) TO KEY33-NEW
                           END-IF
                           IF      KEY33-OLD   NOT =   KEY33-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S340-10     THRU    S340-EX
                           END-IF
                           MOVE    KEY33-NEW   TO      KEY33-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S360-10     THRU    S360-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I3 FROM 1 BY 1
                   UNTIL I3 > I3-MAX
                   IF    ( TBL03-TITLE2 (I3) (1:1) >= "0" AND <= "9" )
                      OR ( TBL03-TITLE2 (I3) (1:1) >= "a" AND <= "z" )
                      OR ( TBL03-TITLE2 (I3) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL03-TITLE2 (I3) (1:1) TO
                                                        KEY32-NEW
                   ELSE
                           MOVE    TBL03-TITLE2 (I3) TO KEY32-NEW
                   END-IF
                   IF      KEY32-OLD    NOT =   KEY32-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S340-10     THRU    S340-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S350-10     THRU    S350-EX
                   MOVE    KEY32-NEW    TO      KEY32-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S300-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S310-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN3-REC (3:48) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN3-REC (3:48) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN3-REC (3:48) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S310-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S320-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL03-TITLE2 (I3) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S320-EX.
           EXIT.

      *    *** WRITE POT1
       S330-10.

           MOVE    TBL03-REC (I3) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S330-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S340-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL03-TITLE2 (I3) (1:3) TO POT1-REC (13:)
                   MOVE    KEY31-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL03-TITLE2 (I3) (1:3) TO POT2-REC (13:)
                   MOVE    KEY32-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL03-TITLE2 (I3) (1:3) TO POT3-REC (13:)
                   MOVE    KEY33-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL03-TITLE2 (I3) (1:1) >= "A"
               AND TBL03-TITLE2 (I3) (1:1) <= "Z" )
              OR ( TBL03-TITLE2 (I3) (1:1) >= "a"
               AND TBL03-TITLE2 (I3) (1:1) <= "z" )
              OR ( TBL03-TITLE2 (I3) (1:1) >= "0"
               AND TBL03-TITLE2 (I3) (1:1) <= "9" )
              OR   TBL03-TITLE2 (I3) (1:1) =  "-"
                   IF    ( TBL03-TITLE2 (I3) (2:1) >= "A"
                       AND TBL03-TITLE2 (I3) (2:1) <= "Z" )
                      OR ( TBL03-TITLE2 (I3) (2:1) >= "a"
                       AND TBL03-TITLE2 (I3) (2:1) <= "z" )
                      OR ( TBL03-TITLE2 (I3) (2:1) >= "0"
                       AND TBL03-TITLE2 (I3) (2:1) <= "9" )
                      OR   TBL03-TITLE2 (I3) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL03-TITLE2 (I3) (3:1) >= "A"
                       AND TBL03-TITLE2 (I3) (3:1) <= "Z" )
                      OR ( TBL03-TITLE2 (I3) (3:1) >= "a"
                       AND TBL03-TITLE2 (I3) (3:1) <= "z" )
                      OR ( TBL03-TITLE2 (I3) (3:1) >= "0"
                       AND TBL03-TITLE2 (I3) (3:1) <= "9" )
                      OR   TBL03-TITLE2 (I3) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL03-PININ (I3) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL03-PININ (I3) TO POT2-REC (17:)

                               MOVE    TBL03-TITLE2 (I3) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL03-PININ (I3) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S340-EX.
           EXIT.

      *    *** WRITE POT2
       S350-10.

           MOVE    TBL03-REC (I3) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S350-EX.
           EXIT.

      *    *** WRITE POT3
       S360-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL03-TITLE2 (I3) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL03-REC (I3) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S360-EX.
           EXIT.

      *    *** WRITE POT4
       S370-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S370-EX.
           EXIT.

      *    *** クリアー
       S380-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S380-EX.
           EXIT.

      *    *** 香港台湾女性アーティスト一覧
       S400-10.

      *    *** READ PIN4
           PERFORM S050-10     THRU    S050-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S410-10     THRU    S410-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** READ PIN4
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM

           SORT    TBL04-AREA
                   ASCENDING KEY TBL04-PININ
                   ASCENDING KEY TBL04-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I4 FROM 1 BY 1
                   UNTIL I4 > I4-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S430-10     THRU    S430-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S450-10     THRU    S450-EX

                   IF      TBL04-CH (I4) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL04-REC (I4) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I4 FROM 1 BY 1
                   UNTIL I4 > I4-MAX
                   IF    ( TBL04-TITLE2 (I4) (1:1) >= "0" AND <= "9" )
                      OR ( TBL04-TITLE2 (I4) (1:1) >= "a" AND <= "z" )
                      OR ( TBL04-TITLE2 (I4) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL04-TITLE2 (I4) (1:1) TO
                                                        KEY41-NEW
                   ELSE
                           MOVE    TBL04-TITLE2 (I4) TO KEY41-NEW
                   END-IF
                   IF      KEY41-OLD   NOT =   KEY41-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S440-10     THRU    S440-EX
                   END-IF
                   MOVE    KEY41-NEW   TO      KEY41-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S420-10     THRU    S420-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S430-10     THRU    S430-EX

                   IF      TBL04-CH (I4) =     "*"
                           IF    ( TBL04-TITLE2 (I4) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL04-TITLE2 (I4) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL04-TITLE2 (I4) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL04-TITLE2 (I4) (1:1) TO
                                                               KEY43-NEW
                           ELSE
                                   MOVE    TBL04-TITLE2(I4) TO KEY43-NEW
                           END-IF
                           IF      KEY43-OLD   NOT =   KEY43-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S440-10     THRU    S440-EX
                           END-IF
                           MOVE    KEY43-NEW   TO      KEY43-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S460-10     THRU    S460-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I4 FROM 1 BY 1
                   UNTIL I4 > I4-MAX
                   IF    ( TBL04-TITLE2 (I4) (1:1) >= "0" AND <= "9" )
                      OR ( TBL04-TITLE2 (I4) (1:1) >= "a" AND <= "z" )
                      OR ( TBL04-TITLE2 (I4) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL04-TITLE2 (I4) (1:1) TO
                                                        KEY42-NEW
                   ELSE
                           MOVE    TBL04-TITLE2 (I4) TO KEY42-NEW
                   END-IF
                   IF      KEY42-OLD    NOT =   KEY42-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S440-10     THRU    S440-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S450-10     THRU    S450-EX
                   MOVE    KEY42-NEW    TO      KEY42-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S400-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S410-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN4-REC (3:42) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN4-REC (3:42) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN4-REC (3:42) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S410-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S420-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL04-TITLE2 (I4) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S420-EX.
           EXIT.

      *    *** WRITE POT1
       S430-10.

           MOVE    TBL04-REC (I4) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S430-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S440-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL04-TITLE2 (I4) (1:3) TO POT1-REC (13:)
                   MOVE    KEY41-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL04-TITLE2 (I4) (1:3) TO POT2-REC (13:)
                   MOVE    KEY42-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL04-TITLE2 (I4) (1:3) TO POT3-REC (13:)
                   MOVE    KEY43-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL04-TITLE2 (I4) (1:1) >= "A"
               AND TBL04-TITLE2 (I4) (1:1) <= "Z" )
              OR ( TBL04-TITLE2 (I4) (1:1) >= "a"
               AND TBL04-TITLE2 (I4) (1:1) <= "z" )
              OR ( TBL04-TITLE2 (I4) (1:1) >= "0"
               AND TBL04-TITLE2 (I4) (1:1) <= "9" )
              OR   TBL04-TITLE2 (I4) (1:1) =  "-"
                   IF    ( TBL04-TITLE2 (I4) (2:1) >= "A"
                       AND TBL04-TITLE2 (I4) (2:1) <= "Z" )
                      OR ( TBL04-TITLE2 (I4) (2:1) >= "a"
                       AND TBL04-TITLE2 (I4) (2:1) <= "z" )
                      OR ( TBL04-TITLE2 (I4) (2:1) >= "0"
                       AND TBL04-TITLE2 (I4) (2:1) <= "9" )
                      OR   TBL04-TITLE2 (I4) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL04-TITLE2 (I4) (3:1) >= "A"
                       AND TBL04-TITLE2 (I4) (3:1) <= "Z" )
                      OR ( TBL04-TITLE2 (I4) (3:1) >= "a"
                       AND TBL04-TITLE2 (I4) (3:1) <= "z" )
                      OR ( TBL04-TITLE2 (I4) (3:1) >= "0"
                       AND TBL04-TITLE2 (I4) (3:1) <= "9" )
                      OR   TBL04-TITLE2 (I4) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL04-PININ (I4) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL04-PININ (I4) TO POT2-REC (17:)

                               MOVE    TBL04-TITLE2 (I4) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL04-PININ (I4) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S440-EX.
           EXIT.

      *    *** WRITE POT2
       S450-10.

           MOVE    TBL04-REC (I4) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S450-EX.
           EXIT.

      *    *** WRITE POT3
       S460-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL04-TITLE2 (I4) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL04-REC (I4) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S460-EX.
           EXIT.

      *    *** WRITE POT4
       S470-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S470-EX.
           EXIT.

      *    *** クリアー
       S480-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S480-EX.
           EXIT.

      *    *** 香港台湾男性アーティスト一覧
       S500-10.

      *    *** READ PIN5
           PERFORM S060-10     THRU    S060-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S510-10     THRU    S510-EX

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE
      *    *** READ PIN5
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM

           SORT    TBL05-AREA
                   ASCENDING KEY TBL05-PININ
                   ASCENDING KEY TBL05-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I5 FROM 1 BY 1
                   UNTIL I5 > I5-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S530-10     THRU    S530-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S550-10     THRU    S550-EX

                   IF      TBL05-CH (I5) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL05-REC (I5) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I5 FROM 1 BY 1
                   UNTIL I5 > I5-MAX
                   IF    ( TBL05-TITLE2 (I5) (1:1) >= "0" AND <= "9" )
                      OR ( TBL05-TITLE2 (I5) (1:1) >= "a" AND <= "z" )
                      OR ( TBL05-TITLE2 (I5) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL05-TITLE2 (I5) (1:1) TO
                                                        KEY51-NEW
                   ELSE
                           MOVE    TBL05-TITLE2 (I5) TO KEY51-NEW
                   END-IF
                   IF      KEY51-OLD   NOT =   KEY51-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S540-10     THRU    S540-EX
                   END-IF
                   MOVE    KEY51-NEW   TO      KEY51-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S520-10     THRU    S520-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S530-10     THRU    S530-EX

                   IF      TBL05-CH (I5) =     "*"
                           IF    ( TBL05-TITLE2 (I5) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL05-TITLE2 (I5) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL05-TITLE2 (I5) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL05-TITLE2 (I5) (1:1) TO
                                                               KEY53-NEW
                           ELSE
                                   MOVE    TBL05-TITLE2(I5) TO KEY53-NEW
                           END-IF
                           IF      KEY53-OLD   NOT =   KEY53-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S540-10     THRU    S540-EX
                           END-IF
                           MOVE    KEY53-NEW   TO      KEY53-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S560-10     THRU    S560-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I5 FROM 1 BY 1
                   UNTIL I5 > I5-MAX
                   IF    ( TBL05-TITLE2 (I5) (1:1) >= "0" AND <= "9" )
                      OR ( TBL05-TITLE2 (I5) (1:1) >= "a" AND <= "z" )
                      OR ( TBL05-TITLE2 (I5) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL05-TITLE2 (I5) (1:1) TO
                                                        KEY52-NEW
                   ELSE
                           MOVE    TBL05-TITLE2 (I5) TO KEY52-NEW
                   END-IF
                   IF      KEY52-OLD    NOT =   KEY52-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S540-10     THRU    S540-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S550-10     THRU    S550-EX
                   MOVE    KEY52-NEW    TO      KEY52-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S500-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S510-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN5-REC (3:42) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN5-REC (3:42) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN5-REC (3:42) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S510-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S520-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL05-TITLE2 (I5) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S520-EX.
           EXIT.

      *    *** WRITE POT1
       S530-10.

           MOVE    TBL05-REC (I5) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S530-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S540-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL05-TITLE2 (I5) (1:3) TO POT1-REC (13:)
                   MOVE    KEY51-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL05-TITLE2 (I5) (1:3) TO POT2-REC (13:)
                   MOVE    KEY52-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL05-TITLE2 (I5) (1:3) TO POT3-REC (13:)
                   MOVE    KEY53-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL05-TITLE2 (I5) (1:1) >= "A"
               AND TBL05-TITLE2 (I5) (1:1) <= "Z" )
              OR ( TBL05-TITLE2 (I5) (1:1) >= "a"
               AND TBL05-TITLE2 (I5) (1:1) <= "z" )
              OR ( TBL05-TITLE2 (I5) (1:1) >= "0"
               AND TBL05-TITLE2 (I5) (1:1) <= "9" )
              OR   TBL05-TITLE2 (I5) (1:1) =  "-"
                   IF    ( TBL05-TITLE2 (I5) (2:1) >= "A"
                       AND TBL05-TITLE2 (I5) (2:1) <= "Z" )
                      OR ( TBL05-TITLE2 (I5) (2:1) >= "a"
                       AND TBL05-TITLE2 (I5) (2:1) <= "z" )
                      OR ( TBL05-TITLE2 (I5) (2:1) >= "0"
                       AND TBL05-TITLE2 (I5) (2:1) <= "9" )
                      OR   TBL05-TITLE2 (I5) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL05-TITLE2 (I5) (3:1) >= "A"
                       AND TBL05-TITLE2 (I5) (3:1) <= "Z" )
                      OR ( TBL05-TITLE2 (I5) (3:1) >= "a"
                       AND TBL05-TITLE2 (I5) (3:1) <= "z" )
                      OR ( TBL05-TITLE2 (I5) (3:1) >= "0"
                       AND TBL05-TITLE2 (I5) (3:1) <= "9" )
                      OR   TBL05-TITLE2 (I5) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL05-PININ (I5) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL05-PININ (I5) TO POT2-REC (17:)

                               MOVE    TBL05-TITLE2 (I5) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL05-PININ (I5) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S540-EX.
           EXIT.

      *    *** WRITE POT2
       S550-10.

           MOVE    TBL05-REC (I5) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S550-EX.
           EXIT.

      *    *** WRITE POT3
       S560-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL05-TITLE2 (I5) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL05-REC (I5) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S560-EX.
           EXIT.

      *    *** WRITE POT4
       S570-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S570-EX.
           EXIT.

      *    *** クリアー
       S580-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S580-EX.
           EXIT.

      *    *** 香港台湾グループアーティスト一覧
       S600-10.

      *    *** READ PIN6
           PERFORM S070-10     THRU    S070-EX

      *    *** POT1 % ジャパリに変換 WRITE
           PERFORM S610-10     THRU    S610-EX

           PERFORM UNTIL WK-PIN6-EOF = HIGH-VALUE
      *    *** READ PIN6
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM

           SORT    TBL06-AREA
                   ASCENDING KEY TBL06-PININ
                   ASCENDING KEY TBL06-TITLE2

      *    *** ALL WRITE
           PERFORM VARYING I6 FROM 1 BY 1
                   UNTIL I6 > I6-MAX

      *    *** WRITE POT1 タイトル
                   PERFORM S630-10     THRU    S630-EX

      *    *** WRITE POT2 タイトル
                   PERFORM S650-10     THRU    S650-EX

                   IF      TBL06-CH (I6) =     "*"
      *    *** WRITE POT3 タイトル
                           MOVE    TBL06-REC (I6) TO   POT3-REC

                           WRITE   POT3-REC
                           ADD     1           TO      WK-POT3-CNT
                    END-IF
           END-PERFORM

           PERFORM VARYING I6 FROM 1 BY 1
                   UNTIL I6 > I6-MAX
                   IF    ( TBL06-TITLE2 (I6) (1:1) >= "0" AND <= "9" )
                      OR ( TBL06-TITLE2 (I6) (1:1) >= "a" AND <= "z" )
                      OR ( TBL06-TITLE2 (I6) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL06-TITLE2 (I6) (1:1) TO
                                                        KEY61-NEW
                   ELSE
                           MOVE    TBL06-TITLE2 (I6) TO KEY61-NEW
                   END-IF
                   IF      KEY61-OLD   NOT =   KEY61-NEW

                           MOVE    "1"         TO      SW-REC
      *    *** WRITE POT1 ジャパリ
                           PERFORM S640-10     THRU    S640-EX
                   END-IF
                   MOVE    KEY61-NEW   TO      KEY61-OLD

      *    *** WRITE POT1 ジャパリ
                   PERFORM S620-10     THRU    S620-EX
      *    *** WRITE POT1 タイトル
                   PERFORM S630-10     THRU    S630-EX

                   IF      TBL06-CH (I6) =     "*"
                           IF    ( TBL06-TITLE2 (I6) (1:1) >= "0" 
                               AND <= "9" )
                              OR ( TBL06-TITLE2 (I6) (1:1) >= "a"
                               AND <= "z" )
                              OR ( TBL06-TITLE2 (I6) (1:1) >= "A"
                               AND <= "Z" )
                                   MOVE    TBL06-TITLE2 (I6) (1:1) TO
                                                               KEY63-NEW
                           ELSE
                                   MOVE    TBL06-TITLE2(I6) TO KEY63-NEW
                           END-IF
                           IF      KEY63-OLD   NOT =   KEY63-NEW

                                   MOVE    "3"         TO      SW-REC
      *    *** WRITE POT3 ジャパリ
                                   PERFORM S640-10     THRU    S640-EX
                           END-IF
                           MOVE    KEY63-NEW   TO      KEY63-OLD
      *    *** WRITE POT3 ジャパリ,タイトル
                           PERFORM S660-10     THRU    S660-EX
                   END-IF
           END-PERFORM

           MOVE    "2"         TO      SW-REC
           PERFORM VARYING I6 FROM 1 BY 1
                   UNTIL I6 > I6-MAX
                   IF    ( TBL06-TITLE2 (I6) (1:1) >= "0" AND <= "9" )
                      OR ( TBL06-TITLE2 (I6) (1:1) >= "a" AND <= "z" )
                      OR ( TBL06-TITLE2 (I6) (1:1) >= "A" AND <= "Z" )
                           MOVE    TBL06-TITLE2 (I6) (1:1) TO
                                                        KEY62-NEW
                   ELSE
                           MOVE    TBL06-TITLE2 (I6) TO KEY62-NEW
                   END-IF
                   IF      KEY62-OLD    NOT =   KEY62-NEW

      *    *** WRITE POT2 ジャパリ
                           PERFORM S640-10     THRU    S640-EX
                   END-IF
      *    *** WRITE POT2 タイトル
                   PERFORM S650-10     THRU    S650-EX
                   MOVE    KEY62-NEW    TO      KEY62-OLD
           END-PERFORM

      *    *** WRITE POT4
           PERFORM S170-10     THRU    S170-EX

      *    *** クリアー
           PERFORM S180-10     THRU    S180-EX
           .
       S600-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S610-10.

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    PIN6-REC (3:48) TO  POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:12)
           MOVE    PIN6-REC (3:48) TO  POT2-REC (13:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *    *** % => ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    PIN6-REC (3:48) TO  POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S610-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S620-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:12)
           MOVE    TBL06-TITLE2 (I6) TO POT1-REC (13:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S620-EX.
           EXIT.

      *    *** WRITE POT1
       S630-10.

           MOVE    TBL06-REC (I6) TO   POT1-REC

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S630-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S640-10.

      *    *** タイトル先頭１文字（３バイト）
      *    *** ジャパリＸ
           EVALUATE TRUE
               WHEN SW-REC = "1"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
      *             MOVE    TBL06-TITLE2 (I6) (1:3) TO POT1-REC (13:)
                   MOVE    KEY61-NEW   TO      POT1-REC (13:)
               WHEN SW-REC = "2"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT2-REC (1:12)
      *             MOVE    TBL06-TITLE2 (I6) (1:3) TO POT2-REC (13:)
                   MOVE    KEY62-NEW   TO      POT2-REC (13:)
               WHEN SW-REC = "3"
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT3-REC (1:12)
      *             MOVE    TBL06-TITLE2 (I6) (1:3) TO POT3-REC (13:)
                   MOVE    KEY63-NEW   TO      POT3-REC (13:)
           END-EVALUATE

           IF    ( TBL06-TITLE2 (I6) (1:1) >= "A"
               AND TBL06-TITLE2 (I6) (1:1) <= "Z" )
              OR ( TBL06-TITLE2 (I6) (1:1) >= "a"
               AND TBL06-TITLE2 (I6) (1:1) <= "z" )
              OR ( TBL06-TITLE2 (I6) (1:1) >= "0"
               AND TBL06-TITLE2 (I6) (1:1) <= "9" )
              OR   TBL06-TITLE2 (I6) (1:1) =  "-"
                   IF    ( TBL06-TITLE2 (I6) (2:1) >= "A"
                       AND TBL06-TITLE2 (I6) (2:1) <= "Z" )
                      OR ( TBL06-TITLE2 (I6) (2:1) >= "a"
                       AND TBL06-TITLE2 (I6) (2:1) <= "z" )
                      OR ( TBL06-TITLE2 (I6) (2:1) >= "0"
                       AND TBL06-TITLE2 (I6) (2:1) <= "9" )
                      OR   TBL06-TITLE2 (I6) (2:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (14:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (14:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (14:1)
                       END-EVALUATE
                   END-IF
                   IF    ( TBL06-TITLE2 (I6) (3:1) >= "A"
                       AND TBL06-TITLE2 (I6) (3:1) <= "Z" )
                      OR ( TBL06-TITLE2 (I6) (3:1) >= "a"
                       AND TBL06-TITLE2 (I6) (3:1) <= "z" )
                      OR ( TBL06-TITLE2 (I6) (3:1) >= "0"
                       AND TBL06-TITLE2 (I6) (3:1) <= "9" )
                      OR   TBL06-TITLE2 (I6) (3:1) =  "-"
                           CONTINUE
                   ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE    SPACE       TO    POT1-REC (15:1)
                           WHEN SW-REC = "2"
                               MOVE    SPACE       TO    POT2-REC (15:1)
                           WHEN SW-REC = "3"
                               MOVE    SPACE       TO    POT3-REC (15:1)
                       END-EVALUATE
                   END-IF
           ELSE
                       EVALUATE TRUE
                           WHEN SW-REC = "1"
                               MOVE   TBL06-PININ (I6) TO POT1-REC (17:)
                           WHEN SW-REC = "2"
                               MOVE   TBL06-PININ (I6) TO POT2-REC (17:)

                               MOVE    TBL06-TITLE2 (I6) (1:3) TO
                                       WK-POT4-REC (J:3)
                               ADD     3           TO      J
                               IF      J           >       10000
                                       DISPLAY WK-PGM-NAME 
                                           "POT4-REC 用 エリアオーバー"
                                       STOP    RUN
                               END-IF
                           WHEN SW-REC = "3"
                               MOVE   TBL06-PININ (I6) TO POT3-REC (17:)
                       END-EVALUATE
           END-IF

           EVALUATE TRUE
               WHEN SW-REC = "1"
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
               WHEN SW-REC = "2"
                   WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
               WHEN SW-REC = "3"
                   WRITE   POT3-REC
                   ADD     1           TO      WK-POT3-CNT
           END-EVALUATE
           .
       S640-EX.
           EXIT.

      *    *** WRITE POT2
       S650-10.

           MOVE    TBL06-REC (I6) TO   POT2-REC

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S650-EX.
           EXIT.

      *    *** WRITE POT3
       S660-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:12)
           MOVE    TBL06-TITLE2 (I6) TO POT3-REC (13:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    TBL06-REC (I6) TO   POT3-REC

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S660-EX.
           EXIT.

      *    *** WRITE POT4
       S670-10.

           WRITE   POT4-REC    FROM    WK-POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S670-EX.
           EXIT.

      *    *** クリアー
       S680-10.

           MOVE    SPACE       TO      WK-POT4-REC
           MOVE    1           TO      J
           .
       S680-EX.
           EXIT.

      *    *** PININ データ テーブルストアー
       S700-10.

           PERFORM UNTIL WK-PIN7-EOF = HIGH-VALUE
      *    *** READ PIN7
                   PERFORM S080-10     THRU    S080-EX
           END-PERFORM

           SORT    TBL07-AREA
                   ASCENDING KEY TBL07-KANJI
           .
       S700-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   PIN3-F
                   PIN4-F
                   PIN5-F
                   PIN6-F
                   PIN7-F
                   POT1-F
                   POT2-F
                   POT3-F
                   POT4-F

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
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 件数 = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 件数 = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-PIN5-CNT TO      WK-PIN5-CNT-E
           DISPLAY WK-PGM-NAME " PIN5 件数 = " WK-PIN5-CNT-E
                   " (" WK-PIN5-F-NAME ")"
           MOVE    WK-PIN6-CNT TO      WK-PIN6-CNT-E
           DISPLAY WK-PGM-NAME " PIN6 件数 = " WK-PIN6-CNT-E
                   " (" WK-PIN6-F-NAME ")"
           MOVE    WK-PIN7-CNT TO      WK-PIN7-CNT-E
           DISPLAY WK-PGM-NAME " PIN7 件数 = " WK-PIN7-CNT-E
                   " (" WK-PIN7-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4 件数 = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
