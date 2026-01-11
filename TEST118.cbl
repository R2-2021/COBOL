      *    *** 中国大陸女性アーティスト一覧
      *    *** 
      *    *** TEST53.中国大陸女性アーティスト一覧.PIN1  自動作成
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
       PROGRAM-ID.             TEST118.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST118_中国大陸女性アーティスト一覧.PIN1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST118_香港台湾女性アーティスト一覧.PIN1
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST118_中国大陸男性アーティスト一覧.PIN1
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST118_香港台湾男性アーティスト一覧.PIN1
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST118_中国大陸グループアーティスト一覧.PIN1
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST118_香港台湾グループアーティスト一覧.PIN1
       SELECT PIN6-F           ASSIGN   WK-PIN6-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アーティスト画像Ｙｏｕｔｕｂｅ
       SELECT PIN7-F           ASSIGN   WK-PIN7-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.



      *    *** TEST53_中国大陸女性アーティスト一覧.PIN1
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾女性アーティスト一覧.PIN1
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国大陸男性アーティスト一覧.PIN1
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾男性アーティスト一覧.PIN1
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_中国大陸グループアーティスト一覧.PIN1
       SELECT POT5-F           ASSIGN   WK-POT5-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST53_香港台湾グループアーティスト一覧.PIN1
       SELECT POT6-F           ASSIGN   WK-POT6-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** アーティスト画像Ｙｏｕｔｕｂｅ
       SELECT POT7-F           ASSIGN   WK-POT7-F-NAME
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
           03                  PIC  X(1000).

       FD  POT5-F.
       01  POT5-REC.
           03                  PIC  X(1000).

       FD  POT6-F.
       01  POT6-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST118 ".

           03  WK-PIN1-F-NAME  PIC  X(064) VALUE 
               "TEST118_中国大陸女性アーティスト一覧.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(064) VALUE 
               "TEST118_香港台湾女性アーティスト一覧.PIN1".

           03  WK-POT1-F-NAME  PIC  X(064) VALUE
               "TEST53_中国大陸女性アーティスト一覧.PIN1".
           03  WK-POT2-F-NAME  PIC  X(064) VALUE
               "TEST53_香港台湾女性アーティスト一覧.PIN1".

           03  WK-PIN3-F-NAME  PIC  X(064) VALUE 
               "TEST118_中国大陸男性アーティスト一覧.PIN1".
 
           03  WK-PIN4-F-NAME  PIC  X(064) VALUE 
               "TEST118_香港台湾男性アーティスト一覧.PIN1".

           03  WK-POT3-F-NAME  PIC  X(064) VALUE
               "TEST53_中国大陸男性アーティスト一覧.PIN1".
           03  WK-POT4-F-NAME  PIC  X(064) VALUE
               "TEST53_香港台湾男性アーティスト一覧.PIN1".

           03  WK-PIN5-F-NAME  PIC  X(064) VALUE 
               "TEST118_中国大陸グループアーティスト一覧.PIN1".
           03  WK-PIN6-F-NAME  PIC  X(064) VALUE 
               "TEST118_香港台湾グループアーティスト一覧.PIN1".

           03  WK-POT5-F-NAME  PIC  X(064) VALUE
               "TEST53_中国大陸グループアーティスト一覧.PIN1".
           03  WK-POT6-F-NAME  PIC  X(064) VALUE
               "TEST53_香港台湾グループアーティスト一覧.PIN1".

           03  WK-PIN7-F-NAME  PIC  X(032) VALUE "TEST118.PIN7".

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
           03  WK-POT5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT6-CNT     BINARY-LONG SYNC VALUE ZERO.

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
           03  WK-POT5-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT6-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN6-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN7-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE        PIC  X(100) VALUE SPACE.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.

           03  WK-ARTIST      PIC  X(100) VALUE SPACE.
           03  WK-ARTIST-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-ARTIST-IMG  PIC  X(200) VALUE SPACE.
           03  WK-ARTIST-IMG-LEN BINARY-LONG SYNC VALUE ZERO.
           03  WK-CHANNEL     PIC  X(100) VALUE SPACE.
           03  WK-CHANNEL-LEN BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  SAVE-AREA.
           03  SV-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  SV-TITLE        PIC  X(100) VALUE SPACE.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 3000.
             05  TBL01-ARTIST  PIC  X(100) VALUE SPACE.
             05  TBL01-ARTIST-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-ARTIST-IMG PIC  X(200) VALUE SPACE.
             05  TBL01-ARTIST-IMG-LEN BINARY-LONG SYNC VALUE ZERO.
      *    *** CHANNEL 登録
             05  TBL01-CHANNEL PIC  X(100) VALUE SPACE.
             05  TBL01-CHANNEL-LEN BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN7
           PERFORM S080-10     THRU    S080-EX

           PERFORM UNTIL WK-PIN7-EOF = HIGH-VALUE

      *    *** READ PIN7
                   PERFORM S080-10     THRU    S080-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** 中国大陸女性アーティスト一覧
                   PERFORM S100-10     THRU    S100-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** 香港台湾女性アーティスト一覧
                   PERFORM S200-10     THRU    S200-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** 中国大陸男性アーティスト一覧
                   PERFORM S300-10     THRU    S300-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** 香港台湾男性アーティスト一覧
                   PERFORM S400-10     THRU    S400-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE
      *    *** 中国大陸グループアーティスト一覧
                   PERFORM S500-10     THRU    S500-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

           PERFORM UNTIL WK-PIN6-EOF = HIGH-VALUE
      *    *** 香港台湾グループアーティスト一覧
                   PERFORM S600-10     THRU    S600-EX
           END-PERFORM

      *    ***　項目クリアー
           PERFORM S140-10     THRU    S140-EX

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
                               POT5-F
                               POT6-F

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
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           READ    PIN3-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ
           .
       S040-EX.
           EXIT.

      *    *** READ PIN4
       S050-10.

           READ    PIN4-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ
           .
       S050-EX.
           EXIT.

      *    *** READ PIN5
       S060-10.

           READ    PIN5-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
           END-READ
           .
       S060-EX.
           EXIT.

      *    *** READ PIN6
       S070-10.

           READ    PIN6-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN6-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN6-CNT
           END-READ
           .
       S070-EX.
           EXIT.

      *    *** READ PIN7
       S080-10.

           MOVE    SPACE       TO      WK-ARTIST
                                       WK-ARTIST-IMG
                                       WK-CHANNEL
           MOVE    ZERO        TO      WK-ARTIST-LEN
                                       WK-ARTIST-IMG-LEN
                                       WK-CHANNEL-LEN

           READ    PIN7-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
                   UNSTRING PIN7-REC
                       DELIMITED BY ","
                           INTO
                               WK-ARTIST     COUNT WK-ARTIST-LEN
                               WK-ARTIST-IMG COUNT WK-ARTIST-IMG-LEN
                               WK-CHANNEL    COUNT WK-CHANNEL-LEN
                   END-UNSTRING

                   IF      WK-PIN7-LEN =       ZERO
      *    *** 中国大陸
                        OR PIN7-REC (1:12) = X"E4B8ADE59BBDE5A4A7E999B8"
      *    *** 香港台湾
                        OR PIN7-REC (1:12) = X"E9A699E6B8AFE58FB0E6B9BE"
                           CONTINUE
                   ELSE

      *    *** TBL01 SET
                           PERFORM S082-10     THRU    S082-EX
                   END-IF
           END-READ
           .
       S080-EX.
           EXIT.

      *    *** TBL01 SET
       S082-10.

           ADD     1           TO      I
           IF      I           >       3000
                   DISPLAY WK-PGM-NAME " TBL01 OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-ARTIST    TO     TBL01-ARTIST (I)
           MOVE    WK-ARTIST-LEN    TO TBL01-ARTIST-LEN (I)
           MOVE    WK-ARTIST-IMG TO    TBL01-ARTIST-IMG (I)
           MOVE    WK-ARTIST-IMG-LEN TO TBL01-ARTIST-IMG-LEN (I)
           MOVE    WK-CHANNEL   TO     TBL01-CHANNEL (I)
           MOVE    WK-CHANNEL-LEN TO   TBL01-CHANNEL-LEN (I)
           MOVE    I           TO     I-MAX
           .
       S082-EX.
           EXIT.

      *    *** 中国大陸女性アーティスト一覧
       S100-10.

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** POT1 % 1件目 WRITE
           PERFORM S110-10     THRU    S110-EX

           IF      WK-PIN1-EOF NOT =   HIGH-VALUE
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-IF

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN1-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN1-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S120-10     THRU    S120-EX

      *    *** （ コード特殊
                       WHEN PIN1-REC (1:3) = X"28E998"
                         OR PIN1-REC (1:1) = X"28"
      *    *** WRITE POT1
                           PERFORM S130-10     THRU    S130-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT1
                   PERFORM S130-10     THRU    S130-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** POT1 % 1件目 WRITE
       S110-10.

           MOVE    "% "        TO      POT1-REC (1:2)
           MOVE    PIN1-REC    TO      POT1-REC (3:WK-PIN1-LEN)
           MOVE    ","         TO      POT1-REC (WK-PIN1-LEN + 3:)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S110-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S120-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT1-REC (1:)
           MOVE    PIN1-REC (1:3) TO   POT1-REC (13:3)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S130-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT1-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT1-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT1-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT1-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT1-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT1-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT1-REC (L:3)
           END-IF

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S130-EX.
           EXIT.

      *    ***　項目クリアー
       S140-10.

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
                                       SV-TITLE
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN
                                       SV-TITLE-LEN
                                       L
           .
       S140-EX.
           EXIT.

      *    *** 香港台湾女性アーティスト一覧
       S200-10.

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** POT2 % 1件目 WRITE
           PERFORM S210-10     THRU    S210-EX

           IF      WK-PIN2-EOF NOT =   HIGH-VALUE
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-IF

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN2-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN2-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S220-10     THRU    S220-EX

      *    *** （ コード特殊
                       WHEN PIN2-REC (1:3) = X"28E998"
                         OR PIN2-REC (1:1) = X"28"
      *    *** WRITE POT2
                           PERFORM S230-10     THRU    S230-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT2
                   PERFORM S230-10     THRU    S230-EX
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** POT2 % 1件目 WRITE
       S210-10.

           MOVE    "% "        TO      POT2-REC (1:2)
           MOVE    PIN2-REC    TO      POT2-REC (3:WK-PIN2-LEN)
           MOVE    ","         TO      POT2-REC (WK-PIN2-LEN + 3:)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT
           .
       S210-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S220-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT2-REC (1:)
           MOVE    PIN2-REC (1:3) TO   POT2-REC (13:3)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           .
       S220-EX.
           EXIT.

      *    *** WRITE POT2
       S230-10.

           MOVE    SPACE       TO      POT2-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT2-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT2-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT2-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT2-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT2-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT2-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT2-REC (L:3)
           END-IF

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S230-EX.
           EXIT.

      *    *** 中国大陸男性アーティスト一覧
       S300-10.

      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

      *    *** POT3 % 1件目 WRITE
           PERFORM S310-10     THRU    S310-EX

           IF      WK-PIN3-EOF NOT =   HIGH-VALUE
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-IF

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN3-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN3-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S320-10     THRU    S320-EX

      *    *** （ コード特殊
                       WHEN PIN3-REC (1:3) = X"28E998"
                         OR PIN3-REC (1:1) = X"28"
      *    *** WRITE POT3
                           PERFORM S330-10     THRU    S330-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT3
                   PERFORM S330-10     THRU    S330-EX
           END-IF
           .
       S300-EX.
           EXIT.

      *    *** POT3 % 1件目 WRITE
       S310-10.

           MOVE    "% "        TO      POT3-REC (1:2)
           MOVE    PIN3-REC    TO      POT3-REC (3:WK-PIN3-LEN)
           MOVE    ","         TO      POT3-REC (WK-PIN3-LEN + 3:)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT
           .
       S310-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S320-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT3-REC (1:)
           MOVE    PIN3-REC (1:3) TO   POT3-REC (13:3)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           .
       S320-EX.
           EXIT.

      *    *** WRITE POT3
       S330-10.

           MOVE    SPACE       TO      POT3-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT3-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT3-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT3-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT3-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT3-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT3-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT3-REC (L:3)
           END-IF

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S330-EX.
           EXIT.

      *    *** 香港台湾男性アーティスト一覧
       S400-10.

      *    *** READ PIN4
           PERFORM S050-10     THRU    S050-EX

      *    *** POT4 % 1件目 WRITE
           PERFORM S410-10     THRU    S410-EX

           IF      WK-PIN4-EOF NOT =   HIGH-VALUE
      *    *** READ PIN4
                   PERFORM S050-10     THRU    S050-EX
           END-IF

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN4-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN4-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S420-10     THRU    S420-EX

      *    *** （ コード特殊
                       WHEN PIN4-REC (1:3) = X"28E998"
                         OR PIN4-REC (1:1) = X"28"
      *    *** WRITE POT4
                           PERFORM S430-10     THRU    S430-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN4
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT4
                   PERFORM S430-10     THRU    S430-EX
           END-IF
           .
       S400-EX.
           EXIT.

      *    *** POT4 % 1件目 WRITE
       S410-10.

           MOVE    "% "        TO      POT4-REC (1:2)
           MOVE    PIN4-REC    TO      POT4-REC (3:WK-PIN4-LEN)
           MOVE    ","         TO      POT4-REC (WK-PIN4-LEN + 3:)

           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT
           .
       S410-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S420-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT4-REC (1:)
           MOVE    PIN4-REC (1:3) TO   POT4-REC (13:3)

           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           .
       S420-EX.
           EXIT.

      *    *** WRITE POT4
       S430-10.

           MOVE    SPACE       TO      POT4-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT4-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT4-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT4-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT4-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT4-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT4-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT4-REC (L:3)
           END-IF

           WRITE   POT4-REC
           ADD     1           TO      WK-POT4-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S430-EX.
           EXIT.

      *    *** 中国大陸グループアーティスト一覧
       S500-10.

      *    *** READ PIN5
           PERFORM S060-10     THRU    S060-EX

      *    *** POT5 % 1件目 WRITE
           PERFORM S510-10     THRU    S510-EX

           IF      WK-PIN5-EOF NOT =   HIGH-VALUE
      *    *** READ PIN5
                   PERFORM S060-10     THRU    S060-EX
           END-IF

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN5-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN5-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S520-10     THRU    S520-EX

      *    *** （ コード特殊
                       WHEN PIN5-REC (1:3) = X"28E998"
                         OR PIN5-REC (1:1) = X"28"
      *    *** WRITE POT5
                           PERFORM S530-10     THRU    S530-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN5
                   PERFORM S060-10     THRU    S060-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT5
                   PERFORM S530-10     THRU    S530-EX
           END-IF
           .
       S500-EX.
           EXIT.

      *    *** POT5 % 1件目 WRITE
       S510-10.

           MOVE    "% "        TO      POT5-REC (1:2)
           MOVE    PIN5-REC    TO      POT5-REC (3:WK-PIN5-LEN)
           MOVE    ","         TO      POT5-REC (WK-PIN5-LEN + 3:)

           WRITE   POT5-REC
           ADD     1           TO      WK-POT5-CNT
           .
       S510-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S520-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT5-REC (1:)
           MOVE    PIN5-REC (1:3) TO   POT5-REC (13:3)

           WRITE   POT5-REC
           ADD     1           TO      WK-POT5-CNT

           .
       S520-EX.
           EXIT.

      *    *** WRITE POT5
       S530-10.

           MOVE    SPACE       TO      POT5-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT5-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT5-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT5-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT5-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT5-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT5-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT5-REC (L:3)
           END-IF

           WRITE   POT5-REC
           ADD     1           TO      WK-POT5-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S530-EX.
           EXIT.

      *    *** 香港台湾グループアーティスト一覧
       S600-10.

      *    *** READ PIN6
           PERFORM S070-10     THRU    S070-EX

      *    *** POT6 % 1件目 WRITE
           PERFORM S610-10     THRU    S610-EX

           IF      WK-PIN6-EOF NOT =   HIGH-VALUE
      *    *** READ PIN6
                   PERFORM S070-10     THRU    S070-EX
           END-IF

           PERFORM UNTIL WK-PIN6-EOF = HIGH-VALUE

      *    *** WK-TITLE : 歌手名（カタカナ）　又は　（歌手名：漢字）
                   MOVE    SPACE       TO      WK-TITLE
                                               WK-TITLE2
                   MOVE    ZERO        TO      WK-TITLE-LEN
                                               WK-TITLE2-LEN
                   UNSTRING PIN6-REC
      *    *** X"09" : TAB
                       DELIMITED BY X"09" OR SPACE
                           INTO
                               WK-TITLE    COUNT WK-TITLE-LEN
                               WK-TITLE2   COUNT WK-TITLE2-LEN
                   END-UNSTRING

                   EVALUATE TRUE
      *    *** 行
                       WHEN PIN6-REC (4:3) = X"E8A18C"
      *    *** ジャパリ WRITE
                           PERFORM S620-10     THRU    S620-EX

      *    *** （ コード特殊
                       WHEN PIN6-REC (1:3) = X"28E998"
                         OR PIN6-REC (1:1) = X"28"
      *    *** WRITE POT6
                           PERFORM S630-10     THRU    S630-EX
                           IF      WK-TITLE2   =       SPACE
                                   CONTINUE
                           ELSE
                                   MOVE    WK-TITLE2   TO      SV-TITLE
                                   MOVE    WK-TITLE2-LEN TO SV-TITLE-LEN
                           END-IF

                       WHEN OTHER
                           MOVE    WK-TITLE    TO      SV-TITLE
                           MOVE    WK-TITLE-LEN TO     SV-TITLE-LEN
                   END-EVALUATE

      *    *** READ PIN6
                   PERFORM S070-10     THRU    S070-EX
           END-PERFORM

           MOVE    SPACE       TO      WK-TITLE
                                       WK-TITLE2
           MOVE    ZERO        TO      WK-TITLE-LEN
                                       WK-TITLE2-LEN

           IF      SV-TITLE    NOT =   SPACE
      *    *** WRITE POT6
                   PERFORM S630-10     THRU    S630-EX
           END-IF
           .
       S600-EX.
           EXIT.

      *    *** POT6 % 1件目 WRITE
       S610-10.

           MOVE    "% "        TO      POT6-REC (1:2)
           MOVE    PIN6-REC    TO      POT6-REC (3:WK-PIN6-LEN)
           MOVE    ","         TO      POT6-REC (WK-PIN6-LEN + 3:)

           WRITE   POT6-REC
           ADD     1           TO      WK-POT6-CNT
           .
       S610-EX.
           EXIT.

      *    *** ジャパリ WRITE
       S620-10.

      *    *** ジャパリ
           MOVE    X"E382B8E383A3E38391E383AA"
                               TO      POT6-REC (1:)
           MOVE    PIN6-REC (1:3) TO   POT6-REC (13:3)

           WRITE   POT6-REC
           ADD     1           TO      WK-POT6-CNT

           .
       S620-EX.
           EXIT.

      *    *** WRITE POT6
       S630-10.

           MOVE    SPACE       TO      POT6-REC
           MOVE    1           TO      L
           MOVE    SV-TITLE    TO      POT6-REC (L:SV-TITLE-LEN)

           ADD     SV-TITLE-LEN TO     L

           MOVE    WK-TITLE    TO      POT6-REC (L:WK-TITLE-LEN)

           ADD     WK-TITLE-LEN TO     L
           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                      OR SW-SEARCH = "Y"
                   IF      TBL01-ARTIST (I) (1:TBL01-ARTIST-LEN(I))  =
      *    *** (ＸＸＸ) 括弧は１バイトとして比較
                           WK-TITLE (2:WK-TITLE-LEN - 2)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE TBL01-ARTIST-IMG (I) 
                                               TO      WK-ARTIST-IMG
                           MOVE TBL01-ARTIST-IMG-LEN (I) 
                                               TO      WK-ARTIST-IMG-LEN
                           MOVE TBL01-CHANNEL (I) 
                                               TO      WK-CHANNEL
                           MOVE TBL01-CHANNEL-LEN (I) 
                                               TO      WK-CHANNEL-LEN
                   END-IF
           END-PERFORM

           IF      SW-SEARCH   =       "Y"
                   MOVE    WK-ARTIST-IMG TO    
                           POT6-REC (L:WK-ARTIST-IMG-LEN)
                   ADD     WK-ARTIST-IMG-LEN TO L
      *    *** ( はTEST53で,になる
                   MOVE    ","         TO      POT6-REC (L:1)
                   ADD     1           TO      L
                   MOVE    WK-CHANNEL TO    
                           POT6-REC (L:WK-CHANNEL-LEN)
                   ADD     WK-CHANNEL-LEN TO   L
                   MOVE    ","         TO      POT6-REC (L:1)
           ELSE
                   MOVE    ",,,"       TO      POT6-REC (L:3)
           END-IF

           WRITE   POT6-REC
           ADD     1           TO      WK-POT6-CNT

           MOVE    SPACE       TO      SV-TITLE
           MOVE    ZERO        TO      SV-TITLE-LEN
           .
       S630-EX.
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
                   POT5-F
                   POT6-F

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
           MOVE    WK-POT5-CNT TO      WK-POT5-CNT-E
           DISPLAY WK-PGM-NAME " POT5 件数 = " WK-POT5-CNT-E
                   " (" WK-POT5-F-NAME ")"
           MOVE    WK-POT6-CNT TO      WK-POT6-CNT-E
           DISPLAY WK-PGM-NAME " POT6 件数 = " WK-POT6-CNT-E
                   " (" WK-POT6-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
