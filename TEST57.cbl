      *    *** アイドル大図鑑
      *    *** TEST53 入力データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST57.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** アイドル大図鑑 データ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** フリガナ データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** グループ名、ＨＴＴＰ、ＩＭＧ データ
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** タレント名、ＩＭＧ データ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    ***  アイドル大図鑑データ フリガナセット後
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
           03  FILLER          PIC  X(100).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN4-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03  FILLER          PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(500).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST57  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
               "TEST57_idol_daizukan.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST55.PIN2".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST83.POT1".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST57.PIN4".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST57.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNTY    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNTN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNTY-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNTN-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-TITLE        PIC  X(100) VALUE SPACE.
           03  WK-FILE         PIC  X(002) VALUE ZERO.
           03  WK-NO           PIC  9(002) VALUE ZERO.
           03  WK-SEX          PIC  9(001) VALUE ZERO.
           03  WK-KAKKO        PIC  9(002) VALUE ZERO.
           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-FURIGANA     PIC  X(030) VALUE SPACE.
           03  WK-FURIGANA-LEN BINARY-LONG SYNC VALUE ZERO.

           03  WK-P1-I1        PIC  X(030) VALUE SPACE.
           03  WK-P1-I2        PIC  X(030) VALUE SPACE.
           03  WK-P1-I3        PIC  X(020) VALUE SPACE.

           03  WK-P1-I1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-P1-I2-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-P1-I3-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-I1           PIC  X(020) VALUE SPACE.
           03  WK-I2           PIC  X(020) VALUE SPACE.
           03  WK-I3           PIC  X(020) VALUE SPACE.
           03  WK-I4           PIC  X(020) VALUE SPACE.
           03  WK-I5           PIC  X(030) VALUE SPACE.

           03  WK-I1-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I2-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I3-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I4-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-I5-LEN       BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-HEX2.
               07              PIC  X(001) VALUE LOW-VALUE.
               07  WK-HEX3     PIC  X(002) VALUE LOW-VALUE.
           03  WK-HEX-SU       REDEFINES WK-HEX
                               PIC  9(009) COMP-X.
           03  WK-SUA          PIC  9(009) VALUE ZERO.
           03  WK-SUL          PIC  9(009) VALUE ZERO.
           03  WK-SUH          PIC  9(009) VALUE ZERO.
           03  WK-OKEY         PIC  X(003) VALUE LOW-VALUE.
           03  WK-NKEY         PIC  X(003) VALUE LOW-VALUE.

           03  WK-GROUP        PIC  X(100) VALUE SPACE.
           03  WK-GROUP2       PIC  X(200) VALUE SPACE.
           03  WK-SHIMEI       PIC  X(100) VALUE SPACE.
           03  WK-HTTP         PIC  X(200) VALUE SPACE.
           03  WK-IMG          PIC  X(200) VALUE SPACE.
           03  WK-IMG2         PIC  X(200) VALUE SPACE.

           03  WK-GROUP-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-GROUP2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-SHIMEI-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTP-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HENKAN-MAE   PIC  X(100) VALUE SPACE.
           03  WK-HENKAN-ATO   PIC  X(200) VALUE SPACE.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I3-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I5-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I6              BINARY-LONG SYNC VALUE ZERO.
           03  I6-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  M2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-DO           PIC  X(001) VALUE "N".
           03  SW-WRITE        PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  SV-AREA.
           03  SV-I            BINARY-LONG SYNC VALUE ZERO.
           03  SV-I6           BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** 苗字1桁用
           03  TBL01-AREA      OCCURS 1000.
             05  TBL01-MYOJI   PIC  X(030) VALUE SPACE.
             05  TBL01-MYOJI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-FURIGANA PIC  X(030) VALUE SPACE.
             05  TBL01-FURIGANA-LEN BINARY-LONG SYNC VALUE ZERO.

      *    *** 苗字2桁用
           03  TBL02-AREA      OCCURS 12000.
             05  TBL02-MYOJI   PIC  X(030) VALUE SPACE.
             05  TBL02-MYOJI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-FURIGANA PIC  X(030) VALUE SPACE.
             05  TBL02-FURIGANA-LEN BINARY-LONG SYNC VALUE ZERO.

      *    *** 苗字3桁以上用
           03  TBL03-AREA      OCCURS 1000.
             05  TBL03-MYOJI   PIC  X(030) VALUE SPACE.
             05  TBL03-MYOJI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL03-FURIGANA PIC  X(030) VALUE SPACE.
             05  TBL03-FURIGANA-LEN BINARY-LONG SYNC VALUE ZERO.

      *    *** 氏名用
           03  TBL05-AREA      OCCURS 1000.
             05  TBL05-SHIMEI  PIC  X(030) VALUE SPACE.
             05  TBL05-SHIMEI-LEN BINARY-LONG SYNC VALUE ZERO.

      *    *** TEST83.POT1
           03  TBL06-AREA      OCCURS 200.
             05  TBL06-GROUP2  PIC  X(200) VALUE SPACE.
             05  TBL06-HTTP    PIC  X(200) VALUE SPACE.
             05  TBL06-IMG     PIC  X(200) VALUE SPACE.
             05  TBL06-GROUP2-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL06-HTTP-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL06-IMG-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** タレント名、ＩＭＧ
           03  TBL07-AREA      OCCURS 1000.
             05  TBL07-SHIMEI  PIC  X(200) VALUE SPACE.
             05  TBL07-IMG2    PIC  X(200) VALUE SPACE.
             05  TBL07-SHIMEI-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL07-IMG2-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** ここに、01 LEVELで TBL04-IDX-MAX 記述すると、異常終了する
      *    *** TABLE SORT は　独立した ０１レベルに記述しないと異常終了する
       01  TBL04-IDX-MAX       BINARY-LONG SYNC VALUE ZERO.
       01  TBL-AREA2.
      *    *** 名前ＳＯＲＴ用用
           03  TBL04-AREA      OCCURS 1 TO 3000
                               DEPENDING ON TBL04-IDX-MAX
                               ASCENDING KEY IS TBL04-FURIGANA
                               INDEXED BY TBL04-IDX.
      *    *** KEY ふりがな　又は誕生日(YYYY-MM-DD)
             05  TBL04-FURIGANA PIC X(030) VALUE HIGH-VALUE.
             05  TBL04-REC     PIC  X(500) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1 ヘッダー入力
           PERFORM S020-10     THRU    S020-EX

      *    *** ヘッダー出力
           PERFORM S160-10     THRU    S160-EX

      *    *** READ PIN1 区切り入力
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX



           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL01 SET
                   PERFORM S032-10     THRU    S032-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *     DISPLAY WK-PGM-NAME " TBL01 I1-MAX=" I1-MAX
      *     DISPLAY WK-PGM-NAME " TBL02 I2-MAX=" I2-MAX
      *     DISPLAY WK-PGM-NAME " TBL03 I3-MAX=" I3-MAX



      *    *** READ PIN3
           PERFORM S040-10     THRU    S040-EX

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE
      *    *** PIN3 TBL SET
                   PERFORM S042-10     THRU    S042-EX
      *    *** READ PIN3
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM



      *    *** READ PIN4
           PERFORM S050-10     THRU    S050-EX

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE
      *    *** PIN4 TBL SET
                   PERFORM S052-10     THRU    S052-EX
      *    *** READ PIN3
                   PERFORM S050-10     THRU    S050-EX
           END-PERFORM



           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1 グループ入力
                   PERFORM S020-10     THRU    S020-EX

      *    *** グループ出力
                   IF      SW-WRITE    =       "G"
                           PERFORM S170-10     THRU    S170-EX
                   END-IF
                   MOVE    WK-PIN1-LEN TO      WK-GROUP-LEN
                   MOVE    PIN1-REC    TO      WK-GROUP

      *    *** READ PIN1 メンバー入力
                   PERFORM S020-10     THRU    S020-EX

      *    *** TBL05 に PIN1　氏名セット
                   PERFORM S150-10     THRU    S150-EX

                   PERFORM VARYING I5 FROM 1 BY 1
                           UNTIL I5 > I5-MAX
      *    *** 苗字 フリガナ チェック
                           PERFORM S110-10     THRU    S110-EX
                   END-PERFORM

      *    *** READ PIN1 区切り入力 最終の区切りは無い
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           IF      SW-WRITE    =       "N"
      *    *** TBL04 SORT
                   SORT    TBL04-AREA
                           ASCENDING KEY TBL04-FURIGANA

      *    *** WRITE POT1 名前順
                   PERFORM S140-10     THRU    S140-EX
           END-IF

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

           OPEN    INPUT       PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F OPEN ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY "出力順 N,B 入力"
                   DISPLAY " "
                   DISPLAY "N.名前順"
                   DISPLAY "G.グループ名順"
                   ACCEPT  SW-WRITE
                   IF      SW-WRITE         =   "N" OR "G"
                           DISPLAY "出力順 OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " N,G INPUT"
                   END-IF
           END-PERFORM

           SET     TBL04-IDX   TO      1

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
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

           MOVE    SPACE       TO      WK-I1
                                       WK-I2
                                       WK-I3
                                       WK-I4
                                       WK-I5
           MOVE    ZERO        TO      WK-I1-LEN
                                       WK-I2-LEN
                                       WK-I3-LEN
                                       WK-I4-LEN
                                       WK-I5-LEN

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT

                   UNSTRING PIN2-REC
                           DELIMITED BY X"09"
                           INTO
                           WK-I1    COUNT WK-I1-LEN
                           WK-I2    COUNT WK-I2-LEN
                           WK-I3    COUNT WK-I3-LEN
                           WK-I4    COUNT WK-I4-LEN
                           WK-I5    COUNT WK-I5-LEN
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

      *    *** TBL01 SET
       S032-10.

           EVALUATE TRUE
               WHEN WK-I2-LEN = 3
                   ADD     1           TO      I1
                   IF      I1          >       1000
                           DISPLAY WK-PGM-NAME " TBL01 OVER I1=" I1
                           STOP    RUN
                   END-IF

                   MOVE    WK-I2       TO      TBL01-MYOJI      (I1)
                   MOVE    WK-I2-LEN   TO      TBL01-MYOJI-LEN  (I1)
                   MOVE    WK-I5       TO      TBL01-FURIGANA   (I1)
                   MOVE    WK-I5-LEN   TO      TBL01-FURIGANA-LEN(I1)
                   MOVE    I1          TO      I1-MAX
               WHEN WK-I2-LEN = 6
                   ADD     1           TO      I2
                   IF      I2          >       12000
                           DISPLAY WK-PGM-NAME " TBL02 OVER I2=" I2
                           STOP    RUN
                   END-IF

                   MOVE    WK-I2       TO      TBL02-MYOJI      (I2)
                   MOVE    WK-I2-LEN   TO      TBL02-MYOJI-LEN  (I2)
                   MOVE    WK-I5       TO      TBL02-FURIGANA   (I2)
                   MOVE    WK-I5-LEN   TO      TBL02-FURIGANA-LEN(I2)
                   MOVE    I2          TO      I2-MAX
               WHEN OTHER
                   ADD     1           TO      I3
                   IF      I3          >       1000
                           DISPLAY WK-PGM-NAME " TBL03 OVER I3=" I3
                           STOP    RUN
                   END-IF

                   MOVE    WK-I2       TO      TBL03-MYOJI      (I3)
                   MOVE    WK-I2-LEN   TO      TBL03-MYOJI-LEN  (I3)
                   MOVE    WK-I5       TO      TBL03-FURIGANA   (I3)
                   MOVE    WK-I5-LEN   TO      TBL03-FURIGANA-LEN(I3)
                   MOVE    I3          TO      I3-MAX
           END-EVALUATE
           .
       S032-EX.
           EXIT.

      *    *** READ PIN3
       S040-10.

           READ    PIN3-F

           MOVE    SPACE       TO      WK-GROUP2
                                       WK-HTTP
                                       WK-IMG
           MOVE    ZERO        TO      WK-GROUP2-LEN
                                       WK-HTTP-LEN
                                       WK-IMG-LEN

           IF      WK-PIN3-STATUS =    ZERO
                   ADD     1           TO      WK-PIN3-CNT

                   UNSTRING PIN3-REC
                           DELIMITED BY ","
                           INTO
                           WK-GROUP2   COUNT WK-GROUP2-LEN
                           WK-HTTP     COUNT WK-HTTP-LEN
                           WK-IMG      COUNT WK-IMG-LEN
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

           ADD     1           TO      I
           IF      I           >       200
                   DISPLAY WK-PGM-NAME
                           " TBL06-TBL OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-GROUP2   TO      TBL06-GROUP2 (I)
           MOVE    WK-HTTP     TO      TBL06-HTTP   (I)
           MOVE    WK-IMG       TO     TBL06-IMG    (I)
           MOVE    WK-GROUP2-LEN TO    TBL06-GROUP2-LEN (I)
           MOVE    WK-HTTP-LEN TO      TBL06-HTTP-LEN   (I)
           MOVE    WK-IMG-LEN  TO      TBL06-IMG-LEN    (I)
           MOVE    I           TO      I-MAX
           .
       S042-EX.
           EXIT.

      *    *** READ PIN4
       S050-10.

           READ    PIN4-F

           MOVE    SPACE       TO      WK-SHIMEI
                                       WK-IMG2
           MOVE    ZERO        TO      WK-SHIMEI-LEN
                                       WK-IMG2-LEN

           IF      WK-PIN4-STATUS =    ZERO
                   ADD     1           TO      WK-PIN4-CNT

                   UNSTRING PIN4-REC
                           DELIMITED BY ","
                           INTO
                           WK-SHIMEI   COUNT WK-SHIMEI-LEN
                           WK-IMG2     COUNT WK-IMG2-LEN
           ELSE
               IF  WK-PIN4-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN4-F READ ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S050-EX.
           EXIT.

      *    *** PIN4 TBL SET
       S052-10.

           ADD     1           TO      I6
           IF      I6          >       200
                   DISPLAY WK-PGM-NAME
                           " TBL07-TBL OVER I6=" I6
                   STOP    RUN
           END-IF

           MOVE    WK-SHIMEI   TO      TBL07-SHIMEI     (I6)
           MOVE    WK-IMG2     TO      TBL07-IMG2       (I6)
           MOVE    WK-SHIMEI-LEN TO    TBL07-SHIMEI-LEN (I6)
           MOVE    WK-IMG2-LEN TO      TBL07-IMG2-LEN   (I6)
           MOVE    I6          TO      I6-MAX
           .
       S052-EX.
           EXIT.

      *    *** 苗字 フリガナ チェック
       S110-10.

      *    *** ひらがな
           IF    ( TBL05-SHIMEI (I5) (1:3) >=   X"E38181"
                                         AND <= X"E38296" )
             AND ( TBL05-SHIMEI (I5) (4:3) >=   X"E38181"
                                         AND <= X"E38296" )
                   MOVE    6           TO      WK-FURIGANA-LEN
                   MOVE    TBL05-SHIMEI(I5) (1:6) TO   WK-FURIGANA (1:6)
      *    *** POT1-REC SET SW-SET=Y
                   PERFORM S120-10     THRU    S120-EX
                   EXIT    PARAGRAPH
           END-IF

      *    *** カタカナ
           IF    ( TBL05-SHIMEI (I5) (1:3) >=   X"E38281"
                                         AND <= X"E383B6" )
             AND ( TBL05-SHIMEI (I5) (4:3) >=   X"E38281"
                                         AND <= X"E383B6" )
                   MOVE    6           TO      WK-FURIGANA-LEN
                   MOVE    TBL05-SHIMEI (I5) (1:3) TO   WK-HEX2
                   IF   ( TBL05-SHIMEI (I5) (1:3) >= X"E38380" 
                                              AND <= X"E3838F" )
                     OR
                        ( TBL05-SHIMEI (I5) (1:3) >= X"E38390" 
                                              AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      WK-FURIGANA (1:3)

                   MOVE    TBL05-SHIMEI (I5) (4:3) TO   WK-HEX2
                   IF    ( TBL05-SHIMEI (I5) (4:3) >= X"E38380"
                                               AND <= X"E3838F" )
                     OR
                         ( TBL05-SHIMEI (I5) (4:3) >= X"E38390"
                                               AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      WK-FURIGANA (4:3)
      *    *** POT1-REC SET SW-SET=Y
                   PERFORM S120-10     THRU    S120-EX
                   EXIT    PARAGRAPH
           END-IF

      *    *** カタカナ
           IF    (TBL05-SHIMEI (I5) (1:3) >= X"E38281" AND <= X"E383B6")
      *    *** ー
             AND ( TBL05-SHIMEI (I5) (4:3) =  X"E383BC" )
                   MOVE    6           TO      WK-FURIGANA-LEN
                   MOVE    TBL05-SHIMEI (I5) (1:3) TO   WK-HEX2
                   IF   ( TBL05-SHIMEI (I5) (1:3) >= X"E38380"
                                              AND <= X"E3838F" )
                     OR
                        ( TBL05-SHIMEI (I5) (1:3) >= X"E38390"
                                              AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      WK-FURIGANA (1:3)
                   MOVE    TBL05-SHIMEI(I5) (4:3) TO   WK-FURIGANA (4:3)
      *    *** POT1-REC SET SW-SET=Y
                   PERFORM S120-10     THRU    S120-EX
                   EXIT    PARAGRAPH
           END-IF

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING I3 FROM 1 BY 1
                   UNTIL I3 > I3-MAX 
                      OR SW-SET = "Y"

                   MOVE    TBL03-MYOJI-LEN (I3) TO L
                   IF   TBL05-SHIMEI (I5) (1:L) = TBL03-MYOJI (I3) (1:L)
                           MOVE    "Y"         TO      SW-SET
                           MOVE    TBL03-FURIGANA-LEN (I3)
                                               TO       WK-FURIGANA-LEN
                                                        L
                           MOVE    TBL03-FURIGANA (I3) (1:L)
                                               TO       WK-FURIGANA
      *    *** POT1-REC SET SW-SET=Y
                           PERFORM S120-10     THRU    S120-EX
                           EXIT    PARAGRAPH
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > I2-MAX 
                      OR SW-SET = "Y"

                   MOVE    TBL02-MYOJI-LEN (I2) TO L
                   IF   TBL05-SHIMEI (I5) (1:L) = TBL02-MYOJI (I2) (1:L)
                           MOVE    "Y"         TO      SW-SET
                           MOVE    TBL02-FURIGANA-LEN (I2)
                                               TO       WK-FURIGANA-LEN
                                                        L
                           MOVE    TBL02-FURIGANA (I2) (1:L)
                                               TO       WK-FURIGANA
      *    *** POT1-REC SET SW-SET=Y
                           PERFORM S120-10     THRU    S120-EX
                           EXIT    PARAGRAPH
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           PERFORM VARYING I1 FROM 1 BY 1
                   UNTIL I1 > I1-MAX 
                      OR SW-SET = "Y"

                   MOVE    TBL01-MYOJI-LEN (I1) TO L
                   IF   TBL05-SHIMEI (I5) (1:L) = TBL01-MYOJI (I1) (1:L)
                           MOVE    "Y"         TO      SW-SET
                           MOVE    TBL01-FURIGANA-LEN (I1)
                                               TO       WK-FURIGANA-LEN
                                                        L
                           MOVE    TBL01-FURIGANA (I1) (1:L)
                                               TO       WK-FURIGANA
      *    *** POT1-REC SET SW-SET=Y
                           PERFORM S120-10     THRU    S120-EX
                           EXIT    PARAGRAPH
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

      *    *** POT1-REC SET SW-SET=N
           PERFORM S130-10     THRU    S130-EX
           .
       S110-EX.
           EXIT.

      *    *** POT1-REC SET SW-SET=Y
       S120-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      J3
      *    *** 名前（漢字）
           MOVE    TBL05-SHIMEI-LEN (I5) TO L
           MOVE    TBL05-SHIMEI (I5) (1:L) TO   POT1-REC (J3:L)
           ADD     L           TO      J3

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I6 FROM 1 BY 1
                   UNTIL I6 > I6-MAX
                       OR SW-SEARCH = "Y"
                   IF      TBL05-SHIMEI (I5) (1:L)
                         = TBL07-SHIMEI (I6) (1:L)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE    I6          TO      SV-I6
                   END-IF
           END-PERFORM
           MOVE    SV-I6       TO      I6

      *     MOVE    "CHANGE"    TO      WDE05-ID
      *     MOVE    WK-HENKAN   TO      WDE05-HENKAN
      *     MOVE    WK-MODE     TO      WDE05-MODE
      *     MOVE    100         TO      WDE05-BUF1-LEN
      *     MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
      *     CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
      *                                 TBL05-SHIMEI (I5) (1:L)
      *                                 WK-HENKAN-ATO

      *     IF  WK-PIN1-CNT < 20
      *         DISPLAY "SW-SEARCH=" SW-SEARCH " L=" L "I6=" I6 
      *             " SV-I6=" SV-I6
      *             " TBL05-SHIMEI (I5) (1:L)=" WK-HENKAN-ATO (1:30)
      *     END-IF

      *    *** SW-WRITE = "N" 名前順
           IF      SW-WRITE      =       "N"
               IF      SW-SEARCH       =       "N"
                   MOVE    " , , ,"    TO      POT1-REC (J3:6)
                   ADD     6           TO      J3
                   MOVE    WK-GROUP-LEN TO     L
                   MOVE    WK-GROUP    TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
               ELSE
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
                   MOVE    TBL07-IMG2-LEN (I6)  
                                       TO      L
                   MOVE    TBL07-IMG2 (I6) (1:L) 
                                       TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
                   MOVE    WK-GROUP-LEN TO     L
                   MOVE    WK-GROUP    TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
               END-IF
           ELSE
               IF      SW-SEARCH       =       "N"
                   MOVE    " , , , ,"  TO      POT1-REC (J3:8)
                   ADD     8           TO      J3
               ELSE
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
                   MOVE    TBL07-IMG2-LEN (I6)  
                                       TO      L
                   MOVE    TBL07-IMG2 (I6) (1:L) 
                                       TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
               END-IF
           END-IF

      *    *** 名前（ひらがな）
           MOVE    WK-FURIGANA-LEN TO     L
           MOVE    WK-FURIGANA (1:L) TO   POT1-REC (J3:L)
           ADD     L           TO      J3

           MOVE    ","         TO      POT1-REC (J3:1)
           ADD     1           TO      J3

      *    *** SW-WRITE = "N" 名前順
           IF      SW-WRITE      =       "N"
                   MOVE    WK-FURIGANA TO      TBL04-FURIGANA(TBL04-IDX)
                   MOVE    POT1-REC    TO      TBL04-REC (TBL04-IDX)

                   ADD     1           TO      WK-POT1-CNTY
                   ADD     1           TO      TBL04-IDX-MAX
                   SET     TBL04-IDX   UP  BY  1
           ELSE
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           IF      TBL04-IDX   >       3000
                   DISPLAY WK-PGM-NAME 
                           " TBL04 OVER TBL04-IDX=" TBL04-IDX
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** POT1-REC SET SW-SET=N
       S130-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      J3
      *    *** 名前（漢字）
           MOVE    TBL05-SHIMEI-LEN (I5) TO L
           MOVE    TBL05-SHIMEI (I5) (1:L) TO   POT1-REC (J3:L)
           ADD     L           TO      J3

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I6 FROM 1 BY 1
                   UNTIL I6 > I6-MAX
                       OR SW-SEARCH = "Y"
                   IF      TBL05-SHIMEI (I5) (1:L) 
                         = TBL07-SHIMEI (I6) (1:L)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE    I6          TO      SV-I6
                   END-IF
           END-PERFORM
           MOVE    SV-I6       TO      I6

      *    *** SW-WRITE = "N" 名前順
           IF      SW-WRITE      =       "N"
               IF      SW-SEARCH       =       "N"
                   MOVE    " , , ,"    TO      POT1-REC (J3:6)
                   ADD     6           TO      J3
                   MOVE    WK-GROUP-LEN TO     L
                   MOVE    WK-GROUP    TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
               ELSE
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
                   MOVE    TBL07-IMG2-LEN (I6)  
                                       TO      L
                   MOVE    TBL07-IMG2 (I6) (1:L) 
                                       TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
                   MOVE    WK-GROUP-LEN TO     L
                   MOVE    WK-GROUP    TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " ,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
               END-IF
           ELSE
               IF      SW-SEARCH       =       "N"
                   MOVE    " , , , ,"  TO      POT1-REC (J3:8)
                   ADD     8           TO      J3
               ELSE
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
                   MOVE    TBL07-IMG2-LEN (I6)  
                                       TO      L
                   MOVE    TBL07-IMG2 (I6) (1:L) 
                                       TO      POT1-REC (J3:L)
                   ADD     L           TO      J3
                   MOVE    " , ,"      TO      POT1-REC (J3:4)
                   ADD     4           TO      J3
               END-IF
           END-IF

      *    *** 名前（ひらがな）
      *     MOVE    WK-FURIGANA-LEN TO     L
      *     MOVE    WK-FURIGANA (1:L) TO   POT1-REC (J3:L)
      *     ADD     L           TO      J3

           MOVE    " ,"        TO      POT1-REC (J3:2)
           ADD     2           TO      J3

      *    *** SW-WRITE = "N" 名前順
           IF      SW-WRITE    =       "N"
      *    *** 苗字ＨＩＴしなかったので、SPACEをセット
                   MOVE    SPACE       TO      TBL04-FURIGANA(TBL04-IDX)
                   MOVE    POT1-REC    TO      TBL04-REC (TBL04-IDX)

                   MOVE    "P"         TO      WFD-ID
                   MOVE    "UTF8"      TO      WFD-KANJI
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               POT1-REC

                   ADD     1           TO      WK-POT1-CNTN
                   ADD     1           TO      TBL04-IDX-MAX
                   SET     TBL04-IDX   UP  BY  1
           ELSE
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           IF      TBL04-IDX   >       3000
                   DISPLAY WK-PGM-NAME 
                           " TBL04 OVER TBL04-IDX=" TBL04-IDX
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** WRITE POT1 名前順
       S140-10.

           IF      SW-DO       =       "Y"
                   MOVE    "$DO=Y"     TO      POT1-REC
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > 3000
                     OR  TBL04-FURIGANA (K) = HIGH-VALUE
               MOVE    TBL04-FURIGANA (K) TO WK-NKEY
               IF      WK-OKEY     NOT =   WK-NKEY
                   MOVE    SPACE       TO      POT1-REC
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA" TO POT1-REC(1:12)

                   MOVE    WK-NKEY     TO      POT1-REC (13:3)

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
               END-IF

               WRITE   POT1-REC    FROM    TBL04-REC (K)
               IF      WK-POT1-STATUS NOT =  ZERO
                       DISPLAY WK-PGM-NAME 
                               " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
               END-IF

               ADD     1           TO      WK-POT1-CNT
               MOVE    WK-NKEY     TO      WK-OKEY
           END-PERFORM
           .
       S140-EX.
           EXIT.

      *    *** TBL05 に PIN1　氏名セット
       S150-10.

           MOVE    1           TO      I5
           MOVE    ZERO        TO      M2
           MOVE    SPACE       TO      TBL05-SHIMEI     (I5)
           MOVE    ZERO        TO      TBL05-SHIMEI-LEN (I5)

           PERFORM VARYING M FROM 1 BY 1
                   UNTIL M > WK-PIN1-LEN 
                   IF      PIN1-REC (M:1) =    ","
      *                 OR  WK-PIN1-LEN =       M 
      *     DISPLAY I5 " SHIMEI=" TBL05-SHIMEI (I5) " " WK-PIN1-CNT
                           ADD     1           TO      I5
                           IF      I5          >       1000
                                   DISPLAY WK-PGM-NAME 
                                           " TBL05 OVER I5=" I5
                                   STOP    RUN
                           END-IF
                           MOVE    ZERO        TO      M2
                           MOVE    SPACE       TO
                                   TBL05-SHIMEI     (I5)
                           MOVE    ZERO        TO      
                                   TBL05-SHIMEI-LEN (I5)
                   ELSE
                           ADD     1           TO      M2
                           IF      M2          >       30
                                   DISPLAY WK-PGM-NAME 
                                           " TBL05 SHIMEI OVER M2=" M2
                                   STOP    RUN
                           END-IF
                           MOVE    PIN1-REC (M:1) TO   
                                   TBL05-SHIMEI (I5) (M2:1)
                           ADD     1           TO      
                                   TBL05-SHIMEI-LEN (I5)
                   END-IF
           END-PERFORM

           MOVE    I5          TO      I5-MAX
           .

       S150-EX.
           EXIT.

      *    *** ヘッダー出力
       S160-10.

           MOVE    PIN1-REC    TO      WK-TITLE
      *    *** TITLE WRITE %XXX...
           IF      SW-WRITE    =       "N"
      *    *** （名前順）,
                   MOVE    X"EFBC88E5908DE5898DE9A086EFBC892C"
                                       TO      WK-TITLE (24:16)
           ELSE
      *    *** （グループ名順）,
                   MOVE
                   X"EFBC88E382B0E383ABE383BCE38397E5908DE9A086EFBC892C"
                                       TO      WK-TITLE (24:25)
           END-IF
           WRITE   POT1-REC    FROM    WK-TITLE
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT
           .
       S160-EX.
           EXIT.

      *    *** グループ出力
       S170-10.

      *    *** ジャパリ
           MOVE    SPACE       TO      POT1-REC
           MOVE    X"E382B8E383A3E38391E383AA" TO POT1-REC(1:12)
           MOVE    WK-PIN1-LEN TO      L
           MOVE    PIN1-REC (1:L) TO POT1-REC(13:L)

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT

      *    *** グループ名
           MOVE    SPACE       TO      POT1-REC
           MOVE    WK-PIN1-LEN TO      L
           MOVE    1           TO      P
           MOVE    PIN1-REC (1:L) TO   POT1-REC (P:L)
           ADD     L           TO      P
           MOVE    " ,"        TO      POT1-REC (P:2)
           ADD     2           TO      P

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                       OR SW-SEARCH = "Y"
                   IF      PIN1-REC (1:L) =    TBL06-GROUP2 (I) (1:L)
                           MOVE    "Y"         TO      SW-SEARCH
                           MOVE    I           TO      SV-I
                   END-IF
           END-PERFORM
           MOVE    SV-I        TO      I

           IF      SW-SEARCH   =       "Y"
                   MOVE    TBL06-HTTP-LEN (I) TO L2
                   MOVE    TBL06-HTTP (I) (1:L2) TO  POT1-REC (P:L2)
                   ADD     L2          TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
                   ADD     2           TO      P

                   MOVE    TBL06-IMG-LEN (I) TO L2
                   MOVE    TBL06-IMG  (I) (1:L2) TO  POT1-REC (P:L2)
                   ADD     L2          TO      P

                   MOVE    " ,"        TO      POT1-REC (P:2)
           ELSE
                   MOVE    " , ,"      TO      POT1-REC (P:4)
           END-IF

           WRITE   POT1-REC
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT
           .
       S170-EX.
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

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
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
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

      *    *** SW-WRITE = "N" 名前順 の時、
      *    *** ひらがなのみ、カタカナのみ、カタカナー、TBL01,TBL02,TBL03
      *    *** (PIN2) から苗字セット出来た件数 CNTY
           DISPLAY WK-PGM-NAME " *** 名前順の時、Yｹﾝｽｳ=苗字セット出来た"
                   " Nｹﾝｽｳ=苗字セット出来無かった ***"
           MOVE    WK-POT1-CNTY TO      WK-POT1-CNTY-E
           DISPLAY WK-PGM-NAME " POT1Yｹﾝｽｳ = " WK-POT1-CNTY-E
                   " (" WK-POT1-F-NAME ")"
      *    *** (PIN2) から苗字セット出来無かった件数 CNTN
           MOVE    WK-POT1-CNTN TO      WK-POT1-CNTN-E
           DISPLAY WK-PGM-NAME " POT1Nｹﾝｽｳ = " WK-POT1-CNTN-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
