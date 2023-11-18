      *    *** 苗字、フリガナセット　ジャパリあーををセット
      *    *** 15.7% 変換出来ない（マッチングしない）
      *    *** TEST53 入力データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST55.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 苗字データ　ＵＴＦ８
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** フリガナ データ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    ***  苗字データ フリガナセット後
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(500).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(500).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(500).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST55  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST55.PIN1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE 
               "TEST53_talent_birthday.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST55.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST55.POT1".

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
           03  WK-POT1-CNTY    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNTN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNTY-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNTN-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-TITLE        PIC  X(500) VALUE SPACE.
           03  WK-FILE         PIC  X(002) VALUE ZERO.
           03  WK-NO           PIC  9(002) VALUE ZERO.
           03  WK-SEX          PIC  9(001) VALUE ZERO.
           03  WK-KAKKO        PIC  9(002) VALUE ZERO.
           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-KAZOE        PIC  9(002) VALUE ZERO.
           03  WK-FURIGANA     PIC  X(030) VALUE SPACE.
           03  WK-FURIGANA-LEN BINARY-LONG SYNC VALUE ZERO.

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
           03  WK-GEINOU.
             05                PIC  X(001) VALUE "%".
             05  WK-SEX-ID     PIC  X(001) VALUE SPACE.
      *    *** 芸能人（名前順　
             05                PIC  X(024) VALUE
               X"E88AB8E883BDE4BABAEFBC88E5908DE5898DE9A086E38080".
      *    *** 変換前 が入っているデータ
      * 01  WDE03-BUF1             PIC  X(001) ANY LENGTH.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I3-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  J3              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
      *    *** SEX=A ALL
      *    *** SEX=M 男性
      *    *** SEX=W 女性
           03  SW-SEX          PIC  X(001) VALUE "A".
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-DO           PIC  X(001) VALUE "N".
           03  SW-BIRTH        PIC  X(001) VALUE "N".

       01  TBL04-IDX-MAX       BINARY-LONG SYNC VALUE ZERO.
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

      *    *** TABLE SORT は　独立した ０１レベルに記述しないと異常終了する
       01  TBL-AREA2.
      *    *** 名前ＳＯＲＴ用用
           03  TBL04-AREA      OCCURS 1 TO 3000
                               DEPENDING ON TBL04-IDX-MAX
                               ASCENDING KEY IS TBL04-FURIGANA
                               INDEXED BY TBL04-IDX.
             05  TBL04-FURIGANA PIC  X(030) VALUE HIGH-VALUE.
             05  TBL04-REC     PIC  X(500) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
      *    *** TBL01 SET
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *     DISPLAY WK-PGM-NAME " TBL01 I1-MAX=" I1-MAX
      *     DISPLAY WK-PGM-NAME " TBL02 I2-MAX=" I2-MAX
      *     DISPLAY WK-PGM-NAME " TBL03 I3-MAX=" I3-MAX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** WRITE POT1
      *    *** 苗字 フリガナ チェック
                   PERFORM S110-10     THRU    S110-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** TBL04 SORT
           SORT    TBL04-AREA
                   ASCENDING KEY TBL04-FURIGANA

      *    *** WRITE POT1
           PERFORM S140-10     THRU    S140-EX

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
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY "出力性別 A,W,M 入力"
                   DISPLAY " "
                   DISPLAY "A.女性、男性"
                   DISPLAY "W.女性"
                   DISPLAY "M.男性"
                   ACCEPT  SW-SEX
                   IF      SW-SEX         =   "A" OR "W" OR "M"
                           DISPLAY " 性別 OK ? Y/N"
                           ACCEPT  SW-YES
                   ELSE
                           DISPLAY " A,W,M INPUT"
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

      *    *** TBL SET
       S100-10.

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
       S100-EX.
           EXIT.

      *    *** 苗字 フリガナ チェック
       S110-10.

           IF      WK-PIN1-LEN =       ZERO
                   ADD     1           TO      WK-SEX
                   EXIT    PARAGRAPH
           END-IF

      *    *** 誕生日
           IF      PIN1-REC (1:4) IS   NUMERIC
                   MOVE    ZERO        TO      WK-SEX
                   MOVE    PIN1-REC (1:4) TO   WK-YYYY
                   EXIT    PARAGRAPH
           END-IF

           IF      PIN1-REC (1:1) =    "%"
      *             MOVE    PIN1-REC    TO      WK-TITLE
                   MOVE    SW-SEX       TO      WK-SEX-ID
                   MOVE    WK-GEINOU    TO      WK-TITLE
                   IF      SW-SEX      =       "A"
                           MOVE    
      *    *** 女性・男性）,
                           X"E5A5B3E680A7E383BBE794B7E680A7EFBC892C"
                                       TO      WK-TITLE (27:19)
                   ELSE
                       IF      SW-SEX      =       "W"
                           MOVE    
      *    *** 女性）,
                           X"E5A5B3E680A7EFBC892C"
                                       TO      WK-TITLE (27:10)
                       ELSE
                           MOVE
      *    *** 男性）,
                           X"E794B7E680A7EFBC892C"
                                       TO      WK-TITLE (27:10)
                       END-IF
                   END-IF
                   EXIT    PARAGRAPH
           END-IF

      *    *** ひらがな
           IF    ( PIN1-REC (1:3) >=   X"E38181" AND <= X"E38296" )
             AND ( PIN1-REC (4:3) >=   X"E38181" AND <= X"E38296" )
                   MOVE    6           TO      WK-FURIGANA-LEN
                   MOVE    PIN1-REC (1:6) TO   WK-FURIGANA (1:6)
      *    *** POT1-REC SET SW-SET=Y
                   PERFORM S120-10     THRU    S120-EX
                   EXIT    PARAGRAPH
           END-IF

      *    *** カタカナ
           IF    ( PIN1-REC (1:3) >=   X"E38281" AND <= X"E383B6" )
             AND ( PIN1-REC (4:3) >=   X"E38281" AND <= X"E383B6" )
                   MOVE    6           TO      WK-FURIGANA-LEN
      *     DISPLAY " "
      *     CALL "COBDUMP" USING  PIN1-REC (1:3)
                   MOVE    PIN1-REC (1:3) TO   WK-HEX2
                   IF   ( PIN1-REC (1:3) >= X"E38380" AND <= X"E3838F" )
                     OR
                        ( PIN1-REC (1:3) >= X"E38390" AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      WK-FURIGANA (1:3)
      *     CALL "COBDUMP" USING  WK-FURIGANA (1:3)

                   MOVE    PIN1-REC (4:3) TO   WK-HEX2
                   IF   ( PIN1-REC (4:3) >= X"E38380" AND <= X"E3838F" )
                     OR
                        ( PIN1-REC (4:3) >= X"E38390" AND <= X"E3839F" )
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
           IF    ( PIN1-REC (1:3) >=   X"E38281" AND <= X"E383B6" )
      *    *** ー
             AND ( PIN1-REC (4:3) =    X"E383BC" )
                   MOVE    6           TO      WK-FURIGANA-LEN
                   MOVE    PIN1-REC (1:3) TO   WK-HEX2
                   IF   ( PIN1-REC (1:3) >= X"E38380" AND <= X"E3838F" )
                     OR
                        ( PIN1-REC (1:3) >= X"E38390" AND <= X"E3839F" )
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 480
                   ELSE
                       COMPUTE WK-HEX-SU = WK-HEX-SU - 288
                   END-IF
                   MOVE    WK-HEX2     TO      WK-FURIGANA (1:3)
                   MOVE    PIN1-REC (4:3) TO   WK-FURIGANA (4:3)
      *    *** POT1-REC SET SW-SET=Y
                   PERFORM S120-10     THRU    S120-EX
                   EXIT    PARAGRAPH
           END-IF

           MOVE    "N"         TO      SW-SET
           PERFORM VARYING I3 FROM 1 BY 1
                   UNTIL I3 > I3-MAX 
                      OR SW-SET = "Y"

                   MOVE    TBL03-MYOJI-LEN (I3) TO L
                   IF      PIN1-REC (1:L) =    TBL03-MYOJI (I3) (1:L)
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
                   IF      PIN1-REC (1:L) =    TBL02-MYOJI (I2) (1:L)
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
                   IF      PIN1-REC (1:L) =    TBL01-MYOJI (I1) (1:L)
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
           MOVE    "N"         TO      SW-BIRTH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-PIN1-LEN

      *    *** （
               IF      PIN1-REC (J:3) =    X"EFBC88"
      *    *** 月
                   IF    ( PIN1-REC (J + 4:3) =    X"E69C88" 
                     OR    PIN1-REC (J + 5:3) =    X"E69C88" )
                       COMPUTE J2 = J + 3
                       IF      PIN1-REC (J2:1) IS  NUMERIC
                           MOVE    ","         TO      POT1-REC (J3:1)
                           ADD     1           TO      J3
                           MOVE    WK-YYYY     TO      POT1-REC (J3:4)
                           ADD     4           TO      J3
      *    *** 年
                           MOVE    X"E5B9B4"   TO      POT1-REC (J3:3)
                           ADD     3           TO      J3
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               MOVE    PIN1-REC (J:1) TO POT1-REC (J3:1)
                               ADD     1           TO      J3
                           END-PERFORM

                           MOVE    "<br>"       TO      POT1-REC (J3:4)
                           ADD     4           TO      J3
                           COMPUTE WK-KAZOE = WDT-DATE-YYYY - WK-YYYY
                           MOVE    WK-KAZOE    TO      POT1-REC (J3:2)
                           ADD     2           TO      J3
      *    *** 歳
                           MOVE    X"E6ADB3"   TO      POT1-REC (J3:3)
                           ADD     3           TO      J3
                           MOVE    "Y"         TO      SW-BIRTH

      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                               MOVE    ","         TO    POT1-REC (J3:1)
                               ADD     1           TO      J3
                               MOVE    WK-FURIGANA-LEN TO  L
                               MOVE    WK-FURIGANA TO    POT1-REC (J3:L)
                               ADD     L           TO      J3
                               ADD     2           TO      J
                           END-IF
      *    *** ど
                           IF      WK-FURIGANA (1:3) = X"E381A9"
                                   MOVE    "Y"         TO      SW-DO
                           END-IF
                       ELSE
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               CONTINUE
                           END-PERFORM
      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                                   ADD     2           TO      J
                           END-IF
                       END-IF
                   ELSE
                       COMPUTE J2 = J + 3
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               CONTINUE
                           END-PERFORM
      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                                   ADD     2           TO      J
                           END-IF
                   END-IF
               ELSE
                       MOVE    PIN1-REC (J:1) TO   POT1-REC (J3:1)
                       ADD     1           TO      J3
               END-IF
           END-PERFORM

           IF      SW-BIRTH    =       "N"
                   MOVE    ",,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
           END-IF

           MOVE    ","         TO      POT1-REC (J3:1)
           ADD     1           TO      J3
           IF      WK-SEX      =       ZERO
      *    *** 女性
                   MOVE    X"E5A5B3E680A7" TO POT1-REC (J3:6)
                   ADD     6           TO      J3
           ELSE
      *    *** 男性
                   MOVE    X"E794B7E680A7" TO POT1-REC (J3:6)
                   ADD     6           TO      J3
           END-IF

           IF      SW-SEX      =       "A"
                   MOVE    WK-FURIGANA TO      TBL04-FURIGANA(TBL04-IDX)
                   MOVE    POT1-REC    TO      TBL04-REC (TBL04-IDX)

                   ADD     1           TO      WK-POT1-CNTY
                   ADD     1           TO      TBL04-IDX-MAX
                   SET     TBL04-IDX   UP  BY  1
           ELSE
               IF      SW-SEX      =       "W"
                 IF      WK-SEX      =       ZERO
                   MOVE    WK-FURIGANA TO      TBL04-FURIGANA(TBL04-IDX)
                   MOVE    POT1-REC    TO      TBL04-REC (TBL04-IDX)

                   ADD     1           TO      WK-POT1-CNTY
                   ADD     1           TO      TBL04-IDX-MAX
                   SET     TBL04-IDX   UP  BY  1
                 ELSE
                   CONTINUE
                 END-IF
               ELSE
                 IF      WK-SEX      >       ZERO
                   MOVE    WK-FURIGANA TO      TBL04-FURIGANA(TBL04-IDX)
                   MOVE    POT1-REC    TO      TBL04-REC (TBL04-IDX)

                   ADD     1           TO      WK-POT1-CNTY
                   ADD     1           TO      TBL04-IDX-MAX
                   SET     TBL04-IDX   UP  BY  1
                 ELSE
                   CONTINUE
                 END-IF
               END-IF
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
           MOVE    "N"         TO      SW-BIRTH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-PIN1-LEN

      *    *** （
               IF      PIN1-REC (J:3) =    X"EFBC88"
      *    *** 月
                   IF    ( PIN1-REC (J + 4:3) =    X"E69C88" 
                     OR    PIN1-REC (J + 5:3) =    X"E69C88" )
                       COMPUTE J2 = J + 3
                       IF      PIN1-REC (J2:1) IS  NUMERIC
                           MOVE    ","         TO      POT1-REC (J3:1)
                           ADD     1           TO      J3
                           MOVE    WK-YYYY     TO      POT1-REC (J3:4)
                           ADD     4           TO      J3
      *    *** 年
                           MOVE    X"E5B9B4"   TO      POT1-REC (J3:3)
                           ADD     3           TO      J3
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               MOVE    PIN1-REC (J:1) TO POT1-REC (J3:1)
                               ADD     1           TO      J3
                           END-PERFORM

                           MOVE    "<br>"      TO      POT1-REC (J3:4)
                           ADD     4           TO      J3
                           COMPUTE WK-KAZOE = WDT-DATE-YYYY - WK-YYYY
                           MOVE    WK-KAZOE    TO      POT1-REC (J3:2)
                           ADD     2           TO      J3
      *    *** 歳
                           MOVE    X"E6ADB3"   TO      POT1-REC (J3:3)
                           ADD     3           TO      J3
                           MOVE    "Y"         TO      SW-BIRTH

      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                               MOVE    ","         TO    POT1-REC (J3:1)
                               ADD     1           TO      J3
                               ADD     2           TO      J
                           END-IF
                       ELSE
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               CONTINUE
                           END-PERFORM
      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                                   ADD     2           TO      J
                           END-IF
                       END-IF
                   ELSE
                       COMPUTE J2 = J + 3
                           PERFORM VARYING J FROM J2 BY 1
                                   UNTIL J > WK-PIN1-LEN
      *    *** ）
                                   OR PIN1-REC (J:3) = X"EFBC89"
                               CONTINUE
                           END-PERFORM
      *    *** ）
                           IF      PIN1-REC (J:3) =    X"EFBC89"
                                   ADD     2           TO      J
                           END-IF
                   END-IF
               ELSE
                       MOVE    PIN1-REC (J:1) TO   POT1-REC (J3:1)
                       ADD     1           TO      J3
               END-IF
           END-PERFORM

           IF      SW-BIRTH    =       "N"
                   MOVE    ",,"        TO      POT1-REC (J3:2)
                   ADD     2           TO      J3
           END-IF

           MOVE    ","         TO      POT1-REC (J3:1)
           ADD     1           TO      J3
           IF      WK-SEX      =       ZERO
      *    *** 女性
                   MOVE    X"E5A5B3E680A7" TO POT1-REC (J3:6)
                   ADD     6           TO      J3
           ELSE
      *    *** 男性
                   MOVE    X"E794B7E680A7" TO POT1-REC (J3:6)
                   ADD     6           TO      J3
           END-IF

           IF      SW-SEX      =       "A"
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
               IF      SW-SEX      =       "W"
                 IF      WK-SEX      =       ZERO
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
                   CONTINUE
                 END-IF
               ELSE
                 IF      WK-SEX      >       ZERO
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
                   CONTINUE
                 END-IF
               END-IF
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

      *    *** WRITE POT1
       S140-10.

      *    *** TITLE WRITE %XXX...
           WRITE   POT1-REC    FROM    WK-TITLE
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME 
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT

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
           MOVE    WK-POT1-CNTY TO     WK-POT1-CNTY-E
           DISPLAY WK-PGM-NAME " POT1Yｹﾝｽｳ = " WK-POT1-CNTY-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT1-CNTN TO     WK-POT1-CNTN-E
           DISPLAY WK-PGM-NAME " POT1Nｹﾝｽｳ = " WK-POT1-CNTN-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
