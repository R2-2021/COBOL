      *    *** DLNA用 ファイルＩＤ、タイトル、作成日、取り出し
      *    *** 
      *    *** L: => F: XCOPY BAT 作成
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST136.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 抽出パラメータ（タイトルなど）
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** XXX.info 情報　(TEST135_N.POT1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** INFO、DTCP コピー用BAT作成 SJIS作成の必要あり
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(100).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(2000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST136 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST136.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST135_N.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST136.POT1.BAT".

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** XCOPY /e /y /i "L:\TVConnect_Z420\202307\1c84c9c0....dtcp" "F:\TVConnect_Z420\アニメ\1c84c9c0....dtcp"
      *    *** XCOPY /e /y /i "L:\TVConnect_Z420\202307\1c84c9c0....dtcp.info" "F:\TVConnect_Z420\アニメ\1c84c9c0....dtcp.info"
      *    *** 1c84c9c0-f05b-11f0-8000-5e9d0000489c.dtcp

           03  WK-DTCP-F1-NAME.
             05                PIC  X(015) VALUE 'XCOPY /e /y /i '.
             05                PIC  X(019) VALUE '"L:\TVConnectSuite\'.
             05  WK-DTCP-F1-YYYYMM
                               PIC  X(006) VALUE SPACE.
             05                PIC  X(001) VALUE '\'.
             05  WK-DTCP-F1-DTCP
                               PIC  X(041) VALUE 'XXX.dtcp'.
             05                PIC  X(002) VALUE '" '.
             05                PIC  X(019) VALUE '"F:\TVConnectSuite\'.
             05  WK-DTCP-F1-FORDER
                               PIC  X(100) VALUE SPACE.
             05                PIC  X(001) VALUE '"'.

           03  WK-INFO-F1-NAME.
             05                PIC  X(015) VALUE 'XCOPY /e /y /i '.
             05                PIC  X(019) VALUE '"L:\TVConnectSuite\'.
             05  WK-INFO-F1-YYYYMM
                               PIC  X(006) VALUE SPACE.
             05                PIC  X(001) VALUE '\'.
             05  WK-INFO-F1-DTCP
                               PIC  X(041) VALUE 'XXX.dtcp'.
             05                PIC  X(007) VALUE '.info" '.
             05                PIC  X(019) VALUE '"F:\TVConnectSuite\'.
             05  WK-INFO-F1-FORDER
                               PIC  X(100) VALUE SPACE.
             05                PIC  X(001) VALUE '"'.

           03  WK-DTCP-F2-NAME.
             05                PIC  X(015) VALUE 'XCOPY /e /y /i '.
             05                PIC  X(019) VALUE '"L:\TVConnect_Z420\'.
             05  WK-DTCP-F2-YYYYMM
                               PIC  X(006) VALUE SPACE.
             05                PIC  X(001) VALUE '\'.
             05  WK-DTCP-F2-DTCP
                               PIC  X(041) VALUE 'XXX.dtcp'.
             05                PIC  X(002) VALUE '" '.
             05                PIC  X(019) VALUE '"F:\TVConnect_Z420\'.
             05  WK-DTCP-F2-FORDER
                               PIC  X(100) VALUE SPACE.
             05                PIC  X(001) VALUE '"'.

           03  WK-INFO-F2-NAME.
             05                PIC  X(015) VALUE 'XCOPY /e /y /i '.
             05                PIC  X(019) VALUE '"L:\TVConnect_Z420\'.
             05  WK-INFO-F2-YYYYMM
                               PIC  X(006) VALUE SPACE.
             05                PIC  X(001) VALUE '\'.
             05  WK-INFO-F2-DTCP
                               PIC  X(041) VALUE 'XXX.dtcp'.
             05                PIC  X(007) VALUE '.info" '.
             05                PIC  X(019) VALUE '"F:\TVConnect_Z420\'.
             05  WK-INFO-F2-FORDER
                               PIC  X(100) VALUE SPACE.
             05                PIC  X(001) VALUE '"'.

      *    *** XXX.info
           03  WK-YYYYMM       PIC  X(006) VALUE SPACE.
           03  WK-DTCP         PIC  X(041) VALUE SPACE.
           03  WK-DATE         PIC  X(010) VALUE SPACE.
           03  WK-TIME         PIC  X(005) VALUE SPACE.
           03  WK-GB           PIC  X(006) VALUE SPACE.
           03  WK-TITLE        PIC  X(2000) VALUE SPACE.
           03  WK-DESC         PIC  X(2000) VALUE SPACE.
           03  WK-GENRE1       PIC  X(100) VALUE SPACE.

           03  WK-TITLE2       PIC  X(100) VALUE SPACE.
           03  WK-FORDER       PIC  X(100) VALUE SPACE.

           03  WK-TITLE2-SJIS  PIC  X(100) VALUE SPACE.
           03  WK-TITLE2-UTF8  PIC  X(100) VALUE SPACE.

           03  WK-FORDER-SJIS  PIC  X(100) VALUE SPACE.
           03  WK-FORDER-UTF8  PIC  X(100) VALUE SPACE.

           03  WK-ACCEPT       PIC  X(001) VALUE SPACE.
           03  WK-ACCEPT2      PIC  X(001) VALUE SPACE.

           03  WK-YYYYMM-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-DTCP-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-DATE-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-TIME-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-GB-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DESC-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-GENRE1-LEN   BINARY-LONG SYNC VALUE ZERO.

           03  WK-TITLE2-LEN   BINARY-LONG SYNC VALUE ZERO.
           03  WK-FORDER-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** 初期値 MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** 初期値 HENKAN=US (SJIS<=UTF8)
           03  WK-HENKAN       PIC  X(002) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  K3              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-HIT          PIC  X(001) VALUE "N".

      *    *** b3148f00-df1d-11f0-8000-649300004a92.dtcp.info
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100.
             05  TBL01-TITLE2  PIC  X(100) VALUE SPACE.
             05  TBL01-FORDER  PIC  X(100) VALUE SPACE.
             05  TBL01-TITLE2-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-FORDER-LEN BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
           PERFORM S030-10     THRU    S030-EX

           PERFORM UNTIL WK-PRM1-EOF = HIGH-VALUE
                   IF      PRM1-REC (1:18) =   
      *    *** 抽出タイトル
                   X"E68ABDE587BAE382BFE382A4E38388E383AB"
                           CONTINUE
                   ELSE
      *    *** TBL01 SET
                           PERFORM S032-10     THRU    S032-EX
                   END-IF

      *    *** READ PRM1
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-CNT = 1031

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** TBL01 SEARCH
                   PERFORM S100-10     THRU    S100-EX
                   IF      SW-HIT      =       "Y"
      *    *** WRITE POT1
                           PERFORM S110-10     THRU    S110-EX
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE,END DISPLAY
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY WK-PGM-NAME " 1:L:\TVConnectSuite\"
                   DISPLAY WK-PGM-NAME " 2:L:\TVConnect_Z420\"

                   DISPLAY WK-PGM-NAME " 1 OR 2 入力"
                   ACCEPT  WK-ACCEPT
                   IF      WK-ACCEPT   =       "1" OR "2"
                           MOVE    "Y"         TO      SW-YES
                           EVALUATE TRUE
                               WHEN WK-ACCEPT = "1"
                                   MOVE    "TEST135_1.POT1" TO
                                           WK-PIN1-F-NAME
                               WHEN WK-ACCEPT = "2"
                                   MOVE    "TEST135_2.POT1" TO
                                           WK-PIN1-F-NAME
                           END-EVALUATE
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY WK-PGM-NAME 
                           " TEST136.PRM1 抽出ファイル変更したか？"
                           " Y(y) OR N(n) 入力"
                   ACCEPT  WK-ACCEPT2
                   IF      WK-ACCEPT2  =       "Y" OR "y"
                           MOVE    "Y"         TO      SW-YES
                   END-IF
           END-PERFORM

           OPEN    INPUT       PRM1-F
                               PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-YYYYMM
                                       WK-DTCP
                                       WK-DATE
                                       WK-TIME
                                       WK-GB
                                       WK-TITLE
                                       WK-DESC
                                       WK-GENRE1
           MOVE    ZERO        TO      WK-YYYYMM-LEN
                                       WK-DTCP-LEN
                                       WK-DATE-LEN
                                       WK-TIME-LEN
                                       WK-GB-LEN
                                       WK-TITLE-LEN
                                       WK-DESC-LEN
                                       WK-GENRE1-LEN

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-YYYYMM   COUNT WK-YYYYMM-LEN
                           WK-DTCP     COUNT WK-DTCP-LEN
                           WK-DATE     COUNT WK-DATE-LEN
                           WK-TIME     COUNT WK-TIME-LEN
                           WK-GB       COUNT WK-GB-LEN
                           WK-TITLE    COUNT WK-TITLE-LEN
                           WK-DESC     COUNT WK-DESC-LEN
                           WK-GENRE1   COUNT WK-GENRE1-LEN
                   END-UNSTRING
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** READ PRM1
       S030-10.

           MOVE    SPACE       TO      WK-TITLE2
                                       WK-FORDER
           MOVE    ZERO        TO      WK-TITLE2-LEN
                                       WK-FORDER-LEN

           READ    PRM1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
               NOT AT END
                   ADD     1           TO      WK-PRM1-CNT
                   UNSTRING PRM1-REC
                           DELIMITED BY ","
                           INTO
                           WK-TITLE2   COUNT WK-TITLE2-LEN
                           WK-FORDER   COUNT WK-FORDER-LEN
                   END-UNSTRING
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** TBL01 SET
       S032-10.

           ADD     1           TO      K1
           IF      K1          >       100
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           MOVE    WK-TITLE2   TO      TBL01-TITLE2     (K1)
           MOVE    WK-TITLE2-LEN TO    TBL01-TITLE2-LEN (K1)
           MOVE    WK-FORDER   TO      TBL01-FORDER     (K1)
           MOVE    WK-FORDER-LEN TO    TBL01-FORDER-LEN (K1)
           MOVE    K1          TO      K1-MAX
           .
       S032-EX.
           EXIT.

      *    *** TBL01 SEARCH
       S100-10.

           MOVE    "N"         TO      SW-HIT


           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX
                      OR SW-HIT = "Y"
                   MOVE    TBL01-TITLE2-LEN (K1) TO    L1
                   MOVE    ZERO        TO      K2

                   PERFORM VARYING K2 FROM 1 BY 1
                           UNTIL K2 + L1 - 1 > WK-TITLE-LEN
                              OR SW-HIT = "Y"

                       IF      WK-TITLE (K2:L1)
                             = TBL01-TITLE2 (K1) (1:L1)
                               MOVE    "Y"         TO      SW-HIT
                               MOVE    WK-TITLE (K2:60) TO 
                                       WK-TITLE2-UTF8
                               MOVE    TBL01-FORDER (K1) TO 
                                       WK-FORDER-UTF8
                       END-IF
                   END-PERFORM
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
      *    *** XXX.info BAT作成
      *    *** XCOPY /e /y /i "L:\TVConnect_Z420\202307\1c84c9c0....dtcp" "F:\TVConnect_Z420\アニメ\1c84c9c0....dtcp"
      *    *** 
       S110-10.

      *    *** コード変換 SJIS<=UTF8

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    100         TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-FORDER-UTF8
                                       WK-FORDER-SJIS

           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    WK-HENKAN   TO      WDE05-HENKAN
           MOVE    WK-MODE     TO      WDE05-MODE
           MOVE    100         TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
           MOVE    WK-PIN1-CNT TO      WDE05-BUF1-CNT
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-TITLE2-UTF8
                                       WK-TITLE2-SJIS

           IF      WK-ACCEPT   =       "1"
                   MOVE    WK-YYYYMM   TO      WK-DTCP-F1-YYYYMM
                                               WK-INFO-F1-YYYYMM
                   MOVE    WK-DTCP     TO      WK-DTCP-F1-DTCP
                                               WK-INFO-F1-DTCP
                   MOVE    WK-FORDER-SJIS TO   WK-DTCP-F1-FORDER
                                               WK-INFO-F1-FORDER

                   WRITE   POT1-REC    FROM    WK-DTCP-F1-NAME
                   ADD     1           TO      WK-POT1-CNT

                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-POT1-CNT=" WK-POT1-CNT
                           " FORDER=" WK-DTCP-F1-FORDER (1:20)
                           " TITLE="  WK-TITLE2-SJIS (1:60)

                   CALL    "SYSTEM"    USING   POT1-REC

                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                   IF      RETURN-CODE NOT =   ZERO
                           DISPLAY WK-PGM-NAME " BAT XCOPY SYSTEM ERROR"
                                   " RETURN-CODE=" RETURN-CODE
                                   " WK-PIN1-CNT=" WK-PIN1-CNT " 001"
                           DISPLAY " POT1-REC=" POT1-REC (1:200)
                           STOP    RUN
                   END-IF

                   WRITE   POT1-REC    FROM    WK-INFO-F1-NAME
                   ADD     1           TO      WK-POT1-CNT

                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-POT1-CNT=" WK-POT1-CNT
                           " FORDER=" WK-INFO-F1-FORDER (1:20)
                           " TITLE="  WK-TITLE2-SJIS (1:60)

                   CALL    "SYSTEM"    USING   POT1-REC

                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                   IF      RETURN-CODE NOT =   ZERO
                           DISPLAY WK-PGM-NAME " BAT XCOPY SYSTEM ERROR"
                                   " RETURN-CODE=" RETURN-CODE
                                   " WK-PIN1-CNT=" WK-PIN1-CNT " 002"
                           DISPLAY " POT1-REC=" POT1-REC (1:200)
                           STOP    RUN
                   END-IF
           ELSE
                   MOVE    WK-YYYYMM   TO      WK-DTCP-F2-YYYYMM
                                               WK-INFO-F2-YYYYMM
                   MOVE    WK-DTCP     TO      WK-DTCP-F2-DTCP
                                               WK-INFO-F2-DTCP
                   MOVE    WK-FORDER-SJIS TO   WK-DTCP-F2-FORDER
                                               WK-INFO-F2-FORDER

                   WRITE   POT1-REC    FROM    WK-DTCP-F2-NAME
                   ADD     1           TO      WK-POT1-CNT

                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-POT1-CNT=" WK-POT1-CNT
                           " FORDER=" WK-DTCP-F2-FORDER (1:20)
                           " TITLE="  WK-TITLE2-SJIS (1:60)

                   CALL    "SYSTEM"    USING   POT1-REC

                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                   IF      RETURN-CODE NOT =   ZERO
                           DISPLAY WK-PGM-NAME " BAT XCOPY SYSTEM ERROR"
                                   " RETURN-CODE=" RETURN-CODE
                                   " WK-PIN1-CNT=" WK-PIN1-CNT " 003"
                           DISPLAY " POT1-REC=" POT1-REC (1:200)
                           STOP    RUN
                   END-IF

                   WRITE   POT1-REC    FROM    WK-INFO-F2-NAME
                   ADD     1           TO      WK-POT1-CNT

                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
                           " WK-POT1-CNT=" WK-POT1-CNT
                           " FORDER=" WK-INFO-F2-FORDER (1:20)
                           " TITLE="  WK-TITLE2-SJIS (1:60)

                   CALL    "SYSTEM"    USING   POT1-REC

                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                   IF      RETURN-CODE NOT =   ZERO
                           DISPLAY WK-PGM-NAME " BAT XCOPY SYSTEM ERROR"
                                   " RETURN-CODE=" RETURN-CODE
                                   " WK-PIN1-CNT=" WK-PIN1-CNT " 004"
                           DISPLAY " POT1-REC=" POT1-REC (1:200)
                           STOP    RUN
                   END-IF
           END-IF

           .
       S110-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 件数 = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
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
