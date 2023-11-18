      *    *** C:,M: BACKUP FILE DIR SORT
      *    *** CHCP 65001 (UTF8) でDIR 取得
      *    *** C.DIR02.BAT で ＤＩＲ取得
      *    *** 
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST85.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT-F
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
      *                    SORT STATUS   WK-SIO1-STATUS.
                               STATUS   WK-SIO1-STATUS.

      *    *** DIR で取得したファイル
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** BACKUPしたファイル (M:koko-PC\...) 初回TEST87で作成
       SELECT PIO1-F           ASSIGN   WK-PIO1-F-NAME
                               STATUS   WK-PIO1-STATUS
           ORGANIZATION INDEXED
           ACCESS RANDOM
           RECORD KEY PIO1-KEY.

      *    *** BACKUP 後(M:koko-PC\...)のファイル名に変換
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** COPY .bat 作成
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** M: の時 ここにも出力
      *    *** BACKUP 後(M:koko-PC\...)のファイル名に変換
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
           03  SIO1-YMD.
             05  SIO1-YY       PIC  X(004).
             05  SIO1-MM       PIC  X(002).
             05  SIO1-DD       PIC  X(002).
           03  SIO1-HM.
             05  SIO1-HH       PIC  X(002).
             05  SIO1-MI       PIC  X(002).
      *    *** 実際はファイル名は250文字まで、パスも含めて259文字まで
           03  SIO1-FILE       PIC  X(256).
           03  SIO1-LEN        PIC  9(003).
           03  SIO1-DIR        PIC  X(003).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIO1-F
           LABEL RECORDS ARE STANDARD.
       01  PIO1-REC.
           03  PIO1-YMD.
             05  PIO1-YY       PIC  X(004).
             05  PIO1-MM       PIC  X(002).
             05  PIO1-DD       PIC  X(002).
           03  PIO1-HM.
             05  PIO1-HH       PIC  X(002).
             05  PIO1-MI       PIC  X(002).
           03  PIO1-KEY        PIC  X(256).
           03  PIO1-LEN        PIC  9(003).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-YMD.
             05  POT1-YY       PIC  X(004).
             05  POT1-MM       PIC  X(002).
             05  POT1-DD       PIC  X(002).
           03  POT1-HM.
             05  POT1-HH       PIC  X(002).
             05  POT1-MI       PIC  X(002).
           03  POT1-KEY        PIC  X(256).
           03  POT1-LEN        PIC  9(003).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03  FILLER          PIC  X(271).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST85  ".

      *    *** CHCP 65001 (UTF8) でDIR 取得
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST85.PIN1".
      *    *** (\COBOLの場所で) DIR /S /O:N > DIR.TXT
      *    *** DIR M:\koko-PC /S /O:N > DIR2.TXT
      *    *** (\kokoの場所で) DIR  /S /O:N > DIR3.TXT
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "DIR.TXT".

      *    *** (M:\koko-PCの場所で) DIR  /S > DIR2.TXT
      *    *** C.DIR02.BAT
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "DIR2.TXT".
      *    *** >TEST85 DIR2.TXT  <= DIR2.TXT 実行時
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "DIR.TXT".
           03  WK-PIO1-F-NAME.
             05                PIC  X(007) VALUE "TEST87.".
             05  WK-PIO1-F-NAME2
                               PIC  X(004) VALUE SPACE.
             05                PIC  X(021) VALUE ".POT1".
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "TEST85.SIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST85.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST85.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST85.POT3".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-SIO1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIO1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1RL-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1RT-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1WR-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIO1RW-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-DIR-CNT      BINARY-LONG SYNC VALUE ZERO.
           03  WK-OTHER-CNT    BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1RL-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1RT-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1WR-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIO1RW-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-DIR-CNT-E    PIC --,---,---,--9 VALUE ZERO.
           03  WK-OTHER-CNT-E  PIC --,---,---,--9 VALUE ZERO.

           03  WK-CHECK-CNT    PIC  9(005) VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.

           03  WK-COM.
             05  WK-COM-ID     PIC  X(008) VALUE SPACE.
             05  WK-COM-CNT-E  PIC  ----,---,--9 VALUE ZERO.
           03  WK-DIR          PIC  X(256) VALUE SPACE.
           03  WK-FILE         PIC  X(001) VALUE SPACE.
      *     03  WK-PC-ID        PIC  X(011) VALUE "M:\koko-PC\".
           03  WK-PC-ID.
             05                PIC  X(003) VALUE "M:\".
      *    *** koko or asus
             05  WK-PC-NAME    PIC  X(004) VALUE SPACE.
             05                PIC  X(004) VALUE "-PC\".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I6              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA,
           03  SW-HIT          PIC  X(001) VALUE "Y".
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-SYSTEM       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX
      *
           SORT    SIO1-F
                   ASCENDING  KEY SIO1-FILE
      *    *** READ AND RELEASE
                   INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                   OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-SEC                SECTION.
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           ACCEPT  WK-ARGUMENT-NUMBER FROM     ARGUMENT-NUMBER

      *    *** PRM1-F 指定無し（ARGUMENT-NUMBER=0）、既定値使用
      *    *** ARGUMENT-NUMBER=1 の時、PRM1-F 指定する
      *    *** ARGUMENT-NUMBER=2 の時、PRM1-F,PRM2-F の順に指定する
           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   MOVE    "DIR.txt"   TO      WK-PIN1-F-NAME
                   MOVE    "koko"      TO      WK-PC-NAME
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PIN1-F=" WK-PIN1-F-NAME
               WHEN 1
                   ACCEPT  WK-PIN1-F-NAME FROM ARGUMENT-VALUE
                   MOVE    "koko"      TO      WK-PC-NAME
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PIN1-F-NAME
               WHEN 2
                   ACCEPT  WK-PIN1-F-NAME FROM ARGUMENT-VALUE
                   ACCEPT  WK-PC-NAME     FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " ARG-2=" WK-PC-NAME
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PIN1-F 1個まで指定可"
                   DISPLAY WK-PGM-NAME " 指定無は、PIN1-F=DIR.txt"
                   STOP    RUN
           END-EVALUATE

           IF      WK-PC-NAME  =       "koko" OR "asus"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " ARG-2=koko OR asus を指定する"
                           " WK-PC-NAME=" WK-PC-NAME
                   STOP    RUN
           END-IF

           DISPLAY " "
           DISPLAY WK-PGM-NAME " WK-PC-NAME=" WK-PC-NAME

           MOVE    WK-PC-NAME  TO      WK-PIO1-F-NAME2

      *     MOVE    "N"         TO      SW-YES
      *     PERFORM UNTIL SW-YES =      "Y"
      *             DISPLAY " "
      *             DISPLAY "PIN1 FILE NAME 数字=?"

      *             DISPLAY " "
      *             DISPLAY "1.DIR.txt   C:\Users"
      *             DISPLAY " "
      *             DISPLAY "2.DIR2.txt  M:\"

      *             ACCEPT  WK-FILE
      *             IF      WK-FILE     =   "1"  OR "2"
      *                     DISPLAY " FILE NAME OK ? Y/N"
      *                     ACCEPT  SW-YES
      *             ELSE
      *                     DISPLAY " FILE NAME 1 OR 2 INPUT"
      *             END-IF
      *     END-PERFORM

      *     EVALUATE WK-FILE
      *         WHEN "1"
      *             MOVE    "DIR.txt"  TO WK-PIN1-F-NAME
      *         WHEN "2"
      *             MOVE    "DIR2.txt" TO WK-PIN1-F-NAME
      *     END-EVALUATE

      *    *** SORT-F はOPEN いらない

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    I-O         PIO1-F
           IF      WK-PIO1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIO1-F OPEN ERROR STATUS="
                           WK-PIO1-STATUS
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

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ AND RELEASE
       S100-SEC                SECTION.
       S100-10.

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                           ADD     1           TO      WK-PIN1-CNT
                           MOVE    WK-PIN1-CNT TO      WK-CHECK-CNT
                           IF      WK-CHECK-CNT =      ZERO
                               MOVE    "L"         TO   WDT-DATE-TIME-ID
                               MOVE    "READ "     TO   WK-COM-ID
                               MOVE    WK-PIN1-CNT TO   WK-COM-CNT-E
                               MOVE    WK-COM      TO   WDT-DATE-LUP-COM
                               CALL   "DATETIME" USING WDT-DATETIME-AREA
                           END-IF
                           IF      WK-PIN1-LEN >       256
                               DISPLAY WK-PGM-NAME
                                       " PIN1-F LEN ERROR =" WK-PIN1-LEN
                                       " WK-PIN1-CNT =" WK-PIN1-CNT
      *                         STOP    RUN
                           END-IF
      *    *** 編集 &  RELEASE
                           PERFORM S110-SEC    THRU    S110-EX
                   ELSE
                       IF  WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** 編集 & RELEASE
       S110-SEC                SECTION.
       S110-10.

           EVALUATE TRUE
               WHEN WK-PIN1-LEN = ZERO
                   CONTINUE
               WHEN PIN1-REC (01:18) =
                    " Volume in drive C"
                   CONTINUE
               WHEN PIN1-REC (01:18) =
                    " Volume in drive M"
                   CONTINUE
               WHEN PIN1-REC (01:21) =
                    " Volume Serial Number"
                   CONTINUE
               WHEN PIN1-REC (22:05) = "<DIR>"
                   CONTINUE
               WHEN PIN1-REC (18:07) = "File(s)"
                   CONTINUE
               WHEN PIN1-REC (19:07) = "File(s)"
                   CONTINUE
               WHEN PIN1-REC (01:24) = "     Total Files Listed:"
                   CONTINUE
               WHEN PIN1-REC (18:06) = "Dir(s)"
                   CONTINUE
               WHEN PIN1-REC (19:06) = "Dir(s)"
                   CONTINUE
               WHEN PIN1-REC (01:14) = " Directory of "
      *    *** I= 1バイト目はスペース
                   COMPUTE I  = WK-PIN1-LEN - 14
                   COMPUTE I2 = WK-PIN1-LEN - 15
                   ADD     1           TO      WK-DIR-CNT
                   MOVE    PIN1-REC (15:I) TO  WK-DIR
                   MOVE    I           TO      I3
      *    *** DIR 変換
                   PERFORM S120-SEC    THRU    S120-EX
               WHEN OTHER
      *    ***PIN1-REC (1:4):YYYY
                   IF      PIN1-REC (1:4) IS NUMERIC
                       IF  SW-HIT      =       "Y"
                           MOVE    PIN1-REC (01:04) TO SIO1-YY
                           MOVE    PIN1-REC (06:02) TO SIO1-MM
                           MOVE    PIN1-REC (09:02) TO SIO1-DD
                           MOVE    PIN1-REC (13:02) TO SIO1-HH
                           MOVE    PIN1-REC (16:02) TO SIO1-MI
                           IF      PIN1-REC (37:1) = SPACE
                               DISPLAY WK-PGM-NAME
                                   " SIO1-F PIN1-REC SPACE WK-PIN1-CNT="
                                       WK-PIN1-CNT
                               DISPLAY SIO1-FILE
      *                         STOP    RUN
                           END-IF
                           MOVE    PIN1-REC (37:) TO  SIO1-FILE (I + 1:)
                           COMPUTE I5 = I +  WK-PIN1-LEN - 36
                           IF      SIO1-FILE (256:1) NOT = SPACE
                               DISPLAY WK-PGM-NAME
                                       " SIO1-F FILE ERROR WK-PIN1-CNT="
                                       WK-PIN1-CNT
                               DISPLAY SIO1-FILE
      *                         STOP    RUN
                           END-IF
                           MOVE    I5          TO   SIO1-LEN
                           MOVE    WK-DIR (1:3) TO  SIO1-DIR (1:3)

                           MOVE    "N"         TO      SW-SYSTEM
                           PERFORM VARYING I6 FROM 37 BY 1
                                   UNTIL I6 > WK-PIN1-LEN - 1
                               IF      PIN1-REC (I6:2) =   "\."
                                   MOVE    "Y"         TO      SW-SYSTEM
                               END-IF
                           END-PERFORM
      *    *** PIN1-REC (37:1)=. FILE名先頭はSYSTEM FILE は除外する
                           IF      PIN1-REC (37:1) = "."
      *    *** TEST87.POT1.dat,TEST87.POT1.idx は除外する
                                OR PIN1-REC (37:15) = "TEST87.POT1.dat"
                                OR PIN1-REC (37:15) = "TEST87.POT1.idx"
                            OR PIN1-REC (37:20) = "TEST87.koko.POT1.dat"
                            OR PIN1-REC (37:20) = "TEST87.koko.POT1.idx"
                            OR PIN1-REC (37:20) = "TEST87.asus.POT1.dat"
                            OR PIN1-REC (37:20) = "TEST87.asus.POT1.idx"
                                OR PIN1-REC (37:07) = "DIR.txt"
                                OR PIN1-REC (37:08) = "DIR1.txt"
                                OR PIN1-REC (37:08) = "dir2.txt"
                                OR PIN1-REC (37:08) = "DIR2.txt"
                                OR PIN1-REC (37:08) = "DIR3.txt"
                                OR PIN1-REC (37:08) = "FILEDUMP"
                                OR PIN1-REC (44:08) = "FILEDUMP"
                                OR PIN1-REC (45:08) = "FILEDUMP"
                                OR SW-SYSTEM = "Y"
                               CONTINUE
      *                         DISPLAY WK-PGM-NAME " FILE-NAME SYSTEM"
      *                                 " WK-PIN1-CNT=" WK-PIN1-CNT
      *                                 " PIN1-REC=" PIN1-REC (1:80)
                           ELSE
                               RELEASE SIO1-REC
                               IF  WK-SIO1-STATUS NOT = ZERO
                                   DISPLAY WK-PGM-NAME
                                           " SIO1-F RELEASE ERROR" 
                                           " WK-SIO1-STATUS="
                                           WK-SIO1-STATUS
                                           " WK-PIN1-CNT=" WK-PIN1-CNT
                                   STOP    RUN
                               END-IF
                               ADD    1           TO      WK-SIO1RL-CNT

                               IF      WK-DIR (1:3) = "C:\"
      *    *** C:\ BACKUP CHECK TEST87.POT1 追加 POT2 COPY.BAT 作成
                                   PERFORM S130-SEC    THRU    S130-EX
                               END-IF
                           END-IF
                       ELSE
                           CONTINUE
                       END-IF
                   ELSE
                       DISPLAY WK-PGM-NAME " FILE-NAME ERROR"
                               " WK-PIN1-CNT=" WK-PIN1-CNT
                               " PIN1-REC=" PIN1-REC (1:80)
                   END-IF
           END-EVALUATE

           .
       S110-EX.
           EXIT.

      *    *** DIR 編集
       S120-SEC                SECTION.
       S120-10.

           MOVE    "Y"         TO      SW-HIT
      *    *** C:\Users\koko\OneDrive\ドキュメント\COBOL =>
      *    *** M:\koko-PC\C\koko\OneDrive\ドキュメント\COBOL
           EVALUATE TRUE

               WHEN PIN1-REC (15:11) = "M:\trashbox"
                   MOVE    "N"         TO      SW-HIT



      *    *** koko skip files
               WHEN PIN1-REC (15:21) = "C:\Users\koko\videos2"
                   MOVE    "N"         TO      SW-HIT

               WHEN PIN1-REC (15:23) = "C:\Users\koko\anaconda3"
                   MOVE    "N"         TO      SW-HIT

               WHEN PIN1-REC (15:15) = "C:\Users\koko\."
                   MOVE    "N"         TO      SW-HIT

               WHEN PIN1-REC (15:21) = "C:\Users\koko\AppData"
                   MOVE    "N"         TO      SW-HIT



      *    *** cassis skip files
               WHEN PIN1-REC (15:17) = "C:\Users\cassis\."
                   MOVE    "N"         TO      SW-HIT
               WHEN PIN1-REC (15:23) = "C:\Users\cassis\AppData"
                   MOVE    "N"         TO      SW-HIT



      *    *** yuuri skip files
               WHEN PIN1-REC (15:16) = "C:\Users\yuuri\."
                   MOVE    "N"         TO      SW-HIT

               WHEN PIN1-REC (15:22) = "C:\Users\yuuri\AppData"
                   MOVE    "N"         TO      SW-HIT

      *         WHEN PIN1-REC (15:09) = "C:\Users\"

               WHEN PIN1-REC (15:13) = "C:\Users\koko"

      *    *** I2 = WK-PIN1-LEN - 15
      *    *** WK-PC-ID = M:\koko-PC\
                   MOVE    WK-PC-ID    TO      SIO1-FILE
                   MOVE    PIN1-REC (16:I2)   
                                       TO      SIO1-FILE (12:I2)
                   MOVE    "C"         TO      SIO1-FILE (12:1)
      *    *** 最後に￥セット
                   MOVE    "\"         TO      SIO1-FILE (12 + I2:1)
                   COMPUTE I = 12 + I2

               WHEN PIN1-REC (15:15) = "C:\Users\cassis"
                   MOVE    WK-PC-ID    TO      SIO1-FILE
                   MOVE    PIN1-REC (16:I2) 
                                       TO      SIO1-FILE (12:I2)
                   MOVE    "C"         TO      SIO1-FILE (12:1)
      *    *** 最後に￥セット
                   MOVE    "\"         TO      SIO1-FILE  (12 + I2:1)
                   COMPUTE I = 12 + I2

               WHEN PIN1-REC (15:14) = "C:\Users\yuuri"
                   MOVE    WK-PC-ID    TO      SIO1-FILE
                   MOVE    PIN1-REC (16:I2)
                                       TO      SIO1-FILE (12:I2)
                   MOVE    "C"         TO      SIO1-FILE (12:1)
      *    *** 最後に￥セット
                   MOVE    "\"         TO      SIO1-FILE (12 + I2:1)
                   COMPUTE I = 12 + I2

               WHEN PIN1-REC (15:15) = "C:\Users\Public"
                   MOVE    WK-PC-ID    TO      SIO1-FILE
                   MOVE    PIN1-REC (16:I2) 
                                       TO      SIO1-FILE (12:I2)
                   MOVE    "C"         TO      SIO1-FILE (12:1)
      *    *** 最後に￥セット
                   MOVE    "\"         TO      SIO1-FILE (12 + I2:1)
                   COMPUTE I = 12 + I2

      *    *** I = WK-PIN1-LEN - 14
               WHEN PIN1-REC (15:03) = "M:\"
                    MOVE    PIN1-REC (15:I) TO SIO1-FILE
      *    *** 最後に￥セット
                    MOVE    "\"        TO      SIO1-FILE (I + 1:1)
                    ADD     1          TO      I
      *    *** I の長さ変更無
               WHEN OTHER
      *    *** Directory of C:\Users\All Users\Acronis\TrueImageHome\Logs
      *    ***  Directory of C:\Users\Default
      *    *** \All Users がある
                   MOVE    "N"         TO      SW-HIT
                   ADD     1           TO      WK-OTHER-CNT
           END-EVALUATE
           .
       S120-EX.
           EXIT.

      *    *** C:\ BACKUP CHECK TEST87.POT1 追加 POT2 XCOPY.BAT 作成
       S130-SEC                SECTION.
       S130-10.

      *    *** XXXXXXXX.CBL => XXXXXXXX.cbl
      *    *** XXXXXXXX.BAT => XXXXXXXX.bat
      *    *** XXXXXXXX.EXE => XXXXXXXX.exe

           MOVE    SIO1-FILE   TO      PIO1-KEY
           READ    PIO1-F
           END-READ

      *    *** 23:KEY NOT FOUND
           IF      WK-PIO1-STATUS =    23

      *    *** PATH SPACE 含むので、引用符"で囲む
      *             MOVE    'XCOPY /e /y "' TO  POT2-REC
                   MOVE    'XCOPY /e /y /i "' TO  POT2-REC
      *             MOVE    14          TO      P
                   MOVE    17          TO      P
                   MOVE    WK-DIR (1:I3) TO    POT2-REC (P:I3)
                   ADD     I3          TO      P
                   MOVE    "\"         TO      POT2-REC (P:1)
                   ADD     1           TO      P
      *    *** I4:FILE名　長さ
                   COMPUTE I4 = WK-PIN1-LEN - 36
                   MOVE    PIN1-REC (37:I4) TO POT2-REC (P:I4)
                   ADD     I4          TO      P
                   MOVE    '" "'       TO      POT2-REC (P:3)
                   ADD     3           TO      P
                   MOVE    SIO1-FILE   TO      POT2-REC (P:I5)
                   ADD     I5          TO      P
                   MOVE    '"'         TO      POT2-REC (P:1)
                   WRITE   POT2-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                           DISPLAY WK-PGM-NAME
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                                   " WK-PIN1-CNT=" 
                                   WK-PIN1-CNT
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT2-CNT

      *             MOVE    I5          TO      PIO1-LEN
                   MOVE    SIO1-REC    TO      PIO1-REC

      *             DISPLAY " "
      *             DISPLAY "WRITE   " PIO1-REC

                   WRITE   PIO1-REC
                   IF      WK-PIO1-STATUS NOT = ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIO1-F WRITE ERROR STATUS="
                                   WK-PIO1-STATUS
                                   " WK-PIN1-CNT=" 
                                   WK-PIN1-CNT
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-PIO1WR-CNT

           ELSE

      *    *** 00:NOT KEY INVALID
               IF      WK-PIO1-STATUS  =       ZERO
                   ADD     1           TO      WK-PIO1-CNT
      *    *** YYYYMMDDHHMM C: 変更あった時、COPY文作成する
                   IF      SIO1-REC (1:12) > PIO1-REC (1:12)

      *                 MOVE    'XCOPY /e /y "' TO  POT2-REC
                       MOVE    'XCOPY /e /y /i "' TO  POT2-REC
      *                 MOVE    14          TO      P
                       MOVE    17          TO      P
                       MOVE    WK-DIR (1:I3) TO    POT2-REC (P:I3)
                       ADD     I3          TO      P
                       MOVE    "\"         TO      POT2-REC (P:1)
                       ADD     1           TO      P
      *    *** I4:FILE名　長さ
                       COMPUTE I4 = WK-PIN1-LEN - 36
                       MOVE    PIN1-REC (37:I4) TO POT2-REC (P:I4)
                       ADD     I4          TO      P
                       MOVE    '" "'       TO      POT2-REC (P:3)
                       ADD     3           TO      P
                       MOVE    SIO1-FILE   TO      POT2-REC (P:I5)
                       ADD     I5          TO      P
                       MOVE    '"'         TO      POT2-REC (P:1)
                       WRITE   POT2-REC
                       IF      WK-POT2-STATUS NOT = ZERO
                           DISPLAY WK-PGM-NAME
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                                   " WK-PIN1-CNT=" 
                                   WK-PIN1-CNT
                           STOP    RUN
                       END-IF
                       ADD     1           TO      WK-POT2-CNT

                       MOVE    SIO1-REC (1:12) TO  PIO1-REC (1:12)

      *                 DISPLAY " "
      *                 DISPLAY "REWRITE " PIO1-REC

                       REWRITE PIO1-REC
                       IF      WK-PIO1-STATUS NOT = ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIO1-F REWRITE ERROR STATUS="
                                   WK-PIO1-STATUS
                                   " WK-PIN1-CNT=" 
                                   WK-PIN1-CNT
                           STOP    RUN
                       END-IF
                       ADD     1           TO      WK-PIO1RW-CNT
                   ELSE
                       IF      SIO1-REC (1:12) = PIO1-REC (1:12)
      *    *** YYYYMMDDHHMM C: 変更無は、COPY文作成しない
      *    *** SIO1-REC (1:12) : 今回取得 dir
      *    *** PIO1-REC (1:12) : 前回までのバックアップしたdir
                          CONTINUE
                       ELSE
      *    ***         IF      SIO1-REC (1:12) < PIO1-REC (1:12)
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME
                                   " BACKUP 分の方が更新履歴大きい"
                                   " SIO1-F SIO1-REC (1:12) <"
                                   " PIO1-REC (1:12) ERROR"
                           DISPLAY WK-PGM-NAME
                                   " SIO1-REC (1:12)=" SIO1-REC (1:12)
                                   " PIO1-REC (1:12)=" PIO1-REC (1:12)
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           DISPLAY WK-PGM-NAME
                                   " WK-DIR  =" WK-DIR   (1:70)
                           DISPLAY WK-PGM-NAME
                                   " PIN1-REC=" PIN1-REC (1:70)
                       END-IF
                   END-IF
               ELSE
                   DISPLAY WK-PGM-NAME
                           " PIO1-F READ ERROR STATUS="
                           WK-PIO1-STATUS
                           " WK-PIN1-CNT=" 
                           WK-PIN1-CNT
                   STOP    RUN
               END-IF
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.

           PERFORM UNTIL   WK-SIO1-EOF   =     HIGH-VALUE

                   RETURN  SIO1-F
                       AT  END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                       NOT AT END

                           ADD    1            TO      WK-SIO1RT-CNT

                           MOVE    SIO1-REC    TO      POT1-REC
                           WRITE   POT1-REC

                           IF      WK-POT1-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                               STOP    RUN
                           END-IF
                           ADD     1           TO      WK-POT1-CNT

                           MOVE    WK-POT1-CNT TO      WK-CHECK-CNT
                           IF      WK-CHECK-CNT =      ZERO
                               MOVE    "L"         TO   WDT-DATE-TIME-ID
                               MOVE    "WRITE"     TO   WK-COM-ID
                               MOVE    WK-POT1-CNT TO   WK-COM-CNT-E
                               MOVE    WK-COM      TO   WDT-DATE-LUP-COM
                               CALL   "DATETIME" USING WDT-DATETIME-AREA
                           END-IF

                           IF      SIO1-DIR (1:3) =   "M:\"
                               MOVE    SIO1-REC    TO      POT3-REC
                               WRITE   POT3-REC

                               IF      WK-POT3-STATUS NOT =  ZERO
                                   DISPLAY WK-PGM-NAME 
                                       " POT3-F WRITE ERROR STATUS="
                                       WK-POT3-STATUS
                                   STOP    RUN
                               END-IF
                               ADD     1           TO      WK-POT3-CNT
                           END-IF
                   END-RETURN

                   IF      WK-SIO1-STATUS NOT = ZERO AND 10
                           DISPLAY WK-PGM-NAME
                                  " SIO1-F RETURN ERROR WK-SIO1-STATUS="
                                   WK-SIO1-STATUS
                           STOP    RUN
                   END-IF

           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-SEC                SECTION.
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIO1-F
           IF      WK-PIO1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIO1-F CLOSE ERROR STATUS="
                           WK-PIO1-STATUS
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

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"

           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"

           MOVE    WK-SIO1RL-CNT TO    WK-SIO1RL-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RL件数= " WK-SIO1RL-CNT-E
                   " (" WK-SIO1-F-NAME ")"
           MOVE    WK-SIO1RT-CNT TO    WK-SIO1RT-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RT件数= " WK-SIO1RT-CNT-E
                   " (" WK-SIO1-F-NAME ")"

           MOVE    WK-PIO1-CNT TO      WK-PIO1-CNT-E
           DISPLAY WK-PGM-NAME " PIO1RD件数= " WK-PIO1-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-PIO1WR-CNT TO    WK-PIO1WR-CNT-E
           DISPLAY WK-PGM-NAME " PIO1WR件数= " WK-PIO1WR-CNT-E
                   " (" WK-PIO1-F-NAME ")"
           MOVE    WK-PIO1RW-CNT TO    WK-PIO1RW-CNT-E
           DISPLAY WK-PGM-NAME " PIO1RW件数= " WK-PIO1RW-CNT-E
                   " (" WK-PIO1-F-NAME ")"

           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 件数 = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 件数 = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    WK-DIR-CNT  TO      WK-DIR-CNT-E
           DISPLAY WK-PGM-NAME " DIR  件数 = " WK-DIR-CNT-E
           MOVE    WK-OTHER-CNT TO     WK-OTHER-CNT-E
           DISPLAY WK-PGM-NAME " OTHER件数 = " WK-OTHER-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
