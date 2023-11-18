      *    *** FILE コンペアー
      *    *** 入力するファイルはどちらも同じ形式の出力でないと
      *    *** PIN1,PIN2 ==> ORGANIZATION LINE   SEQUENTIALか
      *    *** PIN3,PIN4 ==> ORGANIZATION RECORD BINARY  SEQUENTIAL
      *    *** レコード件数は実際の件数とは、違くなる
      *    *** 
      *    *** BINARY FILEのコンペアーも出来るが、X"0D0A" 部分はRAED時に
      *    *** カットされる為、チェックしない
      *    *** 又、レコード長アンマッチ時は、PIN1の長さ分チェックし、
      *    *** レコード長アンマッチであった事をリストに残す
      *    *** パック項目等もチェック可能
      *    *** 
      *    *** パック項目含むファイルはORGANIZATION RECORD 
      *    *** BINARY SEQUENTIAL で出力してないと、レコード途中で
      *    *** エンドになったりする バイト、ズレしてしまう

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILECOMP.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** PIN1 (INPUT) VS PIN2 (COMP) LINE SEQUENTIAL

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PIN3 (INPUT) VS PIN4 (COMP) BINARY SEQUENTIAL

       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

      *    *** コンペアー結果プリント
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(10000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  FILLER          PIC  X(10000).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03  FILLER          PIC  X(10000).

       FD  PIN4-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03  FILLER          PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-POT1-LEN.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILECOMP".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "FILECOMP.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "FILECOMP.PIN2".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "FILECOMP.PIN3".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "FILECOMP.PIN4".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "FILECOMP.POT1".

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
      *    *** BINARY は長さ入力されない為、10000とする
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE 10000.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE 10000.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE 10000.
           03  WK-PIN3-END-BYTE PIC X(001) VALUE LOW-VALUE.
           03  WK-PIN4-END-BYTE PIC X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-0D0A-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN4-0D0A-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-L-MAX2-E     PIC --,---,---,--9 VALUE ZERO.
           03  WK-MATCH-CNT-E  PIC --,---,---,--9 VALUE ZERO.
           03  WK-COMP-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-COMP-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-MATCH-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-0D0A-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-0D0A-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-MODE         PIC  X(001) VALUE SPACE.

           03  WK-MEI1.
             05  FILLER        PIC  X(011) VALUE "ｱﾝﾏｯﾁ ｶｳﾝﾄ=".
             05  WK-MEI1-COMP-CNT  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05  FILLER        PIC  X(010) VALUE " PIN1-CNT=".
             05  WK-MEI1-PIN1-CNT  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05  FILLER        PIC  X(010) VALUE " PIN1-LEN=".
             05  WK-MEI1-PIN1-LEN  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05  FILLER        PIC  X(010) VALUE " PIN2-LEN=".
             05  WK-MEI1-PIN2-LEN  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05  FILLER        PIC  X(010) VALUE "     ｺﾒﾝﾄ=".
             05  WK-MEI1-COM   PIC  X(020) VALUE SPACE.

           03  WK-MEI2.
             05  FILLER        PIC  X(016) VALUE "ｱﾝﾏｯﾁ ﾚｺｰﾄﾞｽｳ  =".
             05  WK-MEI2-COMP-CNT  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.
             05  WK-MEI2-COM   PIC  X(014) VALUE SPACE.
             05  WK-MEI2-COM2  PIC  X(014) VALUE SPACE.

           03  WK-MEI3.
             05  FILLER        PIC  X(016) VALUE "ｱﾝﾏｯﾁ ｶﾗﾑ(ﾊﾞｲﾄ)=".
             05  WK-MEI3-COMP-BYT  PIC  ZZZ,ZZZ,ZZ9 VALUE ZERO.

       01  WK-COMP-REC         PIC  X(10000) VALUE SPACE.
       01  WK-COMP2-REC        PIC  X(10000) VALUE SPACE.
       01  WK-PIN1-REC         PIC  X(10000) VALUE SPACE.
       01  WK-PIN2-REC         PIC  X(10000) VALUE SPACE.
       01  WK-PIN3-REC         PIC  X(10000) VALUE SPACE.
       01  WK-PIN4-REC         PIC  X(10000) VALUE SPACE.
       01  WK-IN-L-REC         PIC  X(10000) VALUE SPACE.
       01  WK-IN-R-REC         PIC  X(10000) VALUE SPACE.
       01  WK-COMP-L-REC       PIC  X(10000) VALUE SPACE.
       01  WK-COMP-R-REC       PIC  X(10000) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  Left-Nibble         BINARY-LONG SYNC VALUE 0.
       01  Right-Nibble        BINARY-LONG SYNC VALUE 0.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG SYNC VALUE 1.

       01  INDEX-AREA.
           03  L-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  L-MAX2          BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEIGYO       PIC  X(001) VALUE "N".
           03  SW-COMP         PIC  X(001) VALUE "0".
           03  SW-LENG         PIC  X(001) VALUE "0".
           03  SW-YES          PIC  X(001) VALUE "Y".
           03  SW-BINARY       PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-I            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX
           IF      WK-MODE     =       "B"

      *    *** READ PIN3(インプット　バイナリーファイル)
                   PERFORM S120-10     THRU    S120-EX

      *    *** READ PIN4(コンペアー　バイナリーファイル)
                   PERFORM S130-10     THRU    S130-EX

      *    *** PIN3,PIN4 チェック
                   PERFORM S300-10     THRU    S300-EX
                          UNTIL   WK-PIN3-EOF   =     HIGH-VALUE OR
                                  WK-PIN4-EOF   =     HIGH-VALUE

      *    *** WRITE POT1 （バイナリーファイル コンペアー結果）
                   PERFORM S310-10     THRU    S310-EX
           ELSE

      *    *** READ PIN1(インプット　LINE SEQUENTIALファイル)
                   PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN2(コンペアー　LINE SEQUENTIALファイル)
                   PERFORM S110-10     THRU    S110-EX

      *    *** PIN1,PIN2 チェック
                   PERFORM S200-10     THRU    S200-EX
                          UNTIL   WK-PIN1-EOF   =     HIGH-VALUE OR
                                  WK-PIN2-EOF   =     HIGH-VALUE

      *    *** WRITE POT1 （ファイル コンペアー結果）
                   PERFORM S210-10     THRU    S210-EX
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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT LINE SEQ(L) "
                           "OR BINARY(B) ?  L OR B で入力"
                   ACCEPT  WK-MODE
                   IF      WK-MODE     =       "B"
                           DISPLAY " BINARY FILE OK ? Y/N"
                   ELSE
                           DISPLAY " LINE SEQUENTIAL OK ? Y/N"
                   END-IF
                   ACCEPT  SW-YES
           END-PERFORM

           IF      WK-MODE     =       "B"
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN3-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN3-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM

               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " COMPARE FILE NAME"
                   ACCEPT  WK-PIN4-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN4-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM
           ELSE
               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN1-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM

               MOVE    "N"         TO      SW-YES
               PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " COMPARE FILE NAME"
                   ACCEPT  WK-PIN2-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN2-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
               END-PERFORM
           END-IF

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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .

       S010-EX.
           EXIT.

      *    *** READ PIN1
       S100-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   MOVE    PIN1-REC    TO      WK-PIN1-REC
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

       S100-EX.
           EXIT.

      *    *** READ PIN2
       S110-10.

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   MOVE    PIN2-REC    TO      WK-PIN2-REC
                   ADD     1           TO      WK-PIN2-CNT
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
       S110-EX.
           EXIT.

      *    *** READ PIN3 BINARY
       S120-10.

           MOVE    HIGH-VALUE  TO      PIN3-REC
           READ    PIN3-F

           IF      WK-PIN3-STATUS =    ZERO OR 4
                   MOVE    PIN3-REC    TO      WK-PIN3-REC
                   ADD     1           TO      WK-PIN3-CNT

      *     MOVE    400         TO      WFD-LEN
      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                PIN3-REC

                   PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > 9999
                       IF      PIN3-REC(I:2) =     X"0D0A"
                           ADD     1           TO      WK-PIN3-0D0A-CNT
                       END-IF
      *    *** X"FF" 直前をレコード長とする
                       IF      PIN3-REC (I:2) =    X"FFFF"
                           COMPUTE WK-PIN3-LEN = I - 1
                           MOVE    10000       TO      I
                       END-IF
                   END-PERFORM

                   IF      WK-PIN3-END-BYTE =  X"0D" AND
                           PIN3-REC (1:1) =    X"0A"
                           ADD     1           TO      WK-PIN3-0D0A-CNT
                   END-IF
                   MOVE    PIN3-REC (10000:1) TO WK-PIN3-END-BYTE
           ELSE
               IF  WK-PIN3-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN3-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** READ PIN4 BINARY
       S130-10.

           MOVE    HIGH-VALUE  TO      PIN4-REC
           READ    PIN4-F

           IF      WK-PIN4-STATUS =    ZERO OR 4
                   MOVE    PIN4-REC    TO      WK-PIN4-REC
                   ADD     1           TO      WK-PIN4-CNT

      *     MOVE    "P"         TO      WK-ID
      *     MOVE    2           TO      WK-SU
      *     CALL    "FILEDUMP"  USING   WK-LINK
      *                                 PIN4-REC

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 9999
                       IF      PIN4-REC(I:2) =     X"0D0A"
                           ADD     1           TO      WK-PIN4-0D0A-CNT
                       END-IF
      *    *** X"FFFF" 直前をレコード長とする
                       IF      PIN4-REC (I:2) =    X"FFFF"
                           COMPUTE WK-PIN4-LEN = I - 1
                           MOVE    10000       TO      I
                       END-IF
                   END-PERFORM

                   IF      WK-PIN4-END-BYTE =  X"0D" AND
                           PIN4-REC (1:1) =    X"0A"
                           ADD     1           TO      WK-PIN4-0D0A-CNT
                   END-IF
                   MOVE    PIN4-REC (10000:1) TO WK-PIN4-END-BYTE
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
       S130-EX.
           EXIT.

      *    *** PIN1,PIN2 CHECK 
       S200-10.

           IF      WK-PIN1-LEN =       WK-PIN2-LEN
                   MOVE    WK-PIN1-LEN TO      L-MAX
           ELSE
                   MOVE    "1"         TO      SW-LENG

                   MOVE    ZERO        TO      WK-MEI1-COMP-CNT
                   MOVE    WK-PIN1-CNT TO      WK-MEI1-PIN1-CNT
                   MOVE    WK-PIN1-LEN TO      WK-MEI1-PIN1-LEN
                   MOVE    WK-PIN2-LEN TO      WK-MEI1-PIN2-LEN
                   MOVE    " ﾚﾝｸﾞｽ ｱﾝﾏｯﾁ " TO  WK-MEI1-COM
                   MOVE    150         TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-MEI1
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   IF      WK-PIN1-LEN >       WK-PIN2-LEN
                           MOVE    WK-PIN2-LEN TO      L-MAX
                   ELSE
                           MOVE    WK-PIN1-LEN TO      L-MAX
                   END-IF
           END-IF

           IF      L-MAX       >       L-MAX2
                   MOVE    L-MAX       TO      L-MAX2
           END-IF
           MOVE    ZERO        TO      SW-COMP
           MOVE    SPACE       TO      WK-COMP-REC

           IF      PIN1-REC (1:L-MAX) = PIN2-REC (1:L-MAX)
                   CONTINUE
           ELSE
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L-MAX
      *    *** BINARY の時、X"0D0A" 以外のBYTEチェック
                   IF      PIN1-REC (I:1) =    PIN2-REC (I:1)
                           CONTINUE
                   ELSE
                       IF      I           >       10000
                           DISPLAY WK-PGM-NAME " COMP-REC OVER I=" I
                           STOP    RUN
                       ELSE
                           MOVE    "?"         TO     WK-COMP-REC (I:1)
                                                      WK-COMP2-REC (I:1)
                           MOVE    "1"         TO     SW-COMP
                       END-IF
                   END-IF
                   IF    ( PIN1-REC (I:1) >=   X"00" AND
                           PIN1-REC (I:1) <=   X"1F"     ) OR
                         ( PIN1-REC (I:1) >=   X"7F" AND
                           PIN1-REC (I:1) <=   X"9F"     ) OR
                         ( PIN1-REC (I:1) >=   X"E0" AND
                           PIN1-REC (I:1) <=   X"FF"     )
                           MOVE    SPACE       TO      PIN1-REC (I:1)
                           MOVE    "Y"         TO      SW-SEIGYO
                   END-IF
                   IF    ( PIN2-REC (I:1) >=   X"00" AND
                           PIN2-REC (I:1) <=   X"1F"     ) OR
                         ( PIN2-REC (I:1) >=   X"7F" AND
                           PIN2-REC (I:1) <=   X"9F"     ) OR
                         ( PIN2-REC (I:1) >=   X"E0" AND
                           PIN2-REC (I:1) <=   X"FF"     )
                           MOVE    SPACE       TO      PIN2-REC (I:1)
                           MOVE    "Y"         TO      SW-SEIGYO
                   END-IF
               END-PERFORM
           END-IF

           IF      SW-COMP     =       "1"
                   ADD     1           TO      WK-COMP-CNT
           ELSE
                   ADD     1           TO      WK-MATCH-CNT
           END-IF

           IF      SW-COMP     =       "1"

                   MOVE    SPACE       TO      POT1-REC
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-COMP-CNT TO      WK-MEI1-COMP-CNT
                   MOVE    WK-PIN1-CNT TO      WK-MEI1-PIN1-CNT
                   MOVE    WK-PIN1-LEN TO      WK-MEI1-PIN1-LEN
                   MOVE    WK-PIN2-LEN TO      WK-MEI1-PIN2-LEN
                   MOVE    " ｱﾝﾏｯﾁ"    TO      WK-MEI1-COM
                   MOVE    150         TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-MEI1
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
                   MOVE    PIN1-REC    TO      POT1-REC
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   IF      SW-SEIGYO   =       "Y" OR "N"
                       PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > L-MAX

                               MOVE    WK-PIN1-REC (I:1) TO  PIC-X

                               DIVIDE PIC-Halfword BY 16
                                      GIVING    Left-Nibble
                                      REMAINDER Right-Nibble

                               ADD     1           TO      Left-Nibble
                                                           Right-Nibble

                               MOVE    Hex-Digit (Left-Nibble)
                                       TO      WK-IN-L-REC (I:1)
                               MOVE    Hex-Digit (Right-Nibble)
                                       TO      WK-IN-R-REC (I:1)

                               MOVE    WK-PIN2-REC (I:1) TO  PIC-X

                               DIVIDE PIC-Halfword BY 16
                                      GIVING    Left-Nibble
                                      REMAINDER Right-Nibble

                               ADD     1           TO      Left-Nibble
                                                           Right-Nibble

                               MOVE    Hex-Digit (Left-Nibble)
                                       TO      WK-COMP-L-REC (I:1)
                               MOVE    Hex-Digit (Right-Nibble)
                                       TO      WK-COMP-R-REC (I:1)
                       END-PERFORM
                   END-IF

                   MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-IN-L-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-IN-R-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    L-MAX       TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN2-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    PIN2-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN2-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-L-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN2-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-R-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN2
           PERFORM S110-10     THRU    S110-EX
           .
       S200-EX.
           EXIT.

      *    *** WRITE POT1 1
       S210-10.

           MOVE    WK-COMP-CNT TO      WK-MEI2-COMP-CNT

           IF      SW-LENG     =       "1"
                   MOVE    " ﾚﾝｸﾞｽ ｱﾝﾏｯﾁ " TO WK-MEI2-COM
           END-IF

           IF      WK-PIN1-EOF NOT =   WK-PIN2-EOF
                   MOVE    " ｹﾝｽｳ  ｱﾝﾏｯﾁ " TO WK-MEI2-COM2
           END-IF

           MOVE    150         TO      WK-POT1-LEN
           WRITE   POT1-REC    FROM    WK-MEI2
           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L-MAX
                   IF      WK-COMP2-REC (I:1) = "?"
                       MOVE    I          TO      WK-MEI3-COMP-BYT
                       MOVE    150        TO      WK-POT1-LEN
                       WRITE   POT1-REC   FROM    WK-MEI3
                       IF      WK-POT1-STATUS =    ZERO
                               ADD     1           TO      WK-POT1-CNT
                       ELSE
                               DISPLAY WK-PGM-NAME
                                       " POT1-F WRITE ERROR STATUS="
                                       WK-POT1-STATUS
                               STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S210-EX.
           EXIT.

      *    *** PIN3,PIN4 CHECK 
       S300-10.
           
           IF      WK-PIN3-LEN =       WK-PIN4-LEN
                   MOVE    WK-PIN3-LEN TO      L-MAX
           ELSE
                   MOVE    "1"         TO      SW-LENG

                   MOVE    ZERO        TO      WK-MEI1-COMP-CNT
                   MOVE    WK-PIN3-CNT TO      WK-MEI1-PIN1-CNT
                   MOVE    WK-PIN3-LEN TO      WK-MEI1-PIN1-LEN
                   MOVE    WK-PIN4-LEN TO      WK-MEI1-PIN2-LEN
                   MOVE    " ﾚﾝｸﾞｽ ｱﾝﾏｯﾁ " TO  WK-MEI1-COM
                   MOVE    150         TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-MEI1
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   IF      WK-PIN3-LEN >       WK-PIN4-LEN
                           MOVE    WK-PIN4-LEN TO      L-MAX
                   ELSE
                           MOVE    WK-PIN3-LEN TO      L-MAX
                   END-IF
           END-IF

           IF      L-MAX       >       L-MAX2
                   MOVE    L-MAX       TO      L-MAX2
           END-IF
           MOVE    ZERO        TO      SW-COMP
           MOVE    SPACE       TO      WK-COMP-REC

           IF      PIN3-REC (1:L-MAX) = PIN4-REC (1:L-MAX)
                   CONTINUE
           ELSE
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L-MAX
                   IF      PIN3-REC (I:1) =    PIN4-REC (I:1)
                           CONTINUE
                   ELSE
                       IF      I           >       10000
                           DISPLAY WK-PGM-NAME " COMP-REC OVER I=" I
                           STOP    RUN
                       ELSE
                           MOVE    "?"         TO     WK-COMP-REC (I:1)
                                                      WK-COMP2-REC (I:1)
                           MOVE    "1"         TO     SW-COMP
                       END-IF
                   END-IF
                   IF    ( PIN3-REC (I:1) >=   X"00" AND
                           PIN3-REC (I:1) <=   X"1F"     ) OR
                         ( PIN3-REC (I:1) >=   X"7F" AND
                           PIN3-REC (I:1) <=   X"9F"     ) OR
                         ( PIN3-REC (I:1) >=   X"E0" AND
                           PIN3-REC (I:1) <=   X"FF"     )
                           MOVE    SPACE       TO      PIN3-REC (I:1)
                           MOVE    "Y"         TO      SW-SEIGYO
                   END-IF
                   IF    ( PIN4-REC (I:1) >=   X"00" AND
                           PIN4-REC (I:1) <=   X"1F"     ) OR
                         ( PIN4-REC (I:1) >=   X"7F" AND
                           PIN4-REC (I:1) <=   X"9F"     ) OR
                         ( PIN4-REC (I:1) >=   X"E0" AND
                           PIN4-REC (I:1) <=   X"FF"     )
                           MOVE    SPACE       TO      PIN4-REC (I:1)
                           MOVE    "Y"         TO      SW-SEIGYO
                   END-IF
               END-PERFORM
           END-IF

           IF      SW-COMP     =       "1"
                   ADD     1           TO      WK-COMP-CNT
           ELSE
                   ADD     1           TO      WK-MATCH-CNT
           END-IF
           IF      SW-COMP     =       "1"

                   MOVE    SPACE       TO      POT1-REC
                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-COMP-CNT TO      WK-MEI1-COMP-CNT
                   MOVE    WK-PIN3-CNT TO      WK-MEI1-PIN1-CNT
                   MOVE    WK-PIN3-LEN TO      WK-MEI1-PIN1-LEN
                   MOVE    WK-PIN4-LEN TO      WK-MEI1-PIN2-LEN
                   MOVE    " ｱﾝﾏｯﾁ"    TO      WK-MEI1-COM
                   MOVE    150         TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-MEI1
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN3-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    PIN3-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   IF      SW-SEIGYO   =       "Y" OR "N"
                       PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > L-MAX

                               MOVE    WK-PIN3-REC (I:1) TO  PIC-X

                               DIVIDE PIC-Halfword BY 16
                                      GIVING    Left-Nibble
                                      REMAINDER Right-Nibble

                               ADD     1           TO      Left-Nibble
                                                           Right-Nibble

                               MOVE    Hex-Digit (Left-Nibble)
                                       TO      WK-IN-L-REC (I:1)
                               MOVE    Hex-Digit (Right-Nibble)
                                       TO      WK-IN-R-REC (I:1)

                               MOVE    WK-PIN4-REC (I:1) TO  PIC-X

                               DIVIDE PIC-Halfword BY 16
                                      GIVING    Left-Nibble
                                      REMAINDER Right-Nibble

                               ADD     1           TO      Left-Nibble
                                                           Right-Nibble

                               MOVE    Hex-Digit (Left-Nibble)
                                       TO      WK-COMP-L-REC (I:1)
                               MOVE    Hex-Digit (Right-Nibble)
                                       TO      WK-COMP-R-REC (I:1)
                       END-PERFORM
                   END-IF

                   MOVE    WK-PIN3-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-IN-L-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN3-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-IN-R-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    L-MAX       TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN4-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    PIN4-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN4-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-L-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF

                   MOVE    WK-PIN4-LEN TO      WK-POT1-LEN
                   WRITE   POT1-REC    FROM    WK-COMP-R-REC
                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

      *    *** READ PIN3 BINARY
           PERFORM S120-10     THRU    S120-EX

      *    *** READ PIN4 BINARY
           PERFORM S130-10     THRU    S130-EX
           .
       S300-EX.
           EXIT.

      *    *** WRITE POT1 1
       S310-10.
           MOVE    WK-COMP-CNT TO      WK-MEI2-COMP-CNT

           IF      SW-LENG     =       "1"
                   MOVE    " ﾚﾝｸﾞｽ ｱﾝﾏｯﾁ " TO WK-MEI2-COM
           END-IF

           IF      WK-PIN3-EOF NOT =   WK-PIN4-EOF
                   MOVE    " ｹﾝｽｳ  ｱﾝﾏｯﾁ " TO WK-MEI2-COM2
           END-IF

           MOVE    150         TO      WK-POT1-LEN
           WRITE   POT1-REC    FROM    WK-MEI2
           IF      WK-POT1-STATUS =    ZERO
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   DISPLAY WK-PGM-NAME
                           " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L-MAX
                   IF      WK-COMP2-REC (I:1) = "?"
                       MOVE    I          TO      WK-MEI3-COMP-BYT
                       MOVE    150        TO      WK-POT1-LEN
                       WRITE   POT1-REC   FROM    WK-MEI3
                       IF      WK-POT1-STATUS =    ZERO
                               ADD     1           TO      WK-POT1-CNT
                       ELSE
                               DISPLAY WK-PGM-NAME
                                       " POT1-F WRITE ERROR STATUS="
                                       WK-POT1-STATUS
                               STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S310-EX.
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
           MOVE    WK-PIN3-0D0A-CNT TO WK-PIN3-0D0A-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 (0D0A)=" WK-PIN3-0D0A-CNT-E
           MOVE    WK-PIN4-CNT TO      WK-PIN4-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 ｹﾝｽｳ = " WK-PIN4-CNT-E
                   " (" WK-PIN4-F-NAME ")"
           MOVE    WK-PIN4-0D0A-CNT TO WK-PIN4-0D0A-CNT-E
           DISPLAY WK-PGM-NAME " PIN4 (0D0A)=" WK-PIN4-0D0A-CNT-E
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    L-MAX2      TO      WK-L-MAX2-E
           DISPLAY WK-PGM-NAME " ﾁｪｯｸ ﾚﾝｸﾞｽ= " WK-L-MAX2-E
           MOVE    WK-MATCH-CNT TO      WK-MATCH-CNT-E
           DISPLAY WK-PGM-NAME " ﾏｯﾁ  ｹﾝｽｳ = " WK-MATCH-CNT-E
           MOVE    WK-COMP-CNT TO      WK-COMP-CNT-E
           DISPLAY WK-PGM-NAME " ｱﾝﾏｯﾁｹﾝｽｳ = " WK-COMP-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
