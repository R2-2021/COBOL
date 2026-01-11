      *    *** キーワードから キーワード含むマッチング分、
      *    *** アンマッチ分データ抽出
      *    *** PIN2 ID=100バイト

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST75.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 抽出前データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** キーワードデータ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後マッチング分データ (PIN1)
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後アンマッチ分データ (PIN1)
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 抽出後アンマッチ分データ (PIN2)
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(10000).

       FD  PIN2-F
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(10000).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(10000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(10000).

       FD  POT3-F.
       01  POT3-REC.
           03  FILLER          PIC  X(10000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST75  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST70.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST46.POT3".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST51.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST70.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBFIND.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "dir.txt".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST99_6.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST97U.POT2".
           03  WK-PIN1-F-NAME  PIC  X(064) VALUE 
      *         "TEST103.Random Walker Yu.PIN2".
      *         "TEST131.PIN1".
      *         "TEST69.POT1".
      *         "TEST56_jyoyu2000_birthday.PIN1".
      *         "TEST57_idol_daizukan.PIN1".
      *         "TEST103.ＭｉｓｓＡＶ　八掛うみ.PIN1".
                "TEST103.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST75.PIN2".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST99.POT1".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST123.POT2".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST127.POT1".
      *     03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST91.POT2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST75.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST75.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST75.POT3".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-ID0          PIC  X(1000) VALUE LOW-VALUE.
           03  WK-ID           PIC  X(1000) VALUE LOW-VALUE.
           03  WK-TITLE        PIC  X(1000) VALUE SPACE.
           03  WK-BR           PIC  X(1000) VALUE SPACE.
           03  WK-HTTPS        PIC  X(1000) VALUE SPACE.
           03  WK-IMG          PIC  X(1000) VALUE SPACE.
           03  WK-KANMA        PIC  X(002) VALUE " ,".
           03  WK-INS-CNT      BINARY-LONG SYNC VALUE ZERO.
           03  WK-BR-CNT       BINARY-LONG SYNC VALUE ZERO.
           03  WK-ID-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-TITLE-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-BR-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-IMG-LEN      BINARY-LONG SYNC VALUE ZERO.
           03  WK-STR-PTR      BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 3000.
             05  TBL01-ID      PIC  X(1000) VALUE HIGH-VALUE.
             05  TBL01-ID-LEN  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-HIT     PIC  X(001) VALUE HIGH-VALUE.

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



      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** PIN1
           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1,POT2
                   PERFORM S100-10     THRU    S100-EX

      *    *** WRITE2 POT1,POT2
      *             PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** PIN2 アンマッチ分 WRITE
      *     PERFORM S120-10     THRU    S120-EX

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
                               POT3-F

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
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    SPACE       TO      WK-ID
           MOVE    ZERO        TO      WK-ID-LEN

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
                   UNSTRING PIN2-REC
                           DELIMITED BY " ," OR "     "
                           INTO
                           WK-ID0
                           WK-ID  COUNT WK-ID-LEN
      *             MOVE    PIN2-REC    TO      WK-ID
      *             MOVE    WK-PIN2-LEN TO      WK-ID-LEN
      
           END-READ
           .
       S030-EX.
           EXIT.

      *    *** PIN2 TBL SET
       S032-10.

           ADD     1           TO      I
           IF      I           >       3000
                   DISPLAY WK-PGM-NAME
                           " TBL01-TBL OVER I=" I
                   STOP    RUN
           END-IF

           MOVE    WK-ID       TO      TBL01-ID     (I)
           MOVE    WK-ID-LEN   TO      TBL01-ID-LEN (I)
           MOVE    I           TO      I-MAX
           .
       S032-EX.
           EXIT.

      *    *** WRITE POT1 , POT2
      *    *** 180.00 秒前後 PERFORM VARYING UNTIL J > WK-PIN1-LEN
      *    *** 1.34 秒 PERFORM VARYING J FROM 2 BY 1 UNTIL J > 2
      *    *** PIN2 先頭コードのみ入っているため、最初のコードのみ比較する
      *    *** 0.58 秒 IF PIN1-REC (J:K) = TBL01-ID (I) (1:K) のみにした
      *    *** 0.50 秒
       S100-10.

           MOVE    "N"         TO      SW-SEARCH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > WK-PIN1-LEN
      *             UNTIL J > 2
                      OR SW-SEARCH = "Y"

                PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > I-MAX
                          OR SW-SEARCH = "Y"
                          OR TBL01-ID (I) = HIGH-VALUE

                   MOVE    TBL01-ID-LEN (I) TO  K
      *             IF      J + K - 1 <= WK-PIN1-LEN 
      *                 AND PIN1-REC (J:K) =    TBL01-ID (I) (1:K)
                   IF       PIN1-REC (J:K) =    TBL01-ID (I) (1:K)
      *             IF      PIN1-REC (2:K) =    TBL01-ID (I) (1:K)

      *    *** K=ZERO だと、条件満たしてしまう
      *             IF      K           NOT =   ZERO
      *                 AND PIN1-REC (1:K) =    TBL01-ID (I) (1:K)
                           MOVE    "Y"         TO      SW-SEARCH
                   END-IF
               END-PERFORM

           END-PERFORM

           IF      SW-SEARCH   =       "Y"
           DISPLAY WK-PIN1-CNT " J=" J " K=" K " I=" I  " I-MAX=" I-MAX
                   
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   WRITE   POT2-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT2-CNT
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** WRITE2 POT1 , POT2
      *    *** 40.34 秒 INSPECT ALL (perform varying 1/4.5)
      *    ***  4.25 秒 INSPECT LEADING (最左端が一致したらなので、
      *    *** INSPECT PIN1-REC (2:) にしたらうまくいった)
       S110-10.

           IF      WK-PIN1-F-NAME =    "TEST97U.POT2"
                   MOVE    ZERO        TO      WK-BR-CNT
                   INSPECT PIN1-REC TALLYING
                           WK-BR-CNT FOR ALL "<br>"
                   IF      WK-BR-CNT   NOT =   ZERO
                           MOVE    SPACE       TO      WK-TITLE
                                                       WK-BR
                                                       WK-HTTPS
                                                       WK-IMG
                           MOVE    ZERO        TO      WK-TITLE-LEN
                                                       WK-BR-LEN
                                                       WK-HTTPS-LEN
                                                       WK-IMG-LEN
                           UNSTRING PIN1-REC
      *                             DELIMITED BY "," OR "<br>"
                                   DELIMITED BY ","
                                   INTO
                                   WK-TITLE    COUNT WK-TITLE-LEN
      *                             WK-BR       COUNT WK-BR-LEN
                                   WK-HTTPS    COUNT WK-HTTPS-LEN
                                   WK-IMG      COUNT WK-IMG-LEN
                           MOVE    SPACE       TO      PIN1-REC
                           MOVE    1           TO      WK-STR-PTR
                           STRING  WK-TITLE DELIMITED BY "<br>"
                                   " ,"     DELIMITED BY SIZE
                                   WK-HTTPS DELIMITED BY SIZE
                                   INTO PIN1-REC
                                   WITH POINTER WK-STR-PTR
                   END-IF
           END-IF

           MOVE    ZERO        TO      WK-INS-CNT

           PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > I-MAX
                          OR WK-INS-CNT NOT = ZERO

                   MOVE    TBL01-ID-LEN (I) TO  K

      *             INSPECT PIN1-REC (2:) TALLYING
      *                     WK-INS-CNT FOR LEADING TBL01-ID (I) (1:K)
                   INSPECT PIN1-REC TALLYING
                           WK-INS-CNT FOR ALL TBL01-ID (I) (1:K)

                   IF      WK-INS-CNT NOT = ZERO
                           MOVE    "1"         TO      TBL01-HIT (I)
                   END-IF
           END-PERFORM

           IF      WK-INS-CNT      =       ZERO
                   WRITE   POT2-REC    FROM    PIN1-REC
      *             MOVE    "DEL"       TO      POT2-REC
      *             MOVE    PIN1-REC (37:50) TO POT2-REC (5:50)
      *             WRITE   POT2-REC
                   ADD     1           TO      WK-POT2-CNT
           ELSE
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** PIN2 アンマッチ分 WRITE
       S120-10.

           PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > I-MAX

                   IF      TBL01-HIT (I) = HIGH-VALUE
                           WRITE   POT3-REC    FROM    TBL01-ID (I)
                           ADD     1           TO      WK-POT3-CNT
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   POT1-F
                   POT2-F
                   POT3-F

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
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 ｹﾝｽｳ = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 ｹﾝｽｳ = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
