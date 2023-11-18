      *    *** バイオリズム 計算
      *    *** FILEITE.T013.PRM1
      *    *** C.FILITEM.T013

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST64.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** バイオリズムデータ プラス

      *    *** C.FILEITEM.T013.bat 
      *    *** REM ＊＊＊　TEST64.POT1 バイオリズム　＊＊＊
      *    *** FILEITEM FILEITEM.T013.PRM1 FILEITEM.0111.PRM2
      *    *** FILEITEM FILEITEM.T014.PRM1 FILEITEM.0111.PRM2
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** バイオリズムデータ マイナス
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-YMD        PIC  X(008).
           03  POT1-NISUU      PIC  9(009).
           03  POT1-TOSHI      PIC  9(003).
           03  POT1-SHINTAI    PIC S9V9(3).
           03  POT1-KANJYO     PIC S9V9(3).
           03  POT1-CHISEI     PIC S9V9(3).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-YMD        PIC  X(008).
           03  POT2-NISUU      PIC  9(009).
           03  POT2-TOSHI      PIC  9(003).
           03  POT2-SHINTAI    PIC S9V9(3).
           03  POT2-KANJYO     PIC S9V9(3).
           03  POT2-CHISEI     PIC S9V9(3).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST64  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST64.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST64.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST64.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SHINTAI      PIC S9V9(8) VALUE ZERO.
           03  WK-KANJYO       PIC S9V9(8) VALUE ZERO.
           03  WK-CHISEI       PIC S9V9(8) VALUE ZERO.
           03  WK-PAI          PIC S9V9(8) VALUE 3.14.
           03  WK-R            PIC S9(5)V9(8) VALUE ZERO.
           03  WK-ID           PIC  9(001) VALUE ZERO.
           03  WK-BIRTH-NISUU  PIC  9(007) VALUE ZERO.
           03  WK-BIRTH        PIC  9(008) VALUE ZERO.
           03  WK-TOSHI        PIC  9(003) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           MOVE    "A"         TO      WDW-DATE2-ID
           MOVE    "N"         TO      SW-YES

           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY " "
                   DISPLAY "誕生日 YYYYMMDD INPUT"
                   ACCEPT  WDW-DATE2-YMD
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
      *             DISPLAY "NISSU=" WDW-NISUU
                   DISPLAY "誕生日 " WDW-DATE2-YMD " OK ?"
                           " Y OR N INPUT"

                   MOVE    WDW-DATE2-YMD TO   WK-BIRTH
                   MOVE    WDW-NISUU   TO     WK-BIRTH-NISUU
                   ACCEPT  SW-YES
           END-PERFORM

           DISPLAY "1.バイオリズム >=＋0.75、<=-0.75 全項目AND条件出力"
                   " １５歳以上"
           DISPLAY "2.バイオリズム >=＋0.75、<=-0.75 身体のみ出力"
                   " １５歳以上"
           DISPLAY "3.バイオリズム >=＋0.75、<=-0.75 感情のみ出力"
                   " １５歳以上"
           DISPLAY "4.バイオリズム >=＋0.75、<=-0.75 知性のみ出力"
                   " １５歳以上"
           DISPLAY "5.バイオリズム >=＋0.75、<=-0.75 全項目OR条件出力"
                   " １５歳以上"
           DISPLAY "6.バイオリズム >=＋0.00、<=-0.00 全項目出力"
                   " 全年齢"
           ACCEPT   WK-ID

           EVALUATE WK-ID
               WHEN "1"
                   PERFORM S100-10     THRU    S100-EX
               WHEN "2"
                   PERFORM S110-10     THRU    S110-EX
               WHEN "3"
                   PERFORM S120-10     THRU    S120-EX
               WHEN "4"
                   PERFORM S130-10     THRU    S130-EX
               WHEN "5"
                   PERFORM S140-10     THRU    S140-EX
               WHEN "6"
                   PERFORM S150-10     THRU    S150-EX
           END-EVALUATE

      *    *** READ PIN1
      *     PERFORM S020-10     THRU    S020-EX

      *     PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
      *    *** WRITE POT1
      *             PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
      *             PERFORM S020-10     THRU    S020-EX
      *     END-PERFORM

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

      *    *** WRITE POT1 全項目
       S100-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500 
               COMPUTE WK-R = ( 2 * FUNCTION PI * I ) / 23.0000000
               COMPUTE WK-SHINTAI = FUNCTION SIN (WK-R)
      *         DISPLAY FUNCTION SIN(WK-SHINTAI)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 28.0000000
               COMPUTE WK-KANJYO  = FUNCTION SIN (WK-R)
      *         DISPLAY FUNCTION SIN(WK-KANJYO)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 33.0000000
               COMPUTE WK-CHISEI  = FUNCTION SIN (WK-R)
      *         DISPLAY FUNCTION SIN(WK-CHISEI)

               MOVE    "R"         TO      WDW-DATE2-ID
               COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
               CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

               IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                       ADD     1           TO      WK-TOSHI
               END-IF

               IF      WK-SHINTAI  >=       0.75
                   AND WK-KANJYO   >=       0.75
                   AND WK-CHISEI   >=       0.75

                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

                 IF      POT1-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT1-SHINTAI
                   MOVE    WK-KANJYO   TO      POT1-KANJYO
                   MOVE    WK-CHISEI   TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
                 END-IF
               END-IF

               IF      WK-SHINTAI  <=       -0.75
                   AND WK-KANJYO   <=       -0.75
                   AND WK-CHISEI   <=       -0.75

                   MOVE    WDW-DATE2-YMD TO    POT2-YMD
                   MOVE    I           TO      POT2-NISUU
                   MOVE    WK-TOSHI    TO      POT2-TOSHI

                 IF      POT2-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT2-SHINTAI
                   MOVE    WK-KANJYO   TO      POT2-KANJYO
                   MOVE    WK-CHISEI   TO      POT2-CHISEI

                   WRITE   POT2-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                               WK-POT2-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT2-CNT
                 END-IF
               END-IF
           END-PERFORM

           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1 身体のみ
       S110-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500 
               COMPUTE WK-R = ( 2 * FUNCTION PI * I ) / 23.0000000
               COMPUTE WK-SHINTAI = FUNCTION SIN (WK-R)

               MOVE    "R"         TO      WDW-DATE2-ID
               COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
               CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

               IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                       ADD     1           TO      WK-TOSHI
               END-IF

               IF      WK-SHINTAI  >=       0.75

                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

                 IF      POT1-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT1-SHINTAI
                   MOVE    ZERO        TO      POT1-KANJYO
                   MOVE    ZERO        TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
                 END-IF
               END-IF

               IF      WK-SHINTAI  <=       -0.75

                   MOVE    WDW-DATE2-YMD TO    POT2-YMD
                   MOVE    I           TO      POT2-NISUU
                   MOVE    WK-TOSHI    TO      POT2-TOSHI

                 IF      POT2-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT2-SHINTAI
                   MOVE    ZERO        TO      POT2-KANJYO
                   MOVE    ZERO        TO      POT2-CHISEI

                   WRITE   POT2-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                               WK-POT2-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT2-CNT
                 END-IF
               END-IF
           END-PERFORM

           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1 感情
       S120-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500 

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 28.0000000
               COMPUTE WK-KANJYO  = FUNCTION SIN (WK-R)

               MOVE    "R"         TO      WDW-DATE2-ID
               COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
               CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

               IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                       ADD     1           TO      WK-TOSHI
               END-IF

               IF      WK-KANJYO   >=       0.75

                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

                 IF      POT1-TOSHI  >=       15
                   MOVE    ZERO        TO      POT1-SHINTAI
                   MOVE    WK-KANJYO   TO      POT1-KANJYO
                   MOVE    ZERO        TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
                 END-IF
               END-IF

               IF      WK-KANJYO   <=       -0.75

                   MOVE    WDW-DATE2-YMD TO    POT2-YMD
                   MOVE    I           TO      POT2-NISUU
                   MOVE    WK-TOSHI    TO      POT2-TOSHI

                 IF      POT2-TOSHI  >=       15
                   MOVE    ZERO        TO      POT2-SHINTAI
                   MOVE    WK-KANJYO   TO      POT2-KANJYO
                   MOVE    ZERO        TO      POT2-CHISEI

                   WRITE   POT2-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                               WK-POT2-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT2-CNT
                 END-IF
               END-IF
           END-PERFORM

           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1 知性
       S130-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500 

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 33.0000000
               COMPUTE WK-CHISEI  = FUNCTION SIN (WK-R)

               MOVE    "R"         TO      WDW-DATE2-ID
               COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
               CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

               IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                       ADD     1           TO      WK-TOSHI
               END-IF

               IF      WK-CHISEI   >=       0.75

                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

                 IF      POT1-TOSHI  >=       15
                   MOVE    ZERO        TO      POT1-SHINTAI
                   MOVE    ZERO        TO      POT1-KANJYO
                   MOVE    WK-CHISEI   TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
                 END-IF
               END-IF

               IF      WK-CHISEI   <=       -0.75

                   MOVE    WDW-DATE2-YMD TO    POT2-YMD
                   MOVE    I           TO      POT2-NISUU
                   MOVE    WK-TOSHI    TO      POT2-TOSHI

                 IF      POT2-TOSHI  >=       15
                   MOVE    ZERO        TO      POT2-SHINTAI
                   MOVE    ZERO        TO      POT2-KANJYO
                   MOVE    WK-CHISEI   TO      POT2-CHISEI

                   WRITE   POT2-REC
                   IF      WK-POT2-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT2-F WRITE ERROR STATUS="
                               WK-POT2-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT2-CNT
                 END-IF
               END-IF
           END-PERFORM

           .
       S130-EX.
           EXIT.

      *    *** WRITE POT1 全項目
       S140-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500
               COMPUTE WK-R =  ( 2 * FUNCTION PI * I ) / 23.0000000
               COMPUTE WK-SHINTAI = FUNCTION SIN (WK-R)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 28.0000000
               COMPUTE WK-KANJYO  = FUNCTION SIN (WK-R)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 33.0000000
               COMPUTE WK-CHISEI  = FUNCTION SIN (WK-R)

               MOVE    "R"         TO      WDW-DATE2-ID
               COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
               CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

               IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                       ADD     1           TO      WK-TOSHI
               END-IF

               IF      WK-SHINTAI  >=       0.75
                    OR WK-KANJYO   >=       0.75
                    OR WK-CHISEI   >=       0.75
                    OR WK-SHINTAI  <=      -0.75
                    OR WK-KANJYO   <=      -0.75
                    OR WK-CHISEI   <=      -0.75
                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

                 IF      POT1-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT1-SHINTAI
                   MOVE    WK-KANJYO   TO      POT1-KANJYO
                   MOVE    WK-CHISEI   TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
                 END-IF
               END-IF
           END-PERFORM

           .
       S140-EX.
           EXIT.

      *    *** WRITE POT1 全項目
       S150-10.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 36500
               COMPUTE WK-R =  ( 2 * FUNCTION PI * I ) / 23.0000000
               COMPUTE WK-SHINTAI = FUNCTION SIN (WK-R)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 28.0000000
               COMPUTE WK-KANJYO  = FUNCTION SIN (WK-R)

               COMPUTE WK-R  = ( 2 * FUNCTION PI * I ) / 33.0000000
               COMPUTE WK-CHISEI  = FUNCTION SIN (WK-R)

                   MOVE    "R"         TO      WDW-DATE2-ID
                   COMPUTE WDW-NISUU = WK-BIRTH-NISUU + I
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   MOVE    WDW-DATE2-YMD TO    POT1-YMD
                   MOVE    I           TO      POT1-NISUU

                   IF      WDW-DATE2-YMD (5:4) = WK-BIRTH (5:4)
                           ADD     1           TO      WK-TOSHI
                   END-IF
                   MOVE    WK-TOSHI    TO      POT1-TOSHI

      *           IF      POT1-TOSHI  >=       15
                   MOVE    WK-SHINTAI  TO      POT1-SHINTAI
                   MOVE    WK-KANJYO   TO      POT1-KANJYO
                   MOVE    WK-CHISEI   TO      POT1-CHISEI

                   WRITE   POT1-REC
                   IF      WK-POT1-STATUS NOT = ZERO
                       DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                       STOP    RUN
                   END-IF

                   ADD     1           TO      WK-POT1-CNT
      *           END-IF
           END-PERFORM

           .
       S150-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
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
