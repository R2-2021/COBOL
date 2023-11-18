      *    *** XVI tag data 作成
      *    *** TEST78 入力データ作成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST84.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** XVI tag データ　ページより、ＫＥＹ　ＷＯＲＤ 貼り付け
      *    *** １件目＝tag_a
      *    *** ２件目＝tag_b
      *    *** ．．．．
      *    *** ２６件目＝tag_z
      *    *** TEST78.tag_all.csv,TEST78.tag_kigou.csv は、区切り
      *    *** が処理出来ない為、EXCEL に貼り付けて作成する
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** tag データ TEST78 入力データ
      *    *** TEST78.tag_a.csv,TEST78.tag_z.csv 自動作成
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** tag データ TEST70 PIN5 (xvitag_t_s.csv) 後半入力データ
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** tag KEY WORD データ
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(60000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           05  FILLER          PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           05  FILLER          PIC  X(1000).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           05  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST84".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST84.PIN1".
      *    *** TEST84.POT1 => TEST78.tag_a.csv - TEST78.tag_z.csv
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST84.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST84.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST84.POT3".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN-MAX BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-TAG-CNT BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-LEN-MAX-E PIC --,---,---,--9 VALUE ZERO.

           03  WK-NUM          BINARY-LONG SYNC VALUE ZERO.
           03  WK-NUM9         PIC  9(001) VALUE ZERO.
           03  WK-Q            BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM1        PIC  X(100) VALUE SPACE.
           03  WK-ITEM2        PIC  X(010) VALUE SPACE.
           03  WK-ITEM1-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM2-LEN    BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-NUM          PIC  X(001) VALUE "N".
           03  SW-SPACE        PIC  X(001) VALUE "N".
           03  SW-HIT          PIC  X(001) VALUE "N".

      *    *** TEST78.tag_all.csv,TEST78.tag_kigou.csv は、区切り
      *    *** が処理出来ない為、EXCEL に貼り付けて作成する
       01  TBL-AREA.
           03  TBL01-AREA.
             05                PIC  X(016) VALUE "TEST78.tag_a.csv".
             05                PIC  X(016) VALUE "TEST78.tag_b.csv".
             05                PIC  X(016) VALUE "TEST78.tag_c.csv".
             05                PIC  X(016) VALUE "TEST78.tag_d.csv".
             05                PIC  X(016) VALUE "TEST78.tag_e.csv".
             05                PIC  X(016) VALUE "TEST78.tag_f.csv".
             05                PIC  X(016) VALUE "TEST78.tag_g.csv".
             05                PIC  X(016) VALUE "TEST78.tag_h.csv".
             05                PIC  X(016) VALUE "TEST78.tag_i.csv".
             05                PIC  X(016) VALUE "TEST78.tag_j.csv".
             05                PIC  X(016) VALUE "TEST78.tag_k.csv".
             05                PIC  X(016) VALUE "TEST78.tag_l.csv".
             05                PIC  X(016) VALUE "TEST78.tag_m.csv".
             05                PIC  X(016) VALUE "TEST78.tag_n.csv".
             05                PIC  X(016) VALUE "TEST78.tag_o.csv".
             05                PIC  X(016) VALUE "TEST78.tag_p.csv".
             05                PIC  X(016) VALUE "TEST78.tag_q.csv".
             05                PIC  X(016) VALUE "TEST78.tag_r.csv".
             05                PIC  X(016) VALUE "TEST78.tag_s.csv".
             05                PIC  X(016) VALUE "TEST78.tag_t.csv".
             05                PIC  X(016) VALUE "TEST78.tag_u.csv".
             05                PIC  X(016) VALUE "TEST78.tag_v.csv".
             05                PIC  X(016) VALUE "TEST78.tag_w.csv".
             05                PIC  X(016) VALUE "TEST78.tag_x.csv".
             05                PIC  X(016) VALUE "TEST78.tag_y.csv".
             05                PIC  X(016) VALUE "TEST78.tag_z.csv".
           03  TBL01-AREA-R    REDEFINES TBL01-AREA.
             05  TBL01-F-NAME  OCCURS 26
                               PIC  X(016).

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

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

           MOVE    TBL01-F-NAME (1) TO WK-POT1-F-NAME
           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   DISPLAY WK-PGM-NAME " WK-POT1-F-NAME=" WK-POT1-F-NAME
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   DISPLAY WK-PGM-NAME " WK-POT2-F-NAME=" WK-POT2-F-NAME
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   DISPLAY WK-PGM-NAME " WK-POT3-F-NAME=" WK-POT3-F-NAME
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
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

                   IF      WK-PIN1-LEN >       WK-PIN1-LEN-MAX
                           MOVE    WK-PIN1-LEN TO      WK-PIN1-LEN-MAX
                   END-IF

                   IF      WK-PIN1-LEN =       60000
                       DISPLAY WK-PGM-NAME " PIN1-F LEN-OVER ERROR LEN="
                           WK-PIN1-LEN
                       STOP    RUN
      *             ELSE
      *                DISPLAY WK-PGM-NAME " WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " WK-PIN1-LEN=" WK-PIN1-LEN
                   END-IF
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND "10"
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** WRITE POT1
       S100-10.

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      J
           MOVE    ZERO        TO      WK-NUM
                                       WK-POT1-TAG-CNT
           MOVE    "N"         TO      SW-NUM
                                       SW-SPACE

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   IF      PIN1-REC (I:1) =    SPACE
                       MOVE    "Y"         TO      SW-SPACE
                       MOVE    "?"         TO      PIN1-REC (I:1)
                   END-IF

                   IF   (( PIN1-REC (I:1) >=   "0"
                       AND PIN1-REC (I:1) <=   "9" ) OR
                           PIN1-REC (I:1) =    "," )
                       AND SW-SPACE    =       "Y"
                       ADD     1           TO      WK-NUM
                   END-IF

                   IF   (( PIN1-REC (I + 1:1) >=   "0"
                       AND PIN1-REC (I + 1:1) <=   "9" ) OR
                           PIN1-REC (I + 1:1) =    "," )
                       AND SW-SPACE    =       "Y"
                       MOVE    "Y"         TO      SW-NUM
                   ELSE
                       MOVE    "N"         TO      SW-NUM
                   END-IF

                   ADD     1           TO      J
                   MOVE    PIN1-REC (I:1) TO   POT1-REC (J:1)

                   IF      WK-NUM      >       ZERO
                       AND SW-NUM      =       "N"
                       AND J           >       ZERO

                       IF      WK-NUM      =       1
                           AND POT1-REC (J:1) =    "0"
                               CONTINUE
                       ELSE

      *    *** ? CHECK
                               PERFORM S120-10     THRU    S120-EX
                               WRITE   POT1-REC

                               IF      WK-POT1-STATUS =    ZERO
                                   ADD     1           TO    WK-POT1-CNT
                                                         WK-POT1-TAG-CNT
                               ELSE
                                   DISPLAY WK-PGM-NAME 
                                           " POT1-F WRITE ERROR STATUS="
                                           WK-POT1-STATUS
                                   STOP    RUN
                               END-IF

      *    *** WRITE POT2
                               PERFORM S130-10     THRU    S130-EX

      *    *** WRITE POT3
                               PERFORM S140-10     THRU    S140-EX
                       END-IF
                       MOVE    SPACE       TO      POT1-REC
                       MOVE    ZERO        TO      J
                       MOVE    ZERO        TO      WK-NUM
                       MOVE    "N"         TO      SW-NUM
                                                   SW-SPACE
                   END-IF
           END-PERFORM

      *    *** CLOSE,OPEN POT1
           PERFORM S110-10     THRU    S110-EX
           .
       S100-EX.
           EXIT.

      *    *** CLOSE,OPEN POT1
       S110-10.

           MOVE    WK-POT1-TAG-CNT TO    WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    ZERO        TO      WK-POT1-TAG-CNT

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   DISPLAY WK-PGM-NAME " WK-POT1-F-NAME=" WK-POT1-F-NAME
                   STOP    RUN
           END-IF

           IF      WK-PIN1-CNT <       26
                   MOVE    TBL01-F-NAME (WK-PIN1-CNT + 1) TO 
                           WK-POT1-F-NAME
                   OPEN    OUTPUT      POT1-F
                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " POT1-F OPEN ERROR STATUS="
                                   WK-POT1-STATUS
                           DISPLAY WK-PGM-NAME " WK-POT1-F-NAME="
                                   WK-POT1-F-NAME
                           STOP    RUN
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** ? CHECK
       S120-10.

      *    *** ? ２つ以上の時、最後の？のみ残す
           MOVE    ZERO        TO      WK-Q
           INSPECT POT1-REC (1:J) TALLYING
                   WK-Q FOR ALL "?"

           IF      WK-Q        >       1
                   PERFORM VARYING K FROM 1 BY 1
                           UNTIL K > J
                       IF      POT1-REC (K:1) =    "?"
                           IF      WK-Q        =       1
                               CONTINUE
                           ELSE
                               MOVE    "-"     TO      POT1-REC (K:1)
                               ADD     -1      TO      WK-Q
                           END-IF
                       ELSE
                           CONTINUE
                       END-IF
                   END-PERFORM
           ELSE
                   CONTINUE
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT2
       S130-10.

      *    *** TEST70 PIN5 用検索データ抽出
           MOVE     SPACE      TO      WK-ITEM1
                                       WK-ITEM2
           MOVE     ZERO       TO      WK-ITEM1-LEN
                                       WK-ITEM2-LEN
           UNSTRING POT1-REC
                    DELIMITED BY "?" OR SPACE
                    INTO
                    WK-ITEM1  COUNT WK-ITEM1-LEN
                    WK-ITEM2  COUNT WK-ITEM2-LEN

           INSPECT POT1-REC (1:J) REPLACING ALL "?" BY ","

           MOVE    "N"         TO      SW-HIT
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > J
                      OR SW-HIT = "Y"
      *    *** TEST71.POT2 XVI KEY WORD 件数多いものから
                   IF    ( POT1-REC (K:7) = "ameture"  OR
                           POT1-REC (K:7) = "actress"  OR
                           POT1-REC (K:3) = "bic"      OR
                           POT1-REC (K:7) = "college"  OR
                           POT1-REC (K:6) = "family"   OR
                           POT1-REC (K:4) = "fuck"     OR
                           POT1-REC (K:4) = "girl"     OR
                           POT1-REC (K:4) = "hard"     OR
                           POT1-REC (K:2) = "hd"       OR
                           POT1-REC (K:3) = "hot"      OR
                           POT1-REC (K:6) = "hentai"   OR
                           POT1-REC (K:8) = "japanese" OR
                           POT1-REC (K:7) = "lesbian"  OR
                           POT1-REC (K:6) = "orgasm"   OR
                           POT1-REC (K:5) = "pussy"    OR
                           POT1-REC (K:4) = "teen"     OR
                           POT1-REC (K:6) = "sister"   OR
                           POT1-REC (K:4) = "soft"     OR
                           POT1-REC (K:6) = "school"   OR
                           POT1-REC (K:6) = "webcam"   OR
                           POT1-REC (K:3) = "wet"      OR
                           POT1-REC (K:5) = "white"    OR
                           POT1-REC (K:5) = "young" )
                       AND NUMVAL (WK-ITEM2) > 1000
                           WRITE   POT2-REC    FROM    POT1-REC

                           IF      WK-POT2-STATUS =    ZERO
                               ADD     1           TO      WK-POT2-CNT
                           ELSE
                               DISPLAY WK-PGM-NAME 
                                       " POT2-F WRITE ERROR STATUS="
                                       WK-POT2-STATUS
                               STOP    RUN
                           END-IF
                           MOVE    "Y"         TO      SW-HIT
                   ELSE
                           CONTINUE
                   END-IF
           END-PERFORM
           .
       S130-EX.
           EXIT.

      *    *** WRITE POT3
       S140-10.

      *    *** KEY WORD 分解する
      *    *** AAA-BBB,CCC,000100
      *    *** AAA
      *    *** BBB
      *    *** CCC
      *    *** このデータＳＯＲＴし、TEST71でKEYダブリカット 件数付加する
      *    *** 発生件数の多い KEY WORD を調査する
           MOVE    SPACE       TO      POT3-REC
           MOVE    ZERO        TO      K2
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > J
                   IF      POT1-REC (K:1) = "-" OR ","
                           WRITE   POT3-REC

                           IF      WK-POT3-STATUS =    ZERO
                               ADD     1           TO      WK-POT3-CNT
                           ELSE
                               DISPLAY WK-PGM-NAME 
                                       " POT3-F WRITE ERROR STATUS="
                                       WK-POT3-STATUS
                               STOP    RUN
                           END-IF

                           MOVE    SPACE       TO      POT3-REC
                           MOVE    ZERO        TO      K2
                   ELSE
                           ADD     1           TO      K2
                           MOVE    POT1-REC (K:1) TO   POT3-REC (K2:1)
                   END-IF

                   IF      POT1-REC (K:1) =    ","
                           MOVE    J           TO      K
                   END-IF
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN1-LEN-MAX TO  WK-PIN1-LEN-MAX-E
           DISPLAY WK-PGM-NAME " PIN1 ﾚﾝｸﾞｽM=" WK-PIN1-LEN-MAX-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " ( TOTAL )"
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
