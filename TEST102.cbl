      *    *** COBRND TEST2

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST102.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 楽天検索データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
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

       FD  POT1-F.
       01  POT1-REC.
           03  POT1-IDX        PIC  9(002).
           03  POT1-I1         PIC  9(002).
           03  POT1-SEQ        PIC  9(008).
           03  POT1-TBL.
             05  POT1-FROM     PIC  9(010).
             05  POT1-TO-CNT   PIC  9(010).
             05  POT1-BETWEEN  PIC  9(010).
             05  POT1-SIGN     PIC  X(001).
             05  POT1-ZERO     PIC  X(001).
             05  POT1-RND      PIC  V9(09).
             05  POT1-FROM2    PIC  9(010).
             05  POT1-TO2      PIC  9(010).
             05  POT1-NUM      PIC S9(010).
             05  POT1-KANA     PIC  X(010).
             05  POT1-ALPHA    PIC  X(010).
             05  POT1-S-NAME   PIC  X(020).
             05  POT1-S-NAME8  PIC  X(030).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST102 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST102.PIN1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST102.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST102.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SEIYU1       PIC  X(030) VALUE SPACE.

      *    *** 変換前 が入っているデータ
      * 01  WDE03-BUF1             PIC  X(001) ANY LENGTH.

      *    *** 変換前のデータの長さ
       01  WDE03-BUF1-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** 16進数 変換後 が入っているデータ
      *    *** 富士通のNETCOBOLの資料によると、項目最大長は64770バイトである
       01  WDE03-BUF2.
      *    *** LLL...
           03  WDE03-BUF2-L-TBL.
             05  WDE03-BUF2-L  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** RRR...
           03  WDE03-BUF2-R-TBL.
             05  WDE03-BUF2-R  OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** LRLR...
           03  WDE03-BUF2-LR-TBL.
             05  WDE03-BUF2-LR-TBL2 OCCURS 65536.
               07  WDE03-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WDE03-BUF2-R2  PIC  X(001) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** データ作成１
           PERFORM S110-10     THRU    S110-EX

      *    *** データ作成２
           PERFORM S120-10     THRU    S120-EX

      *    *** データ作成３
           PERFORM S130-10     THRU    S130-EX

      *    *** データ作成４
           PERFORM S140-10     THRU    S140-EX

      *    *** データ作成５
           PERFORM S150-10     THRU    S150-EX

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                               PIN2-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

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

      *    *** データ作成１
       S110-10.

           MOVE    "N"         TO      WCR-SIGN  (1)
           MOVE    "N"         TO      WCR-ZERO  (1)
           MOVE    ZERO        TO      WCR-FROM  (1)
           MOVE    ZERO        TO      WCR-FROM2 (1)
           MOVE    ZERO        TO      WCR-TO2   (1)

           MOVE    "RND"       TO      WCR-ID
           MOVE    10          TO      WCR-IDX

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 10

                   CALL    "COBRND"    USING   WCR-COBRND-AREA
                   MOVE    WCR-IDX     TO      POT1-IDX
                   MOVE    I1          TO      POT1-I1
                   MOVE    WCR-SEQ     TO      POT1-SEQ
                   MOVE    WCR-FROM    (1) TO  POT1-FROM
                   MOVE    WCR-TO-CNT  (1) TO  POT1-TO-CNT
                   MOVE    WCR-BETWEEN (1) TO  POT1-BETWEEN
                   MOVE    WCR-SIGN    (1) TO  POT1-SIGN
                   MOVE    WCR-ZERO    (1) TO  POT1-ZERO
                   MOVE    WCR-RND     (1) TO  POT1-RND
                   MOVE    WCR-FROM2   (1) TO  POT1-FROM2
                   MOVE    WCR-TO2     (1) TO  POT1-TO2
                   MOVE    WCR-NUM     (1) TO  POT1-NUM
                   MOVE    WCR-KANA    (1) TO  POT1-KANA
                   MOVE    WCR-ALPHA   (1) TO  POT1-ALPHA
                   MOVE    WCR-S-NAME  (1) TO  POT1-S-NAME
                   MOVE    WCR-S-NAME8 (1) TO  POT1-S-NAME8

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** データ作成２
       S120-10.

           MOVE    "1"         TO      WCR-SIGN  (2)
           MOVE    "Y"         TO      WCR-ZERO  (2)
           MOVE    111         TO      WCR-FROM  (2)
           MOVE    1           TO      WCR-TO-CNT(2)
           MOVE    1           TO      WCR-BETWEEN(2)
           MOVE    1111        TO      WCR-FROM2 (2)
           MOVE    2222        TO      WCR-TO2   (2)

           MOVE    "RND"       TO      WCR-ID
           MOVE    10          TO      WCR-IDX

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 10

                   CALL    "COBRND"    USING   WCR-COBRND-AREA
                   MOVE    WCR-IDX     TO      POT1-IDX
                   MOVE    I1          TO      POT1-I1
                   MOVE    WCR-SEQ     TO      POT1-SEQ
                   MOVE    WCR-FROM    (2) TO  POT1-FROM
                   MOVE    WCR-TO-CNT  (2) TO  POT1-TO-CNT
                   MOVE    WCR-BETWEEN (2) TO  POT1-BETWEEN
                   MOVE    WCR-SIGN    (2) TO  POT1-SIGN
                   MOVE    WCR-ZERO    (2) TO  POT1-ZERO
                   MOVE    WCR-RND     (2) TO  POT1-RND
                   MOVE    WCR-FROM2   (2) TO  POT1-FROM2
                   MOVE    WCR-TO2     (2) TO  POT1-TO2
                   MOVE    WCR-NUM     (2) TO  POT1-NUM
                   MOVE    WCR-KANA    (2) TO  POT1-KANA
                   MOVE    WCR-ALPHA   (2) TO  POT1-ALPHA
                   MOVE    WCR-S-NAME  (2) TO  POT1-S-NAME
                   MOVE    WCR-S-NAME8 (2) TO  POT1-S-NAME8

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** データ作成３
       S130-10.

      *    *** POT1出力でFROMが２件目で加算されるのは、S120-10,S120-10で
      *    *** COBRND CALLしている為、COBRND内C1(I)値が１件目のCALLで
      *    *** TO-CNT値を超えている為、２件目以降はTO-CNT値を超えるごとに、
      *    *** FROMがTO-CNT値が加算される


           MOVE    "2"         TO      WCR-SIGN  (3)
           MOVE    "N"         TO      WCR-ZERO  (3)
           MOVE    222         TO      WCR-FROM  (3)
           MOVE    2           TO      WCR-TO-CNT(3)
           MOVE    2           TO      WCR-BETWEEN(3)
      *    *** FRO2 > TO2 の時、自動でCOBRND側で1000,2000にする
           MOVE    9999        TO      WCR-FROM2 (3)
           MOVE    8888        TO      WCR-TO2   (3)

           MOVE    "RND"       TO      WCR-ID
           MOVE    10          TO      WCR-IDX

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 10

                   CALL    "COBRND"    USING   WCR-COBRND-AREA
                   MOVE    WCR-IDX     TO      POT1-IDX
                   MOVE    I1          TO      POT1-I1
                   MOVE    WCR-SEQ     TO      POT1-SEQ
                   MOVE    WCR-FROM    (3) TO  POT1-FROM
                   MOVE    WCR-TO-CNT  (3) TO  POT1-TO-CNT
                   MOVE    WCR-BETWEEN (3) TO  POT1-BETWEEN
                   MOVE    WCR-SIGN    (3) TO  POT1-SIGN
                   MOVE    WCR-ZERO    (3) TO  POT1-ZERO
                   MOVE    WCR-RND     (3) TO  POT1-RND
                   MOVE    WCR-FROM2   (3) TO  POT1-FROM2
                   MOVE    WCR-TO2     (3) TO  POT1-TO2
                   MOVE    WCR-NUM     (3) TO  POT1-NUM
                   MOVE    WCR-KANA    (3) TO  POT1-KANA
                   MOVE    WCR-ALPHA   (3) TO  POT1-ALPHA
                   MOVE    WCR-S-NAME  (3) TO  POT1-S-NAME
                   MOVE    WCR-S-NAME8 (3) TO  POT1-S-NAME8

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S130-EX.
           EXIT.

      *    *** データ作成４
       S140-10.

           MOVE    "3"         TO      WCR-SIGN  (4)
           MOVE    "Y"         TO      WCR-ZERO  (4)
           MOVE    333         TO      WCR-FROM  (4)
           MOVE    3           TO      WCR-TO-CNT(4)
           MOVE    3           TO      WCR-BETWEEN(4)
      *    *** ー記号は受け付けない
           MOVE    -1111       TO      WCR-FROM2 (4)
           MOVE    2222        TO      WCR-TO2   (4)

           MOVE    "RND"       TO      WCR-ID
           MOVE    10          TO      WCR-IDX

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 10

                   CALL    "COBRND"    USING   WCR-COBRND-AREA
                   MOVE    WCR-IDX     TO      POT1-IDX
                   MOVE    I1          TO      POT1-I1
                   MOVE    WCR-SEQ     TO      POT1-SEQ
                   MOVE    WCR-FROM    (4) TO  POT1-FROM
                   MOVE    WCR-TO-CNT  (4) TO  POT1-TO-CNT
                   MOVE    WCR-BETWEEN (4) TO  POT1-BETWEEN
                   MOVE    WCR-SIGN    (4) TO  POT1-SIGN
                   MOVE    WCR-ZERO    (4) TO  POT1-ZERO
                   MOVE    WCR-RND     (4) TO  POT1-RND
                   MOVE    WCR-FROM2   (4) TO  POT1-FROM2
                   MOVE    WCR-TO2     (4) TO  POT1-TO2
                   MOVE    WCR-NUM     (4) TO  POT1-NUM
                   MOVE    WCR-KANA    (4) TO  POT1-KANA
                   MOVE    WCR-ALPHA   (4) TO  POT1-ALPHA
                   MOVE    WCR-S-NAME  (4) TO  POT1-S-NAME
                   MOVE    WCR-S-NAME8 (4) TO  POT1-S-NAME8

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S140-EX.
           EXIT.

      *    *** データ作成５
       S150-10.

           MOVE    "1"         TO      WCR-SIGN  (5)
           MOVE    "Y"         TO      WCR-ZERO  (5)
           MOVE    444         TO      WCR-FROM  (5)
           MOVE    5           TO      WCR-TO-CNT(5)
           MOVE    5           TO      WCR-BETWEEN(5)
           MOVE    3333        TO      WCR-FROM2 (5)
           MOVE    4444        TO      WCR-TO2   (5)

           MOVE    "RND"       TO      WCR-ID
           MOVE    10          TO      WCR-IDX

           PERFORM VARYING I1 FROM 1 BY 1 
                   UNTIL I1 > 10

                   CALL    "COBRND"    USING   WCR-COBRND-AREA
                   MOVE    WCR-IDX     TO      POT1-IDX
                   MOVE    I1          TO      POT1-I1
                   MOVE    WCR-SEQ     TO      POT1-SEQ
                   MOVE    WCR-FROM    (5) TO  POT1-FROM
                   MOVE    WCR-TO-CNT  (5) TO  POT1-TO-CNT
                   MOVE    WCR-BETWEEN (5) TO  POT1-BETWEEN
                   MOVE    WCR-SIGN    (5) TO  POT1-SIGN
                   MOVE    WCR-ZERO    (5) TO  POT1-ZERO
                   MOVE    WCR-RND     (5) TO  POT1-RND
                   MOVE    WCR-FROM2   (5) TO  POT1-FROM2
                   MOVE    WCR-TO2     (5) TO  POT1-TO2
                   MOVE    WCR-NUM     (5) TO  POT1-NUM
                   MOVE    WCR-KANA    (5) TO  POT1-KANA
                   MOVE    WCR-ALPHA   (5) TO  POT1-ALPHA
                   MOVE    WCR-S-NAME  (5) TO  POT1-S-NAME
                   MOVE    WCR-S-NAME8 (5) TO  POT1-S-NAME8

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S150-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   PIN2-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
