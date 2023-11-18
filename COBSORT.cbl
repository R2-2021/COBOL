      *    *** SORT LINE SEQUENTIAL FILE用
      *    *** BINARY SEQUENTIAL はTEST38
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSORT.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
           03  PRM1-PRM        PIC  X(003).
           03  FILLER          PIC  X(077).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.

      *    *** 富士通のCOBOL85の資料によると、レコード最大長は32760バイトである
       01  PIN1-REC.
      *    *** KEY=64 バイトあるので、レコード長最大32686にした
           03  PIN1-DATA       PIC  X(32686).

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
           03  SIO1-KEY1-X.
             05  SIO1-KEY1-X2  PIC  X(010).
             05  SIO1-KEY1-9   REDEFINES SIO1-KEY1-X2
                               PIC S9(010).
             05                PIC  X(054).

           03  SIO1-KEY2-X
             05  SIO1-KEY2-X2  PIC  X(010).
             05  SIO1-KEY2-9   REDEFINES SIO1-KEY2-X2
                               PIC S9(010).
             05                PIC  X(054).

           03  SIO1-KEY3-X.
             05  SIO1-KEY3-X2  PIC  X(010).
             05  SIO1-KEY3-9   REDEFINES SIO1-KEY3-X2
                               PIC S9(010).
             05                PIC  X(054).
      *    *** KEY=64 バイトあるので、レコード長最大32686にした
           03  SIO1-DATA       PIC  X(32686).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(32686).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSORT ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "COBSORT.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE SPACE.
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "SORTWORK".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE SPACE.

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNTX    PIC  9(005) VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-OMIT-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-INCLUDE-CNT-E PIC --,---,---,--9 VALUE ZERO.

           03  WK-OMIT-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-INCLUDE-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-LUP-CNT      BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM
             05 WK-PRM1        PIC  X(001) VALUE SPACE.
             05 WK-PRM2        PIC  X(001) VALUE SPACE.
             05 WK-PRM3        PIC  X(001) VALUE SPACE.

      *    *** ＰＡＣＫはこのファイル入力ではエラーになるので、
      *    *** ＰＤ指定不可とする
      *     ORGANIZATION LINE   SEQUENTIAL.
           03  WK-KEY-CHAR.
             05  WK-KEY1-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY2-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY3-CHAR  PIC  X(002) VALUE SPACE.

           03  WK-KEY          BINARY-LONG SYNC VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-SU-X         PIC  X(010) VALUE ZERO.
           03  WK-SU-ZD        REDEFINES WK-SU-X
                               PIC  9(010).

           03  WK-CSVDT.
             05  WK-CSVDT01    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT02    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT03    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT04    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT05    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT06    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT07    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT08    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT09    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT10    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT11    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT12    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT13    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT14    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT15    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT16    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT17    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT18    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT19    PIC  X(064) VALUE SPACE.
             05  WK-CSVDT20    PIC  X(064) VALUE SPACE.
           03  WK-CSVDT-R      REDEFINES WK-CSVDT.
             05  WK-CSVDT-T    OCCURS 20
               07  WK-CSVDT-T2 PIC  X(064).
               07  WK-CSVDT-T-R REDEFINES WK-CSVDT-T2
                 09  WK-CSVDT-T-ZD
                               PIC  9(010).
                 09            PIC  X(054).

           03  WK-CSV-LT.
             05  WK-CSV-L01    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L02    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L03    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L04    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L05    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L06    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L07    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L08    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L09    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L10    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L11    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L12    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L13    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L14    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L15    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L16    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L17    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L18    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L19    BINARY-LONG SYNC VALUE ZERO.
             05  WK-CSV-L20    BINARY-LONG SYNC VALUE ZERO.
           03  WK-CSV-LT-R     REDEFINES WK-CSV-LT.
             05  WK-CSV-L      OCCURS 20
                               BINARY-LONG SYNC.

           03  WK-PRM1DT.
             05  WK-PRM1DT01   PIC  X(032) VALUE SPACE.
      *    *** F-IN,F-OT ファイル名３２バイト
             05  WK-PRM1DT02   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT03   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT04   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT05   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT06   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT07   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT08   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT09   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT10   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT11   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT12   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT13   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT14   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT15   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT16   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT17   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT18   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT19   PIC  X(032) VALUE SPACE.
             05  WK-PRM1DT20   PIC  X(032) VALUE SPACE.
           03  WK-PRM1DT-R      REDEFINES WK-PRM1DT.
             05  WK-PRM1DT-T    OCCURS 20
                               PIC  X(032).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

      *    *** INCLUDE 用
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 10.
             05  TBL01-CNT     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-POS     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-LEN     BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-TYPE    PIC  X(002) VALUE SPACE.
             05  TBL01-VALUE-CH.
               07  TBL01-VALUE-CH-X PIC X(010) VALUE ZERO.
               07  TBL01-VALUE-ZD REDEFINES TBL01-VALUE-CH-X
                               PIC  9(010).
               07              PIC X(022) VALUE ZERO.

      *    *** OMIT 用
           03  TBL02-AREA      OCCURS 10.
             05  TBL02-CNT     BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-POS     BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-LEN     BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-TYPE    PIC  X(002) VALUE SPACE.
             05  TBL02-VALUE-CH.
               07  TBL02-VALUE-CH-X PIC X(010) VALUE ZERO.
               07  TBL02-VALUE-ZD REDEFINES TBL02-VALUE-CH-X
                               PIC  9(010).
               07              PIC X(022) VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.

           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  O               BINARY-LONG SYNC VALUE ZERO.
           03  O-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P1X             BINARY-LONG SYNC VALUE ZERO.
           03  P2X             BINARY-LONG SYNC VALUE ZERO.
           03  P3X             BINARY-LONG SYNC VALUE ZERO.
           03  PR1             BINARY-LONG SYNC VALUE ZERO.
           03  PR1-MAX         BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-CSV          PIC  X(001) VALUE "N".
           03  SW-KEY1         PIC  X(001) VALUE "N".
           03  SW-KEY2         PIC  X(001) VALUE "N".
           03  SW-KEY3         PIC  X(001) VALUE "N".
           03  SW-OMIT         PIC  X(001) VALUE "N".
           03  SW-INCLUDE      PIC  X(001) VALUE "N".
           03  SW-INCLUDE2     PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.
 
      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** PRM1 READ
           PERFORM S020-10     THRU    S020-EX

           EVALUATE WK-PRM
               WHEN "A  "
                 IF      WK-KEY1-CHAR =      "CH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 ELSE

                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-IF

               WHEN "D  "
                 IF      WK-KEY1-CHAR =      "CH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 ELSE

                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-IF

               WHEN "AA "
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCH  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZD  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCH  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZD  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "AD "
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCH  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZD  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCH  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZD  "
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DA "
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCH  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZD  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCH  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZD  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DD "
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCH  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZD  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCH  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZD  "
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "AAA"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "AAD"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "ADA"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "ADD"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       ASCENDING  KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DAA"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DAD"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       ASCENDING  KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DDA"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       ASCENDING  KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

               WHEN "DDD"
                 EVALUATE TRUE

                     WHEN WK-KEY-CHAR  =      "CHCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "CHZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-X
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDCHZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-X
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDCH"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-X
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

                     WHEN WK-KEY-CHAR  =      "ZDZDZD"
                     SORT    SIO1-F
                       DESCENDING KEY SIO1-KEY1-9
                       DESCENDING KEY SIO1-KEY2-9
                       DESCENDING KEY SIO1-KEY3-9
      *    *** READ AND RELEASE
                       INPUT  PROCEDURE S100-SEC   THRU    S100-EX
      *    *** RETURN AND WRITE
                       OUTPUT PROCEDURE S200-SEC   THRU    S200-EX
                 END-EVALUATE

           END-EVALUATE

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

           ACCEPT  WK-ARGUMENT-NUMBER FROM      ARGUMENT-NUMBER

      *    *** PRM1-F 指定無し（ARGUMENT-NUMBER=0）、既定値使用
      *    *** ARGUMENT-NUMBER=1 の時、PRM1-F 指定する
           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
                   CONTINUE
               WHEN 1
                   ACCEPT  WK-PRM1-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PRM1-F-NAME

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PRM1-F 1個まで指定可"
                   STOP    RUN
           END-EVALUATE

      *    *** SORT-F はOPEN いらない

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** PRM1 READ
       S020-SEC                SECTION.
       S020-10.
           PERFORM UNTIL WK-PRM1-EOF =   HIGH-VALUE
               READ    PRM1-F
               IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
                   UNSTRING PRM1-REC
                       DELIMITED BY "," OR "=" OR SPACE
                       INTO
                       WK-PRM1DT01
                       WK-PRM1DT02
                       WK-PRM1DT03
                       WK-PRM1DT04
                       WK-PRM1DT05
                       WK-PRM1DT06
                       WK-PRM1DT07
                       WK-PRM1DT08
                       WK-PRM1DT09
                       WK-PRM1DT10
                       WK-PRM1DT11
                       WK-PRM1DT12
                       WK-PRM1DT13
                       WK-PRM1DT14
                       WK-PRM1DT15
                       WK-PRM1DT16
                       WK-PRM1DT17
                       WK-PRM1DT18
                       WK-PRM1DT19
                       WK-PRM1DT20

                   EVALUATE TRUE
                       WHEN WK-PRM1DT01(1:4) =    "SORT"
      *    *** SORT= チェック
                           PERFORM S021-10     THRU    S021-EX

                       WHEN WK-PRM1DT01(1:3) =    "KEY"
      *    *** KEY= チェック
                           PERFORM S022-10     THRU    S022-EX

                       WHEN WK-PRM1DT01(1:3) =    "CSV"
      *    *** CSV= チェック
                           PERFORM S023-10     THRU    S023-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-IN"
      *    *** F-IN= チェック
                           PERFORM S024-10     THRU    S024-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-OT"
      *    *** F-OT= チェック
                           PERFORM S025-10     THRU    S025-EX

                       WHEN WK-PRM1DT01(1:7) =    "INCLUDE"
      *    *** INCLUDE= チェック
                           PERFORM S026-10     THRU    S026-EX

                       WHEN WK-PRM1DT01(1:4) =    "OMIT"
      *    *** OMIT= チェック
                           PERFORM S027-10     THRU    S027-EX

                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   IF      WK-PRM1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PRM1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
               END-IF
           END-PERFORM

      *    *** PRM1-F KEY=N 組合せチェック

           IF    ( SW-KEY1     =       "Y" AND
                   SW-KEY2     =       "N" AND
                   SW-KEY3     =       "N" )    OR

                 ( SW-KEY1     =       "Y" AND
                   SW-KEY2     =       "Y" AND
                   SW-KEY3     =       "N" )    OR

                 ( SW-KEY1     =       "Y" AND
                   SW-KEY2     =       "Y" AND
                   SW-KEY3     =       "Y" )
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N PARA ERROR "
                   DISPLAY WK-PGM-NAME 
                           " KEY=N 指定無し：ＳＯＲＴしない、"
                   DISPLAY WK-PGM-NAME 
                           " KEY=1のみ、KEY=1と2のみ、KEY=1と2と3のみ、
                           いずれかを指定する"
                   STOP    RUN
           END-IF
      *    *** PRM1-F KEY=N 指定無い時、ＳＯＲＴしないで出力する
           IF      SW-KEY2     =       "N"
                   MOVE    SPACE       TO      WK-PRM(2:1)
           END-IF
           IF      SW-KEY3     =       "N"
                   MOVE    SPACE       TO      WK-PRM(3:1)
           END-IF

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
           .
       S020-EX.
           EXIT.

      *    *** SORT= CHECK
       S021-SEC                SECTION.
       S021-10.
           IF    ( WK-PRM1DT02(1:1) =  "A" OR "D"          ) AND
                 ( WK-PRM1DT02(2:1) =  "A" OR "D" OR SPACE ) AND
                 ( WK-PRM1DT02(3:1) =  "A" OR "D" OR SPACE )
                   MOVE    WK-PRM1DT02(1:3) TO    WK-PRM
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F SORT= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " SORT= A,Dを指定 KEY1,2,3の順で"
                           " A:昇順,D:降順で指定 1つ目は必須"
                   STOP    RUN
           END-IF
           .
       S021-EX.
           EXIT.

      *    *** KEY= CHECK
       S022-SEC                SECTION.
       S022-10.

           IF      WK-PRM1DT02(1:1) = "1" OR "2" OR "3"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY= 1,2,3で指定 KEY=1は必須"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT03(1:3) = "POS"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT04) TO P

                   IF      SW-CSV      =        "N"
                       IF      P           >=       1 AND
                               P           <=       32686
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Nの時、"
                                   " KEY=N POS= 1-32686の範囲で指定"
                           STOP    RUN
                       END-IF

                   ELSE
                       IF      P           >=      1 AND
                               P           <=      20
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Yの時、"
                                   "KEY=N POS= 1-20の範囲で指定"
                           STOP    RUN
                       END-IF
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS= PARA ERROR"
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS="
                           " 第2ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT05(1:3) = "LEN"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT06) TO L

                   IF    ( L           >=       1  AND
                           L           <=       64 AND
                           WK-PRM1DT08(1:2) =  "CH" ) OR 
                         ( L           >=       1  AND
                           L           <=       10 AND
                           WK-PRM1DT08(1:2) =  "ZD" )
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N LEN= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                  " KEY=N LEN= 1-64(ZDは10)の範囲で指定"
                           STOP    RUN
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN="
                           " 第3ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           EVALUATE WK-PRM1DT02(1:1)
               WHEN "1"
                   MOVE    L           TO      L1
                   MOVE    P           TO      P1
                   MOVE    "Y"         TO      SW-KEY1
               WHEN "2"
                   MOVE    L           TO      L2
                   MOVE    P           TO      P2
                   MOVE    "Y"         TO      SW-KEY2
               WHEN "3"
                   MOVE    L           TO      L3
                   MOVE    P           TO      P3
                   MOVE    "Y"         TO      SW-KEY3
           END-EVALUATE

           IF      WK-PRM1DT07(1:4) = "TYPE"
                   IF      WK-PRM1DT08(1:2) =  "CH" OR "ZD"
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N TYPE= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " KEY=N TYPE= CH,ZD で指定"
                           STOP    RUN
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N TYPE="
                           " 第4ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           EVALUATE WK-PRM1DT02(1:1)
               WHEN "1"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY1-CHAR
               WHEN "2"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY2-CHAR
               WHEN "3"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY3-CHAR
           END-EVALUATE
           .
       S022-EX.
           EXIT.

      *    *** CSV= チェック
       S023-SEC                SECTION.
       S023-10.
      *    *** インプットファイル，（カンマ）編集の時、ＣＳＶを指定する
      *    *** カンマ編集ファイルでも、CSV=N 指定も可、その時、POS=は
      *    *** レコードの位置バイトを指定、
           IF      WK-PRM1DT02(1:1) =  "Y" OR "N"
                   MOVE    WK-PRM1DT02(1:1) TO     SW-CSV
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F CSV= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F CSV= N,Y で指定"
                   STOP    RUN
           END-IF
           .
       S023-EX.
           EXIT.

      *    *** F-IN CHECK
       S024-SEC                SECTION.
       S024-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                           ACCEPT  WK-PIN1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-PIN1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-PIN1-F-NAME
           END-IF           .
       S024-EX.
           EXIT.

      *    *** F-OT CHECK
       S025-SEC                SECTION.
       S025-10.

           IF      WK-PRM1DT02 =       SPACE
      *    *** ファイル名未記入時は、入力をする
                   MOVE    "N"         TO      SW-YES
                   PERFORM UNTIL SW-YES =      "Y"
                           DISPLAY " "
                           DISPLAY WK-PGM-NAME " OUTPUT FILE NAME"
                           ACCEPT  WK-PIN1-F-NAME

                           DISPLAY WK-PGM-NAME " FILE NAME="
                                   WK-PIN1-F-NAME " OK ? Y/N"
                           ACCEPT  SW-YES
                   END-PERFORM
           ELSE
                   MOVE    WK-PRM1DT02 TO      WK-POT1-F-NAME
           END-IF
           .
       S025-EX.
           EXIT.

      *    *** INCLUDE= CHECK
       S026-SEC                SECTION.
       S026-10.

           IF      FUNCTION NUMVAL(WK-PRM1DT02) >= 0 AND 
                   FUNCTION NUMVAL(WK-PRM1DT02) <= 9
                   ADD     FUNCTION NUMVAL(WK-PRM1DT02) 1 GIVING I
                   ADD     1           TO      TBL01-CNT (I)
                   IF      TBL01-CNT(I) >      1
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F INCLUDE= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " INCLUDE=N N同じ値 違う値 0-9で指定"
                           STOP    RUN
                   ELSE
                           CONTINUE
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F INCLUDE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " INCLUDE= 0-9で指定"
                   STOP    RUN
           END-IF

      *    *** INCLUDE 指定有
           MOVE    "Y"         TO      SW-INCLUDE2
           IF      I           >       I-MAX
                   MOVE    I           TO      I-MAX
           END-IF
           IF      WK-PRM1DT03(1:3) = "POS"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT04) TO P

                   IF      SW-CSV      =        "N"
                       IF      P           >=       1 AND
                               P           <=       32686
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F INCLUDE=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Nの時、"
                                   " INCLUDE=N POS= 1-32686の範囲で指定"
                           STOP    RUN
                       END-IF

                   ELSE
                       IF      P           >=      1 AND
                               P           <=      20
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F INCLUDE=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Yの時、"
                                   "INCLUDE=N POS= 1-20の範囲で指定"
                           STOP    RUN
                       END-IF
                   END-IF
           ELSE
                DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N POS="
                           " 第2ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT05(1:3) = "LEN"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT06) TO L

                   IF    ( L           >=       1  AND
                           L           <=       64 AND
                           WK-PRM1DT08(1:2) =  "CH"  ) OR
                         ( L           >=       1  AND
                           L           <=       10 AND
                           WK-PRM1DT08(1:2) =  "ZD"  )
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-FINCLUDE=N LEN= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                             " INCLUDE=N LEN= 1-64(ZD 1-10)の範囲で指定"
                           STOP    RUN
                   END-IF
           ELSE
                DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N LEN="
                           " 第3ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           MOVE    P           TO      TBL01-POS (I)
           MOVE    L           TO      TBL01-LEN (I)

           IF      WK-PRM1DT07(1:4) = "TYPE"
                   IF      WK-PRM1DT08(1:2) =  "CH" OR "ZD"
                           MOVE    WK-PRM1DT08 TO      TBL01-TYPE (I)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F INCLUDE=N TYPE= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " INCLUDE=N TYPE= CH,ZD で指定"
                           STOP    RUN
                   END-IF
           ELSE
               DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N TYPE="
                           " 第4ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT09(1:5) = "VALUE"
                   IF      WK-PRM1DT08(1:2) =  "CH"
                          MOVE    WK-PRM1DT10 TO      TBL01-VALUE-CH (I)
                   ELSE
                          MOVE    WK-PRM1DT10 TO    TBL01-VALUE-CH-X (I)
                          CALL    "C$JUSTIFY" USING TBL01-VALUE-CH-X (I)
                                                      "R"
                          INSPECT TBL01-VALUE-CH-X (I) 
                              REPLACING ALL SPACE BY ZERO
                   END-IF
           ELSE
              DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N VALUE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F INCLUDE=N VALUE="
                           " 第5ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           .
       S026-EX.
           EXIT.

      *    *** OMIT= CHECK
       S027-SEC                SECTION.
       S027-10.

           IF      FUNCTION NUMVAL(WK-PRM1DT02) >= 0 AND 
                   FUNCTION NUMVAL(WK-PRM1DT02) <= 9
                   ADD     FUNCTION NUMVAL(WK-PRM1DT02) 1 GIVING O
                   ADD     1           TO      TBL02-CNT (O)
                   IF      TBL02-CNT(O) >      1
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F OMIT= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " OMIT=N N同じ値 違う値 0-9で指定"
                           STOP    RUN
                   ELSE
                           CONTINUE
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F OMIT= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " OMIT= 0-9で指定"
                   STOP    RUN
           END-IF

           IF      O           >       O-MAX
                   MOVE    O           TO      O-MAX
           END-IF

           IF      WK-PRM1DT03(1:3) = "POS"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT04) TO P

                   IF      SW-CSV      =        "N"
                       IF      P           >=       1 AND
                               P           <=       32686
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F OMIT=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Nの時、"
                                   " OMIT=N POS= 1-32686の範囲で指定"
                           STOP    RUN
                       END-IF

                   ELSE
                       IF      P           >=      1 AND
                               P           <=      20
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F OMIT=N POS= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Yの時、"
                                   "OMIT=N POS= 1-20の範囲で指定"
                           STOP    RUN
                       END-IF
                   END-IF
           ELSE
                DISPLAY WK-PGM-NAME " PRM1-F OMIT=N POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F OMIT=N POS="
                           " 第2ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
           END-IF

           IF      WK-PRM1DT05(1:3) = "LEN"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT06) TO L

                   IF    ( L           >=       1  AND
                           L           <=       64 AND
                           WK-PRM1DT08(1:2) =  "CH"  ) OR
                         ( L           >=       1  AND
                           L           <=       10 AND
                           WK-PRM1DT08(1:2) =  "ZD"  )
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-FOMIT=N LEN= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                " OMIT=N LEN= 1-64(ZD 1-10)の範囲で指定"
                           STOP    RUN
                   END-IF
           ELSE
                DISPLAY WK-PGM-NAME " PRM1-F OMIT=N LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F OMIT=N LEN="
                           " 第3ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
            END-IF

           MOVE    P           TO      TBL02-POS (O)
           MOVE    L           TO      TBL02-LEN (O)

           IF      WK-PRM1DT07(1:4) = "TYPE"
                   IF      WK-PRM1DT08(1:2) =  "CH" OR "ZD"
                           MOVE    WK-PRM1DT08 TO      TBL02-TYPE (O)
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F OMIT=N TYPE= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " OMIT=N TYPE= CH,ZD で指定"
                           STOP    RUN
                   END-IF
           ELSE
               DISPLAY WK-PGM-NAME " PRM1-F OMIT=N TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F OMIT=N TYPE="
                           " 第4ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
            END-IF

           IF      WK-PRM1DT09(1:5) = "VALUE"
                   IF      WK-PRM1DT08(1:2) =  "CH"
                          MOVE    WK-PRM1DT10 TO    TBL02-VALUE-CH (O)
                   ELSE
                          MOVE    WK-PRM1DT10 TO    TBL02-VALUE-CH-X (O)
                          CALL    "C$JUSTIFY" USING TBL02-VALUE-CH-X (O)
                                                      "R"
                          INSPECT TBL02-VALUE-CH-X (O) 
                              REPLACING ALL SPACE BY ZERO
                   END-IF
           ELSE
              DISPLAY WK-PGM-NAME " PRM1-F OMIT=N VALUE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " PRM1-F OMIT=N VALUE="
                           " 第5ﾊﾟﾗﾒｰﾀで指定"
                   STOP    RUN
            END-IF

           .
       S027-EX.
           EXIT.

      *    *** READ AND RELEASE
       S100-SEC                SECTION.
       S100-10.

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                       ADD     1           TO      WK-PIN1-CNT
                                                   WK-LUP-CNT
                       IF      WK-LUP-CNT  >       9999
                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           MOVE    "READ"      TO      WDT-DATE-LUP-COM
                           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
                           MOVE    WK-PIN1-CNT-E TO    WDT-DATE-LUP-COM
                                                       (7:14)
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
                           MOVE    ZERO        TO      WK-LUP-CNT
                       END-IF

                       IF      SW-CSV    =    "Y"
                           UNSTRING PIN1-REC
                               DELIMITED BY ","
                               INTO
                               WK-CSVDT01 COUNT WK-CSV-L01
                               WK-CSVDT02 COUNT WK-CSV-L02
                               WK-CSVDT03 COUNT WK-CSV-L03
                               WK-CSVDT04 COUNT WK-CSV-L04
                               WK-CSVDT05 COUNT WK-CSV-L05
                               WK-CSVDT06 COUNT WK-CSV-L06
                               WK-CSVDT07 COUNT WK-CSV-L07
                               WK-CSVDT08 COUNT WK-CSV-L08
                               WK-CSVDT09 COUNT WK-CSV-L09
                               WK-CSVDT10 COUNT WK-CSV-L10
                               WK-CSVDT11 COUNT WK-CSV-L11
                               WK-CSVDT12 COUNT WK-CSV-L12
                               WK-CSVDT13 COUNT WK-CSV-L13
                               WK-CSVDT14 COUNT WK-CSV-L14
                               WK-CSVDT15 COUNT WK-CSV-L15
                               WK-CSVDT16 COUNT WK-CSV-L16
                               WK-CSVDT17 COUNT WK-CSV-L17
                               WK-CSVDT18 COUNT WK-CSV-L18
                               WK-CSVDT19 COUNT WK-CSV-L19
                               WK-CSVDT20 COUNT WK-CSV-L20

      *    *** CSV=Y,OMIT CHECK
                           PERFORM S110-10     THRU    S110-EX

                           IF      SW-OMIT     =       "N"
      *    *** CSV=Y,INCLUDE CHECK
                               PERFORM S111-10     THRU    S111-EX
                           END-IF

                           MOVE    SPACE           TO    SIO1-KEY1-X
                           MOVE    SPACE           TO    SIO1-KEY2-X
                           MOVE    SPACE           TO    SIO1-KEY3-X

      *    *** KEY1= は必須 PRM2 参照しない
                           IF      WK-KEY1-CHAR =         "CH"
                               MOVE    WK-CSVDT-T(P1) TO     SIO1-KEY1-X
                           ELSE
                                MOVE    ZERO          TO     SIO1-KEY1-9
                                MOVE    WK-CSV-L(P1)  TO  L1

                                COMPUTE P1X = 11 - L1
                                MOVE    WK-CSVDT-T(P1) (1:L1) TO
                                        SIO1-KEY1-9 (P1X:L1)
                                MOVE    1       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                PERFORM S120-10 THRU S120-EX
                           END-IF

                           IF      WK-PRM2     =         "A" OR "D"
                               IF      WK-KEY2-CHAR =         "CH"
                                   MOVE    WK-CSVDT-T(P2) TO SIO1-KEY2-X
                               ELSE
                                   MOVE    ZERO          TO  SIO1-KEY2-9
                                   MOVE    WK-CSV-L(P2)  TO  L2

                                   COMPUTE P2X = 11 - L2
                                   MOVE    WK-CSVDT-T(P2) (1:L2) TO
                                           SIO1-KEY2-9 (P2X:L2)
                                   MOVE    2       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S120-10 THRU S120-EX
                               END-IF
                           END-IF 

                           IF      WK-PRM3     =         "A" OR "D"
                               IF      WK-KEY3-CHAR =         "CH"
                                   MOVE    WK-CSVDT-T(P3) TO SIO1-KEY3-X
                               ELSE
                                   MOVE    ZERO          TO  SIO1-KEY3-9
                                   MOVE    WK-CSV-L(P3)  TO  L3

                                   COMPUTE P3X = 11 - L3
                                   MOVE    WK-CSVDT-T(P3) (1:L3) TO
                                           SIO1-KEY3-9 (P3X:L3)
                                   MOVE    3       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S120-10 THRU S120-EX
                               END-IF
                           END-IF
                       ELSE
      *    *** CSV 以外の時

      *    *** CSV=N,OMIT CHECK
                           PERFORM S112-10     THRU    S112-EX

      *    *** CSV=N,INCLUDE CHECK
                           IF      SW-OMIT     =       "N"
                               PERFORM S113-10     THRU    S113-EX
                           END-IF

      *    *** ＺＤを指定しても残り、スペースになるので、ＳＯＲＴ順は正しい
      *    *** NUMVAL を指定しているので、内部表現のマイナスは処理されない
      *    *** ，カンマ以下は無視する
                           MOVE    SPACE           TO    SIO1-KEY1-X
                           MOVE    SPACE           TO    SIO1-KEY2-X
                           MOVE    SPACE           TO    SIO1-KEY3-X

                           IF      WK-KEY1-CHAR =         "CH"
                               MOVE    PIN1-REC(P1:L1) TO    SIO1-KEY1-X
                           ELSE
                                MOVE    ZERO          TO     SIO1-KEY1-9
                                COMPUTE P1X = 11 - L1
                                MOVE    PIN1-REC(P1:L1) TO   SIO1-KEY1-9
                                                             (P1X:L1)
                                MOVE    1       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                PERFORM S120-10 THRU S120-EX
                           END-IF

                           IF      WK-PRM2     =         "A" OR "D"
                               IF      WK-KEY2-CHAR =         "CH"
                                   MOVE   PIN1-REC(P2:L2) TO SIO1-KEY2-X
                               ELSE
                                MOVE    ZERO          TO     SIO1-KEY2-9
                                COMPUTE P2X = 11 - L2
                                MOVE    PIN1-REC(P2:L2) TO   SIO1-KEY2-9
                                                             (P2X:L2)
                                MOVE    2       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                PERFORM S120-10 THRU S120-EX
                               END-IF
                           END-IF

                           IF      WK-PRM3     =         "A" OR "D"
                               IF      WK-KEY3-CHAR =         "CH"
                                   MOVE   PIN1-REC(P3:L3) TO SIO1-KEY3-X
                               ELSE
                                MOVE    ZERO          TO     SIO1-KEY3-9
                                COMPUTE P3X = 11 - L3
                                MOVE    PIN1-REC(P3:L3) TO   SIO1-KEY3-9
                                                             (P3X:L3)
                                MOVE    3       TO   WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                PERFORM S120-10 THRU S120-EX
                               END-IF
                           END-IF
                       END-IF

      *                 MOVE    WK-PIN1-CNT TO        WK-PIN1-CNTX
      *                 IF      WK-PIN1-CNTX =        ZERO
      *                     DISPLAY WK-PGM-NAME " PIN1-CNT=" WK-PIN1-CNT
      *                 END-IF

                       IF      SW-OMIT     =        "Y"
                           ADD     1           TO       WK-OMIT-CNT
                       ELSE
                           IF      SW-INCLUDE  =        "Y"
                               ADD     1           TO     WK-INCLUDE-CNT
                               MOVE    PIN1-DATA   TO      SIO1-DATA
                               RELEASE SIO1-REC
                           ELSE
      *    *** INCLUDE PARA 無しは、出力対象とする
                               IF      SW-INCLUDE2  =        "N"
                                   MOVE    PIN1-DATA   TO      SIO1-DATA
                                   RELEASE SIO1-REC
                               ELSE
                                   CONTINUE
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF  WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO    WK-PIN1-EOF

                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           MOVE    "READ-E"    TO      WDT-DATE-LUP-COM
                           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
                           MOVE    WK-PIN1-CNT-E TO    WDT-DATE-LUP-COM
                                                       (7:14)
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
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

      *    *** CSV=Y OMIT CHECK
       S110-SEC                SECTION.
       S110-10.

           MOVE    "N"         TO      SW-OMIT
           PERFORM VARYING O FROM 1 BY 1
                   UNTIL O > O-MAX OR
                         SW-OMIT = "Y"
               IF      TBL02-CNT (O) =     1
                   MOVE    TBL02-POS (O) TO    P
                   MOVE    TBL02-LEN (O) TO    L
                   IF      TBL02-TYPE(O) =     "CH"
                       IF     TBL02-VALUE-CH(O) (1:L)
                              = WK-CSVDT-T(P) (1:L)
                           MOVE    "Y"         TO      SW-OMIT
                       END-IF
                   ELSE
                       CALL    "C$JUSTIFY" USING   WK-CSVDT-T(P) "R"
                       INSPECT WK-CSVDT-T(P) 
                           REPLACING ALL SPACE BY ZERO
                       IF     TBL02-VALUE-ZD(O) = WK-CSVDT-T-ZD(P)
                           MOVE    "Y"         TO      SW-OMIT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** CSV=Y INCLUDE CHECK
       S111-SEC                SECTION.
       S111-10.

           MOVE    "N"         TO      SW-INCLUDE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX OR
                         SW-INCLUDE = "Y"
               IF      TBL01-CNT (I) =     1
                   MOVE    TBL01-POS (I) TO    P
                   MOVE    TBL01-LEN (I) TO    L
                   IF      TBL01-TYPE(I) =     "CH"
                       IF     TBL01-VALUE-CH(I) (1:L)
                              = WK-CSVDT-T(P) (1:L)
                           MOVE    "Y"         TO      SW-INCLUDE
                       END-IF
                   ELSE
                       CALL    "C$JUSTIFY" USING   WK-CSVDT-T(P) "R"
                       INSPECT WK-CSVDT-T(P) 
                           REPLACING ALL SPACE BY ZERO
                       IF     TBL01-VALUE-ZD(I) = WK-CSVDT-T-ZD(P)
                           MOVE    "Y"         TO      SW-INCLUDE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       S111-EX.
           EXIT.

      *    *** CSV=N OMIT CHECK
       S112-SEC                SECTION.
       S112-10.

           MOVE    "N"         TO      SW-OMIT
           PERFORM VARYING O FROM 1 BY 1
                   UNTIL O > O-MAX OR
                         SW-OMIT = "Y"
               IF      TBL02-CNT (O) =     1
                   MOVE    TBL02-POS (O) TO    P
                   MOVE    TBL02-LEN (O) TO    L
                   IF      TBL02-TYPE(O) =     "CH"
                       IF     TBL02-VALUE-CH(O) (1:L)
                              = PIN1-REC (P:L)
                           MOVE    "Y"         TO      SW-OMIT
                       END-IF
                   ELSE
                       MOVE    PIN1-REC (P:L) TO   WK-SU-X
                       CALL    "C$JUSTIFY" USING   WK-SU-X "R"
                       INSPECT WK-SU-X
                           REPLACING ALL SPACE BY ZERO
                       IF     TBL02-VALUE-ZD(O) = WK-SU-ZD
                           MOVE    "Y"         TO      SW-OMIT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       S112-EX.
           EXIT.

      *    *** CSV=N INCLUDE CHECK
       S113-SEC                SECTION.
       S113-10.

           MOVE    "N"         TO      SW-INCLUDE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX OR
                         SW-INCLUDE = "Y"
               IF      TBL01-CNT (I) =     1
                   MOVE    TBL01-POS (I) TO    P
                   MOVE    TBL01-LEN (I) TO    L
                   IF      TBL01-TYPE(I) =     "CH"
                       IF     TBL01-VALUE-CH(I) (1:L)
                              = PIN1-REC (P:L)
                           MOVE    "Y"         TO      SW-INCLUDE
                       END-IF
                   ELSE
                       MOVE    PIN1-REC (P:L) TO   WK-SU-X
                       CALL    "C$JUSTIFY" USING   WK-SU-X "R"
                       INSPECT WK-SU-X
                           REPLACING ALL SPACE BY ZERO
                       IF     TBL01-VALUE-ZD(I) = WK-SU-ZD
                           MOVE    "Y"         TO      SW-INCLUDE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       S113-EX.
           EXIT.

      *    *** TYPE=ZD 用 項目チェック
       S120-SEC                SECTION.
       S120-10.

           IF      WK-KEY      =       1
               IF      SIO1-KEY1-9 NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC KEY1 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR
                   CALL    "COBDUMP" USING     SIO1-KEY1-9
                          STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      SIO1-KEY2-9 NOT NUMERIC
                       DISPLAY WK-PGM-NAME 
                           DISPLAY WK-PGM-NAME 
                           " PIN1-REC KEY2 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                           " TYPE=" WK-KEY2-CHAR
                       CALL    "COBDUMP" USING     SIO1-KEY2-9
                       STOP    RUN
                   END-IF
               ELSE
                   IF      SIO1-KEY3-9 NOT NUMERIC
                       DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY3 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                           " TYPE=" WK-KEY3-CHAR
                       CALL    "COBDUMP" USING     SIO1-KEY3-9
                       STOP    RUN
                   END-IF
               END-IF
           END-IF

      *    *** POS= 間違って項目無い所の数指定した時
           IF      WK-KEY      =       1
               IF      L1          =       ZERO
                   DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY=1 POS=の項目長さゼロ"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      L2          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=2 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                               " TYPE=" WK-KEY2-CHAR
                       STOP    RUN
                   END-IF
               ELSE
                   IF      L3          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=3 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                               " TYPE=" WK-KEY3-CHAR
                       STOP    RUN
                   END-IF
               END-IF
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "SORT-E"    TO      WDT-DATE-LUP-COM
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           MOVE    WK-PIN1-CNT-E TO    WDT-DATE-LUP-COM
                                       (7:14)
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    ZERO        TO      WK-LUP-CNT
           PERFORM UNTIL   WK-SIO1-EOF   =     HIGH-VALUE
                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           MOVE    "RETU-E"    TO      WDT-DATE-LUP-COM
                           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
                           MOVE    WK-POT1-CNT-E TO    WDT-DATE-LUP-COM
                                                       (7:14)
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
                       NOT AT END
                           ADD     1           TO      WK-POT1-CNT
                                                       WK-LUP-CNT
                           IF      WK-LUP-CNT  >       9999
                               MOVE    "L"         TO  WDT-DATE-TIME-ID
                               MOVE    "RETURN"    TO  WDT-DATE-LUP-COM
                               MOVE    WK-POT1-CNT TO  WK-POT1-CNT-E
                               MOVE    WK-POT1-CNT-E TO WDT-DATE-LUP-COM
                                                       (7:14)
                               CALL   "DATETIME" USING WDT-DATETIME-AREA
                               MOVE    ZERO        TO  WK-LUP-CNT
                           END-IF
                           MOVE    SIO1-DATA   TO      POT1-DATA
                           WRITE   POT1-REC

                           IF      WK-POT1-STATUS NOT =  ZERO
                                   DISPLAY WK-PGM-NAME
                                           " POT1-F WRITE ERROR STATUS="
                                           WK-POT1-STATUS
                                   STOP    RUN
                           END-IF
                    END-RETURN
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-SEC                SECTION.
       S900-10.

           CLOSE   PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-OMIT-CNT TO      WK-OMIT-CNT-E
           DISPLAY WK-PGM-NAME " OMIT ｹﾝｽｳ = " WK-OMIT-CNT-E
           MOVE    WK-INCLUDE-CNT TO   WK-INCLUDE-CNT-E
           DISPLAY WK-PGM-NAME " INCLUDEｽｳ = " WK-INCLUDE-CNT-E

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
