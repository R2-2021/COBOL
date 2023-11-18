      *    *** テストデータ作成
      *    *** PIN1 は現在未使用

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILEDTCR.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** パラメータデータ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PIN1-F 現在未使用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORTWORK
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
                               STATUS   WK-SIO1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** テストデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(1024).

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.

       01  PRM1-REC.
           03  PRM1-PRM        PIC  X(003).
           03  FILLER          PIC  X(197).

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
      *    *** KEY長 CHは32バイトにする
           03  SIO1-KEY1-X.
             05  SIO1-KEY1-X2  PIC  X(010).
             05  SIO1-KEY1-9   REDEFINES SIO1-KEY1-X2
                               PIC S9(010).
             05                PIC  X(022).

           03  SIO1-KEY2-X
             05  SIO1-KEY2-X2  PIC  X(010).
             05  SIO1-KEY2-9   REDEFINES SIO1-KEY2-X2
                               PIC S9(010).
             05                PIC  X(022).

           03  SIO1-KEY3-X.
             05  SIO1-KEY3-X2  PIC  X(010).
             05  SIO1-KEY3-9   REDEFINES SIO1-KEY3-X2
                               PIC S9(010).
             05                PIC  X(022).

           03  SIO1-DATA       PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(10000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILEDTCR".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "FILEDTCR.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "FILEDTCR.PIN1".
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "SORTWORK".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "FILEDTCR.POT1".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-SIO1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1-RL-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-SIO1-RT-CNT  BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1-RL-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-SIO1-RT-CNT-E PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM          PIC  X(003) VALUE SPACE.
           03  WK-DATA-9       PIC  9(010) VALUE ZERO.
           03  WK-DATA-S9      PIC S9(010) VALUE ZERO.
           03  WK-DATA         PIC  9(005) VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG VALUE ZERO.

           03  WK-PRM01        PIC  X(010) VALUE SPACE.
           03  WK-PRM02        PIC  X(032) VALUE SPACE.
           03  WK-PRM03        PIC  X(010) VALUE SPACE.
           03  WK-PRM04        PIC  X(010) VALUE SPACE.
           03  WK-PRM05        PIC  X(010) VALUE SPACE.
           03  WK-PRM06        PIC  X(010) VALUE SPACE.
           03  WK-PRM07        PIC  X(010) VALUE SPACE.
           03  WK-PRM08        PIC  X(010) VALUE SPACE.
           03  WK-PRM09        PIC  X(010) VALUE SPACE.
           03  WK-PRM10        PIC  X(010) VALUE SPACE.
           03  WK-PRM11        PIC  X(010) VALUE SPACE.
           03  WK-PRM12        PIC  X(010) VALUE SPACE.
           03  WK-PRM13        PIC  X(010) VALUE SPACE.
           03  WK-PRM14        PIC  X(010) VALUE SPACE.
           03  WK-PRM15        PIC  X(010) VALUE SPACE.
           03  WK-PRM16        PIC  X(010) VALUE SPACE.
           03  WK-PRM17        PIC  X(010) VALUE SPACE.
           03  WK-PRM18        PIC  X(010) VALUE SPACE.
           03  WK-PRM19        PIC  X(010) VALUE SPACE.
           03  WK-PRM20        PIC  X(010) VALUE SPACE.

           03  WK-CRE          BINARY-LONG SYNC VALUE ZERO.
           03  WK-IDX-MAX      BINARY-LONG SYNC VALUE ZERO.
           03  WK-TBL01-MAX    BINARY-LONG SYNC VALUE ZERO.

           03  WK-LEN          BINARY-LONG SYNC VALUE ZERO.

           03  WK-COM.
             05  WK-COM-I1     PIC  X(010) VALUE SPACE.
             05  WK-COM-SU     PIC  ZZ,ZZZ,ZZ9 VALUE ZERO.

      *    *** ＰＡＣＫはこのファイル入力ではエラーになるので、
      *    *** ＰＤ指定不可とする
      *     ORGANIZATION LINE   SEQUENTIAL.
           03  WK-KEY-CHAR.
             05  WK-KEY1-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY2-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY3-CHAR  PIC  X(002) VALUE SPACE.

      *    *** 100,000 件　10.03秒
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 99.
             05  TBL01-DT-P    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-L    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-TYPE PIC  X(005) VALUE SPACE.
             05  TBL01-DT-IDX  BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-FROM BINARY-DOUBLE SYNC VALUE ZERO.
             05  TBL01-DT-TO   BINARY-DOUBLE SYNC VALUE ZERO.
             05  TBL01-DT-BET  BINARY-DOUBLE SYNC VALUE ZERO.
             05  TBL01-DT-SIGN PIC  X(001) VALUE SPACE.
             05  TBL01-DT-ZERO PIC  X(001) VALUE SPACE.
      *    *** DT=NN のNNダブりチェック
             05  TBL01-DT-CNT  BINARY-LONG SYNC VALUE ZERO.

           03  TBL02-AREA      OCCURS 3.
             05  TBL02-KEY-P   BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-KEY-L   BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-TYPE    PIC  X(002) VALUE SPACE.

      *     03  TBL03-AREA      OCCURS 10.
      *    *** DT=,TYPE=CODE のIDX=NN NNダブりチェック
      *    *** 同じIDX使用不可
      *       05  TBL03-IDX-CNT  BINARY-LONG SYNC VALUE ZERO.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  INDEX-AREA,
           03  OCC01           USAGE IS INDEX   VALUE ZERO.
      *    *** INDEXED BY 句の定義不要
      *     03  IDX01           BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  N               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.

           03  P01-L           BINARY-LONG SYNC VALUE ZERO.
           03  P02-L           BINARY-LONG SYNC VALUE ZERO.
           03  P03-L           BINARY-LONG SYNC VALUE ZERO.
           03  P04-L           BINARY-LONG SYNC VALUE ZERO.
           03  P05-L           BINARY-LONG SYNC VALUE ZERO.
           03  P06-L           BINARY-LONG SYNC VALUE ZERO.
           03  P07-L           BINARY-LONG SYNC VALUE ZERO.
           03  P08-L           BINARY-LONG SYNC VALUE ZERO.
           03  P09-L           BINARY-LONG SYNC VALUE ZERO.
           03  P10-L           BINARY-LONG SYNC VALUE ZERO.
           03  P11-L           BINARY-LONG SYNC VALUE ZERO.
           03  P12-L           BINARY-LONG SYNC VALUE ZERO.
           03  P13-L           BINARY-LONG SYNC VALUE ZERO.
           03  P14-L           BINARY-LONG SYNC VALUE ZERO.
           03  P15-L           BINARY-LONG SYNC VALUE ZERO.
           03  P16-L           BINARY-LONG SYNC VALUE ZERO.
           03  P17-L           BINARY-LONG SYNC VALUE ZERO.
           03  P18-L           BINARY-LONG SYNC VALUE ZERO.
           03  P19-L           BINARY-LONG SYNC VALUE ZERO.
           03  P20-L           BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA,
           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-CSV          PIC  X(001) VALUE "N".
           03  SW-KEY1         PIC  X(001) VALUE "N".
           03  SW-KEY2         PIC  X(001) VALUE "N".
           03  SW-KEY3         PIC  X(001) VALUE "N".
           03  SW-PRINT        PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.

      *    *** OPEN
           PERFORM S010-SEC    THRU    S010-EX

      *    *** READ PRM1
           PERFORM S020-SEC    THRU    S020-EX

           EVALUATE WK-PRM
               WHEN SPACE
      *    *** READ AND WRITE SORTしない
                   PERFORM S100-SEC    THRU    S100-EX
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
           PERFORM S900-SEC    THRU    S900-EX
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

           ACCEPT  WK-ARGUMENT-NUMBER FROM    ARGUMENT-NUMBER

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

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA
           .
       S010-EX.
           EXIT.

      *    *** READ PRM1
       S020-SEC                SECTION.
       S020-10.

           PERFORM UNTIL   WK-PRM1-EOF   =     HIGH-VALUE

                   READ    PRM1-F

                   IF      WK-PRM1-STATUS =    ZERO
                           ADD     1           TO        WK-PRM1-CNT
                           UNSTRING PRM1-REC
                               DELIMITED BY "," OR  "=" OR SPACE
                               INTO
                               WK-PRM01 COUNT P01-L
                               WK-PRM02 COUNT P02-L
                               WK-PRM03 COUNT P03-L
                               WK-PRM04 COUNT P04-L
                               WK-PRM05 COUNT P05-L
                               WK-PRM06 COUNT P06-L
                               WK-PRM07 COUNT P07-L
                               WK-PRM08 COUNT P08-L
                               WK-PRM09 COUNT P09-L
                               WK-PRM10 COUNT P10-L
                               WK-PRM11 COUNT P11-L
                               WK-PRM12 COUNT P12-L
                               WK-PRM13 COUNT P13-L
                               WK-PRM14 COUNT P14-L
                               WK-PRM15 COUNT P15-L
                               WK-PRM16 COUNT P16-L
                               WK-PRM17 COUNT P17-L
                               WK-PRM18 COUNT P18-L
                               WK-PRM19 COUNT P19-L
                               WK-PRM20 COUNT P20-L
                           END-UNSTRING

                           EVALUATE WK-PRM01
                               WHEN "SORT"
                                   PERFORM S021-10     THRU    S021-EX
                               WHEN "DT"
                                   PERFORM S022-10     THRU    S022-EX
                               WHEN "F-IN"
                                   PERFORM S023-10     THRU    S023-EX
                               WHEN "F-OT"
                                   PERFORM S024-10     THRU    S024-EX
                               WHEN "CSV"
                                   PERFORM S025-10     THRU    S025-EX
                               WHEN "KEY"
                                   PERFORM S026-10     THRU    S026-EX
                               WHEN "CRE"
                                   PERFORM S027-10     THRU    S027-EX
                               WHEN OTHER
                                   CONTINUE
                           END-EVALUATE
                   ELSE
                       IF  WK-PRM1-STATUS =    10
                           MOVE    HIGH-VALUE  TO    WK-PRM1-EOF
                       ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM

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

      *    *** PRM1-F KEY=N 組合せチェック
           IF    ( SW-KEY1     =       "N" AND
                   SW-KEY2     =       "N" AND
                   SW-KEY3     =       "N" )    OR

                 ( SW-KEY1     =       "Y" AND
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
                           いずれかで指定する"
                   STOP    RUN
           END-IF
      *    *** PRM1-F KEY=N 指定無い時、ＳＯＲＴしないで出力する
           IF      SW-KEY1     =       "N"
                   MOVE    SPACE       TO      WK-PRM(1:1)
           END-IF
           IF      SW-KEY2     =       "N"
                   MOVE    SPACE       TO      WK-PRM(2:1)
           END-IF
           IF      SW-KEY3     =       "N"
                   MOVE    SPACE       TO      WK-PRM(3:1)
           END-IF

           MOVE    TBL02-TYPE (1) TO   WK-KEY1-CHAR
           MOVE    TBL02-TYPE (2) TO   WK-KEY2-CHAR
           MOVE    TBL02-TYPE (3) TO   WK-KEY3-CHAR
           .
       S020-EX.
           EXIT.
           
      *    *** SORT=MODE 
       S021-SEC                SECTION.
       S021-10.
      *    *** SORT=AAAを指定していても、KEY=Nが指定していない時は、
      *    *** SORTしない
           IF    ( WK-PRM02(1:1) =     "A" OR "D" OR SPACE ) AND
                 ( WK-PRM02(2:1) =     "A" OR "D" OR SPACE ) AND
                 ( WK-PRM02(3:1) =     "A" OR "D" OR SPACE )
                   MOVE    WK-PRM02(1:3) TO    WK-PRM
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F READ PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " SORT=XXX XはSPACE,A,Dのいずれかで指定する"
                   STOP    RUN
           END-IF
           .
       S021-EX.
           EXIT.

      *    *** DT=NN NN=1-99
       S022-SEC                SECTION.
       S022-10.

      *    *** 第１パラメータ
           IF      WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 99
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO I
                   ADD     1           TO      TBL01-DT-CNT (I)
                   IF      I           >       WK-TBL01-MAX
                           MOVE    I           TO      WK-TBL01-MAX
                   END-IF
                   IF      TBL01-DT-CNT (I) >       1
                       DISPLAY WK-PGM-NAME " PRM1-F DT=NN PARA ERROR="
                               PRM1-REC
                       DISPLAY WK-PGM-NAME 
                               " DT=NN NN 同じものが指定されている"
                       STOP    RUN
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN 1-99の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第２パラメータ
           IF      WK-PRM03    =       "POS"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN POS= 2つめに指定する"
                   STOP    RUN
           END-IF

      *    *** オペレーティングシステムで有効か分からないが、
      *    *** 最大値 65536 にする
           IF      WK-PRM04(1:P04-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM04) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM04) <= 65536
                   MOVE    FUNCTION NUMVAL(WK-PRM04) TO
                                       TBL01-DT-P (I)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN POS= 1-65536の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第３パラメータ
           IF      WK-PRM05    =       "LEN"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN LEN= 3つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM06(1:P06-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM06) <= 256
                   MOVE    FUNCTION NUMVAL(WK-PRM06) TO
                                       TBL01-DT-L (I)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN LEN= 1-256の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第４パラメータ
           IF      WK-PRM07    =       "TYPE"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN TYPE= 4つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM08    =       "NUM" OR "KANA" OR "ALPHA" OR
                                       "SNAME" OR "CODE" OR "SUTF8"
                                    OR "DATE" OR "DTCD"
                   MOVE    WK-PRM08    TO      TBL01-DT-TYPE (I)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           "  DT=NN TYPE= NUM,KANA,ALPHA,SNAME,CODE,"
                           "SUTF8,DATE いずれかで指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM08    =       "NUM" OR "CODE"
                   IF      WK-PRM06(1:P06-L) IS NUMERIC   AND
                           FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                           FUNCTION NUMVAL(WK-PRM06) <= 10
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM1-F  DT=NN TYPE=NUM"
                                               ",CODE LEN= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " TYPE=NUM,CODEの時、"
                                             " NUM=1-10の範囲で指定する"
                           STOP    RUN
                   END-IF
           END-IF

           IF      WK-PRM08    =       "DATE" OR "DTCD"
                   IF      WK-PRM06(1:P06-L) IS NUMERIC
                       AND FUNCTION NUMVAL(WK-PRM06) = 8
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM1-F  DT=NN TYPE=DAT"
                                               "E,DTCD LEN= PARA ERROR="
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " TYPE=DATE,DTCDの時、"
                                             " LEN=8のみ指定する"
                           STOP    RUN
                   END-IF
           END-IF

      *    *** 第５パラメータ
           IF      WK-PRM09    =       "IDX"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F IDX= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN IDX= 5つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM10(1:P10-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM10) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM10) <= 99
                   MOVE    FUNCTION NUMVAL(WK-PRM10) TO
                                       TBL01-DT-IDX (I)
      *    *** CODE,NUM はIDX 指定されていても、未使用とする
      *    *** TBL01- DT=NOをIDXとして使う
                   IF  WK-PRM08 = "CODE" OR "NUM"
                       MOVE  I   TO   TBL01-DT-IDX (I)
                   END-IF
      *             IF      TBL03-IDX-CNT (TBL01-DT-IDX (I)) > 1
      *                 DISPLAY WK-PGM-NAME " PRM1-F IDX=NN PARA ERROR="
      *                         PRM1-REC
      *                 DISPLAY WK-PGM-NAME 
      *                         " DT=NN TYPE=CODE OR NUM"
      *                         " IDX=NN 1-99 で違う数指定する"
      *                 STOP    RUN
      *             END-IF
                   IF      TBL01-DT-IDX (I) >  WK-IDX-MAX
                           MOVE    TBL01-DT-IDX (I) TO WK-IDX-MAX
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN IDX= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN IDX= 1-99 範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第６パラメータ
           IF      WK-PRM11    =       "FROM" OR SPACE
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN FROM= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN FROM= 6つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM11(1:4) =     "FROM"
               IF  WK-PRM12(1:P12-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM12) >= 0 AND
                   FUNCTION NUMVAL(WK-PRM12) <= 100000000
                   MOVE    FUNCTION NUMVAL(WK-PRM12) TO
                                       TBL01-DT-FROM (I)
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN FROM= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN FROM= 0-100000000 範囲で指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM08    =       "DATE" OR "DTCD"
               IF      WK-PRM11(1:4) =     "FROM"
                   MOVE    "A"         TO      WDW-DATE2-ID
                   MOVE    TBL01-DT-FROM (I) TO WDW-DATE2-YMD-9
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
      *    *** YYYYMMDD => NISUU に変換
                   MOVE    WDW-NISUU   TO      TBL01-DT-FROM (I)
               ELSE
                   CONTINUE
               END-IF
           END-IF

      *    *** 第７パラメータ
           IF      WK-PRM13    =       "TO" OR SPACE
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN TO= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN TO= 7つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM13(1:2) =     "TO"
               IF  WK-PRM14(1:P14-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM14) >= 0 AND
                   FUNCTION NUMVAL(WK-PRM14) <= 100000000
                   MOVE    FUNCTION NUMVAL(WK-PRM14) TO
                                       TBL01-DT-TO (I)
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN TO= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN TO= 0-100000000 範囲で指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM08    =       "DATE"
               IF      WK-PRM13(1:2) =     "TO"
                   MOVE    "A"         TO      WDW-DATE2-ID
                   MOVE    TBL01-DT-TO (I) TO  WDW-DATE2-YMD-9
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
      *    *** YYYYMMDD => NISUU に変換
                   MOVE    WDW-NISUU   TO      TBL01-DT-TO (I)
               ELSE
                   CONTINUE
               END-IF
           END-IF

      *    *** 第８パラメータ
           IF      WK-PRM15    =       "BETWEEN" OR
                   WK-PRM15    =       "SIGN" OR
                   WK-PRM15    =       SPACE
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN BETWEEN= OR SIGN="
                                       "PARA ERROR="
                                       PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN BETWEEN= OR SIGN="
                                       " 8つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM15    =       "SIGN"
               IF      WK-PRM16(1:1) =     "-" OR "1" OR "2" OR "3" OR
                                           "N" OR "Y"
                       MOVE    WK-PRM16(1:1) TO TBL01-DT-SIGN (I)
               ELSE
                       DISPLAY WK-PGM-NAME 
                               " PRM1-F DT=NN SIGN= PARA ERROR="
                               PRM1-REC
                       DISPLAY WK-PGM-NAME 
                           " DT=NN SIGN= -,1,2,3,Y,N いずれかで指定する"
                       STOP    RUN
               END-IF
           ELSE
             IF    WK-PRM15    =       "BETWEEN" 
               IF      WK-PRM16(1:P16-L) IS NUMERIC   AND
                       FUNCTION NUMVAL(WK-PRM16) >= 1 AND
                       FUNCTION NUMVAL(WK-PRM16) <= 100000000
                       MOVE    FUNCTION NUMVAL(WK-PRM16) TO
                                           TBL01-DT-BET (I)
               ELSE
                       DISPLAY WK-PGM-NAME 
                               " PRM1-F DT=NN BETWEEN= PARA ERROR="
                               PRM1-REC
                       DISPLAY WK-PGM-NAME 
                            " DT=NN BETWEEN= 1-100000000 範囲で指定する"
                       STOP    RUN
               END-IF
             END-IF
           END-IF

           IF      WK-PRM08    =       "DATE"
               IF      WK-PRM15    =      "BETWEEN" OR "SIGN"
                       DISPLAY WK-PGM-NAME 
                            " DT=NN TYPE=DATE BETWEEN,SIGN 指定不可"
                       STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM08    =       "DTCD"
               IF      WK-PRM15    =      "SIGN"
                       DISPLAY WK-PGM-NAME 
                            " DT=NN TYPE=DTCD SIGN 指定不可"
                       STOP    RUN
               END-IF
           END-IF

      *    *** 第９パラメータ
           IF      WK-PRM17    =       "ZERO" OR SPACE
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN ZERO= PARA ERROR="
                                       PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN ZERO= 9つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM17    =       "ZERO"
               IF      WK-PRM18(1:1) =     "N" OR "Y"
                   MOVE    WK-PRM18(1:1) TO TBL01-DT-ZERO (I)
               ELSE
                   DISPLAY WK-PGM-NAME 
                           " PRM1-F DT=NN ZERO=X PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN ZERO= N,Y いずれかで指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM08    =       "DATE" OR "DTCD"
               IF      WK-PRM17    =       "ZERO"
                       DISPLAY WK-PGM-NAME 
                            " DT=NN TYPE=DATE,DTCD ZERO= 指定不可"
                       STOP    RUN
               END-IF
           END-IF
           .
       S022-EX.
           EXIT.

      *    *** INPUT FILE FILE NAME
       S023-SEC                SECTION.
       S023-10.

           IF      WK-PRM02    NOT =   SPACE
                   MOVE    WK-PRM02    TO     WK-PIN1-F-NAME
                   EXIT    PARAGRAPH
           END-IF.

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME="
                           WK-PIN1-F-NAME " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM.
       S023-EX.
           EXIT.

      *    *** OUTPUT FILE FILE NAME
       S024-SEC                SECTION.
       S024-10.

           IF      WK-PRM02    NOT =   SPACE
                   MOVE    WK-PRM02    TO     WK-POT1-F-NAME
                   EXIT    PARAGRAPH
           END-IF.

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " OUTPUT FILE NAME"
                   ACCEPT  WK-POT1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME="
                           WK-POT1-F-NAME " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM.
       S024-EX.
           EXIT.

      *    *** CSV=
       S025-SEC                SECTION.
       S025-10.

           IF      WK-PRM02    =       "Y"
                   MOVE    "Y"         TO        SW-CSV
           ELSE
               IF      WK-PRM02    =       "N"
                       CONTINUE
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F CSV=X PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " CSV= Y,N で指定する"
                   STOP    RUN
               END-IF
           END-IF.
       S025-EX.
           EXIT.

      *    *** KEY=
       S026-SEC                SECTION.
       S026-10.
      *    *** DT=NN NN=1-99
      *    *** 第１パラメータ
           IF      WK-PRM01    =       "KEY"      AND
                   WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 3
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO I2
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY= 1-3 範囲で指定する"
                   STOP    RUN
           END-IF

           IF      I2          =       1
                   MOVE    "Y"         TO      SW-KEY1
           ELSE
               IF      I2          =       2
                   MOVE    "Y"         TO      SW-KEY2
               ELSE
                   MOVE    "Y"         TO      SW-KEY3
               END-IF
           END-IF

      *    *** 第２パラメータ
           IF      WK-PRM03    =       "POS"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY=N POS= 2つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM04(1:P04-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM04) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM04) <= 65536
                   MOVE    FUNCTION NUMVAL(WK-PRM04) TO TBL02-KEY-P (I2)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N POS= 1-65536 範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第３パラメータ
           IF      WK-PRM05    =       "LEN"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY=N LEN= 3つめに指定する"
                   STOP    RUN
           END-IF

           IF    ( WK-PRM06(1:P06-L) IS NUMERIC    AND
                   FUNCTION NUMVAL(WK-PRM06) >= 1  AND
                   FUNCTION NUMVAL(WK-PRM06) <= 32 AND
                   WK-PRM08 (1:2) =    "CH"           ) OR
                 ( WK-PRM06(1:P06-L) IS NUMERIC    AND
                   FUNCTION NUMVAL(WK-PRM06) >= 1  AND
                   FUNCTION NUMVAL(WK-PRM06) <= 10 AND
                   WK-PRM08 (1:2) =    "ZD"           )
                   MOVE    FUNCTION NUMVAL(WK-PRM06) TO TBL02-KEY-L (I2)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N LEN= 1-32(CH),"
                           " 1-10(ZD) 範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第４パラメータ
           IF      WK-PRM07    =       "TYPE"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY=N TYPE= 4つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM08 (1:2) =    "CH" OR "ZD"
                   MOVE    WK-PRM08 (1:2) TO TBL02-TYPE(I2)
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N TYPE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N TYPE= CH,ZD で指定する"
                   STOP    RUN
           END-IF
           .
       S026-EX.
           EXIT.

      *    *** CRE=
       S027-SEC                SECTION.
       S027-10.

           IF      WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 10000000
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO WK-CRE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F CRE= PARA ERROR="
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " CRE= 1-10000000 の範囲で指定する"
                   STOP    RUN
           END-IF
           .
       S027-EX.
           EXIT.

      *    *** RND 作成 AND RELEASE
       S100-SEC                SECTION.
       S100-10.
      *    *** CRE=N で指定した作成回数分繰り返す
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL  J > WK-CRE

      *    *** DT=N で指定した項目、作成回数分繰り返す
               PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > WK-TBL01-MAX

      *             MOVE    TBL01-DT-IDX  (K) TO      M

      *            IF WCR-SEQ >= ZERO AND WCR-SEQ <= 2
      *              DISPLAY " " 
      *              DISPLAY " " 
      *              DISPLAY "WCR-SEQ=" WCR-SEQ
      *              DISPLAY "J      =" J
      *              DISPLAY "K      =" K
      *              DISPLAY "M      =" M
      *              DISPLAY "WCR-FROM    (M)     =" WCR-FROM    (M)
      *              DISPLAY "WCR-TO-CNT  (M)     =" WCR-TO-CNT  (M)
      *              DISPLAY "WCR-BETWEEN (M)     =" WCR-BETWEEN (M)
      *            END-IF

                   IF      TBL01-DT-TYPE(K)  =       "CODE" OR "DTCD"
      *                IF      WCR-FROM (M)  =       ZERO AND
      *                        WCR-TO-CNT(M) =       ZERO AND
      *                        WCR-BETWEEN(M) =      ZERO
                       IF      WCR-FROM    (K)  =    ZERO AND
                               WCR-TO-CNT  (K) =     ZERO AND
                               WCR-BETWEEN (K) =     ZERO
      *    *** その項目初回作成時のみ、TYPE=CODEで指定した値セット
      *    *** ２件目以降は、１件前の内容でサブルーチン内で作成
      *                    MOVE    TBL01-DT-FROM (K) TO   WCR-FROM    (M)
      *                    MOVE    TBL01-DT-TO   (K) TO   WCR-TO-CNT  (M)
      *                    MOVE    TBL01-DT-BET  (K) TO   WCR-BETWEEN (M)
                          MOVE    TBL01-DT-FROM (K) TO   WCR-FROM    (K)
                          MOVE    TBL01-DT-TO   (K) TO   WCR-TO-CNT  (K)
                          MOVE    TBL01-DT-BET  (K) TO   WCR-BETWEEN (K)
                       ELSE
                           CONTINUE
                       END-IF 
                   ELSE
                     IF      TBL01-DT-TYPE(K)  =       "NUM" OR "DATE"
      *                MOVE    TBL01-DT-FROM (K) TO      WCR-FROM2 (M)
      *                MOVE    TBL01-DT-TO   (K) TO      WCR-TO2   (M)
      *                MOVE    TBL01-DT-SIGN (K) TO      WCR-SIGN  (M)
      *                MOVE    TBL01-DT-ZERO (K) TO      WCR-ZERO  (M)
    
                      MOVE    TBL01-DT-FROM (K) TO      WCR-FROM2 (K)
                      MOVE    TBL01-DT-TO   (K) TO      WCR-TO2   (K)
                      MOVE    TBL01-DT-SIGN (K) TO      WCR-SIGN  (K)
                      MOVE    TBL01-DT-ZERO (K) TO      WCR-ZERO  (K)
                     END-IF
                   END-IF

      *            IF WCR-SEQ >= ZERO AND WCR-SEQ <= 2
      *             DISPLAY " " 
      *             DISPLAY "WCR-SEQ=" WCR-SEQ
      *             DISPLAY "J      =" J
      *             DISPLAY "K      =" K
      *             DISPLAY "M      =" M
      *             DISPLAY "TBL01-DT-IDX  (K) =" TBL01-DT-IDX  (K)
      *             DISPLAY "TBL01-DT-TYPE (K) =" TBL01-DT-TYPE (K)
      *             DISPLAY "WCR-FROM    (M)   =" WCR-FROM   (M)
      *             DISPLAY "WCR-TO-CNT  (M)   =" WCR-TO-CNT (M)
      *             DISPLAY "WCR-BETWEEN (M)   =" WCR-BETWEEN(M)
      *             DISPLAY "WCR-FROM2 (M)     =" WCR-FROM2(M)
      *             DISPLAY "WCR-TO2   (M)     =" WCR-TO2  (M)
      *             DISPLAY "WCR-SIGN (M)      =" WCR-SIGN (M)
      *             DISPLAY "WCR-ZERO (M)      =" WCR-ZERO (M)
      *            END-IF

               END-PERFORM
      *    *** サブルーチン内でWK-IDX-MAXで指定された値、作成を繰り返し、
      *    *** 各項目のIDX=Nで指定した、Nの場所の値をセットする
               MOVE    WK-IDX-MAX  TO      WCR-IDX
               MOVE    "RND"       TO      WCR-ID
               CALL    "COBRND"    USING   WCR-COBRND-AREA

               MOVE    SPACE       TO      SIO1-REC
               PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > WK-TBL01-MAX

                   MOVE    TBL01-DT-P (K) TO P
                   MOVE    TBL01-DT-L (K) TO L
      *             MOVE    TBL01-DT-IDX (K) TO M
                   COMPUTE N = 11 - L

                   EVALUATE TRUE
                       WHEN TBL01-DT-TYPE(K) = "CODE"
      *                     MOVE    WCR-FROM (M)   TO   WK-DATA-9
                           MOVE    WCR-FROM (K)   TO   WK-DATA-9
                           MOVE    WK-DATA-9(N:L) TO   SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "NUM"
                         IF  TBL01-DT-SIGN (K) = "N"
      *                     MOVE    WCR-NUM (M)    TO   WK-DATA-9
                           MOVE    WCR-NUM (K)    TO   WK-DATA-9
                           MOVE    WK-DATA-9(N:L) TO   SIO1-DATA (P:L)
                         ELSE
      *                     MOVE    WCR-NUM (M) TO      WK-DATA-S9
                           MOVE    WCR-NUM (K) TO      WK-DATA-S9
                           MOVE    WK-DATA-S9(N:L) TO  SIO1-DATA (P:L)
                         END-IF
                       WHEN TBL01-DT-TYPE(K) = "KANA"
                           MOVE    WCR-KANA (K) TO     SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "ALPHA"
                           MOVE    WCR-ALPHA (K) TO    SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "SNAME"
                           MOVE    WCR-S-NAME (K) TO   SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "SUTF8"
                           MOVE    WCR-S-NAME8(K) TO   SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "DATE"
                           MOVE    "R"         TO      WDW-DATE2-ID
                           MOVE    WCR-NUM (K) TO      WK-DATA-9
                           MOVE    WK-DATA-9(N:L) TO   WDW-NISUU
                           CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                           MOVE    WDW-DATE2-YMD TO    SIO1-DATA (P:L)
                       WHEN TBL01-DT-TYPE(K) = "DTCD"
                           MOVE    "R"         TO      WDW-DATE2-ID
                           MOVE    WCR-FROM (K) TO     WK-DATA-9
                           MOVE    WK-DATA-9(N:L) TO   WDW-NISUU
                           CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                           MOVE    WDW-DATE2-YMD TO    SIO1-DATA (P:L)
                   END-EVALUATE

                   IF      SW-CSV      =       "Y"
                       MOVE    ","         TO      SIO1-DATA(P+L:1)
                   END-IF
               END-PERFORM

      *    *** KEY=N指定無い時、ＳＯＲＴしないで出力する
           
               IF      WK-PRM      =       SPACE
                       MOVE    SIO1-DATA   TO      POT1-DATA
                       WRITE   POT1-REC
                       ADD     1           TO      WK-POT1-CNT

                       IF      WK-POT1-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME
                               " POT1-F WRITE ERROR STATUS="
                               WK-POT1-STATUS
                               STOP    RUN
                       END-IF

                       MOVE    WK-POT1-CNT TO      WK-DATA
                       IF      WK-DATA     =       ZERO
                           MOVE    "WRITE"     TO      WK-COM-I1
                           MOVE    WK-POT1-CNT TO      WK-COM-SU
                           MOVE    WK-COM      TO      WDT-DATE-LUP-COM
                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
                       END-IF
               ELSE
      *    *** KEY 指定有はKEY1 必須
                   MOVE    TBL02-KEY-P (1) TO P1
                   MOVE    TBL02-KEY-L (1) TO L1

                   MOVE    TBL02-KEY-P (2) TO P2
                   MOVE    TBL02-KEY-L (2) TO L2

                   MOVE    TBL02-KEY-P (3) TO P3
                   MOVE    TBL02-KEY-L (3) TO L3

      *    *** KEY マイナスを考慮し、10,000,000 加えた値 ＫＥＹにセット
                   IF      TBL02-TYPE (1) = "ZD"
                       COMPUTE N = 11 - L1
                       MOVE    ZERO        TO      SIO1-KEY1-X
                       MOVE    SIO1-DATA(P1:L1) TO SIO1-KEY1-X (N:L1)
                   ELSE
                       MOVE    SIO1-DATA(P1:L1) TO SIO1-KEY1-X
                   END-IF

                   IF      WK-PRM(2:1) =         "A" OR "D"
                       IF      TBL02-TYPE (2) = "ZD"
                           COMPUTE N = 11 - L2
                           MOVE    ZERO        TO      SIO1-KEY2-X
                           MOVE    SIO1-DATA(P2:L2) TO SIO1-KEY2-X(N:L2)
                       ELSE
                           MOVE    SIO1-DATA(P2:L2) TO SIO1-KEY2-X
                       END-IF
                   END-IF 

                   IF      WK-PRM(3:1) =         "A" OR "D"
                       IF      TBL02-TYPE (3) = "ZD"
                           COMPUTE N = 11 - L3
                           MOVE    ZERO        TO      SIO1-KEY3-X
                           MOVE    SIO1-DATA(P3:L3) TO SIO1-KEY3-X(N:L3)
                       ELSE
                           MOVE    SIO1-DATA(P3:L3) TO SIO1-KEY3-X
                       END-IF
                   END-IF

                   RELEASE SIO1-REC
                   ADD     1           TO      WK-SIO1-RL-CNT
                   IF      WK-SIO1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME 
                                   " SIO1-F RELEASE ERROR STATUS="
                                   WK-SIO1-STATUS
                                   " WK-SIO1-RL-CNT=" WK-SIO1-RL-CNT
                           STOP    RUN
                   END-IF
                   MOVE    WK-SIO1-RL-CNT TO   WK-DATA
                   IF      WK-DATA     =       ZERO
                       MOVE    "RELEASE"   TO      WK-COM-I1
                       MOVE    WK-SIO1-RL-CNT TO   WK-COM-SU
                       MOVE    WK-COM      TO      WDT-DATE-LUP-COM
                       MOVE    "L"         TO      WDT-DATE-TIME-ID
                       CALL    "DATETIME"  USING   WDT-DATETIME-AREA
                   END-IF
               END-IF

           END-PERFORM

           IF      WK-PRM      NOT =   SPACE
                   MOVE    "RELEASEend" TO     WK-COM-I1
                   MOVE    WK-SIO1-RL-CNT TO   WK-COM-SU
                   MOVE    WK-COM      TO      WDT-DATE-LUP-COM
                   MOVE    "L"         TO      WDT-DATE-TIME-ID
                   CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.

           PERFORM UNTIL   WK-SIO1-EOF   =     HIGH-VALUE
                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                       NOT AT END
                           ADD     1           TO      WK-SIO1-RT-CNT
                           MOVE    SIO1-DATA   TO      POT1-DATA

                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT

                           IF      WK-POT1-STATUS NOT =  ZERO
                                   DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                                   STOP    RUN
                           END-IF
                   END-RETURN
                   IF      WK-SIO1-STATUS NOT =  ZERO AND 10
                           DISPLAY WK-PGM-NAME 
                                   " SIO1-F RETURN ERROR STATUS="
                                   WK-SIO1-STATUS
                                   " WK-SIO1-RT-CNT=" WK-SIO1-RT-CNT
                           STOP    RUN
                   END-IF
                   IF      WK-SIO1-RT-CNT =      1
                       MOVE    "SORT END  " TO     WK-COM-I1
                       MOVE    WK-SIO1-RT-CNT TO   WK-COM-SU
                       MOVE    WK-COM      TO      WDT-DATE-LUP-COM
                       MOVE    "L"         TO      WDT-DATE-TIME-ID
                       CALL    "DATETIME"  USING   WDT-DATETIME-AREA
                   END-IF
           END-PERFORM

           MOVE    "RETURN END" TO     WK-COM-I1
           MOVE    WK-SIO1-RT-CNT TO   WK-COM-SU
           MOVE    WK-COM      TO      WDT-DATE-LUP-COM
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
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

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-SIO1-RL-CNT TO WK-SIO1-RL-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RLｹﾝｽｳ= " WK-SIO1-RL-CNT-E
                   " (" WK-SIO1-F-NAME ")"
           MOVE    WK-SIO1-RT-CNT TO WK-SIO1-RT-CNT-E
           DISPLAY WK-PGM-NAME " SIO1RTｹﾝｽｳ= " WK-SIO1-RT-CNT-E
                   " (" WK-SIO1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
