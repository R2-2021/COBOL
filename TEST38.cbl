      *    *** SORT BINARY SEQUENTIAL FILE用
      *    *** ＰＩＮ１ ＤＡＴＡ＋ＣＲ+ＬＦ＝８０バイト用
      *    *** 
      *    *** レコード長、可変のテストしたが、バイナリーファイルの時、
      *    *** 一つのＦＤ文で入力出来ないので、レコード長ごとに
      *    *** 作成作成する必要あるが、無駄なので作成しない
      *    *** パック項目をアンパックに変換すれば、COBSORTでどんな
      *    *** レコード長でも対応可能である
      *    *** 
      *    *** ＣＳＶは基本、レコード可変長になるので、
      *    *** 固定長に変換してないと
      *    *** このプログラムは使えない 
      *    *** TEST42,TEST43でCSV可変長を固定長に変換出来る

      *    *** SORT LINE SEQUENTIAL FILE用はCOBSORT

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST38.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT パラメータ
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORT 前データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

      *    *** SORTWORK
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORT 後データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
           03  PRM1-PRM        PIC  X(003).
           03  FILLER          PIC  X(077).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.

       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(080).

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
      *    *** PACK 6バイト、11桁なので、11桁にする、CH,ZDは10桁まで
           03  SIO1-KEY1-X     PIC  X(011).
           03  SIO1-KEY1-9     REDEFINES SIO1-KEY1-X
                               PIC S9(011).

           03  SIO1-KEY2-X     PIC  X(011).
           03  SIO1-KEY2-9     REDEFINES SIO1-KEY2-X
                               PIC S9(011).

           03  SIO1-KEY3-X     PIC  X(011).
           03  SIO1-KEY3-9     REDEFINES SIO1-KEY3-X
                               PIC S9(011).

           03  SIO1-DATA       PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST38  ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST38.PRM1".
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

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM
             05 WK-PRM1        PIC  X(001) VALUE SPACE.
             05 WK-PRM2        PIC  X(001) VALUE SPACE.
             05 WK-PRM3        PIC  X(001) VALUE SPACE.

      *    *** ＰＡＣＫはこのファイル入力ではエラーになるので、
      *    *** ORGANIZATION BINARY   SEQUENTIALなので.
      *    *** ＰＤ指定可とする
           03  WK-KEY-CHAR.
             05  WK-KEY1-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY2-CHAR  PIC  X(002) VALUE SPACE.
             05  WK-KEY3-CHAR  PIC  X(002) VALUE SPACE.
           03  WK-KEY-CHAR2.
             05  WK-KEY1-CHAR2 PIC  X(002) VALUE SPACE.
             05  WK-KEY2-CHAR2 PIC  X(002) VALUE SPACE.
             05  WK-KEY3-CHAR2 PIC  X(002) VALUE SPACE.

           03  WK-PSU          PIC S9(011) PACKED-DECIMAL VALUE ZERO.
           03  WK-PSU-X        REDEFINES WK-PSU
                               PIC  X(006).

           03  WK-KEY          BINARY-LONG SYNC VALUE ZERO.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.

           03  WK-CSVDT.
             05  WK-CSVDT01    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT02    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT03    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT04    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT05    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT06    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT07    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT08    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT09    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT10    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT11    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT12    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT13    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT14    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT15    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT16    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT17    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT18    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT19    PIC  X(010) VALUE SPACE.
             05  WK-CSVDT20    PIC  X(010) VALUE SPACE.
           03  WK-CSVDT-R      REDEFINES WK-CSVDT.
             05  WK-CSVDT-T    OCCURS 20
                               PIC  X(010).

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

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.

           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
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

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.
 
      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PRM1
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
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PRM1
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
      *                         P           <=       65536
                               P           <=       80
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N POS= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Nの時、"
      *                             " KEY=N POS= 1-65536の範囲で指定"
                                   " KEY=N POS= 1-80の範囲で指定"
                           STOP    RUN
                       END-IF

                   ELSE
                       IF      P           >=      1 AND
                               P           <=      20
                           CONTINUE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N POS= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Yの時、"
                                   "KEY=N POS= 1-20の範囲で指定"
                           STOP    RUN
                       END-IF
                   END-IF
           END-IF

           IF      WK-PRM1DT05(1:3) = "LEN"
                   MOVE    FUNCTION NUMVAL(WK-PRM1DT06) TO L

                   IF      L           >=       1 AND
                           L           <=       10
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N LEN= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " KEY=N LEN= 1-10の範囲で指定"
                           STOP    RUN
                   END-IF
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
                   IF      WK-PRM1DT08(1:2) =  "CH" OR "ZD" OR "PD"
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N TYPE= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " KEY=N TYPE= CH,ZD,PD で指定"
                           STOP    RUN
                   END-IF
           END-IF

           IF      WK-PRM1DT07(1:4) = "TYPE" AND
                   WK-PRM1DT08(1:2) =   "PD"
                   IF      L           >=       1 AND
                           L           <=       6
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F KEY=N LEN= PARA ERROR"
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " KEY=N TYPE=PD LEN= 1-6の範囲で指定"
                           STOP    RUN
                   END-IF
           END-IF

      *    *** CSV=Y 時 ,"PD項目", PD項目は引用符でくくる予定だが、
      *    *** 利用用途無いため、現在はエラーにする
           IF      WK-PRM1DT08(1:2) = "PD" AND
                   SW-CSV      =     "Y"
                   DISPLAY WK-PGM-NAME
                           " PRM1-F KEY=N TYPE= PARA ERROR"
                           PRM1-REC
                   DISPLAY WK-PGM-NAME
                           " KEY=N TYPE=PD CSV=Yの時、使用不可"
                           " カンマ(,)は16進数でX'2C'となり、"
                           "カンマで項目分割出来ない為"
                   STOP    RUN
           END-IF

           EVALUATE WK-PRM1DT02(1:1)
               WHEN "1"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY1-CHAR
                                                  WK-KEY1-CHAR2
      *    *** TYPE=PD 指定の時、ZDと同じソートキーエリア使用する
                    IF      WK-PRM1DT08(1:2) =   "PD"
                        MOVE    "ZD"         TO   WK-KEY1-CHAR
                    END-IF
               WHEN "2"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY2-CHAR
                                                  WK-KEY2-CHAR2
                    IF      WK-PRM1DT08(1:2) =   "PD"
                        MOVE    "ZD"         TO   WK-KEY2-CHAR
                    END-IF
               WHEN "3"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY3-CHAR
                                                  WK-KEY3-CHAR2
                    IF      WK-PRM1DT08(1:2) =   "PD"
                        MOVE    "ZD"         TO   WK-KEY3-CHAR
                    END-IF
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
                   DISPLAY WK-PGM-NAME " PRM1-F CSV= N,Y で指定"
                   DISPLAY PRM1-REC
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

      *    *** READ AND RELEASE
       S100-SEC                SECTION.
       S100-10.

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                       ADD     1           TO        WK-PIN1-CNT

                       IF      SW-CSV    =    "Y"
      *    *** CSV の時
                               PERFORM S110-10     THRU    S110-EX
                       ELSE
      *    *** CSV 以外の時
                               PERFORM S120-10     THRU    S120-EX
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

      *    *** CSV の時
       S110-SEC                SECTION.
       S110-10.

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
           MOVE    SPACE       TO      SIO1-KEY1-X
           MOVE    SPACE       TO      SIO1-KEY2-X
           MOVE    SPACE       TO      SIO1-KEY3-X

      *    *** KEY1= は必須 PRM2 参照しない
           IF      WK-KEY1-CHAR2 =     "CH"
                   MOVE    WK-CSVDT-T(P1) TO     SIO1-KEY1-X
           ELSE
      *    *** CSV=Y,TYPE=PD は項目チェックでエラーにした
                   IF      WK-KEY1-CHAR2 =     "PD"
                           MOVE    ZERO        TO      WK-PSU
                           MOVE    WK-CSV-L(P1) TO     L1

                           COMPUTE P1X = 7 - L1
                           MOVE    WK-CSVDT-T(P1) (1:L1) TO
                                   WK-PSU-X(P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                           PERFORM S130-10     THRU    S130-EX
                           MOVE    WK-PSU      TO      SIO1-KEY1-9
                   ELSE
                           MOVE    ZERO        TO      SIO1-KEY1-9
                           MOVE    WK-CSV-L(P1) TO     L1

                           COMPUTE P1X = 12 - L1
                           MOVE    WK-CSVDT-T(P1) (1:L1) TO
                                   SIO1-KEY1-9 (P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                           PERFORM S140-10     THRU    S140-EX
                   END-IF
           END-IF

           IF      WK-PRM2     =       "A" OR "D"
                   IF      WK-KEY2-CHAR2 =     "CH"
                           MOVE    WK-CSVDT-T(P2) TO   SIO1-KEY2-X
                   ELSE
                           IF      WK-KEY2-CHAR2 =     "PD"
                                   MOVE    ZERO        TO      WK-PSU
                                   MOVE    WK-CSV-L(P2) TO     L2

                                   COMPUTE P2X = 7 - L2
                                   MOVE    WK-CSVDT-T(P2) (1:L2) TO
                                           WK-PSU-X(P2X:L2)
                                   MOVE    2           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY2-9
                           ELSE
                                   MOVE    ZERO        TO    SIO1-KEY2-9
                                   MOVE    WK-CSV-L(P2) TO      L2

                                   COMPUTE P2X = 12 - L2
                                   MOVE    WK-CSVDT-T(P2) (1:L2) TO
                                           SIO1-KEY2-9 (P2X:L2)
                                   MOVE    2           TO       WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S140-10     THRU     S140-EX
                           END-IF
                   END-IF
           END-IF 

           IF      WK-PRM3     =       "A" OR "D"
                   IF      WK-KEY3-CHAR2 =     "CH"
                           MOVE    WK-CSVDT-T(P3) TO      SIO1-KEY3-X
                   ELSE
                           IF      WK-KEY3-CHAR2 =     "PD"
                                   MOVE    ZERO        TO      WK-PSU
                                   MOVE    WK-CSV-L(P3) TO     L3

                                   COMPUTE P3X = 7 - L3
                                   MOVE    WK-CSVDT-T(P3) (1:L3) TO
                                           WK-PSU-X(P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY3-9
                                 ELSE
                                   MOVE    ZERO        TO    SIO1-KEY3-9
                                   MOVE    WK-CSV-L(P3) TO     L3
                                   COMPUTE P3X = 12 - L3
                                   MOVE    WK-CSVDT-T(P3) (1:L3) TO
                                           SIO1-KEY3-9 (P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S140-10     THRU    S140-EX
                           END-IF
                   END-IF
           END-IF

           MOVE    PIN1-DATA   TO      SIO1-DATA
           RELEASE SIO1-REC
           .
       S110-EX.
           EXIT.

      *    *** CSV 以外の時
       S120-SEC                SECTION.
       S120-10.

           MOVE    SPACE       TO      SIO1-KEY1-X
           MOVE    SPACE       TO      SIO1-KEY2-X
           MOVE    SPACE       TO      SIO1-KEY3-X

           IF      WK-KEY1-CHAR2 =     "CH"
                   MOVE    PIN1-REC(P1:L1) TO  SIO1-KEY1-X
           ELSE
      *    *** CSV=N の時、TYPE=PD 指定可
                   IF      WK-KEY1-CHAR2 =     "PD"
                           MOVE    ZERO        TO      WK-PSU
                           COMPUTE P1X = 7 - L1
                           MOVE    PIN1-REC (P1:L1) TO  WK-PSU-X(P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                           PERFORM S130-10 THRU S130-EX
                           MOVE    WK-PSU      TO      SIO1-KEY1-9
                   ELSE
                           MOVE    ZERO        TO      SIO1-KEY1-9
                           COMPUTE P1X = 12 - L1
                           MOVE    PIN1-REC (P1:L1) TO 
                                   SIO1-KEY1-9 (P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                           PERFORM S140-10     THRU    S140-EX
                   END-IF
           END-IF

           IF      WK-PRM2     =       "A" OR "D"
                   IF      WK-KEY2-CHAR2 =     "CH"
                           MOVE   PIN1-REC (P2:L2) TO  SIO1-KEY2-X
                   ELSE
                           IF      WK-KEY2-CHAR2 =     "PD"
                                   MOVE    ZERO        TO      WK-PSU
                                   COMPUTE P2X = 7 - L2
                                   MOVE    PIN1-REC (P2:L2) TO
                                           WK-PSU-X (P2X:L2)
                                   MOVE    2           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY2-9
                           ELSE
                                   MOVE    ZERO      TO     SIO1-KEY2-9
                                   COMPUTE P2X = 12 - L2
                                   MOVE    PIN1-REC(P2:L2) 
                                                 TO SIO1-KEY2-9 (P2X:L2)
                                   MOVE    2         TO     WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S140-10   THRU   S140-EX
                           END-IF
                   END-IF
           END-IF

           IF      WK-PRM3     =       "A" OR "D"
                   IF      WK-KEY3-CHAR2 =     "CH"
                           MOVE   PIN1-REC(P3:L3) TO   SIO1-KEY3-X
                   ELSE
                           IF      WK-KEY3-CHAR2 =     "PD"
                                   MOVE    ZERO        TO      WK-PSU
                                   COMPUTE P3X = 7 - L3
                                   MOVE    PIN1-REC (P3:L3) TO
                                           WK-PSU-X (P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=PD 用 項目チェック
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY3-9
                           ELSE
                                   MOVE    ZERO        TO    SIO1-KEY3-9
                                   COMPUTE P3X = 12 - L3
                                   MOVE    PIN1-REC (P3:L3) TO
                                           SIO1-KEY3-9 (P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=ZD 用 項目チェック
                                   PERFORM S140-10     THRU    S140-EX

                           END-IF
                   END-IF
           END-IF

      *     IF WK-PIN1-CNT >= 0 AND <=10
      *       DISPLAY WK-PIN1-CNT
      *       DISPLAY SIO1-KEY1-X " " SIO1-KEY2-X  " " SIO1-KEY3-X
      *     END-IF

           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNTX
           IF      WK-PIN1-CNTX =      ZERO
                   DISPLAY WK-PGM-NAME " PIN1-CNT=" WK-PIN1-CNT
           END-IF

      *     MOVE "P" TO WFD-ID
      *     CALL "FILEDUMP" USING WFD-FILEDUMP-AREA
      *                           PIN1-REC 

           MOVE    PIN1-DATA   TO      SIO1-DATA
           RELEASE SIO1-REC
           .
       S120-EX.
           EXIT.

      *    *** TYPE=PD 用 項目チェック
       S130-SEC                SECTION.
       S130-10.

           IF      WK-KEY      =       1
               IF      WK-PSU NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC KEY1 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR2
                   CALL    "COBDUMP" USING     WK-PSU
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      WK-PSU NOT NUMERIC
                       DISPLAY WK-PGM-NAME 
                           DISPLAY WK-PGM-NAME 
                           " PIN1-REC KEY2 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                           " TYPE=" WK-KEY2-CHAR2
                       CALL    "COBDUMP" USING     WK-PSU
                       STOP    RUN
                   END-IF
               ELSE
                   IF      WK-PSU NOT NUMERIC
                       DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY3 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                           " TYPE=" WK-KEY3-CHAR2
                       CALL    "COBDUMP" USING     WK-PSU
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
                           " TYPE=" WK-KEY1-CHAR2
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      L2          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=2 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                               " TYPE=" WK-KEY2-CHAR2
                       STOP    RUN
                   END-IF
               ELSE
                   IF      L3          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=3 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                               " TYPE=" WK-KEY3-CHAR2
                       STOP    RUN
                   END-IF
               END-IF
           END-IF           .
       S130-EX.
           EXIT.

      *    *** TYPE=ZD 用 項目チェック
       S140-SEC                SECTION.
       S140-10.

           IF      WK-KEY      =       1
               IF      SIO1-KEY1-9 NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC KEY1 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR2
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
                           " TYPE=" WK-KEY2-CHAR2
                       CALL    "COBDUMP" USING     SIO1-KEY2-9
                       STOP    RUN
                   END-IF
               ELSE
                   IF      SIO1-KEY3-9 NOT NUMERIC
                       DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY3 NOT NUMERIC"
                           " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                           " TYPE=" WK-KEY3-CHAR2
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
                           " TYPE=" WK-KEY1-CHAR2
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      L2          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=2 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                               " TYPE=" WK-KEY2-CHAR2
                       STOP    RUN
                   END-IF
               ELSE
                   IF      L3          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=3 POS=の項目長さゼロ"
                               " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                               " TYPE=" WK-KEY3-CHAR2
                       STOP    RUN
                   END-IF
               END-IF
           END-IF
           .
       S140-EX.
           EXIT.

      *    *** RETURN AND WRITE
       S200-SEC                SECTION.
       S200-10.
           PERFORM UNTIL WK-SIO1-EOF = HIGH-VALUE
                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                       NOT AT END
                           ADD     1           TO      WK-POT1-CNT
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

           DISPLAY WK-PGM-NAME " END".
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 ｹﾝｽｳ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
