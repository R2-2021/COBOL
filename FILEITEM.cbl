      *    *** 項目一覧の出力、サマリーも出力
      *    *** ＰＲＭ１で出力項目等指定
      *    *** ＰＲＭ２で縦、横出力、明細有り、無し等指定
      *    *** PRINT AREA 2次元でセット　（４０行／１ページ）
      *    *** 改行数、Ａ４縦、Ａ４横　プログラムで設定
      *    *** KEY ブレイク追加
      *    *** PIN3-F マスター入力、UTF8= まだ、未完成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILEITEM.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       SPECIAL-NAMES.
           CURRENCY SIGN IS "\".

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** FILEITEM パラメータデータ１ 出力項目、集計キー等指定
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** FILEITEM パラメータデータ２　Ａ４　縦横、明細有無、改行等
       SELECT PRM2-F           ASSIGN   WK-PRM2-F-NAME
                               STATUS   WK-PRM2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 通常ファイル入力用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PACKED-DECIMAL 用 BINARY レコード最後にX"0D0A"必要
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

      *    *** 現在未使用、マスター入力用、ＩＮＤＥＸファイル入力用
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION INDEXED
           ACCESS RANDOM
           RECORD KEY PIN3-KEY.

      *    *** FILEITEM ダンププリント
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
             05  FILLER        PIC  X(128).

       FD  PRM2-F
           LABEL RECORDS ARE STANDARD.
       01  PRM2-REC.
             05  PRM2-A4TATE   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM2-MEISAI   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM2-KAIGYO   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM2-KAIGYO-BR PIC X(001).
             05  FILLER        PIC  X(001).
             05  PRM2-MOJISU   PIC  9(003).
             05  FILLER        PIC  X(001).
             05  PRM2-GYOU     PIC  9(003).
             05  FILLER        PIC  X(001).
             05  PRM2-FONT     PIC  9(002).
             05  FILLER        PIC  X(062).

      *    *** LINE SEQUENTIAL 
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(32760).

      *    *** BINARY SEQUENTIAL 
       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LENX.
       01  PIN2-REC.
           03  FILLER          PIC  X(32760).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD.
       01  PIN3-REC
           03  PIN3-KEY        PIC  9(004).
           03  PIN3-DATA       PIC  X(2048).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(300).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILEITEM".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "FILEITEM.PRM1".
           03  WK-PRM2-F-NAME  PIC  X(032) VALUE "FILEITEM.PRM2".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "FILEDTCR.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "TEST28_201110_201810.csv".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "FILEDTCR.POT2".
      *    *** INDEX FILE
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST22.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "FILEITEM.POT1".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PRM2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PRM1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE 10000.
           03  WK-PIN2-LENX    BINARY-LONG SYNC VALUE 10000.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PRM2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PRM2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.
           03  WK-TBL01-MAX    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PN1          BINARY-LONG SYNC VALUE 1.

           03  WK-SEQNO-X      PIC  X(010) VALUE SPACE.
           03  WK-SEQNO        PIC  9(010) VALUE ZERO.
           03  WK-F-IN-MODE    PIC  X(001) VALUE "L".
           03  WK-F-OT-MODE    PIC  X(001) VALUE "O".
           03  WK-LISTID       PIC  X(060) VALUE SPACE.

           03  WK-KEY1-MID     PIC  X(010) VALUE "**KEY1小計".
           03  WK-KEY2-MID     PIC  X(010) VALUE "**KEY2小計".
           03  WK-KEY3-MID     PIC  X(010) VALUE "**KEY3小計".
           03  WK-KEY4-MID     PIC  X(010) VALUE "**総  合計".

           03  WK-KENSU        PIC  X(004) VALUE "件数".

      *    *** UTF8
      *    *** 件数
           03  WK-KENSU8       PIC  X(006) VALUE 
               X"E4BBB6E695B0".
      *    *** ＫＥＹ１計
           03  WK-KEY1-MID8    PIC  X(015) VALUE
               X"EFBCABEFBCA5EFBCB9EFBC91E8A888".
      *    *** ＫＥＹ２計
           03  WK-KEY2-MID8    PIC  X(015) VALUE
               X"EFBCABEFBCA5EFBCB9EFBC92E8A888".
      *    *** ＫＥＹ３計
           03  WK-KEY3-MID8    PIC  X(015) VALUE 
               X"EFBCABEFBCA5EFBCB9EFBC93E8A888".
      *    *** ＊＊総合計
           03  WK-KEY4-MID8    PIC  X(015) VALUE
               X"EFBC8AEFBC8AE7B78FE59088E8A888".

           03  WK-KEY1         PIC  X(010) VALUE SPACE.
           03  WK-KEY2         PIC  X(010) VALUE SPACE.
           03  WK-KEY3         PIC  X(010) VALUE SPACE.

           03  WK-PRM          PIC  X(003) VALUE SPACE.
           03  WK-ZSU-X        PIC  X(011) VALUE ZERO.
           03  WK-ZSU          REDEFINES WK-ZSU-X
                               PIC S9(011).
           03  WK-ZSU-1        PIC S9(011)V9   VALUE ZERO.
           03  WK-ZSU-2        PIC S9(011)V99  VALUE ZERO.
           03  WK-ZSU-3        PIC S9(011)V999 VALUE ZERO.

           03  WK-PSU          PIC S9(011) PACKED-DECIMAL
                                           VALUE ZERO.
           03  WK-PSU-X        REDEFINES WK-PSU
                               PIC  X(006).

           03  WK-TIT          PIC  X(060) VALUE SPACE.
           03  WK-DATA3        PIC  X(010) VALUE SPACE.
           03  WK-ARGUMENT-NUMBER BINARY-LONG SYNC VALUE ZERO.
           03  WK-END-BYTE     PIC  X(001) VALUE LOW-VALUE.

           03  WK-KISETU       PIC  X(002) VALUE SPACE.
           03  WK-TITLE        PIC  X(020) VALUE SPACE.
           03  WK-TITLE2       PIC  X(020) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.

           03  WK-KEY1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-KEY2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-KEY3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.
           03  WK-CNT2         PIC  -ZZ,ZZZ,ZZZ,ZZ9 VALUE SPACE.
           03  WK-CNT3         PIC  ZZZ,ZZZ,ZZ9 VALUE SPACE.

           03  WK-ESU-99       PIC  99999999999-     VALUE SPACE.
           03  WK-ESU-9K       PIC  99,999,999,999-  VALUE SPACE.
           03  WK-ESU-ZZ       PIC  -ZZZZZZZZZZ9     VALUE SPACE.
           03  WK-ESU-ZK       PIC  -ZZ,ZZZ,ZZZ,ZZ9  VALUE SPACE.
           03  WK-ESU-EE       PIC  -\\\\\\\\\\\9    VALUE SPACE.
           03  WK-ESU-EK       PIC  -\\\,\\\,\\\,\\9 VALUE SPACE.

           03  WK-ESU-99-1     PIC  9999999999.9-    VALUE SPACE.
           03  WK-ESU-9K-1     PIC  9,999,999,999.9- VALUE SPACE.
           03  WK-ESU-ZZ-1     PIC  -ZZZZZZZZZ9.9    VALUE SPACE.
           03  WK-ESU-ZK-1     PIC  -Z,ZZZ,ZZZ,ZZ9.9 VALUE SPACE.
           03  WK-ESU-EE-1     PIC  -\\\\\\\\\\9.9   VALUE SPACE.
           03  WK-ESU-EK-1     PIC  -\\,\\\,\\\,\\9.9 VALUE SPACE.

           03  WK-ESU-99-2     PIC  999999999.99-    VALUE SPACE.
           03  WK-ESU-9K-2     PIC  999,999,999.99-  VALUE SPACE.
           03  WK-ESU-ZZ-2     PIC  -ZZZZZZZZ9.99    VALUE SPACE.
           03  WK-ESU-ZK-2     PIC  -ZZZ,ZZZ,ZZ9.99  VALUE SPACE.
           03  WK-ESU-EE-2     PIC  -\\\\\\\\\9.99   VALUE SPACE.
           03  WK-ESU-EK-2     PIC  -\\\\,\\\,\\9.99 VALUE SPACE.

           03  WK-ESU-99-3     PIC  99999999.999-    VALUE SPACE.
           03  WK-ESU-9K-3     PIC  99,999,999.999-  VALUE SPACE.
           03  WK-ESU-ZZ-3     PIC  -ZZZZZZZ9.999    VALUE SPACE.
           03  WK-ESU-ZK-3     PIC  -ZZ,ZZZ,ZZ9.999  VALUE SPACE.
           03  WK-ESU-EE-3     PIC  -\\\\\\\\9.999   VALUE SPACE.
           03  WK-ESU-EK-3     PIC  -\\\,\\\,\\9.999 VALUE SPACE.

           03  WK-PIN3-I1      PIC  X(080) VALUE SPACE.
           03  WK-PIN3-I2      PIC  X(080) VALUE SPACE.
           03  WK-PIN3-I3      PIC  X(075) VALUE SPACE.
           03  WK-PIN3-I4      PIC  X(1600) VALUE SPACE.

           03  WK-PRM01        PIC  X(010) VALUE SPACE.
      *    *** 帳票タイトル、ファイル名あるので、60BYTE
           03  WK-PRM02        PIC  X(060) VALUE SPACE.
           03  WK-PRM03        PIC  X(010) VALUE SPACE.
           03  WK-PRM04        PIC  X(010) VALUE SPACE.
           03  WK-PRM05        PIC  X(010) VALUE SPACE.
           03  WK-PRM06        PIC  X(010) VALUE SPACE.
           03  WK-PRM07        PIC  X(010) VALUE SPACE.
           03  WK-PRM08        PIC  X(010) VALUE SPACE.
           03  WK-PRM09        PIC  X(010) VALUE SPACE.
           03  WK-PRM10        PIC  X(010) VALUE SPACE.
           03  WK-PRM11        PIC  X(010) VALUE SPACE.
      *    *** UTF8 対応
           03  WK-PRM12        PIC  X(015) VALUE SPACE.
           03  WK-PRM13        PIC  X(010) VALUE SPACE.
           03  WK-PRM14        PIC  X(010) VALUE SPACE.
           03  WK-PRM15        PIC  X(010) VALUE SPACE.
           03  WK-PRM16        PIC  X(010) VALUE SPACE.
           03  WK-PRM17        PIC  X(010) VALUE SPACE.
           03  WK-PRM18        PIC  X(010) VALUE SPACE.
           03  WK-PRM19        PIC  X(010) VALUE SPACE.
           03  WK-PRM20        PIC  X(010) VALUE SPACE.

           03  WK-TIT1.
             05  WK-TIT1-TIT   PIC  X(060) VALUE SPACE.
      *           "＊＊＊　ＸＸＸＸＸＸＸＸＸＸ　＊＊＊　　".
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-YY    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-MM    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE "/".
             05  WK-TIT1-DD    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-HH    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-MI    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(001) VALUE ":".
             05  WK-TIT1-SS    PIC  9(002) VALUE ZERO.
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.

           03  WK-TIT1-A4T.
             05                PIC  X(018) VALUE SPACE.
             05  WK-TIT1-A4T-1 PIC  X(088) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05                PIC  X(018) VALUE SPACE.
             05  WK-TIT1-A4Y-1 PIC  X(088) VALUE SPACE.

           03  WK-TIT2-A4T.
             05  WK-TIT2-LIDT  PIC  X(060) VALUE "FILEITEM-T".

           03  WK-TIT2-A4Y.
             05  WK-TIT2-LIDY  PIC  X(060) VALUE "FILEITEM-Y".

      *    *** MAX=190,A4縦用
      *    *** LEN*N=NNN
           03  WK-MID1-A4T     PIC  X(200) VALUE SPACE.
           03  WK-HAI-A4T      PIC  X(200) VALUE SPACE.

      *    *** MAX=277,A4横用
      *    *** LEN*N=NNN
           03  WK-MID1-A4Y     PIC  X(300) VALUE SPACE.
           03  WK-HAI-A4Y      PIC  X(300) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE07  REPLACING ==:##:== BY ==WDE07==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  Left-Nibble         BINARY-LONG SYNC VALUE ZERO.
       01  Right-Nibble        BINARY-LONG SYNC VALUE ZERO.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 131
                               PIC  X(277) VALUE SPACE.

       01  KEY-AREA.

             05  KEY-OLD.
               07  KEY-OLD2.
                 09  KEY-OKEY1 PIC  X(010) VALUE LOW-VALUE.
                 09  KEY-OKEY2 PIC  X(010) VALUE LOW-VALUE.
               07  KEY-OKEY3   PIC  X(010) VALUE LOW-VALUE.

             05  KEY-NEW.
               07  KEY-NEW2.
                 09  KEY-NKEY1 PIC  X(010) VALUE LOW-VALUE.
                 09  KEY-NKEY2 PIC  X(010) VALUE LOW-VALUE.
               07  KEY-NKEY3   PIC  X(010) VALUE LOW-VALUE.

       01  CNS-AREA.
      *    *** PX の印字位置
           03  CNS-P           OCCURS 20
                               BINARY-LONG SYNC VALUE ZERO.
           03  CNS-P-NEXT      BINARY-LONG SYNC VALUE ZERO.

      *    *** P1-PX の印字合計桁数　スペース含む
           03  CNS-L-SIZE      BINARY-LONG SYNC VALUE ZERO.
           03  CNS-L-SIZE-O    BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 20.
             05  TBL01-DT-P    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-L    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-P-L  BINARY-LONG SYNC VALUE ZERO.

             05  TBL01-DT-TYPE PIC  X(002) VALUE SPACE.
             05  TBL01-DT-SUM  PIC  X(001) VALUE SPACE.

      *    *** UTF8 の時、１５バイト５文字、調整５バイト
             05  TBL01-DT-TIT  PIC  X(020) VALUE SPACE.
             05  TBL01-DT-TITL BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-TITL8 BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-TITL9 BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-HEN  PIC  X(002) VALUE SPACE.
             05  TBL01-DT-HENP BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-HENL BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-DT-CNT  BINARY-LONG SYNC VALUE ZERO.

             05  TBL01-DT-K1SU PIC S9(011) VALUE ZERO PACKED-DECIMAL.
             05  TBL01-DT-K2SU PIC S9(011) VALUE ZERO PACKED-DECIMAL.
             05  TBL01-DT-K3SU PIC S9(011) VALUE ZERO PACKED-DECIMAL.
             05  TBL01-DT-TOSU PIC S9(011) VALUE ZERO PACKED-DECIMAL.

           03  TBL02-AREA      OCCURS 3.
             05  TBL02-KEY-P   BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-KEY-L   BINARY-LONG SYNC VALUE ZERO.

      *    *** サクラエディタ   v2.4.1.2849 32bit (tag v2.4.1)
      *    *** 印刷ページ設定でページあたりの行文字数、縦行数が
      *    *** 変わるので、以下設定で印刷する

      *    *** 印刷ページ設定
      *    *** 半角フォント　ＭＳゴシック
      *    *** 全角フォント　ＭＳゴシック
      *    *** 行送り０％
      *    *** 余白　上１０、下１０、右１０、左１０ｍｍ
      *    *** 
      *    *** 行あたりの文字数・最大：横時２７７  縦時１９０
      *    *** 縦方向の行数・最大    ：横時９５    縦時１３８

      *    *** PRM2 で指定した、フォント高とサクラエディタの印刷ページ
      *    *** 設定合わせる

      *    *** 縦用、フォント高、文字数、行数
           03  TBL03-AREA.
             05                PIC  X(010) VALUE "20,190,138".
             05                PIC  X(010) VALUE "21,172,131".
             05                PIC  X(010) VALUE "22,172,125".
             05                PIC  X(010) VALUE "23,158,120".
             05                PIC  X(010) VALUE "24,158,115".
             05                PIC  X(010) VALUE "25,146,110".
      *    *** 標準的な値
             05                PIC  X(010) VALUE "26,146,106".
             05                PIC  X(010) VALUE "27,135,102".
             05                PIC  X(010) VALUE "28,135,098".
             05                PIC  X(010) VALUE "29,126,095".
             05                PIC  X(010) VALUE "30,126,092".
             05                PIC  X(010) VALUE "31,118,089".
             05                PIC  X(010) VALUE "32,118,086".
             05                PIC  X(010) VALUE "33,111,083".
             05                PIC  X(010) VALUE "34,111,081".
             05                PIC  X(010) VALUE "35,105,079".
             05                PIC  X(010) VALUE "36,105,076".
             05                PIC  X(010) VALUE "37,100,074".
             05                PIC  X(010) VALUE "38,100,072".
             05                PIC  X(010) VALUE "39,095,071".
             05                PIC  X(010) VALUE "40,095,069".
           03  TBL03-AREA-R    REDEFINES TBL03-AREA
                               OCCURS 21.
             05  TBL03-FONT    PIC  9(002).
             05                PIC  X(001).
             05  TBL03-MOJISU  PIC  9(003).
             05                PIC  X(001).
             05  TBL03-GYOU    PIC  9(003).

      *    *** 横用、フォント高、文字数、行数
           03  TBL04-AREA.
             05                PIC  X(010) VALUE "20,277,095".
             05                PIC  X(010) VALUE "21,251,090".
             05                PIC  X(010) VALUE "22,251,086".
             05                PIC  X(010) VALUE "23,230,082".
             05                PIC  X(010) VALUE "24,230,079".
             05                PIC  X(010) VALUE "25,213,076".
      *    *** 標準的な値
             05                PIC  X(010) VALUE "26,213,073".
             05                PIC  X(010) VALUE "27,197,070".
             05                PIC  X(010) VALUE "28,197,067".
             05                PIC  X(010) VALUE "29,184,065".
             05                PIC  X(010) VALUE "30,184,063".
             05                PIC  X(010) VALUE "31,173,061".
             05                PIC  X(010) VALUE "32,173,059".
             05                PIC  X(010) VALUE "33,162,057".
             05                PIC  X(010) VALUE "34,162,055".
             05                PIC  X(010) VALUE "35,153,054".
             05                PIC  X(010) VALUE "36,153,052".
             05                PIC  X(010) VALUE "37,145,051".
             05                PIC  X(010) VALUE "38,145,050".
             05                PIC  X(010) VALUE "39,138,048".
             05                PIC  X(010) VALUE "40,138,047".
           03  TBL04-AREA-R    REDEFINES TBL04-AREA
                               OCCURS 21.
             05  TBL04-FONT    PIC  9(002).
             05                PIC  X(001).
             05  TBL04-MOJISU  PIC  9(003).
             05                PIC  X(001).
             05  TBL04-GYOU    PIC  9(003).

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C1              BINARY-LONG SYNC VALUE ZERO.
           03  HL              BINARY-LONG SYNC VALUE ZERO.
           03  HP              BINARY-LONG SYNC VALUE ZERO.
           03  H1              BINARY-LONG SYNC VALUE ZERO.
           03  H2              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  N               BINARY-LONG SYNC VALUE ZERO.

           03  P               BINARY-LONG SYNC VALUE ZERO.
           03  PN              OCCURS 20
                               BINARY-LONG SYNC VALUE ZERO.
           03  PN-B            BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO
           03  P5L             BINARY-LONG SYNC VALUE ZERO
           03  P5R             BINARY-LONG SYNC VALUE ZERO
           03  P5              BINARY-LONG SYNC VALUE ZERO
           03  PX              BINARY-LONG SYNC VALUE ZERO
           03  PX2             BINARY-LONG SYNC VALUE ZERO
           03  P-L             BINARY-LONG SYNC VALUE ZERO
           03  P-N             BINARY-LONG SYNC VALUE ZERO.

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

           03  Q               BINARY-LONG SYNC VALUE ZERO.
           03  Q2              BINARY-LONG SYNC VALUE ZERO.
           03  Q3              BINARY-LONG SYNC VALUE ZERO.
           03  Q4              BINARY-LONG SYNC VALUE 1.
      *    *** 印刷Ａ４　縦　MAX　190行,190桁
      *    *** 印刷Ａ４　横　MAX　 93行,277桁
           03  R               BINARY-LONG SYNC VALUE ZERO.
           03  S               BINARY-LONG SYNC VALUE ZERO.
           03  T               BINARY-LONG SYNC VALUE ZERO.
           03  U               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.

      *    *** "1" = A4縦,
      *    *** "0" = A4横
           03  SW-A4TATE       PIC  X(001) VALUE "0".

      *    *** "0" = 明細無し
      *    *** "1" = 明細出力
           03  SW-MEISAI       PIC  X(001) VALUE "0".

      *    *** 明細改行数、明細無しの時、”１”の方が良い
      *    *** "1" = 1行　改行
      *    *** "2" = 2行　改行
           03  SW-KAIGYO       PIC  X(001) VALUE "1".

      *    *** "0" = ブレイク時の改行無し
      *    *** "1" = ブレイク時の改行有り
           03  SW-KAIGYO-BR    PIC  X(001) VALUE "1".

           03  SW-YES          PIC  X(001) VALUE "N".
           03  SW-CSV          PIC  X(001) VALUE "N".
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-SET2         PIC  X(001) VALUE "N".
           03  SW-KEY1         PIC  X(001) VALUE "N".
           03  SW-KEY2         PIC  X(001) VALUE "N".
           03  SW-KEY3         PIC  X(001) VALUE "N".
           03  SW-KEY-BR       PIC  X(001) VALUE "N".
           03  SW-PIN2         PIC  X(001) VALUE "0".
      *    *** SW-UTF8 は止めた
      *     03  SW-UTF8         PIC  X(001) VALUE "N".
           03  SW-MID          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-CSV          PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN1,READ PRM2
           PERFORM S010-10     THRU    S010-EX

      *    *** OPEN2,READ PRM1
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1 OR PIN2
           PERFORM S100-10     THRU    S100-EX

      *    *** PIN1 PRINT,SUM
           PERFORM S200-10     THRU    S200-EX
               WITH TEST AFTER
                   UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN1,READ PRM2
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

           ACCEPT  WK-ARGUMENT-NUMBER FROM      ARGUMENT-NUMBER

      *    *** PRM1-F 指定無し（ARGUMENT-NUMBER=0）、既定値使用
      *    *** ARGUMENT-NUMBER=1 の時、PRM1-F 指定する
      *    *** ARGUMENT-NUMBER=2 の時、PRM1-F,PRM2-F の順に指定する
           EVALUATE WK-ARGUMENT-NUMBER
               WHEN 0
      *             CONTINUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PRM1-F,PRM2-F 初期値"
                   DISPLAY WK-PGM-NAME " PRM1-F=" WK-PRM1-F-NAME
                   DISPLAY WK-PGM-NAME " PRM2-F=" WK-PRM2-F-NAME
               WHEN 1
                   ACCEPT  WK-PRM1-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PRM1-F-NAME
               WHEN 2
                   ACCEPT  WK-PRM1-F-NAME FROM ARGUMENT-VALUE
                   ACCEPT  WK-PRM2-F-NAME FROM ARGUMENT-VALUE
                   DISPLAY WK-PGM-NAME " ARGUMENT-NUMBER="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " ARG-1=" WK-PRM1-F-NAME
                   DISPLAY WK-PGM-NAME " ARG-2=" WK-PRM2-F-NAME
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " WK-ARGUMENT-NUMBER ERROR="
                           WK-ARGUMENT-NUMBER
                   DISPLAY WK-PGM-NAME " PRM1-F,PRM2-F 2個まで指定可"
                   STOP    RUN
           END-EVALUATE

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PRM2-F
           IF      WK-PRM2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM2-F OPEN ERROR STATUS="
                           WK-PRM2-STATUS
                   STOP    RUN
           END-IF

           READ    PRM2-F
               AT  END
                   DISPLAY WK-PGM-NAME " PIN3-F 0 件 設定データ入力する"
                   STOP    RUN
               NOT  AT  END
                   CONTINUE
           END-READ

           IF      PRM2-A4TATE =       "0" OR "1"
                   MOVE    PRM2-A4TATE TO      SW-A4TATE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第１パラ　Ａ４横＝０か"
                           "、Ａ４縦＝１で指定する"
                   STOP    RUN
           END-IF

           IF      PRM2-MEISAI =       "0" OR "1"
                   MOVE    PRM2-MEISAI TO      SW-MEISAI
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第２パラ　明細無＝０か"
                           "、明細有＝１で指定する"
                   STOP    RUN
           END-IF

           IF      PRM2-KAIGYO =       "1" OR "2"
                   MOVE    PRM2-KAIGYO TO      SW-KAIGYO
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第３パラ　１行改行＝１"
                           "か、２行改行＝２で指定する"
                   STOP    RUN
           END-IF

           IF      PRM2-KAIGYO-BR =    "0" OR "1"
                   MOVE    PRM2-KAIGYO-BR TO   SW-KAIGYO-BR
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第４パラ　ブレイク時"
                           "改行無し＝０か、改行有り＝１で指定する"
                   STOP    RUN
           END-IF

      *    *** 文字数 ＺＥＲＯの時、フォント数から自動セット
           IF      PRM2-A4TATE =       "0"
      *    *** 横用、文字数、行数変更
               IF      PRM2-MOJISU =       ZERO    AND
                       PRM2-FONT   IS      NUMERIC AND
                       PRM2-FONT   >=      20      AND
                       PRM2-FONT   <=      40
                   ADD     PRM2-FONT -19 GIVING S
                   MOVE    TBL04-MOJISU(S) TO  PRM2-MOJISU
                   MOVE    TBL04-GYOU  (S) TO  PRM2-GYOU
               END-IF
           ELSE
      *    *** 縦用、文字数、行数変更
               IF      PRM2-MOJISU =       ZERO    AND
                       PRM2-FONT   IS      NUMERIC AND
                       PRM2-FONT   >=      20      AND
                       PRM2-FONT   <=      40
                   ADD     PRM2-FONT -19 GIVING S
                   MOVE    TBL03-MOJISU(S) TO  PRM2-MOJISU
                   MOVE    TBL03-GYOU  (S) TO  PRM2-GYOU
               END-IF
           END-IF

           IF      PRM2-MOJISU IS      NUMERIC
               IF      PRM2-A4TATE =       "0"
      *    *** 横
                   IF      PRM2-MOJISU >=      138 AND
                           PRM2-MOJISU <=      277
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM2-F　第５パラ　"
                                   "文字数　Ａ４横"
                                   "　１３８から２７７の範囲で指定する"
                           STOP    RUN
                   END-IF
               ELSE
      *    *** 縦
                   IF      PRM2-MOJISU >=       95 AND
                           PRM2-MOJISU <=      190
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM2-F　第５パラ　"
                                   "文字数　Ａ４縦"
                                   "　９５から１９０の範囲で指定する"
                           STOP    RUN
                   END-IF
               END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第５パラ　"
                                       "文字数　数字以外 "
                   STOP    RUN
           END-IF

           IF      PRM2-GYOU   IS      NUMERIC
               IF      PRM2-A4TATE =       "0"
      *    *** 横
                   IF      PRM2-GYOU   >=      47 AND
                           PRM2-GYOU   <=      95
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM2-F　第６パラ　"
                                   "行数　Ａ４横"
                                   "　４７から９５の範囲で指定する"
                           STOP    RUN
                   END-IF
               ELSE
      *    *** 縦
                   IF      PRM2-GYOU   >=       69 AND
                           PRM2-GYOU   <=      138
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME " PRM2-F　第６パラ　"
                                   "行数　Ａ４縦"
                                   "　６９から１３８の範囲で指定する"
                           STOP    RUN
                   END-IF
               END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM2-F 第６パラ　"
                                       "行数　数字以外 "
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE07-ID
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           MOVE    HIGH-VALUE  TO      PIN1-REC
                                       PIN2-REC
           .
       S010-EX.
           EXIT.

      *    *** OPEN2,READ PRM1
       S020-10.

           MOVE    1           TO      CNS-P (1)
                                       CNS-P-NEXT
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
                               WHEN "TITLE"
                                   PERFORM S021-10  THRU    S021-EX
                               WHEN "DT"
                                   PERFORM S022-10  THRU    S022-EX
                               WHEN "F-IN"
                                   PERFORM S023-10  THRU    S023-EX
                               WHEN "F-OT"
                                   PERFORM S024-10  THRU    S024-EX
                               WHEN "LISTID"
                                   PERFORM S025-10  THRU    S025-EX
                               WHEN "KEY"
                                   PERFORM S026-10  THRU    S026-EX
                               WHEN "KEYMID"
                                   PERFORM S027-10  THRU    S027-EX
      *    *** 現在指定 不可
      *                         WHEN "CSV"
      *                             PERFORM S028-10  THRU    S028-EX
                               WHEN "MID"
                                   PERFORM S030-10  THRU    S030-EX
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

           IF      WK-F-IN-MODE =      "L"
                   OPEN    INPUT       PIN1-F
                   IF      WK-PIN1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIN1-F OPEN ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           ELSE
                   OPEN    INPUT       PIN2-F
                   IF      WK-PIN2-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIN2-F OPEN ERROR STATUS="
                                   WK-PIN2-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF      WK-F-OT-MODE =      "O"
                   OPEN    OUTPUT      POT1-F
           ELSE
                   OPEN    EXTEND      POT1-F
           END-IF
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

      *    *** KEY 組合せチェック
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
                           " KEY=N 指定なし 1 1,2 1,2,3 で指定する"
                   STOP    RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-TBL01-MAX 
               IF  TBL01-DT-TYPE (I) =   "CH"
                   IF  TBL01-DT-L (I) <      TBL01-DT-TITL (I)
                       ADD     TBL01-DT-TITL (I) 1 TO CNS-L-SIZE
                   ELSE
                       ADD     TBL01-DT-L (I) 1 TO    CNS-L-SIZE
                   END-IF
      *    *** HE,ZD,,,PD,,,
               ELSE
                   IF  TBL01-DT-HENL (I) <   TBL01-DT-TITL (I)
                       ADD     TBL01-DT-TITL (I) 1 TO CNS-L-SIZE
                   ELSE
                       ADD     TBL01-DT-HENL (I) 1 TO CNS-L-SIZE
                   END-IF
               END-IF
           END-PERFORM

      *    *** 自動調整長さPX2,計算する
      *    *** KEY-L(1)(2)(3) はKEY1,2,3 の長さ、11は＊＊総合計＋１
           MOVE    11          TO      PX2
           IF      TBL02-KEY-L (1) >   ZERO
                   ADD     1 TBL02-KEY-L (1) TO      PX2
           END-IF
           IF      TBL02-KEY-L (2) >       ZERO
                   ADD     1 TBL02-KEY-L (2) TO      PX2
           END-IF
           IF      TBL02-KEY-L (3) >       ZERO
                   ADD     1 TBL02-KEY-L (3) TO      PX2
           END-IF

      *    *** 総合計の時の長さ確定する
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-TBL01-MAX 
               IF    ( TBL01-DT-TYPE(I) =  "ZD" OR "Z1" OR "Z2" OR 
                                           "Z3" OR
      *                                    "C1" OR "C2" OR "C3" OR
      *                                    "CT" OR
                                           "PD" OR "P1" OR "P2" OR 
                                           "P3" )  AND 
                       TBL01-DT-SUM (I) =  "Y"

                       COMPUTE PX2 = PX2 + TBL01-DT-TITL(I) + 1

                       EVALUATE TRUE

                           WHEN TBL01-DT-TYPE(I) = "ZD" OR "PD"
                             EVALUATE TRUE
                               WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                                   MOVE    12          TO      HL
                               WHEN TBL01-DT-HEN(I) = "EE" 
                                   MOVE    13          TO      HL
                               WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                                   MOVE    15          TO      HL
                               WHEN OTHER
                                   MOVE    16          TO      HL
                             END-EVALUATE

                           WHEN TBL01-DT-TYPE(I) = "Z1" OR "P1"
                             EVALUATE TRUE
                               WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                                   MOVE    13          TO      HL
                               WHEN TBL01-DT-HEN(I) = "EE" 
                                   MOVE    14          TO      HL
                               WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                                   MOVE    16          TO      HL
                               WHEN OTHER
                                   MOVE    17          TO      HL
                             END-EVALUATE

                           WHEN TBL01-DT-TYPE(I) = "Z2" OR "P2"
                             EVALUATE TRUE
                               WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                                   MOVE    13          TO      HL
                               WHEN TBL01-DT-HEN(I) = "EE" 
                                   MOVE    14          TO      HL
                               WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                                   MOVE    15          TO      HL
                               WHEN OTHER
                                   MOVE    16          TO      HL
                             END-EVALUATE

                           WHEN TBL01-DT-TYPE(I) = "Z3" OR "P3"
                             EVALUATE TRUE
                               WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                                   MOVE    13          TO      HL
                               WHEN TBL01-DT-HEN(I) = "EE" 
                                   MOVE    14          TO      HL
                               WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                                   MOVE    15          TO      HL
                               WHEN OTHER
                                   MOVE    16          TO      HL
                             END-EVALUATE

                           WHEN OTHER
                                MOVE    ZERO   TO      HL
                       END-EVALUATE

                       COMPUTE PX2 = PX2 + HL + 1
               END-IF
           END-PERFORM

      *    *** + 15 は件数表示分 自動調整長さ= PX2
      *    *** 件数(4)+CNT3(11)
           ADD     15          TO      PX2
           MOVE    CNS-L-SIZE  TO      CNS-L-SIZE-O
      *    *** 総合計の方が大きい時、１列の長さ変更する
           IF      CNS-L-SIZE  <       PX2
      *    *** 自動調整長さ加える
                   COMPUTE CNS-L-SIZE = CNS-L-SIZE + (PX2 - CNS-L-SIZE)
           END-IF

           IF      SW-A4TATE   =       "1"
      *    *** 割算で商を求め、商 * CNS-L-SIZEを求める
                   COMPUTE C1 = PRM2-MOJISU / ( CNS-L-SIZE + 1 )
                   COMPUTE C = C1   * ( CNS-L-SIZE + 1 )
                   IF      SW-KAIGYO   =       "2"
      *    *** - 5 (ヘッダー)
                           COMPUTE R = (PRM2-GYOU - 5 ) / 2
                   ELSE
                           COMPUTE R = PRM2-GYOU - 5
                   END-IF
                   IF      CNS-L-SIZE >        PRM2-MOJISU
                       DISPLAY WK-PGM-NAME " PRM1-F DT=XX 項目数ｵｰﾊﾞｰ"
                               " SUM=YをSUM=Nにして、集計項目減らす"
                       DISPLAY WK-PGM-NAME " CNS-L-SIZE=" CNS-L-SIZE
                               " > " PRM2-MOJISU
                       STOP    RUN
                   END-IF
           ELSE
                   COMPUTE C1 = PRM2-MOJISU / ( CNS-L-SIZE + 1 )
                   COMPUTE C = C1   * ( CNS-L-SIZE + 1 )
                   IF      SW-KAIGYO   =       "2"
                           COMPUTE R = (PRM2-GYOU - 5 ) / 2
                   ELSE
                           COMPUTE R = PRM2-GYOU - 5
                   END-IF
                   IF      CNS-L-SIZE >        PRM2-MOJISU
                       DISPLAY WK-PGM-NAME " PRM1-F DT=XX 項目数ｵｰﾊﾞｰ"
                               " SUM=YをSUM=Nにして、集計項目減らす"
                       DISPLAY WK-PGM-NAME " CNS-L-SIZE=" CNS-L-SIZE
                               " > " PRM2-MOJISU
                       STOP    RUN
                   END-IF
           END-IF

           MOVE    1           TO      N
           MOVE    WK-TIT      TO      WK-TIT1-TIT
      *    *** C1は繰り返し列数
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > C1
      *    *** 見出し－ハイホンの出力
               IF      SW-A4TATE   =       "1"
                   MOVE    ALL "-"     TO      WK-HAI-A4T (N:CNS-L-SIZE)
               ELSE
                   MOVE    ALL "-"     TO      WK-HAI-A4Y (N:CNS-L-SIZE)
               END-IF

                  PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > WK-TBL01-MAX
                   IF  TBL01-DT-TYPE (I2) =   "CH"
                       IF  TBL01-DT-L (I2) <      TBL01-DT-TITL (I2)
                           MOVE    TBL01-DT-TITL (I2) TO     L
                       ELSE
                           MOVE    TBL01-DT-L   (I2) TO      L
                       END-IF
                   ELSE
                       IF  TBL01-DT-HENL (I2) <   TBL01-DT-TITL (I2)
                           MOVE    TBL01-DT-TITL (I2) TO     L
                       ELSE
                           MOVE    TBL01-DT-HENL (I2) TO     L
                       END-IF
                   END-IF

                   IF      SW-A4TATE   =       "1"
                     IF      TBL01-DT-TYPE(I2) =       "CH" OR "HE"
                       MOVE    TBL01-DT-TIT (I2) TO    WK-MID1-A4T (N:L)
                     ELSE
                       ADD     N L GIVING P4
                       MOVE    "N"     TO      SW-SET
                       PERFORM VARYING I3 FROM 20 BY -1
                          UNTIL I3 < 1 OR P4 <= N
                          IF      TBL01-DT-TIT (I2) (I3:1) NOT = SPACE
                               OR SW-SET       =      "Y"
                            ADD     -1    TO    P4
                            MOVE    TBL01-DT-TIT (I2) (I3:1) TO
                                    WK-MID1-A4T (P4:1)
                            MOVE    "Y"        TO      SW-SET
                          END-IF
                       END-PERFORM
                     END-IF
                   ELSE
                     IF      TBL01-DT-TYPE(I2) =       "CH" OR "HE"
                       MOVE    TBL01-DT-TIT (I2) TO    WK-MID1-A4Y (N:L)
                     ELSE
                       ADD     N L GIVING P4
                       MOVE    "N"     TO      SW-SET
                       PERFORM VARYING I3 FROM 20 BY -1
                          UNTIL I3 < 1 OR P4 <= N
                          IF      TBL01-DT-TIT (I2) (I3:1) NOT = SPACE
                               OR SW-SET       =      "Y"
                            ADD     -1    TO    P4
                            MOVE    TBL01-DT-TIT (I2) (I3:1) TO
                                    WK-MID1-A4Y (P4:1)
                            MOVE    "Y"        TO      SW-SET
                          END-IF
                       END-PERFORM
                     END-IF
                   END-IF

                   IF  TBL01-DT-TYPE (I2) =   "CH"
                       IF  TBL01-DT-L (I2) <      TBL01-DT-TITL (I2)
                           ADD     TBL01-DT-TITL (I2) 1 TO   N
                       ELSE
                           ADD     TBL01-DT-L    (I2) 1 TO   N
                       END-IF
                   ELSE
      *    *** HE,ZD,,,PD,,,
                       IF  TBL01-DT-HENL (I2) <   TBL01-DT-TITL (I2)
                           ADD     TBL01-DT-TITL (I2) 1 TO   N
                       ELSE
                           ADD     TBL01-DT-HENL (I2) 1 TO   N
                       END-IF
                   END-IF
               END-PERFORM

      *    *** 自動調整後の長さ加える
               IF      CNS-L-SIZE-O <       PX2
                   COMPUTE N = N + 1 + ( PX2 - CNS-L-SIZE-O )
               ELSE
                   COMPUTE N = N + 1
               END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-TBL01-MAX 
                   MOVE    CNS-P (I)   TO      PN(I)
                   IF      I           =       1
                           MOVE    CNS-P (I)   TO      PN-B
                   END-IF

      *    *** ZD,PD,C1等の項目は、右寄せにする
                   IF  TBL01-DT-TYPE (I) (1:1) = "Z" OR "P" OR 
                       TBL01-DT-TYPE (I) = "C1" OR "C2" OR "C3"  OR "CT"
                       IF  TBL01-DT-HENL (I) <   TBL01-DT-TITL (I)
                           COMPUTE PN(I) = PN(I) + TBL01-DT-TITL (I) 
                                         - TBL01-DT-HENL (I)
                       END-IF
                   END-IF
           END-PERFORM

           IF      SW-MID      =       "Y"
               PERFORM VARYING K FROM 1 BY 1
                   UNTIL   K > R + 4
                   EVALUATE TRUE

                     WHEN K = 10
                       MOVE    "CHANGE"    TO      WDE07-ID
                       MOVE    "LISTID"    TO      WDE07-ASCII
                       CALL    "DECODE07"  USING   WDE07-DECODE07-AREA
                       PERFORM VARYING T FROM 1 BY 1
                           UNTIL T > 16
                           WRITE   POT1-REC    FROM    WDE07-LINE (T)
                           ADD     1           TO      WK-POT1-CNT
                       END-PERFORM
                       ADD     15          TO      K

                     WHEN K = 30
                       MOVE    "CHANGE"    TO      WDE07-ID
                       MOVE    WK-LISTID   TO      WDE07-ASCII
                       CALL    "DECODE07"  USING   WDE07-DECODE07-AREA
                       PERFORM VARYING T FROM 1 BY 1
                           UNTIL T > 16
                           WRITE   POT1-REC    FROM    WDE07-LINE (T)
                           ADD     1           TO      WK-POT1-CNT
                       END-PERFORM
                       ADD     15          TO      K

                     WHEN OTHER
                       MOVE    SPACE     TO      POT1-REC
                       WRITE   POT1-REC
                       ADD     1         TO      WK-POT1-CNT
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** TITLE=＊＊＊　帳票タイトル　＊＊＊
       S021-10.

           IF      P02-L    >     60
                   DISPLAY WK-PGM-NAME " PRM1-F TITLE=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                         " TITLE=XX タイトル名６０バイトまで"
                   STOP    RUN
           END-IF

           IF      WK-PRM02    NOT =   SPACE
                   MOVE    WK-PRM02  TO    WK-TIT
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F TITLE PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " TITLE=XXX SPACE 以外指定する"
                           PRM1-REC
                   STOP    RUN
           END-IF
           .
       S021-EX.
           EXIT.

      *    *** DT=NN NN=1-20
       S022-10.
      
      *    *** 第１パラメータ
           IF      WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 20
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO I4
                   ADD     1           TO      I
                                               TBL01-DT-CNT (I4)
                   IF      TBL01-DT-CNT (I4) > 1
                         DISPLAY WK-PGM-NAME " PRM1-F DT=NN PARA ERROR "
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME 
                                   " DT=NN NN 同じものが指定されている"
                           STOP    RUN
                   END-IF
                   IF      I           >       WK-TBL01-MAX
                           MOVE    I           TO      WK-TBL01-MAX
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN 1-20 の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第２パラメータ
           IF      WK-PRM03    =       "POS"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN POS= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN POS= 2つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM04(1:P04-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM04) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM04) <= 32760
                   MOVE    FUNCTION NUMVAL(WK-PRM04) TO
                                       TBL01-DT-P (I)
                   IF      I           NOT =   1
                       MOVE    CNS-P-NEXT  TO      CNS-P (I)
                   END-IF
           ELSE
                 DISPLAY WK-PGM-NAME " PRM1-F DT=NN POS=NNN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN POS=NNN 1-32760の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第３パラメータ
           IF      WK-PRM05    =       "LEN"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN LEN= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN LEN= 3つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM06(1:P06-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM06) >= 1 AND
      *             FUNCTION NUMVAL(WK-PRM06) <= 40
                   FUNCTION NUMVAL(WK-PRM06) <= 80
                   MOVE    FUNCTION NUMVAL(WK-PRM06) TO
                                       TBL01-DT-L (I)
           ELSE
                 DISPLAY WK-PGM-NAME " PRM1-F DT=NN LEN=NNN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
      *                     " DT=NN LEN=NNN 1-40 の範囲で指定する"
                           " DT=NN LEN=NNN 1-80 の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第４パラメータ
           IF      WK-PRM07    =       "TYPE"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN TYPE= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN TYPE= 4つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM08    =       "CH" OR "HE" OR
                                       "ZD" OR "Z1" OR "Z2" OR "Z3" OR
                                       "C1" OR "C2" OR "C3" OR "CT" OR
                                       "PD" OR "P1" OR "P2" OR "P3"
                   MOVE    WK-PRM08    TO      TBL01-DT-TYPE (I)
               EVALUATE TRUE
                   WHEN  WK-PRM08 = "CH"
                       CONTINUE
                   WHEN  WK-PRM08 = "HE"
                       IF      FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                               FUNCTION NUMVAL(WK-PRM06) <= 20
                               COMPUTE TBL01-DT-HENL (I) =
                                   FUNCTION NUMVAL(WK-PRM06) * 2
                       ELSE
                               DISPLAY WK-PGM-NAME
                                     " PRM1-F DT=NN LEN=NNN PARA ERROR "
                                       PRM1-REC
                               DISPLAY WK-PGM-NAME
                                       " DT=NN TYPE=HEは"
                               DISPLAY WK-PGM-NAME 
                                       " LEN=NNN 1-20 の範囲で指定する"
                               STOP    RUN
                       END-IF
                   WHEN  WK-PRM08 = "ZD" OR "Z1" OR "Z2" OR "Z3" 
                       IF      FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                               FUNCTION NUMVAL(WK-PRM06) <= 11
                               CONTINUE
                       ELSE
                               DISPLAY WK-PGM-NAME
                                     " PRM1-F DT=NN LEN=NNN PARA ERROR "
                                       PRM1-REC
                               DISPLAY WK-PGM-NAME
                                       " DT=NN TYPE=ZD,Z1,Z2,Z3は"
                               DISPLAY WK-PGM-NAME 
                                       " LEN=NNN 1-11 の範囲で指定する"
                               STOP    RUN
                       END-IF
                   WHEN  WK-PRM08 = "C1" OR "C2" OR "C3" OR "CT"
                       IF      FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                               FUNCTION NUMVAL(WK-PRM06) <= 9
                               CONTINUE
                       ELSE
                               DISPLAY WK-PGM-NAME
                                     " PRM1-F DT=NN LEN=NNN PARA ERROR "
                                       PRM1-REC
                               DISPLAY WK-PGM-NAME
                                       " DT=NN TYPE=C1,C2.C3,CTは"
                               DISPLAY WK-PGM-NAME 
                                       " LEN=NNN 1-9 の範囲で指定する"
                               STOP    RUN
                       END-IF
      *    *** PACKはDT-Lを数字桁数に変換
                   WHEN OTHER
                       IF   FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                            FUNCTION NUMVAL(WK-PRM06) <= 6
                             MOVE    FUNCTION NUMVAL(WK-PRM06) TO
                                                   TBL01-DT-P-L (I)
                             EVALUATE FUNCTION NUMVAL(WK-PRM06)
                               WHEN 1
                                    MOVE 1    TO    TBL01-DT-L (I)
                               WHEN 2
                                    MOVE 3    TO    TBL01-DT-L (I)
                               WHEN 3
                                    MOVE 5    TO    TBL01-DT-L (I)
                               WHEN 4
                                    MOVE 7    TO    TBL01-DT-L (I)
                               WHEN 5
                                    MOVE 9    TO    TBL01-DT-L (I)
                               WHEN OTHER
                                    MOVE 11   TO    TBL01-DT-L (I)
                             END-EVALUATE
                       ELSE
                           DISPLAY WK-PGM-NAME
                                   " PRM1-F DT=NN LEN=NNN PARA ERROR "
                                   PRM1-REC
                           DISPLAY WK-PGM-NAME
                                   " DT=NN TYPE=PD,P1,P2,P3は"
                           DISPLAY WK-PGM-NAME 
                                   " LEN=NNN 1-6 の範囲で指定する"
                           STOP    RUN
                       END-IF
               END-EVALUATE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F TYPE=XXX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " TYPE=XX CH,HE,ZD,Z1,Z2,Z3,PD,P1,P2,P3"
                           ",C1,C2,C3,CT いずれかで指定する"
                   STOP    RUN
           END-IF

           IF    ( WK-PRM08    =       "Z2" OR "P2" ) AND
                   TBL01-DT-L (I) = 1
                   DISPLAY WK-PGM-NAME
                           " PRM1-F DT=NN TYPE=Z2 OR P2 PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " TYPE=Z2 OR P2 LEN=1 はエラー"
                   STOP    RUN
           END-IF

           IF    ( WK-PRM08    =       "Z3" OR "P3" ) AND
                 ( TBL01-DT-L (I) = 1 OR 2 )
                   DISPLAY WK-PGM-NAME
                           " PRM1-F DT=NN TYPE=Z3 OR P3 PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " TYPE=Z3 OR P3 LEN=1 OR 2 はエラー"
                   STOP    RUN
           END-IF

           IF      WK-PRM08    =       "PD" OR "P1" OR "P2" OR "P3"
               IF  WK-F-IN-MODE =      "B" 
                   CONTINUE
               ELSE
                   DISPLAY WK-PGM-NAME
                           " PRM1-F DT=NN TYPE=PD,P1,P2,P3 PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " TYPE=PD,P1,P2,P3 "
                           " F-INのMODE=B(BINARY)以外はエラー"
                   DISPLAY WK-PGM-NAME 
                           " F-INのMODE=" WK-F-IN-MODE " F-IN=の指定は"
                           "DT=より前に指定する"
                   STOP    RUN
               END-IF
           END-IF

      *    *** 第５パラメータ
           IF      WK-PRM09    =       "SUM"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN IDX= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN SUM= 5つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM10(1:1)  =    "Y"
               IF      WK-PRM08    =     "ZD" OR "Z1" OR "Z2" OR "Z3" OR
                                         "PD" OR "P1" OR "P2" OR "P3"
                   MOVE    WK-PRM10 TO TBL01-DT-SUM (I)
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN SUM=Y PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN SUM=Y は"
                           " TYPE=XX ZD,Z1,Z2,Z3,PD,P1,P2,P3"
                           " の時、指定可"
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-PRM10(1:1)  =    "N"
                   MOVE    WK-PRM10 TO TBL01-DT-SUM (I)
               ELSE
                  DISPLAY WK-PGM-NAME " PRM1-F DT=NN IDX=NN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN SUM=X Y OR N で指定する"
                   STOP    RUN
               END-IF
           END-IF


      *    *** 第６パラメータ
           IF      WK-PRM11    =       "TITLE"
                   CONTINUE
           ELSE
                  DISPLAY WK-PGM-NAME " PRM1-F DT=NN TITLE= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN TITLE= 6つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM12  NOT =     SPACE
                   MOVE    WK-PRM12 TO      TBL01-DT-TIT  (I)
      *     MOVE    "P"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PRM12
                   MOVE    P12-L    TO      TBL01-DT-TITL (I)
                   IF      P12-L    >     10
      *                 MOVE    10    TO     TBL01-DT-TITL (I)
                       DISPLAY WK-PGM-NAME 
                              " PRM1-F DT=NN TITLE=XX PARA ERROR "
                               PRM1-REC
                       DISPLAY WK-PGM-NAME 
                               " DT=NN TITLE=XX 項目タイトルを指定する"
                               " 最大１０バイトまで、漢字可５文字まで"
                       STOP    RUN
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME 
                           " PRM1-F DT=NN TITLE=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN TITLE=XX 項目タイトルを指定する"
                           " 最大１０バイトまで スペースはエラー"
                   STOP    RUN
           END-IF

      *    *** 第７パラメータ
           IF      WK-PRM08    =       "CH" OR "HE"
                   CONTINUE
           ELSE
               IF      WK-PRM13    =       "HEN"
                   CONTINUE
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F DT=NN HEN= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " DT=NN TYPE=数字の時、"
                                       "HEN= 7つめに指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM08    =       "C1" OR "C2" OR "C3" OR "CT"
                   IF  WK-PRM14(1:2) =     "ZK"
                       CONTINUE
                   ELSE
                       DISPLAY WK-PGM-NAME 
                            " PRM1-F DT=NN TYPE=C1,C2,C3,CT PARA ERROR "
                               PRM1-REC
                     DISPLAY WK-PGM-NAME " DT=NN TYPE=C1,C2,C3,CTの時、"
                                       "HEN=ZK のみ指定可"
                       STOP    RUN
                   END-IF
           ELSE
                   CONTINUE
           END-IF

           IF      WK-PRM08    =       "ZD" OR "Z1" OR "Z2" OR "Z3" OR
                                       "C1" OR "C2" OR "C3" OR "CT" OR
                                       "PD" OR "P1" OR "P2" OR "P3"
             IF    WK-PRM14(1:2) =     "99" OR "ZZ" OR "EE" OR
                                       "9K" OR "ZK" OR "EK"
                   MOVE    WK-PRM14 TO TBL01-DT-HEN  (I)
                   MOVE    TBL01-DT-L (I) TO L

                   EVALUATE TRUE
                     WHEN  WK-PRM14(1:2) =     "99" OR "ZZ"
                           COMPUTE N = 12 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 1 TO TBL01-DT-HENL (I)
                     WHEN  WK-PRM14(1:2) =     "EE"
                           COMPUTE N = 12 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 2 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "9K" OR "ZK") AND
                          (TBL01-DT-L(I) >=    1           ) AND 
                          (TBL01-DT-L(I) <=    3           )
                           COMPUTE N = 15 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 1 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "9K" OR "ZK") AND
                          (TBL01-DT-L(I) >=    4           ) AND 
                          (TBL01-DT-L(I) <=    6           )
                           COMPUTE N = 14 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 2 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "9K" OR "ZK") AND
                          (TBL01-DT-L(I) >=    7           ) AND 
                          (TBL01-DT-L(I) <=    9           )
                           COMPUTE N = 13 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 3 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "9K" OR "ZK") AND
                          (TBL01-DT-L(I) >=    10          ) AND 
                          (TBL01-DT-L(I) <=    11          )
                           COMPUTE N = 12 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 4 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "EK") AND
                          (TBL01-DT-L(I) >=    1   ) AND 
                          (TBL01-DT-L(I) <=    3   )
                           COMPUTE N = 15 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 2 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "EK") AND
                          (TBL01-DT-L(I) >=    4   ) AND 
                          (TBL01-DT-L(I) <=    6   )
                           COMPUTE N = 14 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 3 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "EK") AND
                          (TBL01-DT-L(I) >=    7   ) AND 
                          (TBL01-DT-L(I) <=    9   )
                           COMPUTE N = 13 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 4 TO TBL01-DT-HENL (I)
                     WHEN (WK-PRM14(1:2) =     "EK") AND
                          (TBL01-DT-L(I) >=    10  ) AND 
                          (TBL01-DT-L(I) <=    11  )
                           COMPUTE N = 12 - L
                           MOVE    N               TO TBL01-DT-HENP (I)
                           ADD     TBL01-DT-L(I) 5 TO TBL01-DT-HENL (I)
                   END-EVALUATE

                   EVALUATE TRUE
                       WHEN TBL01-DT-TYPE(I) = "ZD" OR "PD"
                           CONTINUE

                       WHEN TBL01-DT-TYPE(I) = "Z1" OR "P1"
                           ADD     1               TO TBL01-DT-HENL (I)

                           IF    ( WK-PRM14(1:2) =     "99" OR
                                                       "ZZ" OR "ZK" OR
                                                       "EE" OR "EZ") AND
                                 ( TBL01-DT-L (I) = 1 )
                               ADD     -1          TO TBL01-DT-HENP (I)
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

                           IF      WK-PRM14(1:2) =     "9K" 
                             IF TBL01-DT-L (I) = 4 OR 7 OR 10 
                                 ADD     1         TO TBL01-DT-HENP (I)
                                 ADD     -1        TO TBL01-DT-HENL (I)
                                  CONTINUE
                             ELSE
                               IF TBL01-DT-L (I) = 1
                                 ADD     -1        TO TBL01-DT-HENP (I)
                                 ADD     1         TO TBL01-DT-HENL (I)
                               ELSE
                                  CONTINUE
                               END-IF
                             END-IF
                           END-IF

                       WHEN TBL01-DT-TYPE(I) = "Z2"
                           ADD     1               TO TBL01-DT-HENL (I)

                           IF      WK-PRM14(1:2) =     "99" AND
                                   TBL01-DT-L (I) = 2
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                           END-IF

                           IF      WK-PRM14(1:2) =     "9K"
                               ADD     -1          TO TBL01-DT-HENL (I)
                               IF TBL01-DT-L (I) = 2
                                   ADD     -2      TO TBL01-DT-HENP (I)
                                   ADD     2       TO TBL01-DT-HENL (I)
                               ELSE 
                                 IF TBL01-DT-L (I) = 3 OR 6 OR 9
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                                 ELSE
                                   CONTINUE
                                 END-IF
                               END-IF
                           END-IF

                           IF    ( WK-PRM14(1:2) =     "ZZ" OR "EE") AND
                                    TBL01-DT-L (I) = 2
                               ADD     -1          TO TBL01-DT-HENP (I)
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

                           IF      WK-PRM14(1:2) =     "ZK"
                               ADD     -1          TO TBL01-DT-HENL (I)
                               IF TBL01-DT-L (I) = 2
                                   ADD     -2      TO TBL01-DT-HENP (I)
                                   ADD     2       TO TBL01-DT-HENL (I)
                               ELSE 
                                 IF TBL01-DT-L (I) = 3 OR 6 OR 9
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                                 ELSE
                                   CONTINUE
                                 END-IF
                               END-IF
                           END-IF

                           IF      WK-PRM14(1:2) =     "EK"
                               ADD     -1          TO TBL01-DT-HENL (I)
                               IF TBL01-DT-L (I) = 2
                                   ADD     -2      TO TBL01-DT-HENP (I)
                                   ADD     2       TO TBL01-DT-HENL (I)
                               ELSE 
                                 IF TBL01-DT-L (I) = 3 OR 6 OR 9
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                                 ELSE
                                   CONTINUE
                                 END-IF
                               END-IF
                           END-IF

                       WHEN TBL01-DT-TYPE(I) = "P2"
                           IF      WK-PRM14(1:2) =     "99" OR "EE" OR 
                                                       "ZZ"
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

                           IF    ( WK-PRM14(1:2) =     "9K" OR "ZK" OR
                                                       "EK" ) AND
      *    *** PACKなのでDT-L ５バイト＝＞９桁に変換されてる
                                 ( TBL01-DT-L (I) = 9 OR 3 ) 
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                           END-IF

                       WHEN TBL01-DT-TYPE(I) = "P3"
                           IF      WK-PRM14(1:2) =     "99"
                               ADD     1           TO TBL01-DT-HENL (I)
                               IF  TBL01-DT-L (I) = 3
                                 ADD     -1        TO TBL01-DT-HENP (I)
                                 ADD     1         TO TBL01-DT-HENL (I)
                               END-IF 
                           END-IF

                           IF    ( WK-PRM14(1:2) =     "9K"  OR "EK" ) 
                               AND
                                   TBL01-DT-L (I) = 3
                               ADD     -2          TO TBL01-DT-HENP (I)
                               ADD     2           TO TBL01-DT-HENL (I)
                           END-IF

                           IF      WK-PRM14(1:2) =     "ZZ" OR "EE"
                                   ADD     1       TO TBL01-DT-HENL (I)
      *    *** PACKなのでDT-L ５バイト＝＞９桁に変換されてる
                               IF  TBL01-DT-L (I) = 3
                                   ADD     -1      TO TBL01-DT-HENP (I)
                                   ADD     1       TO TBL01-DT-HENL (I)
                               END-IF
                           END-IF

                           IF      WK-PRM14(1:2) =     "ZK" AND
                                   TBL01-DT-L (I) = 3
                               ADD     -2          TO TBL01-DT-HENP (I)
                               ADD     2           TO TBL01-DT-HENL (I)
                           END-IF

                       WHEN TBL01-DT-TYPE(I) = "Z3"
                           IF      WK-PRM14(1:2) =     "9K" OR "ZK" OR
                                                       "EK"
                                   CONTINUE
                           ELSE
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

      *    *** TYPE=Z3 LEN=1,2 はエラー
      *    *** LEN=3 だけ、指定する
                           IF   (( WK-PRM14(1:2) =     "99" OR "ZZ") AND
                                   TBL01-DT-L (I) = 3 )
                               ADD     -1          TO TBL01-DT-HENP (I)
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

                           IF   (( WK-PRM14(1:2) =     "9K" OR
                                                       "ZK") AND
                                   TBL01-DT-L (I) = 3 )
                               ADD     -2          TO TBL01-DT-HENP (I)
                               ADD     2           TO TBL01-DT-HENL (I)
                           END-IF

                           IF    ( WK-PRM14(1:2) =     "EE") AND
                                   TBL01-DT-L (I) = 3
                               ADD     -1          TO TBL01-DT-HENP (I)
                               ADD     1           TO TBL01-DT-HENL (I)
                           END-IF

                           IF    ( WK-PRM14(1:2) =     "EK") AND
                                   TBL01-DT-L (I) = 3
                               ADD     -2          TO TBL01-DT-HENP (I)
                               ADD     2           TO TBL01-DT-HENL (I)
                           END-IF
                   END-EVALUATE

                   IF      TBL01-DT-HENL(I) < TBL01-DT-TITL (I)
                       ADD     CNS-P-NEXT TBL01-DT-TITL (I) 1
                               GIVING CNS-P-NEXT
                   ELSE
                       ADD     CNS-P-NEXT TBL01-DT-HENL (I) 1
                                GIVING CNS-P-NEXT
                   END-IF

             ELSE
                  DISPLAY WK-PGM-NAME " PRM1-F DT=NN HEN=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " DT=NN HEN=XX 99,9K,ZZ,ZK,EE,EK"
                           " を指定する"
                   STOP    RUN
             END-IF
           ELSE
      *    *** TYPE=HE
             IF      WK-PRM08    =       "HE"
               IF  TBL01-DT-HENL(I) < TBL01-DT-TITL (I)
                   ADD     CNS-P-NEXT TBL01-DT-TITL (I) 1
                           GIVING CNS-P-NEXT
               ELSE
                   ADD     CNS-P-NEXT TBL01-DT-HENL (I) 1
                           GIVING CNS-P-NEXT
               END-IF
             ELSE
      *    *** TYPE=CH
               IF  TBL01-DT-L(I) < TBL01-DT-TITL (I)
                   ADD     CNS-P-NEXT TBL01-DT-TITL (I) 1
                           GIVING CNS-P-NEXT
               ELSE
                   ADD     CNS-P-NEXT TBL01-DT-L (I) 1
                           GIVING CNS-P-NEXT
               END-IF
             END-IF
           END-IF
           .
       S022-EX.
           EXIT.

      *    *** INPUT FILE FILE NAME
       S023-10.

           IF      P02-L    >     32
                   DISPLAY WK-PGM-NAME " PRM1-F F-IN=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                         " F-IN=XX インプットファイル名３２バイトまで"
                   STOP    RUN
           END-IF

           IF      WK-PRM02    NOT =   SPACE
                   MOVE    WK-PRM02    TO     WK-PIN1-F-NAME
                                              WK-PIN2-F-NAME
                   GO  TO  S023-20
           END-IF

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME ?"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME="
                           WK-PIN1-F-NAME " OK ? Y/N"
                   ACCEPT  SW-YES
           MOVE    WK-PIN1-F-NAME TO   WK-PIN2-F-NAME
           END-PERFORM
           .
       S023-20.
           IF      WK-PRM03    =       "MODE"
                   CONTINUE
           ELSE
               IF      WK-PRM03    =       SPACE
                   MOVE    "L"         TO      WK-F-IN-MODE
                   MOVE    SPACE       TO      WK-PIN2-F-NAME
                   EXIT    PARAGRAPH
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F F-IN=XX,MODE=?"
                                       " PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " F-IN=XX,MODE=L OR LINE OR"
                                        " B OR BINARY 指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM04     =       "L" OR "LINE"
                   MOVE    "L"         TO      WK-F-IN-MODE
                   MOVE    SPACE       TO      WK-PIN2-F-NAME
           ELSE
               IF      WK-PRM04     =       "B" OR "BINARY"
                       MOVE    "B"         TO      WK-F-IN-MODE
                       MOVE    SPACE       TO      WK-PIN1-F-NAME
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F F-IN=XX,MODE=?"
                                       " PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " F-IN=XX,MODE=L OR LINE OR"
                                        " B OR BINARY 指定する"
                   STOP    RUN
               END-IF
           END-IF
           .
       S023-EX.
           EXIT.

      *    *** OUTPUT FILE FILE NAME
       S024-10.

           IF      P02-L    >     32
                   DISPLAY WK-PGM-NAME " PRM1-F F-OT=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                         " F-IN=XX アウトプットファイル名３２バイトまで"
                   STOP    RUN
           END-IF

           IF      WK-PRM02    NOT =   SPACE
                   MOVE    WK-PRM02    TO     WK-POT1-F-NAME
                   GO  TO  S024-20
           END-IF

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " OUTPUT FILE NAME ?"
                   ACCEPT  WK-POT1-F-NAME
                   DISPLAY WK-PGM-NAME " OUTPUT FILE NAME="
                           WK-POT1-F-NAME " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM
           .
       S024-20.
           IF      WK-PRM03    =       "MODE"
                   CONTINUE
           ELSE
               IF      WK-PRM03    =       SPACE
                   MOVE    "O"         TO      WK-F-OT-MODE
                   EXIT    PARAGRAPH
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F F-OT=XX,MODE=?"
                                       " PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " F-OT=XX,MODE=O OR OUTPUT OR"
                                        " E OR EXTEND 指定する"
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PRM04     =       "O" OR "OUTPUT"
                   MOVE    "O"         TO      WK-F-OT-MODE
           ELSE
               IF      WK-PRM04     =       "E" OR "EXTEND"
                       MOVE    "E"         TO      WK-F-OT-MODE
               ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F F-OT=XX,MODE=?"
                                       " PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " F-OT=XX,MODE=O OR OUTPUT OR"
                                        " E OR EXTEND 指定する"
                   STOP    RUN
               END-IF
           END-IF
           .
       S024-EX.
           EXIT.

      *    *** LISTID=
       S025-10.

           IF      P02-L    >     60
                   DISPLAY WK-PGM-NAME " PRM1-F LISTID=XX PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                         " LISTID=XX リストＩＤ名６０バイトまで"
                   STOP    RUN
           END-IF

           IF      WK-PRM02    =       SPACE
                   CONTINUE
           ELSE
                   MOVE    WK-PRM02    TO        WK-LISTID
           END-IF
           .
       S025-EX.
           EXIT.

      *    *** KEY=
       S026-10.
      *    *** 第１パラメータ
           IF      WK-PRM01    =       "KEY"      AND
                   WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 3
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO I5
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N 1-3の範囲で指定する"
                   STOP    RUN
           END-IF

           IF      I5          =       1
                   MOVE    "Y"         TO      SW-KEY1
           ELSE
               IF      I5          =       2
                   MOVE    "Y"         TO      SW-KEY2
               ELSE
                   MOVE    "Y"         TO      SW-KEY3
               END-IF
           END-IF

      *    *** 第２パラメータ
           IF      WK-PRM03    =       "POS"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY=N POS= 2つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM04(1:P04-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM04) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM04) <= 32760
                   MOVE    FUNCTION NUMVAL(WK-PRM04) TO TBL02-KEY-P (I5)
           ELSE
                 DISPLAY WK-PGM-NAME " PRM1-F KEY=N POS=NNN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N POS=NNN 1-32760の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第３パラメータ
           IF      WK-PRM05    =       "LEN"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN= PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEY=N LEN= 3つめに指定する"
                   STOP    RUN
           END-IF

           IF      WK-PRM06(1:P06-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM06) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM06) <= 10
                   MOVE    FUNCTION NUMVAL(WK-PRM06) TO TBL02-KEY-L (I5)
           ELSE
                 DISPLAY WK-PGM-NAME " PRM1-F KEY=N LEN=NNN PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N LEN=NNN 1-10の範囲で指定する"
                   STOP    RUN
           END-IF
           .
       S026-EX.
           EXIT.

      *    *** KEYMID=
       S027-10.
      *    *** 第１パラメータ
           IF      WK-PRM01    =       "KEYMID"   AND
                   WK-PRM02(1:P02-L) IS NUMERIC   AND
                   FUNCTION NUMVAL(WK-PRM02) >= 1 AND
                   FUNCTION NUMVAL(WK-PRM02) <= 4
                   MOVE    FUNCTION NUMVAL(WK-PRM02) TO I5
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEYMID=N PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                           " KEY=N 1-4の範囲で指定する"
                   STOP    RUN
           END-IF

      *    *** 第２パラメータ
           IF      WK-PRM03    =       "TITLE"
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEYMID=N PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME " KEYMID=N TITLE=2つめに指定する"
                   STOP    RUN
           END-IF
           IF      P04-L   =       ZERO
               OR  P04-L   >       10
                   DISPLAY WK-PGM-NAME " PRM1-F KEYMID=N PARA ERROR "
                           PRM1-REC
                   DISPLAY WK-PGM-NAME 
                        " KEYMID=N TITLE=XX ブレイクタイトルを指定する"
                        " 最大１０バイト、漢字可５文字で指定"
                        " P04-L=" P04-L
                   STOP    RUN
           END-IF

           IF      WK-PRM04    NOT =   SPACE
                   EVALUATE TRUE
                       WHEN FUNCTION NUMVAL(WK-PRM02) = 1
                           MOVE    WK-PRM04    TO      WK-KEY1-MID
                       WHEN FUNCTION NUMVAL(WK-PRM02) = 2
                           MOVE    WK-PRM04    TO      WK-KEY2-MID
                       WHEN FUNCTION NUMVAL(WK-PRM02) = 3
                           MOVE    WK-PRM04    TO      WK-KEY3-MID
      *    *** 4は総合計の見出し変更
                       WHEN FUNCTION NUMVAL(WK-PRM02) = 4
                           MOVE    WK-PRM04    TO      WK-KEY4-MID
                   END-EVALUATE
           END-IF
           .
       S027-EX.
           EXIT.

      *    *** インプットファイル，（カンマ）編集の時、ＣＳＶを指定する
      *    *** CSV=Y,LINE SEQUENTIAL可変長,BINARY SEQUENTIAL 固定長 可
      *    *** TYPE=PD はBINARY SEQUENTIAL の時のみ指定可
       S028-10.
           IF      WK-PRM02    =       "Y" OR "N"
                   MOVE    WK-PRM02 TO        SW-CSV
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F CSV= Y OR N ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF
           .
       S028-EX.
           EXIT.

      *    *** MID=
       S030-10.
           IF      WK-PRM02    =       "Y" OR "N"
                   MOVE    WK-PRM02 TO        SW-MID
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F MID= Y OR N ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** READ PIN1 OR PIN2
       S100-10.

           MOVE    KEY-NEW     TO      KEY-OLD
      *    *** 
           IF      WK-F-IN-MODE =      "L"
                   READ    PIN1-F
           ELSE
      *    *** MODE=BINARY はPIN2-F READ
                   MOVE    "N"         TO      SW-SET2
                   PERFORM S120-10     THRU    S120-EX
                           UNTIL WK-PIN2-EOF = HIGH-VALUE OR
                                 SW-SET2     = "Y"
           END-IF


           IF      WK-PIN1-STATUS =    ZERO OR 4
               IF  WK-F-IN-MODE = "L"
                   ADD     1            TO      WK-PIN1-CNT
               ELSE
                   CONTINUE
               END-IF
               IF      TBL02-KEY-P (1) =   ZERO AND
                       TBL02-KEY-P (2) =   ZERO AND
                       TBL02-KEY-P (3) =   ZERO
                   MOVE    SPACE       TO  KEY-NKEY1
                                           KEY-NKEY2
                                           KEY-NKEY3
               ELSE
                   MOVE    TBL02-KEY-P (1) TO P1
                   MOVE    TBL02-KEY-L (1) TO L1

                   MOVE    TBL02-KEY-P (2) TO P2
                   MOVE    TBL02-KEY-L (2) TO L2

                   MOVE    TBL02-KEY-P (3) TO P3
                   MOVE    TBL02-KEY-L (3) TO L3

                   MOVE    PIN1-REC(P1:L1) TO  KEY-NKEY1
                   MOVE    PIN1-REC(P2:L2) TO  KEY-NKEY2
                   MOVE    PIN1-REC(P3:L3) TO  KEY-NKEY3
               END-IF
           ELSE
      *    *** WK-PIN1-STATUS PIN1,PIN2 共通なので、このままにする
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
                                             KEY-NEW
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *    *** 19,1 から漢字始まる時、セットしない
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      M 

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 19
      *    *** 漢字か？一部漢字でない部分あり、
      *    *** 後から指定された部分も指定してない
               IF ( WK-TITLE2 (I:2) >= X"8140" AND 
                    WK-TITLE2 (I:2) <= X"9FFC" )   OR
                  ( WK-TITLE2 (I:2) >= X"E040" AND 
                    WK-TITLE2 (I:2) <= X"EAA4" )
                         MOVE   WK-TITLE2 (I:2) TO   WK-TITLE (M:2)
      *    *** J 1,3,5...
                         ADD    1    TO     I
                         ADD    2    TO     M
                   ELSE
                         MOVE   WK-TITLE2 (I:1) TO   WK-TITLE (M:1)
      *    *** J 1,2,3...
                         ADD    1    TO     M
                   END-IF
           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** 現在ＰＩＮ３未使用
       S110-10.

      *     MOVE    PIN1-MM     TO      PIN3-KEY
      *     COMPUTE PIN3-KEY = FUNCTION NUMVAL(WK-KEY2)
      *     COMPUTE PIN3-KEY = FUNCTION NUMVAL(KEY-OKEY2) 

           READ    PIN3-F
                   KEY PIN3-KEY
               INVALID 
                   CONTINUE
               NOT INVALID
                   CONTINUE
           END-READ

           IF      WK-PIN3-STATUS   =       ZERO
                   ADD     1           TO      WK-PIN3-CNT
           ELSE
               IF  WK-PIN3-STATUS   =       23
                   MOVE    SPACE       TO      WK-PIN3-I1
                                               WK-PIN3-I2
                                               WK-PIN3-I3
                                               WK-PIN3-I4
                   MOVE    ZERO        TO      WK-PIN3-STATUS
                   GO  TO  S110-EX
               ELSE
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR STATUS="
                           WK-PIN3-STATUS
                   DISPLAY WK-PGM-NAME " PIN3-F READ ERROR KEY="
                           PIN3-KEY
                   STOP    RUN
               END-IF
           END-IF

           UNSTRING PIN3-REC
                    DELIMITED BY ","
                    INTO
                    WK-PIN3-I1
                    WK-PIN3-I2
                    WK-PIN3-I3
                    WK-PIN3-I4  COUNT L
           .
       S110-EX.
           EXIT.

      *    *** パック項目はBINARYなので、ＥＮＤ－ＢＹＴＥ＝Ｘ”０Ｄ０Ａ”
      *    *** を１レコード単位に編集し、PIN1-REC にセットし直す
       S120-10.

           IF      SW-PIN2     =       ZERO
                   MOVE    HIGH-VALUE  TO      PIN2-REC
                   READ    PIN2-F
                   MOVE    "1"         TO      SW-PIN2


                   IF      WK-PIN2-STATUS =    ZERO OR 4
                           MOVE    WK-PIN2-STATUS TO    WK-PIN1-STATUS
                   ELSE
                           IF  WK-PIN2-STATUS =    10
                               MOVE    HIGH-VALUE  TO    WK-PIN2-EOF
                               MOVE    WK-PIN2-STATUS TO WK-PIN1-STATUS
                               EXIT    PARAGRAPH
                           ELSE
                               DISPLAY WK-PGM-NAME
                                       " PIN2-F READ ERROR STATUS="
                                       WK-PIN2-STATUS
                               STOP    RUN
                           END-IF
                   END-IF
           ELSE
                   CONTINUE
           END-IF

      *    *** PIN2-F（バイナリーファイル） => PIN1-F へセット
      *    *** Q4 初期値は１
      *    *** SW-PIN2 初期値は０,READ直後は１、
      *    *** 0D0AがあってPIN1-RECセットした時は２、
      *    *** 
           IF      SW-PIN2     =      "1" OR "2" OR "3"
             PERFORM WITH TEST BEFORE
                     VARYING Q FROM Q4 BY 1
                     UNTIL   Q > WK-PIN2-LEN      OR
                             SW-SET2 = "Y"

               IF      Q2      >=      32759
                       DISPLAY WK-PGM-NAME " PIN2-F に X'0D0A' がない"
                       STOP    RUN
               END-IF
      *    *** Q < WK-PIN2-LEN を判定して、レコード範囲内で"FF"判定する
               IF    ( Q                    < WK-PIN2-LEN AND
                       PIN2-REC (Q:1)       = X"FF" AND 
                       PIN2-REC (Q + 1 : 1) = X"FF"     ) OR
                     ( PIN2-REC (Q:1)       = X"FF" AND 
                       WK-END-BYTE          = X"FF"     )
                       MOVE    WK-PIN2-LEN TO      Q
                       MOVE    ZERO        TO      SW-PIN2
                       MOVE    "Y"         TO      SW-SET2
                       MOVE    10          TO      WK-PIN2-STATUS
                       MOVE    WK-PIN2-STATUS TO   WK-PIN1-STATUS
                       EXIT    PARAGRAPH
               ELSE
                   IF    ( Q                    < WK-PIN2-LEN AND
                           PIN2-REC (Q:1)       = X"0D" AND 
                           PIN2-REC (Q + 1 : 1) = X"0A"     ) OR
                         ( PIN2-REC (Q:1)       = X"0A" AND 
                           WK-END-BYTE          = X"0D"     )
                           ADD     1           TO      WK-PIN2-CNT
                           IF  PIN2-REC (Q:1)       = X"0D" AND 
                               PIN2-REC (Q + 1 : 1) = X"0A"
                               ADD     1           TO      Q2
      *    *** Q3=レコード長 0D0A含む長さ
                               COMPUTE Q3 = Q2 + 1
                               MOVE    X"0D0A"     TO    PIN1-REC (Q2:2)

      *    *** Q,Q4 次のレコードの開始位置に変更
                               ADD     2           TO      Q
                           ELSE
      *    *** 0D0A 2レコードに分かれているとき
                               ADD     1           TO      Q2
                               COMPUTE Q3 = Q2
                               MOVE    X"0A"       TO    PIN1-REC (Q2:1)
                               ADD     1           TO      Q
                           END-IF

                           MOVE    ZERO        TO      Q2
      *    *** Q4 をX"0A"の次のバイトにする
                           MOVE    Q           TO      Q4
                           MOVE    "2"         TO      SW-PIN2
                           MOVE    "Y"         TO      SW-SET2
                           IF      Q           >       WK-PIN2-LEN
      *    *** Q > WK-PIN2-LEN の時、次のレコードＲＥＡＤの為、SW-PIN2=0にする
                                   MOVE    ZERO        TO      SW-PIN2
                                   MOVE    1           TO      Q4
                           END-IF
                   ELSE
      *    *** X"0D0A" 以外の時
      *    *** SW-PIN2=1はＲＥＡＤ直後
      *    *** Qは  PIN2-REC サーチ位置
      *    *** Q2は PIN1-REC セット位置
      *    *** Q3は レコード長
                     IF    ( SW-PIN2     =      "1"   ) OR
      *    *** レコード最後がレコード長満たないとき、１バイトづつ送る
      *    *** Q + Q3 > WK-PIN2-LEN
                           ( Q + Q3      >      WK-PIN2-LEN )
                           ADD     1           TO      Q2
                           MOVE    PIN2-REC (Q:1) TO   PIN1-REC (Q2:1)
                           IF      Q       >=     WK-PIN2-LEN
      *    *** PIN1-REC 最後までサーチした時、次のレコードREADの為、
      *    *** SW-PIN2<=0, Q4<=1 サーチ開始位置リセットする
                                   MOVE    ZERO        TO      SW-PIN2
                                   MOVE    1           TO      Q4
      *    *** 最終バイトセーブ
                                   MOVE    PIN2-REC (Q:1) TO WK-END-BYTE
                           END-IF
                     ELSE
      *    *** 1回でも、0D0Aあったとき、Ｑ３：レコード長確定している
      *    *** SW-PIN2=2になっている
                           MOVE    PIN2-REC (Q:Q3) TO  PIN1-REC (1:Q3)
                           ADD     1           TO      WK-PIN2-CNT

                           ADD     Q3          TO      Q
      *    *** Q4 PIN2-REC サーチ開始位置、次のレコードの先頭にする
                           MOVE    Q           TO      Q4
                           MOVE    "3"         TO      SW-PIN2
                           MOVE    "Y"         TO      SW-SET2
                           IF      Q           <       WK-PIN2-LEN
                               IF      PIN2-REC (Q - 2 : 2) = X"0D0A"
                                       CONTINUE
                               ELSE
                                       DISPLAY WK-PGM-NAME
                                              " PIN2-F READ X'0D0A' ﾅｼ "
                                              "WK-PIN2-CNT=" WK-PIN2-CNT
                                               " Q(ｶﾗﾑ)=" Q 
                                       CALL    "COBDUMP" USING 
                                               PIN2-REC (Q - 2 : 2 )
                                       DISPLAY WK-PGM-NAME
                                               " PIN2-F レコード長=" Q3
                                               " 固定長のみ可"
                                       STOP    RUN
                               END-IF
                           ELSE
                               CONTINUE
                           END-IF
                     END-IF
                   END-IF
               END-IF
             END-PERFORM
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** PIN1 PRINT,SUM
       S200-10.

      *    *** PIN2 AT END 時、PIN1もHIGH-VALUE になるので、このままにする
           IF      WK-PIN1-EOF   =     HIGH-VALUE
                   CONTINUE
           ELSE
      *    *** MEISAI PRINT TBL SET
                   PERFORM S210-10     THRU    S210-EX

      *    *** PIN1-F READ 
                   PERFORM S100-10     THRU    S100-EX
           END-IF

           IF      KEY-OLD     =       KEY-NEW
               CONTINUE
           ELSE
               IF      SW-KEY3     =       "Y"
      *    *** KEY1,KEY2,KEY3 ブレイク
                   PERFORM S300-10     THRU    S300-EX
               END-IF

               IF      KEY-OLD2  =         KEY-NEW2
                   CONTINUE
               ELSE
                   IF      SW-KEY2     =       "Y"
      *    *** KEY1,KEY2 ブレイク
                       PERFORM S310-10     THRU    S310-EX
                   END-IF

                   IF      KEY-OKEY1 =         KEY-NKEY1
                       CONTINUE
                   ELSE
                       IF      SW-KEY1     =       "Y"
      *    *** KEY1 ブレイク
                           PERFORM S320-10     THRU    S320-EX
                       END-IF

      *    *** 0件でも、ＡＴ　ＥＮＤ時、総合計 件数出力
                       IF      KEY-NEW       =     HIGH-VALUE
      *    *** AT END ブレイク
                           PERFORM S330-10     THRU    S330-EX
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** MEISAI PRINT TBL SET
       S210-10.

           ADD     1           TO      WK-KEY1-CNT
                                       WK-KEY2-CNT
                                       WK-KEY3-CNT

           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL I2 > WK-TBL01-MAX

               MOVE    TBL01-DT-P (I2) TO P
               MOVE    TBL01-DT-L (I2) TO L
               MOVE    TBL01-DT-P-L (I2) TO P-L
               COMPUTE N = 12 - L
               COMPUTE P-N = 7 - P-L
               IF    ( TBL01-DT-TYPE (I2) = "ZD" OR "Z1" OR "Z2" OR 
                                            "Z3" OR
                                            "PD" OR "P1" OR "P2" OR 
                                            "P3" ) AND 
                       TBL01-DT-SUM (I2) =  "Y"

                   IF      TBL01-DT-TYPE(I2) (1:1) =  "Z"
                       MOVE    ZERO        TO      WK-ZSU
                       MOVE    PIN1-REC(P:L) TO    WK-ZSU-X(N:L)

                       IF      WK-ZSU  NOT NUMERIC
                           DISPLAY WK-PGM-NAME 
                                   " PIN1-REC NOT NUMEIC P=" P " L=" L
                           DISPLAY WK-PGM-NAME 
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           DISPLAY WK-PGM-NAME 
                                   " WK-ZSU-X=" WK-ZSU-X " S200-10"
                           STOP    RUN
                       END-IF
                   ELSE
                       MOVE    ZERO        TO      WK-PSU
                       MOVE    PIN1-REC (P:P-L) TO  WK-PSU-X (P-N:P-L)

                       IF      WK-PSU  NOT NUMERIC
                           DISPLAY WK-PGM-NAME 
                                   " PIN2-REC NOT NUMEIC P=" P
                                   " P-L=" P-L
                           DISPLAY WK-PGM-NAME 
                                   " WK-PIN2-CNT=" WK-PIN2-CNT
                           DISPLAY WK-PGM-NAME 
                                   " WK-PSU-X=" " S200-10"
                           CALL    "COBDUMP" USING WK-PSU-X
                           STOP    RUN
                       END-IF
                       MOVE    WK-PSU  TO      WK-ZSU
                   END-IF

                   ADD     WK-ZSU      TO      TBL01-DT-K3SU (I2)
                       ON SIZE ERROR
                       DISPLAY WK-PGM-NAME 
                               " ADD TBL01-DT-K3SU ON SIZE ERROR"
                       DISPLAY WK-PGM-NAME 
                               " WK-PIN1-CNT=" WK-PIN1-CNT
                               " WK-PIN2-CNT=" WK-PIN2-CNT
                       STOP    RUN
                   END-ADD

                   ADD     WK-ZSU      TO      TBL01-DT-K2SU (I2)
                       ON SIZE ERROR
                       DISPLAY WK-PGM-NAME 
                               " ADD TBL01-DT-K2SU ON SIZE ERROR"
                       DISPLAY WK-PGM-NAME 
                               " WK-PIN1-CNT=" WK-PIN1-CNT
                               " WK-PIN2-CNT=" WK-PIN2-CNT
                       STOP    RUN
                   END-ADD

                   ADD     WK-ZSU      TO      TBL01-DT-K1SU (I2)
                       ON SIZE ERROR
                       DISPLAY WK-PGM-NAME 
                               " ADD TBL01-DT-K1SU ON SIZE ERROR"
                       DISPLAY WK-PGM-NAME 
                               " WK-PIN1-CNT=" WK-PIN1-CNT
                               " WK-PIN2-CNT=" WK-PIN2-CNT
                       STOP    RUN
                   END-ADD

                   ADD     WK-ZSU      TO      TBL01-DT-TOSU (I2)
                       ON SIZE ERROR
                       DISPLAY WK-PGM-NAME 
                               " ADD TBL01-DT-TOSU ON SIZE ERROR"
                       DISPLAY WK-PGM-NAME 
                               " WK-PIN1-CNT=" WK-PIN1-CNT
                               " WK-PIN2-CNT=" WK-PIN2-CNT
                       STOP    RUN
                   END-ADD
                 END-IF
           END-PERFORM

           IF      SW-MEISAI       =     "0"
                   CONTINUE
           ELSE

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-TBL01-MAX 
                       MOVE    TBL01-DT-P (I) TO P
                       MOVE    TBL01-DT-L (I) TO L
                       MOVE    TBL01-DT-P-L (I) TO P-L
                       COMPUTE N = 12 - L
                       COMPUTE P-N = 7 - P-L
                       MOVE    PN(I)         TO    P5
                       IF      TBL01-DT-TYPE (I) (1:1) =  "Z" OR 
                               TBL01-DT-TYPE (I) =  "C1" OR "C2" OR
                                                   "C3" OR "CT"
                           EVALUATE TRUE
                               WHEN TBL01-DT-TYPE (I) (1:2) =  "C1"
                                   MOVE    WK-KEY1-CNT   TO    WK-ZSU
                               WHEN TBL01-DT-TYPE (I) (1:2) =  "C2"
                                   MOVE    WK-KEY2-CNT   TO    WK-ZSU
                               WHEN TBL01-DT-TYPE (I) (1:2) =  "C3"
                                   MOVE    WK-KEY3-CNT   TO    WK-ZSU
                               WHEN TBL01-DT-TYPE (I) (1:2) =  "CT"
                                   IF      WK-F-IN-MODE =      "L"
                                       MOVE    WK-PIN1-CNT TO   WK-ZSU
                                   ELSE
                                       MOVE    WK-PIN2-CNT TO   WK-ZSU
                                   END-IF
                               WHEN OTHER
                                   MOVE    ZERO          TO    WK-ZSU
                                   MOVE    PIN1-REC (P:L) TO WK-ZSU(N:L)
                           END-EVALUATE
                       ELSE
                           IF      TBL01-DT-TYPE (I) (1:1) = "P"
                               MOVE    ZERO        TO      WK-PSU
                               MOVE    PIN1-REC (P:P-L) TO
                                       WK-PSU-X (P-N:P-L)
                               IF      WK-PSU  NOT NUMERIC
                                   DISPLAY WK-PGM-NAME 
                                       " PIN2-REC NOT NUMEIC P=" P
                                       " P-L=" P-L
                                   DISPLAY WK-PGM-NAME 
                                       " WK-PIN2-CNT=" WK-PIN2-CNT
                                   DISPLAY WK-PGM-NAME 
                                       " WK-PSU-X=" " S200-10"
                                   CALL    "COBDUMP" USING WK-PSU-X
                                   STOP    RUN
                               END-IF
                               MOVE    WK-PSU  TO      WK-ZSU
                           END-IF
                       END-IF

                       MOVE    "N"           TO    SW-KEY-BR

                       EVALUATE TRUE
                           WHEN TBL01-DT-TYPE (I) = "CH"
                               MOVE    PIN1-REC(P:L) TO
                                         PR-LINE (J) (P5:L)
                           WHEN TBL01-DT-TYPE(I) = "HE"
      *    *** TYPE=HE ヘキサ変換、プリントエリアにセット
                               PERFORM S260-10     THRU    S260-EX
                           WHEN TBL01-DT-TYPE (I) = "ZD" OR "PD" OR
                                                    "C1" OR "C2" OR 
                                                    "C3" OR "CT" 
      *    *** TYPE=ZD,PD
                               PERFORM S220-10     THRU    S220-EX
                           WHEN TBL01-DT-TYPE (I) = "Z1" OR "P1"
      *    *** TYPE=Z1,P1
                               PERFORM S230-10     THRU    S230-EX
                           WHEN TBL01-DT-TYPE (I) = "Z2" OR "P2"
      *    *** TYPE=Z2,P2
                               PERFORM S240-10     THRU    S240-EX
                           WHEN TBL01-DT-TYPE (I) = "Z3" OR "P3"
      *    *** TYPE=Z3,P3
                               PERFORM S250-10     THRU    S250-EX
                       END-EVALUATE
                   END-PERFORM

                   ADD     1           TO      J
                   MOVE    1           TO      J2
      *    *** PRINT TBL WRITE CHECK
                   PERFORM S280-10     THRU    S280-EX
           END-IF
           .
       S210-EX.
           EXIT.

      *    *** TYPE=ZD,PD
       S220-10.

           IF      WK-ZSU  NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC NOT NUMEIC P=" P " L=" L
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-ZSU-X=" WK-ZSU-X " S220-10"
                   STOP    RUN
           END-IF

      *    *** 件数出力　オーバーフローチェック
           IF      TBL01-DT-TYPE(I) = "C1" OR "C2" OR "C3" OR "CT"
                   IF    ( TBL01-DT-L(I) =     1 AND
                           WK-ZSU      >       9 )  OR
                         ( TBL01-DT-L(I) =     2 AND
                           WK-ZSU      >       99 )  OR
                         ( TBL01-DT-L(I) =     3 AND
                           WK-ZSU      >       999 )  OR
                         ( TBL01-DT-L(I) =     4 AND
                           WK-ZSU      >       9999 )  OR
                         ( TBL01-DT-L(I) =     5 AND
                           WK-ZSU      >       99999 )  OR
                         ( TBL01-DT-L(I) =     6 AND
                           WK-ZSU      >       999999 )  OR
                         ( TBL01-DT-L(I) =     7 AND
                           WK-ZSU      >       9999999 )  OR
                         ( TBL01-DT-L(I) =     8 AND
                           WK-ZSU      >       99999999 )  OR
                         ( TBL01-DT-L(I) =     9 AND
                           WK-ZSU      >       999999999 )
                           DISPLAY WK-PGM-NAME 
                                   " TYPE=C1,C2,C3,C4 指定時の LEN=N "
                                   "N 桁足りない Nの値大きくする" 
                           DISPLAY WK-PGM-NAME 
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           DISPLAY WK-PGM-NAME 
                                   " WK-ZSU-X=" WK-ZSU-X " S220-10"
                           STOP    RUN
                   END-IF
           END-IF

           IF      SW-KEY-BR   =       "N"
                   MOVE    TBL01-DT-HENP(I) TO HP
                   MOVE    TBL01-DT-HENL(I) TO HL
           ELSE
      *    *** SW-KEY-BR = "Y" は総合計の時
                   MOVE    1           TO      HP
                   EVALUATE TRUE
                       WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                           MOVE    12          TO      HL
                       WHEN TBL01-DT-HEN(I) = "EE" 
                           MOVE    13          TO      HL
                       WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                           MOVE    15          TO      HL
      *    *** EK,
                       WHEN OTHER
                           MOVE    16          TO      HL
                   END-EVALUATE
           END-IF
           EVALUATE TRUE

               WHEN TBL01-DT-HEN (I) =  "99"
                   MOVE    WK-ZSU      TO      WK-ESU-99
                   MOVE    WK-ESU-99(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "9K"
                   MOVE    WK-ZSU      TO      WK-ESU-9K
                   MOVE    WK-ESU-9K(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZZ"
                   MOVE    WK-ZSU      TO      WK-ESU-ZZ
                   MOVE    WK-ESU-ZZ(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZK"
                   MOVE    WK-ZSU      TO      WK-ESU-ZK
                   MOVE    WK-ESU-ZK(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EE"
                   MOVE    WK-ZSU      TO      WK-ESU-EE
                   MOVE    WK-ESU-EE(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EK"
                   MOVE    WK-ZSU      TO      WK-ESU-EK
                   MOVE    WK-ESU-EK(HP:HL) TO PR-LINE (J)
                                               (P5:HL)
           END-EVALUATE
           .
       S220-EX.
           EXIT.

      *    *** TYPE=Z1,P1
       S230-10.

           IF      WK-ZSU  NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC NOT NUMEIC P=" P " L=" L
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-ZSU-X=" WK-ZSU-X " S230-10"
                   STOP    RUN
           END-IF

           IF      SW-KEY-BR   =       "N"
                   MOVE    TBL01-DT-HENP(I) TO HP
                   MOVE    TBL01-DT-HENL(I) TO HL
           ELSE
                   MOVE    1           TO      HP
                   EVALUATE TRUE
                       WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                           MOVE    13          TO      HL
                       WHEN TBL01-DT-HEN(I) = "EE" 
                           MOVE    14          TO      HL
                       WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                           MOVE    16          TO      HL
      *    *** EK,
                       WHEN OTHER
                           MOVE    17          TO      HL
                   END-EVALUATE
           END-IF

           COMPUTE WK-ZSU-1 = WK-ZSU / 10.0

           EVALUATE TRUE

               WHEN TBL01-DT-HEN (I) =  "99"
                   MOVE    WK-ZSU-1    TO      WK-ESU-99-1
                   MOVE    WK-ESU-99-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "9K"
                   MOVE    WK-ZSU-1    TO      WK-ESU-9K-1
                   MOVE    WK-ESU-9K-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZZ"
                   MOVE    WK-ZSU-1    TO      WK-ESU-ZZ-1
                   MOVE    WK-ESU-ZZ-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZK"
                   MOVE    WK-ZSU-1    TO      WK-ESU-ZK-1
                   MOVE    WK-ESU-ZK-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EE"
                   MOVE    WK-ZSU-1    TO      WK-ESU-EE-1
                   MOVE    WK-ESU-EE-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EK"
                   MOVE    WK-ZSU-1    TO      WK-ESU-EK-1
                   MOVE    WK-ESU-EK-1(HP:HL) TO PR-LINE (J)
                                               (P5:HL)
           END-EVALUATE
           .
       S230-EX.
           EXIT.

      *    *** TYPE=Z2,P2
       S240-10.

           IF      WK-ZSU  NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC NOT NUMEIC P=" P " L=" L
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-ZSU-X=" WK-ZSU-X " S240-10"
                   STOP    RUN
           END-IF

           IF      SW-KEY-BR   =       "N"
                   MOVE    TBL01-DT-HENP(I) TO HP
                   MOVE    TBL01-DT-HENL(I) TO HL
           ELSE
                   MOVE    1           TO      HP
                   EVALUATE TRUE
                       WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                           MOVE    13          TO      HL
                       WHEN TBL01-DT-HEN(I) = "EE" 
                           MOVE    14          TO      HL
                       WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                           MOVE    15          TO      HL
      *    *** EK,
                       WHEN OTHER
                           MOVE    16          TO      HL
                   END-EVALUATE
           END-IF

           COMPUTE WK-ZSU-2 = WK-ZSU / 100.0

           EVALUATE TRUE

               WHEN TBL01-DT-HEN (I) =  "99"
                   MOVE    WK-ZSU-2    TO      WK-ESU-99-2
                   MOVE    WK-ESU-99-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "9K"
                   MOVE    WK-ZSU-2    TO      WK-ESU-9K-2
                   MOVE    WK-ESU-9K-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZZ"
                   MOVE    WK-ZSU-2    TO      WK-ESU-ZZ-2
                   MOVE    WK-ESU-ZZ-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZK"
                   MOVE    WK-ZSU-2    TO      WK-ESU-ZK-2
                   MOVE    WK-ESU-ZK-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EE"
                   MOVE    WK-ZSU-2    TO      WK-ESU-EE-2
                   MOVE    WK-ESU-EE-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EK"
                   MOVE    WK-ZSU-2    TO      WK-ESU-EK-2
                   MOVE    WK-ESU-EK-2(HP:HL) TO PR-LINE (J)
                                               (P5:HL)
           END-EVALUATE
           .
       S240-EX.
           EXIT.

      *    *** TYPE=Z3,P3
       S250-10.

           IF      WK-ZSU  NOT NUMERIC
                   DISPLAY WK-PGM-NAME 
                           " PIN1-REC NOT NUMEIC P=" P " L=" L
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-ZSU-X=" WK-ZSU-X " S250-10"
                   STOP    RUN
           END-IF

           IF      SW-KEY-BR   =       "N"
                   MOVE    TBL01-DT-HENP(I) TO HP
                   MOVE    TBL01-DT-HENL(I) TO HL
           ELSE
                   MOVE    1           TO      HP
                   EVALUATE TRUE
                       WHEN TBL01-DT-HEN(I) = "99" OR "ZZ" 
                           MOVE    13          TO      HL
                       WHEN TBL01-DT-HEN(I) = "EE" 
                           MOVE    14          TO      HL
                       WHEN TBL01-DT-HEN(I) = "9K" OR "ZK"
                           MOVE    15          TO      HL
      *    *** EK,
                       WHEN OTHER
                           MOVE    16          TO      HL
                   END-EVALUATE
           END-IF

           COMPUTE WK-ZSU-3 = WK-ZSU / 1000.0

           EVALUATE TRUE

               WHEN TBL01-DT-HEN (I) =  "99"
                   MOVE    WK-ZSU-3    TO      WK-ESU-99-3
                   MOVE    WK-ESU-99-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "9K"
                   MOVE    WK-ZSU-3    TO      WK-ESU-9K-3
                   MOVE    WK-ESU-9K-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZZ"
                   MOVE    WK-ZSU-3    TO      WK-ESU-ZZ-3
                   MOVE    WK-ESU-ZZ-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "ZK"
                   MOVE    WK-ZSU-3    TO      WK-ESU-ZK-3
                   MOVE    WK-ESU-ZK-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EE"
                   MOVE    WK-ZSU-3    TO      WK-ESU-EE-3
                   MOVE    WK-ESU-EE-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)

               WHEN TBL01-DT-HEN (I) =  "EK"
                   MOVE    WK-ZSU-3    TO      WK-ESU-EK-3
                   MOVE    WK-ESU-EK-3(HP:HL) TO PR-LINE (J)
                                               (P5:HL)
           END-EVALUATE
           .
       S250-EX.
           EXIT.

      *    *** TYPE=HE ヘキサ変換、プリントエリアにセット
       S260-10.

           MOVE    ZERO        TO      H2
           MOVE    P5          TO      P5L
           ADD     P5 1        GIVING  P5R

           PERFORM VARYING H1 FROM P BY 1
                   UNTIL   H2 = L

                   ADD     1           TO      H2

                   MOVE    PIN1-REC (H1:1) TO  PIC-X

                   DIVIDE PIC-Halfword BY 16
                          GIVING    Left-Nibble
                          REMAINDER Right-Nibble

                   ADD     1           TO      Left-Nibble Right-Nibble

                   MOVE    Hex-Digit (Left-Nibble)
                                       TO      PR-LINE (J) (P5L:1)
                   MOVE    Hex-Digit (Right-Nibble)
                                       TO      PR-LINE (J) (P5R:1)
                   ADD     2           TO      P5L P5R
           END-PERFORM
           .
       S260-EX.
           EXIT.

      *    *** PRINT TBL WRITE
       S270-10.

           ADD     1           TO      WK-PAGE
           MOVE    WK-PAGE     TO      WK-TIT1-PAGE
           MOVE    SPACE       TO      POT1-REC
           IF      PRM2-MOJISU <       105
                   COMPUTE U = PRM2-MOJISU - 89
           ELSE
                   COMPUTE U = C - (( C - 88 ) / 2 ) - 89
           END-IF

           IF      SW-A4TATE   =       "1"

      *             MOVE    WK-TIT1     TO      WK-TIT1-A4T-1
      *             WRITE   POT1-REC    FROM    WK-TIT1-A4T
      *    *** ヘッダー１行目 リスト中央に出力
                   MOVE    WK-TIT1     TO  POT1-REC (U:88)
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-LISTID   TO      WK-TIT2-LIDT
                   WRITE   POT1-REC    FROM    WK-TIT2-A4T
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    SPACE       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   WRITE   POT1-REC    FROM    WK-MID1-A4T
                   ADD     1           TO      WK-POT1-CNT

                   WRITE   POT1-REC    FROM    WK-HAI-A4T
                   ADD     1           TO      WK-POT1-CNT
           ELSE
      *             MOVE    WK-TIT1     TO      WK-TIT1-A4Y-1
      *             WRITE   POT1-REC    FROM    WK-TIT1-A4Y
                   MOVE    WK-TIT1     TO  POT1-REC (U:88)
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    WK-LISTID   TO      WK-TIT2-LIDY
                   WRITE   POT1-REC    FROM    WK-TIT2-A4Y
                   ADD     1           TO      WK-POT1-CNT

                   MOVE    SPACE       TO      POT1-REC
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   WRITE   POT1-REC    FROM    WK-MID1-A4Y
                   ADD     1           TO      WK-POT1-CNT

                   WRITE   POT1-REC    FROM    WK-HAI-A4Y
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    5           TO      L4
           PERFORM VARYING K  FROM 1 BY 1
                   UNTIL   K   >       R
                   WRITE   POT1-REC    FROM    PR-LINE (K)
                   ADD     1           TO      WK-POT1-CNT
                                               L4
                   IF      SW-KAIGYO   =       "2"
                        IF      K          =         R
                                CONTINUE
                        ELSE
                                MOVE    SPACE     TO      POT1-REC
                                WRITE   POT1-REC
                                ADD     1         TO      WK-POT1-CNT
                                                          L4
                        END-IF
                   ELSE
                            CONTINUE
                   END-IF
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           PERFORM VARYING K  FROM L4 BY 1
                   UNTIL   K   >=      PRM2-GYOU
                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S270-EX.
           EXIT.

      *    *** PRINT TBL WRITE CHECK
       S280-10.

           IF      J           >       R
                   MOVE    1           TO       J
      *    *** 縦列　行ＭＡＸ超えたら、右へ移動する
      *    *** １個目の項目が数字項目の時、ブレイク時の出力位置がずれる
      *    *** 為、WK-PN1 に退避する
      *    *** ここを修正すると、２列目以降が直る
                   ADD     CNS-L-SIZE 1 TO      WK-PN1
 
                   ADD     CNS-L-SIZE 1 TO      PN (1)

                   IF      PN (1)      >        C

      *    *** PRINT TBL WRITE
                           PERFORM S270-10     THRU    S270-EX

                           MOVE    SPACE       TO      PRINT-AREA
                           PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > WK-TBL01-MAX 
                                   MOVE    CNS-P(I)  TO      PN(I)
      *    *** １個目の項目が数字項目の時、ブレイク時の出力位置がずれる
      *    *** 為、WK-PN1 に退避する
      *    *** ここを修正すると、１列目が直る
                                   IF      I         =       1
                                       MOVE    1         TO      WK-PN1
                                   END-IF
      *    *** ZD,PD等の項目は、右寄せにする
                                IF  TBL01-DT-TYPE (I) (1:1) = "Z" OR "P"
                                  OR
                                    TBL01-DT-TYPE (I) = "C1" OR "C2" OR
                                                        "C3" OR "CT"
                                  IF  TBL01-DT-HENL (I) < TBL01-DT-TITL
                                                                     (I)
                                      COMPUTE PN (I) = PN (I) 
                                                 + TBL01-DT-TITL (I) 
                                                 - TBL01-DT-HENL (I)
                                  END-IF
                                END-IF
                           END-PERFORM
                   ELSE
                           PERFORM VARYING I FROM 2 BY 1
                                   UNTIL I > WK-TBL01-MAX 
      *    *** PX2 は総合計時の、SUM=Y指定時や件数、位置自動調整する
      *    *** CNS-L-SIZE には、PX2が加算されてる
                                   ADD     CNS-L-SIZE 1 TO   PN (I)
                           END-PERFORM
                   END-IF
           END-IF
           .
       S280-EX.
           EXIT.

      *    *** KEY1,KEY2,KEY3 ブレイク
       S300-10.

      *    *** PIN1 ZERO件の時、ブレイク件数表示しない
           IF      KEY-OLD     =     LOW-VALUE
                   CONTINUE
           ELSE
               IF      J2      =       1
                   IF      SW-KAIGYO-BR =  "1" AND
                           SW-KAIGYO    =  "1"
                       ADD     1       TO      J
                   END-IF
               END-IF
      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX

      *    *** PIN3 READ
      *         PERFORM S110-10     THRU    S110-EX

               MOVE    WK-PN1      TO       PX
               MOVE    WK-KEY3-MID     TO  PR-LINE (J) (PX:10)
               COMPUTE PX = PX + 11

               MOVE    KEY-OKEY1 (1:L1) TO PR-LINE (J) (PX:L1)

               COMPUTE PX = PX + L1 + 1
               MOVE    KEY-OKEY2 (1:L2) TO PR-LINE (J) (PX:L2)

               COMPUTE PX = PX + L2 + 1
               MOVE    KEY-OKEY3 (1:L3) TO PR-LINE (J) (PX:L3)

               COMPUTE PX = PX + L3 + 1

               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                 IF    ( TBL01-DT-TYPE(I) =  "ZD" OR "Z1" OR "Z2" OR
                                             "Z3" OR
                                             "PD" OR "P1" OR "P2" OR
                                             "P3" ) AND 
                         TBL01-DT-SUM (I) =  "Y"

                       MOVE    TBL01-DT-TIT (I)  TO
                               PR-LINE (J) (PX:TBL01-DT-TITL (I))

                       COMPUTE PX = PX + TBL01-DT-TITL(I) + 1

                       MOVE    TBL01-DT-K3SU (I) TO WK-ZSU
                       MOVE    "Y"         TO      SW-KEY-BR

                       MOVE   PX    TO    P5
                       EVALUATE TRUE
                           WHEN TBL01-DT-TYPE (I) = "ZD" OR "PD"
      *    *** TYPE=ZD,PD
                                PERFORM S220-10     THRU    S220-EX
                           WHEN TBL01-DT-TYPE (I) = "Z1" OR "P1"
      *    *** TYPE=Z1,P1
                                PERFORM S230-10     THRU    S230-EX
                           WHEN TBL01-DT-TYPE (I) = "Z2" OR "P2"
      *    *** TYPE=Z2,P2
                                PERFORM S240-10     THRU    S240-EX
                           WHEN TBL01-DT-TYPE (I) = "Z3" OR "P3"
      *    *** TYPE=Z3,P3
                                PERFORM S250-10     THRU    S250-EX
                       END-EVALUATE

                       COMPUTE PX = PX + HL + 1

                  END-IF
               END-PERFORM

               MOVE    WK-KENSU    TO      PR-LINE (J) (PX:04)
               COMPUTE PX = PX + 4
               MOVE    WK-KEY3-CNT TO      WK-CNT3
               MOVE    WK-CNT3     TO      PR-LINE (J) (PX:11)

               MOVE    ZERO        TO      WK-KEY3-CNT
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                       MOVE    ZERO        TO      TBL01-DT-K3SU (I)
               END-PERFORM

               IF      SW-KAIGYO-BR =  "0"
                   ADD     1           TO      J
               ELSE
                   ADD     2           TO      J
               END-IF
               MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX

           END-IF
           .
       S300-EX.
           EXIT.

      *    *** KEY1,KEY2 ブレイク
       S310-10.

      *    *** PIN1 ZERO件の時、ブレイク件数表示しない
           IF      KEY-OLD     =     LOW-VALUE
                   CONTINUE
           ELSE
               IF      J2      =       1
                   IF      SW-KAIGYO-BR =  "1" AND
                           SW-KAIGYO    =  "1"
                       ADD     1       TO      J
                   END-IF
               END-IF
      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX

      *    *** PIN3 READ
      *         PERFORM S110-10     THRU    S110-EX

               MOVE    WK-PN1      TO       PX

               MOVE    WK-KEY2-MID     TO  PR-LINE (J) (PX:10)
               COMPUTE PX = PX + 11

               MOVE    KEY-OKEY1 (1:L1) TO PR-LINE (J) (PX:L1)

               COMPUTE PX = PX + L1 + 1
               MOVE    KEY-OKEY2 (1:L2) TO PR-LINE (J) (PX:L2)

               COMPUTE PX = PX + L2 + 1

               IF      L3  NOT = ZERO
                   COMPUTE PX = PX + L3 + 1
               END-IF

               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                 IF    ( TBL01-DT-TYPE (I) = "ZD" OR "Z1" OR "Z2" OR
                                             "Z3" OR
                                             "PD" OR "P1" OR "P2" OR
                                             "P3" ) AND 
                         TBL01-DT-SUM (I) =  "Y"

                       MOVE    TBL01-DT-TIT(I)  TO
                               PR-LINE (J) (PX:TBL01-DT-TITL(I))

                       COMPUTE PX = PX + TBL01-DT-TITL(I) + 1

                       MOVE    TBL01-DT-K2SU (I) TO WK-ZSU
                       MOVE    "Y"         TO      SW-KEY-BR

                       MOVE   PX    TO    P5
                       EVALUATE TRUE
                           WHEN TBL01-DT-TYPE (I) = "ZD" OR "PD"
      *    *** TYPE=ZD,PD
                                PERFORM S220-10     THRU    S220-EX
                           WHEN TBL01-DT-TYPE (I) = "Z1" OR "P1"
      *    *** TYPE=Z1,P1
                                PERFORM S230-10     THRU    S230-EX
                           WHEN TBL01-DT-TYPE (I) = "Z2" OR "P2"
      *    *** TYPE=Z2,P2
                                PERFORM S240-10     THRU    S240-EX
                           WHEN TBL01-DT-TYPE (I) = "Z3" OR "P3"
      *    *** TYPE=Z3,P3
                                PERFORM S250-10     THRU    S250-EX
                       END-EVALUATE

                       COMPUTE PX = PX + HL + 1
                  END-IF
               END-PERFORM
               MOVE    WK-KENSU    TO      PR-LINE (J) (PX:04)
               COMPUTE PX = PX + 4
               MOVE    WK-KEY2-CNT TO      WK-CNT3
               MOVE    WK-CNT3     TO      PR-LINE (J) (PX:11)

               MOVE    ZERO        TO      WK-KEY2-CNT
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                       MOVE    ZERO        TO      TBL01-DT-K2SU (I)
               END-PERFORM

               IF      SW-KAIGYO-BR =  "0"
                   ADD     1           TO      J
               ELSE
                   ADD     2           TO      J
               END-IF
               MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX

           END-IF
           .
       S310-EX.
           EXIT.

      *    *** KEY1ブレイク
       S320-10.

      *    *** PIN1 ZERO件の時、ブレイク件数表示しない
           IF      KEY-OLD     =     LOW-VALUE
                   CONTINUE
           ELSE

               IF      J2        =       1
                   IF      SW-KAIGYO-BR =  "1" AND
                           SW-KAIGYO    =  "1"
                       ADD     1       TO      J
                   END-IF
               END-IF

      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX

               MOVE    WK-PN1      TO       PX

               MOVE    WK-KEY1-MID     TO  PR-LINE (J) (PX:10)
               COMPUTE PX = PX + 11

               MOVE    KEY-OKEY1 (1:L1) TO PR-LINE (J) (PX:L1)

               COMPUTE PX = PX + L1 + 1

               IF      L2  NOT = ZERO
                   COMPUTE PX = PX + L2 + 1
               END-IF

               IF      L3  NOT = ZERO
                   COMPUTE PX = PX + L3 + 1
               END-IF

               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                 IF    ( TBL01-DT-TYPE (I) = "ZD" OR "Z1" OR "Z2" OR
                                             "Z3" OR
                                             "PD" OR "P1" OR "P2" OR
                                             "P3" ) AND 
                         TBL01-DT-SUM (I) =  "Y"

                       MOVE    TBL01-DT-TIT(I)  TO
                               PR-LINE (J) (PX:TBL01-DT-TITL(I))

                       COMPUTE PX = PX + TBL01-DT-TITL(I) + 1

                       MOVE    TBL01-DT-K1SU(I) TO WK-ZSU
                       MOVE    "Y"         TO      SW-KEY-BR

                       MOVE   PX    TO    P5
                       EVALUATE TRUE
                           WHEN TBL01-DT-TYPE(I) = "ZD" OR "PD"
      *    *** TYPE=ZD,PD
                                PERFORM S220-10     THRU    S220-EX
                           WHEN TBL01-DT-TYPE (I) = "Z1" OR "P1"
      *    *** TYPE=Z1,P1
                                PERFORM S230-10     THRU    S230-EX
                           WHEN TBL01-DT-TYPE (I) = "Z2" OR "P2"
      *    *** TYPE=Z2,P2
                                PERFORM S240-10     THRU    S240-EX
                           WHEN TBL01-DT-TYPE (I) = "Z3" OR "P3"
      *    *** TYPE=Z3,P3
                                PERFORM S250-10     THRU    S250-EX
                       END-EVALUATE

                       COMPUTE PX = PX + HL + 1

                  END-IF
               END-PERFORM

               MOVE    WK-KENSU    TO      PR-LINE (J) (PX:04)
               COMPUTE PX = PX + 4
               MOVE    WK-KEY1-CNT TO      WK-CNT3
               MOVE    WK-CNT3     TO      PR-LINE (J) (PX:11)

               MOVE    ZERO        TO      WK-KEY1-CNT
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > WK-TBL01-MAX 
                       MOVE    ZERO        TO      TBL01-DT-K1SU (I)
               END-PERFORM

               IF      KEY-NEW =       HIGH-VALUE
                       IF      SW-KAIGYO-BR =  "0"
                               ADD     1       TO      J
                       ELSE
                               ADD     2       TO      J
                       END-IF
               ELSE
                       IF      SW-KAIGYO-BR =  "0"
                               ADD     1       TO      J
                       ELSE
                           IF      SW-KAIGYO =  "1"
                               ADD     3       TO      J
                           ELSE
                               ADD     2       TO      J
                           END-IF
                       END-IF
               END-IF
               MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
               PERFORM S280-10     THRU    S280-EX
           END-IF
           .
       S320-EX.
           EXIT.

      *    *** AT END ブレイク
       S330-10.

           MOVE    WK-PN1      TO       PX
           MOVE    WK-KEY4-MID     TO  PR-LINE (J) (PX:10)
           COMPUTE PX = PX + 11

      *    *** L1,L2,L3 はKEY1,2,3 の長さ、11は総合計＋１
      *             COMPUTE PX = PX + 11 + L1 + 1 + L2 + 1 + L3
      *             COMPUTE PX = PX + 11 + L1 + 1 + L2 + 1 + L3 + 1
           IF      L1     NOT = ZERO
                   COMPUTE PX = PX + L1 + 1
           END-IF
           IF      L2     NOT = ZERO
                   COMPUTE PX = PX + L2 + 1
           END-IF
           IF      L3     NOT = ZERO
                   COMPUTE PX = PX + L3 + 1
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > WK-TBL01-MAX 
                   IF    ( TBL01-DT-TYPE (I) = "ZD" OR "Z1" OR "Z2" OR 
                                               "Z3" OR
                                               "PD" OR "P1" OR "P2" OR 
                                               "P3" )  AND 
                           TBL01-DT-SUM (I) =  "Y"

                       MOVE    TBL01-DT-TIT (I)  TO
                               PR-LINE (J) (PX:TBL01-DT-TITL(I))

                       COMPUTE PX = PX + TBL01-DT-TITL(I) + 1

                       MOVE    TBL01-DT-TOSU(I) TO WK-ZSU
                       MOVE    "Y"         TO      SW-KEY-BR

                       MOVE   PX    TO    P5
                       EVALUATE TRUE
                           WHEN TBL01-DT-TYPE (I) = "ZD" OR "PD"
      *    *** TYPE=ZD,PD
                                PERFORM S220-10     THRU    S220-EX
                           WHEN TBL01-DT-TYPE (I) = "Z1" OR "P1"
      *    *** TYPE=Z1,P1
                                PERFORM S230-10     THRU    S230-EX
                           WHEN TBL01-DT-TYPE (I) = "Z2" OR "P2"
      *    *** TYPE=Z2,P2
                                PERFORM S240-10     THRU    S240-EX
                           WHEN TBL01-DT-TYPE (I) = "Z3" OR "P3"
      *    *** TYPE=Z3,P3
                                PERFORM S250-10     THRU    S250-EX
                       END-EVALUATE

                       COMPUTE PX = PX + HL + 1
                   END-IF
           END-PERFORM

           MOVE    WK-KENSU    TO      PR-LINE (J) (PX:04)
           COMPUTE PX = PX + 4
           IF      WK-F-IN-MODE =      "L"
                   MOVE    WK-PIN1-CNT TO      WK-CNT3
           ELSE
                   MOVE    WK-PIN2-CNT TO      WK-CNT3
           END-IF
           MOVE    WK-CNT3     TO      PR-LINE (J) (PX:11)

      *    *** PRINT TBL WRITE
           PERFORM S270-10     THRU    S270-EX
           .
       S330-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F CLOSE ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PRM2-F
           IF      WK-PRM2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM2-F CLOSE ERROR STATUS="
                           WK-PRM2-STATUS
                   STOP    RUN
           END-IF

           IF      WK-F-IN-MODE =      "L"
                   CLOSE   PIN1-F
                   IF      WK-PIN1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIN1-F CLOSE ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           ELSE
                   CLOSE   PIN2-F
                   IF      WK-PIN2-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME
                                   " PIN2-F CLOSE ERROR STATUS="
                                   WK-PIN2-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
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

           MOVE    "CLOSE "    TO      WDE07-ID
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 件数 = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PRM2-CNT TO      WK-PRM2-CNT-E
           DISPLAY WK-PGM-NAME " PRM2 件数 = " WK-PRM2-CNT-E
                   " (" WK-PRM2-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 件数 = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-PIN3-CNT TO      WK-PIN3-CNT-E
           DISPLAY WK-PGM-NAME " PIN3 件数 = " WK-PIN3-CNT-E
                   " (" WK-PIN3-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 頁   = " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
