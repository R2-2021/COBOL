      *    *** 画面テスト
      *    *** TEST21.CBL よりコピー
      *    *** 大前提　SCREEN SECTION で　ＬＩＮＥ，ＣＯＬ指定する
      *    *** ACCEPT,DISPLAY でＬＩＮＥ，ＣＯＬ指定するとき、WORK-AREA
      *    *** 項目でないと、指定した場所に表示されない

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST40.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
      *    *** この指定でCOB-CRT-STATUSが定義しなくても、使用可
      *    *** PF01-PF24,SHIFT,CTRL,ALTも可 1001 - 1064
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
      *     CRT STATUS IS COB-CRT-STATUS. 
           CURRENCY SIGN IS "\".
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** 未使用
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** 未使用
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(136).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST40  ".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST40.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST40.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-SHORI-CNT    BINARY-LONG SYNC VALUE 10000000.
           03  WK-CTR-STATUS   PIC  9(004) VALUE 0.
           03  WK-BINARY       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BI-SHORT     BINARY-SHORT SYNC VALUE 0.
           03  WK-BI-DOUBLE    BINARY-DOUBLE SYNC VALUE 0.
           03  WK-PACK         PIC S9(009) VALUE 0 PACKED-DECIMAL.
           03  WK-UNPACK       PIC S9(009) VALUE 0.
           03  WK-BINARY2      BINARY-LONG SYNC VALUE 12345.
           03  WK-BI-SHORT2    BINARY-SHORT SYNC VALUE 12345.
           03  WK-BI-DOUBLE2   BINARY-DOUBLE SYNC VALUE 12345.
           03  WK-PACK2        PIC S9(009) VALUE 12345 PACKED-DECIMAL.
           03  WK-UNPACK2      PIC S9(009) VALUE 12345.
           03  WK-PACK-V       PIC S9(7)V99 VALUE 0 PACKED-DECIMAL.
           03  WK-UNPACK-V     PIC S9(7)V99 VALUE 0.
           03  WK-SPACE80
             05  FILLER        PIC  X(080) VALUE SPACE.
           03  WK-SPACE2.
             05  FILLER        PIC  X(010) VALUE "ABCDE12345".
             05  FILLER        PIC  X(070) VALUE SPACE.
           03  WK-RAN          PIC  S9V9(9) VALUE 0.
           03  WK-VALUE-200    PIC  X(200) VALUE ALL "ABC".
           03  WK-SPACE        PIC  X(001) VALUE SPACE.
           03  WK-SPACE-100    PIC  X(100) VALUE SPACE.
      *    *** 0.5 SEC = 500,000,000 NANO SEC
           03  WK-NANOSEC      PIC  9(011) VALUE 500000000.
           03  WK-TESTNO-OLD   PIC  9(002) VALUE 0.
           03  WK-TESTNO       PIC  9(002) VALUE 0.
           03  WK-ITEM1        PIC  9(002) VALUE 0.
           03  WK-ITEM2        PIC  9(002) VALUE 0.
           03  WK-ITEM3        PIC  X(010) VALUE SPACE.
           03  WK-ITEM4        PIC  X(010) VALUE SPACE.
           03  WK-PI           PIC  9V9(5) VALUE 0.
           03  WK-DEG          PIC  9(003) VALUE 0.
           03  WK-DEG2         PIC  ZZ9    VALUE ZERO.
           03  WK-RAD          PIC  9V9(3) VALUE 0.
           03  WK-RAD2         PIC  9.9(3) VALUE ZERO.
           03  WK-LINE         PIC  9(002) VALUE ZERO.
           03  WK-WEEK.
             05  WK-SUN        PIC  X(002) VALUE SPACE.
             05  WK-MON        PIC  X(002) VALUE SPACE.
             05  WK-TUE        PIC  X(002) VALUE SPACE.
             05  WK-WED        PIC  X(002) VALUE SPACE.
             05  WK-THU        PIC  X(002) VALUE SPACE.
             05  WK-FRI        PIC  X(002) VALUE SPACE.
             05  WK-SAT        PIC  X(002) VALUE SPACE.
           03  WK-SIN3         PIC  9.9(3) VALUE ZERO.
           03  WK-KANMA        PIC  X(001) VALUE ",".
           03  WK-ALPHA        PIC  X(026) VALUE
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           03  WK-ALPHA2       REDEFINES WK-ALPHA
             05  WK-ALPHA3     OCCURS  26
                               PIC  X(001).
           03  WK-YYYY         PIC  9(004) VALUE ZERO.
           03  WK-YYYYX        PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  9(002) VALUE ZERO.
           03  WK-MMX          PIC  X(002) VALUE SPACE.
           03  WK-AMARI-400    PIC  9(004) VALUE ZERO.
           03  WK-AMARI-100    PIC  9(004) VALUE ZERO.
           03  WK-AMARI-4      PIC  9(004) VALUE ZERO.
           03  WK-DD-X.
             05  WK-DD         OCCURS 7
                               PIC  X(002) VALUE ZERO.
           03  WK-DD2          PIC  9(002) VALUE ZERO.
           03  WK-ERR-COM      PIC  X(080) VALUE SPACE.

           03  WK-SUXX         PIC  9(012) VALUE ZERO.
           03  WK-SUXX12       PIC  X(012) VALUE SPACE.
           03  WK-SUXXX        PIC  X(004) VALUE SPACE.
           03  WK-SUMM         PIC  9(002) VALUE ZERO.
           03  WK-SUDD         PIC  9(002) VALUE ZERO.
           03  WK-CHK-SU       PIC  ZZ,ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-CNT-SU       PIC  ZZ9    VALUE ZERO.
           03  WK-ACT-NUM      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HIT-EOF      PIC  X(001) VALUE LOW-VALUE.
           03  WK-MOJI         PIC  X(001) VALUE SPACE.
      *
           03  WK-CNS-1        BINARY-DOUBLE SYNC VALUE ZERO.
           03  WK-CHK-NUM      BINARY-DOUBLE SYNC VALUE +8589934592.
           03  WK-LO-NUM       BINARY-DOUBLE SYNC VALUE +1.
           03  WK-HI-NUM       BINARY-DOUBLE SYNC VALUE +17179869184.
           03  WK-CNT          BINARY-LONG SYNC VALUE ZERO.
           03  WK-DSP-NUM      PIC Z,ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-SU3          BINARY-LONG SYNC VALUE ZERO.
           03  WK-SU4-X.
      *       05  WK-SU4-9      PIC  9(018) VALUE 0.
      *       05  WK-SU4        REDEFINES WK-SU4-9
      *                         OCCURS 9
      *                         PIC  9(002).
             05  WK-SU4        OCCURS 9 BINARY-LONG.
           03  WK-KETA         BINARY-LONG SYNC VALUE ZERO.
           03  WK-SYOKI        BINARY-LONG SYNC VALUE ZERO.
      *     03  WK-KETA         PIC  9(001) VALUE ZERO.
      *     03  WK-SYOKI        PIC  9(004) VALUE ZERO.
           03  WK-IDX-END      BINARY-LONG SYNC VALUE ZERO.
           03  WK-PU-SU1       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU2       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU3       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU4       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU5       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU6       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU7       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU8       PIC Z,ZZZ   VALUE ZERO.
           03  WK-PU-SU9       PIC Z,ZZZ   VALUE ZERO.

           03  WK-DATA1        PIC  X(005) VALUE SPACE.
           03  WK-DATA2        PIC S9(3)V99 VALUE ZERO.
           03  WK-DATA2-R      REDEFINES WK-DATA2.
             05  WK-DATA2-R1   PIC  X(005).
           03  WK-DATA3        PIC S9(3)V99 VALUE ZERO.
           03  WK-DATA3-R      REDEFINES WK-DATA3.
             05  WK-DATA3-R1   PIC  X(005).
           03  WK-DATA4        PIC  X(005) VALUE ZERO.
      *    *** 画面上ゼロ＝＞ＳＰＡＣＥ変換しない
      *    *** 貨幣記号も出ない
      *    *** ピリオドも編集されない
      *    *** 
           03  WK-DATA5        PIC  -ZZZ,ZZ9.99 VALUE ZERO.
           03  WK-DATA5-R      REDEFINES WK-DATA5.
             05  WK-DATA5-R1   PIC  X(011).
           03  WK-DATA6        PIC  -\\\,\\\.99 VALUE ZERO.
           03  WK-DATA6-R      REDEFINES WK-DATA6.
             05  WK-DATA6-R1   PIC  X(011).
           03  WK-PIN1-I1      PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I2      PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I3      PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I4-R    PIC  X(1600) VALUE SPACE.
           03  WK-PIN1-I4.
             05  WK-PIN1-I4-01 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-02 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-03 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-04 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-05 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-06 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-07 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-08 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-09 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-10 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-11 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-12 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-13 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-14 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-15 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-16 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-17 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-18 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-19 PIC  X(080) VALUE SPACE.
             05  WK-PIN1-I4-20 PIC  X(080) VALUE SPACE.
           03  WK-PIN1-I5      PIC  X(080) VALUE SPACE.

           03  WK-KEI1         PIC  X(002) VALUE "─".
           03  WK-KEI2         PIC  X(002) VALUE "│".
           03  WK-KEI3         PIC  X(002) VALUE "┌".
           03  WK-KEI4         PIC  X(002) VALUE "┐".
           03  WK-KEI5         PIC  X(002) VALUE "┘".
           03  WK-KEI6         PIC  X(002) VALUE "└".
           03  WK-KEI7         PIC  X(002) VALUE "├".
           03  WK-KEI8         PIC  X(002) VALUE "┬".
           03  WK-KEI9         PIC  X(002) VALUE "┤".
           03  WK-KEI10        PIC  X(002) VALUE "┴".
           03  WK-KEI11        PIC  X(002) VALUE "┼".

           03  WK-HAI1.
             05  FILLER        PIC  X(001) VALUE "|"
             05  FILLER        PIC  X(059) VALUE ALL "-"
             05  FILLER        PIC  X(001) VALUE "|"
           03  WK-HAI2.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(012) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(012) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
           03  WK-HAI3.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(006) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(012) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(012) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"

      *    *** HAI4,5,6　はカレンダー用
           03  WK-HAI4.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
           03  WK-HAI5.
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
           03  WK-HAI6.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"

      *    *** HAI7,8,9　はハイローゲーム用
           03  WK-HAI7.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
           03  WK-HAI8.
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
           03  WK-HAI9.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(014) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"

      *    *** HAI10,11,12　は魔方陣用
           03  WK-HAI10.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
           03  WK-HAI10S.
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "┌"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┬"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┐"
           03  WK-HAI11.
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
           03  WK-HAI11S.
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
             05  FILLER        PIC  X(004) VALUE ALL "　"
             05  FILLER        PIC  X(002) VALUE "│"
           03  WK-HAI12.
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
           03  WK-HAI12S.
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "├"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┼"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┤"
           03  WK-HAI13.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"
           03  WK-HAI13S.
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "└"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┴"
             05  FILLER        PIC  X(004) VALUE ALL "─"
             05  FILLER        PIC  X(002) VALUE "┘"

           03  WK-TIT1.
             05  FILLER        PIC  X(020) VALUE SPACE.
             05  FILLER        PIC  N(014) VALUE
                 "＊＊＊　初期メニュー　＊＊＊".
             05  FILLER        PIC  X(010) VALUE SPACE.
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

           03  WK-MID1         PIC  X(012) VALUE
               "メニュー項目".
           03  WK-MID2.
             05  FILLER        PIC  X(022) VALUE
               "│    │              "
             05  FILLER        PIC  X(022) VALUE
               "│    │              "
             05  FILLER        PIC  X(024) VALUE
               "│    │              │"
           03  WK-HAI          PIC  X(080) VALUE
               ALL "-".
           03  WK-MEI1.
             05  WK-MEI1-I00   PIC  X(080) VALUE
               "００．初期メニュー".
             05  WK-MEI1-I01   PIC  X(080) VALUE
               "０１．魔方陣（縦、横、斜め　加えて同じ)".
             05  WK-MEI1-I02   PIC  X(080) VALUE
               "０２．魔方陣（縦、横、斜め　真中引いて同じ)".
             05  WK-MEI1-I03   PIC  X(080) VALUE
               "０３．魔方陣（縦、横、斜め　任意数字加えて同じ)".
             05  WK-MEI1-I04    PIC  X(080) VALUE
               "０４．魔方陣（縦、横、斜め　任意数字真中引いて同じ)".
             05  WK-MEI1-I05   PIC  X(080) VALUE
               "０５．花火１".
             05  WK-MEI1-I06   PIC  X(080) VALUE
               "０６．花火２".
             05  WK-MEI1-I07   PIC  X(080) VALUE
               "０７．花火３".
             05  WK-MEI1-I08   PIC  X(080) VALUE
               "０８．誕生日当てゲーム".
             05  WK-MEI1-I09   PIC  X(080) VALUE
               "０９．".
             05  WK-MEI1-I10   PIC  X(080) VALUE
               "１０．カレンダー表示".
             05  WK-MEI1-I11   PIC  X(080) VALUE
               "１１．ハイローゲーム".
             05  WK-MEI1-I99   PIC  X(080) VALUE
               "９９．終了".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

      *    *** CALL "CBL_OC_KEISEN" 用
      *    *** 罫線表示
           03  KEI-CMD         PIC  9(002) VALUE 1.
           03  KEI-LINE        PIC  9(002) VALUE 10.
           03  KEI-COL         PIC  9(002) VALUE 10.
           03  KEI-LEN1        PIC  9(002) VALUE 6.
           03  KEI-LEN2        PIC  9(002) VALUE 1.
           03  KEI-COLOR       PIC  9(002) VALUE 0.
           03  KEI-PRN         PIC  9(002) VALUE 0.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 40
                               PIC  X(136) VALUE SPACE.
       
       01  INDEX-AREA.
           03  B               BINARY-LONG SYNC VALUE ZERO.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C2              BINARY-LONG SYNC VALUE ZERO.
           03  C3              BINARY-LONG SYNC VALUE ZERO.
           03  C4              BINARY-LONG SYNC VALUE ZERO.
           03  CL              BINARY-LONG SYNC VALUE ZERO.
           03  CL2             BINARY-LONG SYNC VALUE ZERO.
           03  CL3             BINARY-LONG SYNC VALUE ZERO.
           03  F               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I6              BINARY-LONG SYNC VALUE ZERO.
           03  I7              BINARY-LONG SYNC VALUE ZERO.
           03  I8              BINARY-LONG SYNC VALUE ZERO.
           03  I9              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE 1.
           03  P2              BINARY-LONG SYNC VALUE 3.
           03  P3              BINARY-LONG SYNC VALUE 4.
           03  P4              BINARY-LONG SYNC VALUE 5.
           03  P5              BINARY-LONG SYNC VALUE 6.
           03  R               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-DISPLAY      PIC  X(001) VALUE ZERO.
           03  SW-ERROR        PIC  X(001) VALUE ZERO.
           03  SW-AMP          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-L            BINARY-LONG SYNC VALUE ZERO.
           03  SV-K            BINARY-LONG SYNC VALUE ZERO.

       01  COLORS-AREA.
           03  BLACK           PIC  9(001) VALUE 0.
           03  YELLOW          PIC  9(001) VALUE 1.
           03  GREEN           PIC  9(001) VALUE 2.
           03  CYAN            PIC  9(001) VALUE 3.
           03  RED             PIC  9(001) VALUE 4.
           03  MAGENTA         PIC  9(001) VALUE 5.
           03  BROWN           PIC  9(001) VALUE 6.
      *    *** HIGHLIGHT時は黄色
           03  YELLOWH         PIC  9(001) VALUE 6.
           03  GREY            PIC  9(001) VALUE 7.
      *    *** HIGHLIGHT時は白色
           03  WHITE           PIC  9(001) VALUE 7.
           03  COLORS-NAME.
             05  COLOR-0L      PIC  X(010) VALUE "BLACK     ".
             05  COLOR-0H      PIC  X(010) VALUE "DARK GREY ".
             05  COLOR-1       PIC  X(010) VALUE "YELLOW    ".
             05  COLOR-2       PIC  X(010) VALUE "GREEN     ".
             05  COLOR-3       PIC  X(010) VALUE "CYAN      ".
             05  COLOR-4       PIC  X(010) VALUE "RED       ".
             05  COLOR-5       PIC  X(010) VALUE "MAGENTA   ".
             05  COLOR-6L      PIC  X(010) VALUE "BROWN     ".
             05  COLOR-6H      PIC  X(010) VALUE "YELLOW    ".
             05  COLOR-7L      PIC  X(010) VALUE "LIGHT GREY".
             05  COLOR-7H      PIC  X(010) VALUE "WHITE     ".

       SCREEN                  SECTION.
       01  SCREEN-AREA.

           03  SCR00-AREA.
             05  LINE 25 COL 8 VALUE "メニューＮｏ"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
      *          HIGHLIGHT
      *          OVERLINE
      *          UNDERLINE
                 .
      *    *** TO は入力項目として定義する
             05  COL PLUS 2 TO WK-TESTNO
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *          HIGHLIGHT
      *          OVERLINE
      *          UNDERLINE
                 AUTO-SKIP
                 BLANK WHEN ZERO
                 PIC 9(002).
      *
      * 不明          JUST
      * 入力＊        SECURE
      * 不明          FULL
      * 不明           PROMPT
      * 反転           REVERSE-VIDEO
      * 画面初期化     ERASE EOL
      * 指定出来ない   EOS
             05  COL PLUS 2 USING WK-ITEM1
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           HIGHLIGHT
      *           UNDERLINE
                 AUTO-SKIP
                 BLANK WHEN ZERO
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM2
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           HIGHLIGHT
      *           UNDERLINE
                 AUTO-SKIP
                 BLANK WHEN ZERO
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM3
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           HIGHLIGHT
      *           UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

             05  COL PLUS 2 USING WK-ITEM4
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

           03  SCR01-AREA
      *    *** 集団項目で色指定出来る
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR01-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "＊＊＊　魔方陣　＊＊＊".

           03  SCR01-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "数字桁　入力".

      *    *** USINGは入力、出力両方に使える　最初に下線が表示されてる
             05  USING WK-KETA
                 LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                 AUTO
                       PIC  9(001).

             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "初期値　入力".

             05  USING WK-SYOKI
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                       PIC  9(004).

             05  FROM WK-ERR-COM
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC X(050).

           03  SCR01-3-AREA.
             05  BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

      *    *** FROM は出力項目として定義する
               07  FROM WK-PU-SU1
                   LINE L COL C
                   PIC  ZZZ9.
               07  FROM WK-PU-SU2
                   COL PLUS 2
                   PIC  ZZZ9.
               07  FROM WK-PU-SU3
                   COL PLUS 2
                   PIC  ZZZ9.

      *    *** 花火用
           03  SCR05-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

           03  SCR05-1-AREA.
             05  LINE 12 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "X".
             05  LINE 13 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "X   X".
             05  LINE 14 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "X".

           03  SCR05-2-AREA.
             05  LINE 11 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "O".
             05  LINE 12 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "O   O".
             05  LINE 13 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "O     O".
             05  LINE 14 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "O   O".
             05  LINE 15 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "O".

           03  SCR05-3-AREA.
             05  LINE 10 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H".
             05  LINE 11 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H   H".
             05  LINE 12 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H     H".
             05  LINE 13 COL 29
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H       H".
             05  LINE 14 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H     H".
             05  LINE 15 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H   H".
             05  LINE 16 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H".

           03  SCR05-4-AREA.
             05  LINE 9 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H".
             05  LINE 10 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H   H".
             05  LINE 11 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H     H".
             05  LINE 12 COL 29
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H       H".
             05  LINE 13 COL 28
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H         H".
             05  LINE 14 COL 29
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H       H".
             05  LINE 15 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H     H".
             05  LINE 16 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H   H".
             05  LINE 17 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "H".

           03  SCR05-5-AREA.
             05  LINE 8 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q".
             05  LINE 9 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q   Q".
             05  LINE 10 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q     Q".
             05  LINE 11 COL 29
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q       Q".
             05  LINE 12 COL 28
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q         Q".
             05  LINE 13 COL 27
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q           Q".
             05  LINE 14 COL 28
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q         Q".
             05  LINE 15 COL 29
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q       Q".
             05  LINE 16 COL 30
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q     Q".
             05  LINE 17 COL 31
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q   Q".
             05  LINE 18 COL 33
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "Q".

           03  SCR07-1-AREA.
             05  LINE L COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "  X".
             05  LINE PLUS 1 COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "X   X".
             05  LINE PLUS 1 COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 VALUE "  X".

           03  SCR07-2-AREA.
             05  LINE L COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 LOWLIGHT
                 VALUE "  X".
             05  LINE PLUS 1 COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 LOWLIGHT
                 VALUE "X   X".
             05  LINE PLUS 1 COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 LOWLIGHT
                 VALUE "  X".

      *    *** EOS 指定はエラーになる
           03  SCR07-3-AREA.
             05  ERASE EOL LINE L COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 .
             05  ERASE EOL LINE PLUS 1 COL CL
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 .
             05  LINE PLUS 1 COL CL ERASE EOL 
                 BACKGROUND-COLOR BLACK
                 FOREGROUND-COLOR BLACK
                 .

      *    *** 誕生日当てゲーム用
           03  SCR08-AREA
      *    *** 集団項目で色指定出来る
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR08-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "＊＊＊　誕生日当てゲーム　＊＊＊".

           03  SCR08-2-AREA.
             05  LINE 3 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "月に４を掛けて、９を加えて、２５を掛けて、".
             05  LINE 3
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "日を加えた、数字入力してください".
             05  USING WK-SUXXX
                 LINE 3 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
      *           AUTO
                       PIC  X(004).
             05  LINE 5 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "計算式：（月＊４＋９）*２５＋日".

      *    *** PIC X(NNN) 記入しないと表示されない
             05  FROM WK-ERR-COM
                 LINE 5 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC X(048).

           03  SCR08-3-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

             05  LINE 7 COL 1
                 VALUE "誕生日は".

             05  FROM WK-SUMM
                 LINE 7 COL PLUS 2
                 UNDERLINE
                       PIC  Z9.
             05  LINE 7 COL PLUS 1
                 VALUE "月".

             05  FROM WK-SUDD
                 LINE 7 COL PLUS 2
                 UNDERLINE
                       PIC  Z9.
             05  LINE 7 COL PLUS 1
                 VALUE "日ですね".

           03  SCR08-4-AREA.
             05  FROM WK-ERR-COM
                 LINE 11 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC X(080).

           03  SCR08-5-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

             05  LINE 7 COL 23
                 VALUE "　？？".

      *    *** カレンダー用
           03  SCR10-AREA
      *    *** 集団項目で色指定出来る
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
                 
             05  LINE WK-LINE COL 8
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
                 VALUE "│".
      *    *** FROM は出力項目として定義する
             05  FROM WK-SUN
      *           LINE WK-LINE COL PLUS 1
      *           LINE WK-LINE COL PLUS 1
                 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC  X(002).
      *       05  LINE WK-LINE
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
             05  VALUE "│"
                 COL PLUS 1.
             05  FROM WK-MON
                 LINE WK-LINE 
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".
             05  FROM WK-TUE
                 LINE WK-LINE 
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".
             05  FROM WK-WED
                 LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".
             05  FROM WK-THU
                 LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".
             05  FROM WK-FRI
                 LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".
             05  FROM WK-SAT
                 LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 COL PLUS 1
                 PIC  X(002).
             05  LINE WK-LINE
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 COL PLUS 1
                 VALUE "│".

           03  SCR10-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "＊＊＊　カレンダー　＊＊＊".

           03  SCR10-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "年　入力".

      *    *** USINGは入力、出力両方に使える　最初に下線が表示されてる
             05  USING WK-YYYYX
                 LINE 2 COL  12
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
      *           AUTO
                       PIC  X(004).
      *       05  LINE 2 COL 18
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
      *           VALUE "1582年10月15日(グレゴリオ暦)以降、算出".

           03  SCR10-3-AREA.

             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "月　入力".

             05  USING WK-MMX
                 LINE 3 COL  12
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
      *           AUTO
                       PIC  X(002).

      *    *** PIC X(NNN) 記入しないと表示されない
             05  FROM WK-ERR-COM
                 LINE 3 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC X(060).

           03  SCR10-4-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

             05  FROM WDW-DATE2-YYYY
                 LINE 4 COL 40
                       PIC  ZZZ9.
             05  LINE 4 COL 44
                 VALUE "年".

             05  FROM WDW-DATE2-MM
                 LINE 4 COL 47
                       PIC  Z9.

             05  LINE 4 COL 49
                 VALUE "月".

           03  SCR10-5-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT.

             05  FROM WK-DD(1)
                 LINE L COL 11
                       PIC  X(002).
             05  FROM WK-DD(2)
                       COL PLUS 4
                       PIC  X(002).
             05  FROM WK-DD(3)
                       COL PLUS 4
                       PIC  X(002).
             05  FROM WK-DD(4)
                       COL PLUS 4
                       PIC  X(002).
             05  FROM WK-DD(5)
                       COL PLUS 4
                       PIC  X(002).
             05  FROM WK-DD(6)
                       COL PLUS 4
                       PIC  X(002).
             05  FROM WK-DD(7)
                       COL PLUS 4
                       PIC  X(002).

           03  SCR10-6-AREA.
             05  LINE 22 COL 8
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "ＰＦ０７：前月　ＰＦ０８：翌月".
             05  LINE 22 COL 40
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "ＰＦ０９：前年　ＰＦ１０：翌年".

      *    *** ハイローゲーム用
           03  SCR11-AREA
      *    *** 集団項目で色指定出来る
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
                 
             05  LINE L COL 1
                 VALUE "│".
             05  COL PLUS 4
                 VALUE "│".
             05  COL PLUS 14
                 VALUE "│".
             05  COL PLUS 4
                 VALUE "│".
             05  COL PLUS 14
                 VALUE "│".
             05  COL PLUS 4
                 VALUE "│".
             05  COL PLUS 14
                 VALUE "│".

           03  SCR11-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR CYAN
                 HIGHLIGHT
                 VALUE "＊＊＊　ハイローゲーム　＊＊＊".

           03  SCR11-2-AREA.
             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "数字１０桁以内で入力".

      *    *** USINGは入力、出力両方に使える　最初に下線が表示されてる
             05  USING WK-SUXX12
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  X(012).
    
      *    *** PIC X(NNN) 記入しないと表示されない
             05  FROM WK-ERR-COM
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 PIC X(040).

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** ００．初期メニュー
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX
      *
           PERFORM UNTIL WK-TESTNO = 99
                   EVALUATE TRUE
                       WHEN WK-TESTNO = 0
      *    *** ００．初期メニュー
                            PERFORM S020-10     THRU    S020-EX
                       WHEN WK-TESTNO = 1
      *    *** ０１．魔方陣（縦、横、斜め　加えて同じ)
                            PERFORM S210-10     THRU    S210-EX
                       WHEN WK-TESTNO = 2
      *    *** ０２．魔方陣（縦、横、斜め　真中引いて同じ)
                            PERFORM S220-10     THRU    S220-EX
                       WHEN WK-TESTNO = 3
      *    *** ０３．魔方陣（縦、横、斜め　任意数字加えて同じ)
                            PERFORM S230-10     THRU    S230-EX
                       WHEN WK-TESTNO = 4
      *    *** ０４．魔方陣（縦、横、斜め　任意数字真中引いて同じ)
                            PERFORM S240-10     THRU    S240-EX
                       WHEN WK-TESTNO = 5
      *    *** ０５．花火１
                            PERFORM S250-10     THRU    S250-EX
                       WHEN WK-TESTNO = 6
      *    *** ０６．花火２
                            PERFORM S260-10     THRU    S260-EX
                       WHEN WK-TESTNO = 7
      *    *** ０７．花火３
                            PERFORM S270-10     THRU    S270-EX
                       WHEN WK-TESTNO = 8
      *    *** ０８．誕生日当てゲーム
                            PERFORM S280-10     THRU    S280-EX
                       WHEN WK-TESTNO = 9 AND 
                            WK-PIN1-EOF NOT = HIGH-VALUE
      *    *** ０９．
                            PERFORM S290-10     THRU    S290-EX
      *    *** READ PIN1
                            PERFORM S100-10     THRU    S100-EX
                       WHEN WK-TESTNO = 10
      *    *** １０．カレンダー表示
                            PERFORM S300-10     THRU    S300-EX
                       WHEN WK-TESTNO = 11
      *    *** １１．ハイローゲーム
                            PERFORM S310-10     THRU    S310-EX
                   END-EVALUATE
      *    *** 初期メニュー表示　入力待ち
                   PERFORM S030-10    THRU    S030-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.

      *    DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA
           .
       S010-EX.
           EXIT.

      *    *** ００．初期メニュー
       S020-10.
           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       25
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           DISPLAY WK-TIT1
                   AT LINE 1 COL 1
      *    *** COLOR 指定しないと、背景：黒、印字：白になる
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MID1
                   AT LINE 2 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-HAI
                   AT LINE 3 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I00
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I01
                   AT LINE 5 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I02
                   AT LINE 6 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I03
                   AT LINE 7 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I04
                   AT LINE 8 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I05
                   AT LINE 9 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I06
                   AT LINE 10 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I07
                   AT LINE 11 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I08
                   AT LINE 12 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I09
                   AT LINE 13 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I10
                   AT LINE 14 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I11
                   AT LINE 15 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           DISPLAY WK-MEI1-I99
                   AT LINE 23 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           MOVE    WK-ITEM3    TO      WK-ITEM4
           DISPLAY SCR00-AREA

           .
       S020-EX.
           EXIT.

      *    *** 初期メニュー表示　入力待ち
       S030-10.

           MOVE    WK-TESTNO   TO      WK-TESTNO-OLD
           ACCEPT  SCR00-AREA

           IF      WK-TESTNO   NOT =   WK-TESTNO-OLD
      *    *** カレンダー　一度メニュー変更したら、リセットのため、
      *    *** WK-YYYY,WK-MM ＺＥＲＯにする
                   MOVE    ZERO        TO      WK-YYYY
                   MOVE    ZERO        TO      WK-MM
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** READ PIN1
       S100-10.

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

      *    *** 256バイトまでしか入らない
           UNSTRING PIN1-REC
                    DELIMITED BY ","
                    INTO
                    WK-PIN1-I1
                    WK-PIN1-I2
                    WK-PIN1-I3
                    WK-PIN1-I4-R  COUNT L
                    WK-PIN1-I5
           
           MOVE    SPACE       TO      WK-PIN1-I4
           MOVE    1           TO      J 
           MOVE    ZERO        TO      J2 K
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > L
      *    *** ANKのとき、
      *             IF  ( WK-PIN1-I4-R (I:1) >= X"20" AND 
      *                   WK-PIN1-I4-R (I:1) <= X"7E" )   OR
      *                 ( WK-PIN1-I4-R (I:1) >= X"A1" AND 
      *                   WK-PIN1-I4-R (I:1) <= X"DF" )
               IF ( WK-PIN1-I4-R (I:2) >= X"8140" AND 
                    WK-PIN1-I4-R (I:2) <= X"9FFC" )   OR
                  ( WK-PIN1-I4-R (I:2) >= X"E040" AND 
                    WK-PIN1-I4-R (I:2) <= X"EAA4" )
                       MOVE    WK-PIN1-I4-R (I:2) TO WK-PIN1-I4 (J:2)
                       ADD     2           TO      K
      *    *** J 1,3,5...
      *    *** J 81.83,85...
                       ADD     1           TO      I
                       ADD     2           TO      J
               ELSE
                       MOVE   WK-PIN1-I4-R (I:1) TO  WK-PIN1-I4 (J:1)
                       ADD     1           TO      K
      *    *** J 1,2,3...
      *    *** J 81,82,83...
      *    *** J 161.162,163...
                       ADD     1           TO      J
               END-IF

               IF      K           >       73
                       ADD     80          TO      J2
      *    *** J 81,161...
                       ADD     J2 1        GIVING  J
                       MOVE    ZERO        TO      K
               END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN
           END-PERFORM
           GO  TO  S100-EX

      *    *** 下記の方法では、無理だった　最初の漢字判定が、一文字目の
      *    *** ２バイト目と次の１バイト目で漢字で判定されることがあるため
           MOVE    1520        TO      J
           MOVE    81          TO      K
           PERFORM VARYING I FROM 77 BY 80
                   UNTIL I > 880
      *    *** 77,2 漢字のとき、
               IF ( WK-PIN1-I4 (I:2) >= X"8140" AND 
                    WK-PIN1-I4 (I:2) <= X"9FFC" )   OR
                  ( WK-PIN1-I4 (I:2) >= X"E040" AND 
                    WK-PIN1-I4 (I:2) <= X"EAA4" )
                   COMPUTE I2 = I - 2
      *    *** 75,2 も漢字か？
                   IF ( WK-PIN1-I4 (I2:2) >= X"8140" AND 
                        WK-PIN1-I4 (I2:2) <= X"9FFC" )   OR
                      ( WK-PIN1-I4 (I2:2) >= X"E040" AND 
                        WK-PIN1-I4 (I2:2) <= X"EAA4" )
                        MOVE    WK-PIN1-I4 (I2:J) TO WK-PIN1-I4 (K:J)
                        MOVE    SPACE         TO WK-PIN1-I4 (I2:6)
                        COMPUTE K = K + 80
                        COMPUTE J = J - 80
                   ELSE
                     COMPUTE I2 = I - 1
      *    *** 76,1 漢字か？
                     IF ( WK-PIN1-I4 (I2:1) >= X"81" AND 
                          WK-PIN1-I4 (I2:1) <= X"9F" )   OR
                        ( WK-PIN1-I4 (I2:1) >= X"E0" AND 
                          WK-PIN1-I4 (I2:1) <= X"EA" )
                        MOVE    WK-PIN1-I4 (I2:J) TO WK-PIN1-I4 (K:J)
                        MOVE    SPACE         TO WK-PIN1-I4 (I2:5)
                        COMPUTE K = K + 80
                        COMPUTE J = J - 80
      *    *** 7,1 ANK
                     ELSE
                        MOVE    WK-PIN1-I4 (I:J) TO WK-PIN1-I4 (K:J)
                        MOVE    SPACE         TO WK-PIN1-I4 (I:4)
                        COMPUTE K = K + 80
                        COMPUTE J = J - 80
                   END-IF
               ELSE
                   COMPUTE I2 = I - 2
      *    *** 75,2 も漢字か？
                   IF ( WK-PIN1-I4 (I2:2) >= X"8140" AND 
                        WK-PIN1-I4 (I2:2) <= X"9FFC" )   OR
                      ( WK-PIN1-I4 (I2:2) >= X"E040" AND 
                        WK-PIN1-I4 (I2:2) <= X"EAA4" )
                        MOVE    WK-PIN1-I4 (I2:J) TO WK-PIN1-I4 (K:J)
                        MOVE    SPACE         TO WK-PIN1-I4 (I2:6)
                        COMPUTE K = K + 80
                        COMPUTE J = J - 80
                   ELSE
                     COMPUTE I2 = I - 1
      *    *** 76,1 も漢字か？
                     IF ( WK-PIN1-I4 (I2:1) >= X"81" AND 
                          WK-PIN1-I4 (I2:1) <= X"9F" )   OR
                        ( WK-PIN1-I4 (I2:1) >= X"E0" AND 
                          WK-PIN1-I4 (I2:1) <= X"EA" )
                        MOVE    WK-PIN1-I4 (I2:J) TO WK-PIN1-I4 (K:J)
                        MOVE    SPACE         TO WK-PIN1-I4 (I2:5)
                        COMPUTE K = K + 80
                        COMPUTE J = J - 80
                     ELSE
      *    *** 77,1 ANK?
                       MOVE    WK-PIN1-I4 (I:J) TO WK-PIN1-I4 (K:J)
                       MOVE    SPACE         TO WK-PIN1-I4 (I:4)
                       COMPUTE K = K + 80
                       COMPUTE J = J - 80
               END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN

           END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** ０１．魔方陣（縦、横、斜め　加えて同じ)
       S210-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** 罫線、入力項目
           MOVE    1           TO      WK-KETA
           MOVE    1           TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** 順列作成
           PERFORM S212-10     THRU    S212-EX

           .
       S210-EX.
           EXIT.

      *    *** 罫線、入力項目
       S211-10.
      *    *** 魔方陣
           DISPLAY SCR01-1-AREA

      *    *** 罫線

           DISPLAY WK-HAI10
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 5 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12
                   AT LINE 6 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 7 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12
                   AT LINE 8 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 9  COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI13
                   AT LINE 10 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY



           DISPLAY WK-HAI10
                   AT LINE 11 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 12 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12
                   AT LINE 13 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 14 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12
                   AT LINE 15 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11
                   AT LINE 16 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI13
                   AT LINE 17 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY



           DISPLAY WK-HAI10S
                   AT LINE 18 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11S
                   AT LINE 19 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12S
                   AT LINE 20 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11S
                   AT LINE 21 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI12S
                   AT LINE 22 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI11S
                   AT LINE 23 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI13S
                   AT LINE 24 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** 桁、初期値　入力

           IF      WK-TESTNO   =     3 OR 4
               MOVE    ZERO        TO      SW-ERROR
               MOVE    SPACE       TO      WK-ERR-COM
               ACCEPT  SCR01-2-AREA

               PERFORM TEST AFTER 
                   UNTIL SW-ERROR = "0"
                   
      *             IF    ( WK-KETA     IS      NUMERIC AND
                   IF    ( WK-KETA     >=      1       AND
                           WK-KETA     <=      4       )
                       MOVE    ZERO        TO      SW-ERROR
                       MOVE    SPACE       TO      WK-ERR-COM
                       EVALUATE TRUE
                           WHEN WK-KETA = 1
                                IF  WK-SYOKI    =       1
                                    CONTINUE
                                ELSE
                                    MOVE    "1"         TO      SW-ERROR
                                    MOVE   "１桁は初期値1のみで指定"
                                            TO      WK-ERR-COM
                                    MOVE    ZERO        TO      WK-SYOKI
                                END-IF
                           WHEN WK-KETA = 2
                                IF  WK-SYOKI    >=      10 AND
                                    WK-SYOKI    <=      91
                                    CONTINUE
                                ELSE
                                    MOVE    "1"         TO      SW-ERROR
                                    MOVE 
                                    "２桁は初期値10-91の範囲で指定"
                                            TO      WK-ERR-COM
                                    MOVE    ZERO        TO      WK-SYOKI
                                END-IF
                           WHEN WK-KETA = 3
                                IF  WK-SYOKI    >=      100 AND
                                    WK-SYOKI    <=      991
                                    CONTINUE
                                ELSE
                                    MOVE    "1"         TO      SW-ERROR
                                    MOVE
                                   "３桁は初期値100-991の範囲で指定"
                                            TO      WK-ERR-COM
                                    MOVE    ZERO        TO      WK-SYOKI
                                END-IF
                           WHEN WK-KETA = 4
                                IF  WK-SYOKI    >=      1000 AND
                                    WK-SYOKI    <=      9991
                                    CONTINUE
                                ELSE
                                    MOVE    "1"         TO      SW-ERROR
                                    MOVE
                                    "４桁は初期値1000-9991の範囲で指定"
                                           TO      WK-ERR-COM
                                    MOVE    ZERO        TO      WK-SYOKI
                                END-IF
                      END-EVALUATE

                      IF      SW-ERROR    =       "0"
                           MOVE    SPACE   TO      WK-ERR-COM
                           DISPLAY SCR01-2-AREA
                      ELSE
                           DISPLAY SCR01-2-AREA
                           ACCEPT  SCR01-2-AREA
                      END-IF
                      
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE  "桁は数字、１から４の範囲で指定"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WK-SYOKI
                           DISPLAY SCR01-2-AREA
                           ACCEPT  SCR01-2-AREA
                   END-IF
               END-PERFORM
           ELSE
               CONTINUE
           END-IF
           .
       S211-EX.
           EXIT.

      *    *** 順列作成
       S212-10.
      *    *** WK-IDX-END = + 8 にする
           COMPUTE WK-IDX-END = WK-SYOKI + 8
           MOVE    ZERO        TO      J

      *    ***P1
           PERFORM VARYING I1 FROM WK-SYOKI BY 1
                   UNTIL   I1 > WK-IDX-END
             MOVE    I1          TO      WK-SU4(1)
      *    ***P2
             PERFORM VARYING I2 FROM WK-SYOKI BY 1
                     UNTIL   I2 > WK-IDX-END
               MOVE    I2          TO      WK-SU4(2)
      *    ***I1
               IF  WK-SU4(2) = WK-SU4(1)
                 CONTINUE
               ELSE
      *    ***P3
                 PERFORM VARYING I3 FROM WK-SYOKI BY 1
                         UNTIL   I3 > WK-IDX-END
                   MOVE    I3          TO      WK-SU4(3)
      *    ***I2
                   IF  WK-SU4(3) = WK-SU4(1) OR
                       WK-SU4(3) = WK-SU4(2)
                     CONTINUE
                   ELSE
      *    ***P4
                     PERFORM VARYING I4 FROM WK-SYOKI BY 1
                             UNTIL   I4 > WK-IDX-END
                       MOVE    I4          TO      WK-SU4(4)
      *    ***I3
                       IF  WK-SU4(4) = WK-SU4(1) OR
                           WK-SU4(4) = WK-SU4(2) OR
                           WK-SU4(4) = WK-SU4(3)
                         CONTINUE
                       ELSE
      *    ***P5
                         PERFORM VARYING I5 FROM WK-SYOKI BY 1
                                 UNTIL   I5 > WK-IDX-END
                           MOVE    I5          TO      WK-SU4(5)
      *    ***I4
                           IF  WK-SU4(5) = WK-SU4(1) OR
                               WK-SU4(5) = WK-SU4(2) OR
                               WK-SU4(5) = WK-SU4(3) OR
                               WK-SU4(5) = WK-SU4(4)
                             CONTINUE
                           ELSE
      *    ***P6
                             PERFORM VARYING I6 FROM WK-SYOKI BY 1
                                     UNTIL   I6 > WK-IDX-END
                               MOVE    I6          TO      WK-SU4(6)
      *    ***I5
                               IF  WK-SU4(6) = WK-SU4(1) OR
                                   WK-SU4(6) = WK-SU4(2) OR
                                   WK-SU4(6) = WK-SU4(3) OR
                                   WK-SU4(6) = WK-SU4(4) OR
                                   WK-SU4(6) = WK-SU4(5)
                                 CONTINUE
                               ELSE
      *    ***P7
                                 PERFORM VARYING I7 FROM WK-SYOKI BY 1
                                         UNTIL   I7 > WK-IDX-END
                                   MOVE    I7          TO      WK-SU4(7)
      *    ***I6
                                   IF  WK-SU4(7) = WK-SU4(1) OR
                                       WK-SU4(7) = WK-SU4(2) OR
                                       WK-SU4(7) = WK-SU4(3) OR
                                       WK-SU4(7) = WK-SU4(4) OR
                                       WK-SU4(7) = WK-SU4(5) OR
                                       WK-SU4(7) = WK-SU4(6)
                                     CONTINUE
                                   ELSE
      *    ***P8
                                     PERFORM VARYING I8 FROM WK-SYOKI
                                             BY 1 
                                             UNTIL I8 > WK-IDX-END
                                       MOVE    I8          TO
                                               WK-SU4(8)
      *    ***I7
                                       IF  WK-SU4(8) = WK-SU4(1) OR
                                           WK-SU4(8) = WK-SU4(2) OR
                                           WK-SU4(8) = WK-SU4(3) OR
                                           WK-SU4(8) = WK-SU4(4) OR
                                           WK-SU4(8) = WK-SU4(5) OR
                                           WK-SU4(8) = WK-SU4(6) OR
                                           WK-SU4(8) = WK-SU4(7)
                                         CONTINUE
                                       ELSE
      *    ***P9
                                         PERFORM VARYING I9 FROM 
                                                 WK-SYOKI BY 1
                                                 UNTIL   I9 > WK-IDX-END
                                           MOVE    I9          TO
                                                   WK-SU4(9)
      *    ***I8
                                           IF  WK-SU4(9) = WK-SU4(1) OR
                                               WK-SU4(9) = WK-SU4(2) OR
                                               WK-SU4(9) = WK-SU4(3) OR
                                               WK-SU4(9) = WK-SU4(4) OR
                                               WK-SU4(9) = WK-SU4(5) OR
                                               WK-SU4(9) = WK-SU4(6) OR
                                               WK-SU4(9) = WK-SU4(7) OR
                                               WK-SU4(9) = WK-SU4(8)
                                             CONTINUE
                                           ELSE
      *    *** +,-　合計　各列同じか
                                             PERFORM S212-1-10     THRU
                                                     S212-1-EX
      *    *** I8
                                           END-IF
                                         END-PERFORM

      *    *** I7
                                       END-IF
                                     END-PERFORM

      *    *** I6
                                   END-IF
                                 END-PERFORM

      *    *** I5
                               END-IF
                             END-PERFORM

      *    *** I4
                           END-IF
                         END-PERFORM

      *    *** I3
                       END-IF
                     END-PERFORM

      *    *** I2
                   END-IF
                 END-PERFORM
      *    *** I1
               END-IF
             END-PERFORM

           END-PERFORM
           .
       S212-EX.
           EXIT.

      *    *** +,-　合計　各列同じか
       S212-1-10.

           IF      WK-TESTNO   =     1 OR 3
               COMPUTE WK-SU3 = WK-SU4(1) + WK-SU4(2) + WK-SU4(3)
               IF ( WK-SU3 =
                   WK-SU4(4) + WK-SU4(5) + WK-SU4(6) ) AND
                 ( WK-SU3 =
                   WK-SU4(7) + WK-SU4(8) + WK-SU4(9) ) AND

                 ( WK-SU3 =
                   WK-SU4(1) + WK-SU4(4) + WK-SU4(7) ) AND
                 ( WK-SU3 =
                   WK-SU4(2) + WK-SU4(5) + WK-SU4(8) ) AND
                 ( WK-SU3 =
                   WK-SU4(3) + WK-SU4(6) + WK-SU4(9) ) AND

                 ( WK-SU3 =
                   WK-SU4(1) + WK-SU4(5) + WK-SU4(9) ) AND
                 ( WK-SU3 =
                   WK-SU4(3) + WK-SU4(5) + WK-SU4(7) )

      *    *** 同一　各マスに出力
                   PERFORM S212-1-1-10     THRU    S212-1-1-EX
               ELSE
                   CONTINUE
           ELSE
              COMPUTE WK-SU3 = WK-SU4(1) - WK-SU4(2) + WK-SU4(3)
               IF ( WK-SU3 =
                   WK-SU4(4) - WK-SU4(5) + WK-SU4(6) ) AND
                 ( WK-SU3 =
                   WK-SU4(7) - WK-SU4(8) + WK-SU4(9) ) AND

                 ( WK-SU3 =
                   WK-SU4(1) - WK-SU4(4) + WK-SU4(7) ) AND
                 ( WK-SU3 =
                   WK-SU4(2) - WK-SU4(5) + WK-SU4(8) ) AND
                 ( WK-SU3 =
                   WK-SU4(3) - WK-SU4(6) + WK-SU4(9) ) AND

                 ( WK-SU3 =
                   WK-SU4(1) - WK-SU4(5) + WK-SU4(9) ) AND
                 ( WK-SU3 =
                   WK-SU4(3) - WK-SU4(5) + WK-SU4(7) )

      *    *** 同一　各マスに出力
                   PERFORM S212-1-1-10     THRU    S212-1-1-EX
               ELSE
                   CONTINUE
               END-IF

           END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     ADD     1           TO      WFD-SEQ
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-SU4-9

      *     WRITE    POT1-REC    FROM    WK-SU4-9
      *     ADD      1           TO      WK-POT1-CNT
           .
       S212-1-EX.
           EXIT.

      *    *** 同一　各マスに出力
       S212-1-1-10.
      *             DISPLAY " "
      *             DISPLAY WK-SU4(1) " "  WK-SU4(2) " " WK-SU4(3) " "
      *             DISPLAY WK-SU4(4) " "  WK-SU4(5) " " WK-SU4(6) " "
      *             DISPLAY WK-SU4(7) " "  WK-SU4(8) " " WK-SU4(9)

           ADD     1           TO      J

           EVALUATE TRUE
                 WHEN J = 1 
                   MOVE    5           TO      L
                   MOVE    3           TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    7           TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    9           TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 2 
                   MOVE    12          TO      L
                   MOVE    3           TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    14          TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    16          TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 3 
                   MOVE    19          TO      L
                   MOVE    3           TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    21          TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    23          TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 4 
                   MOVE    5           TO      L
                   MOVE    27          TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    7           TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    9           TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 5 
                   MOVE    12          TO      L
                   MOVE    27          TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    14          TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    16          TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 6 
                   MOVE    19          TO      L
                   MOVE    27          TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    21          TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    23          TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 7 
                   MOVE    5           TO      L
                   MOVE    51          TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    7           TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    9           TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
                 WHEN J = 8 
                   MOVE    12          TO      L
                   MOVE    51          TO      C
                   MOVE    WK-SU4(1)   TO      WK-PU-SU1
                   MOVE    WK-SU4(2)   TO      WK-PU-SU2
                   MOVE    WK-SU4(3)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    14           TO      L
                   MOVE    WK-SU4(4)   TO      WK-PU-SU1
                   MOVE    WK-SU4(5)   TO      WK-PU-SU2
                   MOVE    WK-SU4(6)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA

                   MOVE    16          TO      L
                   MOVE    WK-SU4(7)   TO      WK-PU-SU1
                   MOVE    WK-SU4(8)   TO      WK-PU-SU2
                   MOVE    WK-SU4(9)   TO      WK-PU-SU3
                   DISPLAY SCR01-3-AREA
           END-EVALUATE
           .
       S212-1-1-EX.
           EXIT.

      *    *** ０２．魔方陣（縦、横、斜め　真中引いて同じ)
       S220-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** 罫線、入力項目
           MOVE    1           TO      WK-KETA
           MOVE    1           TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** 順列作成
           PERFORM S212-10     THRU    S212-EX

           .
       S220-EX.
           EXIT.

      *    *** ０３．魔方陣（縦、横、斜め　任意数字加えて同じ)
       S230-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** 罫線、入力項目
           MOVE    ZERO        TO      WK-KETA
           MOVE    ZERO        TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** 順列作成
           PERFORM S212-10     THRU    S212-EX

           .
       S230-EX.
           EXIT.

      *    *** ０４．魔方陣（縦、横、斜め　任意数字真中引いて同じ)
       S240-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** 罫線、入力項目
           MOVE    ZERO        TO      WK-KETA
           MOVE    ZERO        TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** 順列作成
           PERFORM S212-10     THRU    S212-EX

           .
       S240-EX.
           EXIT.

      *    *** ０５．花火１
       S250-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR BLACK
                   END-DISPLAY
           END-PERFORM

           MOVE    100000000   TO      WK-NANOSEC

           DISPLAY "|"
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY



           MOVE    RED         TO      C
           DISPLAY SCR05-1-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    GREEN       TO      C
           DISPLAY SCR05-2-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    YELLOW        TO      C
           DISPLAY SCR05-3-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    CYAN        TO      C
           DISPLAY SCR05-4-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    MAGENTA     TO      C
           DISPLAY SCR05-5-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    YELLOW      TO      C
           DISPLAY SCR05-4-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    RED         TO      C
           DISPLAY SCR05-5-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM



           MOVE    RED         TO      C
           DISPLAY SCR05-1-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    GREEN       TO      C
           DISPLAY SCR05-2-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    YELLOW        TO      C
           DISPLAY SCR05-3-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    CYAN        TO      C
           DISPLAY SCR05-4-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    MAGENTA     TO      C
           DISPLAY SCR05-5-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    YELLOW      TO      C
           DISPLAY SCR05-4-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    RED         TO      C
           DISPLAY SCR05-5-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           MOVE    BLACK       TO      C
           DISPLAY SCR05-5-AREA

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           MOVE    3           TO      WCR-IDX
           MOVE    2000        TO      WCR-FROM   (1)
           MOVE    100         TO      WCR-TO-CNT (1)
           MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    "-"         TO      WCR-SIGN   (1)
                                       WCR-SIGN   (2)
           MOVE    "N"         TO      WCR-ZERO   (1)
           MOVE    1000        TO      WCR-FROM2  (1)
           MOVE    10000       TO      WCR-TO2    (1)
           MOVE    2000        TO      WCR-FROM2  (2)
           MOVE    20000       TO      WCR-TO2    (2)

           MOVE    "RND"       TO      WCR-ID
           PERFORM VARYING C2 FROM 1 BY 1 
                   UNTIL C2 > 100
                   CALL    "COBRND"    USING   WCR-COBRND-AREA

                   COMPUTE L ROUNDED = ( WCR-RND(1) * 10 ) + 8

                   EVALUATE TRUE
                       WHEN L = 8 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28 
                       WHEN L = 9 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 10 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 11 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 12 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 13
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 20 ) + 23 
                       WHEN L = 14 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 15 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 16 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 17 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 18 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28
                   END-EVALUATE 

                   COMPUTE C ROUNDED =  WCR-RND(3) * 7

      *             DISPLAY "X"
                   MOVE    WCR-ALPHA(3) TO     WK-MOJI
                   DISPLAY WK-MOJI
                           AT LINE L COL CL
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR C
                                HIGHLIGHT
                   END-DISPLAY

                   DISPLAY " "
                           AT LINE 01 COL 01
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR BLACK
                   END-DISPLAY
                   CALL    "CBL_OC_NANOSLEEP" USING WK-NANOSEC

      *             DISPLAY " "
      *                     AT LINE L COL CL
      *                     WITH BACKGROUND-COLOR BLACK
      *                          FOREGROUND-COLOR BLACK
      *                          LOWLIGHT
      *                          HIGHLIGHT
      *             END-DISPLAY
           END-PERFORM

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           .

       S250-EX.
           EXIT.

      *    *** ０６．花火２
       S260-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           MOVE    100000000   TO      WK-NANOSEC

           DISPLAY "|"
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY



           MOVE    3           TO      WCR-IDX
           MOVE    2000        TO      WCR-FROM   (1)
           MOVE    100         TO      WCR-TO-CNT (1)
           MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    "-"         TO      WCR-SIGN   (1)
                                       WCR-SIGN   (2)
           MOVE    "N"         TO      WCR-ZERO   (1)
           MOVE    1000        TO      WCR-FROM2  (1)
           MOVE    10000       TO      WCR-TO2    (1)
           MOVE    2000        TO      WCR-FROM2  (2)
           MOVE    20000       TO      WCR-TO2    (2)

           MOVE    "RND"       TO      WCR-ID
           PERFORM VARYING C2 FROM 1 BY 1 
                   UNTIL C2 > 100
                   CALL    "COBRND"    USING   WCR-COBRND-AREA

                   COMPUTE L ROUNDED = ( WCR-RND(1) * 10 ) + 8

                   EVALUATE TRUE
                       WHEN L = 8 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28 
                       WHEN L = 9 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 10 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 11 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 12 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 13
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 20 ) + 23 
                       WHEN L = 14 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 15 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 16 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 17 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 18 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28
                   END-EVALUATE 

                   COMPUTE C ROUNDED =  WCR-RND(3) * 7

      *             DISPLAY "O"
                   MOVE    WCR-KANA (3) TO     WK-MOJI
                   DISPLAY WK-MOJI
                           AT LINE L COL CL
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR C
                                HIGHLIGHT
                   END-DISPLAY

                   DISPLAY " "
                           AT LINE 01 COL 01
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR BLACK
                   END-DISPLAY
                   CALL    "CBL_OC_NANOSLEEP" USING WK-NANOSEC

      *             DISPLAY " "
      *                     AT LINE L COL CL
      *                     WITH BACKGROUND-COLOR BLACK
      *                          FOREGROUND-COLOR BLACK
      *                          LOWLIGHT
      *                          HIGHLIGHT
      *             END-DISPLAY
           END-PERFORM

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           .
       S260-EX.
           EXIT.

      *    *** ０７．花火３
       S270-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           MOVE    100000000   TO      WK-NANOSEC

           DISPLAY "|"
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 24 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 23 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 22 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY

           DISPLAY "|"
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 21 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 20 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 19 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 18 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 17 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 16 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 15 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY


           DISPLAY "|"
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY " "
                   AT LINE 01 COL 01
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC

           DISPLAY " "
                   AT LINE 14 COL 33
                   WITH BACKGROUND-COLOR BLACK
                        FOREGROUND-COLOR BLACK
           END-DISPLAY



           MOVE    3           TO      WCR-IDX
           MOVE    2000        TO      WCR-FROM   (1)
           MOVE    100         TO      WCR-TO-CNT (1)
           MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    "-"         TO      WCR-SIGN   (1)
                                       WCR-SIGN   (2)
           MOVE    "N"         TO      WCR-ZERO   (1)
           MOVE    1000        TO      WCR-FROM2  (1)
           MOVE    10000       TO      WCR-TO2    (1)
           MOVE    2000        TO      WCR-FROM2  (2)
           MOVE    20000       TO      WCR-TO2    (2)

           MOVE    "RND"       TO      WCR-ID
           PERFORM VARYING C2 FROM 1 BY 1 
                   UNTIL C2 > 100
                   CALL    "COBRND"    USING   WCR-COBRND-AREA

                   COMPUTE L ROUNDED = ( WCR-RND(1) * 10 ) + 8

                   EVALUATE TRUE
                       WHEN L = 8 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28 
                       WHEN L = 9 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 10 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 11 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 12 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 13
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 20 ) + 23 
                       WHEN L = 14 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 18 ) + 24 
                       WHEN L = 15 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 16 ) + 25 
                       WHEN L = 16 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 14 ) + 26 
                       WHEN L = 17 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 12 ) + 27 
                       WHEN L = 18 
                           COMPUTE CL ROUNDED = ( WCR-RND(2) * 10 ) + 28
                   END-EVALUATE 

                   COMPUTE C ROUNDED =  WCR-RND(3) * 7

      *             COMPUTE CL2 = CL  - 1
      *             COMPUTE CL3 = CL2 - 1

                   DISPLAY SCR07-1-AREA
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR C
                                HIGHLIGHT
                   END-DISPLAY

                   DISPLAY " "
                           AT LINE 01 COL 01
                           WITH BACKGROUND-COLOR BLACK
                                FOREGROUND-COLOR BLACK
                   END-DISPLAY
                   CALL    "CBL_OC_NANOSLEEP" USING WK-NANOSEC

      *             COMPUTE CL2 = CL  - 1
      *             COMPUTE CL3 = CL2 - 1

      *             DISPLAY SCR07-2-AREA
                   DISPLAY SCR07-3-AREA
                   END-DISPLAY
           END-PERFORM

           .
       S270-EX.
           EXIT.

      *    *** ０８．誕生日当てゲーム
       S280-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           DISPLAY SCR08-1-AREA

           MOVE    ZERO        TO      SW-ERROR
           MOVE    SPACE       TO      WK-SUXXX
           MOVE    SPACE       TO      WK-ERR-COM
           PERFORM TEST AFTER
                   UNTIL SW-ERROR = ZERO
      *    *** IS NUMERIC のチェックはXタイプでないと出来ない
      *    *** ９タイプは英字入力しても、項目に入力されない為
                   ACCEPT  SCR08-2-AREA
      *    *** ３桁数字も入力あるので、ＮＵＭＶＡＬでＮＵＭＥＲＩＣチェック
                   CALL "C$JUSTIFY" USING WK-SUXXX "R"
                   INSPECT WK-SUXXX REPLACING ALL SPACE BY ZERO
                   IF      WK-SUXXX     IS     NUMERIC
                       IF  NUMVAL(WK-SUXXX) >= 326 AND
                           NUMVAL(WK-SUXXX) <= 1456
                           MOVE    ZERO        TO      SW-ERROR
                           MOVE    SPACE       TO      WK-ERR-COM
                           DISPLAY SCR08-2-AREA
                       ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE    SPACE       TO      WK-SUXXX
                           MOVE
                      "３２６から１４５６の範囲です、再入力してください"
                                               TO      WK-ERR-COM
                           DISPLAY SCR08-2-AREA
                       END-IF
                   ELSE
      *    *** NUMVALでチェックしたので、以下条件は発生しない
                       MOVE    "1"         TO      SW-ERROR
                       MOVE    SPACE       TO      WK-SUXXX
                       MOVE    "数字を入力してください" TO WK-ERR-COM
                       DISPLAY SCR08-2-AREA
                   END-IF
           END-PERFORM

           MOVE    NUMVAL(WK-SUXXX) TO     WK-SUXX

           COMPUTE WK-SUMM = ( WK-SUXX - 225 ) / 100

           COMPUTE WK-SUDD = ( WK-SUXX - 225 ) - ( WK-SUMM * 100 )

           DISPLAY SCR08-3-AREA

           IF      WK-SUMM     >=      1 AND
                   WK-SUMM     <=      12
                   CONTINUE
           ELSE
                   MOVE 
                   "月がおかしいですね？もう一度計算してみてください" 
                                       TO     WK-ERR-COM
                   DISPLAY SCR08-4-AREA
                   DISPLAY SCR08-5-AREA
           END-IF

           IF      WK-SUMM     =       1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               IF  WK-SUDD     >=      1 AND
                   WK-SUDD     <=      31
                   CONTINUE
               ELSE
                   MOVE 
                   "日がおかしいですね？もう一度入力してみてください" 
                                       TO     WK-ERR-COM
                   DISPLAY SCR08-4-AREA
                   DISPLAY SCR08-5-AREA
               END-IF
           ELSE
              IF   WK-SUMM     =       4 OR 6 OR 9 OR 11
               IF  WK-SUDD     >=      1 AND
                   WK-SUDD     <=      30
                   CONTINUE
               ELSE
                   MOVE 
                   "日がおかしいですね？もう一度入力してみてください" 
                                       TO     WK-ERR-COM
                   DISPLAY SCR08-4-AREA
                   DISPLAY SCR08-5-AREA
               END-IF
              ELSE
                IF   WK-SUMM     =       2
                 IF  WK-SUDD     >=      1 AND
                     WK-SUDD     <=      29
                     CONTINUE
                 ELSE
                     MOVE 
                   "日がおかしいですね？もう一度入力してみてください" 
                                         TO     WK-ERR-COM
                     DISPLAY SCR08-4-AREA
                   DISPLAY SCR08-5-AREA
                 END-IF
                ELSE
                 CONTINUE
                END-IF
              END-IF
           END-IF
           .
       S280-EX.
           EXIT.

      *    *** ０９．
       S290-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM
           .

       S290-EX.
           EXIT.

      *    *** １０．カレンダー表示
       S300-10.

           IF      WK-YYYY     =       ZERO AND
                   WK-MM       =       ZERO
                   CONTINUE
           ELSE
               IF  COB-CRT-STATUS =    1007 OR 1008 OR 1009 OR 1010
                   EVALUATE TRUE

                      WHEN COB-CRT-STATUS = 1007
                           ADD     -1          TO      WK-MM
                           IF      WK-MM       =       ZERO
                               MOVE    12          TO      WK-MM
                               IF      WK-YYYY     =       ZERO
                                   MOVE    9999        TO      WK-YYYY
                               ELSE
                                   ADD     -1          TO      WK-YYYY
                               END-IF
                           END-IF

                      WHEN COB-CRT-STATUS = 1008
                           ADD     +1          TO      WK-MM
                           IF      WK-MM       =       13
                                   MOVE    1           TO      WK-MM
                                   ADD     +1          TO      WK-YYYY
                           END-IF

                      WHEN COB-CRT-STATUS = 1009
      *    *** 月は表示月と同じとする
                           IF      WK-YYYY     =       ZERO
                               MOVE    9999        TO      WK-YYYY
                           ELSE
                               ADD     -1          TO      WK-YYYY
                           END-IF

                      WHEN COB-CRT-STATUS = 1010
      *    *** 月は表示月と同じとする
                           ADD     +1          TO      WK-YYYY
                   END-EVALUATE
                   GO  TO  S300-20
               ELSE
                   CONTINUE
               END-IF
           END-IF

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM

           ACCEPT  WDW-DATE2-YMD FROM   DATE YYYYMMDD
      *     ACCEPT  WDW-DATE2-WEEK FROM  DAY-OF-WEEK

      *    *** 当日のうるう年判定
           MOVE    "A"         TO      WDW-DATE2-ID
      *****     MOVE    WDW-DATE2-YYYY TO   WDW-DATE2-YYYY
      *****     MOVE    WDW-DATE2-MM TO     WDW-DATE2-MM
           MOVE    01          TO      WDW-DATE2-DD
           CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA


      *    *** ＊＊＊　カレンダー　＊＊＊　のセット
           DISPLAY SCR10-1-AREA

      *    *** 当日の年月セット
           DISPLAY SCR10-4-AREA

           MOVE    WDW-DATE2-YYYY TO   WK-YYYY
           MOVE    WDW-DATE2-MM   TO   WK-MM
           MOVE    SPACE       TO      WK-YYYYX
           MOVE    SPACE       TO      WK-MMX

           DISPLAY WK-HAI4
                   AT LINE 5 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    6           TO      WK-LINE
           MOVE    "日"        TO      WK-SUN
           MOVE    "月"        TO      WK-MON
           MOVE    "火"        TO      WK-TUE
           MOVE    "水"        TO      WK-WED
           MOVE    "木"        TO      WK-THU
           MOVE    "金"        TO      WK-FRI
           MOVE    "土"        TO      WK-SAT

      *    *** 曜日セット
           DISPLAY SCR10-AREA

      *    *** ＰＦＸＸ　前月、翌月　セット
           DISPLAY SCR10-6-AREA

           MOVE    SPACE       TO      WK-WEEK

      *    *** 曜日ＳＰＡＣＥセット後　罫線のみ　セット
           MOVE    8           TO      WK-LINE
           DISPLAY SCR10-AREA
           MOVE    10          TO      WK-LINE
           DISPLAY SCR10-AREA
           MOVE    12          TO      WK-LINE
           DISPLAY SCR10-AREA
           MOVE    14          TO      WK-LINE
           DISPLAY SCR10-AREA
           MOVE    16          TO      WK-LINE
           DISPLAY SCR10-AREA
           MOVE    18          TO      WK-LINE
           DISPLAY SCR10-AREA

           DISPLAY WK-HAI5
                   AT LINE 7 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *     DISPLAY WDW-DATEWEEK-AREA
      *             AT LINE 20 COL 8
      *             WITH 
      *             BACKGROUND-COLOR CYAN
      *             FOREGROUND-COLOR WHITE
      *     END-DISPLAY

           MOVE    SPACE       TO      WK-DD-X
           MOVE    ZERO        TO      WK-DD2
      *    *** 1=月曜日、7=日曜日を
      *    *** 1=日曜日、7=土曜日に変更
           IF      WDW-DATE2-WEEK =    7
                   MOVE    1           TO      WDW-DATE2-WEEK
           ELSE
                   ADD     1           TO      WDW-DATE2-WEEK
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
      *    *** １行目
               IF      I           >=      WDW-DATE2-WEEK AND
                       I           <=      7
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    8           TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI5
                   AT LINE 9 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ２行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WDW-DATE2-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    10          TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI5
                   AT LINE 11 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ３行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WDW-DATE2-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    12          TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI5
                   AT LINE 13 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ４行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WDW-DATE2-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    14          TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI5
                   AT LINE 15 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ５行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WDW-DATE2-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    16          TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI5
                   AT LINE 17 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ６行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WDW-DATE2-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    18          TO      L
           DISPLAY  SCR10-5-AREA

           DISPLAY WK-HAI6
                   AT LINE 19 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY



      *    *** 年、月　入力

           MOVE    ZERO        TO      SW-ERROR
           MOVE    SPACE       TO      WK-ERR-COM
           DISPLAY SCR10-2-AREA
           DISPLAY SCR10-3-AREA
           ACCEPT  SCR10-2-AREA
           ACCEPT  SCR10-3-AREA

           IF      COB-CRT-STATUS =    1007 OR 1008 OR 1009 OR 1010
                   EVALUATE TRUE
                      WHEN COB-CRT-STATUS = 1007
                           ADD     -1          TO      WK-MM
                           IF      WK-MM       =       ZERO
                               MOVE    12          TO      WK-MM
                               ADD     -1          TO      WK-YYYY
                           END-IF
                      WHEN COB-CRT-STATUS = 1008
                           ADD     +1          TO      WK-MM
                           IF      WK-MM       =       13
                                   MOVE    1           TO      WK-MM
                                   ADD     +1          TO      WK-YYYY
                           END-IF
                      WHEN COB-CRT-STATUS = 1009
      *    *** 月は表示月と同じとする
                           ADD     -1          TO      WK-YYYY
                      WHEN COB-CRT-STATUS = 1010
      *    *** 月は表示月と同じとする
                           ADD     +1          TO      WK-YYYY
                   END-EVALUATE
                   GO  TO  S300-20
           END-IF

           PERFORM TEST AFTER 
                   UNTIL SW-ERROR = "0"

                   CALL "C$JUSTIFY" USING WK-YYYYX "R"
                   INSPECT WK-YYYYX REPLACING ALL SPACE BY ZERO

                   CALL "C$JUSTIFY" USING WK-MMX "R"
                   INSPECT WK-MMX REPLACING ALL SPACE BY ZERO

                   IF      WK-YYYYX    IS      NUMERIC AND
      *                     WK-YYYY     >=      1582    AND
                           WK-MMX      IS      NUMERIC AND
                           NUMVAL(WK-MMX) >=    1      AND
                           NUMVAL(WK-MMX) <=    12
                           MOVE    NUMVAL(WK-YYYYX) TO WK-YYYY
                           MOVE    NUMVAL(WK-MMX)   TO WK-MM
                           MOVE    SPACE       TO      WK-YYYYX
                           MOVE    SPACE       TO      WK-MMX
                           MOVE    "0"         TO      SW-ERROR
                           MOVE    SPACE       TO      WK-ERR-COM
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
      *                     MOVE  "年は1582以降、月は1から12の範囲で指定"
                           MOVE  "年は数字、月は1から12の範囲で指定"
                                               TO      WK-ERR-COM
                           MOVE    SPACE       TO      WK-YYYYX
                           MOVE    SPACE       TO      WK-MMX
                           DISPLAY SCR10-2-AREA
                           DISPLAY SCR10-3-AREA
      *    *** 次の年月桁少ないかもですので、入力エリアクリアーする
                           ACCEPT  SCR10-2-AREA
                           ACCEPT  SCR10-3-AREA
                   END-IF
           END-PERFORM
           .

      *    *** PF07,08,09,10 入力の時、年、月　入力スキップする
       S300-20.

           MOVE    WK-YYYY     TO      WDW-DATE2-YYYY
           MOVE    WK-MM       TO      WDW-DATE2-MM
      *    *** WK-YYYYX,WK-MMX 次回の入力用に年月クリアーのセット
           DISPLAY SCR10-2-AREA
           DISPLAY SCR10-3-AREA
      *    *** 入力した年月のセット
           DISPLAY SCR10-4-AREA

           MOVE    "A"         TO      WDW-DATE2-ID
           MOVE    WK-YYYY     TO      WDW-DATE2-YYYY
           MOVE    WK-MM       TO      WDW-DATE2-MM
           MOVE    01          TO      WDW-DATE2-DD
           CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     DISPLAY WDT-DATEWEEK-AREA
      *             AT LINE 21 COL 8
      *             WITH 
      *             BACKGROUND-COLOR CYAN
      *             FOREGROUND-COLOR WHITE
      *     END-DISPLAY

           MOVE    SPACE       TO      WK-DD-X
           MOVE    ZERO        TO      WK-DD2
      *    *** 1=月曜日、7=日曜日を
      *    *** 1=日曜日、7=土曜日に変更
           IF      WDW-DATE2-WEEK =    7
                   MOVE    1           TO      WDW-DATE2-WEEK
           ELSE
                   ADD     1           TO      WDW-DATE2-WEEK
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
      *    *** １行目
               IF      I           >=      WDW-DATE2-WEEK AND
                       I           <=      7
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    8           TO      L
           DISPLAY  SCR10-5-AREA

      *    *** ２行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WK-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    10          TO      L
           DISPLAY  SCR10-5-AREA

      *    *** ３行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WK-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    12          TO      L
           DISPLAY  SCR10-5-AREA

      *    *** ４行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
               IF      WK-DD2      <      WDW-DATE2-DD2 (WK-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    14          TO      L
           DISPLAY  SCR10-5-AREA

      *    *** ５行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WK-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    16          TO      L
           DISPLAY  SCR10-5-AREA

      *    *** ６行目
           MOVE    SPACE       TO      WK-DD-X
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
               IF      WK-DD2      <       WDW-DATE2-DD2 (WK-MM)
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    18          TO      L
           DISPLAY  SCR10-5-AREA

           .
       S300-EX.
           EXIT.

      *    *** １１．ハイローゲーム
       S310-10.

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM

      *    *** ＊＊＊　ハイローゲーム　＊＊＊　のセット
           DISPLAY SCR11-1-AREA

      *    *** 罫線１セット
           DISPLAY WK-HAI7
                   AT LINE 4 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** 見出し
           DISPLAY WK-MID2
                   AT LINE 5 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** SEQ COMPUTER-NUM
           DISPLAY "SEQ"
                   AT LINE 5 COL 4
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY
           DISPLAY "COMPUTER-NUM"
                   AT LINE 5 COL 11
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY
           DISPLAY "SEQ"
                   AT LINE 5 COL 26
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY
           DISPLAY "COMPUTER-NUM"
                   AT LINE 5 COL 33
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY
           DISPLAY "SEQ"
                   AT LINE 5 COL 48
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY
           DISPLAY "COMPUTER-NUM"
                   AT LINE 5 COL 55
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

      *    *** 罫線２セット
           DISPLAY WK-HAI8
                   AT LINE 6 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** 罫線３セット
           PERFORM VARYING L FROM 7 BY 1
                   UNTIL L > 21
                   DISPLAY SCR11-AREA
           END-PERFORM

      *    *** 罫線４セット
           DISPLAY WK-HAI9
                   AT LINE 22 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** 数　入力
           MOVE    SPACE       TO      WK-ERR-COM
           MOVE    ZERO        TO      WK-SUXX
           MOVE    SPACE       TO      WK-SUXX12

           ACCEPT  SCR11-2-AREA

           PERFORM TEST AFTER 
                   UNTIL SW-ERROR = "0"
                   CALL "C$JUSTIFY" USING WK-SUXX12 "R"
                   INSPECT WK-SUXX12 REPLACING ALL SPACE BY ZERO

                   IF      WK-SUXX12  IS       NUMERIC
                       IF  NUMVAL(WK-SUXX12) >= 1 AND
      *    *** 数字　１０桁以内か
                           NUMVAL(WK-SUXX12) <  10000000000
                           MOVE    "0"         TO      SW-ERROR
                           MOVE    SPACE       TO      WK-ERR-COM
                           MOVE    NUMVAL(WK-SUXX12) TO WK-SUXX
                           INSPECT WK-SUXX12 REPLACING LEADING
                                   ZERO BY SPACE
                           DISPLAY SCR11-2-AREA
                       ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE
                             "数は数字、1から9,999,999,999の範囲で指定"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WK-SUXX
                           MOVE    SPACE       TO      WK-SUXX12
                           DISPLAY SCR11-2-AREA
                           ACCEPT  SCR11-2-AREA
                       END-IF
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE
                             "数は数字、1から9,999,999,999の範囲で指定"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WK-SUXX
                           MOVE    SPACE       TO      WK-SUXX12
                           DISPLAY SCR11-2-AREA
                           ACCEPT  SCR11-2-AREA
                   END-IF
           END-PERFORM
           

      *    *** HILOWG.CBL よりコピー
      *     DISPLAY "１から１０２４の数字を当てます、数を選んで下さい"
      *     DISPLAY " "

           MOVE    6           TO      L
           MOVE    LOW-VALUE   TO      WK-HIT-EOF
           MOVE    +8589934592 TO      WK-CHK-NUM
           MOVE    +1          TO      WK-LO-NUM
           MOVE    +17179869184 TO     WK-HI-NUM
           MOVE    ZERO        TO      WK-CNT

           PERFORM UNTIL   WK-HIT-EOF    =     HIGH-VALUE
           
      *             MOVE    WK-CHK-NUM    TO    WK-DSP-NUM
      *             DISPLAY "あなたが選んだ数字は" WK-DSP-NUM "ですか？"
      *             DISPLAY " "
      *             DISPLAY "HITなら9999,大きいなら3,小さいなら1エンター"
      *             ACCEPT  WK-ACT-NUM FROM     CONSOLE

                   EVALUATE TRUE
                       WHEN WK-CHK-NUM = WK-SUXX
                            MOVE    9999        TO      WK-ACT-NUM
                       WHEN WK-CHK-NUM > WK-SUXX
                            MOVE    1           TO      WK-ACT-NUM
                       WHEN OTHER
                            MOVE    3           TO      WK-ACT-NUM
                   END-EVALUATE

                   ADD     1           TO  WK-CNT
                   EVALUATE TRUE
                       WHEN WK-CNT < 16
                            ADD     1          TO      L
                            MOVE    WK-CNT     TO      WK-CNT-SU
                            MOVE    WK-CHK-NUM TO      WK-CHK-SU
                            DISPLAY WK-CNT-SU
                                    AT LINE L COL 4
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 9
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            IF      WK-CNT = 15
                                MOVE   6       TO      L
                            END-IF
                       WHEN  WK-CNT < 31
                            ADD     1          TO      L
                            MOVE    WK-CNT     TO      WK-CNT-SU
                            MOVE    WK-CHK-NUM TO      WK-CHK-SU
                            DISPLAY WK-CNT-SU
                                    AT LINE L COL 26
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 31
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            IF      WK-CNT = 30
                                MOVE   6       TO      L
                            END-IF
                       WHEN  WK-CNT < 46
                            ADD     1          TO      L
                            MOVE    WK-CNT     TO      WK-CNT-SU
                            MOVE    WK-CHK-NUM TO      WK-CHK-SU
                            DISPLAY WK-CNT-SU
                                    AT LINE L COL 48
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 53
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
                                    HIGHLIGHT
                            END-DISPLAY
                            IF      WK-CNT = 45
                                MOVE   6       TO      L
                            END-IF
                       WHEN OTHER
                            CONTINUE
                   END-EVALUATE

                   IF      WK-ACT-NUM     =    9999
                           MOVE    HIGH-VALUE   TO      WK-HIT-EOF
                   ELSE

                     IF    WK-ACT-NUM   =    1
                           MOVE   WK-CHK-NUM    TO      WK-HI-NUM
                           COMPUTE WK-CHK-NUM =
                                 ( WK-CHK-NUM - WK-LO-NUM ) / 2
                                 + WK-LO-NUM
                     ELSE
                           MOVE   WK-CHK-NUM    TO      WK-LO-NUM
                           COMPUTE WK-CHK-NUM =
                                 ( WK-HI-NUM - WK-CHK-NUM ) / 2
                                 + WK-CHK-NUM
                     END-IF
                   END-IF
                   

      *             DISPLAY "LO=" WK-LO-NUM " HI=" WK-HI-NUM " CHK="
      *                     WK-CHK-NUM
           END-PERFORM
      *     MOVE    WK-CHK-NUM  TO      WK-DSP-NUM
      *     DISPLAY "あなたの選んだ数字は" WK-DSP-NUM "ですね"
      *     DISPLAY " "
      *     DISPLAY WK-DSP-NUM "回でＨＩＴしました"

           DISPLAY "あなたの選んだ数字は"
                   AT LINE 23 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           MOVE    WK-SUXX     TO      WK-DSP-NUM
           DISPLAY WK-DSP-NUM
                   AT LINE 23 COL 22
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           DISPLAY "ですね"
                   AT LINE 23 COL 36
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           DISPLAY "コンピューターは"
                   AT LINE 24 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           MOVE    WK-CNT      TO      WK-DSP-NUM
           DISPLAY WK-DSP-NUM
                   AT LINE 24 COL 22
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           DISPLAY "回でＨＩＴしました"
                   AT LINE 24 COL 36
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT

           END-DISPLAY
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

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *    DISPLAY WK-PGM-NAME " END"
      *    DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT
      *    DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA
           .

       S900-EX.
           EXIT.
