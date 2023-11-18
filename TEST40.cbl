      *    *** ��ʃe�X�g
      *    *** TEST21.CBL ���R�s�[
      *    *** ��O��@SCREEN SECTION �Ł@�k�h�m�d�C�b�n�k�w�肷��
      *    *** ACCEPT,DISPLAY �łk�h�m�d�C�b�n�k�w�肷��Ƃ��AWORK-AREA
      *    *** ���ڂłȂ��ƁA�w�肵���ꏊ�ɕ\������Ȃ�

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST40.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
      *    *** ���̎w���COB-CRT-STATUS����`���Ȃ��Ă��A�g�p��
      *    *** PF01-PF24,SHIFT,CTRL,ALT���� 1001 - 1064
           FUNCTION ALL INTRINSIC. 
       SPECIAL-NAMES.
      *     CRT STATUS IS COB-CRT-STATUS. 
           CURRENCY SIGN IS "\".
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ���g�p
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���g�p
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
      *    *** ��ʏ�[�������r�o�`�b�d�ϊ����Ȃ�
      *    *** �ݕ��L�����o�Ȃ�
      *    *** �s���I�h���ҏW����Ȃ�
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

           03  WK-KEI1         PIC  X(002) VALUE "��".
           03  WK-KEI2         PIC  X(002) VALUE "��".
           03  WK-KEI3         PIC  X(002) VALUE "��".
           03  WK-KEI4         PIC  X(002) VALUE "��".
           03  WK-KEI5         PIC  X(002) VALUE "��".
           03  WK-KEI6         PIC  X(002) VALUE "��".
           03  WK-KEI7         PIC  X(002) VALUE "��".
           03  WK-KEI8         PIC  X(002) VALUE "��".
           03  WK-KEI9         PIC  X(002) VALUE "��".
           03  WK-KEI10        PIC  X(002) VALUE "��".
           03  WK-KEI11        PIC  X(002) VALUE "��".

           03  WK-HAI1.
             05  FILLER        PIC  X(001) VALUE "|"
             05  FILLER        PIC  X(059) VALUE ALL "-"
             05  FILLER        PIC  X(001) VALUE "|"
           03  WK-HAI2.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(012) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(012) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI3.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(006) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(012) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(012) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"

      *    *** HAI4,5,6�@�̓J�����_�[�p
           03  WK-HAI4.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI5.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI6.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"

      *    *** HAI7,8,9�@�̓n�C���[�Q�[���p
           03  WK-HAI7.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI8.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI9.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(014) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"

      *    *** HAI10,11,12�@�͖����w�p
           03  WK-HAI10.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI10S.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI11.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI11S.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "�@"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI12.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI12S.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI13.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
           03  WK-HAI13S.
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE SPACE
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"
             05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(002) VALUE "��"

           03  WK-TIT1.
             05  FILLER        PIC  X(020) VALUE SPACE.
             05  FILLER        PIC  N(014) VALUE
                 "�������@�������j���[�@������".
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
               "���j���[����".
           03  WK-MID2.
             05  FILLER        PIC  X(022) VALUE
               "��    ��              "
             05  FILLER        PIC  X(022) VALUE
               "��    ��              "
             05  FILLER        PIC  X(024) VALUE
               "��    ��              ��"
           03  WK-HAI          PIC  X(080) VALUE
               ALL "-".
           03  WK-MEI1.
             05  WK-MEI1-I00   PIC  X(080) VALUE
               "�O�O�D�������j���[".
             05  WK-MEI1-I01   PIC  X(080) VALUE
               "�O�P�D�����w�i�c�A���A�΂߁@�����ē���)".
             05  WK-MEI1-I02   PIC  X(080) VALUE
               "�O�Q�D�����w�i�c�A���A�΂߁@�^�������ē���)".
             05  WK-MEI1-I03   PIC  X(080) VALUE
               "�O�R�D�����w�i�c�A���A�΂߁@�C�Ӑ��������ē���)".
             05  WK-MEI1-I04    PIC  X(080) VALUE
               "�O�S�D�����w�i�c�A���A�΂߁@�C�Ӑ����^�������ē���)".
             05  WK-MEI1-I05   PIC  X(080) VALUE
               "�O�T�D�ԉ΂P".
             05  WK-MEI1-I06   PIC  X(080) VALUE
               "�O�U�D�ԉ΂Q".
             05  WK-MEI1-I07   PIC  X(080) VALUE
               "�O�V�D�ԉ΂R".
             05  WK-MEI1-I08   PIC  X(080) VALUE
               "�O�W�D�a�������ăQ�[��".
             05  WK-MEI1-I09   PIC  X(080) VALUE
               "�O�X�D".
             05  WK-MEI1-I10   PIC  X(080) VALUE
               "�P�O�D�J�����_�[�\��".
             05  WK-MEI1-I11   PIC  X(080) VALUE
               "�P�P�D�n�C���[�Q�[��".
             05  WK-MEI1-I99   PIC  X(080) VALUE
               "�X�X�D�I��".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

      *    *** CALL "CBL_OC_KEISEN" �p
      *    *** �r���\��
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
      *    *** HIGHLIGHT���͉��F
           03  YELLOWH         PIC  9(001) VALUE 6.
           03  GREY            PIC  9(001) VALUE 7.
      *    *** HIGHLIGHT���͔��F
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
             05  LINE 25 COL 8 VALUE "���j���[�m��"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
      *          HIGHLIGHT
      *          OVERLINE
      *          UNDERLINE
                 .
      *    *** TO �͓��͍��ڂƂ��Ē�`����
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
      * �s��          JUST
      * ���́�        SECURE
      * �s��          FULL
      * �s��           PROMPT
      * ���]           REVERSE-VIDEO
      * ��ʏ�����     ERASE EOL
      * �w��o���Ȃ�   EOS
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
      *    *** �W�c���ڂŐF�w��o����
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR01-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "�������@�����w�@������".

           03  SCR01-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�������@����".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
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
                 VALUE "�����l�@����".

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

      *    *** FROM �͏o�͍��ڂƂ��Ē�`����
               07  FROM WK-PU-SU1
                   LINE L COL C
                   PIC  ZZZ9.
               07  FROM WK-PU-SU2
                   COL PLUS 2
                   PIC  ZZZ9.
               07  FROM WK-PU-SU3
                   COL PLUS 2
                   PIC  ZZZ9.

      *    *** �ԉΗp
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

      *    *** EOS �w��̓G���[�ɂȂ�
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

      *    *** �a�������ăQ�[���p
           03  SCR08-AREA
      *    *** �W�c���ڂŐF�w��o����
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

           03  SCR08-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "�������@�a�������ăQ�[���@������".

           03  SCR08-2-AREA.
             05  LINE 3 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "���ɂS���|���āA�X�������āA�Q�T���|���āA".
             05  LINE 3
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�����������A�������͂��Ă�������".
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
                 VALUE "�v�Z���F�i�����S�{�X�j*�Q�T�{��".

      *    *** PIC X(NNN) �L�����Ȃ��ƕ\������Ȃ�
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
                 VALUE "�a������".

             05  FROM WK-SUMM
                 LINE 7 COL PLUS 2
                 UNDERLINE
                       PIC  Z9.
             05  LINE 7 COL PLUS 1
                 VALUE "��".

             05  FROM WK-SUDD
                 LINE 7 COL PLUS 2
                 UNDERLINE
                       PIC  Z9.
             05  LINE 7 COL PLUS 1
                 VALUE "���ł���".

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
                 VALUE "�@�H�H".

      *    *** �J�����_�[�p
           03  SCR10-AREA
      *    *** �W�c���ڂŐF�w��o����
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
                 
             05  LINE WK-LINE COL 8
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
                 VALUE "��".
      *    *** FROM �͏o�͍��ڂƂ��Ē�`����
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
             05  VALUE "��"
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
                 VALUE "��".
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
                 VALUE "��".
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
                 VALUE "��".
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
                 VALUE "��".
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
                 VALUE "��".
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
                 VALUE "��".

           03  SCR10-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 HIGHLIGHT
                 VALUE "�������@�J�����_�[�@������".

           03  SCR10-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�N�@����".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
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
      *           VALUE "1582�N10��15��(�O���S���I��)�ȍ~�A�Z�o".

           03  SCR10-3-AREA.

             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "���@����".

             05  USING WK-MMX
                 LINE 3 COL  12
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
      *           AUTO
                       PIC  X(002).

      *    *** PIC X(NNN) �L�����Ȃ��ƕ\������Ȃ�
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
                 VALUE "�N".

             05  FROM WDW-DATE2-MM
                 LINE 4 COL 47
                       PIC  Z9.

             05  LINE 4 COL 49
                 VALUE "��".

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
                 VALUE "�o�e�O�V�F�O���@�o�e�O�W�F����".
             05  LINE 22 COL 40
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�o�e�O�X�F�O�N�@�o�e�P�O�F���N".

      *    *** �n�C���[�Q�[���p
           03  SCR11-AREA
      *    *** �W�c���ڂŐF�w��o����
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
                 
             05  LINE L COL 1
                 VALUE "��".
             05  COL PLUS 4
                 VALUE "��".
             05  COL PLUS 14
                 VALUE "��".
             05  COL PLUS 4
                 VALUE "��".
             05  COL PLUS 14
                 VALUE "��".
             05  COL PLUS 4
                 VALUE "��".
             05  COL PLUS 14
                 VALUE "��".

           03  SCR11-1-AREA.
             05  LINE 1 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR CYAN
                 HIGHLIGHT
                 VALUE "�������@�n�C���[�Q�[���@������".

           03  SCR11-2-AREA.
             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�����P�O���ȓ��œ���".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-SUXX12
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  X(012).
    
      *    *** PIC X(NNN) �L�����Ȃ��ƕ\������Ȃ�
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

      *    *** �O�O�D�������j���[
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX
      *
           PERFORM UNTIL WK-TESTNO = 99
                   EVALUATE TRUE
                       WHEN WK-TESTNO = 0
      *    *** �O�O�D�������j���[
                            PERFORM S020-10     THRU    S020-EX
                       WHEN WK-TESTNO = 1
      *    *** �O�P�D�����w�i�c�A���A�΂߁@�����ē���)
                            PERFORM S210-10     THRU    S210-EX
                       WHEN WK-TESTNO = 2
      *    *** �O�Q�D�����w�i�c�A���A�΂߁@�^�������ē���)
                            PERFORM S220-10     THRU    S220-EX
                       WHEN WK-TESTNO = 3
      *    *** �O�R�D�����w�i�c�A���A�΂߁@�C�Ӑ��������ē���)
                            PERFORM S230-10     THRU    S230-EX
                       WHEN WK-TESTNO = 4
      *    *** �O�S�D�����w�i�c�A���A�΂߁@�C�Ӑ����^�������ē���)
                            PERFORM S240-10     THRU    S240-EX
                       WHEN WK-TESTNO = 5
      *    *** �O�T�D�ԉ΂P
                            PERFORM S250-10     THRU    S250-EX
                       WHEN WK-TESTNO = 6
      *    *** �O�U�D�ԉ΂Q
                            PERFORM S260-10     THRU    S260-EX
                       WHEN WK-TESTNO = 7
      *    *** �O�V�D�ԉ΂R
                            PERFORM S270-10     THRU    S270-EX
                       WHEN WK-TESTNO = 8
      *    *** �O�W�D�a�������ăQ�[��
                            PERFORM S280-10     THRU    S280-EX
                       WHEN WK-TESTNO = 9 AND 
                            WK-PIN1-EOF NOT = HIGH-VALUE
      *    *** �O�X�D
                            PERFORM S290-10     THRU    S290-EX
      *    *** READ PIN1
                            PERFORM S100-10     THRU    S100-EX
                       WHEN WK-TESTNO = 10
      *    *** �P�O�D�J�����_�[�\��
                            PERFORM S300-10     THRU    S300-EX
                       WHEN WK-TESTNO = 11
      *    *** �P�P�D�n�C���[�Q�[��
                            PERFORM S310-10     THRU    S310-EX
                   END-EVALUATE
      *    *** �������j���[�\���@���͑҂�
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

      *    *** �O�O�D�������j���[
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
      *    *** COLOR �w�肵�Ȃ��ƁA�w�i�F���A�󎚁F���ɂȂ�
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

      *    *** �������j���[�\���@���͑҂�
       S030-10.

           MOVE    WK-TESTNO   TO      WK-TESTNO-OLD
           ACCEPT  SCR00-AREA

           IF      WK-TESTNO   NOT =   WK-TESTNO-OLD
      *    *** �J�����_�[�@��x���j���[�ύX������A���Z�b�g�̂��߁A
      *    *** WK-YYYY,WK-MM �y�d�q�n�ɂ���
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

      *    *** 256�o�C�g�܂ł�������Ȃ�
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
      *    *** ANK�̂Ƃ��A
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

      *    *** ���L�̕��@�ł́A�����������@�ŏ��̊������肪�A�ꕶ���ڂ�
      *    *** �Q�o�C�g�ڂƎ��̂P�o�C�g�ڂŊ����Ŕ��肳��邱�Ƃ����邽��
           MOVE    1520        TO      J
           MOVE    81          TO      K
           PERFORM VARYING I FROM 77 BY 80
                   UNTIL I > 880
      *    *** 77,2 �����̂Ƃ��A
               IF ( WK-PIN1-I4 (I:2) >= X"8140" AND 
                    WK-PIN1-I4 (I:2) <= X"9FFC" )   OR
                  ( WK-PIN1-I4 (I:2) >= X"E040" AND 
                    WK-PIN1-I4 (I:2) <= X"EAA4" )
                   COMPUTE I2 = I - 2
      *    *** 75,2 ���������H
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
      *    *** 76,1 �������H
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
      *    *** 75,2 ���������H
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
      *    *** 76,1 ���������H
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

      *    *** �O�P�D�����w�i�c�A���A�΂߁@�����ē���)
       S210-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** �r���A���͍���
           MOVE    1           TO      WK-KETA
           MOVE    1           TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** ����쐬
           PERFORM S212-10     THRU    S212-EX

           .
       S210-EX.
           EXIT.

      *    *** �r���A���͍���
       S211-10.
      *    *** �����w
           DISPLAY SCR01-1-AREA

      *    *** �r��

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

      *    *** ���A�����l�@����

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
                                    MOVE   "�P���͏����l1�݂̂Ŏw��"
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
                                    "�Q���͏����l10-91�͈̔͂Ŏw��"
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
                                   "�R���͏����l100-991�͈̔͂Ŏw��"
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
                                    "�S���͏����l1000-9991�͈̔͂Ŏw��"
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
                           MOVE  "���͐����A�P����S�͈̔͂Ŏw��"
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

      *    *** ����쐬
       S212-10.
      *    *** WK-IDX-END = + 8 �ɂ���
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
      *    *** +,-�@���v�@�e�񓯂���
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

      *    *** +,-�@���v�@�e�񓯂���
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

      *    *** ����@�e�}�X�ɏo��
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

      *    *** ����@�e�}�X�ɏo��
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

      *    *** ����@�e�}�X�ɏo��
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

      *    *** �O�Q�D�����w�i�c�A���A�΂߁@�^�������ē���)
       S220-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** �r���A���͍���
           MOVE    1           TO      WK-KETA
           MOVE    1           TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** ����쐬
           PERFORM S212-10     THRU    S212-EX

           .
       S220-EX.
           EXIT.

      *    *** �O�R�D�����w�i�c�A���A�΂߁@�C�Ӑ��������ē���)
       S230-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** �r���A���͍���
           MOVE    ZERO        TO      WK-KETA
           MOVE    ZERO        TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** ����쐬
           PERFORM S212-10     THRU    S212-EX

           .
       S230-EX.
           EXIT.

      *    *** �O�S�D�����w�i�c�A���A�΂߁@�C�Ӑ����^�������ē���)
       S240-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *** �r���A���͍���
           MOVE    ZERO        TO      WK-KETA
           MOVE    ZERO        TO      WK-SYOKI
           PERFORM S211-10     THRU    S211-EX

      *    *** ����쐬
           PERFORM S212-10     THRU    S212-EX

           .
       S240-EX.
           EXIT.

      *    *** �O�T�D�ԉ΂P
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

      *    *** �O�U�D�ԉ΂Q
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

      *    *** �O�V�D�ԉ΂R
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

      *    *** �O�W�D�a�������ăQ�[��
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
      *    *** IS NUMERIC �̃`�F�b�N��X�^�C�v�łȂ��Əo���Ȃ�
      *    *** �X�^�C�v�͉p�����͂��Ă��A���ڂɓ��͂���Ȃ���
                   ACCEPT  SCR08-2-AREA
      *    *** �R�����������͂���̂ŁA�m�t�l�u�`�k�łm�t�l�d�q�h�b�`�F�b�N
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
                      "�R�Q�U����P�S�T�U�͈̔͂ł��A�ē��͂��Ă�������"
                                               TO      WK-ERR-COM
                           DISPLAY SCR08-2-AREA
                       END-IF
                   ELSE
      *    *** NUMVAL�Ń`�F�b�N�����̂ŁA�ȉ������͔������Ȃ�
                       MOVE    "1"         TO      SW-ERROR
                       MOVE    SPACE       TO      WK-SUXXX
                       MOVE    "��������͂��Ă�������" TO WK-ERR-COM
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
                   "�������������ł��ˁH������x�v�Z���Ă݂Ă�������" 
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
                   "�������������ł��ˁH������x���͂��Ă݂Ă�������" 
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
                   "�������������ł��ˁH������x���͂��Ă݂Ă�������" 
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
                   "�������������ł��ˁH������x���͂��Ă݂Ă�������" 
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

      *    *** �O�X�D
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

      *    *** �P�O�D�J�����_�[�\��
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
      *    *** ���͕\�����Ɠ����Ƃ���
                           IF      WK-YYYY     =       ZERO
                               MOVE    9999        TO      WK-YYYY
                           ELSE
                               ADD     -1          TO      WK-YYYY
                           END-IF

                      WHEN COB-CRT-STATUS = 1010
      *    *** ���͕\�����Ɠ����Ƃ���
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

      *    *** �����̂��邤�N����
           MOVE    "A"         TO      WDW-DATE2-ID
      *****     MOVE    WDW-DATE2-YYYY TO   WDW-DATE2-YYYY
      *****     MOVE    WDW-DATE2-MM TO     WDW-DATE2-MM
           MOVE    01          TO      WDW-DATE2-DD
           CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA


      *    *** �������@�J�����_�[�@�������@�̃Z�b�g
           DISPLAY SCR10-1-AREA

      *    *** �����̔N���Z�b�g
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
           MOVE    "��"        TO      WK-SUN
           MOVE    "��"        TO      WK-MON
           MOVE    "��"        TO      WK-TUE
           MOVE    "��"        TO      WK-WED
           MOVE    "��"        TO      WK-THU
           MOVE    "��"        TO      WK-FRI
           MOVE    "�y"        TO      WK-SAT

      *    *** �j���Z�b�g
           DISPLAY SCR10-AREA

      *    *** �o�e�w�w�@�O���A�����@�Z�b�g
           DISPLAY SCR10-6-AREA

           MOVE    SPACE       TO      WK-WEEK

      *    *** �j���r�o�`�b�d�Z�b�g��@�r���̂݁@�Z�b�g
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
      *    *** 1=���j���A7=���j����
      *    *** 1=���j���A7=�y�j���ɕύX
           IF      WDW-DATE2-WEEK =    7
                   MOVE    1           TO      WDW-DATE2-WEEK
           ELSE
                   ADD     1           TO      WDW-DATE2-WEEK
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
      *    *** �P�s��
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

      *    *** �Q�s��
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

      *    *** �R�s��
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

      *    *** �S�s��
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

      *    *** �T�s��
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

      *    *** �U�s��
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



      *    *** �N�A���@����

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
      *    *** ���͕\�����Ɠ����Ƃ���
                           ADD     -1          TO      WK-YYYY
                      WHEN COB-CRT-STATUS = 1010
      *    *** ���͕\�����Ɠ����Ƃ���
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
      *                     MOVE  "�N��1582�ȍ~�A����1����12�͈̔͂Ŏw��"
                           MOVE  "�N�͐����A����1����12�͈̔͂Ŏw��"
                                               TO      WK-ERR-COM
                           MOVE    SPACE       TO      WK-YYYYX
                           MOVE    SPACE       TO      WK-MMX
                           DISPLAY SCR10-2-AREA
                           DISPLAY SCR10-3-AREA
      *    *** ���̔N�������Ȃ������ł��̂ŁA���̓G���A�N���A�[����
                           ACCEPT  SCR10-2-AREA
                           ACCEPT  SCR10-3-AREA
                   END-IF
           END-PERFORM
           .

      *    *** PF07,08,09,10 ���͂̎��A�N�A���@���̓X�L�b�v����
       S300-20.

           MOVE    WK-YYYY     TO      WDW-DATE2-YYYY
           MOVE    WK-MM       TO      WDW-DATE2-MM
      *    *** WK-YYYYX,WK-MMX ����̓��͗p�ɔN���N���A�[�̃Z�b�g
           DISPLAY SCR10-2-AREA
           DISPLAY SCR10-3-AREA
      *    *** ���͂����N���̃Z�b�g
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
      *    *** 1=���j���A7=���j����
      *    *** 1=���j���A7=�y�j���ɕύX
           IF      WDW-DATE2-WEEK =    7
                   MOVE    1           TO      WDW-DATE2-WEEK
           ELSE
                   ADD     1           TO      WDW-DATE2-WEEK
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 7
      *    *** �P�s��
               IF      I           >=      WDW-DATE2-WEEK AND
                       I           <=      7
                   ADD     1           TO      WK-DD2
                   MOVE    WK-DD2      TO      WK-DD (I)
               END-IF
           END-PERFORM

           MOVE    8           TO      L
           DISPLAY  SCR10-5-AREA

      *    *** �Q�s��
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

      *    *** �R�s��
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

      *    *** �S�s��
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

      *    *** �T�s��
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

      *    *** �U�s��
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

      *    *** �P�P�D�n�C���[�Q�[��
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

      *    *** �������@�n�C���[�Q�[���@�������@�̃Z�b�g
           DISPLAY SCR11-1-AREA

      *    *** �r���P�Z�b�g
           DISPLAY WK-HAI7
                   AT LINE 4 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ���o��
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

      *    *** �r���Q�Z�b�g
           DISPLAY WK-HAI8
                   AT LINE 6 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** �r���R�Z�b�g
           PERFORM VARYING L FROM 7 BY 1
                   UNTIL L > 21
                   DISPLAY SCR11-AREA
           END-PERFORM

      *    *** �r���S�Z�b�g
           DISPLAY WK-HAI9
                   AT LINE 22 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

      *    *** ���@����
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
      *    *** �����@�P�O���ȓ���
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
                             "���͐����A1����9,999,999,999�͈̔͂Ŏw��"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WK-SUXX
                           MOVE    SPACE       TO      WK-SUXX12
                           DISPLAY SCR11-2-AREA
                           ACCEPT  SCR11-2-AREA
                       END-IF
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE
                             "���͐����A1����9,999,999,999�͈̔͂Ŏw��"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WK-SUXX
                           MOVE    SPACE       TO      WK-SUXX12
                           DISPLAY SCR11-2-AREA
                           ACCEPT  SCR11-2-AREA
                   END-IF
           END-PERFORM
           

      *    *** HILOWG.CBL ���R�s�[
      *     DISPLAY "�P����P�O�Q�S�̐����𓖂Ă܂��A����I��ŉ�����"
      *     DISPLAY " "

           MOVE    6           TO      L
           MOVE    LOW-VALUE   TO      WK-HIT-EOF
           MOVE    +8589934592 TO      WK-CHK-NUM
           MOVE    +1          TO      WK-LO-NUM
           MOVE    +17179869184 TO     WK-HI-NUM
           MOVE    ZERO        TO      WK-CNT

           PERFORM UNTIL   WK-HIT-EOF    =     HIGH-VALUE
           
      *             MOVE    WK-CHK-NUM    TO    WK-DSP-NUM
      *             DISPLAY "���Ȃ����I�񂾐�����" WK-DSP-NUM "�ł����H"
      *             DISPLAY " "
      *             DISPLAY "HIT�Ȃ�9999,�傫���Ȃ�3,�������Ȃ�1�G���^�["
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
      *     DISPLAY "���Ȃ��̑I�񂾐�����" WK-DSP-NUM "�ł���"
      *     DISPLAY " "
      *     DISPLAY WK-DSP-NUM "��łg�h�s���܂���"

           DISPLAY "���Ȃ��̑I�񂾐�����"
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

           DISPLAY "�ł���"
                   AT LINE 23 COL 36
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
                   HIGHLIGHT
           END-DISPLAY

           DISPLAY "�R���s���[�^�[��"
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

           DISPLAY "��łg�h�s���܂���"
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
      *    DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT
      *    DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA
           .

       S900-EX.
           EXIT.
