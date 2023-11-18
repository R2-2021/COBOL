      *    *** ��ʃe�X�g
      *    *** ��O��@SCREEN SECTION �Ł@�k�h�m�d�C�b�n�k�w�肷��
      *    *** ACCEPT,DISPLAY �łk�h�m�d�C�b�n�k�w�肷��Ƃ��AWORK-AREA
      *    *** ���ڂłȂ��ƁA�w�肵���ꏊ�ɕ\������Ȃ�

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST21.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
      *    *** ���̎w���COB-CRT-STATUS����`���Ȃ��Ă��A�g�p��
      *    *** PF01-PF24,SHIFT,CTRL,ALT���� 1001 - 1064
      *    *** NUMVAL ���̎w��ŃG���[�Ȃ��Ȃ�AFUNCTION �̎w����s�v
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

           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST21  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST21.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST21.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.

           03  WK-SHORI-CNT    BINARY-LONG VALUE 10000000.
           03  WK-CTR-STATUS   PIC  9(004) VALUE 0.
           03  WK-BINARY       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BI-SHORT     BINARY-SHORT  SYNC VALUE 0.
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
           03  WK-NANOSEC01    PIC  9(011) VALUE 300000000.
           03  WK-TESTNO-OLD   PIC  9(002) VALUE 0.
           03  WK-TESTNO       PIC  9(002) VALUE 0.
           03  WK-ITEM1        PIC  9(002) VALUE 0.
           03  WK-ITEM2        PIC  9(002) VALUE 0.
           03  WK-ITEM3        PIC  X(020) VALUE SPACE.
           03  WK-ITEM4        PIC  X(020) VALUE SPACE.
           03  WK-PI           PIC  9V9(5) VALUE 0.
           03  WK-DEG          PIC  9(003) VALUE 0.
           03  WK-DEG2         PIC  ZZ9    VALUE ZERO.
           03  WK-RAD          PIC  9V9(3) VALUE 0.
           03  WK-RAD2         PIC  9.9(3) VALUE ZERO.
           03  WK-LINE         PIC  9(002) VALUE ZERO.
           03  WK-WEEK.
             05  WFD-SUN       PIC  X(002) VALUE SPACE.
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
           03  WK-YYYY-2       PIC  9(004) VALUE ZERO.
           03  WK-MM           PIC  9(002) VALUE ZERO.
           03  WK-AMARI-400    PIC  9(004) VALUE ZERO.
           03  WK-AMARI-100    PIC  9(004) VALUE ZERO.
           03  WK-AMARI-4      PIC  9(004) VALUE ZERO.
           03  WK-DD-X.
             05  WK-DD         OCCURS 7
                               PIC  X(002) VALUE ZERO.
           03  WK-DD2          PIC  9(002) VALUE ZERO.
           03  WK-ERR-COM      PIC  X(060) VALUE SPACE.

           03  WFD-SUXX        PIC  9(012) VALUE ZERO.
           03  WK-CHK-SU       PIC  ZZ,ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-CNT-SU       PIC  ZZ9    VALUE ZERO.
           03  WK-ACT-NUM      BINARY-LONG VALUE ZERO.
           03  WK-HIT-EOF      PIC  X(001) VALUE LOW-VALUE.

           03  WK-CNS-1        BINARY-DOUBLE SYNC VALUE ZERO.
           03  WK-CHK-NUM      BINARY-DOUBLE SYNC VALUE +8589934592.
           03  WK-LO-NUM       BINARY-DOUBLE SYNC VALUE +1.
           03  WK-HI-NUM       BINARY-DOUBLE SYNC VALUE +17179869184.
           03  WK-CNT          BINARY-LONG SYNC VALUE ZERO.
           03  WK-DSP-NUM      PIC Z,ZZZ,ZZZ,ZZ9 VALUE ZERO.

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
           03  WK-DATA5        PIC  -ZZZ,ZZ9.99 VALUE ZERO.
           03  WK-DATA5-R      REDEFINES WK-DATA5.
             05  WK-DATA5-R1   PIC  X(011).
           03  WK-DATA6        PIC  -\\\,\\\.99 VALUE ZERO.
           03  WK-DATA6-R      REDEFINES WK-DATA6.
             05  WK-DATA6-R1   PIC  X(011).
           03  WK-DATA7        OCCURS 16
                               PIC  X(080) VALUE SPACE.
           03  WK-DATA8        PIC  X(080) VALUE SPACE.
           03  WK-MOJI         PIC  X(080) VALUE SPACE.
           03  WK-PTN          PIC  X(002) VALUE ZERO.
           03  WK-ASCII-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-SOKUDO       PIC  X(002) VALUE ZERO.
           03  WK-COLOR        PIC  X(001) VALUE ZERO.

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
      *       05  FILLER        PIC  X(004) VALUE ALL "��"
             05  FILLER        PIC  X(004) VALUE "����"
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

           03  WK-TIT1.
             05  FILLER        PIC  X(020) VALUE SPACE.
             05  FILLER        PIC  X(026) VALUE
                 "�������@��ʃe�X�g�@������".
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

           03  WK-MID1         PIC  X(010) VALUE
               "�e�X�g����".
           03  WK-MID2.
             05  FILLER        PIC  X(022) VALUE
               "�� SEQ��  COMPUTER-NUM"
             05  FILLER        PIC  X(022) VALUE
               "�� SEQ��  COMPUTER-NUM".
             05  FILLER        PIC  X(024) VALUE
               "�� SEQ��  COMPUTER-NUM��".
           03  WK-HAI          PIC  X(080) VALUE
               ALL "-".
           03  WK-MEI1.
             05  WK-MEI1-I00   PIC  X(080) VALUE
               "�O�O�D�������j���[".
             05  WK-MEI1-I01   PIC  X(080) VALUE
               "�O�P�D��ʐF�ݒ�@�O�`�V�Ŏw��@�e�@�a�@�k�^�g".
             05  WK-MEI1-I02   PIC  X(080) VALUE
               "�O�Q�D�����_�������\���@���w��".
             05  WK-MEI1-I03   PIC  X(080) VALUE
               "�O�R�D�e�n�q�d�C�a�`�b�j�f�q�n�t�m�c�T���v��".
             05  WK-MEI1-I04    PIC  X(080) VALUE
               "�O�S�D�k�h�m�d�C�b�n�k�t�l�m�@�o�k�t�r�C�l�h�m�t�r".
             05  WK-MEI1-I05   PIC  X(080) VALUE
               "�O�T�D�c�d�f�x�C�q�`�f���W�A���C�r�h�m�i�q�`�f�j�l".
             05  WK-MEI1-I06   PIC  X(080) VALUE
               "�O�U�D�������ړ��́A�r���\���P".
             05  WK-MEI1-I07   PIC  X(080) VALUE
               "�O�V�D�������ړ��́A�r���\���Q".
             05  WK-MEI1-I08   PIC  X(080) VALUE
               "�O�W�D�������ړ��́A�r���\���R".
             05  WK-MEI1-I09   PIC  X(080) VALUE
               "�O�X�D�t�@�C�����e�\��".
             05  WK-MEI1-I10   PIC  X(080) VALUE
               "�P�O�D�J�����_�[�\��".
             05  WK-MEI1-I11   PIC  X(080) VALUE
               "�P�P�D�n�C���[�Q�[��".
             05  WK-MEI1-I12   PIC  X(080) VALUE
               "�P�Q�D�o�i�[".
             05  WK-MEI1-I13   PIC  X(080) VALUE
               "�P�R�D�o�i�[�i�E�N���C�i�j".
             05  WK-MEI1-I99   PIC  X(080) VALUE
               "�X�X�D�I��".

      *    *** CALL "CBL_OC_KEISEN" �p
      *    *** �r���\��
           03  KEI-CMD         PIC  9(002) VALUE 1.
           03  KEI-LINE        PIC  9(002) VALUE 10.
           03  KEI-COL         PIC  9(002) VALUE 10.
           03  KEI-LEN1        PIC  9(002) VALUE 6.
           03  KEI-LEN2        PIC  9(002) VALUE 1.
           03  KEI-COLOR       PIC  9(002) VALUE 0.
           03  KEI-PRN         PIC  9(002) VALUE 0.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

           COPY    CPDECODE07  REPLACING ==:##:== BY ==WDE07==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES PIC X(1).

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

      * 01  PRINT-AREA.
      *     03  PR-LINE         OCCURS 40
      *                         PIC  X(136) VALUE SPACE.

       01  INDEX-AREA.
           03  B               BINARY-LONG SYNC VALUE ZERO.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  C2              BINARY-LONG SYNC VALUE ZERO.
           03  C3              BINARY-LONG SYNC VALUE ZERO.
           03  C4              BINARY-LONG SYNC VALUE ZERO.
           03  F               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE ZERO.
           03  I5              BINARY-LONG SYNC VALUE ZERO.
           03  I6              BINARY-LONG SYNC VALUE ZERO.
           03  I7              BINARY-LONG SYNC VALUE ZERO.
           03  I8              BINARY-LONG SYNC VALUE ZERO.
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
           03  SW-END          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-L            PIC S9(009) VALUE 0 COMP-3.
           03  SV-K            PIC S9(009) VALUE 0 COMP-3.

       01  COLORS-AREA.
           03  BLACK           PIC  9(001) VALUE 0.
           03  BLUE            PIC  9(001) VALUE 1.
           03  GREEN           PIC  9(001) VALUE 2.
           03  CYAN            PIC  9(001) VALUE 3.
           03  RED             PIC  9(001) VALUE 4.
           03  MAGENTA         PIC  9(001) VALUE 5.
           03  BROWN           PIC  9(001) VALUE 6.
      *    *** HIGHLIGHT���͉��F
           03  YELLOW          PIC  9(001) VALUE 6.
           03  GREY            PIC  9(001) VALUE 7.
      *    *** HIGHLIGHT���͔��F
           03  WHITE           PIC  9(001) VALUE 7.
           03  COLORS-NAME.
             05  COLOR-0L      PIC  X(010) VALUE "BLACK     ".
             05  COLOR-0H      PIC  X(010) VALUE "DARK GREY ".
             05  COLOR-1       PIC  X(010) VALUE "BLUE      ".
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

           03  SCR01-AREA.
             05  LINE 25 COL 1           VALUE " "
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN.

           03  SCR00-AREA.
             05  LINE 25 COL 10         VALUE "�e�X�g�m��"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
      *          HIGHLIGHT
      *          OVERLINE
                 UNDERLINE
                 .
      *    *** TO �͓��͍��ڂƂ��Ē�`����
             05  COL PLUS 2 TO WK-TESTNO
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *          OVERLINE
      *           UNDERLINE
                 AUTO-SKIP
      *           PIC 9(002).
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
      *           UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM2
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           UNDERLINE
                 AUTO-SKIP
                 PIC 9(002).

             05  COL PLUS 2 USING WK-ITEM3
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
      *           UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

             05  COL PLUS 2 USING WK-ITEM4
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR BLACK
                 UNDERLINE
                 AUTO-SKIP
                 PIC X(020).

           03  SCR04-AREA.
             05  LINE  9 COL 10          VALUE "�`�a�b"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN.
             05  LINE PLUS 1 COL PLUS  1 VALUE "�`�a�b"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN.
             05  LINE PLUS 2 COL MINUS 2 VALUE "�`�a�b"
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN.

           03  SCR05-AREA.
             05  USING WK-DEG2
                 LINE L COL C
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
                 PIC 9(003).
             05  USING WK-KANMA
                 LINE L COL MINUS 2
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
      *    *** SPACE ������ƕ\�����|�ɂȂ�
      *          PIC X(002).
                 PIC X(001).
             05  USING WK-RAD2
                 LINE L COL MINUS 1
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
                 PIC X(005).
             05  USING WK-KANMA
                 LINE L COL MINUS 2
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
                 PIC X(001).
             05  USING WK-SIN3
                 LINE L COL MINUS 1
                 BACKGROUND-COLOR WHITE
                 FOREGROUND-COLOR CYAN
                 PIC X(005).

           03  SCR06-AREA.
             05  USING WK-DATA1
      *           LINE 9 COL 10
                 LINE 10 COL 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  USING WK-DATA2
                 LINE 10 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
      *           SIGN LEADING
                 PIC  X(005).
             05  USING WK-DATA3
                 LINE 10 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  FROM WK-DATA4
                 LINE 10 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(005).
             05  FROM WK-DATA5
                 LINE 10 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).
             05  FROM WK-DATA6
                 LINE 10 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).

           03  SCR07-AREA.
             05  LINE 10 COL 9
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  USING WK-DATA1
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  USING WK-DATA2
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  USING WK-DATA3
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  FROM WK-DATA4
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(005).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  FROM WK-DATA5
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".
             05  FROM WK-DATA6
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).
             05  LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "|".

           03  SCR08-AREA.
             05  LINE 10 COL 8
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  USING WK-DATA1
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  USING WK-DATA2
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  USING WK-DATA3
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 AUTO
                 PIC  X(005).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  FROM WK-DATA4
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(005).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  FROM WK-DATA5
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".
             05  FROM WK-DATA6
                 LINE 10 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC  X(011).
             05  LINE 10
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".

           03  SCR08-1-AREA.
             05  USING WK-DATA1
                 LINE 10 COL  11
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
      *           AUTO
                 PIC  X(005).

           03  SCR08-2-AREA.
             05  USING WK-DATA2
                 LINE 10 COL  19
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
      *           AUTO
                 PIC  S9(03)V99.

           03  SCR08-3-AREA.
             05  SCR08-3-DATA3
                 USING WK-DATA3
      *           LINE 10 COL  27
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
      *           AUTO
                 PIC  9(03)V99.

           03  SCR10-AREA
      *    *** �W�c���ڂŐF�w��o����
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.
                 
             05  LINE WK-LINE COL 8
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
                 VALUE "��".
      *    *** FROM �͏o�͍��ڂƂ��Ē�`����
             05  FROM WFD-SUN
      *           LINE WK-LINE COL PLUS 1
      *           LINE WK-LINE COL PLUS 1
                 COL PLUS 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
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
                 VALUE "�������@�J�����_�[�@������".

           03  SCR10-2-AREA.
             05  LINE 2 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�N�@����".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-YYYY
                 LINE 2 COL  12
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  9(004).
      *       05  LINE 2 COL 18
      *           BACKGROUND-COLOR CYAN
      *           FOREGROUND-COLOR WHITE
      *           VALUE "1582�N10��15��(�O���S���I��)�ȍ~�A�Z�o".

             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "���@����".

             05  USING WK-MM
                 LINE 3 COL  12
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                       PIC  9(002).

      *    *** PIC X(NNN) �L�����Ȃ��ƕ\������Ȃ�
             05  FROM WK-ERR-COM
                 LINE 3 COL 20
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC X(060).

           03  SCR10-3-AREA.
             05  USING WDW-DATE2-YYYY
                 LINE 4 COL 40
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                       PIC  9(004).
             05  LINE 4 COL 44
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�N".

             05  USING WDW-DATE2-MM
                 LINE 4 COL 47
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                       PIC  9(002).

             05  LINE 4 COL 49
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "��".

           03  SCR10-4-AREA
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE.

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

           03  SCR10-5-AREA.
             05  LINE 22 COL 8
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�o�e�O�V�F�O���@�o�e�O�W�F����".
             05  LINE 22 COL 40
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�o�e�O�X�F�O�N�@�o�e�P�O�F���N".

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
                 FOREGROUND-COLOR WHITE
                 VALUE "�������@�n�C���[�Q�[���@������".

           03  SCR11-2-AREA.
             05  LINE 3 COL 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�����P�O���ȓ��œ���".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WFD-SUXX
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
                 BLANK WHEN ZERO
                 AUTO
                       PIC  9(012).
    
      *    *** PIC X(NNN) �L�����Ȃ��ƕ\������Ȃ�
             05  FROM WK-ERR-COM
                 LINE 3 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 PIC X(040).

           03  SCR12-1-AREA.

             05  LINE 1 COL 30
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�������@�o�i�[�@������".

           03  SCR12-2-AREA.

             05  LINE 2 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�o�̓p�^�[��(01-10�j".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-PTN
                 LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  X(002).

             05  LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�\�������i01-99�j".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-SOKUDO
                 LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  X(002).

             05  LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�F�i0-7�j".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-COLOR
                 LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *           BLANK WHEN ZERO
                 AUTO
                       PIC  X(001).

             05  LINE 2 COL PLUS 2
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 VALUE "�W�O�����ȓ�".

      *    *** USING�͓��́A�o�͗����Ɏg����@�ŏ��ɉ������\������Ă�
             05  USING WK-MOJI
                 LINE 3 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR WHITE
                 UNDERLINE
      *          BLANK WHEN ZERO
                 AUTO
                       PIC  X(080).

           03  SCR12-3-AREA.
      *    *** 
             05  FROM WK-DATA7 (01)
                 LINE 05 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                     PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (02)
                 LINE 06 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (03)
                 LINE 07 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (04)
                 LINE 08 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (05)
                 LINE 09 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (06)
                 LINE 10 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (07)
                 LINE 11 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (08)
                 LINE 12 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (09)
                 LINE 13 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (10)
                 LINE 14 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (11)
                 LINE 15 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (12)
                 LINE 16 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (13)
                 LINE 17 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (14)
                 LINE 18 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (15)
                 LINE 19 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (16)
                 LINE 20 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA8
                 LINE 22 COL 1
                 BACKGROUND-COLOR CYAN
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

           03  SCR13-3-AREA.
      *    *** 
             05  FROM WK-DATA7 (01)
                 LINE 05 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                     PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (02)
                 LINE 06 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (03)
                 LINE 07 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (04)
                 LINE 08 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (05)
                 LINE 09 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (06)
                 LINE 10 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (07)
                 LINE 11 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (08)
                 LINE 12 COL 1
                 BACKGROUND-COLOR BLUE
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (09)
                 LINE 13 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (10)
                 LINE 14 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (11)
                 LINE 15 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (12)
                 LINE 16 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (13)
                 LINE 17 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (14)
                 LINE 18 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (15)
                 LINE 19 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA7 (16)
                 LINE 20 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

      *    *** 
             05  FROM WK-DATA8
                 LINE 22 COL 1
                 BACKGROUND-COLOR YELLOW
                 FOREGROUND-COLOR C
                 HIGHLIGHT
                 PIC X(080).

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** �O�O�D�������j���[
           PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN1
           PERFORM S100-10     THRU    S100-EX

           PERFORM UNTIL WK-TESTNO = 99
                   EVALUATE TRUE

                       WHEN WK-TESTNO = 0
      *    *** �O�O�D�������j���[
                            PERFORM S020-10     THRU    S020-EX

                       WHEN WK-TESTNO = 1
      *    *** �O�P�D��ʐF�ݒ�@�O�`�V�Ŏw��@�e�@�a�@�k�^�g.
                            PERFORM S210-10     THRU    S210-EX

                       WHEN WK-TESTNO = 2
      *    *** �O�Q�D�����_�������\���@���w��
                            PERFORM S220-10     THRU    S220-EX

                       WHEN WK-TESTNO = 3
      *    *** �O�R�D�e�n�q�d�C�a�`�b�j�f�q�n�t�m�c�T���v��
                            PERFORM S230-10     THRU    S230-EX

                       WHEN WK-TESTNO = 4
      *    *** �O�S�D�k�h�m�d�C�b�n�k�t�l�m�@�o�k�t�r�C�l�h�m�t�r
                            PERFORM S240-10     THRU    S240-EX

                       WHEN WK-TESTNO = 5
      *     *** �O�T�D�c�d�f�x�C�q�`�f���W�A���C�r�h�m�i�q�`�f�j�l
                            PERFORM S250-10     THRU    S250-EX

                       WHEN WK-TESTNO = 6
      *    *** �O�U�D�������ړ��́A�r���\���P
                            PERFORM S260-10     THRU    S260-EX

                       WHEN WK-TESTNO = 7
      *    *** �O�V�D�������ړ��́A�r���\���Q
                            PERFORM S270-10     THRU    S270-EX

                       WHEN WK-TESTNO = 8 
      *    *** �O�W�D�������ړ��́A�r���\���R
                            PERFORM S280-10     THRU    S280-EX

                       WHEN WK-TESTNO = 9 AND 
                            WK-PIN1-EOF NOT = HIGH-VALUE
      *    *** �O�X�D�t�@�C�����e�\��
                            PERFORM S290-10     THRU    S290-EX
      *    *** READ PIN1
                            PERFORM S100-10     THRU    S100-EX

                       WHEN WK-TESTNO = 10
      *    *** �P�O�D�J�����_�[�\��
                            PERFORM S300-10     THRU    S300-EX

                       WHEN WK-TESTNO = 11
      *    *** �P�P�D�n�C���[�Q�[��
                            PERFORM S310-10     THRU    S310-EX

                       WHEN WK-TESTNO = 12
      *    *** �P�Q�D�o�i�[
                            PERFORM S320-10     THRU    S320-EX

                       WHEN WK-TESTNO = 13
      *    *** �P�R�D�o�i�[�i�E�N���C�i�j
                            PERFORM S330-10     THRU    S330-EX
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

           MOVE    "OPEN  "    TO      WDE07-ID
           CALL    "DECODE07" USING    WDE07-DECODE07-AREA
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
           END-DISPLAY
           DISPLAY WK-MID1
                   AT LINE 2 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-HAI
                   AT LINE 3 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I00
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I01
                   AT LINE 5 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I02
                   AT LINE 6 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I03
                   AT LINE 7 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I04
                   AT LINE 8 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I05
                   AT LINE 9 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I06
                   AT LINE 10 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I07
                   AT LINE 11 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I08
                   AT LINE 12 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I09
                   AT LINE 13 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I10
                   AT LINE 14 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I11
                   AT LINE 15 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I12
                   AT LINE 16 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I13
                   AT LINE 17 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-MEI1-I99
                   AT LINE 23 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    SPACE       TO      WK-ITEM3
                                       WK-ITEM4
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
                   ADD     1           TO        WK-PIN1-CNT
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
                   EXIT    PARAGRAPH
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
                         MOVE   WK-PIN1-I4-R (I:2) TO   WK-PIN1-I4(J:2)
                         ADD    2    TO     K
      *    *** J 1,3,5...
      *    *** J 81.83,85...
                         ADD    1    TO     I
                         ADD    2    TO     J
                   ELSE
                         MOVE   WK-PIN1-I4-R (I:1) TO   WK-PIN1-I4(J:1)
                         ADD    1    TO     K
      *    *** J 1,2,3...
      *    *** J 81,82,83...
      *    *** J 161.162,163...
                         ADD    1    TO     J
                   END-IF
                   
      *             IF    K    >   73
                   IF    K    >   77
                         ADD   80    TO     J2
      *    *** J 81,161...
                         ADD   J2 1  GIVING J
                         MOVE  ZERO  TO     K
                   END-IF

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     ADD     1           TO      WFD-SEQ
      *     MOVE    800         TO      WFD-LEN
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-PIN1-I4
      *                                 WFD-LEN
           END-PERFORM.
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

      *    *** �O�P�D��ʐF�ݒ�@�O�`�V�Ŏw��@�e�@�a�@�k�^�g.
       S210-10.

      *    *** ���͂Ȃ����̏����l
           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       25
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           IF      COB-CRT-STATUS =    1007 OR 1008 OR 1009 OR 1010
                   EVALUATE TRUE
                      WHEN COB-CRT-STATUS = 1007
                           IF      WK-ITEM1    =       ZERO
                               MOVE    7           TO      WK-ITEM1
                           ELSE
                               ADD     -1          TO      WK-ITEM1
                           END-IF
                      WHEN COB-CRT-STATUS = 1008
                           ADD     +1          TO      WK-ITEM1
                           IF      WK-ITEM1    >=      8
                               MOVE    0           TO      WK-ITEM1
                           END-IF
                      WHEN COB-CRT-STATUS = 1009
                           IF      WK-ITEM2    =       ZERO
                               MOVE    7           TO      WK-ITEM2
                           ELSE
                               ADD     -1          TO      WK-ITEM2
                           END-IF
                      WHEN COB-CRT-STATUS = 1010
                           ADD     +1          TO      WK-ITEM2
                           IF      WK-ITEM2    >=      8
                               MOVE    0           TO      WK-ITEM2
                           END-IF
                   END-EVALUATE
           ELSE
                   CONTINUE
           END-IF

           PERFORM VARYING L FROM 5 BY 1
                   UNTIL   L   >       23
                   DISPLAY WK-SPACE2
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WK-ITEM1
                                FOREGROUND-COLOR WK-ITEM2
                                LOWLIGHT
                   END-DISPLAY

                   DISPLAY WK-SPACE2
                           AT LINE L COL 41
                           WITH BACKGROUND-COLOR WK-ITEM1
                                FOREGROUND-COLOR WK-ITEM2
                                HIGHLIGHT
                   END-DISPLAY
           END-PERFORM



           DISPLAY "LOWLIGHT"
                   AT LINE 2 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY "B-COLOR="
                   AT LINE 3 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY WK-ITEM1
                   AT LINE 3 COL 9
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           EVALUATE TRUE
               WHEN WK-ITEM1 = 0
                   DISPLAY COLOR-0L
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 1
                   DISPLAY COLOR-1
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 2
                   DISPLAY COLOR-2
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 3
                   DISPLAY COLOR-3
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 4
                   DISPLAY COLOR-4
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 5
                   DISPLAY COLOR-5
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 6
                   DISPLAY COLOR-6L
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 7
                   DISPLAY COLOR-7L
                           AT LINE 3 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
           END-EVALUATE

           DISPLAY "F-COLOR="
                   AT LINE 4 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY WK-ITEM2
                   AT LINE 4 COL 9
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           EVALUATE TRUE
               WHEN WK-ITEM2 = 0
                   DISPLAY COLOR-0L
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 1
                   DISPLAY COLOR-1
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 2
                   DISPLAY COLOR-2
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 3
                   DISPLAY COLOR-3
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 4
                   DISPLAY COLOR-4
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 5
                   DISPLAY COLOR-5
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 6
                   DISPLAY COLOR-6L
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 7
                   DISPLAY COLOR-7L
                           AT LINE 4 COL 12
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
           END-EVALUATE



           DISPLAY "HIGHLIGHT"
                   AT LINE 2 COL 41
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY "B-COLOR="
                   AT LINE 3 COL 41
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY WK-ITEM1
                   AT LINE 3 COL 49
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           EVALUATE TRUE
               WHEN WK-ITEM1 = 0
                   DISPLAY COLOR-0L
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 1
                   DISPLAY COLOR-1
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 2
                   DISPLAY COLOR-2
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 3
                   DISPLAY COLOR-3
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 4
                   DISPLAY COLOR-4
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 5
                   DISPLAY COLOR-5
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 6
                   DISPLAY COLOR-6L
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM1 = 7
                   DISPLAY COLOR-7L
                           AT LINE 3 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
           END-EVALUATE

           DISPLAY "F-COLOR="
                   AT LINE 4 COL 41
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           DISPLAY WK-ITEM2
                   AT LINE 4 COL 49
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY

           EVALUATE TRUE
               WHEN WK-ITEM2 = 0
                   DISPLAY COLOR-0H
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 1
                   DISPLAY COLOR-1
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 2
                   DISPLAY COLOR-2
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 3
                   DISPLAY COLOR-3
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 4
                   DISPLAY COLOR-4
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 5
                   DISPLAY COLOR-5
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 6
                   DISPLAY COLOR-6H
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
               WHEN WK-ITEM2 = 7
                   DISPLAY COLOR-7H
                           AT LINE 4 COL 52
                           WITH BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                                HIGHLIGHT
                   END-DISPLAY
           END-EVALUATE

           DISPLAY 
           "PF07:B-COLOR=-,PF08:B-COLOR=+,PF09:F-COLOR=-,PF10:F-COLOR=+"
                   AT LINE 24 COL 1
                   WITH BACKGROUND-COLOR CYAN
                        FOREGROUND-COLOR WHITE
                        HIGHLIGHT
           END-DISPLAY
           .
       S210-EX.
           EXIT.

      *    *** �O�Q�D�����_�������\���@���w��
       S220-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           MOVE    4           TO      WCR-IDX
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

           MOVE    99          TO      WK-ITEM1
           MOVE    "RND"       TO      WCR-ID
           PERFORM VARYING C2 FROM 1 BY 1 
                   UNTIL C2 > WK-ITEM1
      *             MOVE FUNCTION RANDOM TO WK-RAN
      *             COMPUTE L ROUNDED = WK-RAN * 24
                   CALL    "COBRND"    USING   WCR-COBRND-AREA

                   COMPUTE L ROUNDED = WCR-RND(1) * 24
                   IF L = ZERO
                      MOVE 1 TO L
                   END-IF

      *             MOVE FUNCTION RANDOM TO WK-RAN
      *             COMPUTE C ROUNDED = WK-RAN * 80

                   COMPUTE C ROUNDED = WCR-RND(2) * 80
                   IF C = ZERO
                      MOVE 1 TO C
                   END-IF

      *             MOVE FUNCTION RANDOM TO WK-RAN
      *             COMPUTE C3 ROUNDED = WK-RAN * 7
                   COMPUTE C3 ROUNDED = WCR-RND(3) * 7
                   IF C3 = ZERO
                      MOVE 1 TO C3
                   END-IF

      *             MOVE FUNCTION RANDOM TO WK-RAN
      *             COMPUTE C4 ROUNDED = WK-RAN * 26
      *    *** * 26 �͉p����ω�������
                   COMPUTE C4 ROUNDED = WCR-RND(4) * 26
                   IF C4 = ZERO
                      MOVE 1 TO C4
                   END-IF

                   DISPLAY WK-ALPHA3 (C4)
                           AT LINE L COL C
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR C3
                   END-DISPLAY
      *             CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
      *             CALL 'C$SLEEP' USING 1

           END-PERFORM
           .
       S220-EX.
           EXIT.

      *    *** �O�R�D�e�n�q�d�C�a�`�b�j�f�q�n�t�m�c�T���v��
       S230-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           DISPLAY "LOWLIGHT"
                   AT LINE 2
                   COL 1
                   WITH 
                   BACKGROUND-COLOR WHITE
                   FOREGROUND-COLOR CYAN
           END-DISPLAY

      *    *** LOWLIGHT
           PERFORM VARYING B FROM 0 BY 1 UNTIL B > 7
               PERFORM VARYING F FROM  0 BY 1 UNTIL F > 7
                   COMPUTE L = B + 3
                   COMPUTE C = F * 9 + 2
                   DISPLAY " COLOUR " AT LINE L COL C
                           WITH BACKGROUND-COLOR B
                                FOREGROUND-COLOR F
                                LOWLIGHT
                   END-DISPLAY
               END-PERFORM
           END-PERFORM

           DISPLAY "HIGHLIGHT"
                   AT LINE 14 COL 1
                   WITH 
                   BACKGROUND-COLOR WHITE
                   FOREGROUND-COLOR CYAN
           END-DISPLAY

      *    *** HIGHLIGHT
           PERFORM VARYING B FROM 0 BY 1 UNTIL B > 7
               PERFORM VARYING F FROM  0 BY 1 UNTIL F > 7
                   COMPUTE L = B + 15
                   COMPUTE C = F * 9 + 2
                   DISPLAY " COLOUR " AT LINE L COL C
                           WITH BACKGROUND-COLOR B
                                FOREGROUND-COLOR F
                                HIGHLIGHT
                   END-DISPLAY
               END-PERFORM
           END-PERFORM

           .
       S230-EX.
           EXIT.

      *    *** �O�S�D�k�h�m�d�C�b�n�k�t�l�m�@�o�k�t�r�C�l�h�m�t�r
       S240-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           DISPLAY SCR04-AREA 
                   WITH BACKGROUND-COLOR WHITE
                        FOREGROUND-COLOR BLACK
           END-DISPLAY
           .
       S240-EX.
           EXIT.

      *     *** �O�T�D�c�d�f�x�C�q�`�f���W�A���C�r�h�m�i�q�`�f�j�l
       S250-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           MOVE    FUNCTION PI TO WK-PI
           MOVE    0         TO   L
           MOVE    1         TO   C
           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > 72
                   COMPUTE WK-DEG WK-DEG2 = I * 5
                   COMPUTE WK-RAD WK-RAD2 = WK-DEG * WK-PI / 180
                   COMPUTE WK-SIN3 ROUNDED = 
                           FUNCTION SIN(WK-RAD)

                   ADD  1     TO    L

               IF SW-DISPLAY = "0"
      *    *** SCR04-AREA�̕\��
      *    *** DISPLAY SCRXX-�Ŏw�肷��ƁA�f�[�^�傪�D�悳���
      *    *** ���L�@�v�h�s�g��͖��������
                   DISPLAY SCR05-AREA
      *    *** DISPLAY �ꍀ�ڂ����\���o���Ȃ��A
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR BLACK
                   END-DISPLAY
               ELSE
                   DISPLAY SCR05-AREA
      *    *** DISPLAY �ꍀ�ڂ����\���o���Ȃ��A
      *    *** SCR04-AREA�@�ł͂��܂��\���o���Ȃ�
                   DISPLAY WK-DEG2
                           AT LINE L COL C
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR CYAN
      *    *** BLINK�͂a�`�b�j�����ɂȂ邾��
                                BLINK
                   END-DISPLAY
                   COMPUTE C2 = C + 3
                   DISPLAY ", "
                           AT LINE L COL C2
                           
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR CYAN
                                REVERSE-VIDEO
                   END-DISPLAY
      *    *** REVERSE-VIDEO ���]�ɂȂ�
                   COMPUTE C2 = C2 + 2
                   DISPLAY WK-RAD2
                           AT LINE L COL C2
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR CYAN
                   END-DISPLAY
                   COMPUTE C2 = C2 + 5
                   DISPLAY ", "
                           AT LINE L COL C2
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR CYAN
                   END-DISPLAY
                   COMPUTE C2 = C2 + 2
                   DISPLAY WK-SIN3
                           AT LINE L COL C2
                           WITH BACKGROUND-COLOR WHITE
                                FOREGROUND-COLOR CYAN
                   END-DISPLAY
               END-IF

               IF   I     =    24 OR 48 
                        MOVE  0  TO  L
                        ADD   20 TO  C
               END-IF

      *            CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC
           END-PERFORM
           .
       S250-EX.
           EXIT.

      *    *** �O�U�D�������ړ��́A�r���\���P
       S260-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

      *    *************************************************************
      *    *** CALL    "CBL_OC_KEISEN" USING  �͑S�R�g���Ȃ��I�I       *
      *    *************************************************************
      *    *** CALL     �gCBL_OC_KEISEN�h  USING  
      *    ***  KEI-CMD,�i�r���̎�ށj
      *    ***  KEI-LINE, �i�J�n�s�j
      *    ***  KEI-COL, �i�J�n���j
      *    ***  KEI-LNG1, �i�����P�j
      *    ***  KEI-LNG2, �i�����Q�j
      *    ***  KEI-COLOR, �i�r���F���j
      *    ***  KEI-PRN. �i�\���I�v�V�������j
      *    ***  �����Ή�
      *
      *    *** CBL_OC_KEISEN �g�p���@�s��
      *    *** CMD=1 ����
      *    *** CMD=2 ���
      *    *** CMD=3 ������
      *    *** CMD=4 ������
      *    *** CMD=5 �l�p
      *    *** CMD=0�C6 �P�Ƃł͏o�Ȃ�

      *    *** CMD=5 �l�p
           MOVE    5           TO      KEI-CMD
           MOVE    9           TO      KEI-LINE
           MOVE    9           TO      KEI-COL
           MOVE    54          TO      KEI-LEN1
           MOVE    3           TO      KEI-LEN2
           MOVE    2           TO      KEI-COLOR
           MOVE    2           TO      KEI-PRN
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

      *    *** ���͍��ڂ��r���Əd�Ȃ�Ə����Ă��܂�
      *    *** CMD=1 ����
           MOVE    1           TO      KEI-CMD
      *     MOVE    10          TO      KEI-LINE
           MOVE    11          TO      KEI-LINE
           MOVE    09          TO      KEI-COL
           MOVE    52          TO      KEI-LEN1
      *     CALL    "CBL_OC_KEISEN" USING
      *             KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
      *             KEI-COLOR KEI-PRN

      *    *** CMD=2 ���
           MOVE    2           TO      KEI-CMD
      *     MOVE    10          TO      KEI-LINE
           MOVE    09          TO      KEI-LINE
           MOVE    09          TO      KEI-COL
           MOVE    52          TO      KEI-LEN1
      *     CALL    "CBL_OC_KEISEN" USING
      *             KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
      *             KEI-COLOR KEI-PRN
      *
      *    *** CMD=4 ������
           MOVE    4           TO      KEI-CMD
           MOVE    09          TO      KEI-LINE
           MOVE    09          TO      KEI-COL
      *     MOVE    9           TO      KEI-LINE
      *     MOVE    9          TO      KEI-COL
           MOVE    1           TO      KEI-LEN1
      *     CALL    "CBL_OC_KEISEN" USING
      *             KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
      *             KEI-COLOR KEI-PRN

           MOVE    4           TO      KEI-CMD
           MOVE    9           TO      KEI-LINE
           MOVE    15          TO      KEI-COL
           MOVE    3           TO      KEI-LEN1
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           MOVE    4           TO      KEI-CMD
           MOVE    9           TO      KEI-LINE
           MOVE    22          TO      KEI-COL
           MOVE    3           TO      KEI-LEN1
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           MOVE    4           TO      KEI-CMD
           MOVE    9           TO      KEI-LINE
           MOVE    29          TO      KEI-COL
           MOVE    3           TO      KEI-LEN1
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           MOVE    4           TO      KEI-CMD
           MOVE    9           TO      KEI-LINE
           MOVE    36         TO      KEI-COL
           MOVE    3           TO      KEI-LEN1
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           MOVE    4           TO      KEI-CMD
           MOVE    9          TO      KEI-LINE
           MOVE    49          TO      KEI-COL
           MOVE    3           TO      KEI-LEN1
           CALL    "CBL_OC_KEISEN" USING
                   KEI-CMD KEI-LINE KEI-COL KEI-LEN1 KEI-LEN2
                   KEI-COLOR KEI-PRN

           MOVE    SPACE       TO      WK-DATA1
                                       WK-DATA4
                                       WK-DATA2-R1
                                       WK-DATA3-R1
                                       WK-DATA5-R1
                                       WK-DATA6-R1
      *     MOVE    ZERO        TO      WK-DATA2
      *                                 WK-DATA3
      *                                 WK-DATA5
      *                                 WK-DATA6

           ACCEPT  SCR06-AREA
      *     ACCEPT  WK-DATA1 AT LINE 12 COL 10,

      *    MOVE    WK-DATA3    TO      WK-DATA3-R1
           MOVE    WK-DATA1    TO      WK-DATA4
           MOVE    WK-DATA2    TO      WK-DATA5
           COMPUTE WK-DATA6 = WK-DATA3 * -1
      *     MOVE    WK-DATA3    TO      WK-DATA6

           DISPLAY SCR06-AREA
           .
       S260-EX.
           EXIT.

      *    *** �O�V�D�������ړ��́A�r���\���Q
       S270-10.

           PERFORM VARYING L FROM 1 BY 1
                   UNTIL   L   >       24
                   DISPLAY WK-SPACE80
                           AT LINE L COL 1
                           WITH 
                                BACKGROUND-COLOR CYAN
                                FOREGROUND-COLOR WHITE
                   END-DISPLAY
           END-PERFORM

           DISPLAY WK-HAI1
                   AT LINE 9 COL 9
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI1
                   AT LINE 11 COL 9
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    SPACE       TO      WK-DATA1
                                       WK-DATA4
                                       WK-DATA2-R1
                                       WK-DATA3-R1
                                       WK-DATA5-R1
                                       WK-DATA6-R1
      *     MOVE    ZERO        TO      WK-DATA2
      *                                 WK-DATA3
      *                                 WK-DATA5
      *                                 WK-DATA6
           ACCEPT  SCR07-AREA
      *     ACCEPT  WK-DATA1 AT LINE 12 COL 10,

      *     MOVE    WK-DATA3    TO      WK-DATA3-R1
           MOVE    WK-DATA1    TO      WK-DATA4
           MOVE    WK-DATA2    TO      WK-DATA5
      *     MOVE    WK-DATA3    TO      WK-DATA6
           COMPUTE WK-DATA6 = WK-DATA3 * -1

           DISPLAY SCR07-AREA
           .
       S270-EX.
           EXIT.

      *    *** �O�W�D�������ړ��́A�r���\���R
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

           DISPLAY WK-HAI2
                   AT LINE 9 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-HAI3
                   AT LINE 11 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    SPACE       TO      WK-DATA1
                                       WK-DATA4
                                       WK-DATA2-R1
                                       WK-DATA3-R1
                                       WK-DATA5-R1
                                       WK-DATA6-R1
      *     MOVE    ZERO        TO      WK-DATA2
      *                                 WK-DATA3
      *                                 WK-DATA5
      *                                 WK-DATA6

           DISPLAY SCR08-AREA
      *     ACCEPT  SCR08-AREA

      *     ACCEPT  WK-DATA1 AT LINE 12 COL 10,

      *    *** �o�h�b�@�w�i�m�m�m�j�ŁA
      *    *** 
      *    *** �������ڂ����͏o���邪�A���ڂ̕ϊ��œ��e�����Ă��܂��A
      *    *** �Q��ڂœ��͂����
           ACCEPT  SCR08-1-AREA

      *    *** �������ځ@�|�i�}�C�i�X�j�A�D�i�R���}�j���͏o���Ȃ�
      *    *** �����������͏o���Ȃ�
           ACCEPT  SCR08-2-AREA

      *     ACCEPT  SCR08-3-DATA3
      *    *** ACCEPT�Ł@LINE,COL�w�肵�Ă��A
      *    *** ��{���ڂł��_���ALINE=1,COL=1�œ��͂����Ă��܂�
      *            AT LINE 10 COL  27
      *    *** LINE,COL�@�w�莞�́ASCREEN SECTION�̍��ڂł́A������
      *    *** �ʒu�ɓ��͂����Ȃ��@WORK SECTION�̍��ڂȂ�n�j
           ACCEPT  WK-DATA3
                   AT LINE 10 COL  27
                   WITH 
      *    *** WORK���ړ��͎��A�b�n�k�n�q�w�肵�Ȃ��ƁA�����l�ɖ߂��Ă��܂�
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-ACCEPT

      *     MOVE    WK-DATA3    TO      WK-DATA3-R1
           MOVE    WK-DATA1    TO      WK-DATA4
           MOVE    WK-DATA2    TO      WK-DATA5
      *     MOVE    WK-DATA3    TO      WK-DATA6
           COMPUTE WK-DATA6 = WK-DATA3 * -1

      *    *** PIC X(NNN) => -NNNN �œ��͌�ANUMVAL�Ő����ϊ��o����
           MOVE    FUNCTION NUMVAL (WK-DATA1) TO WK-DATA6

           DISPLAY SCR08-AREA
           .
       S280-EX.
           EXIT.

      *    *** �O�X�D�t�@�C�����e�\��
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

           DISPLAY WK-PIN1-I1
                   AT LINE 1 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I2
                   AT LINE 2 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I3
                   AT LINE 4 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY WK-PIN1-I4-01
                   AT LINE 6 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-02
                   AT LINE 7 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-03
                   AT LINE 8  COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-04
                   AT LINE 9  COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-05
                   AT LINE 10 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-06
                   AT LINE 11 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-07
                   AT LINE 12 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-08
                   AT LINE 13 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-09
                   AT LINE 14 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-10
                   AT LINE 15 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-11
                   AT LINE 16 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-12
                   AT LINE 17 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-13
                   AT LINE 18 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-14
                   AT LINE 19 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-15
                   AT LINE 20 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-16
                   AT LINE 21 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-17
                   AT LINE 22 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-18
                   AT LINE 23 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           DISPLAY WK-PIN1-I4-19
                   AT LINE 24 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
      *     DISPLAY WK-PIN1-I4-20
      *             AT LINE 25 COL 1
      *             WITH 
      *             BACKGROUND-COLOR CYAN
      *             FOREGROUND-COLOR WHITE
      *     END-DISPLAY
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
                                   IF      WK-YYYY     =       9999
                                     ADD     ZERO        TO      WK-YYYY
                                   ELSE
                                     ADD     +1          TO      WK-YYYY
                                   END-IF
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
                           IF      WK-YYYY     =       9999
                                   MOVE    ZERO        TO      WK-YYYY
                           ELSE
                                   ADD     +1          TO      WK-YYYY
                           END-IF
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
           DISPLAY SCR10-3-AREA

           MOVE    WDW-DATE2-YYYY TO    WK-YYYY
           MOVE    WDW-DATE2-MM   TO    WK-MM

           DISPLAY WK-HAI4
                   AT LINE 5 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    6           TO      WK-LINE
           MOVE    "��"        TO      WFD-SUN
           MOVE    "��"        TO      WK-MON
           MOVE    "��"        TO      WK-TUE
           MOVE    "��"        TO      WK-WED
           MOVE    "��"        TO      WK-THU
           MOVE    "��"        TO      WK-FRI
           MOVE    "�y"        TO      WK-SAT

      *    *** �j���Z�b�g
           DISPLAY SCR10-AREA

      *    *** �o�e�w�w�@�O���A�����@�Z�b�g
           DISPLAY SCR10-5-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

           DISPLAY WK-HAI6
                   AT LINE 19 COL 8
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY



      *    *** �N�A���@����

           MOVE    SPACE       TO      WK-ERR-COM
           ACCEPT  SCR10-2-AREA

           IF      COB-CRT-STATUS =    1007 OR 1008 OR 1009 OR 1010
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
                                   IF      WK-YYYY     =       9999
                                     MOVE    ZERO        TO      WK-YYYY
                                   ELSE
                                     ADD     +1          TO      WK-YYYY
                                   END-IF
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
                           IF      WK-YYYY     =       9999
                                   MOVE    ZERO        TO      WK-YYYY
                           ELSE
                                   ADD     +1          TO      WK-YYYY
                           END-IF
                   END-EVALUATE
                   GO  TO  S300-20
           END-IF

           PERFORM TEST AFTER 
                   UNTIL SW-ERROR = "0"

                   IF      WK-YYYY     IS      NUMERIC AND
      *                     WK-YYYY     >=      1582    AND
                           WK-MM       IS      NUMERIC AND
                           WK-MM       >=      1       AND
                           WK-MM       <=      12
                           MOVE    "0"         TO      SW-ERROR
                           MOVE    SPACE       TO      WK-ERR-COM
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
      *                     MOVE  "�N��1582�ȍ~�A����1����12�͈̔͂Ŏw��"
                           MOVE  "�N�͐����A����1����12�͈̔͂Ŏw��"
                                               TO      WK-ERR-COM
                           DISPLAY SCR10-2-AREA
                           ACCEPT  SCR10-2-AREA
                   END-IF
           END-PERFORM
           .

      *    *** PF07,08,09,10 ���͂̎��A�N�A���@���̓X�L�b�v����
       S300-20.

           MOVE    WK-YYYY     TO      WDW-DATE2-YYYY
           MOVE    WK-MM       TO      WDW-DATE2-MM
      *    *** WK-YYYY,WK-MM �ύX�����N���̃Z�b�g
           DISPLAY SCR10-2-AREA
      *    *** ���͂����N���̃Z�b�g
           DISPLAY SCR10-3-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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
           DISPLAY  SCR10-4-AREA

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

      *    *** ���o���ASEQ COMPUTER-NUM
           DISPLAY WK-MID2
                   AT LINE 5 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
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
           MOVE    ZERO        TO      WFD-SUXX
           
           ACCEPT  SCR11-2-AREA

           PERFORM TEST AFTER 
                   UNTIL SW-ERROR = "0"

                   IF      WFD-SUXX     IS      NUMERIC AND
                           WFD-SUXX     >=      1       AND
      *    *** �����@�P�O���ȓ���
                           WFD-SUXX     <      10000000000
                           MOVE    "0"         TO      SW-ERROR
                           MOVE    SPACE       TO      WK-ERR-COM
                   ELSE
                           MOVE    "1"         TO      SW-ERROR
                           MOVE
                             "���͐����A1����9,999,999,999�͈̔͂Ŏw��"
                                               TO      WK-ERR-COM
                           MOVE    ZERO        TO      WFD-SUXX
                           DISPLAY SCR11-2-AREA
                           ACCEPT  SCR11-2-AREA
                   END-IF
           END-PERFORM
           

      *    *** HILOWGCBL ���R�s�[
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
                       WHEN WK-CHK-NUM = WFD-SUXX
                            MOVE    9999        TO      WK-ACT-NUM
                       WHEN WK-CHK-NUM > WFD-SUXX
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
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 9
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
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
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 31
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
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
                            END-DISPLAY
                            DISPLAY WK-CHK-SU
                                    AT LINE L COL 53
                                    WITH 
                                    BACKGROUND-COLOR CYAN
                                    FOREGROUND-COLOR WHITE
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
           END-DISPLAY

           MOVE    WFD-SUXX     TO      WK-DSP-NUM
           DISPLAY WK-DSP-NUM
                   AT LINE 23 COL 21
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY "�ł���"
                   AT LINE 23 COL 35
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY "�R���s���[�^�[��"
                   AT LINE 24 COL 1
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           MOVE    WK-CNT      TO      WK-DSP-NUM
           DISPLAY WK-DSP-NUM
                   AT LINE 24 COL 21
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY

           DISPLAY "��łg�h�s���܂���"
                   AT LINE 24 COL 35
                   WITH 
                   BACKGROUND-COLOR CYAN
                   FOREGROUND-COLOR WHITE
           END-DISPLAY
           .
       S310-EX.
           EXIT.

      *    *** �P�Q�D�o�i�[
       S320-10.

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM

      *    *** �������@�o�i�[�@�������@�̃Z�b�g
           MOVE    "0:�D 1:�� 2:�� 3:���"
                               TO      WK-ITEM3
           MOVE    "4:�� 5:�� 6:�� 7:��"
                               TO      WK-ITEM4
           DISPLAY SCR00-AREA
           DISPLAY SCR12-1-AREA

      *    *** �����@����
           MOVE    SPACE       TO      WK-ERR-COM
           MOVE    SPACE       TO      WK-PTN
           MOVE    SPACE       TO      WK-SOKUDO
           MOVE    SPACE       TO      WK-COLOR
           MOVE    SPACE       TO      WK-MOJI

           ACCEPT  SCR12-2-AREA

           IF      WK-PTN      NOT NUMERIC
                   MOVE    1           TO      WK-PTN
           END-IF
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    WK-MOJI     TO      WDE07-ASCII
           MOVE    WK-PTN      TO      WDE07-PTN
           IF      WK-SOKUDO   >=      "01"
               AND WK-SOKUDO   <=      "99"
                   COMPUTE WK-NANOSEC01 = NUMVAL (WK-SOKUDO) * 10000000
           ELSE
                   COMPUTE WK-NANOSEC01 = 500000000
           END-IF

      *    *** color=6 YELLOW
      *     MOVE    6           TO      C
           IF      WK-COLOR    >=      "0"
               AND WK-COLOR    <=      "7"
                   MOVE    NUMVAL (WK-COLOR) TO C
                   MOVE    "X"         TO      WK-COLOR
           ELSE
                   MOVE    ZERO        TO      C
           END-IF

           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** �������@�Z�p���[�^�@�������@�̏o��
           MOVE    80          TO      WK-ASCII-LEN
           PERFORM VARYING I FROM 80 BY -1
                   UNTIL I < 2
                      OR WDE07-ASCII-TBL (I) NOT = SPACE
                   IF    WDE07-ASCII-TBL (I) = SPACE
                         ADD     -1        TO      WK-ASCII-LEN
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-END

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   MOVE    SPACE    TO  WK-DATA7 (I) 
           END-PERFORM
           MOVE    SPACE    TO  WK-DATA8 
           MOVE    1           TO      I3
                                       I4
                                       I6
           MOVE    80          TO      I5

           MOVE    1           TO      I8
           MOVE    63          TO      I7
           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL SW-END = "Y"

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   MOVE    WDE07-LINE(I) (I8:) TO WK-DATA7 (I) (I7:)
               END-PERFORM

      *     MOVE    "P"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WDE07-LINE(I)

               MOVE    WDE07-ASCII (I6:) TO WK-DATA8 (I5:)

               ADD     -1          TO      I7
               IF      I7          <       1
                       MOVE    1           TO      I7
                       ADD     1           TO      I8
                       IF      I8          >     ( WK-ASCII-LEN - 1 )
                                                 * 17

                               PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > 16
                                       MOVE    SPACE    TO  WK-DATA7 (I) 
                               END-PERFORM

                               MOVE    1           TO      I8
                               MOVE    63          TO      I7
                       END-IF
               END-IF

               ADD     -1          TO      I5
               IF      I5          <       1
                       MOVE    1           TO      I5
                       ADD     1           TO      I6
                       IF      I6          >       WK-ASCII-LEN
                               MOVE    SPACE       TO      WK-DATA8
                               MOVE    1           TO      I6
                               MOVE    80          TO      I5
                       END-IF
               END-IF

               DISPLAY SCR12-3-AREA
               CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01

               ADD     1           TO      I3
               IF      I3          >     ( WK-ASCII-LEN * 17  ) * 2
                                        + 80
      *         IF      I3          >     ( WK-ASCII-LEN * 17  )
                   MOVE    "Y"         TO      SW-END
               END-IF

               ADD     1           TO      I4
               IF      I4          >       17
                   MOVE    1           TO      I4
                   IF      WK-COLOR    =       "X"
                           CONTINUE
                   ELSE
                       ADD     1           TO      C
                       IF      C           >       7
                           MOVE    ZERO        TO      C
                       END-IF
                   END-IF
               END-IF

           END-PERFORM

           PERFORM VARYING L FROM 4 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM
           MOVE    SPACE       TO      WK-ITEM3
           MOVE    SPACE       TO      WK-ITEM4

           .
       S320-EX.
           EXIT.

      *    *** �P�Q�D�o�i�[�i�E�N���C�i�j
       S330-10.

           PERFORM VARYING L FROM 1 BY 1
                  UNTIL   L   >       24
                  EVALUATE TRUE
                      WHEN L <= 3
                        DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                        END-DISPLAY
                      WHEN L <= 13
                        DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR BLUE
                          FOREGROUND-COLOR WHITE
                      WHEN L <= 23
                        DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR YELLOW
                          FOREGROUND-COLOR WHITE
                        END-DISPLAY
                      WHEN OTHER
                        DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                        END-DISPLAY
                  END-EVALUATE
           END-PERFORM

      *    *** �������@�o�i�[�@�������@�̃Z�b�g
           MOVE    "0:�D 1:�� 2:�� 3:���"
                               TO      WK-ITEM3
           MOVE    "4:�� 5:�� 6:�� 7:��"
                               TO      WK-ITEM4
           DISPLAY SCR00-AREA
           DISPLAY SCR12-1-AREA

      *    *** �����@����
           MOVE    SPACE       TO      WK-ERR-COM
           MOVE    SPACE       TO      WK-PTN
           MOVE    SPACE       TO      WK-SOKUDO
           MOVE    SPACE       TO      WK-COLOR
           MOVE    SPACE       TO      WK-MOJI

           ACCEPT  SCR12-2-AREA

           IF      WK-PTN      NOT NUMERIC
                   MOVE    1           TO      WK-PTN
           END-IF
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    WK-MOJI     TO      WDE07-ASCII
           MOVE    WK-PTN      TO      WDE07-PTN
           IF      WK-SOKUDO   >=      "01"
               AND WK-SOKUDO   <=      "99"
                   COMPUTE WK-NANOSEC01 = NUMVAL (WK-SOKUDO) * 10000000
           ELSE
                   COMPUTE WK-NANOSEC01 = 500000000
           END-IF

      *    *** color=6 YELLOW
      *     MOVE    6           TO      C
           IF      WK-COLOR    >=      "0"
               AND WK-COLOR    <=      "7"
                   MOVE    NUMVAL (WK-COLOR) TO C
                   MOVE    "X"         TO      WK-COLOR
           ELSE
                   MOVE    ZERO        TO      C
           END-IF

           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** �������@�Z�p���[�^�@�������@�̏o��
           MOVE    80          TO      WK-ASCII-LEN
           PERFORM VARYING I FROM 80 BY -1
                   UNTIL I < 2
                      OR WDE07-ASCII-TBL (I) NOT = SPACE
                   IF    WDE07-ASCII-TBL (I) = SPACE
                         ADD     -1        TO      WK-ASCII-LEN
                   END-IF
           END-PERFORM

           MOVE    "N"         TO      SW-END

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   MOVE    SPACE    TO  WK-DATA7 (I) 
           END-PERFORM
           MOVE    SPACE    TO  WK-DATA8 
           MOVE    1           TO      I3
                                       I4
                                       I6
           MOVE    80          TO      I5

           MOVE    1           TO      I8
           MOVE    63          TO      I7
           PERFORM VARYING I2 FROM 1 BY 1
                   UNTIL SW-END = "Y"

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   MOVE    WDE07-LINE(I) (I8:) TO WK-DATA7 (I) (I7:)
               END-PERFORM

      *     MOVE    "P"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WDE07-LINE(I)

               MOVE    WDE07-ASCII (I6:) TO WK-DATA8 (I5:)

               ADD     -1          TO      I7
               IF      I7          <       1
                       MOVE    1           TO      I7
                       ADD     1           TO      I8
                       IF      I8          >     ( WK-ASCII-LEN - 1 )
                                                 * 17

                               PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > 16
                                       MOVE    SPACE    TO  WK-DATA7 (I) 
                               END-PERFORM

                               MOVE    1           TO      I8
                               MOVE    63          TO      I7
                       END-IF
               END-IF

               ADD     -1          TO      I5
               IF      I5          <       1
                       MOVE    1           TO      I5
                       ADD     1           TO      I6
                       IF      I6          >       WK-ASCII-LEN
                               MOVE    SPACE       TO      WK-DATA8
                               MOVE    1           TO      I6
                               MOVE    80          TO      I5
                       END-IF
               END-IF

               DISPLAY SCR13-3-AREA
               CALL "CBL_OC_NANOSLEEP" USING WK-NANOSEC01

               ADD     1           TO      I3
               IF      I3          >     ( WK-ASCII-LEN * 17  ) * 2
                                        + 80
      *         IF      I3          >     ( WK-ASCII-LEN * 17  )
                   MOVE    "Y"         TO      SW-END
               END-IF

               ADD     1           TO      I4
               IF      I4          >       17
                   MOVE    1           TO      I4
                   IF      WK-COLOR    =       "X"
                           CONTINUE
                   ELSE
                       ADD     1           TO      C
                       IF      C           >       7
                           MOVE    ZERO        TO      C
                       END-IF
                   END-IF
               END-IF

           END-PERFORM

           PERFORM VARYING L FROM 4 BY 1
                  UNTIL   L   >       24
                  DISPLAY WK-SPACE80
                          AT LINE L COL 1
                          WITH 
                          BACKGROUND-COLOR CYAN
                          FOREGROUND-COLOR WHITE
                  END-DISPLAY
           END-PERFORM
           MOVE    SPACE       TO      WK-ITEM3
           MOVE    SPACE       TO      WK-ITEM4

           .
       S330-EX.
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

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           MOVE    "CLOSE "    TO      WDE07-ID
           CALL    "DECODE07" USING    WDE07-DECODE07-AREA

      *    DISPLAY WK-PGM-NAME " END"
      *    DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT
      *    DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT
      *
           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .

       S900-EX.
           EXIT.
