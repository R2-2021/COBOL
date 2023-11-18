      *    *** SORT BINARY SEQUENTIAL FILE�p
      *    *** �o�h�m�P �c�`�s�`�{�b�q+�k�e���W�O�o�C�g�p
      *    *** 
      *    *** ���R�[�h���A�ς̃e�X�g�������A�o�C�i���[�t�@�C���̎��A
      *    *** ��̂e�c���œ��͏o���Ȃ��̂ŁA���R�[�h�����Ƃ�
      *    *** �쐬�쐬����K�v���邪�A���ʂȂ̂ō쐬���Ȃ�
      *    *** �p�b�N���ڂ��A���p�b�N�ɕϊ�����΁ACOBSORT�łǂ��
      *    *** ���R�[�h���ł��Ή��\�ł���
      *    *** 
      *    *** �b�r�u�͊�{�A���R�[�h�ϒ��ɂȂ�̂ŁA
      *    *** �Œ蒷�ɕϊ����ĂȂ���
      *    *** ���̃v���O�����͎g���Ȃ� 
      *    *** TEST42,TEST43��CSV�ϒ����Œ蒷�ɕϊ��o����

      *    *** SORT LINE SEQUENTIAL FILE�p��COBSORT

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST38.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SORT �p�����[�^
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORT �O�f�[�^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

      *    *** SORTWORK
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORT ��f�[�^
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
      *    *** PACK 6�o�C�g�A11���Ȃ̂ŁA11���ɂ���ACH,ZD��10���܂�
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

      *    *** �o�`�b�j�͂��̃t�@�C�����͂ł̓G���[�ɂȂ�̂ŁA
      *    *** ORGANIZATION BINARY   SEQUENTIAL�Ȃ̂�.
      *    *** �o�c�w��Ƃ���
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
      *    *** F-IN,F-OT �t�@�C�����R�Q�o�C�g
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

      *    *** PRM1-F �w�薳���iARGUMENT-NUMBER=0�j�A����l�g�p
      *    *** ARGUMENT-NUMBER=1 �̎��APRM1-F �w�肷��
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
                   DISPLAY WK-PGM-NAME " PRM1-F 1�܂Ŏw���"
                   STOP    RUN
           END-EVALUATE

      *    *** SORT-F ��OPEN ����Ȃ�

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
      *    *** SORT= �`�F�b�N
                           PERFORM S021-10     THRU    S021-EX

                       WHEN WK-PRM1DT01(1:3) =    "KEY"
      *    *** KEY= �`�F�b�N
                           PERFORM S022-10     THRU    S022-EX

                       WHEN WK-PRM1DT01(1:3) =    "CSV"
      *    *** CSV= �`�F�b�N
                           PERFORM S023-10     THRU    S023-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-IN"
      *    *** F-IN= �`�F�b�N
                           PERFORM S024-10     THRU    S024-EX

                       WHEN WK-PRM1DT01(1:4) =    "F-OT"
      *    *** F-OT= �`�F�b�N
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

      *    *** PRM1-F KEY=N �g�����`�F�b�N

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
                           " KEY=N �w�薳���F�r�n�q�s���Ȃ��A"
                   DISPLAY WK-PGM-NAME 
                           " KEY=1�̂݁AKEY=1��2�̂݁AKEY=1��2��3�̂݁A
                           �����ꂩ���w�肷��"
                   STOP    RUN
           END-IF
      *    *** PRM1-F KEY=N �w�薳�����A�r�n�q�s���Ȃ��ŏo�͂���
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
                   DISPLAY WK-PGM-NAME " SORT= A,D���w�� KEY1,2,3�̏���"
                           " A:����,D:�~���Ŏw�� 1�ڂ͕K�{"
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
                   DISPLAY WK-PGM-NAME " KEY= 1,2,3�Ŏw�� KEY=1�͕K�{"
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
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=N�̎��A"
      *                             " KEY=N POS= 1-65536�͈̔͂Ŏw��"
                                   " KEY=N POS= 1-80�͈̔͂Ŏw��"
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
                           DISPLAY WK-PGM-NAME " PRM1-F CSV=Y�̎��A"
                                   "KEY=N POS= 1-20�͈̔͂Ŏw��"
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
                                   " KEY=N LEN= 1-10�͈̔͂Ŏw��"
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
                                   " KEY=N TYPE= CH,ZD,PD �Ŏw��"
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
                                   " KEY=N TYPE=PD LEN= 1-6�͈̔͂Ŏw��"
                           STOP    RUN
                   END-IF
           END-IF

      *    *** CSV=Y �� ,"PD����", PD���ڂ͈��p���ł�����\�肾���A
      *    *** ���p�p�r�������߁A���݂̓G���[�ɂ���
           IF      WK-PRM1DT08(1:2) = "PD" AND
                   SW-CSV      =     "Y"
                   DISPLAY WK-PGM-NAME
                           " PRM1-F KEY=N TYPE= PARA ERROR"
                           PRM1-REC
                   DISPLAY WK-PGM-NAME
                           " KEY=N TYPE=PD CSV=Y�̎��A�g�p�s��"
                           " �J���}(,)��16�i����X'2C'�ƂȂ�A"
                           "�J���}�ō��ڕ����o���Ȃ���"
                   STOP    RUN
           END-IF

           EVALUATE WK-PRM1DT02(1:1)
               WHEN "1"
                    MOVE    WK-PRM1DT08(1:2) TO   WK-KEY1-CHAR
                                                  WK-KEY1-CHAR2
      *    *** TYPE=PD �w��̎��AZD�Ɠ����\�[�g�L�[�G���A�g�p����
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

      *    *** CSV= �`�F�b�N
       S023-SEC                SECTION.
       S023-10.

      *    *** �C���v�b�g�t�@�C���C�i�J���}�j�ҏW�̎��A�b�r�u���w�肷��
      *    *** �J���}�ҏW�t�@�C���ł��ACSV=N �w����A���̎��APOS=��
      *    *** ���R�[�h�̈ʒu�o�C�g���w��A
           IF      WK-PRM1DT02(1:1) =  "Y" OR "N"
                   MOVE    WK-PRM1DT02(1:1) TO     SW-CSV
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F CSV= N,Y �Ŏw��"
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
      *    *** �t�@�C�������L�����́A���͂�����
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
      *    *** �t�@�C�������L�����́A���͂�����
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
      *    *** CSV �̎�
                               PERFORM S110-10     THRU    S110-EX
                       ELSE
      *    *** CSV �ȊO�̎�
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

      *    *** CSV �̎�
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

      *    *** KEY1= �͕K�{ PRM2 �Q�Ƃ��Ȃ�
           IF      WK-KEY1-CHAR2 =     "CH"
                   MOVE    WK-CSVDT-T(P1) TO     SIO1-KEY1-X
           ELSE
      *    *** CSV=Y,TYPE=PD �͍��ڃ`�F�b�N�ŃG���[�ɂ���
                   IF      WK-KEY1-CHAR2 =     "PD"
                           MOVE    ZERO        TO      WK-PSU
                           MOVE    WK-CSV-L(P1) TO     L1

                           COMPUTE P1X = 7 - L1
                           MOVE    WK-CSVDT-T(P1) (1:L1) TO
                                   WK-PSU-X(P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                           PERFORM S130-10     THRU    S130-EX
                           MOVE    WK-PSU      TO      SIO1-KEY1-9
                   ELSE
                           MOVE    ZERO        TO      SIO1-KEY1-9
                           MOVE    WK-CSV-L(P1) TO     L1

                           COMPUTE P1X = 12 - L1
                           MOVE    WK-CSVDT-T(P1) (1:L1) TO
                                   SIO1-KEY1-9 (P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY2-9
                           ELSE
                                   MOVE    ZERO        TO    SIO1-KEY2-9
                                   MOVE    WK-CSV-L(P2) TO      L2

                                   COMPUTE P2X = 12 - L2
                                   MOVE    WK-CSVDT-T(P2) (1:L2) TO
                                           SIO1-KEY2-9 (P2X:L2)
                                   MOVE    2           TO       WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY3-9
                                 ELSE
                                   MOVE    ZERO        TO    SIO1-KEY3-9
                                   MOVE    WK-CSV-L(P3) TO     L3
                                   COMPUTE P3X = 12 - L3
                                   MOVE    WK-CSVDT-T(P3) (1:L3) TO
                                           SIO1-KEY3-9 (P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
                                   PERFORM S140-10     THRU    S140-EX
                           END-IF
                   END-IF
           END-IF

           MOVE    PIN1-DATA   TO      SIO1-DATA
           RELEASE SIO1-REC
           .
       S110-EX.
           EXIT.

      *    *** CSV �ȊO�̎�
       S120-SEC                SECTION.
       S120-10.

           MOVE    SPACE       TO      SIO1-KEY1-X
           MOVE    SPACE       TO      SIO1-KEY2-X
           MOVE    SPACE       TO      SIO1-KEY3-X

           IF      WK-KEY1-CHAR2 =     "CH"
                   MOVE    PIN1-REC(P1:L1) TO  SIO1-KEY1-X
           ELSE
      *    *** CSV=N �̎��ATYPE=PD �w���
                   IF      WK-KEY1-CHAR2 =     "PD"
                           MOVE    ZERO        TO      WK-PSU
                           COMPUTE P1X = 7 - L1
                           MOVE    PIN1-REC (P1:L1) TO  WK-PSU-X(P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                           PERFORM S130-10 THRU S130-EX
                           MOVE    WK-PSU      TO      SIO1-KEY1-9
                   ELSE
                           MOVE    ZERO        TO      SIO1-KEY1-9
                           COMPUTE P1X = 12 - L1
                           MOVE    PIN1-REC (P1:L1) TO 
                                   SIO1-KEY1-9 (P1X:L1)
                           MOVE    1           TO      WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY2-9
                           ELSE
                                   MOVE    ZERO      TO     SIO1-KEY2-9
                                   COMPUTE P2X = 12 - L2
                                   MOVE    PIN1-REC(P2:L2) 
                                                 TO SIO1-KEY2-9 (P2X:L2)
                                   MOVE    2         TO     WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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
      *    *** TYPE=PD �p ���ڃ`�F�b�N
                                   PERFORM S130-10     THRU    S130-EX
                                   MOVE    WK-PSU      TO    SIO1-KEY3-9
                           ELSE
                                   MOVE    ZERO        TO    SIO1-KEY3-9
                                   COMPUTE P3X = 12 - L3
                                   MOVE    PIN1-REC (P3:L3) TO
                                           SIO1-KEY3-9 (P3X:L3)
                                   MOVE    3           TO      WK-KEY
      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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

      *    *** TYPE=PD �p ���ڃ`�F�b�N
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

      *    *** POS= �Ԉ���č��ږ������̐��w�肵����
           IF      WK-KEY      =       1
               IF      L1          =       ZERO
                   DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY=1 POS=�̍��ڒ����[��"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR2
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      L2          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=2 POS=�̍��ڒ����[��"
                               " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                               " TYPE=" WK-KEY2-CHAR2
                       STOP    RUN
                   END-IF
               ELSE
                   IF      L3          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=3 POS=�̍��ڒ����[��"
                               " CSV=" SW-CSV " POS=" P3 " LEN=" L3 
                               " TYPE=" WK-KEY3-CHAR2
                       STOP    RUN
                   END-IF
               END-IF
           END-IF           .
       S130-EX.
           EXIT.

      *    *** TYPE=ZD �p ���ڃ`�F�b�N
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

      *    *** POS= �Ԉ���č��ږ������̐��w�肵����
           IF      WK-KEY      =       1
               IF      L1          =       ZERO
                   DISPLAY WK-PGM-NAME
                           " PIN1-REC KEY=1 POS=�̍��ڒ����[��"
                           " CSV=" SW-CSV " POS=" P1 " LEN=" L1 
                           " TYPE=" WK-KEY1-CHAR2
                   STOP    RUN
               END-IF
           ELSE
               IF      WK-KEY      =       2
                   IF      L2          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=2 POS=�̍��ڒ����[��"
                               " CSV=" SW-CSV " POS=" P2 " LEN=" L2 
                               " TYPE=" WK-KEY2-CHAR2
                       STOP    RUN
                   END-IF
               ELSE
                   IF      L3          =       ZERO
                       DISPLAY WK-PGM-NAME
                               " PIN1-REC KEY=3 POS=�̍��ڒ����[��"
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
           DISPLAY WK-PGM-NAME " PRM1 �ݽ� = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
