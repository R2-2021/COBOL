      *    *** �W�v�\�i�]�[�������A�p�b�N�����L�j�̏o��
      *    *** PRINT AREA 2�����ŃZ�b�g
      *    *** ���s�����Z�b�g�A�`�S�c�A�`�S���@�v���O�����Őݒ�
      *    *** KEY �u���C�N�ǉ��A�N�����v�A�N���v�A�����v
      *    *** �]�[�����A�p�b�N���W�v
      *    *** INDEX FILE READ �ǉ�

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST37.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
      *    *** PACK�@���ځABINARY���ڂ��鎞�ABINARY   SEQUENTIAL���K�v
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

      *    *** PIN2 INDEXED FILE READ ? (INVALID �܂�)
      *    *** 100,000���@READ�̏������ԁ@         12.75�b
      *    *** �u���C�N���@1,087���@READ�̏������ԁ@6.83�b
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION INDEXED
           ACCESS RANDOM
           RECORD KEY PIN2-KEY.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
             05  PRM1-A4TATE   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM1-MEISAI   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM1-KAIGYO   PIC  X(001).
             05  FILLER        PIC  X(001).
             05  PRM1-KAIGYO-BR PIC X(001).
             05  FILLER        PIC  X(003).
             05  FILLER        PIC  X(070).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-SEQNO      PIC  9(010).
           03  PIN1-K1         PIC  X(001).
           03  PIN1-YYYY       PIC  9(004).
           03  PIN1-K2         PIC  X(001).
           03  PIN1-MM         PIC  9(002).
           03  PIN1-K3         PIC  X(001).
           03  PIN1-ZSU1       PIC S9(005).
           03  PIN1-K4         PIC  X(001).
           03  PIN1-PSU1       PIC S9(009) PACKED-DECIMAL.
           03  PIN1-K5         PIC  X(001).
           03  PIN1-DATA3      PIC  X(020).
      *    *** BINARY   SEQUENTIAL�̎��AFILLER,CRLF����`����
      *    *** �����O�X���킹��
           03  FILLER          PIC  X(027).
           03  PIN1-CRLF       PIC  X(002).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC
           03  PIN2-KEY        PIC  9(004).
           03  PIN2-DATA       PIC  X(2048).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(250).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST37  ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST37.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST36.POT2".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "TEST28_201110_201810.csv".
      *    *** INDEX FILE
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST22.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST37.POT1".

           03  WK-PRM1-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS       PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PAGE         BINARY-LONG SYNC VALUE ZERO.
           03  WK-PAGE-E       PIC --,---,---,--9 VALUE ZERO.
           03  WK-TALLYING     BINARY-LONG SYNC VALUE ZERO.

           03  WK-SEQNO-X      PIC  X(010) VALUE SPACE.
           03  WK-SEQNO        PIC  9(010) VALUE ZERO.

           03  WK-YYYY         PIC  X(004) VALUE SPACE.
           03  WK-MM           PIC  X(002) VALUE SPACE.

           03  WK-ZSU1-X       PIC  X(005) VALUE SPACE.
           03  WK-ZSU1         REDEFINES WK-ZSU1-X
                               PIC S9(005).
           03  WK-YYYYMM-ZSU1  PIC S9(011) VALUE ZERO PACKED-DECIMAL.
           03  WK-YYYY-ZSU1    PIC S9(011) VALUE ZERO PACKED-DECIMAL.
           03  WK-TOTAL-ZSU1   PIC S9(011) VALUE ZERO PACKED-DECIMAL.

           03  WK-PSU1-X       PIC  X(005) VALUE SPACE.
           03  WK-PSU1         REDEFINES WK-PSU1-X
                               PIC S9(009) PACKED-DECIMAL.
           03  WK-YYYYMM-PSU1  PIC S9(011) VALUE ZERO PACKED-DECIMAL.
           03  WK-YYYY-PSU1    PIC S9(011) VALUE ZERO PACKED-DECIMAL.
           03  WK-TOTAL-PSU1   PIC S9(011) VALUE ZERO PACKED-DECIMAL.

           03  WK-DATA3        PIC  X(010) VALUE SPACE.

           03  WK-KISETU       PIC  X(002) VALUE SPACE.
           03  WK-TITLE        PIC  X(020) VALUE SPACE.
           03  WK-TITLE2       PIC  X(020) VALUE SPACE.
           03  WK-SITE         PIC  X(100) VALUE SPACE.
           03  WK-YYYYMM-CNT   BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CNT          PIC  ZZZ9   VALUE SPACE.
           03  WK-CNT2         PIC  ZZ,ZZZ,ZZ9 VALUE SPACE.
           03  WK-ESU          PIC  ---,---,---,--9 VALUE SPACE.

           03  WK-PIN2-I1      PIC  X(080) VALUE SPACE.
           03  WK-PIN2-I2      PIC  X(080) VALUE SPACE.
           03  WK-PIN2-I3      PIC  X(075) VALUE SPACE.
           03  WK-PIN2-I4      PIC  X(1600) VALUE SPACE.

           03  WK-TIT1.
             05  FILLER        PIC  X(040) VALUE
      *    ***   <=            40��                     =>
                 "�������@���D�@�W�v�@������".
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
             05  FILLER        PIC  X(002) VALUE SPACE.
             05  WK-TIT1-PAGE  PIC  ZZ,ZZ9 VALUE ZERO.

           03  WK-TIT1-A4T.
             05  FILLER        PIC  X(040) VALUE "TEST37-T".
             05  WK-TIT1-A4T-1 PIC  X(076) VALUE SPACE.

           03  WK-TIT1-A4Y.
             05  FILLER        PIC  X(070) VALUE "TEST37-Y".
             05  WK-TIT1-A4Y-1 PIC  X(076) VALUE SPACE.

      *    *** ����y�[�W�ݒ�Ńy�[�W������̍s�������A�c�s����
      *    *** �ς��̂ŁA�ȉ��ݒ�ň������

      *    *** ����y�[�W�ݒ�
      *    *** ���p�t�H���g�@�l�r�S�V�b�N
      *    *** �S�p�t�H���g�@�l�r�S�V�b�N
      *    *** �t�H���g���@�Q�U�����A�V�D�R����
      *    *** �s����O��
      *    *** �]���@��P�O�A���P�O�A�E�P�O�A���P�O����
      *    *** 
      *    *** �s������̕������F�����Q�P�R  �c���P�S�U
      *    *** �c�����̍s���F    �����V�P    �c���P�O�S

      *    *** MAX=146,A4�c�p
      *    *** 62*2=124
                 03  WK-MID1-A4T  PIC  X(124) VALUE ALL
                                                     "       NO.   �N ��
      -       "             ZSU             PSU     ���D�� ".
           03  WK-HAI-A4T   PIC  X(124) VALUE ALL
                                         "------------------------------
      -    "------------------------------- ".

      *    *** MAX=213,A4���p
      *    *** 62*3=186
           03  WK-MID1-A4Y  PIC  X(186) VALUE ALL
                                                     "       NO.   �N ��
      -       "             ZSU             PSU     ���D�� ".
           03  WK-HAI-A4Y   PIC  X(186) VALUE ALL
                                         "------------------------------
      -    "------------------------------- ".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PRINT-AREA.
           03  PR-LINE         OCCURS 101
                               PIC  X(213) VALUE SPACE.

       01  KEY-AREA.
             05  KEY-OLD.
               07  KEY-OYYYY   PIC  X(004) VALUE LOW-VALUE.
               07  KEY-OMM     PIC  X(002) VALUE LOW-VALUE.
             05  KEY-NEW.
               07  KEY-NYYYY   PIC  X(004) VALUE LOW-VALUE.
               07  KEY-NMM     PIC  X(002) VALUE LOW-VALUE.

       01  CNS-AREA.
      *    *** PX �̈󎚈ʒu
           03  CNS-P1          BINARY-LONG SYNC VALUE 1.
           03  CNS-P2          BINARY-LONG SYNC VALUE 12.
           03  CNS-P3          BINARY-LONG SYNC VALUE 17.
           03  CNS-P4          BINARY-LONG SYNC VALUE 20.
           03  CNS-P5          BINARY-LONG SYNC VALUE 36.
           03  CNS-P6          BINARY-LONG SYNC VALUE 52.
      *    *** 52+10=>62 1 LINE
      *    *** PX �̌���
           03  CNS-P1-L        BINARY-LONG SYNC VALUE 10.
           03  CNS-P2-L        BINARY-LONG SYNC VALUE 4.
           03  CNS-P3-L        BINARY-LONG SYNC VALUE 2.
           03  CNS-P4-L        BINARY-LONG SYNC VALUE 15.
           03  CNS-P5-L        BINARY-LONG SYNC VALUE 15.
           03  CNS-P6-L        BINARY-LONG SYNC VALUE 10.
      *    *** P1-PX �̈󎚍��v�����@�X�y�[�X�܂�
           03  CNS-L-SIZE      BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  C               BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE 1.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.
           03  P4              BINARY-LONG SYNC VALUE ZERO.
           03  P5              BINARY-LONG SYNC VALUE ZERO.
           03  P6              BINARY-LONG SYNC VALUE ZERO.
           03  PX              BINARY-LONG SYNC VALUE ZERO.

      *    *** �s������̕������F�����Q�P�R  �c���P�S�U
      *    *** �c�����̍s���F    �����V�P    �c���P�O�S
           03  R               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.

      *    *** "1" = A4�c,
      *    *** "0" = A4��
           03  SW-A4TATE       PIC  X(001) VALUE "1".

      *    *** "0" = ���ז���
      *    *** "1" = ���׏o��
           03  SW-MEISAI       PIC  X(001) VALUE "1".

      *    *** ���׉��s���A���ז����̎��A�h�P�h�̕����ǂ�
      *    *** "1" = 1�s�@���s
      *    *** "2" = 2�s�@���s
           03  SW-KAIGYO       PIC  X(001) VALUE "1".

      *    *** "0" = �u���C�N���̉��s����
      *    *** "1" = �u���C�N���̉��s�L��
           03  SW-KAIGYO-BR    PIC  X(001) VALUE "1".

           03  SW-ERROR        PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM WITH TEST AFTER
                   UNTIL WK-PIN1-EOF = HIGH-VALUE
                   IF      WK-PIN1-EOF NOT =   HIGH-VALUE
      *    *** MEISAI PRINT TBL SET
                           PERFORM S100-10     THRU    S100-EX

      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-IF

                   IF      KEY-OLD     =       KEY-NEW
                       CONTINUE
                   ELSE
      *    *** YYYY,MM �u���C�N
                       PERFORM S130-10     THRU    S130-EX
                       IF      KEY-OYYYY   =       KEY-NYYYY
                           CONTINUE
                       ELSE
      *    *** YYYY �u���C�N
                           PERFORM S140-10     THRU    S140-EX
                       END-IF

      *    *** 0���ł��A�`�s�@�d�m�c���A�����o��
                       IF      WK-PIN1-EOF =       HIGH-VALUE
      *    *** AT END PRINT
                           PERFORM S150-10     THRU    S150-EX
                       END-IF
                   END-IF
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

           MOVE    WDT-DATE-YY TO      WK-TIT1-YY
           MOVE    WDT-DATE-MM TO      WK-TIT1-MM
           MOVE    WDT-DATE-DD TO      WK-TIT1-DD

           MOVE    WDT-DATE-HH TO      WK-TIT1-HH
           MOVE    WDT-DATE-MI TO      WK-TIT1-MI
           MOVE    WDT-DATE-SS TO      WK-TIT1-SS

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           READ    PRM1-F
               AT  END
                   CONTINUE
               NOT  AT  END
                   CONTINUE
           END-READ
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F READ ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "N"         TO      SW-ERROR
           IF      PRM1-A4TATE =       "0" OR "1"
                   MOVE    PRM1-A4TATE TO      SW-A4TATE
           ELSE
                   MOVE    "Y"         TO      SW-ERROR
           END-IF
           IF      PRM1-MEISAI =       "0" OR "1"
                   MOVE    PRM1-MEISAI TO      SW-MEISAI
           ELSE
                   MOVE    "Y"         TO      SW-ERROR
           END-IF
           IF      PRM1-KAIGYO =       "1" OR "2"
                   MOVE    PRM1-KAIGYO TO      SW-KAIGYO
           ELSE
                   MOVE    "Y"         TO      SW-ERROR
           END-IF
           IF      PRM1-KAIGYO-BR =    "0" OR "1"
                   MOVE    PRM1-KAIGYO-BR TO   SW-KAIGYO-BR
           ELSE
                   MOVE    "Y"         TO      SW-ERROR
           END-IF

           IF      SW-ERROR    =       "Y"
                   DISPLAY WK-PGM-NAME " PRM1-F PARA ERROR PRM1-REC="
                           PRM1-REC
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
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

           COMPUTE CNS-L-SIZE = CNS-P1-L + CNS-P2-L + CNS-P3-L
                              + CNS-P4-L + CNS-P5-L + CNS-P6-L
                              + 6

           IF      SW-A4TATE   =       "1"
      *    *** ���Z�ŏ������߁A�� * CNS-L-SIZE�����߂�
                   COMPUTE C = 146 / CNS-L-SIZE
                   COMPUTE C = C   * CNS-L-SIZE
      *    *** �s������̕������F�����Q�P�R  �c���P�S�U
      *    *** �c�����̍s���F    �����V�P    �c���P�O�S
                   IF      SW-KAIGYO   =       "2"
      *    *** 50 = 101 / 2
                           MOVE    50          TO      R
                   ELSE
      *    *** 101 = 104 - 3 (�w�b�_�[)
                           MOVE   101          TO      R
                   END-IF
           ELSE
                   COMPUTE C = 213 / CNS-L-SIZE
                   COMPUTE C = C   * CNS-L-SIZE
      *    *** 34 = 68 / 2
                   IF      SW-KAIGYO   =       "2"
                           MOVE    34          TO      R
                   ELSE
      *    *** 68 = 71 - 3 (�w�b�_�[)
                           MOVE    68          TO      R
                   END-IF
           END-IF

           MOVE    CNS-P1      TO      P1
           MOVE    CNS-P2      TO      P2
           MOVE    CNS-P3      TO      P3
           MOVE    CNS-P4      TO      P4
           MOVE    CNS-P5      TO      P5
           MOVE    CNS-P6      TO      P6
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    KEY-NEW     TO      KEY-OLD

           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT

      *    *** PIN1-REC �Œ�`����΁A�]�[�������ŏ��������
      *    *** UNSTRING �Ŏ��o���ƁANUMVAL�Ő����ɂ��Ȃ��ƁA�[���ɂȂ�
      *     IF WK-PIN1-CNT < 25
      *     DISPLAY PIN1-ZSU1
      *     END-IF

      *             UNSTRING PIN1-REC
      *                      DELIMITED BY ","
      *                 INTO
      *                      WK-SEQNO-X
      *                      WK-YYYY
      *                      WK-MM
      *                     
      *                      WK-ZSU1-X
      *                      WK-PSU1-X
      *                      WK-DATA3

      *             MOVE    WK-YYYY     TO      KEY-NYYYY
      *             MOVE    WK-MM       TO      KEY-NMM
                   MOVE    PIN1-YYYY    TO      KEY-NYYYY
                   MOVE    PIN1-MM      TO      KEY-NMM

      *             MOVE    FUNCTION NUMVAL (WK-SEQNO-X) TO  WK-SEQNO

      *             MOVE    ZERO        TO      WK-TALLYING
      *             INSPECT WK-ZSU1-X TALLYING WK-TALLYING
      *                   FOR ALL "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" 
      *             MOVE    FUNCTION SUBSTITUTE(WK-ZSU1-X
      *                                                ,"p","0"
      *                                                ,"q","1"
      *                                                ,"r","2"
      *                                                ,"s","3"
      *                                                ,"t","4"
      *                                                ,"u","5"
      *                                                ,"v","6"
      *                                                ,"w","7"
      *                                                ,"x","8"
      *                                                ,"y","9")
      *                         TO      WK-ZSU1-X

      *    *** p-y=X"70"-X"79" �}�C�i�X�̎�,
      *    *** NUMVAL�@�̓}�C�i�X�̎��A�}�C�i�X�L����������A���P�����Ȃ�
      *             MOVE    FUNCTION NUMVAL(WK-ZSU1-X) TO WK-ZSU1

      *             IF      WK-TALLYING NOT = ZERO
      *                     COMPUTE WK-ZSU1 = WK-ZSU1 * -1
      *             END-IF 

           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                                               KEY-NEW
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

           GO  TO  S020-EX

      *    *** 19,1 ���犿���n�܂鎞�A�Z�b�g���Ȃ�
           MOVE    SPACE       TO      WK-TITLE
           MOVE    1           TO      M 

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 19
      *    *** �������H�ꕔ�����łȂ���������A
      *    *** �ォ��w�肳�ꂽ�������w�肵�ĂȂ�
               IF ( WK-TITLE2 (I:2) >= X"8140" AND 
                    WK-TITLE2 (I:2) <= X"9FFC" )   OR
                  ( WK-TITLE2 (I:2) >= X"E040" AND 
                    WK-TITLE2 (I:2) <= X"EAA4" )
                         MOVE   WK-TITLE2 (I:2) TO   WK-TITLE (M:2)
      *    *** J 1,3,5...
                         ADD    1            TO      I
                         ADD    2            TO      M
                   ELSE
                         MOVE   WK-TITLE2 (I:1) TO   WK-TITLE (M:1)
      *    *** J 1,2,3...
                         ADD    1            TO      M
                   END-IF
           END-PERFORM

           MOVE    "P"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
           .

       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           MOVE    PIN1-MM     TO      PIN2-KEY
      *     COMPUTE PIN2-KEY = FUNCTION NUMVAL(WK-MM)
      *     COMPUTE PIN2-KEY = FUNCTION NUMVAL(KEY-OMM) 

           READ    PIN2-F
                   KEY PIN2-KEY
               INVALID 
                   CONTINUE
               NOT INVALID
                   CONTINUE
           END-READ

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
           ELSE
               IF  WK-PIN2-STATUS =    23
                   MOVE    SPACE       TO      WK-PIN2-I1
                                               WK-PIN2-I2
                                               WK-PIN2-I3
                                               WK-PIN2-I4
                   GO  TO  S030-EX
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR KEY="
                           PIN2-KEY
                   STOP    RUN
               END-IF
           END-IF

           UNSTRING PIN2-REC
                    DELIMITED BY ","
                    INTO
                    WK-PIN2-I1
                    WK-PIN2-I2
                    WK-PIN2-I3
                    WK-PIN2-I4  COUNT L
           .
       S030-EX.
           EXIT.

      *    *** MEISAI PRINT TBL SET
       S100-10.

           ADD     1           TO      WK-YYYYMM-CNT
                                       WK-YYYY-CNT

           MOVE    PIN1-ZSU1   TO      WK-ZSU1
           MOVE    PIN1-PSU1   TO      WK-PSU1

           ADD     WK-ZSU1     TO      WK-YYYYMM-ZSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-YYYYMM-ZSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-ADD

           ADD     WK-ZSU1     TO      WK-YYYY-ZSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-YYYY-ZSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-ADD

           ADD     WK-ZSU1     TO      WK-TOTAL-ZSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-TOTAL-ZSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-TOTAL-ZSU1=" WK-TOTAL-ZSU1
                   STOP    RUN
           END-ADD

           ADD     WK-PSU1     TO      WK-YYYYMM-PSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-YYYYMM-PSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-ADD

           ADD     WK-PSU1     TO      WK-YYYY-PSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-YYYY-PSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   STOP    RUN
           END-ADD

           ADD     WK-PSU1     TO      WK-TOTAL-PSU1
                   ON SIZE ERROR
                   DISPLAY WK-PGM-NAME 
                           " ADD WK-TOTAL-PSU1 ON SIZE ERROR"
                   DISPLAY WK-PGM-NAME 
                           " WK-PIN1-CNT=" WK-PIN1-CNT
                   DISPLAY WK-PGM-NAME 
                           " WK-TOTAL-PSU1=" WK-TOTAL-PSU1
                   STOP    RUN
           END-ADD

           IF      SW-MEISAI   =       "0"
                   CONTINUE
           ELSE
      *    *** ���̃v���O�����ł́ASEQNO,ZSU1,PSU1 ���A���̕\���k���͂��Ȃ�
                   MOVE    PIN1-SEQNO  TO      WK-CNT2
                   MOVE    WK-CNT2     TO      PR-LINE (J) (P1:CNS-P1-L)
                   MOVE    PIN1-YYYY   TO      PR-LINE (J) (P2:CNS-P2-L)
                   MOVE    PIN1-MM     TO      PR-LINE (J) (P3:CNS-P3-L)
                   MOVE    PIN1-ZSU1   TO      WK-ESU
                   MOVE    WK-ESU      TO      PR-LINE (J) (P4:CNS-P4-L)
                   MOVE    PIN1-PSU1   TO      WK-ESU
                   MOVE    WK-ESU      TO      PR-LINE (J) (P5:CNS-P5-L)
                   MOVE    PIN1-DATA3  TO      PR-LINE (J) (P6:CNS-P6-L)

                   ADD     1           TO      J
                   MOVE    1           TO      J2
      *    *** PRINT TBL WRITE CHECK
                   PERFORM S110-10     THRU    S110-EX
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** PRINT TBL WRITE CHECK
       S110-10.

           IF      J           >       R
                   MOVE    1           TO      J
                   ADD     CNS-L-SIZE  TO      P1
                   IF      P1          >       C

      *    *** PRINT TBL WRITE
                           PERFORM S120-10     THRU    S120-EX

                           MOVE    SPACE       TO      PRINT-AREA
                           MOVE    CNS-P1      TO      P1
                           MOVE    CNS-P2      TO      P2
                           MOVE    CNS-P3      TO      P3
                           MOVE    CNS-P4      TO      P4
                           MOVE    CNS-P5      TO      P5
                           MOVE    CNS-P6      TO      P6
                   ELSE
                           ADD     CNS-L-SIZE  TO      P2
                           ADD     CNS-L-SIZE  TO      P3
                           ADD     CNS-L-SIZE  TO      P4
                           ADD     CNS-L-SIZE  TO      P5
                           ADD     CNS-L-SIZE  TO      P6
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** PRINT TBL WRITE
       S120-10.

           ADD     1           TO      WK-PAGE
           MOVE    WK-PAGE     TO      WK-TIT1-PAGE

           IF      SW-A4TATE   =       "1"
                   MOVE    WK-TIT1     TO      WK-TIT1-A4T-1
                   WRITE   POT1-REC    FROM    WK-TIT1-A4T
                   ADD     1           TO      WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           MOVE    SPACE       TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           CONTINUE
                   END-IF
                   WRITE   POT1-REC    FROM    WK-MID1-A4T
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-HAI-A4T
                   ADD     1           TO      WK-POT1-CNT
           ELSE
                   MOVE    WK-TIT1     TO      WK-TIT1-A4Y-1
                   WRITE   POT1-REC    FROM    WK-TIT1-A4Y
                   ADD     1           TO      WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           MOVE    SPACE       TO      POT1-REC
                           WRITE   POT1-REC
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           CONTINUE
                   END-IF
                   WRITE   POT1-REC    FROM    WK-MID1-A4Y
                   ADD     1           TO      WK-POT1-CNT
                   WRITE   POT1-REC    FROM    WK-HAI-A4Y
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           PERFORM VARYING K  FROM 1 BY 1
                   UNTIL   K > R
                   WRITE   POT1-REC    FROM     PR-LINE (K)
                   ADD     1           TO      WK-POT1-CNT
                   IF      SW-KAIGYO   =       "2"
                           IF      SW-A4TATE =       "0"
                                   IF      K         =       R
                                           CONTINUE
                                   ELSE
                                           MOVE    SPACE     TO POT1-REC
                                           WRITE   POT1-REC
                                           ADD     1     TO  WK-POT1-CNT
                                   END-IF
                            ELSE
                                   MOVE    SPACE     TO POT1-REC
                                   WRITE   POT1-REC
                                   ADD     1     TO  WK-POT1-CNT
                            END-IF
                   ELSE
                            CONTINUE
                   END-IF
           END-PERFORM
           .
       S120-EX.
           EXIT.

      *    *** YYYY,MM �u���C�N
       S130-10.

           IF      J2      =       1
                   IF      SW-KAIGYO-BR =      "1" AND
                           SW-KAIGYO    =      "1"
                           ADD     1           TO      J
                   END-IF
           END-IF
      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

      *    *** PIN2 READ
      *     PERFORM S030-10     THRU    S030-EX

           COMPUTE PX = P1 + 0
           MOVE    "**�N�����v" TO     PR-LINE (J) (PX:10)

           COMPUTE PX = PX + 11
           MOVE    KEY-OYYYY   TO      PR-LINE (J) (PX:4)

           COMPUTE PX = PX + 5
           MOVE    KEY-OMM     TO      PR-LINE (J) (PX:2)

           MOVE    WK-YYYYMM-ZSU1 TO   WK-ESU
           COMPUTE PX = PX + 3
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-YYYYMM-PSU1 TO   WK-ESU
           COMPUTE PX = PX + 16 
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-YYYYMM-CNT TO    WK-CNT2
           COMPUTE PX = PX + 16
           MOVE    WK-CNT2      TO     PR-LINE (J) (PX:10)

      *         COMPUTE PX = PX + 11
      *         MOVE    " ��**"     TO      PR-LINE (J) (PX:5)
           MOVE    ZERO        TO      WK-YYYYMM-CNT
                                       WK-YYYYMM-ZSU1
                                       WK-YYYYMM-PSU1

           IF      SW-KAIGYO-BR =  "0"
                   ADD     1           TO      J
           ELSE
                   ADD     2           TO      J
           END-IF
           MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S130-EX.
           EXIT.

      *    *** YYYY �u���C�N
       S140-10.

           IF      J2        =       1
                   IF      SW-KAIGYO-BR =  "1" AND
                           SW-KAIGYO    =  "1"
                           ADD     1       TO      J
                   END-IF
           END-IF

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX

           COMPUTE PX = P1 + 0
           MOVE    "**�N�@���v" TO     PR-LINE (J) (PX:10)

           COMPUTE PX = PX + 11
           MOVE    KEY-OYYYY   TO      PR-LINE (J) (PX:4)

           MOVE    WK-YYYY-ZSU1 TO     WK-ESU
           COMPUTE PX = PX + 8
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-YYYY-PSU1 TO     WK-ESU
           COMPUTE PX = PX + 16
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-YYYY-CNT TO      WK-CNT2
           COMPUTE PX = PX + 16
           MOVE    WK-CNT2     TO      PR-LINE (J) (PX:10)

      *         COMPUTE PX = PX + 11
      *         MOVE    " ��**"     TO      PR-LINE (J) (PX:5)
           MOVE    ZERO        TO      WK-YYYY-CNT
                                       WK-YYYY-ZSU1
                                       WK-YYYY-PSU1

           IF      KEY-NEW     =       HIGH-VALUE
                   IF      SW-KAIGYO-BR =      "0"
                           ADD     1           TO      J
                   ELSE
                           ADD     2           TO      J
                   END-IF
           ELSE
                   IF      SW-KAIGYO-BR =      "0"
                           ADD     1           TO      J
                   ELSE
                           IF      SW-KAIGYO   =       "1"
                                   ADD     3           TO      J
                           ELSE
                                   ADD     2           TO      J
                           END-IF
                   END-IF
           END-IF
           MOVE    J           TO      J2

      *    *** PRINT TBL WRITE CHECK
           PERFORM S110-10     THRU    S110-EX
           .
       S140-EX.
           EXIT.
           .

      *    *** AT END PRINT
       S150-10.

           COMPUTE PX = P1 + 0
           MOVE    "**���@���v" TO     PR-LINE (J) (PX:10)

           MOVE    WK-TOTAL-ZSU1 TO    WK-ESU
           COMPUTE PX = PX + 19
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-TOTAL-PSU1 TO    WK-ESU
           COMPUTE PX = PX + 16
           MOVE    WK-ESU      TO      PR-LINE (J) (PX:15)

           MOVE    WK-PIN1-CNT TO      WK-CNT2
           COMPUTE PX = PX + 16
           MOVE    WK-CNT2     TO      PR-LINE (J) (PX:10)

      *             COMPUTE PX = PX + 11
      *             MOVE    " ��**"     TO      PR-LINE (J) (PX:5)

      *    *** PRINT TBL WRITE
           PERFORM S120-10     THRU    S120-EX
           .
       S150-EX.
           EXIT.

      *    *** CLOSE
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

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
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
           DISPLAY WK-PGM-NAME " PRM1 �ݽ� = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 �ݽ� = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-PAGE     TO      WK-PAGE-E
           DISPLAY WK-PGM-NAME " POT1 �߰��= " WK-PAGE-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
