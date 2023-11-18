      *    *** ��ʓ��͍��ڃ`�F�b�N�ϊ��T�u���[�`��
      *    *** 
      *    *** ���g�p�̂��߁A������

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBENTCK.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBENTCK".

       01  INDEX-AREA.
           03  I               BINARY-LONG VALUE ZERO.
           03  J               BINARY-LONG VALUE ZERO.
           03  K               BINARY-LONG VALUE ZERO.

       LINKAGE                 SECTION.
       01  LINK-AREA.
      *    *** ID=X9 �w�^�C�v���X�^�C�v�ɕϊ�
      *    *** ID=XX XXX
      *    *** ID=XX XXX
           03  ENTCK-ID        PIC  X(002).
           03  ENTCK-ITEMIN    PIC  X(010).
           03  ENTCK-ITEMOUT   PIC  X(010).
           03  ENTCK-ERROR     PIC  X(001).
           03  ENTCK-ERRORNO   PIC  9(002).

       PROCEDURE               DIVISION
                   USING       LINK-AREA.

       M100-10.

           IF      ENTCK-ID    =       "X9"
                   PERFORM S010-10     THRU    S010-EX
           END-IF
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** �w�^�C�v���X�^�C�v�ɕϊ�
       S010-10.

      *    *** --99.55  => ERROR �|���Q����
      *    *** A9..9    => ERROR �D���Q����
      *    *** 9-9.55   => ERROR �|���ŏ����Ō�ȊO�ɂ���
      *    *** 99.-55   => ERROR �|���ŏ����Ō�ȊO�ɂ���
      *    *** 9 9.55   => ERROR �X�y�[�X���ŏ����Ō�ȊO�ɂ���
      *    *** 99. 55   => ERROR �X�y�[�X���ŏ����Ō�ȊO�ɂ���
      *    *** 99.55-   => OK
      *    ***  99.55   => OK
      *    *** +99.55   => OK
      *    *** -99.55   => OK
      *    *** 99.      => OK
      *    *** .99      => OK
      *    *** 99       => OK

      *    *** �E��
           CALL "C$JUSTIFY" USING ENTCK-ITEMIN "R"
      *    *** �O�X�y�[�X�����[��
           INSPECT ENTCK-ITEMIN REPLACING ALL SPACE BY ZERO
           INSPECT ENTCK-ITEMIN TALLING
           .
       S010-EX.
           EXIT.


































