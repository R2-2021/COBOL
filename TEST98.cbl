      *    *** Youtube channel �ǉ��f�[�^�`�F�b�N
      *    *** 
      *    *** Youtube channel �S�ĕ\����AAlt+A�őS�ē\��t���A
      *    *** Google�X�v���b�h�V�[�g�ɂ`�F�P�ɓ\��t���`�Ń\�[�g���A
      *    *** �g�������Ń_�E�����[�h��AURL���o��https:���a��ɁA
      *    *** �ēx�d���������ɓ\��t���A
      *    *** csv�ŏo�͂��� YoutubeChannel.csv�@
      *    *** �i�t�s�e�W�ŏo�́j
      *    *** TEST98.POT2 �`�����l������<br>���ꂽ���A�`�����l���o�^�Ґ�
      *    *** ���O�̃`�����l�����Ŕ�r�����̂ŁA<br>�ȍ~�̒ǉ�����������
      *    *** ��r�ΏۊO�ɂ���
      *    *** 
      *    *** �L�[���[�h���� �L�[���[�h�܂ރ}�b�`���O���A
      *    *** �A���}�b�`���f�[�^���o
      *    *** PIN1 ID=500�o�C�g
      *    *** 
      *    *** Youtube Channel�� ()=>[]�ɕύX����A�����́i�j��

      *    *** �A���}�b�`�����R
      *    *** �@�P�D�`�����l�������ς����
      *    *** �@�Q�DYoutubeChannel20220427.csv �t�s�e�W�ŏo�͂���
      *    *** �@�@�i�r�i�h�r�j�ł̓}�b�`���O���Ȃ�
      *    *** �@�R�D�i�j�P�o�C�g�n�A�Q�o�C�g�n����[]
      *    *** �@   Duck Travel (���E���ɗF�B�����)
      *    *** �@�S�DEmerson, Lake and Palmer=> Emerson Lake and Palmer
      *    *** �@�T�D�A => �D
      *    *** �@�U�D�O���A���j������Ă݂��B=> �O���D���j������Ă݂��B
      *    *** �@�V�D����A�����X�P�b�`�B��������D�����X�P�b�`�B

      *    *** TEST98.POT2 �A���}�b�`����
      *    *** C:\Users\xxxx\OneDrive\�h�L�������g\HTML\YouTube�ėp\
      *    *** Youtubechannel.files
      *    *** ��image0nnn.png nnnn�͊�Ń`�����l���̃T���l�C���摜��
      *    *** �o�^����
      *    *** TEST97U.POT2 ��TEST98.POT2 �A���}�b�`����image0nnn.png
      *    *** ��ǉ����āA�o�^����
      *    *** 
      *    *** C:\Users\xxxx\OneDrive\�h�L�������g\HTML\YouTube�ėp\
      *    *** Youtubechannel.files�̃t�@�C�����A
      *    *** ���g�p imageNNNN=���o���Ƃ��A���̂܂܂ł����Ȃ����A
      *    *** �Ō��NNNN��DISPLAY���ꂽNNNN�ɕύX���āA
      *    *** TEST97U.POT2�����l�ɕύX����΁A���g�p�����Ȃ��Ȃ�

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST98.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ���o�O�f�[�^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** �L�[���[�h�f�[�^
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���o��}�b�`���O���f�[�^
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���o��A���}�b�`���f�[�^
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** �����������_�u���f�[�^
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC            PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03                  PIC  X(1000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03                  PIC  X(1000).

       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST98  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
      *         "YoutubeChannel20220427.csv".
               "YoutubeChannel.csv".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST97U.POT2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST98.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST98.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST98.POT3".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CHANNEL      PIC  X(500) VALUE SPACE.
           03  WK-CHANNEL-LEN  BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTTPS        PIC  X(100) VALUE SPACE.
           03  WK-INS-CNT      BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEQ          PIC  X(005) VALUE SPACE.
           03  WK-IMG-SEQ      PIC  9(004) VALUE ZERO.
           03  WK-PNG          PIC  X(036) VALUE SPACE.
           03  WK-PNG-NO       PIC  9(004) VALUE ZERO.
      *    *** �`�����l���o�^�Ґ�
           03  WK-TOUROKU-CH.
             05                PIC  X(013) VALUE
               X"E38381E383A3E383B3E3838DE3",
             05                PIC  X(014) VALUE
               X"83ABE799BBE98CB2E88085E695B0".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  O               BINARY-LONG SYNC VALUE ZERO.
           03  T               BINARY-LONG SYNC VALUE ZERO.
           03  T1              BINARY-LONG SYNC VALUE ZERO.
           03  T1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  T2              BINARY-LONG SYNC VALUE ZERO.
           03  T2-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".
           03  SW-YES          PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 2000.
             05  TBL01-HTTPS   PIC  X(100) VALUE SPACE.

           03  TBL02-AREA      OCCURS 2000.
             05  TBL02-PNG-CK  PIC  X(001) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** OPEN PIN2
                   PERFORM S012-10     THRU    S012-EX

                   MOVE    ZERO        TO      WK-INS-CNT

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX

                   PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE
                              OR WK-INS-CNT NOT = ZERO
      *    *** PIN1,PIN2 CHECK
                           PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM

      *    *** CLOSE PIN2
                   PERFORM S014-10     THRU    S014-EX

      *    *** WRITE POT1,POT2
                   PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM



      *    *** OPEN PIN2
           PERFORM S012-10     THRU    S012-EX

      *    *** READ PIN2
           PERFORM S030-10     THRU    S030-EX

      *    *** HTTPS �_�u���`�F�b�N
           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** TBL01 SET
                   PERFORM S032-10     THRU    S032-EX

      *    *** READ PIN2
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

      *    *** TBL02 IMAGENNNN NNNN�`�F�b�N
           PERFORM S120-10     THRU    S120-EX

      *    *** CLOSE PIN2
           PERFORM S014-10     THRU    S014-EX

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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F OPEN ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY WK-PGM-NAME " imagennnn nnnn �C���v�b�g"
                   ACCEPT  WK-IMG-SEQ

                   DISPLAY WK-PGM-NAME " nnnn=" WK-IMG-SEQ " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** OPEN PIN2
       S012-10.

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           MOVE    LOW-VALUE   TO      WK-PIN2-EOF
           .
       S012-EX.
           EXIT.

      *    *** CLOSE PIN2
       S014-10.

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S014-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-CHANNEL
           MOVE    ZERO        TO      WK-CHANNEL-LEN

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
                   UNSTRING PIN1-REC
                           DELIMITED BY "," OR WK-TOUROKU-CH
                           INTO
      *                     WK-SEQ
                           WK-CHANNEL  COUNT WK-CHANNEL-LEN
           END-READ

           IF      WK-PIN1-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** READ PIN2
       S030-10.

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ

           IF      WK-PIN2-STATUS NOT =  ZERO AND 10
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF
           .
       S030-EX.
           EXIT.

      *    *** TBL01 SET
       S032-10.

           MOVE     SPACE      TO      WK-CHANNEL
                                       WK-HTTPS
                                       WK-PNG
           UNSTRING PIN2-REC
                    DELIMITED BY ","
                    INTO
                    WK-CHANNEL
                    WK-HTTPS
                    WK-PNG

           IF      WK-HTTPS (1:1) NOT = SPACE
               PERFORM VARYING T FROM 1 BY 1
                   UNTIL T > T1-MAX
                   IF      TBL01-HTTPS (T) =   WK-HTTPS
      *    *** ����WK-HTTPS ������Ƃ��A�_�u��Ƃ���POT3�֏o��
                           WRITE   POT3-REC    FROM    PIN2-REC

                           IF      WK-POT3-STATUS =    ZERO
                                   ADD     1           TO    WK-POT3-CNT
                           ELSE
                                   DISPLAY WK-PGM-NAME
                                           " POT3-F WRITE ERROR STATUS="
                                           WK-POT3-STATUS
                                   STOP    RUN
                           END-IF
                   END-IF
               END-PERFORM

      *    *** C:\Users\xxxx\OneDrive\�h�L�������g\HTML\YouTube�ėp\Youtubechannel.files
      *    *** WK-PNG = ./Youtubechannel.files/image0189.png
      *    *** imageNNNN NNNN ���g�p�����`�F�b�N
      *    *** WK-PNG-NO��0001,0003,0005,...
      *    *** WK-PNG-NO=0001 T2=( 0001 + 1 ) / 2=1
      *    *** WK-PNG-NO=0003 T2=( 0003 + 1 ) / 2=2
      *    *** WK-PNG-NO=0005 T2=( 0005 + 1 ) / 2=3
               MOVE    WK-PNG (29:4) TO    WK-PNG-NO
               COMPUTE T2 = ( WK-PNG-NO + 1 ) / 2
               MOVE    "S"         TO      TBL02-PNG-CK (T2)
               IF      T2          >       T2-MAX
                       MOVE    T2          TO       T2-MAX
               END-IF
           END-IF

           IF      WK-HTTPS (1:1) NOT = SPACE
                   ADD     1           TO      T1
                   IF      T1          >       2000
                           DISPLAY WK-PGM-NAME " TBL01 OVER T1=" T1
                           STOP    RUN
                   END-IF

                   MOVE    WK-HTTPS    TO      TBL01-HTTPS (T1)
                   MOVE    T1          TO      T1-MAX
           END-IF
           .
       S032-EX.
           EXIT.

      *    *** PIN1,PIN2 CHECK
       S100-10.

      *     PERFORM VARYING I FROM 1 BY 1
      *                 UNTIL I > WK-PIN2-LEN - WK-CHANNEL-LEN
      *                    OR WK-INS-CNT NOT = ZERO

                   MOVE    WK-CHANNEL-LEN TO K

      *             INSPECT PIN1-REC (2:) TALLYING
      *                     WK-INS-CNT FOR LEADING TBL01-ID (I) (1:K)
      *             INSPECT PIN2-REC TALLYING
      *                     WK-INS-CNT FOR ALL WK-CHANNEL (1:K)
           IF      WK-CHANNEL (1:K) =  PIN2-REC (1:K)
                   ADD     1           TO      WK-INS-CNT
           END-IF

      *     END-PERFORM
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1,POT2
       S110-10.

           IF      WK-INS-CNT      =       ZERO
                   MOVE    SPACE       TO      POT2-REC
                   MOVE    WK-CHANNEL (1:K) TO POT2-REC (1:K)
                   ADD     1 K         GIVING  O
                   PERFORM VARYING I FROM O BY 1
                           UNTIL PIN1-REC (I:1) = ","
                           CONTINUE
                   END-PERFORM

                   MOVE    I           TO      I2
                   PERFORM VARYING I FROM I2 BY 1
                           UNTIL PIN1-REC (I:1) = SPACE                           CONTINUE
                           MOVE    PIN1-REC (I:1)
                                       TO      POT2-REC (O:1)
                           ADD     1           TO      O
                   END-PERFORM

                   MOVE    " ,./Youtubechannel.files/imagennnn.png"
                                       TO      POT2-REC (O:38)
                   MOVE    WK-IMG-SEQ  TO      POT2-REC (O + 30:4)
                   ADD     2           TO      WK-IMG-SEQ
                   WRITE   POT2-REC

                   IF      WK-POT2-STATUS =    ZERO
                           ADD     1           TO      WK-POT2-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF
           ELSE
                   WRITE   POT1-REC    FROM    PIN1-REC

                   IF      WK-POT1-STATUS =    ZERO
                           ADD     1           TO      WK-POT1-CNT
                   ELSE
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** TBL02 IMAGENNNN NNNN�`�F�b�N
       S120-10.

      *    *** T2=1,WK-PNG-NO=1*2-1=1
      *    *** T2=2,WK-PNG-NO=2*2-1=3
      *    *** T2=3,WK-PNG-NO=3*2-1=5
           PERFORM VARYING T2 FROM 1 BY 1
                   UNTIL T2 > T2-MAX
                   IF      TBL02-PNG-CK (T2) =     SPACE
                           COMPUTE WK-PNG-NO = T2 * 2 - 1
                           DISPLAY WK-PGM-NAME " ���g�p imageNNNN="
                                   WK-PNG-NO
                   END-IF
           END-PERFORM 
           .
       S120-EX.

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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT3-F
           IF      WK-POT3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT3-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN2-CNT TO      WK-PIN2-CNT-E
           DISPLAY WK-PGM-NAME " PIN2 �ݽ� = " WK-PIN2-CNT-E
                   " (" WK-PIN2-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 �ݽ� = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 �ݽ� = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
