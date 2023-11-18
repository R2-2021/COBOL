      *    *** YOUTUBE ������A���L�[�őS�^�C�g���\����
      *    *** �b�������{�` �y�[�X�g�����t�@�C�����b�r�u�f�[�^�o��

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST39.

       ENVIRONMENT             DIVISION.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ���[�`���[�u�f�[�^

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���[�`���[�u��̓f�[�^�P
      *    *** 1:29:56,1996 �H�_ 720P �����y �����U,
      *    *** 1:39:40,���� ���`�d�e HD 1080p,
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���[�`���[�u��̓f�[�^�Q
      *    *** 197,��������d�e,?�ߔ\,7 ���O�ɍX�V,
      *    *** 265,���� �d�e,Michael Chiu,����X�V,
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���[�`���[�u��̓f�[�^�R
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** ���[�`���[�u��̓f�[�^�S
      *    *** ����,00220000,10 �����O,2007 �N���T�r�s��?�e.,
      *    *** ����,00087,1 �����O,��?�^?�e�s�����p�Y�t����d�e-���A�^.,
       SELECT POT4-F           ASSIGN   WK-POT4-F-NAME
                               STATUS   WK-POT4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(1000).

      *    *** �P�^�C�g���A�J�n�����F�����F�����̂���
       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(2000).

      *    *** �����^�C�g���A�ŏ������̂���
       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  POT2-DATA.
             05  FILLER        PIC  X(2000).

      *    *** �^�C�g�����A�����F�����F�����̂���
       FD  POT3-F
           LABEL RECORDS ARE STANDARD.
       01  POT3-REC.
           03  POT3-DATA.
             05  FILLER        PIC  X(2000).

      *    *** �^�C�g�����A�����F�����F�����̂���
       FD  POT4-F
           LABEL RECORDS ARE STANDARD.
       01  POT4-REC.
           03  POT4-DATA.
             05  FILLER        PIC  X(2000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST39  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "youtube.���ꒆ��all".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "youtube.����d�eall".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "youtube.�ŐV�d�e".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST39.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST39.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST39.POT3".
           03  WK-POT4-F-NAME  PIC  X(032) VALUE "TEST39.POT4".

           03  WK-PIN1-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT3-STATUS       PIC  9(002) VALUE ZERO.
           03  WK-POT4-STATUS       PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT1    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT2    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ITEM-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT3-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT4-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-REC          PIC  X(2000) VALUE SPACE.
           03  WK-REC2         PIC  X(2000) VALUE SPACE.

           03  WK-SIKAISU      PIC  X(012) VALUE ZERO.
           03  WK-SIKAISU-9    REDEFINES   WK-SIKAISU
                               PIC  9(012).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA,
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE 1.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA,
           03  SW-SRH          PIC  X(001) VALUE "0".
           03  SW-LIB          PIC  X(001) VALUE "0".
           03  SW-PIRIODO      PIC  X(001) VALUE "0".

       01  TBL-AREA,
           03  TBL01-AREA      OCCURS 2000.
             05  TBL01-GYO     BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX
      
      *    *** READ PIN1 TBL SET
           PERFORM S030-10     THRU    S030-EX
                   UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** CLOSE PIN1
           PERFORM S040-10     THRU    S040-EX

      *    *** READ PIN1
           IF      SW-LIB      =       "1"
               PERFORM S020-10     THRU    S020-EX
                   UNTIL WK-PIN1-EOF = HIGH-VALUE OR
                         PIN1-REC (1:15) = 
      *    *** ���C�u�����̍s�܂œǂݔ�΂�
                                       X"E383A9E382A4E38396E383A9E383AA"

      *    *** ���C�u������̎��̃��R�[�hREAD
      *    *** READ PIN1
               IF      WK-PIN1-EOF NOT = HIGH-VALUE
                       PERFORM S020-10     THRU    S020-EX
               END-IF
           END-IF

      *    *** CHECK & WK-REC SET & READ PIN1
           PERFORM S100-10     THRU    S100-EX
                   UNTIL WK-PIN1-EOF = HIGH-VALUE

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

           OPEN    OUTPUT      POT4-F
           IF      WK-POT4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F OPEN ERROR STATUS="
                           WK-POT4-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS   =       ZERO
                   ADD     1           TO      WK-PIN1-CNT2
           ELSE
               IF  WK-PIN1-STATUS   =       10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE

                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

      *     IF      WK-PIN1-CNT2 >= 10 AND 
      *            WK-PIN1-CNT2 <= 40

      *            MOVE    "P"         TO      WFD-ID
      *            CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        PIN1-REC (1:100)
      *     END-IF

           .
       S020-EX.
           EXIT.

      *    *** READ PIN1
       S030-10.

           READ    PIN1-F

           IF      WK-PIN1-STATUS   =       ZERO
                   ADD     1           TO      WK-PIN1-CNT1
           ELSE
               IF  WK-PIN1-STATUS   =       10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE

                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF

           IF      WK-PIN1-LEN =       15 AND
                   PIN1-REC (1:15) = 
      *    *** ���C�u����
                                       X"E383A9E382A4E38396E383A9E383AA"
                   MOVE    "1"         TO      SW-LIB
           END-IF

           MOVE    WK-PIN1-LEN TO      L

      *    *** �Đ���
      *    *** �^�C�g���J�n�s�@�s�a�k�Z�b�g
      *    *** �Đ����̂P�s�O���^�C�g���J�n�s�ɂȂ��Ă���
           IF    ( WK-PIN1-LEN =       9 AND
                   PIN1-REC(1:9) =     X"E5868DE7949FE4B8AD" )  OR

      *    *** �^�C�g���J�n�s�@�s�a�k�Z�b�g
      *    *** nn:nn:nn �P�s�O���^�C�g���J�n�s�ɂȂ��Ă���
                (  WK-PIN1-LEN >=      2  AND
                   WK-PIN1-LEN <=      10 AND
                 ( PIN1-REC(2:1) =     ":" OR
                   PIN1-REC(3:1) =     ":" ))
                   ADD     1           TO      J
                   IF      J           >       2000
                           DISPLAY WK-PGM-NAME " TBL01 ð��� OVER J="
                           J
                           STOP    RUN
                   END-IF
                   ADD     -1  WK-PIN1-CNT1 GIVING TBL01-GYO (J)
                   MOVE    J           TO      J-MAX
           ELSE
                   ADD    WK-PIN1-LEN -5 GIVING      P2

      *    *** nn �񎋒��@�s�a�k�Z�b�g
      *    *** nn �񎋒� �P�s�R�s�オ�^�C�g���J�n�s�ɂȂ��Ă���
                   IF   ( WK-PIN1-LEN >=      6  AND
      *    *** ����
                          PIN1-REC(P2:6) =     X"E8A696E881B4" )  OR

      *    *** ���^�^�C�g���@�Q���ڈȍ~
                         ( L           >=      1 AND
                           L           <=      3 AND
                           PIN1-REC(1:L) IS NUMERIC )             OR
      *    *** �E�O�p�@1����
                         ( PIN1-REC(1:L) =   X"E296B6" )
                           ADD     1           TO      J
                           IF      J           >       2000
                               DISPLAY WK-PGM-NAME " TBL01 ð��� OVER"
                               " J=" J
                               STOP    RUN
                           END-IF
                           MOVE    WK-PIN1-CNT1 TO     TBL01-GYO (J)
                           MOVE    J           TO      J-MAX

                           IF      SW-LIB      =       "0"    AND
                                 ( WK-PIN1-LEN >=      6  AND
      *    *** ���C�u�����Ȃ����A
      *    *** �����̂Q�s�O���^�C�g���s
                                   PIN1-REC(P2:6) =     X"E8A696E881B4")
                                   ADD     -2          TO  TBL01-GYO (J)
                           END-IF
           END-IF
      *            MOVE    "P"         TO      WFD-ID
      *            CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        PIN1-REC
      *    END-IF

           .
       S030-EX.
           EXIT.

      *    *** CLOSE PIN1
       S040-10.

      *    *** PIN1 CLOSE ���āA������xOPEN����
           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           MOVE    LOW-VALUE   TO       WK-PIN1-EOF
           .
       S040-EX.
           EXIT.

      *    *** CHECK & WK-REC SET & READ PIN1
       S100-10.

      *    *** TBL ���
           PERFORM S110-10     THRU    S110-EX

           IF      SW-SRH      =       "1" AND
                   P           >       1
               IF      WK-REC(2:1) =     ":" OR
                       WK-REC(3:1) =     ":"
                       WRITE  POT1-REC     FROM    WK-REC
                       ADD    1            TO      WK-POT1-CNT
               ELSE
      *    *** �w�w�w�w,nn:n, OR
      *    *** �w�w�w�w,nn:nn,
                   IF      WK-REC (P - 4 : 1) =      ":"      OR
                           WK-REC (P - 3 : 1) =      ":"
                               WRITE  POT3-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT3-CNT
                   ELSE
      *    *** ����
                       IF      WK-REC (1:6) =      X"E8A696E881B4"
                               WRITE  POT4-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT4-CNT
                       ELSE
                               WRITE  POT2-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT2-CNT
                       END-IF
                   END-IF
               END-IF

               MOVE   SPACE        TO      WK-REC
               MOVE   1            TO      P
               MOVE   ZERO         TO      WK-ITEM-CNT
           END-IF

      *    *** ���̏����͓ǂݔ�΂�
           IF    ( WK-PIN1-LEN  =       ZERO      ) OR
      *
                 ( WK-PIN1-LEN  =       3 AND
                   PIN1-REC(1:3) =      X"E280A2" ) OR
      *    *** �Đ����X�g�̑S�̂�����
                 ( WK-PIN1-LEN  =       33    AND
                   PIN1-REC(1:15) =     
            X"E5868DE7949FE383AAE382B9E38388" AND
                   PIN1-REC(16:18) =     
            X"E381AEE585A8E4BD93E38292E8A68BE3828B" ) OR
      *    *** �Đ���
                 ( WK-PIN1-LEN  =      9 AND
                   PIN1-REC(1:9) =     X"E5868DE7949FE4B8AD" )
                   CONTINUE
           ELSE
                   MOVE    WK-PIN1-LEN TO      L L2
                   IF      P + L       >       2000
                           DISPLAY WK-PGM-NAME " WK-REC SET ERROR" 
                                   " WK-PIN1-CNT2=" WK-PIN1-CNT2
                                   " P=" P
                                   " L=" L
                           STOP    RUN
                   END-IF
      *    *** �����̂Ƃ��A
      *    *** ������O�ɂ���Ann�񎋒� => ����nn��
                   IF  L2       >=   6 AND
                       PIN1-REC(L2 - 5 : 6) = X"E8A696E881B4"
                       MOVE    X"E8A696E881B4" TO  WK-REC(P:6)
                       ADD     6           TO      P
                       ADD     -6          TO      L2
                       MOVE    PIN1-REC(1:L2) TO   WK-REC(P:L2)
                       ADD     L2          TO      P
      *    *** LIB=0�̎��A�����͂R���ڂȂ̂ŁA�擪�ɂ���
                       IF      SW-LIB      =       "0"
                           MOVE    WK-REC      TO      WK-REC2
                           MOVE    WK-REC2(1 : P - L - 1) TO
                                               WK-REC(L + 2 : P - L - 1)
                           MOVE    WK-REC2(P - L:L) TO
                                              WK-REC(1:L)
                           MOVE    ","         TO      WK-REC(L + 1 : 1)
                       END-IF

      *    *** ����,000000000000,�ɂ���
                       PERFORM S120-10     THRU    S120-EX

      *                 DISPLAY   PIN1-REC(1:L)
      *                 DISPLAY   "P=" P " L=" L
      *                 IF WK-PIN1-CNT2 > 500
      *                    STOP RUN
      *                 END-IF
                   ELSE
      *    *** �Q���ڂɂ����F�����F����������Ƃ��A
      *    *** �P���ڂƓ���ւ���
      *     IF WK-PIN1-CNT2 >= 1 AND
      *        WK-PIN1-CNT2 <= 105
      *            MOVE    "P"         TO      WFD-ID
      *            CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        WK-REC(1:200)
      *        DISPLAY "WK-PIN1-CNT2  =" WK-PIN1-CNT2
      *        DISPLAY "WK-ITEM-CNT   =" WK-ITEM-CNT
      *        DISPLAY "L             =" L
      *        DISPLAY "PIN1-REC(1:10)=" PIN1-REC(1:10) 
      *     END-IF
                       IF      WK-ITEM-CNT =       1 AND
                               L           >=      3 AND
                             ( PIN1-REC(L - 2 : 1) = ":" OR
                               PIN1-REC(L - 1 : 1) = ":" )
                           MOVE    WK-REC      TO      WK-REC2
                           MOVE    WK-REC2(1 : P - 1)
                                                TO WK-REC(L + 2 : P - 1)
                           MOVE    PIN1-REC(1:L) TO    WK-REC(1:L)
                           MOVE    ","         TO      WK-REC(L + 1 : 1)
                       ELSE
                           MOVE    PIN1-REC(1:L) TO    WK-REC(P:L)
                       END-IF
      
                       ADD     L           TO      P
                       
                   END-IF

                   MOVE    ","         TO      WK-REC(P:1)
                   ADD     1           TO      P
                   ADD     1           TO      WK-ITEM-CNT
           END-IF

      *    *** PIN1-READ
           PERFORM S020-10     THRU    S020-EX
           
           IF      WK-PIN1-EOF =       HIGH-VALUE AND
                   P           >       1
               IF      WK-REC(2:1) =     ":" OR
                       WK-REC(3:1) =     ":"
                       WRITE  POT1-REC     FROM    WK-REC
                       ADD    1            TO      WK-POT1-CNT
               ELSE
                   IF      WK-REC (P - 4 : 1) =      ":"      OR
                           WK-REC (P - 3 : 1) =      ":"
                               WRITE  POT3-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT3-CNT
                   ELSE
                       IF      WK-REC (1:6) =      X"E8A696E881B4"
                               WRITE  POT4-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT4-CNT
                       ELSE
                               WRITE  POT2-REC     FROM    WK-REC
                               ADD    1            TO      WK-POT2-CNT
                       END-IF
                   END-IF

               END-IF
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** TBL ���
       S110-10.

           MOVE    "0"         TO      SW-SRH
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > J-MAX    OR   
                         SW-SRH = "1"
      *    *** �^�C�g���J�n�s���H
                   IF      TBL01-GYO (J) =     WK-PIN1-CNT2
                           MOVE    "1"         TO      SW-SRH
                   END-IF
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** �����񐔂𐔎��ɕύX ��A���A������ăJ���}�}��
      *    *** �M���M�����������Ȃ��ƁA�v����
       S120-10.

      *    MOVE    "X"         TO      WFD-ID
      *    CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                        WK-REC(1:L)
      *    *** WK-REC=����9999 ��̗l�ɓ����Ă���
      *    *** ����2.5�� �񁁁�����,00025000
      *    *** ����3.5�� �񁁁�����,350000000
      *    *** 
           MOVE    ZERO        TO      WK-SIKAISU
           MOVE    12          TO      J2
           MOVE    "0"         TO      SW-PIRIODO

      *    *** PERFORM ��FROM�@�}�C�i�X�͋L���o���Ȃ�
      *     PERFORM VARYING J FROM L - 10  BY -1
           ADD     L -4        GIVING  L3
           PERFORM VARYING J FROM L3   BY -1
                   UNTIL J < 7
      *    *** ��
                   IF      WK-REC (J - 2: 3) = X"E4B887"
                           MOVE    8           TO      J2
                           ADD     -3          TO      J
                   END-IF
      *    *** ��
                   IF      WK-REC (J - 2: 3) = X"E58484"
                           MOVE    4           TO      J2
                           ADD     -3          TO      J
                   END-IF
                   IF      WK-REC (J:1) IS     NUMERIC
                           MOVE    WK-REC (J:1) TO     WK-SIKAISU (J2:1)
                           ADD     -1          TO      J2
                   END-IF
      *    *** �s���I�h��
                   IF      WK-REC (J:1) =      "."
                           MOVE    "1"         TO      SW-PIRIODO
                   END-IF
           END-PERFORM

      *    *** �����_�L�͂P�^�P�O�ɂ���
           IF      SW-PIRIODO  =       "1"
                   COMPUTE WK-SIKAISU-9 = WK-SIKAISU-9 / 10
           END-IF

      *    *** 12���ڂ���Z�b�g����
           MOVE    12          TO      J2
           PERFORM VARYING J FROM L  BY -1
                   UNTIL J < 7
                   IF     J2 < 1
                       MOVE    "0"           TO      WK-REC (J:1)
                   ELSE
                       MOVE    WK-SIKAISU (J2:1) TO  WK-REC (J:1)
                       ADD     -1            TO      J2
                   END-IF
           END-PERFORM

      *     DISPLAY "WK-REC-E (1:L)=" WK-REC (1:L)
      *     DISPLAY "WK-SIKAISU=" WK-SIKAISU

      *    *** �J���}�����}��
           MOVE    ","         TO      WK-REC(7:1)
                
           .
       S120-EX.
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

           CLOSE   POT2-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT3-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT3-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT4-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT4-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT1 TO     WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 1�ݽ�= " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-PIN1-CNT2 TO     WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 2�ݽ�= " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 �ݽ� = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"
           MOVE    WK-POT3-CNT TO      WK-POT3-CNT-E
           DISPLAY WK-PGM-NAME " POT3 �ݽ� = " WK-POT3-CNT-E
                   " (" WK-POT3-F-NAME ")"
           MOVE    WK-POT4-CNT TO      WK-POT4-CNT-E
           DISPLAY WK-PGM-NAME " POT4 �ݽ� = " WK-POT4-CNT-E
                   " (" WK-POT4-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
