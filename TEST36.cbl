      *    *** SORT �T���v���@COBRND�Ńf�[�^�쐬
      *    *** 1,000,000��SORT�@��34�b

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST36.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** ���g�p
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** SORTWORK
       SELECT SIO1-F           ASSIGN   WK-SIO1-F-NAME.

      *    *** SORT��f�[�^ 
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** PACKED-DECIMAL�܂ގ��ABINARY SEQUENTIAL �w�肵��
      *    *** ���R�[�h�̍Ō��CRLF=X"0D0A"���K�v
      *    *** �����ƃ��R�[�h���������Ă��܂�
      *    *** ���̃��R�[�h�Ǎ��ł́ABINARY SEQUENTIAL �w�肵��
      *    *** ���R�[�h��`��CRLF���w�肵�āA���R�[�h�����킹��
      *     ORGANIZATION IS RECORD BINARY SEQUENTIAL. 

      *    *** SORT��f�[�^ �o�C�i���f�[�^
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       SD  SIO1-F
           LABEL RECORDS ARE STANDARD.
       01  SIO1-REC.
           03  SIO1-SEQNO      PIC  9(010).
           03  SIO1-K1         PIC  X(001).
           03  SIO1-YYYY       PIC  9(004).
           03  SIO1-K2         PIC  X(001).
           03  SIO1-MM         PIC  9(002).
           03  SIO1-K3         PIC  X(001).
      *     03  SIO1-DATA1      PIC -9(005).
           03  SIO1-DATA1      PIC S9(005).
           03  SIO1-K4         PIC  X(001).
      *    *** COMP-3�̓t�@�C���󂯓n���o���Ȃ��@X"10" X"0D" �o�̓t�@�C��
      *    *** ����͂���ƁA�t�@�C������
          03  SIO1-DATA2       PIC S9(009) PACKED-DECIMAL.
      *     03  SIO1-DATA2      PIC -9(005).
           03  SIO1-K5         PIC  X(001).
           03  SIO1-DATA3      PIC  X(020).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  FILLER          PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(080).
      *     03  POT1-CRLF       PIC  X(002).

      *    *** BINARY�p
       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(078).
           03  POT2-CRLF       PIC  X(002).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST36  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST36.PIN1".
           03  WK-SIO1-F-NAME  PIC  X(032) VALUE "TEST36.SIO1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST36.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST36.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-LEN          BINARY-LONG VALUE ZERO.

           03  WK-DATA1        PIC SV9(07) VALUE ZERO PACKED-DECIMAL.
           03  WK-SEED         PIC  9(006) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPCOBRND    REPLACING ==:##:== BY ==WCR==.

       01  INDEX-AREA,
           03  I               BINARY-LONG VALUE ZERO.
           03  J               BINARY-LONG VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-SEC                SECTION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

           SORT    SIO1-F
                   ASCENDING  KEY SIO1-YYYY
      *             DESCENDING KEY SIO1-MM
                   ASCENDING  KEY SIO1-MM

      *    *** READ AND RELEASE
                   INPUT  PROCEDURE S100-SEC   THRU    S100-EX

      *    *** RETURN AND WRITE
                   OUTPUT PROCEDURE S200-SEC   THRU    S200-EX

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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA
           .
       S010-EX.
           EXIT.

      *    *** READ AND RELEASE
       S100-SEC                SECTION.
       S100-10.

           MOVE    2           TO      WCR-IDX

           MOVE    2000        TO      WCR-FROM   (1)
           MOVE    10          TO      WCR-TO-CNT (1)
           MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    "-"         TO      WCR-SIGN   (1)
           MOVE    "N"         TO      WCR-ZERO   (1)

      *    *** WCR-FROM,WCR-TO-CNT,WCR-BETWEEN �w�肵�����A
      *    *** WCR-FROM �� WCR-BETWEEN ���������l���߂� 
      *    *** WCR-TO-CNT �� �����l���߂�

      *    *** (1) ��WCR-FROM2,WCR-TO2 �w�肵�Ă��Ȃ��ׁA
      *    *** WK-NUM�� 0-100000 �̒l���߂�

           MOVE    2000        TO      WCR-FROM2  (2)
           MOVE    3000        TO      WCR-TO2    (2)
           MOVE    "-"         TO      WCR-SIGN   (2)

           PERFORM UNTIL   WK-PIN1-EOF   =     HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                           ADD     1           TO        WK-PIN1-CNT
                           RELEASE SIO1-REC    FROM      PIN1-REC
                   ELSE
                       IF  WK-PIN1-STATUS =    10

                           PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > 1000
      *                         MOVE    FUNCTION RANDOM(WK-SEED) TO
      *                         MOVE    FUNCTION RANDOM TO
      *                                 WK-DATA1
      *                         COMPUTE WK-SEED = WK-DATA1 * 100000

      *    *** .999999�A.555555�A.444444�͕΂�L��A�_��������
      *      COMPUTE WK-DATA1 = ( WK-SEED * WK-DATA1 ) + .333333
      *     DISPLAY WK-DATA1
      *     COMPUTE WK-SEED  =   WK-DATA1 * 100000


      *                         COMPUTE SIO1-YYYY
      *                                 SIO1-DATA3 = (WK-DATA1 * 100) +
      *                                               2000
      *    *** �쐬�����P�O�O�O�ÂA2000,2001,...,2009�ō쐬
                               MOVE    "RND"       TO      WCR-ID
                               CALL    "COBRND"    USING WCR-COBRND-AREA
      *                         DISPLAY "WCR-FROM (1)=" WCR-FROM (1)
      *                         DISPLAY "WCR-FROM (2)=" WCR-FROM (2)
      *                         COMPUTE J = ( I - 1 )  / 1000
      *                         COMPUTE SIO1-YYYY  =  J * 1 + 2000
                               MOVE     WCR-S-NAME(1) TO   SIO1-DATA3 
      *                         MOVE    WK-FROM    TO    SIO1-YYYY
                               MOVE    WCR-FROM(1)    TO    SIO1-YYYY
      *                         MOVE    FUNCTION RANDOM(WK-SEED) TO
      *                         MOVE    FUNCTION RANDOM TO
      *                                 WK-DATA1
      *                        COMPUTE WK-SEED = WK-DATA1 * 100000

      *    *** I / 1000 = �����_�ȉ��K�v�Ȃ̂ŁA�ȉ��̂܂܂ɂ���
      *                         COMPUTE WK-DATA1 = ( WK-SEED * WK-DATA1 )
      *                                          + ( ( I / 1000 )  + 1 )
      *                                          * .111111
      *                         COMPUTE WK-SEED  =   WK-DATA1 * 100000

                               MOVE    WCR-RND(1) TO WK-DATA1
                               COMPUTE SIO1-MM ROUNDED = WK-DATA1 * 12
                               IF      SIO1-MM = ZERO
                                       MOVE   1    TO    SIO1-MM
                               END-IF

                               MOVE    ","         TO    SIO1-K1
                                                         SIO1-K2
                                                         SIO1-K3
                                                         SIO1-K4
                                                         SIO1-K5
      *                         MOVE    FUNCTION RANDOM TO
      *                         MOVE    FUNCTION RANDOM TO
      *                                 WK-DATA1
      *                         COMPUTE WK-SEED = WK-DATA1 * 100000
      *                         COMPUTE WK-DATA1 = ( WK-SEED * WK-DATA1 )
      *                                          + ( ( I / 1000 )  + 1 )
      *                                          * .111111
      *                         COMPUTE WK-SEED  =   WK-DATA1 * 100000

      *                         MOVE    WCR-RND(2) TO WK-DATA1
      *                         COMPUTE SIO1-DATA1 = WK-DATA1 * 111111
                               MOVE    WCR-NUM(1)   TO  SIO1-DATA1

      *                         MOVE    FUNCTION RANDOM(WK-SEED) TO
      *                         MOVE    FUNCTION RANDOM TO
      *                                 WK-DATA1
      *                         COMPUTE WK-SEED = WK-DATA1 * 100000
      *                         COMPUTE WK-DATA1 = ( WK-SEED * WK-DATA1 )
      *                                          + ( ( I / 1000 )  + 1 )
      *                                          * .111111
      *                         COMPUTE WK-SEED  =   WK-DATA1 * 100000

      *                         MOVE    WCR-RND(3) TO WK-DATA1
      *                         COMPUTE SIO1-DATA2 = WK-DATA1 * 222222
                               MOVE     WCR-NUM(2)   TO  SIO1-DATA2

      *                         COMPUTE SIO1-DATA3 = WK-DATA1 * 100000
      *                         MOVE    SPACE       TO    SIO1-SPACE
                               RELEASE SIO1-REC

                           END-PERFORM
                           MOVE    "L"         TO      WDT-DATE-TIME-ID
                           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
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

      *    *** RETURN AND WRITE
       S200-SEC            SECTION.
       S200-10.

           PERFORM UNTIL   WK-SIO1-EOF   =     HIGH-VALUE

                   RETURN  SIO1-F
                       AT END
                           MOVE    HIGH-VALUE  TO      WK-SIO1-EOF
                       NOT AT END
                           ADD     1           TO      WK-POT1-CNT
                                                       WK-POT2-CNT
                           MOVE    WK-POT1-CNT TO      SIO1-SEQNO
                           MOVE    SIO1-REC    TO      POT1-REC
                                                       POT2-REC
                           MOVE    X"0D0A"     TO      POT2-CRLF
                           WRITE   POT1-REC

      *     IF WK-POT1-CNT = 73 OR 74 OR 225 OR 319
      *     MOVE    "P"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC
      *     END-IF

                           IF      WK-POT1-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME 
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                               STOP    RUN
                           END-IF

                           WRITE   POT2-REC
                           IF      WK-POT2-STATUS NOT =  ZERO
                               DISPLAY WK-PGM-NAME 
                                   " POT2-F WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                               STOP    RUN
                           END-IF
                           MOVE    "P"         TO      WFD-ID
                           MOVE    WK-POT2-CNT TO      WFD-SEQ
                           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                                       POT1-REC
                   END-RETURN
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-SEC                SECTION.
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

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"
           MOVE    WK-POT2-CNT TO      WK-POT2-CNT-E
           DISPLAY WK-PGM-NAME " POT2 �ݽ� = " WK-POT2-CNT-E
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
