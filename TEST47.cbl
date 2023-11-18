      *    *** �A�j���C�g�Q���D�ꗗ�p

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST47.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1 HTML ��̓f�[�^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST10.PIN2 YYYYMM �f�[�^
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** �A�j�����D�f�[�^
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** �A�j���@�^�C�g���A�����T�C�g�f�[�^
       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

      *    *** HTML DATA
       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(10000).

      *    *** ���g�p
       FD  PIN2-F
           LABEL RECORDS ARE STANDARD.
       01  PIN2-REC.
           03  PIN2-YYYYMM     PIC  X(006).
           03                  PIC  X(074).

      *    *** ���D�T�C�g�f�[�^
       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1024).

      *    *** ���g�p
       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC.
           03  FILLER          PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST47  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST47.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST47.POT2".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN2-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT2-CNT-E   PIC --,---,---,--9 VALUE ZERO.

      *    *** �ޔ��G���A
           03  WK-HTML0        PIC  X(256) VALUE SPACE.
      *    *** ���D�ڍ׃T�C�g
           03  WK-HTML1        PIC  X(256) VALUE SPACE.
      *    *** �摜�܂Ƃ߃T�C�g
           03  WK-HTML2        PIC  X(256) VALUE SPACE.
      *    *** ���D��
           03  WK-HTML3        PIC  X(256) VALUE SPACE.
      *    *** 
           03  WK-HTML4        PIC  X(256) VALUE SPACE.
      *    *** 
           03  WK-HTML5        PIC  X(256) VALUE SPACE.
      *    *** 
           03  WK-HTML6        PIC  X(256) VALUE SPACE.
      *    *** 
           03  WK-HTML7        PIC  X(256) VALUE SPACE.

           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML3-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML4-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML5-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML6-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML7-L      BINARY-LONG SYNC VALUE ZERO.

           03  WK-A-GYO        PIC  9(001) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-DET          PIC  X(001) VALUE ZERO.

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

      *    *** ���R�[�h�ǂݔ�΂��A���s�S�ڂ܂�
           PERFORM UNTIL WK-A-GYO    = 4
                      OR WK-PIN1-EOF = HIGH-VALUE

      *    *** ���s
                   IF      PIN1-REC (1:6) =    X"E38182E8A18C"
                           ADD     1         TO      WK-A-GYO
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           PERFORM UNTIL PIN1-REC (1:6) = "</div>"
                      OR WK-PIN1-EOF = HIGH-VALUE
      *    *** ���R�[�h�ҏW
                   PERFORM S100-10     THRU    S100-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
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

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           READ    PIN2-F

           IF      WK-PIN2-STATUS =    ZERO
                   ADD     1           TO      WK-PIN2-CNT
           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF

           IF      PIN2-YYYYMM =       SPACE
                   CONTINUE
           ELSE
                   MOVE    "_"             TO  WK-POT1-F-NAME (07:01)
                   MOVE    PIN2-YYYYMM     TO  WK-POT1-F-NAME (08:06)
                   MOVE    ".POT1"         TO  WK-POT1-F-NAME (14:05)

                   MOVE    "_"             TO  WK-POT2-F-NAME (07:01)
                   MOVE    PIN2-YYYYMM     TO  WK-POT2-F-NAME (08:06)
                   MOVE    ".POT2"         TO  WK-POT2-F-NAME (14:05)
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

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.
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

           .
       S020-EX.
           EXIT.

       S100-10.

      *     IF WK-PIN1-CNT > 400  
      *        DISPLAY WK-PIN1-CNT
      *        DISPLAY "SW-H2" SW-H2
      *        DISPLAY "SW-DD" SW-DD
      *        CALL "COBDUMP" USING  PIN1-REC WK-BUF-L

           EVALUATE TRUE

               WHEN PIN1-REC (1:4) =  "<h2 " OR "<br>" OR "</h2" 
                                   OR "</a>"
                 OR PIN1-REC (1:2) =   " /"
      *    *** ���s�A���s�A�c
                 OR ( PIN1-REC (4:3) =    X"E8A18C" AND WK-PIN1-LEN = 6)

                   CONTINUE

      *    *** 10 = <a href=""
               WHEN PIN1-REC (1:8) =  "<a href="
                   COMPUTE L = WK-PIN1-LEN - 10
                   PERFORM VARYING I FROM WK-PIN1-LEN BY -1
                           UNTIL PIN1-REC(I:1) = '"'
                              OR I < 10
                           ADD     -1          TO      L
                   END-PERFORM

      *     IF WK-PIN1-CNT >= 4700 AND <= 4800
      *       DISPLAY PIN1-REC (1:40) " " WK-PIN1-CNT
      *       DISPLAY PIN1-REC (43:3)
      *    END-IF
                   IF      PIN1-REC(43:3) =     "det"
      *    *** ���D�ڍ׃T�C�g
                           MOVE    PIN1-REC(10:L) TO   WK-HTML1
                           MOVE    L           TO      WK-HTML1-L
                           MOVE    "1"         TO      SW-DET
                   ELSE
      *    *** ���D�摜�܂Ƃ߃T�C�g
                           MOVE    PIN1-REC(10:L) TO   WK-HTML2
                           MOVE    L           TO      WK-HTML2-L
                   END-IF

      *    *** �摜�܂Ƃ�
               WHEN PIN1-REC (1:15) =  X"E794BBE5838FE381BEE381A8E38281"
      *    *** WRITE POT1
                   PERFORM S120-10     THRU    S120-EX

      *    *** ���D��
               WHEN OTHER
                   IF      SW-DET      =       "1"
                           MOVE    WK-PIN1-LEN TO      L
                           MOVE    PIN1-REC(1:L) TO    WK-HTML3
                           MOVE    L           TO      WK-HTML3-L
                           MOVE    ZERO        TO      SW-DET
                   ELSE
                           DISPLAY WK-PIN1-CNT " " PIN1-REC (1:40)
      *                     CONTINUE
                   END-IF

           END-EVALUATE

      *****     GO  TO  S100-20

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "M"         TO      WFD-TYPE
      *     MOVE    "PIN1"      TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 PIN1-REC (1:100)
           .

       S100-20.
      *    IF      WK-PIN1-CNT =  8207
      *             CALL "COBDUMP" USING  PIN1-REC (1:100)
      *             MOVE    "P"         TO      WFD-ID
      *             MOVE    "M"         TO      WFD-TYPE
      *             CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC (1:200)
      *     END-IF
           .
       S100-EX.
           EXIT.

      *    *** 1,1 = "<" �̎��A
       S110-10.

           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
      *    *** ���D�f�[�^�o��
       S120-10.

      *     IF  WK-HTML2-L > 256
      *         DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *         DISPLAY "WK-HTML2-L =" WK-HTML2-L
      *         STOP    RUN
      *     END-IF

           MOVE    SPACE       TO      POT1-REC

      *    *** ���D��
           MOVE    WK-HTML3 (1:30) TO  POT1-REC (1:30)

           MOVE    31          TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
      *    *** ���D�ڍ׃T�C�g
           MOVE    WK-HTML1    TO      POT1-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
      *    *** �摜�܂Ƃ߃T�C�g
           MOVE    WK-HTML2    TO      POT1-REC (K:WK-HTML2-L)

           ADD     WK-HTML2-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    SPACE       TO      WK-HTML1
                                       WK-HTML2
                                       WK-HTML3
                                       WK-HTML4
                                       WK-HTML5
                                       WK-HTML6
                                       WK-HTML7

           MOVE    ZERO        TO      WK-HTML1-L
                                       WK-HTML2-L
                                       WK-HTML3-L
                                       WK-HTML4-L
                                       WK-HTML5-L
                                       WK-HTML6-L
                                       WK-HTML7-L

           MOVE    ZERO        TO      SW-DET
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

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
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


           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
