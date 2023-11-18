      *    *** �A�j���C�g�Q�A�j���ꗗ�p

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST26.

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

      *    *** �A�j���@�^�C�g���A�X�^�b�t�f�[�^
       SELECT POT3-F           ASSIGN   WK-POT3-F-NAME
                               STATUS   WK-POT3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(1000).

       FD  PIN2-F.
       01  PIN2-REC.
           03  PIN2-YYYYMM     PIC  X(006).

       FD  POT1-F.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       FD  POT2-F.
       01  POT2-REC.
           03  FILLER          PIC  X(1000).

       FD  POT3-F.
       01  POT3-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST26  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST10.PIN2".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST26.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "TEST26.POT2".
           03  WK-POT3-F-NAME  PIC  X(032) VALUE "TEST26.POT3".

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
           03  WK-OP-KAKKO     BINARY-LONG SYNC VALUE ZERO.
           03  WK-OP-KAKKO-2   BINARY-LONG SYNC VALUE ZERO.
           03  WK-ED-KAKKO     BINARY-LONG SYNC VALUE ZERO.
           03  WK-ED-KAKKO-2   BINARY-LONG SYNC VALUE ZERO.
      *
      *    *** �ޔ��G���A
           03  WK-HTML0        PIC  X(256) VALUE SPACE.
      *    *** �^�C�g����
           03  WK-HTML1        PIC  X(256) VALUE SPACE.
      *    *** �^�C�g�����h�l�f
           03  WK-HTML2        PIC  X(256) VALUE SPACE.
      *    *** ���D�@�ŐV�E�l�C���
           03  WK-HTML3        PIC  X(256) VALUE SPACE.
      *    *** �L�����N�^�[��
           03  WK-HTML4        PIC  X(256) VALUE SPACE.
      *    *** ���D��
           03  WK-HTML5        PIC  X(256) VALUE SPACE.
      *    *** �T�C�g
           03  WK-HTML6        PIC  X(256) VALUE SPACE.
      *    *** �s�u�C�z�M
           03  WK-HTML7        PIC  X(256) VALUE SPACE.
      *    *** �z�M�A�N����
           03  WK-HTML8        PIC  X(256) VALUE SPACE.
      *    *** �z�M�A�N����2
           03  WK-HTML9        PIC  X(008) VALUE ZERO.
      *    *** �I�[�v�j���O
           03  WK-OP           PIC  X(512) VALUE SPACE.
      *    *** �G���f�B���O
           03  WK-ED           PIC  X(512) VALUE SPACE.
      *    *** �A�j������
           03  WK-ANIME-SS     PIC  X(256) VALUE SPACE.
      *    *** �ē�
           03  WK-KANTOKU      PIC  X(256) VALUE SPACE.
      *    *** �X�^�b�t
           03  WK-STAFF        PIC  X(256) VALUE SPACE.

           03  WK-HTML0-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML1-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML2-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML3-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML4-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML5-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML6-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML7-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML8-L      BINARY-LONG SYNC VALUE ZERO.
           03  WK-HTML9-L      BINARY-LONG SYNC VALUE 8.
           03  WK-OP-L         BINARY-LONG SYNC VALUE ZERO.
           03  WK-ED-L         BINARY-LONG SYNC VALUE ZERO.
           03  WK-ANIME-SS-L   BINARY-LONG SYNC VALUE ZERO.
           03  WK-KANTOKU-L    BINARY-LONG SYNC VALUE ZERO.
           03  WK-STAFF-L      BINARY-LONG SYNC VALUE ZERO.

           03  WK-HED1.
             05  WK-HED1-YYYY  PIC  X(004) VALUE SPACE.
             05  FILLER        PIC  X(001) VALUE ",".
             05  WK-HED1-MM    PIC  X(002) VALUE SPACE..
             05  FILLER        PIC  X(001) VALUE ",".
             05  WK-HED1-KISETU PIC X(003) VALUE SPACE.
             05  FILLER        PIC  X(001) VALUE ",".

           03  WK-HED2.
             05  FILLER        PIC  X(008) VALUE "2019,01,".
      *    *** �~
             05  WK-FUYU       PIC  X(003) VALUE X"E586AC".


      *       05  FILLER        PIC  X(008) VALUE "2018,04,".
      *    *** �t
             05  WK-HARU       PIC  X(003) VALUE X"E698A5".


      *       05  FILLER        PIC  X(008) VALUE "2018,07,".
      *    *** ��
             05  WK-NATU       PIC  X(003) VALUE X"E5A48F".


      *       05  FILLER        PIC  X(008) VALUE "2018,10,".
      *    *** �H
             05  WK-AKI        PIC  X(003) VALUE X"E7A78B".

      *       05  FILLER        PIC  X(001) VALUE ",".
      *    *** �����T�C�g
           03  WK-SITE.
             05  FILLER        PIC  X(003) VALUE X"E585AC".
             05  FILLER        PIC  X(003) VALUE X"E5BC8F".
             05  FILLER        PIC  X(003) VALUE X"E382B5".
             05  FILLER        PIC  X(003) VALUE X"E382A4".
             05  FILLER        PIC  X(003) VALUE X"E38388".
      *    *** ����HP
           03  WK-SITE2.
             05  FILLER        PIC  X(003) VALUE X"E585AC".
             05  FILLER        PIC  X(003) VALUE X"E5BC8F".
             05  FILLER        PIC  X(002) VALUE "HP".
             05  FILLER        PIC  X(001) VALUE X"09".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SPAN-CLASS   PIC  X(001) VALUE ZERO.
           03  SW-CENTER       PIC  X(001) VALUE ZERO.
           03  SW-CAST         PIC  X(001) VALUE ZERO.
           03  SW-STAFF        PIC  X(001) VALUE ZERO.
           03  SW-TR-ALIGN     PIC  X(001) VALUE ZERO.
           03  SW-KORON        PIC  X(001) VALUE ZERO.
           03  SW-SITE         PIC  X(001) VALUE ZERO.
           03  SW-TITLE        PIC  X(001) VALUE ZERO.
           03  SW-OP           PIC  X(001) VALUE "N".
           03  SW-ED           PIC  X(001) VALUE "N".
           03  SW-KAKKO        PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-X            PIC  X(001) VALUE ZERO.
           03  SV-PIN1-CNT-OP  BINARY-LONG SYNC VALUE ZERO.
           03  SV-PIN1-CNT-ED  BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

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

                   MOVE    "_"             TO  WK-POT3-F-NAME (07:01)
                   MOVE    PIN2-YYYYMM     TO  WK-POT3-F-NAME (08:06)
                   MOVE    ".POT3"         TO  WK-POT3-F-NAME (14:05)
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

      *    *** ���R�[�h�ҏW
       S100-10.
      *     IF WK-PIN1-CNT > 400  
      *        DISPLAY WK-PIN1-CNT
      *        DISPLAY "SW-H2" SW-H2
      *        DISPLAY "SW-DD" SW-DD
      *        CALL "COBDUMP" USING  PIN1-REC WK-BUF-L

           IF      PIN1-REC(1:1) =     "<"
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I   >       WK-PIN1-LEN
                   IF      PIN1-REC(I:1) =     "<" 
                       MOVE    +1      TO      L
                       PERFORM VARYING J FROM I BY 1
                           UNTIL   PIN1-REC(J:1) =       ">" OR
                                   PIN1-REC(J:2) =       """ " OR
      *    *** <img src="xxxxxx" alt="xxxxx">
      *    *** alt�̑O��X"C2A0" UTF-8 �́@�X�y�[�X�H
                                   PIN1-REC(J:3) =       X"22C2A0" OR
                                   J > WK-PIN1-LEN
                           ADD     1           TO      L
                       END-PERFORM
                       ADD     1           TO      L
                       MOVE    PIN1-REC(I:L) TO    WK-HTML0
                       MOVE    L           TO      WK-HTML0-L
      *    *** "<" �̌�A">"���� �̎�
                       PERFORM S110-10     THRU    S110-EX
                       MOVE    WK-PIN1-LEN     TO      I
                  ELSE
                       MOVE    WK-PIN1-LEN     TO      I
                  END-IF
               END-PERFORM
           ELSE
      *    *** UTF8 = �L���X�g?
                   IF      PIN1-REC(1:12) =  X"E382ADE383A3E382B9E38388"
                       AND WK-PIN1-LEN    =  12
                           MOVE    "1"         TO      SW-CAST
                   ELSE
      *    *** UTF8 = �X�^�b�t?
                     IF    PIN1-REC(1:12) =  X"E382B9E382BFE38383E38395" 
                       AND WK-PIN1-LEN    =  12
                           MOVE    "1"         TO      SW-STAFF
                     ELSE
      *    *** 1,1 "<" �ȊO�̎�
                           PERFORM S130-10     THRU    S130-EX
                     END-IF
                   END-IF
           END-IF

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

      *    *** "<" �̌�A">"���� �̎�
       S110-10.

           EVALUATE TRUE

               WHEN PIN1-REC(1:7) =    "<title>"
                   MOVE    "1"         TO      SW-TITLE

               WHEN PIN1-REC(1:8) =    "</title>"
                   MOVE    "0"         TO      SW-TITLE

      *    *** 2019.04 ����<span����<h2�̎��Ƀ^�C�g������悤�ɂȂ���
               WHEN WK-HTML0(1:12) =   "<span class=" OR
                    WK-HTML0(1:09) =   "<h2 class"
                   MOVE    "1"         TO      SW-SPAN-CLASS

               WHEN WK-HTML0(1:7)  =   "</span>" OR
                    WK-HTML0(1:5)  =   "</h2>"
                   MOVE    "0"         TO      SW-SPAN-CLASS

               WHEN WK-HTML0(1:8)  =     "<CENTER>" OR "<center>"
                   MOVE    "1"         TO      SW-CENTER

      *         WHEN WK-HTML0(1:9)  =   "</CENTER>"
               WHEN WK-HTML0(1:9)  =   "</CENTER>" OR "</center>"
                   MOVE    "0"         TO      SW-CENTER

               WHEN WK-HTML0(1:4)  =   "<br>" OR "<BR>" OR "</a>"
                   IF      WK-HTML0(1:4)  =   "<br>" OR "<BR>"
                       MOVE    "N"         TO      SW-OP
                                                   SW-ED
                   END-IF
                   IF      SW-CAST     =       "1"
                           IF      WK-HTML4 NOT = SPACE
      *    *** ���D�f�[�^�o��
                                   PERFORM S120-10     THRU    S120-EX
                           END-IF
      *                     MOVE    "0"         TO      SW-CAST
      *                     MOVE    SPACE       TO      WK-HTML1
      *                                                 WK-HTML2
                            MOVE    SPACE       TO      WK-HTML3
                            MOVE    1           TO      WK-HTML3-L
                   END-IF

               WHEN WK-HTML0(1:6)  =   "<br />"
                   MOVE    "N"         TO      SW-OP
                                               SW-ED

               WHEN WK-HTML0(1:5)  =   "</th>"
                   MOVE    "N"         TO      SW-OP
                                               SW-ED
                   MOVE    SPACE       TO      WK-HTML6
                   MOVE    1           TO      WK-HTML6-L

                   IF      SW-CAST     =       "1"
                       MOVE    ZERO        TO      SW-CAST
                   END-IF

                   IF      SW-STAFF    =       "1"
                       MOVE    ZERO        TO      SW-STAFF
                   END-IF

      *         WHEN WK-HTML0(1:10) =   "<tr align="
      *             MOVE    ZERO        TO      SW-CAST

               WHEN WK-HTML0(1:9) =    "<img src="
                   IF    SW-CENTER =   "1"
                         COMPUTE K = L - 9
      *    *** �^�C�g���h�l�f
                         ADD     -3            TO      K
                         MOVE    WK-HTML0(11:K) TO     WK-HTML2
                         MOVE    K             TO      WK-HTML2-L
      *                   DISPLAY "HTML2=" WK-HTML2 (1:70)
                   END-IF

               WHEN WK-HTML0 (1:8) = "<a href="

      *             IF      SV-PIN1-CNT-OP + 1 =    WK-PIN1-CNT
      *                     IF      PIN1-REC (1:8) =    "<a href="
      *                             MOVE    "Y"         TO      SW-OP
      *             END-IF

      *             IF      SV-PIN1-CNT-ED + 1 =    WK-PIN1-CNT
      *                     IF      PIN1-REC (1:8) =    "<a href="
      *                             MOVE    "Y"         TO      SW-ED
      *             END-IF

      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    "A"         TO      WFD-TYPE
      *     MOVE    "SW-CAST"   TO      WFD-ITEM
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 SW-CAST
      *    *** <a href="" ?
                   IF    WK-HTML0(10:1) = """"
                         DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
                         DISPLAY WK-HTML0 (1:10) " SW-CAST=" SW-CAST
      *                   STOP    RUN
                         MOVE    15          TO      L
                         MOVE    SPACE       TO      WK-HTML0(10:1)
                   END-IF
                   IF      SW-CAST      =       "1"
                         COMPUTE K = L - 9
      *    *** ���D���
                         ADD     -3            TO      K
                         MOVE    WK-HTML0(10:K) TO     WK-HTML3
                         MOVE    K             TO      WK-HTML3-L
                   ELSE
      *    *** �����T�C�g
      *****             IF      SW-SITE      =       "1"
                             COMPUTE K = L - 9
      *                       ADD     -3            TO      K
      *    *** �T�C�g�A�h���X�A�Ō�́^�܂œ����
                             ADD     -2            TO      K
                             MOVE    WK-HTML0(10:K) TO     WK-HTML6
                             MOVE    K             TO      WK-HTML6-L
      *****              END-IF
                   END-IF

               WHEN OTHER 
                   CONTINUE
           END-EVALUATE
           .
       S110-EX.
           EXIT.

      *    *** ���D�f�[�^�o��
       S120-10.

           IF  WK-HTML2-L > 256
               DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
               DISPLAY "WK-HTML2-L =" WK-HTML2-L
               STOP    RUN
           END-IF
           MOVE    SPACE       TO      POT1-REC

           IF      WK-HTML1    =      "animateTimes"
                   GO  TO  S120-20
           END-IF

           MOVE    12          TO      K
           MOVE    WK-HED1     TO      POT1-REC (1:12)

           ADD     +1          TO      K
           MOVE    WK-HTML1 (1:WK-HTML1-L) TO  POT1-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML2    TO      POT1-REC (K:WK-HTML2-L)

           ADD     WK-HTML2-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML3    TO      POT1-REC (K:WK-HTML3-L)

           ADD     WK-HTML3-L  TO      K
           MOVE    " ,"        TO      POT1-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML4    TO      POT1-REC (K:WK-HTML4-L)

           ADD     WK-HTML4-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML5    TO      POT1-REC (K:WK-HTML5-L)

           ADD     WK-HTML5-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML6    TO      POT1-REC (K:WK-HTML6-L)

           ADD     WK-HTML6-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML7    TO      POT1-REC (K:WK-HTML7-L)

           ADD     WK-HTML7-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML8    TO      POT1-REC (K:WK-HTML8-L)

           ADD     WK-HTML8-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           ADD     +1          TO      K
           MOVE    WK-HTML9    TO      POT1-REC (K:WK-HTML9-L)

           ADD     WK-HTML9-L  TO      K
           MOVE    ","         TO      POT1-REC (K:1)

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           .
       S120-20.
      *****MOVE    SPACE       TO      WK-HTML1  WK-HTML2
      *****MOVE    SPACE       TO      WK-HTML3
           MOVE    SPACE       TO      WK-HTML4  WK-HTML5
           MOVE    1           TO      WK-HTML4-L
                                       WK-HTML5-L

           MOVE    ZERO        TO      SW-SPAN-CLASS
                                       SW-CENTER
      *****                                 SW-CAST
                                       WK-OP-L
                                       WK-ED-L
                                       WK-OP-KAKKO
                                       WK-OP-KAKKO-2
                                       WK-ED-KAKKO
                                       WK-ED-KAKKO-2
           MOVE    "N"         TO      SW-OP
                                       SW-ED
                                       SW-KAKKO
           .
       S120-EX.
           EXIT.

      *    *** 1,1 "<" �ȊO�̎�
       S130-10.

           IF      WK-PIN1-LEN >       256
      *             DISPLAY "PIN1-REC=" PIN1-REC (1:20)
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " WK-PIN1-LEN=" WK-PIN1-LEN
      *                     " SW-TITLE=" SW-TITLE
      *                     " SW-CAST=" SW-CAST

                   MOVE   256          TO      WK-PIN1-LEN
           END-IF

           IF      SW-TITLE    =       "1"

                   MOVE    PIN1-REC(1:4) TO    WK-HED1-YYYY
                   EVALUATE TRUE
                       WHEN PIN1-REC(5:3) =    WK-FUYU
                            MOVE   WK-FUYU     TO      WK-HED1-KISETU
                            MOVE   "01"        TO      WK-HED1-MM
                       WHEN PIN1-REC(5:3) =    WK-HARU
                            MOVE   WK-HARU     TO      WK-HED1-KISETU
                            MOVE   "04"        TO      WK-HED1-MM
                       WHEN PIN1-REC(5:3) =    WK-NATU
                            MOVE   WK-NATU     TO      WK-HED1-KISETU
                            MOVE   "07"        TO      WK-HED1-MM
                       WHEN PIN1-REC(5:3) =    WK-AKI
                            MOVE   WK-AKI      TO      WK-HED1-KISETU
                            MOVE   "10"        TO      WK-HED1-MM
                       WHEN OTHER
                            MOVE   SPACE       TO      WK-HED1-KISETU
                   END-EVALUATE
                   MOVE    "0"         TO      SW-TITLE
           END-IF

           EVALUATE TRUE

               WHEN PIN1-REC (1:4) = "OP :"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-OP
                   MOVE    PIN1-REC (5:WK-PIN1-LEN - 4) TO WK-OP
                   COMPUTE WK-OP-L = WK-PIN1-LEN - 4
                   MOVE    ZERO        TO      WK-OP-KAKKO
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-OP-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-OP

               WHEN PIN1-REC (1:2) = "OP"
      *    *** �F
                AND PIN1-REC (3:3) = X"EFBC9A"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-OP
                   MOVE    PIN1-REC (6:WK-PIN1-LEN - 5) TO WK-OP
                   COMPUTE WK-OP-L = WK-PIN1-LEN - 5
                   MOVE    ZERO        TO      WK-OP-KAKKO
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-OP-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-OP

               WHEN PIN1-REC (1:2) = "OP"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-OP
                   MOVE    PIN1-REC (3:WK-PIN1-LEN - 2) TO WK-OP
                   COMPUTE WK-OP-L = WK-PIN1-LEN - 2
                   MOVE    ZERO        TO      WK-OP-KAKKO
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-OP-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-OP-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-OP

               WHEN PIN1-REC (1:4) = "ED :"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-ED
                   MOVE    PIN1-REC (5:WK-PIN1-LEN - 4) TO WK-ED
                   COMPUTE WK-ED-L = WK-PIN1-LEN - 4
                   MOVE    ZERO        TO      WK-ED-KAKKO
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-ED-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-ED

               WHEN PIN1-REC (1:2) = "ED"
      *    *** �F
                AND PIN1-REC (3:3) = X"EFBC9A"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-ED
                   MOVE    PIN1-REC (6:WK-PIN1-LEN - 5) TO WK-ED
                   COMPUTE WK-ED-L = WK-PIN1-LEN - 5
                   MOVE    ZERO        TO      WK-ED-KAKKO
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-ED-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-ED

               WHEN PIN1-REC (1:2) = "ED"
                AND PIN1-REC (1:10) NOT = "EDENS ZERO"
                   MOVE    WK-PIN1-CNT TO      SV-PIN1-CNT-ED
                   MOVE    PIN1-REC (3:WK-PIN1-LEN - 2) TO WK-ED
                   COMPUTE WK-ED-L = WK-PIN1-LEN - 2
                   MOVE    ZERO        TO      WK-ED-KAKKO
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
                           ALL '('
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �i
                           ALL X'EFBC88'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO FOR 
      *    *** �u
                           ALL X'E3808C'

                   MOVE    ZERO        TO      WK-ED-KAKKO-2
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
                           ALL ')'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �j
                           ALL X'EFBC89'
                   INSPECT PIN1-REC TALLYING WK-ED-KAKKO-2 FOR 
      *    *** �v
                           ALL X'E3808D'

                   MOVE    "Y"         TO      SW-ED
           END-EVALUATE

      *     IF  WK-PIN1-CNT > 23000
      *         DISPLAY WK-PIN1-CNT  
      *             SW-OP WK-OP-L WK-OP-KAKKO WK-OP-KAKKO-2
      *             SW-ED WK-ED-L WK-ED-KAKKO WK-ED-KAKKO-2
      *             SW-KAKKO
      *     END-IF

           IF      SW-OP       =       "Y"
               IF      WK-OP-KAKKO =       ZERO
      *    *** ����S130-10�ɂ́A<a href= �͗��Ȃ����A����͎c��
                   IF      PIN1-REC (1:8) =    "<a href="
                           CONTINUE
                   ELSE
                           IF    ( PIN1-REC (1:1) =    ")" AND
                                   WK-PIN1-LEN    =    1 )
      *    *** �j
                              OR ( PIN1-REC (1:3) =    X"EFBC89" AND
                                   WK-PIN1-LEN    =    3 )
      *    *** �v
                              OR ( PIN1-REC (1:3) =    X"E3808D" AND
                                   WK-PIN1-LEN    =    3 )
                                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO
                                         WK-OP (WK-OP-L + 1:WK-PIN1-LEN)
                                   COMPUTE WK-OP-L = WK-OP-L + 0
                                                   + WK-PIN1-LEN
                                   MOVE    "N"         TO      SW-OP
                           ELSE
      *     DISPLAY "OP" SW-OP  SW-KAKKO WK-PIN1-CNT SV-PIN1-CNT-OP
      *             PIN1-REC (1:40) WK-OP-L
                                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO
                                         WK-OP (WK-OP-L + 1:WK-PIN1-LEN)
                                   COMPUTE WK-OP-L = WK-OP-L + 0
                                                   + WK-PIN1-LEN
      *    *** OP�F�uDash and Go!�v
      *    *** <a href="https://www.animatetimes.com/tag/details.php?id=5681">
      *    *** ��؈���
      *    *** WK-OP-KAKKO = ZERO �Ȃ̂ŁASW-OP <= N �ɂ���
                                   IF      SV-PIN1-CNT-OP + 2 = 
                                           WK-PIN1-CNT
                                       AND SW-KAKKO = "N"
                                       MOVE    "N"         TO      SW-OP
                                   END-IF
                           END-IF
                   END-IF
               ELSE
                   MOVE    "Y"         TO      SW-KAKKO
                   MOVE    ZERO        TO      WK-OP-KAKKO
               END-IF
           ELSE
               CONTINUE
           END-IF

           IF      SW-ED       =       "Y"
               IF      WK-ED-KAKKO =       ZERO
      *    *** ����S130-10�ɂ́A<a href= �͗��Ȃ����A����͎c��
                   IF      PIN1-REC (1:8) =    "<a href="
                           CONTINUE
                   ELSE
                           IF    ( PIN1-REC (1:1) =    ")" AND
                                   WK-PIN1-LEN    =    1 )
      *    *** �j
                              OR ( PIN1-REC (1:3) =    X"EFBC89" AND
                                   WK-PIN1-LEN    =    3 )
      *    *** �v
                              OR ( PIN1-REC (1:3) =    X"E3808D" AND
                                   WK-PIN1-LEN    =    3 )
                                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO
                                         WK-ED (WK-ED-L + 1:WK-PIN1-LEN)
                                   COMPUTE WK-ED-L = WK-ED-L + 0
                                                   + WK-PIN1-LEN
                                   MOVE    "N"         TO      SW-ED
                           ELSE
                                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO
                                         WK-ED (WK-ED-L + 1:WK-PIN1-LEN)
                                   COMPUTE WK-ED-L = WK-ED-L + 0
                                                   + WK-PIN1-LEN
                                   IF      SV-PIN1-CNT-ED + 2 = 
                                           WK-PIN1-CNT
                                       AND SW-KAKKO = "N"
                                       MOVE    "N"         TO      SW-ED
                                   END-IF
                           END-IF
                   END-IF
               ELSE
                   MOVE    "Y"         TO      SW-KAKKO
                   MOVE    ZERO        TO      WK-ED-KAKKO
               END-IF
           ELSE
               CONTINUE
           END-IF

           IF      SW-STAFF    =       "1"
      *    *** STAFF WRITE POT3
                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO WK-STAFF
                   MOVE    WK-PIN1-LEN TO      WK-STAFF-L
      *    *** �X�^�b�t�f�[�^�o��
                   PERFORM S150-10     THRU    S150-EX
           END-IF

           IF      SW-SPAN-CLASS =     "1"
      *    *** �^�C�g����
      *    *** 2017 ���A�^�C�g����X"09" = TAB ���
                   IF    PIN1-REC (WK-PIN1-LEN:1) = X"09"
                         ADD     -1          TO      WK-PIN1-LEN
                   END-IF
                   MOVE    PIN1-REC (1:WK-PIN1-LEN) TO WK-HTML1
                   MOVE    WK-PIN1-LEN TO      WK-HTML1-L

           ELSE

               IF      SW-CAST     =       "1"
      *    *** UTF8 : �� �L�����N�^�[�F
                       IF      PIN1-REC(WK-PIN1-LEN - 2:3) =   X"EFBC9A"
      *    *** �L�����N�^�[��
                           MOVE    PIN1-REC (1:WK-PIN1-LEN - 3)
                                                     TO      WK-HTML4
                           ADD     WK-PIN1-LEN -3  GIVING  WK-HTML4-L
                           
                       ELSE
      *    *** ���D���@���́@�L�����N�^�[���F���D�����H
                           MOVE    ZERO      TO      SW-KORON
                           PERFORM VARYING J FROM 1 BY 3
                               UNTIL   J        >         WK-PIN1-LEN
                               IF      PIN1-REC(J:3) =      X"EFBC9A"
                                   MOVE    "1"       TO      SW-KORON
                                   ADD     J 3       GIVING  P
      *    *** �L�����N�^�[�F���D���@�R�����ŕ�������
                                   ADD     J -1      GIVING  L
                                   MOVE    PIN1-REC (1:L) TO WK-HTML4
                                   MOVE    L         TO      WK-HTML4-L

                                   MOVE    WK-PIN1-LEN   TO      J
                               END-IF
                           END-PERFORM

                           IF  SW-KORON  =    "1"
      *    *** �L�����N�^�[���F���D���̎��A
                               COMPUTE L = WK-PIN1-LEN - P + 1 
                               MOVE    PIN1-REC (P:L) TO   WK-HTML5
                               MOVE    L           TO      WK-HTML5-L
                           ELSE
      *    *** ���D���݂̂̎��A
                               MOVE    PIN1-REC (1:WK-PIN1-LEN) 
                                                       TO      WK-HTML5
                               MOVE    WK-PIN1-LEN     TO     WK-HTML5-L
                           END-IF
                       END-IF
               ELSE

      *    *** �N
                   IF  PIN1-REC (5:03) = X"E5B9B4"
      *    *** ��
                   AND ( PIN1-REC (09:03) = X"E69C88" OR 
                         PIN1-REC (10:03) = X"E69C88" )
      *    *** ��
                   AND ( PIN1-REC (13:03) = X"E697A5" OR 
                         PIN1-REC (14:03) = X"E697A5" OR
                         PIN1-REC (15:03) = X"E697A5" )
                       MOVE    PIN1-REC (1:WK-PIN1-LEN)  TO WK-HTML8
                       MOVE    WK-PIN1-LEN     TO      WK-HTML8-L

                       MOVE    ZERO            TO      WK-HTML9
                       MOVE    PIN1-REC (1:4)  TO      WK-HTML9 (1:4)
      *    *** ��
                       IF  PIN1-REC (09:03) = X"E69C88"
                           MOVE    PIN1-REC (8:1) TO      WK-HTML9 (6:1)
      *    *** ��
                           IF  PIN1-REC (13:03) = X"E697A5" 
                               MOVE    PIN1-REC (12:1) TO WK-HTML9 (8:1)
                           ELSE
                               MOVE    PIN1-REC (12:2) TO WK-HTML9 (7:2)
                           END-IF
                       ELSE
                           MOVE    PIN1-REC (8:2) TO      WK-HTML9 (5:2)
      *    *** ��
                           IF  PIN1-REC (14:03) = X"E697A5" 
                               MOVE    PIN1-REC (13:1) TO WK-HTML9 (8:1)
                           ELSE
                               MOVE    PIN1-REC (13:2) TO WK-HTML9 (7:2)
                           END-IF
                       END-IF
                   END-IF
      *    *** TV,�z�M
                   IF  PIN1-REC (1:08) = "TOKYO MX" OR
                       PIN1-REC (1:07) = "TOKYOMX"  OR
                       PIN1-REC (1:02) = "BS"       OR
                       PIN1-REC (1:03) = "tvk"      OR
                       PIN1-REC (1:03) = "NHK"      OR
                       PIN1-REC (1:03) = "TBS"      OR
                       PIN1-REC (7:03) = "TBS"      OR
      *    *** �ǔ��e���r?
                     PIN1-REC (1:15) = X"E8AAADE5A3B2E38386E383ACE38393"
                                                    OR
      *    *** �l�a�r�E�s�a�r
                     PIN1-REC (1:21) = 
                           X"EFBCADEFBCA2EFBCB3E383BBEFBCB4EFBCA2EFBCB3"
                                                    OR
      *    *** �e���r����?
                     PIN1-REC (1:15) = X"E38386E383ACE38393E69DB1E4BAAC"
                                                    OR
      *    *** �t�W�e���r?
                     PIN1-REC (1:15) = X"E38395E382B8E38386E383ACE38393"
                                                    OR
      *    *** ���{�e���r?
                     PIN1-REC (1:15) = X"E697A5E69CACE38386E383ACE38393"
                                                    OR
      *    *** �a�r? �a�r���e����
                     PIN1-REC (1:06) = X"EFBCA2"
      *                 PIN1-REC (1:06) = "BS�t�W"   OR
      *                 PIN1-REC (1:10) = "BS�W���p��" OR
      *                 PIN1-REC (1:06) = "BS-TBS" OR
      *                 PIN1-REC (1:10) = "���{�e���r" OR
      *                 PIN1-REC (1:04) = "�t�W" OR
      *                 PIN1-REC (1:10) = "�e���r����"
                       IF  WK-HTML7 (1:8) = "TOKYO MX" OR
                           WK-HTML7 (1:7) = "TOKYOMX"
                           CONTINUE
                       ELSE
                           MOVE    PIN1-REC (1:WK-PIN1-LEN)  TO WK-HTML7
                           MOVE    WK-PIN1-LEN     TO      WK-HTML7-L
                       END-IF
                   END-IF
      *    *** ���D��
      *****        MOVE    ZERO      TO      SW-SITE
                   
                   IF  PIN1-REC (1:2) = "TV"  OR
                       PIN1-REC (1:3) = " TV" OR
      *    ***�w ���H
                       PIN1-REC (1:3) = X"E3808E" OR
      *    *** �u��?
                       PIN1-REC (1:3) = X"E3808C" OR
      *    *** �A�j�����H
                       PIN1-REC (1:9) = X"E382A2E3838BE383A1"
      *                 PERFORM TEST BEFORE
                       PERFORM TEST AFTER
                               VARYING J FROM 3 BY 1
                               UNTIL   J        >       WK-PIN1-LEN - 15
      *    *** UTF8 �����T�C�g���H
                               IF      PIN1-REC(J:15)   =  WK-SITE
      *****                        MOVE    "1"       TO      SW-SITE
                                   IF      WK-HTML1 (1:1) NOT = SPACE
      *    *** �^�C�g���f�[�^�o��
                                       PERFORM S140-10   THRU    S140-EX
                                   END-IF
                               END-IF
                       END-PERFORM
                   ELSE 
      *    *** �����T�C�g
                       IF      PIN1-REC(1:15)   =  WK-SITE
                               IF      WK-HTML1 (1:1) NOT = SPACE
      *    *** �^�C�g���f�[�^�o��
                                   PERFORM S140-10   THRU    S140-EX
                               END-IF
                       END-IF

      *    *** ����HP
                       IF      PIN1-REC(WK-PIN1-LEN - 8:09)  =  WK-SITE2
                               IF      WK-HTML1 (1:1) NOT = SPACE
      *    *** �^�C�g���f�[�^�o��
                                   PERFORM S140-10   THRU    S140-EX
                               END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       S130-EX.
           EXIT.

      *    *** �^�C�g���f�[�^�o��
       S140-10.
           MOVE    SPACE       TO      POT2-REC

           MOVE    12          TO      K
           MOVE    WK-HED1     TO      POT2-REC (1:12)

           ADD     +1          TO      K
           MOVE    WK-HTML1 (1:WK-HTML1-L) TO  POT2-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML2    TO      POT2-REC (K:WK-HTML2-L)

           ADD     WK-HTML2-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML6    TO      POT2-REC (K:WK-HTML6-L)

           ADD     WK-HTML6-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML7    TO      POT2-REC (K:WK-HTML7-L)

           ADD     WK-HTML7-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML8    TO      POT2-REC (K:WK-HTML8-L)

           ADD     WK-HTML8-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-HTML9    TO      POT2-REC (K:WK-HTML9-L)

           ADD     WK-HTML9-L  TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-OP       TO      POT2-REC (K:WK-OP-L)

           ADD     WK-OP-L     TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-ED       TO      POT2-REC (K:WK-ED-L)

           ADD     WK-ED-L     TO      K
           MOVE    " ,"        TO      POT2-REC (K:2)

           WRITE   POT2-REC
           ADD     1           TO      WK-POT2-CNT

      *****MOVE    SPACE       TO      WK-HTML1  WK-HTML2
      *****MOVE    SPACE       TO      WK-HTML3
           MOVE    SPACE       TO      WK-HTML4  WK-HTML5
           MOVE    1           TO      WK-HTML4-L
                                       WK-HTML5-L
           MOVE    SPACE       TO      WK-HTML1
                                       WK-HTML2
           MOVE    SPACE       TO      WK-HTML6  WK-HTML7
                                       WK-HTML8
                                       WK-OP
                                       WK-ED
           MOVE    ZERO        TO      WK-HTML9
           MOVE    1           TO      WK-HTML1-L
                                       WK-HTML2-L
           MOVE    1           TO      WK-HTML6-L
                                       WK-HTML7-L
                                       WK-HTML8-L
                                       WK-OP-L
                                       WK-ED-L

           MOVE    ZERO        TO      SW-SPAN-CLASS
                                       SW-CENTER
      *****                            SW-CAST
                                       WK-OP-L
                                       WK-ED-L
                                       WK-OP-KAKKO
                                       WK-OP-KAKKO-2
                                       WK-ED-KAKKO
                                       WK-ED-KAKKO-2
           MOVE    "N"         TO      SW-OP
                                       SW-ED
                                       SW-KAKKO
           .
       S140-EX.
           EXIT.

      *    *** �X�^�b�t�f�[�^�o��
       S150-10.
           MOVE    SPACE       TO      POT3-REC

           MOVE    12          TO      K
           MOVE    WK-HED1     TO      POT3-REC (1:12)

           ADD     +1          TO      K
           MOVE    WK-HTML1 (1:WK-HTML1-L) TO  POT3-REC (K:WK-HTML1-L)

           ADD     WK-HTML1-L  TO      K
           MOVE    " ,"        TO      POT3-REC (K:2)

           ADD     +2          TO      K
           MOVE    WK-STAFF    TO      POT3-REC (K:WK-STAFF-L)

           ADD     WK-STAFF-L  TO      K
           MOVE    " ,"        TO      POT3-REC (K:2)

           WRITE   POT3-REC
           ADD     1           TO      WK-POT3-CNT

           .
       S150-EX.
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
