      *    *** MissAV用 TEST103.Ｍｉｓｓａｖ.PIN2 女優名追加データ自動作成
      *    *** 

      *    *** TEST132
      *    ***    |
      *    *** TEST104
      *    ***    |
      *    *** TEST53
      *    ***    |
      *    *** TEST54
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST132.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST10.POT1
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.


      *    *** TEST104 PIN2 データ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST132 ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST132.POT1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-NAME         OCCURS 5
                               PIC  X(100) VALUE SPACE.
           03  WK-NAME-LEN     OCCURS 5
                               BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEQNUM       BINARY-LONG SYNC VALUE ZERO.
           03  WK-SEQNUM2      PIC  9(001) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.
           03  K1-MAX          BINARY-LONG SYNC VALUE ZERO.
           03  K2              BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  P               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-MISSAV       PIC  X(001) VALUE "N".
           03  SW-END          PIC  X(001) VALUE "N".
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1000.
             05  TBL01-NAME    PIC  X(100) VALUE SPACE.
             05  TBL01-NAME-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-SEQNUM  BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** START DISPLAY,OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL SW-MISSAV =   "Y"
                      OR WK-PIN1-EOF = HIGH-VALUE

      *    *** MissAV女優名あ
                   IF      PIN1-REC (1:18) =
                           X"4D6973734156E5A5B3E584AAE5908DE38182"
                           MOVE    "Y"         TO      SW-MISSAV
                   END-IF

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

           PERFORM UNTIL PIN1-REC (1:4) = "<H3 "
                      OR WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** <H3 の１件後READ
      *    *** READ PIN1
           IF      WK-PIN1-EOF NOT = HIGH-VALUE
                   PERFORM S020-10     THRU    S020-EX
           END-IF

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                      OR SW-END = "Y"

                   IF      PIN1-REC (1:6) =    "MissAV"
                        OR PIN1-REC (1:7) =    "#MissAV"
                        OR WK-PIN1-LEN = 3

      *    *** #MissAV男優名
                           IF      PIN1-REC (1:16) =    
                                   X"234D6973734156E794B7E584AAE5908D"
                                   MOVE    "Y"         TO      SW-END
                           ELSE
                                   CONTINUE
                           END-IF
                   ELSE
      *    *** TBL01 SET
                           PERFORM S100-10     THRU    S100-EX
                   END-IF

                   PERFORM UNTIL PIN1-REC (1:4) = "<H3 "
                              OR WK-PIN1-EOF = HIGH-VALUE

      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

      *    *** <H3 の１件後READ
      *    *** READ PIN1
                   IF      WK-PIN1-EOF NOT = HIGH-VALUE
                           PERFORM S020-10     THRU    S020-EX
                   END-IF
           END-PERFORM

      *    *** WRITE POT1
           PERFORM S110-10     THRU    S110-EX

      *    *** CLOSE,END DISPLAY
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

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
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** TBL01 SET
       S100-10.

           MOVE    SPACE       TO      WK-NAME (1)
                                       WK-NAME (2)
                                       WK-NAME (3)
                                       WK-NAME (4)
                                       WK-NAME (5)
           MOVE    ZERO        TO      WK-NAME-LEN (1)
                                       WK-NAME-LEN (2)
                                       WK-NAME-LEN (3)
                                       WK-NAME-LEN (4)
                                       WK-NAME-LEN (5)

           UNSTRING PIN1-REC
      *    *** ・無修正
                   DELIMITED BY X"E383BBE784A1E4BFAEE6ADA3"
                             OR SPACE
      *    *** （
                             OR X"EFBC88"
      *    *** 、
                             OR X"E38081"
      *    *** ）・ＳＭ・乱交
                        OR X"EFBC89E383BBEFBCB3EFBCADE383BBE4B9B1E4BAA4"
      *    *** ）
                             OR X"EFBC89"
      *    *** ・単体
                             OR X"E383BBE58D98E4BD93"
      *    *** ・複数
                             OR X"E383BBE8A487E695B0"
      *    *** ・イメージ
                             OR X"E383BBE382A4E383A1E383BCE382B8"
      *    *** ・
                             OR X"E383BB"
                   INTO
                   WK-NAME (1) COUNT WK-NAME-LEN (1)
                   WK-NAME (2) COUNT WK-NAME-LEN (2)
                   WK-NAME (3) COUNT WK-NAME-LEN (3)
                   WK-NAME (4) COUNT WK-NAME-LEN (4)
                   WK-NAME (5) COUNT WK-NAME-LEN (5)

           END-UNSTRING

           MOVE    -1        TO      WK-SEQNUM
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 5
               IF      WK-NAME-LEN (I) > ZERO
                   MOVE    "N"         TO      SW-SEARCH
                   PERFORM VARYING K2 FROM 1 BY 1
                           UNTIL K2 > K1
                              OR SW-SEARCH = "Y"
                       IF      WK-NAME (I) = TBL01-NAME (K2)
                               MOVE    "Y"         TO      SW-SEARCH
                       ELSE
                               CONTINUE
                       END-IF
                   END-PERFORM

                   IF      SW-SEARCH   =       "N"
                       ADD     1           TO      K1
                       IF      K1          >       1000
                               DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                               STOP    RUN
                       ELSE
                               MOVE    WK-NAME (I) TO TBL01-NAME (K1)
                               MOVE    WK-NAME-LEN (I) TO
                                       TBL01-NAME-LEN (K1)
                               ADD     1           TO      WK-SEQNUM
                               MOVE    WK-SEQNUM   TO
                                       TBL01-SEQNUM (K1)
                       END-IF
                   ELSE
                       CONTINUE
                   END-IF
               ELSE
                   CONTINUE
               END-IF
           END-PERFORM

           MOVE    K1          TO      K1-MAX
           .
       S100-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           PERFORM VARYING K1 FROM 1 BY 1
                   UNTIL K1 > K1-MAX
                   MOVE    1           TO      P
                   MOVE    TBL01-NAME (K1) TO  POT1-REC
                   COMPUTE P = P + TBL01-NAME-LEN (K1)

                   MOVE    ","         TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    TBL01-NAME-LEN (K1) TO L
                   MOVE    TBL01-NAME (K1) TO  POT1-REC (P:L)
                   COMPUTE P = P + TBL01-NAME-LEN (K1)

                   MOVE    ","         TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    TBL01-SEQNUM (K1) TO WK-SEQNUM2
                   MOVE    WK-SEQNUM2  TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   MOVE    ","         TO      POT1-REC (P:1)
                   ADD     1           TO      P

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S110-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PIN1-F
                   POT1-F

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 件数 = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
