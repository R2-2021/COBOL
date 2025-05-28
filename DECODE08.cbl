      *    *** NETFLIX データ読み取り

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE08.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** NETFLIX データ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE08".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST53_NETFLIX.PIN1".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-K1           PIC  X(100) VALUE SPACE.
           03  WK-K2           PIC  X(100) VALUE SPACE.
           03  WK-K3           PIC  X(100) VALUE SPACE.

           03  WK-K1-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-K2-LEN       BINARY-LONG SYNC VALUE ZERO.
           03  WK-K3-LEN       BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

      *     COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  K1              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

      *    *** NETFLIX
       01  TBL-AREA5.
           03  TBL01-AREA      OCCURS 300
                               ASCENDING KEY IS TBL01-NAME
                               INDEXED BY TBL01-IDX.
             05  TBL01-NAME    PIC  X(100) VALUE HIGH-VALUE.
             05  TBL01-NAME-LEN BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-NFADDR  PIC  X(100) VALUE SPACE.
             05  TBL01-NFADDR-LEN BINARY-LONG SYNC VALUE ZERO.

       LINKAGE                 SECTION.

           COPY    CPDECODE08  REPLACING ==:##:== BY ==LDE08==.

       PROCEDURE               DIVISION
                   USING       LDE08-DECODE08-AREA.
       M100-10.

           EVALUATE LDE08-ID

               WHEN "OPEN  "

      *    *** OPEN
                   PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           IF      WK-PIN1-LEN =       ZERO
                                OR PIN1-REC (1:1) =   "%"
      *    *** ジャパリ
                                OR PIN1-REC(1:12) = 
                                   X"E382B8E383A3E38391E383AA"
                                OR WK-K2   (1:1)  = SPACE
                                   CONTINUE
      *    *** TBL01 SET
                           ELSE
                                   PERFORM S022-10     THRU    S022-EX
                           END-IF
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

      *    *** TBL01 SORT
                   SORT    TBL01-AREA
                           ASCENDING KEY TBL01-NAME

               WHEN "SEARCH"

      *    *** SEARCH
                   PERFORM S100-10     THRU    S100-EX

               WHEN "CLOSE "
      *    *** CLOSE
                   PERFORM S900-10     THRU    S900-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDE08-ID PARA ERROR="
                           LDE08-ID
                   DISPLAY WK-PGM-NAME 
                           " LDE08-ID OPEN,SEARCH,CLOSE 指定"
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN
       S010-10.

      *     DISPLAY WK-PGM-NAME " START"
      *     MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
      *     MOVE    "S"         TO      WDT-DATE-TIME-ID
      *     CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           OPEN    INPUT       PIN1-F

      *     MOVE    "O"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           MOVE    SPACE       TO      WK-K1
                                       WK-K2
                                       WK-K3
           MOVE    ZERO        TO      WK-K1-LEN
                                       WK-K2-LEN
                                       WK-K3-LEN

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT  AT  END
                   ADD     1           TO      WK-PIN1-CNT

                   UNSTRING PIN1-REC
                           DELIMITED BY ","
                           INTO
                           WK-K1    COUNT WK-K1-LEN
                           WK-K2    COUNT WK-K2-LEN
                           WK-K3    COUNT WK-K3-LEN
           END-READ
           .
       S020-EX.
           EXIT.

       S022-10.

           ADD     1           TO      K1
           IF      K1          >       300
                   DISPLAY WK-PGM-NAME " TBL01 OVER K1=" K1
                   STOP    RUN
           END-IF

           MOVE    WK-K1       TO      TBL01-NAME       (K1)
           MOVE    WK-K1-LEN   TO      TBL01-NAME-LEN   (K1)
           MOVE    WK-K2       TO      TBL01-NFADDR     (K1)
           MOVE    WK-K2-LEN   TO      TBL01-NFADDR-LEN (K1)
           .
       S022-EX.
           EXIT.

      *    *** TBL SET
       S100-10.

           SEARCH  ALL TBL01-AREA
               AT  END
                   MOVE    "N"         TO      LDE08-SEARCH
                   MOVE    SPACE       TO      LDE08-NFADDR
                   MOVE    ZERO        TO      LDE08-NFADDR-LEN

               WHEN TBL01-NAME (TBL01-IDX) (1:TBL01-NAME-LEN(TBL01-IDX))
                 =  LDE08-NAME (1:LDE08-NAME-LEN)

                   MOVE    "Y"         TO      LDE08-SEARCH
                   MOVE    TBL01-NFADDR (TBL01-IDX) TO
                           LDE08-NFADDR
                   MOVE    TBL01-NFADDR-LEN (TBL01-IDX) TO 
                           LDE08-NFADDR-LEN
           END-SEARCH
           .
       S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F

      *     MOVE    "C"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 件数 = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"

      *     MOVE    "E"         TO      WDT-DATE-TIME-ID
      *     CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
