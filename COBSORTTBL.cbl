      *    *** SORT TABLE —p

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSORTTBL.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
                               STATUS   WK-PRM1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           LABEL RECORDS ARE STANDARD.
       01  PRM1-REC.
           03  PRM1-PRM        PIC  X(003).
           03  FILLER          PIC  X(077).

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(1024).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-DATA.
             05  FILLER        PIC  X(1024).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(010) VALUE "COBSORTTBL".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "COBSORTTBL.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32X.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSORTTBL.POT1".

           03  WK-PRM1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-TBL01-EOF    PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.


           03  WK-PRM          PIC  X(003) VALUE SPACE.

      *    *** 100,000 Œ@10.03•b
      *    *** OCCURS 1,000,000 ‚Å‚àˆ—‰Â”\
      *    *** KEY X TYPE‚Ì‚Ý
       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 1 TO 10000 TIMES
                               DEPENDING ON TBL01-IDX-MAX
                               ASCENDING KEY TBL01-KEY1
                                             TBL01-KEY2
                                             TBL01-KEY3
                               INDEXED BY TBL01-IDX.
             05  TBL01-KEY1    PIC  X(010) VALUE SPACE.
             05  TBL01-KEY2    PIC  X(010) VALUE SPACE.
             05  TBL01-KEY3    PIC  X(010) VALUE SPACE.
             05  TBL01-DATA    PIC  X(1024) VALUE SPACE.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA,
      *     03  TBL01-IDX-MAX   BINARY-LONG SYNC VALUE ZERO.
           03  TBL01-IDX-MAX   BINARY-LONG SYNC VALUE ZERO.
      *    *** INDEXED BY ‹å‚Ì’è‹`•s—v
      *     03  TBL01-IDX           BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.

           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.

           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P3              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA,
           03  SW-YES          PIC  X(001) VALUE "N".
    
       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** PRM1 READ
           PERFORM S020-10     THRU    S020-EX

      *    *** READ,TBL SET
           PERFORM S030-10     THRU    S030-EX

           EVALUATE WK-PRM
               WHEN "A  "
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1

               WHEN "D  "
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 

               WHEN "AA "
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 

               WHEN "AD "
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 

               WHEN "DA "
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 

               WHEN "DD "
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 

               WHEN "AAA"
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 
                   ASCENDING  KEY TBL01-KEY3 

               WHEN "AAD"
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 
                   DESCENDING KEY TBL01-KEY3 

               WHEN "ADA"
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 
                   ASCENDING  KEY TBL01-KEY3 

               WHEN "ADD"
                   SORT    TBL01-AREA 
                   ASCENDING  KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 
                   DESCENDING KEY TBL01-KEY3 

               WHEN "DAA"
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 
                   ASCENDING  KEY TBL01-KEY3 

               WHEN "DAD"
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   ASCENDING  KEY TBL01-KEY2 
                   DESCENDING KEY TBL01-KEY3 

               WHEN "DDA"
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 
                   ASCENDING  KEY TBL01-KEY3 

               WHEN "DDD"
                   SORT    TBL01-AREA 
                   DESCENDING KEY TBL01-KEY1 
                   DESCENDING KEY TBL01-KEY2 
                   DESCENDING KEY TBL01-KEY3 

           END-EVALUATE

      *    *** TBL WRITE
           PERFORM S200-10     THRU    S200-EX

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

           MOVE    "N"         TO      SW-YES
           PERFORM UNTIL SW-YES =      "Y"
                   DISPLAY " "
                   DISPLAY WK-PGM-NAME " INPUT FILE NAME"
                   ACCEPT  WK-PIN1-F-NAME
                   DISPLAY WK-PGM-NAME " FILE NAME=" WK-PIN1-F-NAME
                           " OK ? Y/N"
                   ACCEPT  SW-YES
           END-PERFORM

      *    *** SORT-F ‚ÍOPEN ‚¢‚ç‚È‚¢

           OPEN    INPUT       PRM1-F
           IF      WK-PRM1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PRM1-F OPEN ERROR STATUS="
                           WK-PRM1-STATUS
                   STOP    RUN
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

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC
           .
       S010-EX.
           EXIT.

      *    *** PRM1 READ
       S020-10.
      *    *** ‚PŒ–Ú
           READ    PRM1-F

           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
                   IF      WK-PRM1-STATUS =    10
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF    ( PRM1-REC(1:1) =     "A" OR "D" ) AND
                 ( PRM1-REC(2:1) =     "A" OR "D" OR SPACE ) AND
                 ( PRM1-REC(3:1) =     "A" OR "D" OR SPACE )
                   MOVE    PRM1-REC(1:3) TO    WK-PRM
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F READ PARA ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF

      *    *** ‚QŒ–Ú KEY‚P‚Â–Ú
           READ    PRM1-F
           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
                   IF      WK-PRM1-STATUS =    10
                           CONTINUE
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF      PRM1-REC(1:3) IS NUMERIC AND
                   PRM1-REC(5:3) IS NUMERIC
                   MOVE    FUNCTION NUMVAL(PRM1-REC(1:3)) TO P1
                   MOVE    FUNCTION NUMVAL(PRM1-REC(5:3)) TO L1
                   IF      WK-PRM(2:1) = SPACE
                           EXIT    PARAGRAPH
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY1 PARA ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF

      *    *** ‚QŒ–Ú KEY‚Q‚Â–Ú
           READ    PRM1-F
           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
                   IF      WK-PRM1-STATUS =    10
                           EXIT    PARAGRAPH
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF    ( WK-PRM(2:1) =       "A" OR "D" ) AND
                   PRM1-REC(1:3) IS NUMERIC AND
                   PRM1-REC(5:3) IS NUMERIC
                   MOVE    FUNCTION NUMVAL(PRM1-REC(1:3)) TO P2
                   MOVE    FUNCTION NUMVAL(PRM1-REC(5:3)) TO L2
                   IF      WK-PRM(3:1) = SPACE
                           EXIT    PARAGRAPH
                   END-IF
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY2 PARA ERROR="
                           WK-PRM
                   DISPLAY WK-PGM-NAME " PRM1-F KEY2 PARA ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF

      *    *** ‚RŒ–Ú KEY‚R‚Â–Ú
           READ    PRM1-F
           IF      WK-PRM1-STATUS =    ZERO
                   ADD     1           TO      WK-PRM1-CNT
           ELSE
                   IF      WK-PRM1-STATUS =    10
                           EXIT    PARAGRAPH
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PRM1-F READ ERROR STATUS="
                                   WK-PRM1-STATUS
                           STOP    RUN
                   END-IF
           END-IF

           IF    ( WK-PRM(3:1) =       "A" OR "D" ) AND
                   PRM1-REC(1:3) IS NUMERIC AND
                   PRM1-REC(5:3) IS NUMERIC
                   MOVE    FUNCTION NUMVAL(PRM1-REC(1:3)) TO P3
                   MOVE    FUNCTION NUMVAL(PRM1-REC(5:3)) TO L3
           ELSE
                   DISPLAY WK-PGM-NAME " PRM1-F KEY3 PARA ERROR="
                           WK-PRM
                   DISPLAY WK-PGM-NAME " PRM1-F KEY3 PARA ERROR="
                           PRM1-REC
                   STOP    RUN
           END-IF

           .
       S020-EX.
           EXIT.

      *    *** READ , TBL SET
       S030-10.

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   READ    PIN1-F

                   IF      WK-PIN1-STATUS =    ZERO
                           ADD     1           TO      WK-PIN1-CNT
                           MOVE    PIN1-REC(P1:L1) TO
                                   TBL01-KEY1 (TBL01-IDX)

                           IF      WK-PRM(2:1) =         "A" OR "D"
                               MOVE    PIN1-REC(P2:L2) TO
                                       TBL01-KEY2 (TBL01-IDX)
                           END-IF 

                           IF      WK-PRM(3:1) =         "A" OR "D"
                               MOVE    PIN1-REC(P3:L3) TO
                                       TBL01-KEY3 (TBL01-IDX)
                           END-IF

                           MOVE    PIN1-DATA   TO  TBL01-DATA(TBL01-IDX)
      *    *** TBL01-IDX ‰Šú’l@‚P‚ª“ü‚Á‚Ä‚¢‚é
                           SET     TBL01-IDX   UP BY   1
                   ELSE
                       IF  WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                           MOVE    WK-PIN1-CNT TO      TBL01-IDX-MAX
                       ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                       END-IF
                   END-IF
           END-PERFORM
           .
       S030-EX.
           EXIT.

      *    *** TBL WRITE
       S200-10.

      *    *** INDEXED BY ‚Ì“YŽšAVARYING‚ÅŽg—p‰Â
           PERFORM VARYING TBL01-IDX FROM 1 BY 1
               UNTIL TBL01-IDX > TBL01-IDX-MAX
                   MOVE    TBL01-DATA (TBL01-IDX) TO  POT1-DATA

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT

                   IF      WK-POT1-STATUS NOT =  ZERO
                           DISPLAY WK-PGM-NAME
                                   " POT1-F WRITE ERROR STATUS="
                                   WK-POT1-STATUS
                           STOP    RUN
                   END-IF
           END-PERFORM
           .
       S200-EX.
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
           DISPLAY WK-PGM-NAME " PRM1 ¹Ý½³ = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ¹Ý½³ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ¹Ý½³ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
