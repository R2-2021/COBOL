      *    *** PRIME_NUMBER

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PRIME_NUMBER.

       DATA                    DIVISION.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(012) VALUE "PRIME_NUMBER".

           03  WK-NUMBER-DISP  PIC  ZZZ9   VALUE ZERO.
           03  WK-NUMBER       BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI        BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-HIT          PIC  X(001) VALUE "N".

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I2-MAX          BINARY-LONG SYNC VALUE ZERO.

       01  TABLE-AREA.
           03  TBL01-AREA      OCCURS 1000.
             05  TBL01-NUMBER  BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

           PERFORM VARYING I FROM 2 BY 1
                   UNTIL   I > 1000
                   MOVE    "N"         TO      SW-HIT
                   PERFORM VARYING I2 FROM 1 BY 1
                           UNTIL   I2 > I2-MAX
                                OR SW-HIT = "Y"
                           DIVIDE I BY  TBL01-NUMBER (I2) 
                                   GIVING WK-NUMBER
                                   REMAINDER WK-AMARI
                           IF      WK-AMARI    =       ZERO
                                   MOVE    "Y"         TO      SW-HIT
                           END-IF
                   END-PERFORM
                   IF      SW-HIT      =       "N"
                           MOVE    I           TO      WK-NUMBER-DISP
                           DISPLAY WK-NUMBER-DISP "," WITH NO ADVANCING
                           MOVE    I           TO      TBL01-NUMBER (I2)
                           MOVE    I2          TO      I2-MAX
                   END-IF
           END-PERFORM
           .
       M100-EX.
           STOP    RUN.

