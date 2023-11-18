      *    *** FIBONACCI

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FIBONACCI2.

       DATA                    DIVISION.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(012) VALUE "FIBONACCI".

           03  WK-NUMBER-DISP  PIC  ZZZ,ZZZ,ZZZ,ZZZ,ZZ9 VALUE ZERO.
           03  WK-FIBONACCI    PIC S9(015) PACKED-DECIMAL VALUE ZERO.
           03  WK-R5           PIC S9V9(10) PACKED-DECIMAL VALUE ZERO.
           03  WK-R5P          PIC S9V9(10) PACKED-DECIMAL VALUE ZERO.
           03  WK-R5M          PIC S9V9(10) PACKED-DECIMAL VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

           COMPUTE WK-R5  ROUNDED = 5 ** 0.5
           COMPUTE WK-R5P ROUNDED = (1 + WK-R5) / 2.0
           COMPUTE WK-R5M ROUNDED = (1 - WK-R5) / 2.0

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I > 20
                   COMPUTE WK-FIBONACCI ROUNDED = 
                         ( WK-R5P ** I - WK-R5M ** I ) / WK-R5

                   MOVE    WK-FIBONACCI TO     WK-NUMBER-DISP
                   DISPLAY WK-NUMBER-DISP " " WITH NO ADVANCING
           END-PERFORM
           .
       M100-EX.
           STOP    RUN.
