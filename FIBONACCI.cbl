      *    *** FIBONACCI

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FIBONACCI.

       DATA                    DIVISION.

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(012) VALUE "FIBONACCI".

           03  WK-N-DISP       PIC  Z,ZZ9 VALUE ZERO.
           03  WK-NUMBER-DISP  PIC  ZZZ,ZZ9 VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  TABLE-AREA.
           03  TBL-FI          OCCURS 20
                               PIC S9(36) PACKED-DECIMAL VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

           MOVE    1           TO      TBL-FI (1)
                                       TBL-FI (2)
           PERFORM VARYING I FROM 1 BY 1
      *    *** 173 ‚ªDISPLAY 36Œ… MAX
                   UNTIL   I > 20

                   IF      I            =      1 OR 2
                           MOVE    I           TO      WK-N-DISP
      *                     DISPLAY "N=" WK-N-DISP " " WITH NO ADVANCING
                           MOVE    TBL-FI (I)  TO      WK-NUMBER-DISP
                           DISPLAY WK-NUMBER-DISP " " WITH NO ADVANCING
                   ELSE
                           MOVE    I           TO      WK-N-DISP
      *                     DISPLAY "N=" WK-N-DISP " " WITH NO ADVANCING
                           COMPUTE TBL-FI (I) = TBL-FI (I - 2)
                                              + TBL-FI (I - 1)
                           COMPUTE WK-NUMBER-DISP = 
                                   TBL-FI (I - 2) + TBL-FI (I - 1)
                           DISPLAY WK-NUMBER-DISP " " WITH NO ADVANCING
                   END-IF
           END-PERFORM
           .
       M100-EX.
           STOP    RUN.
