      *    *** CALL ªÃﬁŸ∞¡› TEST04S1.dllçÏÇÈ
      *    *** COBC TEST04S1

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST04S1.

       DATA                    DIVISION.
       LINKAGE                 SECTION.
       01  LK-PAR1             PIC   9(005).
       01  LK-PAR2             PIC  S9(005)V99.
       01  LK-PAR3             PIC  S9(005)V99.
       01  LK-PAR4             PIC  S9(005)V99.

       PROCEDURE   DIVISION    USING   LK-PAR1
                                       LK-PAR2
                                       LK-PAR3
                                       LK-PAR4
           .
       M100-10.

           EVALUATE LK-PAR1
               WHEN 1
                   COMPUTE LK-PAR4 =   LK-PAR2 + LK-PAR3
               WHEN 2
                   COMPUTE LK-PAR4 =   LK-PAR2 - LK-PAR3
               WHEN 3
                   COMPUTE LK-PAR4 =   LK-PAR2 * LK-PAR3
               WHEN OTHER
                   COMPUTE LK-PAR4 =   LK-PAR2 / LK-PAR3
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.
