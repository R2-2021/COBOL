     IDENTIFICATION DIVISION.
     PROGRAM-ID. PRIME-LIST.

     DATA DIVISION.
     WORKING-STORAGE SECTION.
     01  MAX-NUM           PIC 9(03) VALUE 100.
     01  NUM-TABLE.
         05 NUM-ENTRY      PIC X OCCURS 100 TIMES.
     01  I                 PIC 9(03).
     01  J                 PIC 9(03).
     01  K                 PIC 9(03).

     PROCEDURE DIVISION.
     MAIN-LOGIC.
         PERFORM INITIALIZE-TABLE
         PERFORM SIEVE-ALGORITHM
         PERFORM DISPLAY-PRIMES
         STOP RUN.

     INITIALIZE-TABLE.
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NUM
             MOVE 'P' TO NUM-ENTRY(I)
         END-PERFORM
         MOVE 'N' TO NUM-ENTRY(1).

     SIEVE-ALGORITHM.
         PERFORM VARYING I FROM 2 BY 1 UNTIL I * I > MAX-NUM
             IF NUM-ENTRY(I) = 'P'
                 COMPUTE K = I * I
                 PERFORM MARK-MULTIPLES UNTIL K > MAX-NUM
             END-IF
         END-PERFORM.

     MARK-MULTIPLES.
         MOVE 'N' TO NUM-ENTRY(K)
         COMPUTE K = K + I.

     DISPLAY-PRIMES.
         DISPLAY "1～100までの素数リスト:"
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NUM
             IF NUM-ENTRY(I) = 'P'
                 DISPLAY I " " WITH NO ADVANCING
             END-IF
         END-PERFORM
         DISPLAY " ".
