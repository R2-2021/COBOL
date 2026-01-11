     IDENTIFICATION DIVISION.
     PROGRAM-ID. PRIME-CHECKER.

     DATA DIVISION.
     WORKING-STORAGE SECTION.
     01  NUMBER-INPUT      PIC 9(04).
     01  DIVISOR           PIC 9(04).
     01  SQUARE-ROOT       PIC 9(04).
     01  WK-REMAINDER         PIC 9(04).
     01  IS-PRIME          PIC X(03) VALUE 'YES'.

     PROCEDURE DIVISION.
     MAIN-LOGIC.
         DISPLAY "数値を入力してください（4桁まで）: ".
         ACCEPT NUMBER-INPUT.

         IF NUMBER-INPUT < 2
             MOVE 'NO' TO IS-PRIME
         ELSE
             COMPUTE SQUARE-ROOT = FUNCTION INTEGER(FUNCTION SQRT(NUMBER-INPUT)) + 1
             PERFORM VARYING DIVISOR FROM 2 BY 1 UNTIL DIVISOR > SQUARE-ROOT
                 DIVIDE NUMBER-INPUT BY DIVISOR GIVING SQUARE-ROOT REMAINDER WK-REMAINDER
                 IF WK-REMAINDER = 0
                     MOVE 'NO' TO IS-PRIME
                     EXIT PERFORM
                 END-IF
             END-PERFORM
         END-IF.

         DISPLAY "数値 " NUMBER-INPUT " は " END-DISPLAY
         
             IF IS-PRIME = 'YES' 
             THEN DISPLAY "素数です" END-DISPLAY
             ELSE DISPLAY "素数ではありません" END-DISPLAY
             END-IF
         STOP RUN.
