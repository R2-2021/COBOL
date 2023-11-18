       IDENTIFICATION DIVISION.
       PROGRAM-ID. corrdemo.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Item-1 VALUE 1 PIC 99V99.
       PROCEDURE DIVISION.
       100-Main SECTION.
       P1.
           ADD 19 81.43 TO Item-1
             ON SIZE ERROR
               DISPLAY 'Item-1:' Item-1
               DISPLAY 'Error: ' FUNCTION EXCEPTION-STATUS
               DISPLAY 'Where: ' FUNCTION EXCEPTION-LOCATION
               DISPLAY ' What: ' FUNCTION EXCEPTION-STATEMENT
               END-ADD.
           STOP RUN.
