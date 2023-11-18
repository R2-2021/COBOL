      *    *** FizzBuzz

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FIZZBUZZ.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FIZZBUZZ".

           03  WK-SU           PIC  9(003) VALUE ZERO.
           03  WK-D3-DIVIDE    PIC  9(003) VALUE ZERO.
           03  WK-D3-AMARI     PIC  9(003) VALUE ZERO.
           03  WK-D5-DIVIDE    PIC  9(003) VALUE ZERO.
           03  WK-D5-AMARI     PIC  9(003) VALUE ZERO.

           03  WK-D3           PIC  9(3)V99 VALUE ZERO.
           03  WK-D3-R         REDEFINES WK-D3.
             05                PIC  9(003).
             05  WK-D3-SHOSU   PIC  9(002).

           03  WK-D5           PIC  9(3)V99 VALUE ZERO.
           03  WK-D5-R         REDEFINES WK-D5.
             05                PIC  9(003).
             05  WK-D5-SHOSU   PIC  9(002).

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       PROCEDURE   DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** FizzBuzz
           PERFORM S100-10     THRU    S100-EX

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

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-D3
           .
       S010-EX.
           EXIT.

      *    *** FizzBuzz
       S100-10.

      *    *** EVALUATE
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    SPACE       TO      WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           DISPLAY " "
           PERFORM VARYING WK-SU FROM 1 BY 1
                   UNTIL WK-SU > 100

                   COMPUTE WK-D3 = WK-SU / 3.0
                   COMPUTE WK-D5 = WK-SU / 5.0

                   EVALUATE TRUE
                       WHEN WK-D3-SHOSU = ZERO
                        AND WK-D5-SHOSU = ZERO
                           DISPLAY "FizzBuzz," WITH NO ADVANCING
                       WHEN WK-D3-SHOSU =       ZERO
                           DISPLAY "Fizz,"     WITH NO ADVANCING
                       WHEN WK-D5-SHOSU =       ZERO
                           DISPLAY "Buzz,"     WITH NO ADVANCING
                       WHEN OTHER
                           DISPLAY WK-SU ","   WITH NO ADVANCING
                   END-EVALUATE
           END-PERFORM

      *    *** IF
           DISPLAY " "
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "EVALUATE"  TO      WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           DISPLAY " "
           PERFORM VARYING WK-SU FROM 1 BY 1
                   UNTIL WK-SU > 100

                   COMPUTE WK-D3 = WK-SU / 3.0
                   COMPUTE WK-D5 = WK-SU / 5.0

                   IF      WK-D3-SHOSU = ZERO
                       AND WK-D5-SHOSU = ZERO
                           DISPLAY "FizzBuzz," WITH NO ADVANCING
                   ELSE
                       IF  WK-D3-SHOSU =       ZERO
                           DISPLAY "Fizz,"     WITH NO ADVANCING
                       ELSE
                           IF  WK-D5-SHOSU =       ZERO
                               DISPLAY "Buzz,"   WITH NO ADVANCING
                           ELSE
                               DISPLAY WK-SU "," WITH NO ADVANCING
                           END-IF
                       END-IF
                   END-IF
           END-PERFORM

      *    *** IF
           DISPLAY " "
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "IF-COMPUTE" TO     WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           DISPLAY " "
           PERFORM VARYING WK-SU FROM 1 BY 1
                   UNTIL WK-SU > 100

                   DIVIDE WK-SU BY 3 GIVING WK-D3-DIVIDE
                          REMAINDER WK-D3-AMARI
                   DIVIDE WK-SU BY 5 GIVING WK-D5-DIVIDE
                          REMAINDER WK-D5-AMARI

                   IF      WK-D3-AMARI = ZERO
                       AND WK-D5-AMARI = ZERO
                           DISPLAY "FizzBuzz," WITH NO ADVANCING
                   ELSE
                       IF  WK-D3-AMARI =       ZERO
                           DISPLAY "Fizz,"     WITH NO ADVANCING
                       ELSE
                           IF  WK-D5-AMARI =       ZERO
                               DISPLAY "Buzz,"   WITH NO ADVANCING
                           ELSE
                               DISPLAY WK-SU "," WITH NO ADVANCING
                           END-IF
                       END-IF
                   END-IF
           END-PERFORM

           DISPLAY " "
           MOVE    "L"         TO      WDT-DATE-TIME-ID
           MOVE    "IF-DIVIDE" TO      WDT-DATE-LUP-COM
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           DISPLAY " "
           .
         S100-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-D3

           DISPLAY WK-PGM-NAME " END"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
