      *    *** COBRND サブルーチンチェック
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBRNDCK.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBRNDCK".
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

      *    *** seed usage float-long にしたら、ランダム値出る様になった
      * 01  random-SEED          PIC 99V999999999 VALUE 10.
      * 01  random-SEED          usage BINARY-LONG.
       01  random-SEED         PIC  9(015) VALUE ZERO.
       01  random-float        float-long.
       01  random-integer      pic  9(002).
       01  tally1              pic  9(003).

       01  Bin-Item-1          PIC  9(003) COMP-5 VALUE 32760.
       01  Disp-Item-1         PIC  9(006).

       01  results.
           05 hits             pic  9(009) occurs 10 times.
       01  first-ten           pic  9(002).

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==WFD==.

           COPY    CPCOBRND   REPLACING ==:##:== BY ==WCR==.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA.
             05  TBL01-I1      OCCURS 10
                               PIC  X(020) VALUE "12345678901234567890".
       PROCEDURE   DIVISION.
       M100-10.
           DISPLAY WK-PGM-NAME " START"

      *    *** FILEDUMP OPEN
           MOVE    "O"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    1           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA

      *    *** FILEDUMP 項目出力(ID="X") 
      *     MOVE    "X"         TO      WFD-ID
      *     MOVE    1           TO      WFD-SU
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 WK-DATA2

      *     CALL    "COBDUMP"   USING   TBL01-AREA
      *                                 WK-LEN

           MOVE    "STR"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

                              
           MOVE    1           TO      WCR-IDX
           MOVE    2000        TO      WCR-FROM   (1)
           MOVE    100         TO      WCR-TO-CNT (1)
           MOVE    1           TO      WCR-BETWEEN(1)
           MOVE    SPACE       TO      WCR-SIGN   (1)
           MOVE    "N"         TO      WCR-ZERO   (1)
           MOVE    1           TO      WCR-FROM2  (1)
           MOVE    10          TO      WCR-TO2    (1)


      *     *** compute random-float =  FUNCTION random()
      *     *** 使用の時の、出現回数、バラツキが出る
      *C:\Users\xxxx\Documents\COBOL>COBRNDCK
      *COBXREF START
      *05 02 07 03 02 02 02 03 03 03 ...
      *001: 000000033
      *002: 000339452
      *003: 000338984
      *004: 000118256
      *005: 000034085
      *006: 000033617
      *007: 000033709
      *008: 000034215
      *009: 000033875
      *010: 000033774
      *COBRNFCK END

      *    *** CALL    "COBRND"    USING   WCR-COBRND-AREA 
      *    *** 使用の時の、出現回数
      *C:\Users\xxxx\Documents\COBOL>COBRNDCK
      *COBXREF START
      *02 10 05 09 05 09 01 04 08 08 ...
      *001: 000099180
      *002: 000098723
      *003: 000094378
      *004: 000102639
      *005: 000109455
      *006: 000107976
      *007: 000100205
      *008: 000092405
      *009: 000101672
      *010: 000093367
      *COBRNFCK END


           DISPLAY " * 100"
      *> compute random-float = random(0)
      *     perform 100000 times
           perform varying i from 1 by 1
                until i > 10000
      *         compute random-float =  FUNCTION random() * 10.0
      *         compute random-float =  FUNCTION random(random-SEED)
                               MOVE    "RND"       TO      WCR-ID
                               CALL    "COBRND"    USING WCR-COBRND-AREA
      *    *** seed i に変更しても、バラツキ　random()の時と同じ
      *         compute random-float =  FUNCTION random(i)
                compute random-float =  WCR-RND(1) 
                                     * 10.0
                COMPUTE Random-SEED = random-float * 100
                
      *         MOVE FUNCTION random() TO  Random-SEED
      *         DISPLAY Random-SEED " "
      *                 Random-float
               compute random-integer = random-float + 1.0
               if random-integer < 1 or > 10 then
                   display "anomaly: " random-integer upon syserr
               end-if
               add 1 to hits(random-integer)
               if first-ten < 10 then
                   display random-integer space with no advancing
                   add 1 to first-ten
               end-if
           end-perform
           display "..."

           perform varying tally1 from 1 by 1 until tally1 > 10
               display tally1 ": " hits(tally1)
           end-perform

           MOVE Bin-Item-1 TO Disp-Item-1
           DISPLAY 'Bin-Item-1=' Bin-Item-1 ' Disp-Item-1=' Disp-Item-1
           END-DISPLAY

           ADD 5 TO Bin-Item-1
           MOVE Bin-Item-1 TO Disp-Item-1
           DISPLAY 'Bin-Item-1=' Bin-Item-1 ' Disp-Item-1=' Disp-Item-1
           END-DISPLAY

           MOVE 32767 TO Bin-Item-1
           MOVE Bin-Item-1 TO Disp-Item-1
           DISPLAY 'Bin-Item-1=' Bin-Item-1 ' Disp-Item-1=' Disp-Item-1

           DISPLAY FUNCTION PI
           DISPLAY FUNCTION E

           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *
       S900-10.

           MOVE    "END"       TO      WCR-ID
           CALL    "COBRND"    USING   WCR-COBRND-AREA

      *    *** FILEDUMP CLOSE
           MOVE    "C"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL01-AREA
           DISPLAY WK-PGM-NAME " END"
           .
       S900-EX.
           EXIT.
