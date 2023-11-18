      *    *** ï∂éöÉ^ÉCÉvÉ`ÉFÉbÉN USAGE 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBSAM02.

       ENVIRONMENT             DIVISION.
      * Configuration section.
      * repository.
      *     function all intrinsic.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(10000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03  PIN2-DATA       PIC  X(10000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD
      *     RECORD VARYING DEPENDING ON WK-POT1-LEN.
           .
       01  POT1-REC.
           03  POT1-DATA       PIC  X(10000).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-POT2-LEN.
       01  POT2-REC.
      *    *** 100BYTE Ç‹Ç≈Ç»ÇÁOKÇæÇ™ÅA200BYTE à»è„ÇæÇ∆1åèñ⁄ÇÕç∂äÒÇπÇæÇ™ÅA
      *    *** 2åèñ⁄à»ç~âEäÒÇπÇ…Ç»Ç¡ÇƒÇµÇ‹Ç§ÅAFILEDUMPÇ≈ÇÕñ‚ëËÇ»Çµ
           03  POT2-DATA       PIC  X(200).
      *     03  POT2-DATA       PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBSAM02".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST32.POT1".
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST10.POT1".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST38.POT1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "COBSAM02.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "COBSAM02.POT2".

           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-DATA1        PIC  X(010) VALUE "ABC".
      *    *** & ÇÕ ALL Ç™Ç†ÇÈÇ∆ÉGÉâÅ[Ç…Ç»ÇÈ
      *     03  WK-DATA2        PIC  X(010) VALUE  ALL "ABC" & "DEF".
           03  WK-DATA2        PIC  X(010) VALUE "ABC" & "DEF".
           03  WK-DATA3        PIC  9(010) VALUE ZERO.
           03  WK-DATA4        PIC  9V9(5) VALUE ZERO.
           03  WK-DATA5        PIC S9(9)V99 VALUE ZERO.
           03  WK-DATA5-X      REDEFINES WK-DATA5
                               PIC  X(011).
           03  WK-DATA6        PIC  X(010) VALUE SPACE.
           03  WK-DATA7.
             05                PIC  X(010) VALUE "ABC       ".
           03  WK-DATA09       PIC  X(010) VALUE "A112345678".
           03  WK-DATA10       PIC  X(010) VALUE "AABBCDEFFF".

           03  WK-SEED         PIC  9(006) VALUE ZERO.
           03  WK-TESTNO       PIC  X(002) VALUE ZERO.

      *     03  WK-ESU1         PIC +9.9E+99 VALUE -5.4E-79.
      *     03  WK-ESU2         PIC +9.9(5)E+99 VALUE 12.34567E+00.
      *     03  WK-ESU3         PIC +9(8)VE-99 VALUE +12345678E-09.

      *    *** PIC Ç…EÇÕéwíËèoóàÇ»Ç¢
      *     03  WK-ESU1         PIC +9.9E+99 VALUE ZERO.
      *     03  WK-ESU2         PIC +99.9(5)E-99 VALUE ZERO.
      *     03  WK-ESU3         PIC +9(8)VE-99 VALUE ZERO.

       01  WK-BUF2.
           03  WK-BUF2-L-TBL.
             05  WK-BUF2-L     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-R-TBL.
             05  WK-BUF2-R     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-LR-TBL.
             05  WK-BUF2-LR-TBL2 OCCURS 65536.
               07  WK-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WK-BUF2-R2  PIC  X(001) VALUE SPACE.

       01  WDE04-AREA.
           03  WDE04-SHORI     PIC  X(005) VALUE "FIRST".
           03  WDE04-BUF1-LEN  BINARY-LONG SYNC VALUE ZERO.

           03  WDE04-REC-LEN   BINARY-LONG SYNC VALUE ZERO.

      *    *** ïœä∑ëO Ç™ì¸Ç¡ÇƒÇ¢ÇÈÉfÅ[É^
      * 01  WDE05-BUF1             PIC  X(010) VALUE SPACE.
       01  WK-WDE05-BUF1             PIC  X(010).

      *    *** ïœä∑å„ Ç™ì¸Ç¡ÇƒÇ¢ÇÈÉfÅ[É^
       01  WK-WDE05-BUF2             PIC  X(010) VALUE SPACE.

       01  form pic $-z(7)9.9(8).

       01  newline       PIC x VALUE x"0a"

      *    *** 38 ï∂éöÇ‹Ç≈éwíËâ¬î\
       01 big-value pic $$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$$,$$9.99.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDATEWEEK  REPLACING ==:##:== BY ==WDW==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

           COPY    CPDECODE07  REPLACING ==:##:== BY ==WDE07==.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES
                               PIC  X(001).

       01  Left-Nibble         COMP-5
                               PIC  9(002).
       01  Nibble              REDEFINES Left-Nibble 
                               BINARY-CHAR.

       01  Right-Nibble        COMP-5 
                               PIC  9(002).


       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUES.
           05  PIC-X           PIC X.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 100
                               ASCENDING KEY IS TBL01-KEY
                               INDEXED BY TBL01-IDX.

             05  TBL01-I       PIC  9(003) VALUE ZERO.
             05  TBL01-KEY     PIC  9(003) VALUE ZERO.
             05  TBL01-DATA    PIC  9(003) VALUE ZERO.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG SYNC VALUE 1.

           03  CNS-0B          BINARY-LONG SYNC VALUE ZERO.
           03  CNS-1B          BINARY-LONG SYNC VALUE 1.
           03  CNS-2B          BINARY-LONG SYNC VALUE 2.
           03  CNS-012B        BINARY-LONG SYNC VALUE 012.

           03  CNS-09          PIC  9(001) VALUE ZERO.
           03  CNS-19          PIC  9(001) VALUE 1.
           03  CNS-29          PIC  9(001) VALUE 2.
           03  CNS-0129        PIC  9(003) VALUE 012.

           03  CNS-0X          PIC  X(001) VALUE "0".
           03  CNS-1X          PIC  X(001) VALUE "1".
           03  CNS-2X          PIC  X(001) VALUE "2".
           03  CNS-012X        PIC  X(003) VALUE "012".

           03  CNS-L           PIC  X(001) VALUE "L".
           03  CNS-R           PIC  X(001) VALUE "R".
           03  CNS-C           PIC  X(001) VALUE "C".

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I-MAX           BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-DOUBLE SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SET          PIC  X(001) VALUE "N".
           03  SW-YES          PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-I            BINARY-LONG SYNC VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
      *    PERFORM S020-10     THRU    S020-EX

      *    *** READ PIN2
      *    PERFORM S030-10     THRU    S030-EX

           DISPLAY "TEST NO INPUT"
           DISPLAY "00.C$JUSTIFY,NUMVAL,random,"
           DISPLAY "01.WRITE"
           DISPLAY "02.DECODE03"
           DISPLAY "03.HEX DIVIDE "
           DISPLAY "04.FILEDUMP"
           DISPLAY "05.FILEDUMP"
           DISPLAY "06.DECODE03-2"
           DISPLAY "07.DECODE04"
           DISPLAY "08.IF CHECK"
           DISPLAY "09.DECODE05"
           DISPLAY "10.SEARCH ALL"
           DISPLAY "11.ADD OVERFLOW"
           DISPLAY "12.DATEWEEK ID=R ì˙êîÇ©ÇÁì˙ïtÇãÅÇﬂÇÈ"
           DISPLAY "13.DATEWEEK ID=A ì˙ïtÇ©ÇÁì˙êîÇãÅÇﬂÇÈ"
           DISPLAY "14.äOïîïÇìÆè¨êîì_çÄñ⁄"
           DISPLAY "15.DECODE07 ï∂éöÉZÉpÉåÅ[É^"

           ACCEPT  WK-TESTNO

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           EVALUATE WK-TESTNO

               WHEN "00"
                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           PERFORM S200-10     THRU    S200-EX
      *    *** READ PIN1
      *                    PERFORM S020-10     THRU    S020-EX
                   END-PERFORM
               WHEN "01"
                   PERFORM S210-10     THRU    S210-EX

               WHEN "02"
                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           PERFORM S220-10     THRU    S220-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN "03"
                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           PERFORM S230-10     THRU    S230-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN "04"
      *    *** FILEDUMP DECODE03 ïœçXå„éûä‘í≤ç∏
                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           PERFORM S240-10     THRU    S240-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN "05"
      *    *** FILEDUMP DECODE03 ïœçXå„éûä‘í≤ç∏
                   PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE
                           PERFORM S250-10     THRU    S250-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU    S020-EX
                   END-PERFORM

               WHEN "06"
      *    *** FILEDUMP DECODE03-2 ïœçXå„éûä‘í≤ç∏
                   PERFORM S260-10     THRU    S260-EX

               WHEN "07"
      *    *** DECODE03 TEST
                   PERFORM UNTIL   WK-PIN2-EOF =     HIGH-VALUE
                           PERFORM S270-10     THRU    S270-EX
      *    *** READ PIN2
                           PERFORM S030-10     THRU    S030-EX
                   END-PERFORM

               WHEN "08"
      *    *** IF CHECK
                   PERFORM S280-10     THRU    S280-EX

               WHEN "09"
      *    *** DECODE05
                   PERFORM S290-10     THRU    S290-EX

               WHEN "10"
      *    *** SEARCH ALL
                   PERFORM S300-10     THRU    S300-EX

               WHEN "11"
      *    *** ADD OVERFLOW
                   PERFORM S310-10     THRU    S310-EX

               WHEN "12"
      *    *** DATEWEEK ID=R
                   PERFORM S320-10     THRU    S320-EX

               WHEN "13"
      *    *** DATEWEEK ID=A
                   PERFORM S330-10     THRU    S330-EX

               WHEN "14"
      *    *** äOïîïÇìÆè¨êîì_çÄñ⁄
                   PERFORM S340-10     THRU    S340-EX

               WHEN "15"
      *    *** DECODE07 ï∂éöÉZÉpÉåÅ[É^
                   PERFORM S350-10     THRU    S350-EX

           END-EVALUATE

           MOVE    "L"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

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

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "FIRST"     TO      WDE04-SHORI

           MOVE    "O"         TO      WFD-ID
      *     MOVE    WK-PGM-NAME TO      WFD-PGM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05" USING    WDE05-DECODE05-AREA
                                       WK-WDE05-BUF1
                                       WK-WDE05-BUF2

           MOVE    "OPEN  "    TO      WDE07-ID
           CALL    "DECODE07" USING    WDE07-DECODE07-AREA
           .
       S010-EX.
           EXIT.

      *    *** PIN1 READ
       S020-10.

      *     MOVE    ZERO        TO      WK-PIN1-LEN
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO        WK-PIN1-CNT

      *     IF WK-PIN1-CNT < 10
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " LEN=" WK-PIN1-LEN
      *     END-IF

           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO    WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .

       S020-EX.
           EXIT.

      *    *** PIN2 READ
       S030-10.

           IF      WDE04-SHORI   =       "FIRST" OR "READ " OR "END  "
                   MOVE    HIGH-VALUE  TO      PIN2-REC
                   DISPLAY "1  WK-PIN2-CNT=" WK-PIN2-CNT
                           " LEN=" WK-PIN2-LEN
                   READ    PIN2-F
                   DISPLAY "2  WK-PIN2-CNT=" WK-PIN2-CNT
                           " LEN=" WK-PIN2-LEN

                   IF      WK-PIN2-STATUS =    ZERO OR 04
                           ADD     1           TO      WK-PIN2-CNT

      *             DISPLAY "PIN2-STATUS=" WK-PIN2-STATUS 
      *                     " WK-PIN2-CNT=" WK-PIN2-CNT

           MOVE    "P"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           MOVE    WK-PIN2-LEN TO      WFD-LEN
           MOVE    WK-PIN2-CNT TO      WFD-SEQ
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN2-REC
                                       WFD-LEN
                   END-IF
           ELSE
                   CONTINUE
           END-IF

           IF      WK-PIN2-STATUS =    ZERO OR 04
                   CONTINUE

      *     IF WK-PIN1-CNT < 10
      *             DISPLAY "WK-PIN1-CNT=" WK-PIN1-CNT
      *                     " LEN=" WK-PIN1-LEN
      *     END-IF

           ELSE
               IF  WK-PIN2-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN2-F READ ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .

       S030-EX.
           EXIT.

      *    *** 
       S200-10.

           MOVE    "       ABC" TO     WK-DATA1
           DISPLAY ":" WK-DATA1 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA1 CNS-L
           DISPLAY ":" WK-DATA1 ": ïœä∑å„ C$JUSTIFY L"

           DISPLAY ":" WK-DATA1 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA1 CNS-R
           DISPLAY ":" WK-DATA1 ": ïœä∑å„ C$JUSTIFY R"

           DISPLAY ":" WK-DATA1 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA1 CNS-C
           DISPLAY ":" WK-DATA1 ": ïœä∑å„ C$JUSTIFY C"


           MOVE    "       ABC" TO     WK-DATA7
      *    *** (10,1)=C ÇÕà⁄ìÆÇµÇ»Ç¢
           DISPLAY " " 
           DISPLAY ":" WK-DATA7 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA7(2:8) CNS-L
           DISPLAY ":" WK-DATA7 ": ïœä∑å„ C$JUSTIFY L WK-DATA7(2:8) "

           DISPLAY ":" WK-DATA7 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA7(2:8) CNS-R
           DISPLAY ":" WK-DATA7 ": ïœä∑å„ C$JUSTIFY R WK-DATA7(2:8) "

           DISPLAY ":" WK-DATA7 ": ïœçXëO"
           CALL "C$JUSTIFY" USING WK-DATA7(2:8) CNS-C
           DISPLAY ":" WK-DATA7 ": ïœä∑å„ C$JUSTIFY C WK-DATA7(2:8) "

      *> base 2 numeric literal,ERROR
      *     DISPLAY B#101                        
      *> base 8 numeric literal,ERROR
      *     DISPLAY O#1777777777777777777777     
      *> base 16 numeric literal,ERROR
      *     DISPLAY X#ffffffffffffffff           
      *> base 16 numeric literal,ERROR

      *     DISPLAY H#ffffffffffffffff           
      *> numeric as base 2,ERROR
      *     DISPLAY B"000001010"                 
      *> string literal in base 2,ERROR
      *     DISPLAY BX"00001010"                 
      *> numeric as base 16 ?
           DISPLAY H"DECAFBAD"                  
      *> 16bit character National
           DISPLAY N"ABCDE"                     
      *> 16bit National in base 16
           DISPLAY NX"20304050"                 
      *> L String literal??,ERROR
      *     DISPLAY L"ABCDE"                     
      *> string literal concatenation
           DISPLAY "ABC" & "DEF"                
      *> string as base 16 pairs
           DISPLAY X"0a00"                      
      *> nul byte suffix literal,ERROR
      *     MOVE Z"C-string" TO add-zero-byte 

      *    *** ERROR
      *     DISPLAY
      *        "Caller    " FUNCTION MODULE-CALLER-ID       newline
      *        "Date      " FUNCTION MODULE-DATE            newline
      *        "Formatted " FUNCTION MODULE-FORMATTED-DATE  newline
      *        "Id        " FUNCTION MODULE-ID              newline
      *        "Path      " FUNCTION MODULE-PATH            newline
      *        "Source    " FUNCTION MODULE-SOURCE          newline
      *        "Time      " FUNCTION MODULE-TIME            newline

      *    *** ERROR ROUNDED MODE 
      *     ADD     1 2 3  GIVING WK-DATA3 ROUNDED MODE NEAREST-EVEN

           display when-compiled
           display function when-compiled

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > 10
      *         move function current-date to datetime21
      *         move datetime21(8:9) to seed
               display 'seed=' WK-seed
               compute WK-DATA4 = function random (WK-seed)
               COMPUTE WK-SEED = WK-DATA4 * 100000
               display 'num-ran = ' WK-DATA4
           END-PERFORM

           DISPLAY '"' FUNCTION TRIM("   abc   ") '"'
           DISPLAY '"' FUNCTION TRIM("   abc   " LEADING) '"'
           DISPLAY '"' FUNCTION TRIM("   abc   " TRAILING) '"'

           DISPLAY    FUNCTION NUMVAL(-12345) 
      *     DISPLAY    FUNCTION NUMVAL(12345-) 
           DISPLAY    FUNCTION NUMVAL(+12345) 
      *     DISPLAY    FUNCTION NUMVAL(12345+) 
      *    *** NUMVAL Å|ãLçÜÇÕîªífÇ∑ÇÈÇ™ÅAÉsÉäÉIÉhÇÕñ≥éãÇ≥ÇÍÇÈ
           MOVE    FUNCTION NUMVAL(-123.45) TO WK-DATA5
           DISPLAY WK-DATA5
           MOVE    FUNCTION NUMVAL(-123.45) TO WK-DATA5
           DISPLAY WK-DATA5

      *    *** I(0) ìYéöÉ[ÉçÇ≈Ç‡ÉGÉâÅ[ÅAÉèÅ[ÉjÉìÉOÇ…Ç»ÇÁÇ»Ç¢
      *    *** OCCURS OVER  Ç≈Ç‡ÉGÉâÅ[ÅAÉèÅ[ÉjÉìÉOÇ…Ç»ÇÁÇ»Ç¢
           MOVE     ZERO       TO     I
           DISPLAY "TBL01-KEY(I)=" TBL01-KEY(I) " I=" I

           MOVE     "AAAAAAAAAAA" TO     WK-DATA5-X
           IF       WK-DATA5    IS NUMERIC
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NUMERIC" 
           ELSE
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NOT NUMERIC" 
           END-IF

           MOVE     "12345678901" TO     WK-DATA5-X
           IF       WK-DATA5    IS NUMERIC
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NUMERIC" 
           ELSE
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NOT NUMERIC" 
           END-IF

      *    *** SPACE <= 0 Ç…Ç»Ç¡ÇƒÇÈ
           MOVE     "123456789v " TO     WK-DATA5-X
           IF       WK-DATA5    IS NUMERIC
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NUMERIC" 
           ELSE
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NOT NUMERIC" 
           END-IF

           MOVE     "1234567890v" TO     WK-DATA5-X
           IF       WK-DATA5    IS NUMERIC
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NUMERIC" 
           ELSE
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NOT NUMERIC" 
           END-IF

           MOVE     "1234567890a" TO     WK-DATA5-X
           IF       WK-DATA5    IS NUMERIC
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NUMERIC" 
           ELSE
                    DISPLAY "WK-DATA5-X=" WK-DATA5-X " NOT NUMERIC" 
           END-IF

      *     ACCEPT   WK-DATA6    FROM      COMMAND-LINE
      *     ACCEPT   WK-DATA6    FROM      SYSIN
           ACCEPT   WK-DATA6    FROM      ARGUMENT-NUMBER
           DISPLAY  WK-DATA6

           ACCEPT   WK-DATA6    FROM      ARGUMENT-VALUE
           DISPLAY  WK-DATA6

           ACCEPT   WK-DATA6    FROM      ARGUMENT-VALUE
           DISPLAY  WK-DATA6
           .
       S200-EX.
           EXIT.

      *    *** POT1 WRITE 1
       S210-10.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I-MAX
                   MOVE    TBL01-KEY  (I) TO   POT1-DATA

                   WRITE   POT1-REC  
                   ADD     1          TO       WK-POT1-CNT
           END-PERFORM
           .
       S210-EX.
           EXIT.

       S220-10.
      *    *** TEST10.POT1 HTML 4185åè
      *    *** ÇOÅDÇRÇSïb
      
      *     MOVE    SPACE       TO      PIN1-REC
      *    *** 3.72 , 3.91 , 

      *     MOVE    ALL "ABC"   TO      PIN1-REC
      *    *** 5.76 , 5.79 , 3.06

      *     MOVE    ALL "1234567890" TO PIN1-REC
      *    *** 8.34 , 7.57 , 9.28

      *     MOVE    10000       TO      WK-PIN1-LEN
      *                                 WK-POT1-LEN
           CALL    "DECODE03"  USING   PIN1-REC
                                       WK-PIN1-LEN
                                       WK-BUF2

           COMPUTE WK-POT1-LEN = WK-PIN1-LEN * 2
           WRITE   POT1-REC    FROM    WK-BUF2-LR-TBL
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1 WRITE ERROR STATUS=" 
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT

           .
       S220-EX.
           EXIT.

       S230-10.
      *    *** TEST10.POT1 HTML 4185åè
      *    *** ÇQÅDÇWÇPïb
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
                   MOVE   PIN1-REC (I:1) TO PIC-X
                   DIVIDE PIC-Halfword BY 16
                          GIVING Left-Nibble
                          REMAINDER Right-Nibble

                   ADD 1 TO Left-Nibble Right-Nibble

                   MOVE Hex-Digit (Left-Nibble)
                           TO WK-BUF2-L2 (I)

                   MOVE Hex-Digit (Right-Nibble)
                           TO WK-BUF2-R2 (I)
           END-PERFORM

           COMPUTE WK-POT1-LEN = WK-PIN1-LEN * 2
           WRITE   POT1-REC    FROM    WK-BUF2-LR-TBL
           IF      WK-POT1-STATUS NOT = ZERO
                   DISPLAY WK-PGM-NAME " POT1 WRITE ERROR STATUS=" 
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           ADD     1           TO      WK-POT1-CNT

          .
       S230-EX.
           EXIT.

       S240-10.

      *    *** TEST10.POT1 HTML 4185åè
      *    *** ÇOÅDÇUÇQïbÅ@ÉåÉìÉOÉXÅ@ÉZÉbÉgÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇwÇÃéûÅA
      *    *** ÇOÅDÇVÇTïb
      *    *** Å@ÇPÉoÉCÉgëOÇ∆ìØÇ∂Ç»ÇÁÇrÇdÇ`ÇqÇbÇgé~ÇﬂÇΩÇ∆Ç´ÅAè≠ÇµíxÇ≠Ç»Ç¡ÇΩ
      *    *** ÇOÅDÇRÇXïbÅ@Ç≥ÇÁÇ…ÇsÇxÇoÇdÅÅÇwÇÃéûÅAÇPÇOÇOÉoÉCÉgÇÃÇ›ïœä∑Ç…ïœçX
      *    *** ÇUÅDÇPÇQïbÅ@ÉåÉìÉOÉXÇPÇOÅCÇOÇOÇOå≈íËÇ…ÇµÇΩÇsÇxÇoÇdÅÅÇwÇÃéû

      *    *** ÇOÅDÇVÇPïbÅ@ÉåÉìÉOÉXÅ@ÉZÉbÉgÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇoÇÃéûÅA
      *    *** ÇUÇOÅDÇUÇTïbÅ@ÉåÉìÉOÉXÅ@ÇPÇOÅCÇOÇOÇOå≈íËÇ…ÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇoÇÃéûÅA
      *    *** ÇTÇWÅDÇOÇUïbÅ@ÉåÉìÉOÉXÅ@ÇPÇOÅCÇOÇOÇOå≈íËÇ…ÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇoÇÃéûÅA
      *    *** Å@ÇPÉoÉCÉgëOÇ∆ìØÇ∂Ç»ÇÁÇrÇdÇ`ÇqÇbÇgé~ÇﬂÇΩÇ∆Ç´ÅAè≠ÇµëÅÇ≠Ç»Ç¡ÇΩ

      *    *** HEX ïœçXÉçÉWÉbÉNÉeÉXÉg
      *    MOVE    "X"         TO      WFD-ID
           MOVE    "P"         TO      WFD-ID
           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           MOVE    WK-PIN1-LEN TO      WFD-LEN
      *     MOVE    10000       TO      WFD-LEN
           MOVE    "UTF8"      TO      WFD-KANJI
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
                                       WFD-LEN

          .
       S240-EX.
           EXIT.

       S250-10.

      *    *** TEST10.POT1 HTML 4185åè
      *    *** ÇPÅDÇWÇUïbÅ@ÉåÉìÉOÉXÅ@ÉZÉbÉgÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇwÇÃéûÅA
      *    *** ÇVÅDÇQÇTïbÅ@ÉåÉìÉOÉXÇPÇOÅCÇOÇOÇOå≈íËÇ…ÇµÇΩÇsÇxÇoÇdÅÅÇwÇÃéû

      *    *** ÇRÅDÇQÇTïbÅ@ÉåÉìÉOÉXÅ@ÉZÉbÉgÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇoÇÃéûÅA
      *    *** ÇUÇPÇQÅDÇQÇQïbÅ@ÉåÉìÉOÉXÅ@ÇPÇOÅCÇOÇOÇOå≈íËÇ…ÇµÇΩéûÅ@ÇsÇxÇoÇdÅÅÇoÇÃéûÅA

      *    *** HEX ïœçXÉçÉWÉbÉNÉeÉXÉg
      *     MOVE    "X"         TO      WFD-ID
           MOVE    "P"         TO      WFD-ID
           MOVE    WK-PIN1-CNT TO      WFD-SEQ
           MOVE    WK-PIN1-LEN TO      WFD-LEN
      *     MOVE    10000       TO      WFD-LEN
           MOVE    "UTF8"      TO      WFD-KANJI
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       PIN1-REC
                                       WFD-LEN

          .
       S250-EX.
           EXIT.

       S260-10.

      *     MOVE    "X"         TO      WFD-ID
           MOVE    "P"         TO      WFD-ID
      *     MOVE    WK-PIN1-CNT TO      WFD-SEQ
      *     MOVE    WK-PIN1-LEN TO      WFD-LEN
      *     MOVE    "UTF8"      TO      WFD-KANJI
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA10
      *                                 WFD-LEN

           .
       S260-EX.
           EXIT.

       S270-10.

           MOVE     WK-PIN2-LEN TO      WDE04-BUF1-LEN
           
      *     DISPLAY "WDE04-SHORI=" WDE04-SHORI
      *     DISPLAY "WK-PIN2-LEN   =" WK-PIN2-LEN
      *     DISPLAY "WDE04-BUF1-LEN=" WDE04-BUF1-LEN
           CALL     "DECODE04"  USING   WDE04-AREA
                                        PIN2-REC
                                        WK-BUF2-L-TBL

           IF      WDE04-SHORI    NOT =   "END  "
                   MOVE    HIGH-VALUE   TO     POT2-REC
                   MOVE    WDE04-REC-LEN TO    WK-POT2-LEN
                   MOVE    200          TO     WK-POT2-LEN
      *             MOVE    80           TO      WK-POT2-LEN
      *             MOVE    WK-BUF2-L-TBL (1:WK-POT2-LEN) TO
      *                     POT2-REC      (1:WK-POT2-LEN)
      *             WRITE   POT2-REC
           IF WK-POT2-CNT < 10
                   WRITE   POT2-REC    FROM    WK-BUF2-L-TBL

           END-IF
           
           MOVE    "X"         TO      WFD-ID

           MOVE    80          TO      WFD-LEN
           MOVE    2           TO      WFD-SU
           MOVE    WK-POT2-CNT TO      WFD-SEQ
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT2-REC (1:80)
                                       WFD-LEN

                   IF      WK-POT2-STATUS NOT = ZERO
                         DISPLAY WK-PGM-NAME " POT2 WRITE ERROR STATUS="
                                   WK-POT2-STATUS
                           STOP    RUN
                   END-IF
                   ADD     1           TO      WK-POT2-CNT
           END-IF
           .
       S270-EX.
           EXIT.

       S280-10.

           IF      WK-DATA09   =       "A1"
                   DISPLAY WK-PGM-NAME " DATA09 THEN "
           ELSE
                   DISPLAY WK-PGM-NAME " DATA09 ELSE "
           END-IF

           IF      WK-DATA09(1:2) =    "A1"
                   DISPLAY WK-PGM-NAME " DATA09 THEN "
           ELSE
                   DISPLAY WK-PGM-NAME " DATA09 ELSE "
           END-IF
           .
       S280-EX.
           EXIT.

       S290-10.

      *    *** HENKAN=SU SJIS => UTF8
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    "SU"        TO      WDE05-HENKAN
           MOVE    "AK"        TO      WDE05-MODE
           MOVE    10          TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
           MOVE    "ìVãC"      TO      WK-WDE05-BUF1
      *     MOVE    SPACE       TO      WDE05-BUF2
           MOVE    ALL "ABC"   TO      WK-WDE05-BUF2
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-WDE05-BUF1
                                       WK-WDE05-BUF2
           DISPLAY WK-WDE05-BUF1 " MAIN"
           CALL    "COBDUMP"   USING   WK-WDE05-BUF1
           CALL    "COBDUMP"   USING   WK-WDE05-BUF2

      *    *** HENKAN=US UTF8 => SJIS
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    "US"        TO      WDE05-HENKAN
           MOVE    10          TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
           MOVE    X"E5A4A9E6B097" TO  WK-WDE05-BUF1
           MOVE    SPACE       TO      WK-WDE05-BUF2
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-WDE05-BUF1
                                       WK-WDE05-BUF2
           CALL    "COBDUMP"   USING   WK-WDE05-BUF2

      *    *** HENKAN=US UTF8 => SJIS
           MOVE    "CHANGE"    TO      WDE05-ID
           MOVE    "US"        TO      WDE05-HENKAN
           MOVE    10          TO      WDE05-BUF1-LEN
                                       WDE05-BUF2-LEN
          MOVE    X"E5A4A92021E6B097" TO  WK-WDE05-BUF1
           MOVE    SPACE       TO      WK-WDE05-BUF2
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-WDE05-BUF1
                                       WK-WDE05-BUF2
           CALL    "COBDUMP"   USING   WK-WDE05-BUF2
           .
       S290-EX.
           EXIT.

       S300-10.

           PERFORM VARYING I FROM 100 BY -1
                   UNTIL I < 1
                   MOVE    I           TO      TBL01-I    (I)
                   MOVE    I           TO      TBL01-KEY  (I)
                   MOVE    I           TO      TBL01-DATA (I)
           END-PERFORM

           SORT    TBL01-AREA
                   ASCENDING KEY TBL01-KEY


           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 100
                   DISPLAY TBL01-I    (I) " "
                           TBL01-KEY  (I) " "
                           TBL01-DATA (I)
           END-PERFORM

      *    *** SEARCH ALL ïîï™éQè∆ (1:1) AND (2:1) Ç≈ÇÕ àÍïîÇÃçÄñ⁄ÇnÇj
      *    *** Ç≈Ç‡ÅAÇgÇhÇsÇµÇƒÇµÇ‹Ç§

           DISPLAY " "
           DISPLAY "TEST01"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY CNS-1

               WHEN TBL01-KEY (TBL01-IDX) =   CNS-1

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY CNS-1
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST02"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012B"

               WHEN TBL01-KEY (TBL01-IDX) (1:1) =   CNS-0B AND 
                    TBL01-KEY (TBL01-IDX) (2:1) =   CNS-1B AND
                    TBL01-KEY (TBL01-IDX) (3:1) =   CNS-2B

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012B"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST03"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012B"

               WHEN TBL01-KEY (TBL01-IDX)  =   CNS-012B
                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012B"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST04"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "0129"

               WHEN TBL01-KEY (TBL01-IDX) (1:1) =   CNS-09 AND 
                    TBL01-KEY (TBL01-IDX) (2:1) =   CNS-19 AND
                    TBL01-KEY (TBL01-IDX) (3:1) =   CNS-29

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "0129"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST05"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "0129"

               WHEN TBL01-KEY (TBL01-IDX)  =   CNS-0129 

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "0129"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST06"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012X"

               WHEN TBL01-KEY (TBL01-IDX) (1:1) =   CNS-0X AND 
                    TBL01-KEY (TBL01-IDX) (2:1) =   CNS-1X AND
                    TBL01-KEY (TBL01-IDX) (3:1) =   CNS-2X

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012X"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST07"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012X"

               WHEN TBL01-KEY (TBL01-IDX)  =   CNS-012X
                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "012X"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST08"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "034"

               WHEN TBL01-KEY (TBL01-IDX) (1:1) =   X"30" AND 
                    TBL01-KEY (TBL01-IDX) (2:1) =   X"33" AND
                    TBL01-KEY (TBL01-IDX) (3:1) =   X"34"

                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "034"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH

           DISPLAY " "
           DISPLAY "TEST09"
           SEARCH  ALL TBL01-AREA
               AT END
                   DISPLAY "AT END KEY=" TBL01-KEY (TBL01-IDX) 
                   DISPLAY "034"

               WHEN TBL01-KEY (TBL01-IDX)  =   X"303334"
                   DISPLAY " HIT KEY  ="  TBL01-KEY (TBL01-IDX) 
                   DISPLAY "034"
                   DISPLAY "TBL01-IDX=" TBL01-IDX
           END-SEARCH
           .
       S300-EX.
           EXIT.

      *    *** ADD OVERFLOW BINARY-LONG 999,999,999 Ç‹Ç≈ï\åªâ¬î\
       S310-10.

      *    *** 7.1.9. Using Compiler Configuration Files
      *    *** # Binary byte size - 
      *    ***   defines the allocated bytes according to PIC
      *    *** # Value:    signed unsigned bytes
      *    *** #           ------ -------- -----
      *    *** # '2-4-8'   1 -  4              2
      *    *** #           5 -  9              4 BINARY-LONG    9åÖÇ‹Ç≈
      *    *** #          10 - 18              8 BINARY-DOUBLE 18åÖÇ‹Ç≈
      *    *** #
      *    *** # '1-2-4-8' 1 -  2              1
      *    *** #           3 -  4              2
      *    *** #           5 -  9              4 
      *    *** #          10 - 18              8
      *    *** #
      *    *** # '1--8'    1 -  2    1 - 2     1
      *    *** #           3 -  4    3 - 4     2
      *    *** #           5 -  6    5 - 7     3
      *    *** #           7 -  9    8 - 9     4
      *    *** #          10 - 11   10 - 12    5
      *    *** #          12 - 14   13 - 14    6
      *    *** #          15 - 16   15 - 16    7
      *    *** #          17 - 18   17 - 18    8
      *    ***           PIC 9(NN)
      *    *** binary-size: 1-2-4-8

           MOVE    999999990   TO      L2
           PERFORM VARYING L FROM 99999999 BY 1
                   UNTIL L > 999999999999
                   ADD     1           TO      L2
                       SIZE ERROR
                           DISPLAY WK-PGM-NAME " ADD OVER FLOW "
                         DISPLAY WK-PGM-NAME " L (BINARY-DOUBLE) L =" L 
                         DISPLAY WK-PGM-NAME " L2(BINARY-LONG  ) L2=" L2
                           EXIT PERFORM
                       NOT SIZE ERROR
      *                     CONTINUE
                           DISPLAY WK-PGM-NAME " ADD L2=" L2
                   END-ADD
           END-PERFORM
           .
       S310-EX.
           EXIT.
           .

      *    *** DATEWEEK ID=R NISUU=>YYYYMMDD
       S320-10.

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    1           TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    31          TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    32          TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    60          TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    61          TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    366         TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

      *     MOVE    "R"         TO      WDW-DATE2-ID
      *     MOVE    367         TO      WDW-NISUU
      *     CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA

           MOVE    "R"         TO      WDW-DATE2-ID
           MOVE    "N"         TO      SW-YES

           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY " "
                   DISPLAY "NISUU INPUT"
                   ACCEPT  WDW-NISUU
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   DISPLAY "YYYYMMDD=" WDW-DATE2-YMD
                           " NISSU=" WDW-NISUU
                   DISPLAY " "
                   DISPLAY "END Y OR N INPUT"
                   ACCEPT  SW-YES
           END-PERFORM
           
           .
       S320-EX.
           EXIT.

      *    *** DATEWEEK ID=A YYYYMMDD=>NISUU
       S330-10.

           MOVE    "A"         TO      WDW-DATE2-ID
           MOVE    "N"         TO      SW-YES

           PERFORM UNTIL SW-YES = "Y"
                   DISPLAY " "
                   DISPLAY "YYYYMMDD INPUT"
                   ACCEPT  WDW-DATE2-YMD
                   CALL    "DATEWEEK"  USING   WDW-DATEWEEK-AREA
                   DISPLAY "NISSU=" WDW-NISUU
                   DISPLAY " "
                   DISPLAY "END Y OR N INPUT"
                   ACCEPT  SW-YES
           END-PERFORM
           .
       S330-EX.
           EXIT.

      *    *** äOïîïÇìÆè¨êîì_çÄñ⁄
       S340-10.

      *     DISPLAY WK-ESU1
      *     DISPLAY WK-ESU2
      *     DISPLAY WK-ESU3

      *     WRITE   POT1-REC    FROM    WK-ESU1
      *     WRITE   POT1-REC    FROM    WK-ESU2
      *     WRITE   POT1-REC    FROM    WK-ESU3
           .
       S340-EX.
           EXIT.

      *    *** DECODE07 ï∂éöÉZÉpÉåÅ[É^
       S350-10.

      *    *** ASCII ÇPÇUï∂éöïœä∑â¬î\ÅAÇOÅ|ÇXÅCÇ`Å|Çy 

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇP" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** í èÌèoóÕéû
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇQ" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *    *** è„ë§âEÇ÷ÅAâ∫ë§ç∂Ç÷ÇP
           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    02          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇR" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    03          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§âEÇ÷ÅAâ∫ë§ç∂Ç÷ÇQ
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇS" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    04          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§âEÇ÷ÅAâ∫ë§ç∂Ç÷ÇR
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇT" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    05          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§ç∂Ç÷ÅAâ∫ë§âEÇ÷ÇP
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇU" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    06          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§ç∂Ç÷ÅAâ∫ë§âEÇ÷ÇQ
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇV" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    07          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§ç∂Ç÷ÅAâ∫ë§âEÇ÷ÇR
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇW" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    08          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** è„ë§âEÇ÷ÅAâ∫ë§ç∂Ç÷Å@è„â∫ãtì]
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇX" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    09          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** ç∂âEãtì]
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

           MOVE    SPACE       TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           MOVE    "ÉpÉ^Å[ÉìÇPÇO" TO     POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    10          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      *    *** îΩì]

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM



      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "123456789,./\<>" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "?_;:]+*}@[`{-^\" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "!""#$%&'()=~|" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "abcdefghijklm" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "ABCDEFGHIJKLM" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "nopqrstuvwxyz" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM

      *    *** 
           MOVE    "CHANGE"    TO      WDE07-ID
           MOVE    "NOPQRSTUVWXYZ" TO WDE07-ASCII
           MOVE    01          TO      WDE07-PTN
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 16
                   WRITE   POT1-REC    FROM    WDE07-LINE (I)
                   ADD     1           TO      WK-POT1-CNT
           END-PERFORM
           .
       S350-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       WK-WDE05-BUF1
                                       WK-WDE05-BUF2

           MOVE    "CLOSE "    TO      WDE07-ID
           CALL    "DECODE07" USING    WDE07-DECODE07-AREA

           DISPLAY WK-PGM-NAME " END"
           DISPLAY WK-PGM-NAME " PIN1 π›Ω≥ = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN2 π›Ω≥ = " WK-PIN2-CNT
                   " (" WK-PIN2-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 π›Ω≥ = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT2 π›Ω≥ = " WK-POT2-CNT
                   " (" WK-POT2-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
