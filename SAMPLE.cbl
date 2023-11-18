*> Modified: 2015-12-08/06:46-0500
 identification division.
 program-id. SAMPLE.

 environment division.
 configuration section.
 repository.
     function all intrinsic.

 data division.
 working-storage section.
 01 WORK-AREA.
   03  WK-PGM-NAME PIC X(8) VALUE "SAMPLE".
   03  WK-I1      PIC S9(9) VALUE ZERO PACKED-DECIMAL.
   03  WK-I2      PIC S9(9) VALUE ZERO PACKED-DECIMAL.
   03  WK-I3      PIC S9(9) VALUE ZERO PACKED-DECIMAL.
01 enumerated-value CONSTANT AS 500.
01 some-string      CONSTANT AS "immutable value".
 procedure division.
 demonstration section.
 MOVE 123456789 TO WK-I1
 MOVE 123456789 TO WK-I2
 COMPUTE WK-I3 = WK-I1 * WK-I2
   ON SIZE ERROR
     PERFORM hard-exception
 END-COMPUTE
   

 goback.

*> informational warnings and abends
 soft-exception.
   display space upon syserr
   display "--Exception Report-- " upon syserr
   display "Time of exception:   " current-date upon syserr
*> UNDEFINE ERROR
*>   display "Module:              " FUNCTION module-id upon syserr
*>   display "Module-path:         " FUNCTION module-path upon syserr
*>   display "Module-source:       " FUNCTION module-source upon syserr
*>   display "Module-caller-id: " FUNCTION module-CALLER-ID upon syserr
*>   display "Module-date:      " FUNCTION Module-DATE upon syserr
*>   display "Module-time:      " FUNCTION Module-time upon syserr
*>   display "Module-formatted-date:" FUNCTION Module-FORMATTED-DATE
*>    upon syserr
   display "Exception-file:      " exception-file upon syserr
   display "Exception-status:    " exception-status upon syserr
   display "Exception-location:  " exception-location upon syserr
   display "Exception-statement: " exception-statement upon syserr
 .

 hard-exception.
     perform soft-exception
     stop run returning 127
 .

 end program SAMPLE.
