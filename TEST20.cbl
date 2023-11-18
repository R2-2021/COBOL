*> ***************************************************************
identification division.
program-id. TEST20.
environment division.
configuration section.
repository.
function all intrinsic.
data division.
working-storage section.
01 black constant as 0.
01 blue constant as 1.
01 green constant as 2.
01 cyan constant as 3.
01 red constant as 4.
01 magenta constant as 5.
01 brown constant as 6.
01 white constant as 7.
01 anykey pic x.
01 anykey2 pic n(4).
01 backing pic 9.
01 foreing pic 9.
01 l pic 99.
01 c pic 99.
01 c2 pic 9999.
01 c3 pic 99.
01 WK-A pic 9.
01 ran pic 9v99999999.
01 sleep pic s9(11) value 500000000.
screen section.
01 gnu-cobol-colours3 value "X".
01 gnu-cobol-colours2 line 1 column 1 value "X".
01 gnu-cobol-colours.
*>  05 line 1 column 1 value 
*>"                                                                                 "
*>                                      background-color white.
*> 05 line 2 column 1 value
*>"                                                                                 "
*>                                      background-color white.
05 line  7 column 1 value "Cyan    3" foreground-color cyan
                                      background-color white.
05 line  8 column plus 1 value "Cyan    4" foreground-color cyan
                                      background-color white.
05 line  9 column minus 1


 value "Cyan    5" foreground-color cyan
                                      background-color white.




05 line plus 2 column 30 value "Enter to exit"
                  foreground-color black
                  background-color white.
05 column plus 2 using anykey2.
*> ***************************************************************

procedure division.
DISPLAY "TEST 0 OR 1 OR 2 OR 3 OR 4 OR 5 INPUT" at line 24 column 1
ACCEPT WK-A
IF  WK-A = 0
    GO  PA00
ELSE
    IF  WK-A = 1
        GO  PA01
    ELSE
        IF  WK-A = 2
            GO  PA02
        ELSE
            IF  WK-A = 3
                GO  PA03
            ELSE
                IF  WK-A = 4
                    GO  PA04
                ELSE
                    GO  PA05
                END-IF
            END-IF
        END-IF
    END-IF
END-IF
.
PA00.
*> display a table of colour combinations
perform varying l from 1 by 1 until l > 25
    display
"                                                                                 "
            at line l column 1
            with background-color white
*>               foreground-color white
    end-display
end-perform.
*>go l1.
accept gnu-cobol-colours end-accept.
GOBACK.

PA01.
perform varying c2 from 1 by 1 until c2 > 100
    move function random to ran
    compute l rounded = ran * 25
    if l = zero
       move 1 to l
    end-if
    move function random to ran
    compute c rounded = ran * 80
    if c = zero
       move 1 to c
    end-if
    move function random to ran
    compute c3 rounded = ran * 7
    if c3 = zero
       move 1 to c3
    end-if
    display "X"
            at line l column c
            with background-color white
                 foreground-color c3
    end-display
*>    call "CBL_OC_NANOSLEEP" using sleep

end-perform.
accept gnu-cobol-colours end-accept.
GOBACK.

PA02.
perform varying backing from 0 by 1 until backing > 7
  perform varying foreing from 0 by 1 until foreing > 7
    compute l = backing + 15
    compute c = foreing * 10 + 2
    display " colour " at line l column c
            with background-color backing
                foreground-color foreing
    end-display
  end-perform
end-perform


*> put up the form oriented screen section
accept gnu-cobol-colours end-accept
GOBACK.

PA03.
display  gnu-cobol-colours2
            at line 1 column 1
            with background-color blue
                 foreground-color white
end-display
call "CBL_OC_NANOSLEEP" using sleep.
display  gnu-cobol-colours2
           
            with 
                 background-color blue
                 foreground-color white
end-display
call "CBL_OC_NANOSLEEP" using sleep.
accept gnu-cobol-colours end-accept
GOBACK.

PA04.

perform varying l from 3 by 1 until l > 25
   display
"################################################################################"
            at line l column 1
            with background-color cyan
               foreground-color white
    end-display

end-perform.
accept gnu-cobol-colours end-accept
GOBACK.

PA05.
display anykey2 at line 10 column 40
            with background-color white
                 foreground-color cyan
    end-display
call "CBL_OC_NANOSLEEP" using sleep.
l1.
*>accept gnu-cobol-colours3 end-accept.
accept gnu-cobol-colours end-accept

goback.
