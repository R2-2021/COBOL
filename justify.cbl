*> source-str の文字左寄せ、中央寄せ、右寄せ
program-id. justify.
environment division.
configuration section.
source-computer. IBMPC.
object-computer. IBMPC.
data division.
WORKING-STORAGE section.
01 source-str pic x(80)
value " this is a test of the internal voice communication system".
01 just-str pic x(80).
01 justification pic x.
01 result pic s9(9) comp-5.
procedure division.
move source-str to just-str.
*> Left justification
move "L" to justification.
perform demonstrate-justification.
*> case change to upper, demonstrate LENGTH verb
call "C$TOUPPER" using just-str
by value function length( just-str )
returning result
end-call.
*> Centre
move "C" to justification.
perform demonstrate-justification.
*> case change to lower
call "C$TOLOWER" using just-str
by value 80
returning result
end-call.
*> Right, default if no second argument
call "C$JUSTIFY" using just-str
returning result
end-call.
move "R" to justification.
perform show-justification.
exit program.
stop run.
*> ***************************************************************
demonstrate-justification.
call "C$JUSTIFY" using just-str
justification
returning result
end-call
if result not equal 0 then
display "Problem: " result
stop run
end-if
perform show-justification
.
*> ***************************************************************
show-justification.
evaluate justification
when "L" display "Left justify"
when "C" display "Centred (in UPPERCASE)"
when other display "Right justify"
end-evaluate
display "Source: |" source-str "|"
display "Justified: |" just-str "|"
display space
.
