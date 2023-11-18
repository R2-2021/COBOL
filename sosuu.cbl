*>GPT が記述したプログラムです、小文字は修正した。

IDENTIFICATION DIVISION.
PROGRAM-ID. PRIME-NUMBERS.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM PIC 9(5).
01 I PIC 9(5).
01 wk-div pic 9(5).
01 wk-rem pic 9(5).
01 PRIME PIC X(3) VALUE 'YES'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    DISPLAY 'ENTER A NUMBER: '.
    ACCEPT NUM.
    IF NUM <= 1
        DISPLAY 'NOT A PRIME NUMBER'
        STOP RUN
    END-IF.
    PERFORM VARYING I FROM 2 BY 1 UNTIL I > NUM / 2
*>      IF NUM MOD I = 0
        divide NUM by I giving wk-div remainder wk-rem
        display "NUM=" NUM " I=" I " wk-rem=" wk-rem
        if wk-rem = 0
            MOVE 'NO' TO PRIME
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF PRIME = 'YES'
        DISPLAY 'PRIME NUMBER'
    ELSE
        DISPLAY 'NOT A PRIME NUMBER'
    END-IF.
a.
    STOP RUN.