*>cobc -x -free maincob.cbl subc.o

IDENTIFICATION DIVISION.
 PROGRAM-ID. maincob.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Arg1 PIC X(7).
 01 Arg2 PIC X(7).
 01 Arg3 USAGE BINARY-LONG.
 PROCEDURE DIVISION.
 000-Main.
 DISPLAY 'Starting cobmain'.
 MOVE 123456789 TO Arg3.
 STRING 'Arg1'
 X'00'
DELIMITED SIZE
INTO Arg1
 END-STRING.
 STRING 'Arg2'
 X'00'
DELIMITED SIZE
INTO Arg2
 END-STRING.
 CALL 'subc' USING BY CONTENT Arg1,
*> CALL 'subc' USING BY REFERENCE Arg1,
 BY REFERENCE Arg2,
BY REFERENCE Arg3.
 DISPLAY 'Back'.
 DISPLAY 'Arg1=' Arg1.
 DISPLAY 'Arg2=' Arg2.
 DISPLAY 'Arg3=' Arg3.
 DISPLAY 'Returned value='
 RETURN-CODE.
 STOP RUN.