000010* @OPTIONS MAIN
000020*----------------------------------------------------------------
000030* ����(SIN)�A�]��(COS)�A����(TAN)�̃O���t��`�悵�܂��B
000040*----------------------------------------------------------------
000050 IDENTIFICATION DIVISION.
000060 PROGRAM-ID. SAMSIN.
000070 DATA DIVISION.
000080 WORKING-STORAGE SECTION.
000090 01 WK-A PIC X(1) VALUE SPACE.
000100 01 PI PIC S9(3)V9(15) VALUE 3.141592653589793.
000100 01 VAL PIC S9(3)V9(15).
000110 01 LINE-POS PIC 9(2).
000120 01 COL-POS PIC 9(2).
000130 01 GRAPH-CODE PIC X(1).
000140 01 COUNTER PIC 9(4) BINARY.
000150 01 S-COUNTER PIC S9(4) BINARY.
000160 PROCEDURE DIVISION. 
677
000170 DISPLAY "�ǂ̃O���t��`���܂����H(SIN:S, COS:C, TAN:T) >> "
000180 WITH NO ADVANCING.
000190 ACCEPT GRAPH-CODE.
000200 PERFORM TEST BEFORE
000210 VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 80
000220 DISPLAY "-" AT LINE 13 COLUMN COUNTER
000230 END-PERFORM.
000240 PERFORM TEST BEFORE
000250 VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER = 26
000260 DISPLAY "|" AT LINE COUNTER COLUMN 40
000270 END-PERFORM.
000280 DISPLAY "+" AT LINE 13 COLUMN 40.
000290*----------------------------------------------------------------
000300* ����(SIN)�̃O���t��`�悵�܂��B
000310*----------------------------------------------------------------
000320 EVALUATE GRAPH-CODE
000330 WHEN "S"
000340 PERFORM TEST BEFORE
000350 VARYING S-COUNTER FROM -39 BY 1 UNTIL S-COUNTER = 40
000360 COMPUTE VAL = 12 * (FUNCTION SIN(PI / 39 * S-COUNTER))
000370 COMPUTE LINE-POS ROUNDED = 13 - VAL
000380 COMPUTE COL-POS = 40 + S-COUNTER
000390 DISPLAY "*" AT LINE LINE-POS COLUMN COL-POS
000400 END-PERFORM
000410*----------------------------------------------------------------
000420* �]��(COS)�̃O���t��`�悵�܂��B
000430*----------------------------------------------------------------
000440 WHEN "C"
000450 PERFORM TEST BEFORE
000460 VARYING S-COUNTER FROM -39 BY 1 UNTIL S-COUNTER = 40
000470 COMPUTE VAL = 12 * (FUNCTION COS(PI / 39 * S-COUNTER))
000480 COMPUTE LINE-POS ROUNDED = 13 - VAL
000490 COMPUTE COL-POS = 40 + S-COUNTER
000500 DISPLAY "*" AT LINE LINE-POS COLUMN COL-POS
000510 END-PERFORM
000520*----------------------------------------------------------------
000530* ����(TAN)�̃O���t��`�悵�܂��B
000540*----------------------------------------------------------------
000550 WHEN "T"
000560 PERFORM TEST BEFORE
000570 VARYING S-COUNTER FROM -38 BY 1 UNTIL S-COUNTER = 39
000580 COMPUTE VAL = 0.5 * (FUNCTION TAN(PI / 2 / 39 * S-COUNTER))
000590 COMPUTE LINE-POS ROUNDED = 13 - VAL
000600 COMPUTE COL-POS = 40 + S-COUNTER
000610 DISPLAY "*" AT LINE LINE-POS COLUMN COL-POS
000620 END-PERFORM
000630 END-EVALUATE.
           ACCEPT WK-A.
000640 END PROGRAM
           SAMSIN.