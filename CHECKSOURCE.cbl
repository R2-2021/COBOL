IDENTIFICATION DIVISION.
 PROGRAM-ID. CHECKSOURCE.
 *>****************************************************************
 *>* This subprogram will scan a line of source code it is given *>*
 *>* looking for "LINKAGE SECTION" or "IDENTIFICATION DIVISION". **
 *>* **
 *>* ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE*** **
 *>* **
 *>* These two strings must be found IN THEIR ENTIRETY within **
 *>* the 1st 80 columns of program source records, and cannot **
 *>* follow either a "*>" sequence OR a "*" in col 7. **
 *>****************************************************************
 *>* DATE CHANGE DESCRIPTION **
 *>* ====== ==================================================== **
 *>* GC0809 Initial coding. **
 *>*****************************************************************
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
 FUNCTION ALL INTRINSIC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Compressed-Src.
 05 CS-Char OCCURS 80 TIMES PIC X(1).

 01 Flags.
 05 F-Found-SPACE PIC X(1).
 88 88-Skipping-SPACE VALUE "Y".
 88 88-Not-Skipping-SPACE VALUE "N".

 01 I USAGE BINARY-CHAR.

 01 J USAGE BINARY-CHAR.
 LINKAGE SECTION.
 01 Argument-1.
 02 A1-Char OCCURS 80 TIMES PIC X(1).

 01 Argument-2 PIC X(1).
 88 88-A2-LINKAGE-SECTION VALUE "L".
 88 88-A2-IDENTIFICATION-DIVISION VALUE "I".
 88 88-A2-Nothing-Special VALUE " ".


 PROCEDURE DIVISION USING Argument-1, Argument-2.

 000-Main SECTION.

 010-Initialize.
 SET 88-A2-Nothing-Special TO TRUE
 IF A1-Char (7) = "*"
 GOBACK
 END-IF
 .

 020-Compress-Multiple-SPACES.
 SET 88-Not-Skipping-SPACE TO TRUE
 MOVE 0 TO J
 MOVE SPACES TO Compressed-Src
 PERFORM VARYING I FROM 1 BY 1
 UNTIL I > 80
 IF A1-Char (I) = SPACE
 IF 88-Not-Skipping-SPACE
 ADD 1 TO J
 MOVE UPPER-CASE(A1-Char (I)) TO CS-Char (J)
 SET 88-Skipping-SPACE TO TRUE
 END-IF
 ELSE
 SET 88-Not-Skipping-SPACE TO TRUE
 ADD 1 TO J 
 MOVE A1-Char (I) TO CS-Char (J)
 END-IF
 END-PERFORM
 .

 030-Scan-Compressed-Src.
 PERFORM VARYING I FROM 1 BY 1
     UNTIL I > 66
     EVALUATE TRUE
         WHEN CS-Char (I) = "*"
              IF Compressed-Src (I : 2) = "*>"
                 GOBACK
              END-IF
         WHEN (CS-Char (I) = "L") AND (I < 66)
              IF Compressed-Src (I : 15) = "LINKAGE SECTION"
                 SET 88-A2-LINKAGE-SECTION TO TRUE
                 GOBACK
              END-IF
         WHEN (CS-Char (I) = "I") AND (I < 58)
              IF Compressed-Src (I : 23) = "IDENTIFICATION " &
                 "DIVISION"
                 SET 88-A2-IDENTIFICATION-DIVISION TO TRUE
                 GOBACK
              END-IF
     END-EVALUATE
 END-PERFORM
 .

 099-Never-Found-Either-One.
 GOBACK
 .
 END PROGRAM CHECKSOURCE. 
