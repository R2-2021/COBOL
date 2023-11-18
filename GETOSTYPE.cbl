
 IDENTIFICATION DIVISION.
 PROGRAM-ID. GETOSTYPE.
 *>****************************************************************
 *>* This subprogram determine the OS type the program is run- **
 *>* ning under, passing that result back in RETURN-CODE as fol- **
 *>* lows: **
 *>* **
 *>* 0: Cannot be determined **
 *>* 1: Native Windows or Windows/MinGW **
 *>* 2: Cygwin **
 *>* 3: UNIX/Linux/MacOS **
 *>****************************************************************
 *>* DATE CHANGE DESCRIPTION **
 *>>* ====== ==================================================== **
 *>* GC0909 Initial coding. **
 *>****************************************************************
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
 FUNCTION ALL INTRINSIC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Env-Path PIC X(1024).
 01 Tally1 USAGE BINARY-LONG.

 PROCEDURE DIVISION.
 000-Main SECTION.
 010-Get-TEMP-Var.
   MOVE SPACES TO Env-Path
   ACCEPT Env-Path
     FROM ENVIRONMENT "PATH"
     ON EXCEPTION
       MOVE 0 TO RETURN-CODE
       GOBACK
    END-ACCEPT

 IF Env-Path = SPACES
   MOVE 0 TO RETURN-CODE
 ELSE
   MOVE 0 TO Tally1
   INSPECT Env-Path 
     TALLYING Tally1 FOR ALL ";"
   IF Tally1 = 0 *> Must be some form of UNIX
     MOVE 0 TO Tally1
     INSPECT Env-Path
       TALLYING TALLY1 FOR ALL "/cygdrive/"
     IF Tally1 = 0 *> UNIX/MacOS
       MOVE 3 TO RETURN-CODE
     ELSE *> Cygwin
       MOVE 2 TO RETURN-CODE
     END-IF
   ELSE *> Assume Windows[/MinGW]
     MOVE 1 TO RETURN-CODE
   END-IF
 END-IF
 GOBACK
 .
 END PROGRAM GETOSTYPE.
