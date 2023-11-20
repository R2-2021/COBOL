      *    *** COBXREF 用サブルーチン
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTING.
      *> *****************************************************************
      *> ** This subprogram generates a cross-reference listing of an **
      *> ** OpenCOBOL program. **
      *> ** **
      *> ** Linkage: CALL "LISTING" USING <source> **
      *> ** <xref> **
      *> ** <filename> **
      *> ** **
      *> ** Where: **
      *> ** <source> is a PIC X(1) flag indicating **
      *> ** whether or not a source listing **
      *> ** should be produced (space=NO, **
      *> ** non-space=yes) **
      *> ** <xref> is a PIC X(1) flag indicating **
      *> ** whether or not an xref listing **
      *> ** should be produced (space=NO, **
      *> ** non-space=yes) **
      *> ** <filename> is the [path]filename of the **
      *> ** program being listed and/or **
      *> ** xreffed in a PIC X(256) form. **
      *> *****************************************************************
      *> ** **
      *> ** AUTHOR: GARY L. CUTLER **
      *> ** CutlerGL@gmail.com **
      *> ** Copyright (C) 2010, Gary L. Cutler, GPL **
      *> ** **
      *> ** DATE-WRITTEN: April 1, 2010 **
      *> ** **
      *> *****************************************************************
      *> ** DATE CHANGE DESCRIPTION **
      *> ** ====== ==================================================== **
      *> **  Initial coding **
      *> *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT Expand-Code ASSIGN TO Expanded-Src-Filename
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT Report-File ASSIGN TO Report-Filename
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT Sort-File ASSIGN TO DISK.

       SELECT Source-Code ASSIGN TO Src-Filename
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT POT1-F ASSIGN WK-POT1-F-NAME
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Expand-Code. 
       01 Expand-Code-Rec.
       05 ECR-1 PIC X.
       05 ECR-2-256 PIC X(256).
       01 Expand-Code-Rec-Alt.
       05 ECR-1-128 PIC X(128).
       05 ECR-129-256 PIC X(128).

       FD Report-File.
       01 Report-Rec PIC X(135).

       SD Sort-File.
       01 Sort-Rec.
       05 SR-Prog-ID PIC X(15).
       05 SR-Token-UC PIC X(32).
       05 SR-Token PIC X(32).
       05 SR-Section PIC X(15).
       05 SR-Line-No-Def PIC 9(6).
       05 SR-Reference.
       10 SR-Line-No-Ref PIC 9(6).
       10 SR-Ref-Flag PIC X(1).

      *>
       FD Source-Code.
       01 Source-Code-Rec.
         05 SCR-1-128.
           10 FILLER PIC X(6).
           10 SCR-7 PIC X(1).
           10 FILLER PIC X(121).
         05 SCR-129-256 PIC X(128).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  POT1-PGM-ID     PIC  X(015).
           03  POT1-IDENT      PIC  X(032).
           03  POT1-DEF-NO-X.
             05  POT1-DEF-NO   PIC  9(006).
           03  POT1-SECTION    PIC  X(015).
           03  POT1-REF.
             05  POT1-REF-NO   PIC  9(006).
             05  POT1-DEFREF-ID PIC  X(001).

       WORKING-STORAGE SECTION.
       78 Line-Nos-Per-Rec VALUE 8.

       01 Cmd PIC X(256).

       01 Delim PIC X(2).

       01 Detail-Line-S.
       05 DLS-Line-No PIC ZZZZZ9.
       05 FILLER PIC X(1).
       05 DLS-Statement PIC X(128).

       01 Detail-Line-X.
       05 DLX-Prog-ID PIC X(15).
       05 FILLER PIC X(1).
       05 DLX-Token PIC X(32).
       05 FILLER PIC X(1).
       05 DLX-Line-No-Def PIC ZZZZZ9.
       05 FILLER PIC X(1).
       05 DLX-Section PIC X(15).
       05 FILLER PIC X(1).
       05 DLX-Reference OCCURS Line-Nos-Per-Rec TIMES.
       10 DLX-Line-No-Ref PIC ZZZZZ9.
       10 DLX-Ref-Flag PIC X(1).
       10 FILLER PIC X(1).

       01 Dummy PIC X(1).

       01 Env-TEMP PIC X(256).

       01 Expanded-Src-Filename PIC X(256).

       01 Filename PIC X(256).
       01 FileDEFREFname PIC X(004) VALUE "XREF".

       01 Flags.
       05 F-First-Record PIC X(1).
       05 F-In-Which-Pgm PIC X(1).
       88 In-Main-Module VALUE 'M'.
       88 In-Copybook VALUE 'C'.
       05 F-Last-Token-Ended-Sent PIC X(1).
       05 F-Processing-PICTURE PIC X(1).
       05 F-Token-Ended-Sentence PIC X(1).

       01 Group-Indicators.
       05 GI-Prog-ID PIC X(15).
       05 GI-Token PIC X(32).

       01 Heading-1S.
      *05 FILLER PIC X(125) VALUE
       05 FILLER PIC X(112) VALUE 
       "OpenCOBOL 1.1 06FEB2009 Source Listing - " &
       "OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL".
       05 FILLER   PIC X(006) VALUE " PAGE ".
       05 H1S-PAGE PIC Z,ZZ9.
       05          PIC X(002) VALUE SPACE.
       05 H1S-Date PIC 9999/99/99.

       01 Heading-1X.
      *05 FILLER PIC X(125) VALUE 
       05 FILLER PIC X(112) VALUE 

       "OpenCOBOL 1.1 06FEB2009 Cross-Reference Listing - " &
       "OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL".
       05          PIC X(006) VALUE " PAGE ".
       05 H1X-PAGE PIC Z,ZZ9.
       05          PIC X(002) VALUE SPACE.
       05 H1X-Date PIC 9999/99/99.

       01 Heading-2 PIC X(135).

       01 Heading-4S PIC X(16) VALUE
       "Line Statement".

       01 Heading-4X PIC X(96) VALUE
       "PROGRAM-ID Identifier/Register/Function Defn Wher" 
       & "e Defined References (* = Updated)".

       01 Heading-5S PIC X(135) VALUE
       "====== ====================================================="
       & "============================================================"
       & "===============".

       01 Heading-5X PIC X(135) VALUE
       "=============== ================================ ====== ===="
       & "=========== ================================================"
       &"===============".

       01 Held-Reference PIC X(100).

       01 I USAGE BINARY-LONG.

       01 J USAGE BINARY-LONG.

       01 WK-PAGE USAGE BINARY-LONG VALUE ZERO.

       01 Lines-Left USAGE BINARY-LONG.

       01 Lines-Per-Page USAGE BINARY-LONG.

       01 Lines-Per-Page-ENV PIC X(256).

       01 Num-UserNames USAGE BINARY-LONG.

       01 PIC-X10 PIC X(10).

       01 PIC-X32 PIC X(32).

       01 PIC-X256 PIC X(256).

       01 Program-Path PIC X(256).

      * 01 Report-Filename PIC X(256).

       01 Reserved-Words.
       05 FILLER PIC X(33) VALUE "IABS".
       05 FILLER PIC X(33) VALUE "VACCEPT".
       05 FILLER PIC X(33) VALUE " ACCESS".
       05 FILLER PIC X(33) VALUE "IACOS".
       05 FILLER PIC X(33) VALUE " ACTIVE-CLASS".
       05 FILLER PIC X(33) VALUE "VADD".
       05 FILLER PIC X(33) VALUE " ADDRESS".
       05 FILLER PIC X(33) VALUE " ADVANCING".
       05 FILLER PIC X(33) VALUE "KAFTER".
       05 FILLER PIC X(33) VALUE " ALIGNED".
       05 FILLER PIC X(33) VALUE " ALL".
       05 FILLER PIC X(33) VALUE "VALLOCATE".
       05 FILLER PIC X(33) VALUE " ALPHABET".
       05 FILLER PIC X(33) VALUE " ALPHABETIC".
       05 FILLER PIC X(33) VALUE " ALPHABETIC-LOWER".
       05 FILLER PIC X(33) VALUE " ALPHABETIC-UPPER".
       05 FILLER PIC X(33) VALUE " ALPHANUMERIC".
       05 FILLER PIC X(33) VALUE " ALPHANUMERIC-EDITED".
       05 FILLER PIC X(33) VALUE " ALSO".
       05 FILLER PIC X(33) VALUE "VALTER".
       05 FILLER PIC X(33) VALUE " ALTERNATE".
       05 FILLER PIC X(33) VALUE " AND".
       05 FILLER PIC X(33) VALUE "IANNUITY".
       05 FILLER PIC X(33) VALUE " ANY".
       05 FILLER PIC X(33) VALUE " ANYCASE".
       05 FILLER PIC X(33) VALUE " ARE".
       05 FILLER PIC X(33) VALUE " AREA".
       05 FILLER PIC X(33) VALUE " AREAS".
       05 FILLER PIC X(33) VALUE " ARGUMENT-NUMBER".
       05 FILLER PIC X(33) VALUE " ARGUMENT-VALUE".
       05 FILLER PIC X(33) VALUE " AS".
       05 FILLER PIC X(33) VALUE " ASCENDING".
       05 FILLER PIC X(33) VALUE "IASIN".
       05 FILLER PIC X(33) VALUE " ASSIGN".
       05 FILLER PIC X(33) VALUE " AT".
       05 FILLER PIC X(33) VALUE "IATAN". 
       05 FILLER PIC X(33) VALUE " AUTHOR".
       05 FILLER PIC X(33) VALUE " AUTO".
       05 FILLER PIC X(33) VALUE " AUTO-SKIP".
       05 FILLER PIC X(33) VALUE " AUTOMATIC".
       05 FILLER PIC X(33) VALUE " AUTOTERMINATE".
       05 FILLER PIC X(33) VALUE " BACKGROUND-COLOR".
       05 FILLER PIC X(33) VALUE " BASED".
       05 FILLER PIC X(33) VALUE " BEEP".
       05 FILLER PIC X(33) VALUE " BEFORE".
       05 FILLER PIC X(33) VALUE " BELL".
       05 FILLER PIC X(33) VALUE " BINARY".
       05 FILLER PIC X(33) VALUE " BINARY-C-LONG".
       05 FILLER PIC X(33) VALUE " BINARY-CHAR".
       05 FILLER PIC X(33) VALUE " BINARY-DOUBLE".
       05 FILLER PIC X(33) VALUE " BINARY-LONG".
       05 FILLER PIC X(33) VALUE " BINARY-SHORT".
       05 FILLER PIC X(33) VALUE " BIT".
       05 FILLER PIC X(33) VALUE " BLANK".
       05 FILLER PIC X(33) VALUE " BLINK".
       05 FILLER PIC X(33) VALUE " BLOCK".
       05 FILLER PIC X(33) VALUE " BOOLEAN".
       05 FILLER PIC X(33) VALUE " BOTTOM".
       05 FILLER PIC X(33) VALUE "YBY".
       05 FILLER PIC X(33) VALUE "IBYTE-LENGTH".
       05 FILLER PIC X(33) VALUE "MC01".
       05 FILLER PIC X(33) VALUE "MC02".
       05 FILLER PIC X(33) VALUE "MC03".
       05 FILLER PIC X(33) VALUE "MC04".
       05 FILLER PIC X(33) VALUE "MC05".
       05 FILLER PIC X(33) VALUE "MC06".
       05 FILLER PIC X(33) VALUE "MC07".
       05 FILLER PIC X(33) VALUE "MC08".
       05 FILLER PIC X(33) VALUE "MC09".
       05 FILLER PIC X(33) VALUE "MC10".
       05 FILLER PIC X(33) VALUE "MC11".
       05 FILLER PIC X(33) VALUE "MC12".
       05 FILLER PIC X(33) VALUE "VCALL".
       05 FILLER PIC X(33) VALUE "VCANCEL".
       05 FILLER PIC X(33) VALUE " CF".
       05 FILLER PIC X(33) VALUE " CH".
       05 FILLER PIC X(33) VALUE " CHAINING".
       05 FILLER PIC X(33) VALUE "ICHAR".
       05 FILLER PIC X(33) VALUE " CHARACTER".
       05 FILLER PIC X(33) VALUE " CHARACTERS".
       05 FILLER PIC X(33) VALUE " CLASS".
       05 FILLER PIC X(33) VALUE " CLASS-ID".
       05 FILLER PIC X(33) VALUE "VCLOSE".
       05 FILLER PIC X(33) VALUE "ICOB-CRT-STATUS".
       05 FILLER PIC X(33) VALUE " CODE".
       05 FILLER PIC X(33) VALUE " CODE-SET".
       05 FILLER PIC X(33) VALUE " COL".
       05 FILLER PIC X(33) VALUE " COLLATING".
       05 FILLER PIC X(33) VALUE " COLS".
       05 FILLER PIC X(33) VALUE " COLUMN".
       05 FILLER PIC X(33) VALUE " COLUMNS".
       05 FILLER PIC X(33) VALUE "ICOMBINED-DATETIME".
       05 FILLER PIC X(33) VALUE " COMMA".
       05 FILLER PIC X(33) VALUE " COMMAND-LINE".
       05 FILLER PIC X(33) VALUE "VCOMMIT".
       05 FILLER PIC X(33) VALUE " COMMON".
       05 FILLER PIC X(33) VALUE " COMP".
       05 FILLER PIC X(33) VALUE " COMP-1".
       05 FILLER PIC X(33) VALUE " COMP-2".
       05 FILLER PIC X(33) VALUE " COMP-3".
       05 FILLER PIC X(33) VALUE " COMP-4".
       05 FILLER PIC X(33) VALUE " COMP-5".
       05 FILLER PIC X(33) VALUE " COMP-X".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-1".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-2".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-3".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-4".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-5".
       05 FILLER PIC X(33) VALUE " COMPUTATIONAL-X".
       05 FILLER PIC X(33) VALUE "VCOMPUTE".
       05 FILLER PIC X(33) VALUE "ICONCATENATE".
       05 FILLER PIC X(33) VALUE " CONDITION".
       05 FILLER PIC X(33) VALUE "KCONFIGURATION".
       05 FILLER PIC X(33) VALUE "MCONSOLE".
       05 FILLER PIC X(33) VALUE " CONSTANT"
       .
       05 FILLER PIC X(33) VALUE " CONTAINS".
       05 FILLER PIC X(33) VALUE " CONTENT".
       05 FILLER PIC X(33) VALUE "VCONTINUE".
       05 FILLER PIC X(33) VALUE " CONTROL". 
       05 FILLER PIC X(33) VALUE " CONTROLS".
       05 FILLER PIC X(33) VALUE "KCONVERTING".
       05 FILLER PIC X(33) VALUE " COPY".
       05 FILLER PIC X(33) VALUE " CORR".
       05 FILLER PIC X(33) VALUE " CORRESPONDING".
       05 FILLER PIC X(33) VALUE "ICOS".
       05 FILLER PIC X(33) VALUE "KCOUNT".
       05 FILLER PIC X(33) VALUE " CRT".
       05 FILLER PIC X(33) VALUE " CURRENCY".
       05 FILLER PIC X(33) VALUE "ICURRENT-DATE".
       05 FILLER PIC X(33) VALUE " CURSOR".

       05 FILLER PIC X(33) VALUE " CYCLE".
       05 FILLER PIC X(33) VALUE "KDATA".
       05 FILLER PIC X(33) VALUE " DATA-POINTER".
       05 FILLER PIC X(33) VALUE " DATE".
       05 FILLER PIC X(33) VALUE " DATE-COMPILED".
       05 FILLER PIC X(33) VALUE " DATE-MODIFIED".
       05 FILLER PIC X(33) VALUE "IDATE-OF-INTEGER".
       05 FILLER PIC X(33) VALUE "IDATE-TO-YYYYMMDD".
       05 FILLER PIC X(33) VALUE " DATE-WRITTEN".
       05 FILLER PIC X(33) VALUE " DAY".
       05 FILLER PIC X(33) VALUE "IDAY-OF-INTEGER".
       05 FILLER PIC X(33) VALUE " DAY-OF-WEEK".
       05 FILLER PIC X(33) VALUE "IDAY-TO-YYYYDDD".
       05 FILLER PIC X(33) VALUE " DE".
       05 FILLER PIC X(33) VALUE " DEBUGGING".
       05 FILLER PIC X(33) VALUE " DECIMAL-POINT".
       05 FILLER PIC X(33) VALUE " DECLARATIVES".
       05 FILLER PIC X(33) VALUE " DEFAULT".
       05 FILLER PIC X(33) VALUE "VDELETE".
       05 FILLER PIC X(33) VALUE " DELIMITED".
       05 FILLER PIC X(33) VALUE "KDELIMITER".
       05 FILLER PIC X(33) VALUE " DEPENDING".
       05 FILLER PIC X(33) VALUE " DESCENDING".
       05 FILLER PIC X(33) VALUE " DESTINATION".
       05 FILLER PIC X(33) VALUE " DETAIL".
       05 FILLER PIC X(33) VALUE " DISABLE".
       05 FILLER PIC X(33) VALUE " DISK".
       05 FILLER PIC X(33) VALUE "VDISPLAY".
       05 FILLER PIC X(33) VALUE "VDIVIDE".
       05 FILLER PIC X(33) VALUE "KDIVISION".
       05 FILLER PIC X(33) VALUE "KDOWN".
       05 FILLER PIC X(33) VALUE " DUPLICATES".
       05 FILLER PIC X(33) VALUE " DYNAMIC".
       05 FILLER PIC X(33) VALUE "IE".
       05 FILLER PIC X(33) VALUE " EBCDIC".
       05 FILLER PIC X(33) VALUE " EC".
       05 FILLER PIC X(33) VALUE "VELSE".
       05 FILLER PIC X(33) VALUE " END".
       05 FILLER PIC X(33) VALUE " END-ACCEPT".
       05 FILLER PIC X(33) VALUE " END-ADD".
       05 FILLER PIC X(33) VALUE " END-CALL".
       05 FILLER PIC X(33) VALUE " END-COMPUTE".
       05 FILLER PIC X(33) VALUE " END-DELETE".
       05 FILLER PIC X(33) VALUE " END-DISPLAY".
       05 FILLER PIC X(33) VALUE " END-DIVIDE".
       05 FILLER PIC X(33) VALUE " END-EVALUATE".
       05 FILLER PIC X(33) VALUE " END-IF".
       05 FILLER PIC X(33) VALUE " END-MULTIPLY".
       05 FILLER PIC X(33) VALUE " END-OF-PAGE".
       05 FILLER PIC X(33) VALUE " END-PERFORM".
       05 FILLER PIC X(33) VALUE " END-READ".
       05 FILLER PIC X(33) VALUE " END-RETURN".
       05 FILLER PIC X(33) VALUE " END-REWRITE".
       05 FILLER PIC X(33) VALUE " END-SEARCH".
       05 FILLER PIC X(33) VALUE " END-START".
       05 FILLER PIC X(33) VALUE " END-STRING".
       05 FILLER PIC X(33) VALUE " END-SUBTRACT".
       05 FILLER PIC X(33) VALUE " END-UNSTRING".
       05 FILLER PIC X(33) VALUE " END-WRITE".
       05 FILLER PIC X(33) VALUE "VENTRY".
       05 FILLER PIC X(33) VALUE "KENVIRONMENT".
       05 FILLER PIC X(33) VALUE " ENVIRONMENT-NAME".
       05 FILLER PIC X(33) VALUE " ENVIRONMENT-VALUE".
       05 FILLER PIC X(33) VALUE " EO".
       05 FILLER PIC X(33) VALUE " EOL".
       05 FILLER PIC X(33) VALUE " EOP".
       05 FILLER PIC X(33) VALUE " EOS".
       05 FILLER PIC X(33) VALUE " EQUAL".
       05 FILLER PIC X(33) VALUE "KEQUALS".
       05 FILLER PIC X(33) VALUE " ERASE".
       05 FILLER PIC X(33) VALUE " ERROR".
       05 FILLER PIC X(33) VALUE " ESCAPE".
       05 FILLER PIC X(33) VALUE "VEVALUATE". 
       05 FILLER PIC X(33) VALUE " EXCEPTION".
       05 FILLER PIC X(33) VALUE "IEXCEPTION-FILE".
       05 FILLER PIC X(33) VALUE "IEXCEPTION-LOCATION".
       05 FILLER PIC X(33) VALUE " EXCEPTION-OBJECT".
       05 FILLER PIC X(33) VALUE "IEXCEPTION-STATEMENT".
       05 FILLER PIC X(33) VALUE "IEXCEPTION-STATUS".
       05 FILLER PIC X(33) VALUE " EXCLUSIVE".
       05 FILLER PIC X(33) VALUE "VEXIT".
       05 FILLER PIC X(33) VALUE "IEXP".
       05 FILLER PIC X(33) VALUE "IEXP10".
       05 FILLER PIC X(33) VALUE " EXTEND".
       05 FILLER PIC X(33) VALUE " EXTERNAL".
       05 FILLER PIC X(33) VALUE "IFACTORIAL".
       05 FILLER PIC X(33) VALUE " FACTORY".
       05 FILLER PIC X(33) VALUE " FALSE".
       05 FILLER PIC X(33) VALUE "KFD".
       05 FILLER PIC X(33) VALUE "KFILE".
       05 FILLER PIC X(33) VALUE " FILE-CONTROL".
       05 FILLER PIC X(33) VALUE " FILE-ID".
       05 FILLER PIC X(33) VALUE " FILLER".
       05 FILLER PIC X(33) VALUE " FINAL".
       05 FILLER PIC X(33) VALUE " FIRST".
       05 FILLER PIC X(33) VALUE " FLOAT-BINARY-16".
       05 FILLER PIC X(33) VALUE " FLOAT-BINARY-34".
       05 FILLER PIC X(33) VALUE " FLOAT-BINARY-7".
       05 FILLER PIC X(33) VALUE " FLOAT-DECIMAL-16".
       05 FILLER PIC X(33) VALUE " FLOAT-DECIMAL-34".
       05 FILLER PIC X(33) VALUE " FLOAT-EXTENDED".
       05 FILLER PIC X(33) VALUE " FLOAT-LONG".
       05 FILLER PIC X(33) VALUE " FLOAT-SHORT".
       05 FILLER PIC X(33) VALUE " FOOTING".
       05 FILLER PIC X(33) VALUE " FOR".
       05 FILLER PIC X(33) VALUE " FOREGROUND-COLOR".
       05 FILLER PIC X(33) VALUE " FOREVER".
       05 FILLER PIC X(33) VALUE " FORMAT".
       05 FILLER PIC X(33) VALUE "MFORMFEED".
       05 FILLER PIC X(33) VALUE "IFRACTION-PART".
       05 FILLER PIC X(33) VALUE "VFREE".
       05 FILLER PIC X(33) VALUE " FROM".
       05 FILLER PIC X(33) VALUE " FULL".
       05 FILLER PIC X(33) VALUE " FUNCTION".
       05 FILLER PIC X(33) VALUE " FUNCTION-ID".
       05 FILLER PIC X(33) VALUE " FUNCTION-POINTER".
       05 FILLER PIC X(33) VALUE "VGENERATE".
       05 FILLER PIC X(33) VALUE " GET".
       05 FILLER PIC X(33) VALUE "KGIVING".
       05 FILLER PIC X(33) VALUE " GLOBAL".
       05 FILLER PIC X(33) VALUE "VGO".
       05 FILLER PIC X(33) VALUE "VGOBACK".
       05 FILLER PIC X(33) VALUE " GREATER".
       05 FILLER PIC X(33) VALUE " GROUP".
       05 FILLER PIC X(33) VALUE " GROUP-USAGE".
       05 FILLER PIC X(33) VALUE " HEADING".
       05 FILLER PIC X(33) VALUE " HIGH-VALUE".
       05 FILLER PIC X(33) VALUE " HIGH-VALUES".
       05 FILLER PIC X(33) VALUE " HIGHLIGHT".
       05 FILLER PIC X(33) VALUE " I-O".
       05 FILLER PIC X(33) VALUE " I-O-CONTROL".
       05 FILLER PIC X(33) VALUE "KID".
       05 FILLER PIC X(33) VALUE "KIDENTIFICATION".
       05 FILLER PIC X(33) VALUE "VIF".
       05 FILLER PIC X(33) VALUE " IGNORE".
       05 FILLER PIC X(33) VALUE " IGNORING".
       05 FILLER PIC X(33) VALUE " IN".
       05 FILLER PIC X(33) VALUE " INDEX".
       05 FILLER PIC X(33) VALUE "KINDEXED".
       05 FILLER PIC X(33) VALUE " INDICATE".
       05 FILLER PIC X(33) VALUE " INFINITY".
       05 FILLER PIC X(33) VALUE " INHERITS".
       05 FILLER PIC X(33) VALUE " INITIAL".
       05 FILLER PIC X(33) VALUE " INITIALISED".
       05 FILLER PIC X(33) VALUE "VINITIALIZE".
       05 FILLER PIC X(33) VALUE " INITIALIZED".
       05 FILLER PIC X(33) VALUE "VINITIATE".
       05 FILLER PIC X(33) VALUE " INPUT".
       05 FILLER PIC X(33) VALUE "KINPUT-OUTPUT".
       05 FILLER PIC X(33) VALUE "VINSPECT".
       05 FILLER PIC X(33) VALUE " INSTALLATION".
       05 FILLER PIC X(33) VALUE "IINTEGER".
       05 FILLER PIC X(33) VALUE "IINTEGER-OF-DATE".
       05 FILLER PIC X(33) VALUE "IINTEGER-OF-DAY".
       05 FILLER PIC X(33) VALUE "IINTEGER-PART".
       05 FILLER PIC X(33) VALUE " INTERFACE".
       05 FILLER PIC X(33) VALUE " INTERFACE-ID". 
       05 FILLER PIC X(33) VALUE "KINTO".
       05 FILLER PIC X(33) VALUE " INTRINSIC".
       05 FILLER PIC X(33) VALUE " INVALID".
       05 FILLER PIC X(33) VALUE " INVOKE".
       05 FILLER PIC X(33) VALUE " IS".
       05 FILLER PIC X(33) VALUE " JUST".
       05 FILLER PIC X(33) VALUE " JUSTIFIED".
       05 FILLER PIC X(33) VALUE " KEY".
       05 FILLER PIC X(33) VALUE " LABEL".
       05 FILLER PIC X(33) VALUE " LAST".
       05 FILLER PIC X(33) VALUE " LEADING".
       05 FILLER PIC X(33) VALUE " LEFT".
       05 FILLER PIC X(33) VALUE " LEFT-JUSTIFY".
       05 FILLER PIC X(33) VALUE "ILENGTH".
       05 FILLER PIC X(33) VALUE " LESS".
       05 FILLER PIC X(33) VALUE " LIMIT".
       05 FILLER PIC X(33) VALUE " LIMITS".
       05 FILLER PIC X(33) VALUE " LINAGE".
       05 FILLER PIC X(33) VALUE "ILINAGE-COUNTER".
       05 FILLER PIC X(33) VALUE " LINE".
       05 FILLER PIC X(33) VALUE " LINE-COUNTER".
       05 FILLER PIC X(33) VALUE " LINES".
       05 FILLER PIC X(33) VALUE "KLINKAGE".
       05 FILLER PIC X(33) VALUE "KLOCAL-STORAGE".
       05 FILLER PIC X(33) VALUE " LOCALE".
       05 FILLER PIC X(33) VALUE "ILOCALE-DATE".
       05 FILLER PIC X(33) VALUE "ILOCALE-TIME".
       05 FILLER PIC X(33) VALUE "ILOCALE-TIME-FROM-SECONDS".
       05 FILLER PIC X(33) VALUE " LOCK".
       05 FILLER PIC X(33) VALUE "ILOG".
       05 FILLER PIC X(33) VALUE "ILOG10".
       05 FILLER PIC X(33) VALUE " LOW-VALUE".
       05 FILLER PIC X(33) VALUE " LOW-VALUES".
       05 FILLER PIC X(33) VALUE " LOWER".
       05 FILLER PIC X(33) VALUE "ILOWER-CASE".
       05 FILLER PIC X(33) VALUE " LOWLIGHT".
       05 FILLER PIC X(33) VALUE " MANUAL".
       05 FILLER PIC X(33) VALUE "IMAX".
       05 FILLER PIC X(33) VALUE "IMEAN".
       05 FILLER PIC X(33) VALUE "IMEDIAN".
       05 FILLER PIC X(33) VALUE " MEMORY".
       05 FILLER PIC X(33) VALUE "VMERGE".
       05 FILLER PIC X(33) VALUE " METHOD".
       05 FILLER PIC X(33) VALUE " METHOD-ID".
       05 FILLER PIC X(33) VALUE "IMIDRANGE".
       05 FILLER PIC X(33) VALUE "IMIN".
       05 FILLER PIC X(33) VALUE " MINUS".
       05 FILLER PIC X(33) VALUE "IMOD".
       05 FILLER PIC X(33) VALUE " MODE".
       05 FILLER PIC X(33) VALUE "VMOVE".
       05 FILLER PIC X(33) VALUE " MULTIPLE".
       05 FILLER PIC X(33) VALUE "VMULTIPLY".
       05 FILLER PIC X(33) VALUE " NATIONAL".
       05 FILLER PIC X(33) VALUE " NATIONAL-EDITED".
       05 FILLER PIC X(33) VALUE " NATIVE".
       05 FILLER PIC X(33) VALUE " NEGATIVE".
       05 FILLER PIC X(33) VALUE " NESTED".
       05 FILLER PIC X(33) VALUE "VNEXT".
       05 FILLER PIC X(33) VALUE " NO".
       05 FILLER PIC X(33) VALUE " NOT".
       05 FILLER PIC X(33) VALUE " NULL".
       05 FILLER PIC X(33) VALUE " NULLS".
       05 FILLER PIC X(33) VALUE " NUMBER".
       05 FILLER PIC X(33) VALUE "INUMBER-OF-CALL-PARAMETERS".
       05 FILLER PIC X(33) VALUE " NUMBERS".
       05 FILLER PIC X(33) VALUE " NUMERIC".
       05 FILLER PIC X(33) VALUE " NUMERIC-EDITED".
       05 FILLER PIC X(33) VALUE "INUMVAL".
       05 FILLER PIC X(33) VALUE "INUMVAL-C".
       05 FILLER PIC X(33) VALUE " OBJECT".
       05 FILLER PIC X(33) VALUE " OBJECT-COMPUTER".
       05 FILLER PIC X(33) VALUE " OBJECT-REFERENCE".
       05 FILLER PIC X(33) VALUE " OCCURS".
       05 FILLER PIC X(33) VALUE " OF".
       05 FILLER PIC X(33) VALUE " OFF".
       05 FILLER PIC X(33) VALUE " OMITTED".
       05 FILLER PIC X(33) VALUE " ON".
       05 FILLER PIC X(33) VALUE " ONLY".
       05 FILLER PIC X(33) VALUE "VOPEN".
       05 FILLER PIC X(33) VALUE " OPTIONAL".
       05 FILLER PIC X(33) VALUE " OPTIONS".
       05 FILLER PIC X(33) VALUE " OR".
       05 FILLER PIC X(33) VALUE "IORD".
       05 FILLER PIC X(33) VALUE "IORD-MAX". 
       05 FILLER PIC X(33) VALUE "IORD-MIN".
       05 FILLER PIC X(33) VALUE " ORDER".
       05 FILLER PIC X(33) VALUE " ORGANIZATION".
       05 FILLER PIC X(33) VALUE " OTHER".
       05 FILLER PIC X(33) VALUE " OUTPUT".
       05 FILLER PIC X(33) VALUE " OVERFLOW".
       05 FILLER PIC X(33) VALUE " OVERLINE".
       05 FILLER PIC X(33) VALUE " OVERRIDE".
       05 FILLER PIC X(33) VALUE " PACKED-DECIMAL".
       05 FILLER PIC X(33) VALUE " PADDING".
       05 FILLER PIC X(33) VALUE " PAGE".
       05 FILLER PIC X(33) VALUE " PAGE-COUNTER".
       05 FILLER PIC X(33) VALUE " PARAGRAPH".
       05 FILLER PIC X(33) VALUE "VPERFORM".
       05 FILLER PIC X(33) VALUE " PF".
       05 FILLER PIC X(33) VALUE " PH".
       05 FILLER PIC X(33) VALUE "IPI".
       05 FILLER PIC X(33) VALUE "KPIC".
       05 FILLER PIC X(33) VALUE "KPICTURE".
       05 FILLER PIC X(33) VALUE " PLUS".
       05 FILLER PIC X(33) VALUE "KPOINTER".
       05 FILLER PIC X(33) VALUE " POSITION".
       05 FILLER PIC X(33) VALUE " POSITIVE".
       05 FILLER PIC X(33) VALUE " PRESENT".
       05 FILLER PIC X(33) VALUE "IPRESENT-VALUE".
       05 FILLER PIC X(33) VALUE " PREVIOUS".
       05 FILLER PIC X(33) VALUE "MPRINTER".
       05 FILLER PIC X(33) VALUE " PRINTING".
       05 FILLER PIC X(33) VALUE "KPROCEDURE".
       05 FILLER PIC X(33) VALUE " PROCEDURE-POINTER".
       05 FILLER PIC X(33) VALUE " PROCEDURES".
       05 FILLER PIC X(33) VALUE " PROCEED".
       05 FILLER PIC X(33) VALUE " PROGRAM".
       05 FILLER PIC X(33) VALUE "KPROGRAM-ID".
       05 FILLER PIC X(33) VALUE " PROGRAM-POINTER".
       05 FILLER PIC X(33) VALUE " PROMPT".
       05 FILLER PIC X(33) VALUE " PROPERTY".
       05 FILLER PIC X(33) VALUE " PROTOTYPE".
       05 FILLER PIC X(33) VALUE " QUOTE".
       05 FILLER PIC X(33) VALUE " QUOTES".
       05 FILLER PIC X(33) VALUE " RAISE".
       05 FILLER PIC X(33) VALUE " RAISING".
       05 FILLER PIC X(33) VALUE "IRANDOM".
       05 FILLER PIC X(33) VALUE "IRANGE".
       05 FILLER PIC X(33) VALUE " RD".
       05 FILLER PIC X(33) VALUE "VREAD".
       05 FILLER PIC X(33) VALUE "VREADY".
       05 FILLER PIC X(33) VALUE " RECORD".
       05 FILLER PIC X(33) VALUE " RECORDING".
       05 FILLER PIC X(33) VALUE " RECORDS".
       05 FILLER PIC X(33) VALUE " RECURSIVE".
       05 FILLER PIC X(33) VALUE "KREDEFINES".
       05 FILLER PIC X(33) VALUE " REEL".
       05 FILLER PIC X(33) VALUE " REFERENCE".
       05 FILLER PIC X(33) VALUE " RELATIVE".
       05 FILLER PIC X(33) VALUE "VRELEASE".
       05 FILLER PIC X(33) VALUE "IREM".
       05 FILLER PIC X(33) VALUE " REMAINDER".
       05 FILLER PIC X(33) VALUE " REMARKS".
       05 FILLER PIC X(33) VALUE " REMOVAL".
       05 FILLER PIC X(33) VALUE "KRENAMES".
       05 FILLER PIC X(33) VALUE "KREPLACING".
       05 FILLER PIC X(33) VALUE "KREPORT".
       05 FILLER PIC X(33) VALUE " REPORTING".
       05 FILLER PIC X(33) VALUE " REPORTS".
       05 FILLER PIC X(33) VALUE " REPOSITORY".
       05 FILLER PIC X(33) VALUE " REPRESENTS-NOT-A-NUMBER".
       05 FILLER PIC X(33) VALUE " REQUIRED".
       05 FILLER PIC X(33) VALUE " RESERVE".
       05 FILLER PIC X(33) VALUE " RESUME".
       05 FILLER PIC X(33) VALUE " RETRY".
       05 FILLER PIC X(33) VALUE "VRETURN".
       05 FILLER PIC X(33) VALUE "IRETURN-CODE".
       05 FILLER PIC X(33) VALUE "KRETURNING".
       05 FILLER PIC X(33) VALUE "IREVERSE".
       05 FILLER PIC X(33) VALUE " REVERSE-VIDEO".
       05 FILLER PIC X(33) VALUE " REWIND".
       05 FILLER PIC X(33) VALUE "VREWRITE".
       05 FILLER PIC X(33) VALUE " RF".
       05 FILLER PIC X(33) VALUE " RH".
       05 FILLER PIC X(33) VALUE " RIGHT".
       05 FILLER PIC X(33) VALUE " RIGHT-JUSTIFY".
       05 FILLER PIC X(33) VALUE "VROLLBACK".
       05 FILLER PIC X(33) VALUE " ROUNDED". 
       05 FILLER PIC X(33) VALUE " RUN".
       05 FILLER PIC X(33) VALUE " SAME".
       05 FILLER PIC X(33) VALUE "KSCREEN".
       05 FILLER PIC X(33) VALUE " SCROLL".
       05 FILLER PIC X(33) VALUE "KSD".
       05 FILLER PIC X(33) VALUE "VSEARCH".
       05 FILLER PIC X(33) VALUE "ISECONDS-FROM-FORMATTED-TIME".
       05 FILLER PIC X(33) VALUE "ISECONDS-PAST-MIDNIGHT".
       05 FILLER PIC X(33) VALUE "KSECTION".
       05 FILLER PIC X(33) VALUE " SECURE".
       05 FILLER PIC X(33) VALUE " SECURITY".
       05 FILLER PIC X(33) VALUE " SEGMENT-LIMIT".
       05 FILLER PIC X(33) VALUE " SELECT".
       05 FILLER PIC X(33) VALUE " SELF".
       05 FILLER PIC X(33) VALUE " SENTENCE".
       05 FILLER PIC X(33) VALUE " SEPARATE".
       05 FILLER PIC X(33) VALUE " SEQUENCE".
       05 FILLER PIC X(33) VALUE " SEQUENTIAL".
       05 FILLER PIC X(33) VALUE "VSET".
       05 FILLER PIC X(33) VALUE " SHARING".
       05 FILLER PIC X(33) VALUE "ISIGN".
       05 FILLER PIC X(33) VALUE " SIGNED".
       05 FILLER PIC X(33) VALUE " SIGNED-INT".
       05 FILLER PIC X(33) VALUE " SIGNED-LONG".
       05 FILLER PIC X(33) VALUE " SIGNED-SHORT".
       05 FILLER PIC X(33) VALUE "ISIN".
       05 FILLER PIC X(33) VALUE " SIZE".
       05 FILLER PIC X(33) VALUE "VSORT".
       05 FILLER PIC X(33) VALUE " SORT-MERGE".
       05 FILLER PIC X(33) VALUE "ISORT-RETURN".
       05 FILLER PIC X(33) VALUE " SOURCE".
       05 FILLER PIC X(33) VALUE " SOURCE-COMPUTER".
       05 FILLER PIC X(33) VALUE " SOURCES".
       05 FILLER PIC X(33) VALUE " SPACE".
       05 FILLER PIC X(33) VALUE " SPACE-FILL".
       05 FILLER PIC X(33) VALUE " SPACES".
       05 FILLER PIC X(33) VALUE " SPECIAL-NAMES".
       05 FILLER PIC X(33) VALUE "ISQRT".
       05 FILLER PIC X(33) VALUE " STANDARD".
       05 FILLER PIC X(33) VALUE " STANDARD-1".
       05 FILLER PIC X(33) VALUE " STANDARD-2".
       05 FILLER PIC X(33) VALUE "ISTANDARD-DEVIATION".
       05 FILLER PIC X(33) VALUE "VSTART".
       05 FILLER PIC X(33) VALUE " STATUS".
       05 FILLER PIC X(33) VALUE "VSTOP".
       05 FILLER PIC X(33) VALUE "ISTORED-CHAR-LENGTH".
       05 FILLER PIC X(33) VALUE "VSTRING".
       05 FILLER PIC X(33) VALUE "ISUBSTITUTE".
       05 FILLER PIC X(33) VALUE "ISUBSTITUTE-CASE".
       05 FILLER PIC X(33) VALUE "VSUBTRACT".
       05 FILLER PIC X(33) VALUE "ISUM".
       05 FILLER PIC X(33) VALUE " SUPER".
       05 FILLER PIC X(33) VALUE "VSUPPRESS".
       05 FILLER PIC X(33) VALUE "MSWITCH-1".
       05 FILLER PIC X(33) VALUE "MSWITCH-2".
       05 FILLER PIC X(33) VALUE "MSWITCH-3".
       05 FILLER PIC X(33) VALUE "MSWITCH-4".
       05 FILLER PIC X(33) VALUE "MSWITCH-5".
       05 FILLER PIC X(33) VALUE "MSWITCH-6".
       05 FILLER PIC X(33) VALUE "MSWITCH-7".
       05 FILLER PIC X(33) VALUE "MSWITCH-8".
       05 FILLER PIC X(33) VALUE " SYMBOLIC".
       05 FILLER PIC X(33) VALUE " SYNC".
       05 FILLER PIC X(33) VALUE " SYNCHRONIZED".
       05 FILLER PIC X(33) VALUE "MSYSERR".
       05 FILLER PIC X(33) VALUE "MSYSIN".
       05 FILLER PIC X(33) VALUE "MSYSIPT".
       05 FILLER PIC X(33) VALUE "MSYSLIST".
       05 FILLER PIC X(33) VALUE "MSYSLST".
       05 FILLER PIC X(33) VALUE "MSYSOUT".
       05 FILLER PIC X(33) VALUE " SYSTEM-DEFAULT".
       05 FILLER PIC X(33) VALUE " TABLE".
       05 FILLER PIC X(33) VALUE "KTALLYING".
       05 FILLER PIC X(33) VALUE "ITAN".
       05 FILLER PIC X(33) VALUE " TAPE".
       05 FILLER PIC X(33) VALUE "VTERMINATE".
       05 FILLER PIC X(33) VALUE " TEST".
       05 FILLER PIC X(33) VALUE "ITEST-DATE-YYYYMMDD".
       05 FILLER PIC X(33) VALUE "ITEST-DAY-YYYYDDD".
       05 FILLER PIC X(33) VALUE " THAN".
       05 FILLER PIC X(33) VALUE " THEN".
       05 FILLER PIC X(33) VALUE " THROUGH".
       05 FILLER PIC X(33) VALUE " THRU".
       05 FILLER PIC X(33) VALUE " TIME". 
       05 FILLER PIC X(33) VALUE " TIMES".
       05 FILLER PIC X(33) VALUE "KTO".
       05 FILLER PIC X(33) VALUE " TOP".
       05 FILLER PIC X(33) VALUE " TRAILING".
       05 FILLER PIC X(33) VALUE " TRAILING-SIGN".
       05 FILLER PIC X(33) VALUE "VTRANSFORM".
       05 FILLER PIC X(33) VALUE "ITRIM".
       05 FILLER PIC X(33) VALUE " TRUE".
       05 FILLER PIC X(33) VALUE " TYPE".
       05 FILLER PIC X(33) VALUE " TYPEDEF".
       05 FILLER PIC X(33) VALUE " UNDERLINE".
       05 FILLER PIC X(33) VALUE " UNIT".
       05 FILLER PIC X(33) VALUE " UNIVERSAL".
       05 FILLER PIC X(33) VALUE "VUNLOCK".
       05 FILLER PIC X(33) VALUE " UNSIGNED".
       05 FILLER PIC X(33) VALUE " UNSIGNED-INT".
       05 FILLER PIC X(33) VALUE " UNSIGNED-LONG".
       05 FILLER PIC X(33) VALUE " UNSIGNED-SHORT".
       05 FILLER PIC X(33) VALUE "VUNSTRING".
       05 FILLER PIC X(33) VALUE " UNTIL".
       05 FILLER PIC X(33) VALUE "KUP".
       05 FILLER PIC X(33) VALUE " UPDATE".
       05 FILLER PIC X(33) VALUE " UPON".
       05 FILLER PIC X(33) VALUE " UPPER".
       05 FILLER PIC X(33) VALUE "IUPPER-CASE".
       05 FILLER PIC X(33) VALUE " USAGE".
       05 FILLER PIC X(33) VALUE "VUSE".
       05 FILLER PIC X(33) VALUE " USER-DEFAULT".
       05 FILLER PIC X(33) VALUE "KUSING".
       05 FILLER PIC X(33) VALUE " VAL-STATUS".
       05 FILLER PIC X(33) VALUE " VALID".
       05 FILLER PIC X(33) VALUE " VALIDATE".
       05 FILLER PIC X(33) VALUE " VALIDATE-STATUS".
       05 FILLER PIC X(33) VALUE " VALUE".
       05 FILLER PIC X(33) VALUE " VALUES".
       05 FILLER PIC X(33) VALUE "IVARIANCE".
       05 FILLER PIC X(33) VALUE "KVARYING".
       05 FILLER PIC X(33) VALUE " WAIT".
       05 FILLER PIC X(33) VALUE "VWHEN".
       05 FILLER PIC X(33) VALUE "IWHEN-COMPILED".
       05 FILLER PIC X(33) VALUE " WITH".
       05 FILLER PIC X(33) VALUE " WORDS".
       05 FILLER PIC X(33) VALUE "KWORKING-STORAGE".
       05 FILLER PIC X(33) VALUE "VWRITE".
       05 FILLER PIC X(33) VALUE "IYEAR-TO-YYYY".
       05 FILLER PIC X(33) VALUE " YYYYDDD".
       05 FILLER PIC X(33) VALUE " YYYYMMDD".
       05 FILLER PIC X(33) VALUE " ZERO".
       05 FILLER PIC X(33) VALUE " ZERO-FILL".
       05 FILLER PIC X(33) VALUE " ZEROES".
       05 FILLER PIC X(33) VALUE " ZEROS".
       01 Reserved-Word-Table REDEFINES Reserved-Words.
         05 Reserved-Word OCCURS 591 TIMES
            ASCENDING KEY RW-Word
            INDEXED RW-Idx.
           10 RW-Type PIC X(1).
           10 RW-Word PIC X(32).

       01 Saved-Section PIC X(15).

       01 Search-Token PIC X(32).

       01 Source-Line-No PIC 9(6).

       01 Src-Ptr USAGE BINARY-LONG.

       01 Syntax-Parsing-Items.
         05 SPI-Current-Char PIC X(1).
         88 Current-Char-Is-Punct VALUE "=", "(", ")", "*", "/",
       "&", ";", ",", "<", ">",
       ":".
         88 Current-Char-Is-Quote VALUE '"', "'".
         88 Current-Char-Is-X VALUE "x", "X".
         88 Current-Char-Is-Z VALUE "z", "Z".

         05 SPI-Current-Division PIC X(1).
         88 In-IDENTIFICATION-DIVISION VALUE "I", "?".
         88 In-ENVIRONMENT-DIVISION VALUE "E".
         88 In-DATA-DIVISION VALUE "D".
         88 In-PROCEDURE-DIVISION VALUE "P".

         05 SPI-Current-Line-No PIC 9(6).
         05 SPI-Current-Program-ID.
           10 FILLER PIC X(12).
           10 SPI-CP-13-15 PIC X(3).
         05 SPI-Current-Section. 
           10 SPI-CS-1 PIC X(1).
           10 SPI-CS-2-14.
             15 FILLER PIC X(10).
             15 SPI-CS-11-14 PIC X(3).
           10 SPI-CS-15 PIC X(1).
         05 SPI-Current-Token PIC X(32).
         05 SPI-Current-Token-UC PIC X(32).
         05 SPI-Current-Verb PIC X(12).
         05 SPI-Next-Char PIC X(1).
         88 Next-Char-Is-Quote VALUE '"', "'".

         05 SPI-Prior-Token PIC X(32).
         05 SPI-Token-Type PIC X(1).
         88 Token-Is-EOF VALUE HIGH-VALUES.
         88 Token-Is-Identifier VALUE "I".
         88 Token-Is-Key-Word VALUE "K", "V".
         88 Token-Is-Literal-Alpha VALUE "L".
         88 Token-Is-Literal-Number VALUE "N".
         88 Token-Is-Verb VALUE "V".

       01 Tally USAGE BINARY-LONG.

       01 Todays-Date PIC 9(8).
       01 WK-POT1-F-NAME PIC X(256).

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==WFD==.

       LINKAGE SECTION.
       01 Produce-Source-Listing PIC X(1).
       01 Produce-Xref-Listing PIC X(1).
       01 Src-Filename PIC X(256).
       01 Report-Filename PIC X(256).
       01 POT1-ID.
         03 POT1-OPEN  PIC X(001).
         03 POT1-CLOSE PIC X(001).

      *>
       PROCEDURE DIVISION USING Produce-Source-Listing
           Produce-Xref-Listing
           Src-Filename
           Report-Filename
           POT1-ID
           .

       000-Main SECTION.
       001-Init.

           PERFORM 100-Initialization

           PERFORM 200-Execute-cobc

           OPEN OUTPUT Report-File
           IF POT1-OPEN = "O"
              OPEN OUTPUT POT1-F
           ELSE
              OPEN EXTEND POT1-F
           END-IF

           IF Produce-Source-Listing NOT = SPACE
               PERFORM 500-Produce-Source-Listing
           END-IF

           IF Produce-Xref-Listing NOT = SPACE
              SORT Sort-File
                  ASCENDING KEY SR-Prog-ID
                  SR-Token-UC
                  SR-Line-No-Ref
                  INPUT PROCEDURE 300-Tokenize-Source
                  OUTPUT PROCEDURE 400-Produce-Xref-Listing
           END-IF

           CLOSE Report-File
      *     IF POT1-CLOSE = "C"
               CLOSE POT1-F
      *     END-IF
           GOBACK
           .
      *>
       100-Initialization SECTION.
      *> *****************************************************************
      *> ** Perform all program-wide initialization operations **
      *> *****************************************************************
       101-Establish-Working-Env.
           MOVE TRIM(Src-Filename,Leading) TO Src-Filename
           ACCEPT Env-TEMP
               FROM ENVIRONMENT "TEMP"
           END-ACCEPT

           ACCEPT Lines-Per-Page-ENV
               FROM ENVIRONMENT "OCXREF_LINES"
           END-ACCEPT

           INSPECT Src-Filename REPLACING ALL "\" BY "/"
           INSPECT Env-TEMP REPLACING ALL "\" BY "/"
           MOVE Src-Filename TO Program-Path
           MOVE Program-Path TO Heading-2
           CALL "C$JUSTIFY"
               USING Heading-2, "Right"
           END-CALL

           MOVE LENGTH(TRIM(Src-Filename,Trailing)) TO I
           MOVE 0 TO J

           PERFORM UNTIL Src-Filename(I:1) = '/'
               OR I = 0
               SUBTRACT 1 FROM I
               ADD 1 TO J
           END-PERFORM

           MOVE SPACE TO Filename
           UNSTRING Src-Filename((I + 1):J) DELIMITED BY "."
               INTO Filename, Dummy
           END-UNSTRING

           MOVE SPACE TO Expanded-Src-Filename
           STRING TRIM(Env-TEMP,Trailing)
               "/" TRIM(Filename,Trailing) ".i" 
               DELIMITED SIZE
               INTO Expanded-Src-Filename
           END-STRING

           MOVE SPACE TO Report-Filename
           STRING Program-Path(1:I)
               TRIM(Filename,Trailing)
               ".lst"
               DELIMITED SIZE
              INTO Report-Filename
           END-STRING

           MOVE SPACE TO WK-POT1-F-NAME
           STRING Program-Path(1:I)
      *         TRIM(Filename,Trailing)
               TRIM(FileDEFREFname,Trailing)
               ".defref"
               DELIMITED SIZE
              INTO WK-POT1-F-NAME
           END-STRING

           IF Lines-Per-Page-ENV NOT = SPACES
               MOVE NUMVAL(Lines-Per-Page-ENV) TO Lines-Per-Page
           ELSE
      *         MOVE 60 TO Lines-Per-Page
      *    *** sakura フォント高27、横、余白 上20mm、その他10mm
               MOVE 61 TO Lines-Per-Page
           END-IF

           ACCEPT Todays-Date FROM DATE YYYYMMDD
           END-ACCEPT

           MOVE Todays-Date TO H1X-Date
               H1S-Date
           MOVE "????????????..." TO SPI-Current-Program-ID
           MOVE SPACES TO SPI-Current-Verb
               Held-Reference
           MOVE "Y" TO F-First-Record
           .
      *>
       200-Execute-cobc SECTION.
       201-Build-Cmd.
           MOVE SPACE TO CMD
           STRING "cobc -E "
               TRIM(Program-Path, Trailing)
               " > "
               TRIM(Expanded-Src-Filename,Trailing)
               DELIMITED SIZE
               INTO Cmd
           END-STRING

           CALL "SYSTEM" USING Cmd
           END-CALL
      *     DISPLAY "Cmd=" Cmd
      *Cmd=cobc -E TEST03.CBL > C:/Users/xxxx/AppData/Local/Temp/TEST03.i

           IF RETURN-CODE NOT = 0
               DISPLAY "Cross-reference terminated by previous errors"
               UPON SYSERR
               END-DISPLAY
               GOBACK
           END-IF
           .
       209-Exit.
       EXIT.

       300-Tokenize-Source SECTION.
       301-Driver.
           OPEN INPUT Expand-Code

           MOVE SPACES TO Expand-Code-Rec
           MOVE 256 TO Src-Ptr
           MOVE 0 TO Num-UserNames
               SPI-Current-Line-No
           MOVE "?" TO SPI-Current-Division

           PERFORM FOREVER
               PERFORM 310-Get-Token

               IF Token-Is-EOF
                   EXIT PERFORM
               END-IF

               MOVE UPPER-CASE(SPI-Current-Token)
                   TO SPI-Current-Token-UC
               IF Token-Is-Verb
                   MOVE SPI-Current-Token-UC TO SPI-Current-Verb
                   SPI-Prior-Token

                   IF Held-Reference NOT = SPACES
                       MOVE Held-Reference TO Sort-Rec
                       MOVE SPACES TO Held-Reference
                       RELEASE Sort-Rec
                   END-IF
               END-IF

               EVALUATE TRUE
                   WHEN In-IDENTIFICATION-DIVISION
                     PERFORM 320-IDENTIFICATION-DIVISION
                   WHEN In-ENVIRONMENT-DIVISION
                     PERFORM 330-ENVIRONMENT-DIVISION
                   WHEN In-DATA-DIVISION
                     PERFORM 340-DATA-DIVISION
                   WHEN In-PROCEDURE-DIVISION
                     PERFORM 350-PROCEDURE-DIVISION
               END-EVALUATE 

               IF Token-Is-Key-Word
                   MOVE SPI-Current-Token-UC TO SPI-Prior-Token
               END-IF

               IF F-Token-Ended-Sentence = "Y"
                   AND SPI-Current-Division NOT = "I"
                   MOVE SPACES TO SPI-Prior-Token
                   SPI-Current-Verb
               END-IF

           END-PERFORM

           CLOSE Expand-Code
           EXIT SECTION
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       310-Get-Token.
           *>-- Position to 1st non-blank character
           MOVE F-Token-Ended-Sentence TO F-Last-Token-Ended-Sent
           MOVE "N" TO F-Token-Ended-Sentence

           PERFORM UNTIL Expand-Code-Rec(Src-Ptr : 1) NOT = SPACE
             IF Src-Ptr > 255
               READ Expand-Code AT END
                   IF Held-Reference NOT = SPACES
                       MOVE Held-Reference TO Sort-Rec
                       MOVE SPACES TO Held-Reference
                       RELEASE Sort-Rec
                   END-IF
                   SET Token-Is-EOF TO TRUE
                   MOVE 0 TO SPI-Current-Line-No
                   EXIT PARAGRAPH
               END-READ

               IF ECR-1 = "#"
                   PERFORM 311-Control-Record
               ELSE
                   PERFORM 312-Expand-Code-Record
               END-IF
             ELSE
               ADD 1 TO Src-Ptr
             END-IF
           END-PERFORM
          *>-- Extract token string
           MOVE Expand-Code-Rec(Src-Ptr : 1) TO SPI-Current-Char
           MOVE Expand-Code-Rec(Src-Ptr + 1: 1) TO SPI-Next-Char
           IF SPI-Current-Char = "."
               ADD 1 TO Src-Ptr
               MOVE SPI-Current-Char TO SPI-Current-Token
               MOVE SPACE TO SPI-Token-Type
               MOVE "Y" TO F-Token-Ended-Sentence
               EXIT PARAGRAPH
           END-IF

           IF Current-Char-Is-Punct
           AND SPI-Current-Char = "="
           AND SPI-Current-Division = "P"
           ADD 1 TO Src-Ptr
               MOVE "EQUALS" TO SPI-Current-Token
               MOVE "K" TO SPI-Token-Type
               EXIT PARAGRAPH
           END-IF

           IF Current-Char-Is-Punct 
           *> So subscripts don t get flagged w/ "*"
           AND SPI-Current-Char = "("
           AND SPI-Current-Division = "P"
               MOVE SPACES TO SPI-Prior-Token
           END-IF

           IF Current-Char-Is-Punct
               ADD 1 TO Src-Ptr
               MOVE SPI-Current-Char TO SPI-Current-Token
               MOVE SPACE TO SPI-Token-Type
               EXIT PARAGRAPH
           END-IF

           IF Current-Char-Is-Quote
               ADD 1 TO Src-Ptr
               UNSTRING Expand-Code-Rec
                 DELIMITED BY SPI-Current-Char
                 INTO SPI-Current-Token
                 WITH POINTER Src-Ptr
               END-UNSTRING

               IF Expand-Code-Rec(Src-Ptr : 1) = "."
                   MOVE "Y" TO F-Token-Ended-Sentence
                   ADD 1 TO Src-Ptr
               END-IF
               SET Token-Is-Literal-Alpha TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Current-Char-Is-X AND Next-Char-Is-Quote
               ADD 2 TO Src-Ptr 
               UNSTRING Expand-Code-Rec
                 DELIMITED BY SPI-Next-Char
                 INTO SPI-Current-Token
                 WITH POINTER Src-Ptr
               END-UNSTRING
               IF Expand-Code-Rec(Src-Ptr : 1) = "."
                   MOVE "Y" TO F-Token-Ended-Sentence
                   ADD 1 TO Src-Ptr
               END-IF
               SET Token-Is-Literal-Number TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Current-Char-Is-Z AND Next-Char-Is-Quote
               ADD 2 TO Src-Ptr
               UNSTRING Expand-Code-Rec
                 DELIMITED BY SPI-Next-Char
                 INTO SPI-Current-Token
                 WITH POINTER Src-Ptr
               END-UNSTRING

               IF Expand-Code-Rec(Src-Ptr : 1) = "."
                   MOVE "Y" TO F-Token-Ended-Sentence
                   ADD 1 TO Src-Ptr
               END-IF

               SET Token-Is-Literal-Alpha TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF F-Processing-PICTURE = "Y"
               UNSTRING Expand-Code-Rec
                 DELIMITED BY ". " OR " "
                 INTO SPI-Current-Token
                 DELIMITER IN Delim
                 WITH POINTER Src-Ptr
               END-UNSTRING

               IF Delim = ". "
                   MOVE "Y" TO F-Token-Ended-Sentence
                   ADD 1 TO Src-Ptr
               END-IF

               IF UPPER-CASE(SPI-Current-Token) = "IS"
                   MOVE SPACE TO SPI-Token-Type
                   EXIT PARAGRAPH
               ELSE
                   MOVE "N" TO F-Processing-PICTURE
                   MOVE SPACE TO SPI-Token-Type
                   EXIT PARAGRAPH
               END-IF
           END-IF

           UNSTRING Expand-Code-Rec
             DELIMITED BY ". " OR " " OR "=" OR "(" OR ")" OR "*"
             OR "/" OR "&" OR ";" OR "," OR "<"
             OR ">" OR ":"
             INTO SPI-Current-Token
             DELIMITER IN Delim
             WITH POINTER Src-Ptr
           END-UNSTRING

           IF Delim = ". "
               MOVE "Y" TO F-Token-Ended-Sentence
           END-IF

           IF Delim NOT = ". " AND " "
               SUBTRACT 1 FROM Src-Ptr
           END-IF

           *>-- Classify Token
           MOVE UPPER-CASE(SPI-Current-Token) TO Search-Token
           IF Search-Token = "EQUAL" OR "EQUALS"
               MOVE "EQUALS" TO SPI-Current-Token
               MOVE "K" TO SPI-Token-Type
               EXIT PARAGRAPH
           END-IF

           SEARCH ALL Reserved-Word
             WHEN RW-Word (RW-Idx) = Search-Token
                 MOVE RW-Type (RW-Idx) TO SPI-Token-Type
                 EXIT PARAGRAPH
           END-SEARCH

           *>-- Not a reserved word, must be a user name
           SET Token-Is-Identifier TO TRUE *> NEEDS EXPANSION!!!!

           PERFORM 313-Check-For-Numeric-Token

           IF Token-Is-Literal-Number
               IF (F-Last-Token-Ended-Sent = "Y")
               AND (SPI-Current-Division = "D")
                   MOVE "LEVEL #" TO SPI-Current-Token
                   MOVE "K" TO SPI-Token-Type
                   EXIT PARAGRAPH
               ELSE
                   EXIT PARAGRAPH
               END-IF 
           END-IF
           EXIT PARAGRAPH
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       311-Control-Record.
       UNSTRING ECR-2-256
           DELIMITED BY '"'
             INTO PIC-X10, PIC-X256, Dummy
           END-UNSTRING

           INSPECT PIC-X10 REPLACING ALL '"' BY SPACE
           COMPUTE I = NUMVAL(PIC-X10) - 1

           IF TRIM(PIC-X256,Trailing) = TRIM(Program-Path,Trailing)
               MOVE I TO SPI-Current-Line-No
               SET In-Main-Module TO TRUE
               IF Saved-Section NOT = SPACES
                   MOVE Saved-Section TO SPI-Current-Section
               END-IF
           ELSE
               SET In-Copybook TO TRUE
               IF Saved-Section = SPACES
                   MOVE SPI-Current-Section TO Saved-Section
               END-IF
               MOVE LENGTH(TRIM(PIC-X256,Trailing)) TO I
               MOVE 0 TO J

               PERFORM UNTIL PIC-X256(I:1) = '/'
                    OR I = 0
                   SUBTRACT 1 FROM I
                   ADD 1 TO J
               END-PERFORM

               UNSTRING PIC-X256((I + 1):J) DELIMITED BY "."
                 INTO Filename, Dummy
               END-UNSTRING
               MOVE "[" TO SPI-CS-1
               MOVE Filename TO SPI-CS-2-14

               IF SPI-CS-11-14 NOT = SPACES
                   MOVE "..." TO SPI-CS-11-14
               END-IF
               MOVE "]" TO SPI-CS-15
           END-IF

           MOVE SPACES TO Expand-Code-Rec *> Force another READ
           MOVE 256 TO Src-Ptr
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       312-Expand-Code-Record.
           MOVE 1 TO Src-Ptr
           IF In-Main-Module
               ADD 1 To SPI-Current-Line-No
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       313-Check-For-Numeric-Token.
           MOVE SPI-Current-Token TO PIC-X32
           INSPECT PIC-X32
           REPLACING TRAILING SPACES BY "0"

           IF PIC-X32 IS NUMERIC *> Simple Unsigned Integer
               SET Token-Is-Literal-Number TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF PIC-X32(1:1) = "+" OR "-"
               MOVE "0" TO PIC-X32(1:1)
           END-IF

           MOVE 0 TO Tally
           INSPECT PIC-X32
             TALLYING Tally FOR ALL "."
           IF Tally = 1
               INSPECT PIC-X32 REPLACING ALL "." BY "0"
           END-IF

           IF PIC-X32 IS NUMERIC
               SET Token-Is-Literal-Number TO TRUE
               EXIT PARAGRAPH
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       320-IDENTIFICATION-DIVISION.
           IF Token-Is-Key-Word AND SPI-Current-Token = "DIVISION"
           MOVE SPI-Prior-Token TO SPI-Current-Division
           EXIT PARAGRAPH
           END-IF
           IF SPI-Prior-Token = "PROGRAM-ID"
           MOVE SPACES TO SPI-Prior-Token
           MOVE SPI-Current-Token TO SPI-Current-Program-ID
           IF SPI-CP-13-15 NOT = SPACES
           MOVE "..." TO SPI-CP-13-15
           END-IF 
           EXIT PARAGRAPH
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           330-ENVIRONMENT-DIVISION.
           IF Token-Is-Key-Word AND SPI-Current-Token = "DIVISION"
           MOVE SPI-Prior-Token TO SPI-Current-Division
           EXIT PARAGRAPH
           END-IF
           IF Token-Is-Key-Word AND SPI-Current-Token = "SECTION"
           MOVE SPI-Prior-Token TO SPI-Current-Section
           EXIT PARAGRAPH
           END-IF
           IF Token-Is-Identifier
           PERFORM 361-Release-Ref
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           340-DATA-DIVISION.
           IF Token-Is-Key-Word AND SPI-Current-Token = "DIVISION"
           MOVE SPI-Prior-Token TO SPI-Current-Division
           EXIT PARAGRAPH
           END-IF
           IF Token-Is-Key-Word AND SPI-Current-Token = "SECTION"
           MOVE SPI-Prior-Token TO SPI-Current-Section
           EXIT PARAGRAPH
           END-IF
           IF (SPI-Current-Token = "PIC" OR "PICTURE")
           AND (Token-Is-Key-Word)
           MOVE "Y" TO F-Processing-PICTURE
           EXIT PARAGRAPH
           END-IF
           IF Token-Is-Identifier
           EVALUATE SPI-Prior-Token
           WHEN "FD"
           PERFORM 360-Release-Def
           MOVE SPACES TO SPI-Prior-Token
           WHEN "SD"
           PERFORM 360-Release-Def
           MOVE SPACES TO SPI-Prior-Token
           WHEN "LEVEL #"
           PERFORM 360-Release-Def
           MOVE SPACES TO SPI-Prior-Token
           WHEN "INDEXED"
           PERFORM 360-Release-Def
           MOVE SPACES TO SPI-Prior-Token
           WHEN "USING"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN "INTO"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           EXIT PARAGRAPH
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           350-PROCEDURE-DIVISION.
           IF SPI-Current-Section NOT = "PROCEDURE"
           MOVE "PROCEDURE" TO SPI-Current-Section
           END-IF
           IF Token-Is-Key-Word AND SPI-Current-Token = "DIVISION"
           MOVE SPI-Prior-Token TO SPI-Current-Division
           EXIT PARAGRAPH
           END-IF
           IF SPI-Current-Verb = SPACES
           IF Token-Is-Identifier
           PERFORM 360-Release-Def
           MOVE SPACES TO SPI-Prior-Token
           END-IF
           EXIT PARAGRAPH
           END-IF
           IF NOT Token-Is-Identifier
           EXIT PARAGRAPH
           END-IF
           EVALUATE SPI-Current-Verb
           WHEN "ACCEPT"
           PERFORM 351-ACCEPT
           WHEN "ADD"
           PERFORM 351-ADD
           WHEN "ALLOCATE"
           PERFORM 351-ALLOCATE 
           WHEN "CALL"
           PERFORM 351-CALL
           WHEN "COMPUTE"
           PERFORM 351-COMPUTE
           WHEN "DIVIDE"
           PERFORM 351-DIVIDE
           WHEN "FREE"
           PERFORM 351-FREE
           WHEN "INITIALIZE"
           PERFORM 351-INITIALIZE
           WHEN "INSPECT"
           PERFORM 351-INSPECT
           WHEN "MOVE"
           PERFORM 351-MOVE
           WHEN "MULTIPLY"
           PERFORM 351-MULTIPLY
           WHEN "PERFORM"
           PERFORM 351-PERFORM
           WHEN "SET"
           PERFORM 351-SET
           WHEN "STRING"
           PERFORM 351-STRING
           WHEN "SUBTRACT"
           PERFORM 351-SUBTRACT
           WHEN "TRANSFORM"
           PERFORM 351-TRANSFORM
           WHEN "UNSTRING"
           PERFORM 351-UNSTRING
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-ACCEPT.
           EVALUATE SPI-Prior-Token
           WHEN "ACCEPT"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-ADD.
           EVALUATE SPI-Prior-Token
           WHEN "GIVING"
           PERFORM 362-Release-Upd
           WHEN "TO"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-ALLOCATE.
           EVALUATE SPI-Prior-Token
           WHEN "ALLOCATE"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN "RETURNING"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-CALL.
           EVALUATE SPI-Prior-Token
           WHEN "RETURNING"
           PERFORM 362-Release-Upd
           WHEN "GIVING"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-COMPUTE.
           EVALUATE SPI-Prior-Token
           WHEN "COMPUTE"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE 
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-DIVIDE.
           EVALUATE SPI-Prior-Token
           WHEN "INTO"
           PERFORM 363-Set-Upd
           MOVE Sort-Rec TO Held-Reference
           WHEN "GIVING"
           IF Held-Reference NOT = SPACES
           MOVE Held-Reference To Sort-Rec
           MOVE SPACES To Held-Reference
           SR-Ref-Flag
           RELEASE Sort-Rec
           END-IF
           PERFORM 362-Release-Upd
           WHEN "REMAINDER"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-FREE.
           PERFORM 362-Release-Upd
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-INITIALIZE.
           EVALUATE SPI-Prior-Token
           WHEN "INITIALIZE"
           PERFORM 362-Release-Upd
           WHEN "REPLACING"
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-INSPECT.
           EVALUATE SPI-Prior-Token
           WHEN "INSPECT"
           PERFORM 364-Set-Ref
           MOVE SPACES TO Held-Reference
           MOVE SPACES TO SPI-Prior-Token
           WHEN "TALLYING"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN "REPLACING"
           IF Held-Reference NOT = SPACES
           MOVE Held-Reference TO Sort-Rec
           MOVE SPACES TO Held-Reference
           MOVE "*" TO SR-Ref-Flag
           RELEASE Sort-Rec
           END-IF
           MOVE SPACES TO SPI-Prior-Token
           WHEN "CONVERTING"
           IF Held-Reference NOT = SPACES
           MOVE Held-Reference TO Sort-Rec
           MOVE SPACES TO Held-Reference
           MOVE "*" TO SR-Ref-Flag
           RELEASE Sort-Rec
           END-IF
           MOVE SPACES TO SPI-Prior-Token
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-MOVE.
           EVALUATE SPI-Prior-Token
           WHEN "TO"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-MULTIPLY.
           EVALUATE SPI-Prior-Token
           WHEN "BY"
           PERFORM 363-Set-Upd
           MOVE Sort-Rec TO Held-Reference
           WHEN "GIVING"
           MOVE Held-Reference TO Sort-Rec
           MOVE SPACES TO Held-Reference
           SR-Ref-Flag
           RELEASE Sort-Rec 
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-PERFORM.
           EVALUATE SPI-Prior-Token
           WHEN "VARYING"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN "AFTER"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-SET.
           EVALUATE SPI-Prior-Token
           WHEN "SET"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-STRING.
           EVALUATE SPI-Prior-Token
           WHEN "INTO"
           PERFORM 362-Release-Upd
           WHEN "POINTER"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-SUBTRACT.
           EVALUATE SPI-Prior-Token
           WHEN "GIVING"
           PERFORM 362-Release-Upd
           WHEN "FROM"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-TRANSFORM.
           EVALUATE SPI-Prior-Token
           WHEN "TRANSFORM"
           PERFORM 362-Release-Upd
           MOVE SPACES TO SPI-Prior-Token
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           351-UNSTRING.
           EVALUATE SPI-Prior-Token
           WHEN "INTO"
           PERFORM 362-Release-Upd
           WHEN "DELIMITER"
           PERFORM 362-Release-Upd
           WHEN "COUNT"
           PERFORM 362-Release-Upd
           WHEN "POINTER"
           PERFORM 362-Release-Upd
           WHEN "TALLYING"
           PERFORM 362-Release-Upd
           WHEN OTHER
           PERFORM 361-Release-Ref
           END-EVALUATE
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           360-Release-Def.
           MOVE SPACES TO Sort-Rec
           MOVE SPI-Current-Program-ID TO SR-Prog-ID
           MOVE SPI-Current-Token-UC TO SR-Token-UC
           MOVE SPI-Current-Token TO SR-Token
           MOVE SPI-Current-Section TO SR-Section
           MOVE SPI-Current-Line-No TO SR-Line-No-Def 
           MOVE 0 TO SR-Line-No-Ref
           RELEASE Sort-Rec
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           361-Release-Ref.
           PERFORM 364-Set-Ref
           RELEASE Sort-Rec
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           362-Release-Upd.
           PERFORM 363-Set-Upd
           RELEASE Sort-Rec
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           363-Set-Upd.
           MOVE SPACES TO Sort-Rec
           MOVE SPI-Current-Program-ID TO SR-Prog-ID
           MOVE SPI-Current-Token-UC TO SR-Token-UC
           MOVE SPI-Current-Token TO SR-Token
           MOVE SPI-Current-Section TO SR-Section
           MOVE SPI-Current-Line-No TO SR-Line-No-Ref
           MOVE "*" TO SR-Ref-Flag
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           364-Set-Ref.
           MOVE SPACES TO Sort-Rec
           MOVE SPI-Current-Program-ID TO SR-Prog-ID
           MOVE SPI-Current-Token-UC TO SR-Token-UC
           MOVE SPI-Current-Token TO SR-Token
           MOVE SPI-Current-Section TO SR-Section
           MOVE SPI-Current-Line-No TO SR-Line-No-Ref
           .
          *>
       400-Produce-Xref-Listing SECTION.
           401-Init.
           MOVE SPACES TO Detail-Line-X
           Group-Indicators
           MOVE 0 TO I
           Lines-Left
           .

       402-Process-Sorted-Recs.
           PERFORM FOREVER
               RETURN Sort-File AT END
                   EXIT PERFORM
               END-RETURN

      *     MOVE    "P"         TO      WFD-ID
      *     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                 Sort-Rec

               IF SR-Prog-ID NOT = GI-Prog-ID
               OR SR-Token-UC NOT = GI-Token
                   IF Detail-Line-X NOT = SPACES
                   PERFORM 410-Generate-Report-Line
                   END-IF

                   IF SR-Prog-ID NOT = GI-Prog-ID
                       MOVE 0 TO Lines-Left
                   END-IF
                   MOVE SR-Prog-ID TO GI-Prog-ID
                   MOVE SR-Token-UC TO GI-Token
               END-IF

               MOVE SR-Prog-ID     TO POT1-PGM-ID
               MOVE SR-Token       TO POT1-IDENT
               MOVE SR-Section     TO POT1-SECTION

               IF Detail-Line-X = SPACES
                   MOVE SR-Prog-ID TO DLX-Prog-ID
                   MOVE SR-Token TO DLX-Token
                   MOVE SR-Section TO DLX-Section
                   IF SR-Line-No-Def NOT = SPACES
                       MOVE SR-Line-No-Def TO DLX-Line-No-Def
                                              POT1-DEF-NO
                   ELSE
                       MOVE SPACE TO POT1-DEF-NO-X
                   END-IF
               END-IF

               IF SR-Reference > '000000'
                   ADD 1 TO I
                   IF I > Line-Nos-Per-Rec
                       PERFORM 410-Generate-Report-Line
                       MOVE 1 TO I
                   END-IF
                   MOVE SR-Line-No-Ref TO DLX-Line-No-Ref (I)
                   MOVE SR-Ref-Flag TO DLX-Ref-Flag (I)

                   MOVE SR-Line-No-Ref TO POT1-REF-NO
                   MOVE SR-Ref-Flag    TO POT1-DEFREF-ID
                   WRITE POT1-REC
               ELSE
                   MOVE SPACE TO POT1-REF
                   WRITE POT1-REC
               END-IF
           END-PERFORM

           IF Detail-Line-X NOT = SPACES
               PERFORM 410-Generate-Report-Line
           END-IF
           EXIT SECTION
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       410-Generate-Report-Line.
           IF Lines-Left < 1
             IF F-First-Record = "Y" 
               MOVE "N" TO F-First-Record
               ADD 1 TO WK-PAGE
               MOVE WK-PAGE TO H1X-PAGE
               WRITE Report-Rec FROM Heading-1X BEFORE 1
             ELSE
               MOVE SPACES TO Report-Rec
               WRITE Report-Rec BEFORE PAGE
               MOVE SPACES TO Report-Rec
               WRITE Report-Rec BEFORE 1
               ADD 1 TO WK-PAGE
               MOVE WK-PAGE TO H1X-PAGE
               WRITE Report-Rec FROM Heading-1X BEFORE 1
             END-IF

             WRITE Report-Rec FROM Heading-2 BEFORE 1
             WRITE Report-Rec FROM Heading-4X BEFORE 1
             WRITE Report-Rec FROM Heading-5X BEFORE 1
             COMPUTE
             Lines-Left = Lines-Per-Page - 4
             END-COMPUTE
           END-IF
           
      *     DISPLAY "410"
      *     DISPLAY Detail-Line-X(1:80)
           
           WRITE Report-Rec FROM Detail-Line-X BEFORE 1
           MOVE SPACES TO Detail-Line-X
           MOVE 0 TO I
           SUBTRACT 1 FROM Lines-Left
           .
          *>
       500-Produce-Source-Listing SECTION.
       501-Generate-Source-Listing.
      *     DISPLAY "501"
           OPEN INPUT Source-Code
           Expand-Code
           MOVE 0 TO Source-Line-No
           PERFORM FOREVER
           READ Expand-Code AT END
      *     DISPLAY "501"
      *     DISPLAY "Lines-Left=" Lines-Left
           MOVE SPACES TO Report-Rec
           WRITE Report-Rec BEFORE Lines-Left
           EXIT PERFORM
           END-READ
           IF ECR-1 = "#"
           PERFORM 510-Control-Record
           ELSE
           PERFORM 520-Expand-Code-Record
           END-IF
           END-PERFORM
           CLOSE Source-Code
           Expand-Code
           EXIT SECTION
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       510-Control-Record.
      *     DISPLAY "510"
           UNSTRING ECR-2-256
           DELIMITED BY """"
            INTO PIC-X10, PIC-X256, Dummy
           END-UNSTRING
           IF TRIM(PIC-X256,Trailing) = TRIM(Program-Path,Trailing) *> Main Pgm
           SET In-Main-Module TO TRUE
           IF Source-Line-No > 0
           READ Expand-Code END-READ
           END-IF
           ELSE *> COPY
           SET In-Copybook TO TRUE
           END-IF
           .
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       520-Expand-Code-Record.
      *     DISPLAY "520"
           IF In-Main-Module
               ADD 1 To SPI-Current-Line-No
               READ Source-Code AT END NEXT SENTENCE END-READ

      *     DISPLAY "520"
      *     DISPLAY SCR-1-128(1:80)
      *     DISPLAY SCR-129-256 (1:80)
      *     DISPLAY Expand-Code-Rec(1:80)

               ADD 1 TO Source-Line-No
               MOVE SPACES TO Detail-Line-S
               MOVE Source-Line-No TO DLS-Line-No
               MOVE SCR-1-128 TO DLS-Statement
               IF SCR-7 = "/"
                   MOVE 0 TO Lines-Left
               END-IF
               PERFORM 530-Generate-Source-Line
               IF SCR-129-256 NOT = SPACES
                   MOVE SPACES TO Detail-Line-S
                   MOVE SCR-129-256 TO DLS-Statement
                   PERFORM 530-Generate-Source-Line
               END-IF
           ELSE

      *        DISPLAY "520"
      *        DISPLAY SCR-1-128(1:80)
      *        DISPLAY SCR-129-256 (1:80)
      *        DISPLAY Expand-Code-Rec(1:80)
      *        DISPLAY "1 " Detail-Line-S(1:78)

               IF Expand-Code-Rec NOT = SPACES
                   MOVE SPACES TO Detail-Line-S
      *             MOVE ECR-1-128 TO DLS-Statement
      
      *    *** COPY句 レベル番号 桁下げて出力
                   EVALUATE TRUE
                       WHEN ECR-1-128 (1:2) = "01"
                            MOVE ECR-1-128 TO DLS-Statement (1:128)
                       WHEN ECR-1-128 (1:2) = "02"
                            MOVE ECR-1-128 TO DLS-Statement (3:126)
                       WHEN ECR-1-128 (1:2) = "03"
                            MOVE ECR-1-128 TO DLS-Statement (5:124)
                       WHEN ECR-1-128 (1:2) = "04"
                            MOVE ECR-1-128 TO DLS-Statement (7:122)
                       WHEN ECR-1-128 (1:2) = "05"
                            MOVE ECR-1-128 TO DLS-Statement (9:120)
                       WHEN ECR-1-128 (1:2) = "06"
                            MOVE ECR-1-128 TO DLS-Statement(11:118)
                       WHEN ECR-1-128 (1:2) = "07"
                            MOVE ECR-1-128 TO DLS-Statement(13:116)
                       WHEN ECR-1-128 (1:2) = "08"
                            MOVE ECR-1-128 TO DLS-Statement(15:114)
                       WHEN ECR-1-128 (1:2) = "09"
                            MOVE ECR-1-128 TO DLS-Statement(17:112)
                       WHEN ECR-1-128 (1:2) = "10"
                            MOVE ECR-1-128 TO DLS-Statement(19:110)
                       WHEN OTHER
                            MOVE ECR-1-128 TO DLS-Statement (1:128)
                   END-EVALUATE

      *        DISPLAY "2 " Detail-Line-S(1:78)
      *        DISPLAY "2 " Detail-Line-S(1:78)

                   PERFORM 530-Generate-Source-Line
                   IF ECR-129-256 NOT = SPACES
                       MOVE SPACES TO Detail-Line-S
                       MOVE ECR-129-256 TO DLS-Statement
      *         DISPLAY "3 " Detail-Line-S(1:78)
                       PERFORM 530-Generate-Source-Line
                   END-IF 
               END-IF
           END-IF
           .
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       530-Generate-Source-Line.
      *     DISPLAY "530"
           IF Lines-Left < 1
           IF F-First-Record = "Y"
           MOVE "N" TO F-First-Record
           ADD 1 TO WK-PAGE
           MOVE WK-PAGE TO H1S-PAGE
           WRITE Report-Rec FROM Heading-1S BEFORE 1
           ELSE
           MOVE SPACES TO Report-Rec
           WRITE Report-Rec BEFORE PAGE
           MOVE SPACES TO Report-Rec
           WRITE Report-Rec BEFORE 1
           ADD 1 TO WK-PAGE
           MOVE WK-PAGE TO H1S-PAGE
           WRITE Report-Rec FROM Heading-1S BEFORE 1
           END-IF
           WRITE Report-Rec FROM Heading-2 BEFORE 1
           WRITE Report-Rec FROM Heading-4S BEFORE 1
           WRITE Report-Rec FROM Heading-5S BEFORE 1
           COMPUTE
           Lines-Left = Lines-Per-Page - 4
           END-COMPUTE
           END-IF
           
      *   DISPLAY "530"
      *    DISPLAY Detail-Line-S(1:80)
           
           WRITE Report-Rec FROM Detail-Line-S BEFORE 1
           MOVE SPACES TO Detail-Line-S
           SUBTRACT 1 FROM Lines-Left
           .

           END PROGRAM LISTING. 
