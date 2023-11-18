 IDENTIFICATION DIVISION.
 PROGRAM-ID. OCic.
 *>****************************************************************
 *>* This program provides a Textual User Interface (TUI) to the **
 *>* process of compiling and (optionally) executing an OpenCOBOL**
 *>* program. **
 *>* **
 *>* This programs execution syntax is as follows: **
 *>* **
 *>* ocic <program-path-and-filename> [ <switch>... ] **
 *>* **
 *>* Once executed, a display screen will be presented showing **
 *>* the compilation options that will be used. The user will **
 *>*. have the opportunity to change options, specify new ones **
 *>* and specify any program execution arguments to be used if **
 *>* you select the "Execute" option. When you press the Enter **
 *>* key the program will be compiled. **
 *>* **
 *>* The SCREEN SECTION contains an image of the screen. **
 *>* **
 *>* The "010-Parse-Args" section in the PROCEDURE DIVISION has **
 *>* documentation on switches and their function. **
 *>****************************************************************
 *>* **
 *>* AUTHOR: GARY L. CUTLER **
 *>* CutlerGL@gmail.com **
 *>* Copyright (C) 2009-2010, Gary L. Cutler, GPL **
 *>* **
 *>* DATE-WRITTEN: June 14, 2009 **
 *>* **
 *>****************************************************************
 *>* Note: Depending on which extended DISPLAY handler you re **
 *>* using (PDCurses, Curses, ...), you may need to un- **
 *>* comment any source lines tagged with "SCROLL" in cols **
 *>* 1-6 in order to have error messages scroll properly **
 *>* in the OCic shell window. **
 *>****************************************************************
 *>* DATE CHANGE DESCRIPTION ** 
 *>* ====== ==================================================== **
 *>*  Don t display compiler messages file if compilation **
 *>* Is successful. Also don t display messages if the **
 *>* output file is busy (just put a message on the **
 *>* screen, leave the OC screen up & let the user fix **
 *>* the problem & resubmit. **
 *>*  When 'EXECUTE' is selected, a 'FILE BUSY' error will **
 *>* still cause the (old) executable to be launched. **
 *>* Also, the 'EXTRA SWITCHES' field is being ignored. **
 *>* Changed the title bar to lowlighted reverse video & **
 *>* the message area to highlighted reverse-video. **
 *>*  Add a SPACE in from of command-line args when **
 *>* executing users program. Add a SPACE after the **
 *>* -ftraceall switch when building cobc command. **
 *>*  Convert to work on Cygwin/Linux as well as MinGW **
 *>*  Virtualized the key codes for S-F1 thru S-F7 as they **
 *>* differ depending upon whether PDCurses or NCurses is **
 *>* being used. **
 *>*  Introduced the cross-reference and source listing **
 *>* features. Also fixed a bug in @EXTRA switch proces- **
 *>* sing where garbage will result if more than the **
 *>* @EXTRA switch is specified. **
 *>****************************************************************
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
 FUNCTION ALL INTRINSIC.

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.

 SELECT Bat-File ASSIGN TO Bat-File-Name
 ORGANIZATION IS LINE SEQUENTIAL.

 SELECT Cobc-Output ASSIGN TO Cobc-Output-File
 ORGANIZATION IS LINE SEQUENTIAL.

 SELECT Source-Code ASSIGN TO File-Name
 ORGANIZATION IS LINE SEQUENTIAL
 FILE STATUS IS FSM-Status.

 DATA DIVISION.
 FILE SECTION.
 FD Bat-File.
 01 Bat-File-Rec PIC X(2048).

 FD Cobc-Output.
 01 Cobc-Output-Rec PIC X(256).

 FD Source-Code.
 01 Source-Code-Record PIC X(80).

 WORKING-STORAGE SECTION.
 COPY screenio.

*>           COPY    CPFILEDUMP REPLACING ==WFD== BY ==WFD==.
       01  WFD-FILEDUMP-AREA.
           03  WFD-ID         PIC  X(001) VALUE SPACE.
           03  WFD-SU         PIC  9(001) VALUE 1.
           03  WFD-TYPE       PIC  X(001) VALUE "M".
           03  WFD-SEQ        PIC  9(009) VALUE ZERO.
           03  WFD-ITEM       PIC  X(010) VALUE SPACE.
           03  WFD-PGM        PIC  X(008) VALUE SPACE.
           03  FILLER          PIC  X(092) VALUE SPACE..
       01  WFD-LEN            BINARY-LONG VALUE 100.

 01 COB-COLOR-BLACK  PIC 9(1) VALUE 0.
 01 COB-COLOR-WHITE  PIC 9(1) VALUE 7.
 01 COB-COLOR-BLUE   PIC 9(1) VALUE 1.
 01 COB-COLOR-CYAN   PIC 9(1) VALUE 3.
 01 COB-COLOR-RED    PIC 9(1) VALUE 4.

 01 COB-SCR-F1       PIC 9(4) VALUE 1001.
 01 COB-SCR-F2       PIC 9(4) VALUE 1002.
 01 COB-SCR-F3       PIC 9(4) VALUE 1003.
 01 COB-SCR-F4       PIC 9(4) VALUE 1004.
 01 COB-SCR-F5       PIC 9(4) VALUE 1005.
 01 COB-SCR-F6       PIC 9(4) VALUE 1006.
 01 COB-SCR-F7       PIC 9(4) VALUE 1007.
 01 COB-SCR-F8       PIC 9(4) VALUE 1008.
 01 COB-SCR-F9       PIC 9(4) VALUE 1009.
 01 COB-SCR-F10      PIC 9(4) VALUE 1010.
 01 COB-SCR-F11      PIC 9(4) VALUE 1011.
 01 COB-SCR-F12      PIC 9(4) VALUE 1012.
 01 COB-SCR-F13      PIC 9(4) VALUE 1013.
 01 COB-SCR-F14      PIC 9(4) VALUE 1014.
 01 COB-SCR-F15      PIC 9(4) VALUE 1015.
 01 COB-SCR-F16      PIC 9(4) VALUE 1016.
 01 COB-SCR-F17      PIC 9(4) VALUE 1017.
 01 COB-SCR-F18      PIC 9(4) VALUE 1018.
 01 COB-SCR-F19      PIC 9(4) VALUE 1019.
 01 COB-SCR-F20      PIC 9(4) VALUE 1020.
 01 COB-SCR-ESC      PIC 9(4) VALUE 2005.



 01 Bat-File-Name PIC X(256).

 01 Cmd PIC X(512).

 01 Cobc-Cmd PIC X(256).

 01 Cobc-Output-File PIC X(256).

 01 Command-Line-Args PIC X(256).

 01 Config-File PIC X(12).

 01 Config-Keys.
 05 CK-S-F1 PIC 9(4).
 05 CK-S-F2 PIC 9(4).
 05 CK-S-F3 PIC 9(4).
 05 CK-S-F4 PIC 9(4).
 05 CK-S-F5 PIC 9(4).
 05 CK-S-F6 PIC 9(4).
 05 CK-S-F7 PIC 9(4).

 01 Dir-Char PIC X(1).

 01 Dummy PIC X(1).

 01 Env-TEMP PIC X(256).

 01 File-Name.
 05 FN-Char OCCURS 256 TIMES PIC X(1).

 01 File-Status-Message.
 05 FILLER PIC X(13) VALUE 'Status Code: '. 
 05 FSM-Status PIC 9(2).
 05 FILLER PIC X(11) VALUE ', Meaning: '.
 05 FSM-Msg PIC X(25).

 01 Flags.

 05 F-Compilation-Succeeded PIC X(1).
 88 88-Compile-OK VALUE 'Y'.
 88 88-Compile-OK-Warn VALUE 'W'.
 88 88-Compile-Failed VALUE 'N'.

 05 F-Complete PIC X(1).
 88 88-Complete VALUE 'Y'.
 88 88-Not-Complete VALUE 'N'.

 05 F-IDENT-DIVISION PIC X(1).
 88 88-1st-Prog-Complete VALUE 'Y'.
 88 88-More-To-1st-Prog VALUE 'N'.

 05 F-LINKAGE-SECTION PIC X(1).
 88 88-Compile-As-Subpgm VALUE 'Y'.
 88 88-Compile-As-Mainpgm VALUE 'N'.

 05 F-No-Switch-Changes PIC X(1).
 88 88-No-Switch-Changes VALUE 'Y'.
 88 88-Switch-Changes VALUE 'N'.

 05 F-Output-File-Busy PIC X(1).
 88 88-Output-File-Busy VALUE 'Y'.
 88 88-Output-File-Avail VALUE 'N'.

 05 F-Source-Record-Type PIC X(1).
 88 88-Source-Rec-Linkage VALUE 'L'.
 88 88-Source-Rec-Ident VALUE 'I'.
 88 88-Source-Rec-IgnoCOB-COLOR-RED VALUE ' '.

 05 F-Switch-Error PIC X(1).
 88 88-Switch-Is-Bad VALUE 'Y'.
 88 88-Switch-Is-Good VALUE 'N'.

 01 Horizontal-Line PIC X(80).

 01 I USAGE BINARY-LONG.

 01 J USAGE BINARY-LONG.

 01 MS USAGE BINARY-LONG.

 01 ML USAGE BINARY-LONG.

 01 OC-Compiled PIC XXXX/XX/XXBXX/XX.

 01 OS-Type USAGE BINARY-LONG.
 88 OS-Unknown VALUE 0.
 88 OS-Windows VALUE 1.
 88 OS-Cygwin VALUE 2.
 88 OS-UNIX VALUE 3.

 01 OS-Type-Literal PIC X(7).

 01 Output-Message PIC X(80).

 01 Path-Delimiter PIC X(1).

 01 Prog-Folder PIC X(256).

 01 Prog-Extension PIC X(30).

 01 Prog-File-Name PIC X(40).

 01 Prog-Name PIC X(31).

 01 Report-Filename PIC X(256).

 78 Selection-Char VALUE '>'.

 01 Switch-Display.
 05 SD-Switch-And-Value PIC X(19).
 05 FILLER PIC X(1).
 05 SD-Description PIC X(60).

 01 Switch-Keyword PIC X(12).
 88 Switch-Is-CONFIG VALUE '@CONFIG', '@C'.
 88 Switch-Is-DEBUG VALUE '@DEBUG', '@D'.
 88 Switch-Is-DLL VALUE '@DLL'.
 88 Switch-Is-EXECUTE VALUE '@EXECUTE', '@E'.
 88 Switch-Is-EXTRA VALUE '@EXTRA', '@EX'.
 88 Switch-Is-NOTRUNC VALUE '@NOTRUNC', '@N'.
 88 Switch-Is-TRACE VALUE '@TRACE', '@T'.
 88 Switch-Is-SOURCE VALUE '@SOURCE', '@S'.
 88 Switch-Is-XREF VALUE '@XREF', '@X'.

 01 Switch-Keyword-And-Value PIC X(256).
 
 01 Switch-Value.
 05 SV-1 PIC X(1).
 05 FILLER PIC X(255).
 01 Switch-Value-Alt REDEFINES Switch-Value
 PIC X(256).
 88 Valid-Config-Filename
 VALUE 'BS2000', 'COBOL85', 'COBOL2002', 'DEFAULT',
 'IBM', 'MF', 'MVS'.

 01 Switches.
 05 S-ARGS PIC X(75) VALUE SPACES.
 05 S-CfgS.
 10 S-Cfg-BS2000 PIC X(1) VALUE ' '.
 10 S-Cfg-COBOL85 PIC X(1) VALUE ' '.
 10 S-Cfg-COBOL2002 PIC X(1) VALUE ' '.
 10 S-Cfg-DEFAULT PIC X(1) VALUE Selection-Char.
 10 S-Cfg-IBM PIC X(1) VALUE ' '.
 10 S-Cfg-MF PIC X(1) VALUE ' '.
 10 S-Cfg-MVS PIC X(1) VALUE ' '.
 05 S-EXTRA PIC X(75) VALUE SPACES.
 05 S-Yes-No-Switches.
 10 S-DEBUG PIC X(1) VALUE 'N'.
 10 S-DLL PIC X(1) VALUE 'N'.
 10 S-XREF PIC X(1) VALUE 'N'.
 10 S-SOURCE PIC X(1) VALUE 'N'.
 10 S-EXECUTE PIC X(1) VALUE 'N'.
 10 S-NOTRUNC PIC X(1) VALUE 'Y'.
 10 S-SUBROUTINE PIC X(1) VALUE 'A'.
 10 S-TRACE PIC X(1) VALUE 'N'.
 10 S-TRACEALL PIC X(1) VALUE 'N'.

 01 Tally USAGE BINARY-LONG.

 SCREEN SECTION.
 *>
 *> Here is the layout of the OCic screen.
 *>
 *> Note that this program can utilize the traditional PC line-drawing characters,
 *> if they are available.
 *>
 *> If this program is run on Windows, it must run with codepage 437 activated to
 *> display the line-drawing characters. With a native Windows build or a
 *> Windows/MinGW build, one could use the command "chcp 437" to set that codepage
 *> for display within a Windows console window (that should be the default, though).
 *> With a Windows/Cygwin build, set the environment variable CYGWIN to a value of
 *> "codepage:oem" (this cannot be done from within the program though - you will
 *> have to use the "Computer/Advanced System Settings/Environment Variables" (Vista or
 *> Windows 7) function to define the variable. XP Users: use "My Computer/Properties/
 *> Advanced/Environment Variables".
 *>
 *> To use OCic without the line-drawing characters, comment-out the first set of
 *> 78 "LD" items and uncomment the second.
 *>
 *> The following sample screen layout shows how the screen looks with line-drawing
 *> characters disabled.
 *>
 *>===================================================================================
 *> OCic (2010/04/02 11:36) - OpenCOBOL V1.1 Interactive Compilation Windows 01
 *> +-----------------------------------------------------------------------------+ 02
 *> | Program: OCic F-Key: Select Opt | 03
 *> | Folder: E:\OpenCOBOL\Samples Enter: Compile | 04
 *> | Filename: OCic.cbl Esc: Quit | 05
 *> +-----------------------------------------------------------------------------+ 06
 *> On/Off Switches: Configuration: 07
 *> +---------------------------------------------------------+-------------------+ 08
 *> | F1 Compile debug lines F8 Produce source listing | S-F1 BS2000 | 09
 *> | F2 Always make DLLs F9 Produce xref listing | S-F2 COBOL85 | 10
 *> | F3 Pgm is a SUBROUTINE | S-F3 COBOL2002 | 11
 *> | F4 Execute if compile OK | S-F4 > Default | 12
 *> | F5 > No COMP/BINARY trunc | S-F5 IBM | 13
 *> | F6 Trace procedures | S-F6 MicroFocus | 14
 *> | F7 Trace proc + stmnts | S-F7 MVS | 15
 *> +---------------------------------------------------------+-------------------+ 16
 *> Additional "cobc" Switches (if any): 17
 *> +-----------------------------------------------------------------------------+ 18
 *> | -O2________________________________________________________________________ | 19
 *> +-----------------------------------------------------------------------------+ 20
 *> Program Execution Arguments (if any): 21
 *> +-----------------------------------------------------------------------------+ 22
 *> | ___________________________________________________________________________ | 23
 *> +-----------------------------------------------------------------------------+ 24
 *> OCic Copyright (C) 2009-2010, Gary L. Cutler, GPL 25
 *>===================================================================================
 *>12345678901234567890123456789012345678901234567890123456789012345678901234567890 
 *> 1 2 3 4 5 6 7 8
 *>
 *> USE THESE CHARS FOR LINE-DRAWING IF YOU HAVE ACCESS TO PC-DOS CODEPAGE 437:
 *>
 *>78 LD-UL-Corner VALUE X"DA".
 *>78 LD-LL-Corner VALUE X"C0".
 *>78 LD-UR-Corner VALUE X"BF".
 *>78 LD-LR-Corner VALUE X"D9".
 *>78 LD-Upper-T VALUE X"C2".
 *>78 LD-Lower-T VALUE X"C1".
 *>78 LD-Horiz-Line VALUE X"C4".
 *>78 LD-Vert-Line VALUE X"B3".
 *>
 *> USE THESE CHARS FOR LINE-DRAWING IF YOU DO NOT HAVE ACCESS TO PC-DOS CODEPAGE 437:
 *>
 78 LD-UL-Corner VALUE '+'.
 78 LD-LL-Corner VALUE '+'.
 78 LD-UR-Corner VALUE '+'.
 78 LD-LR-Corner VALUE '+'.
 78 LD-Upper-T VALUE '+'.
 78 LD-Lower-T VALUE '+'.
 78 LD-Horiz-Line VALUE '-'.
 78 LD-Vert-Line VALUE '|'.
 *>
 01 Blank-Screen LINE 1 COLUMN 1 BLANK SCREEN.

 01 Switches-Screen BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-WHITE AUTO.
 *>
 *> GENERAL SCREEN FRAMEWORK
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-BLUE HIGHLIGHT.
 05 LINE 02 COL 02 VALUE LD-UL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-UR-Corner.

 05 LINE 03 COL 02 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.


 05 LINE 04 COL 02 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 05 COL 02 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 06 COL 02 VALUE LD-LL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-LR-Corner.

 05 LINE 08 COL 02 VALUE LD-UL-Corner.
 05 PIC X(57) FROM Horizontal-Line.
 05 COL 60 VALUE LD-Upper-T.
 05 PIC X(19) FROM Horizontal-Line.
 05 COL 80 VALUE LD-UR-Corner.

 05 LINE 09 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 10 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 11 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 12 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 13 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 14 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 15 COL 02 VALUE LD-Vert-Line.
 05 COL 60 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.
 
 05 LINE 16 COL 02 VALUE LD-LL-Corner.
 05 PIC X(57) FROM Horizontal-Line.
 05 COL 60 VALUE LD-Lower-T.
 05 PIC X(19) FROM Horizontal-Line.
 05 COL 80 VALUE LD-LR-Corner.

 05 LINE 18 COL 02 VALUE LD-UL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-UR-Corner.

 05 LINE 19 COL 02 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 20 COL 02 VALUE LD-LL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-LR-Corner.

 05 LINE 22 COL 02 VALUE LD-UL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-UR-Corner.

 05 LINE 23 COL 02 VALUE LD-Vert-Line.
 05 COL 80 VALUE LD-Vert-Line.

 05 LINE 24 COL 02 VALUE LD-LL-Corner.
 05 PIC X(77) FROM Horizontal-Line.
 05 COL 80 VALUE LD-LR-Corner.

 *>
 *> TOP AND BOTTOM LINES
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLUE BLINK
 FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
 05 LINE 01 COL 01 VALUE ' OCic ('.
 05 PIC X(16) FROM OC-Compiled.
 05 VALUE ') OpenCOBOL 1.1 06FEB2009 ' &
 'Interactive Compilation '.
 05 LINE 25 COL 01 PIC X(81) FROM Output-Message.
 *>
 *> LABELS
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-CYAN HIGHLIGHT.
 05 LINE 07 COL 04 VALUE 'On/Off Switches:'.
 05 COL 62 VALUE 'Configuration:'.
 05 LINE 17 COL 04 VALUE 
 'Additional "cobc" Switches (if any):'.
 05 LINE 21 COL 04 VALUE 'Program Execution Arguments (if any):'.
 *>
 *> TOP SECTION BACKGROUND
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-CYAN LOWLIGHT.
 05 LINE 03 COL 04 VALUE 'Program: '.
 05 LINE 04 COL 04 VALUE 'Folder: '.
 05 LINE 05 COL 04 VALUE 'Filename: '.

 05 LINE 03 COL 62 VALUE 'F-Key: Select Opt'.
 05 LINE 04 COL 62 VALUE 'Enter: Compile '.
 05 LINE 05 COL 62 VALUE 'Esc: Quit '.
 *>
 *> TOP SECTION PROGRAM INFO
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
 05 LINE 03 COL 14 PIC X(47) FROM Prog-Name.
 05 LINE 04 COL 14 PIC X(47) FROM Prog-Folder.
 05 LINE 05 COL 14 PIC X(47) FROM Prog-File-Name.
 *>
 *> MIDDLE LEFT SECTION F-KEYS
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
 05 LINE 09 COL 04 VALUE 'F1'.
 05 LINE 10 COL 04 VALUE 'F2'.
 05 LINE 11 COL 04 VALUE 'F3'.
 05 LINE 12 COL 04 VALUE 'F4'.
 05 LINE 13 COL 04 VALUE 'F5'.
 05 LINE 14 COL 04 VALUE 'F6'.
 05 LINE 15 COL 04 VALUE 'F7'.
 05 LINE 09 COL 32 VALUE 'F8'.
 05 LINE 10 COL 32 VALUE 'F9'.
 *>
 *> MIDDLE LEFT SECTION SWITCHES 
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-RED HIGHLIGHT.
 05 LINE 09 COL 07 PIC X(1) FROM S-DEBUG.
 05 LINE 10 COL 07 PIC X(1) FROM S-DLL.
 05 LINE 11 COL 07 PIC X(1) FROM S-SUBROUTINE.
 05 LINE 12 COL 07 PIC X(1) FROM S-EXECUTE.
 05 LINE 13 COL 07 PIC X(1) FROM S-NOTRUNC.
 05 LINE 14 COL 07 PIC X(1) FROM S-TRACE.
 05 LINE 15 COL 07 PIC X(1) FROM S-TRACEALL.
*> 05 LINE 09 COL 35 PIC X(1) FROM S-SOURCE.
*> 05 LINE 10 COL 35 PIC X(1) FROM S-XREF.
 05 LINE 09 COL 35 PIC X(1) USING S-SOURCE.
 05 LINE 10 COL 35 PIC X(1) USING S-XREF.
 *>
 *> MIDDLE LEFT SECTION BACKGROUND
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-CYAN LOWLIGHT.
 05 LINE 09 COL 09 VALUE 'Compile debug lines '.
 05 LINE 10 COL 09 VALUE 'Always make DLLs '.
 05 LINE 11 COL 09 VALUE 'Pgm is a SUBROUTINE '.
 05 LINE 12 COL 09 VALUE 'Execute if compile OK '.
 05 LINE 13 COL 09 VALUE 'No COMP/BINARY trunc '.
 05 LINE 14 COL 09 VALUE 'Trace procedures '.
 05 LINE 15 COL 09 VALUE 'Trace proc + stmnts '.
 05 LINE 09 COL 37 VALUE 'Produce source listing'.
 05 LINE 10 COL 37 VALUE 'Produce xref listing '.
 *>
 *> MIDDLE RIGHT SECTION F-KEYS
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
 05 LINE 09 COL 62 VALUE 'S-F1'.
 05 LINE 10 COL 62 VALUE 'S-F2'.
 05 LINE 11 COL 62 VALUE 'S-F3'.
 05 LINE 12 COL 62 VALUE 'S-F4'.
 05 LINE 13 COL 62 VALUE 'S-F5'.
 05 LINE 14 COL 62 VALUE 'S-F6'.
 05 LINE 15 COL 62 VALUE 'S-F7'.
 *>
 *> MIDDLE RIGHT SECTION SWITCHES
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-RED HIGHLIGHT.
 05 LINE 09 COL 67 PIC X(1) FROM S-Cfg-BS2000.
 05 LINE 10 COL 67 PIC X(1) FROM S-Cfg-COBOL85.
 05 LINE 11 COL 67 PIC X(1) FROM S-Cfg-COBOL2002.
 05 LINE 12 COL 67 PIC X(1) FROM S-Cfg-DEFAULT.
 05 LINE 13 COL 67 PIC X(1) FROM S-Cfg-IBM.
 05 LINE 14 COL 67 PIC X(1) FROM S-Cfg-MF.
 05 LINE 15 COL 67 PIC X(1) FROM S-Cfg-MVS.
 *>
 *> MIDDLE RIGHT SECTION BACKGROUND
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-CYAN LOWLIGHT.
 05 LINE 09 COL 69 VALUE 'BS2000 '.
 05 LINE 10 COL 69 VALUE 'COBOL85 '.
 05 LINE 11 COL 69 VALUE 'COBOL2002 '.
 05 LINE 12 COL 69 VALUE 'Default '.
 05 LINE 13 COL 69 VALUE 'IBM '.
 05 LINE 14 COL 69 VALUE 'MicroFocus'.
 05 LINE 15 COL 69 VALUE 'MVS '.
 *>
 *> FREE-FORM OPTIONS FIELDS
 *>
 03 BACKGROUND-COLOR COB-COLOR-BLACK
 FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
 05 LINE 19 COL 04 PIC X(75) USING S-EXTRA.
 05 LINE 23 COL 04 PIC X(75) USING S-ARGS.
 *>/

 PROCEDURE DIVISION.
 *>****************************************************************
 *>* Legend to procedure names: **
 *>* **
 *>* 00x-xxx All MAIN driver procedures **
 *>* 0xx-xxx All GLOBAL UTILITY procedures **
 *>* 1xx-xxx All INITIALIZATION procedures **
 *>* 2xx-xxx All CORE PROCESSING procedures **
 *>* 9xx-xxx All TERMINATION procedures **
 *>****************************************************************

 DECLARATIVES.
 000-File-Error SECTION.
 USE AFTER STANDARD ERROR PROCEDURE ON Source-Code.

 000-Handle-Error. 
 COPY FileStat-Msgs
   REPLACING STATUS BY FSM-Status
   MSG BY FSM-Msg.

 MOVE SPACES TO Output-Message
 IF FSM-Status = 35
   DISPLAY
     'File not found: "'
     TRIM(File-Name,TRAILING)
     '"'
   END-DISPLAY
 ELSE
   DISPLAY
     'Error accessing file: "'
     TRIM(File-Name,TRAILING)
     '"'
   END-DISPLAY
 END-IF
 GOBACK
 .
 END DECLARATIVES.

 *>/
 000-Main SECTION.

*> ACCEPT Dummy
*> FROM CONSOLE
*> END-ACCEPT

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       Cobc-Output-Rec

 PERFORM 100-Initialization
 SET 88-Not-Complete TO TRUE

 
 PERFORM UNTIL 88-Complete
 
   PERFORM 200-Let-User-Set-Switches
 
   PERFORM 210-Run-Compiler

*> ｺﾝﾊﾟｲﾙ
   IF (88-Compile-OK OR 88-Compile-OK-Warn)
   AND (S-XREF NOT = SPACE OR S-SOURCE NOT = SPACE)
 
*> listing XREF
     PERFORM 220-Make-Listing
   END-IF

 
   IF (S-EXECUTE NOT = SPACES)
   AND (88-Output-File-Avail)
     PERFORM 230-Run-Program
   END-IF
 
 END-PERFORM

.
 009-Done.
 PERFORM 900-Terminate
 .
 *> -- Control will NOT return
 *>/
 010-Parse-Args SECTION.
 *>****************************************************************
 *>* Process a sequence of KEYWORD=VALUE items. These are items **
 *>* specified on the command-line to provide the initial **
 *>* options shown selected on the screen. When integrating **
 *>* OCic into an edirot or framework, include these switches on **
 *>* the ocic.exe command the editor/framework executes. Any **
 *>* underlined choice is the default value for that switch. **
 *>* **
 *>* @CONFIG=BS2000|COBOL85|COBOL2002|DEFAULT|IBM|MF|MVS **
 *>* ======= **
 *>* This switch specifies the default cobc compiler configura- **
 *>* tion file to be used **
 *>* **
 *>* @DEBUG=YES|NO **
 *>* == **
 *>* This switch specifies whether (YES) or not (NO) debugging **
 *>* lines (those with a "D" in column 7) will be compiled. **
 *>* **
 *>* @DLL=YES|NO **
 *>* == **
 *>* Use this switch to force ALL compiled programs to be built **
 *>* as DLLs ("@DLL=YES"). When main programs are built as DLLs **
 *>* they must be executed using the cobcrun utility. When **
 *>* "@DLL=NO" is in effect, main programs are generated as **
 *>>* actual "exe" files and only subprograms will be generated **
 *>* as DLLs. **
 *>* **
 *>* @EXECUTE=YES|NO **
 *>* == **
 *>* This switch specifies whether ("@EXECUTE=YES") or not **
 *>* ("@EXECUTE=NO") the program will be executed after it is **
 *>* successfully compiled. **
 *>* **
 *>* @EXTRA=extra cobc argument(s) **
 *>* **
 *>* This switch allows you to specify additional cobc arguments **
 *>* that aren t managed by the other OC switches. If used, **
 *>* this must be the last switch specified on the command line, **
 *>* as everything that follows the "=" will be placed on the ** 
 *>* cobc command generated by OC. **
 *>* **
 *>* @NOTRUNC=YES|NO **
 *>* === **
 *>* This switch specifies whether (YES) or not (NO) the sup- **
 *>>* pression of binary field truncation will occur. If a PIC **
 *>* 99 COMP field (one byte of storage), for example, is given **
 *>* the value 123, it may have its value truncated to 23 when **
 *>* DISPLAYed. Regardless of the NOTRUNC setting, internally **
 *>* the full precision of the field (allowing a maximum value **
 *>* of 255) will be preserved. Even though truncation - if it **
 *>* does occur - would appear to have a minimal disruption on **
 *>* program operation, it has a significant effect on program **
 *>* run-time speed. **
 *>* **
 *>* @TRACE=YES|NO|ALL **
 *>* == **
 *>* This switch controls whether or not code will be added to **
 *>* the object program to produce execution-time logic traces. **
 *>* A specification of "@TRACE=NO" means no such code will be **
 *>* produced. By specifying "@TRACE=YES", code will be genera- **
 *>* ted to display procedure names as they are entered. A **
 *>* "@TRACE=ALL" specification will generate not only procedure **
 *>* traces (as "@TRACE=YES" would) but also statement-level **
 *>* traces too! All trace output is written to STDERR, so **
 *>* adding a "2>file" to the execution of the program will pipe **
 *>* the trace output to a file. You may find it valuable to **
 *>* add your own DISPLAY statements to the debugging output via **
 *>* "DISPLAY xx UPON SYSERR" The SYSERR device corresponds to **
 *>* the Windows or UNIX STDERR device and will therefore honor **
 *>* any "2>file" placed at the end of your program s execution. **
 *>* Add a "D" in column 7 and you can control the generation or **
 *>* ignoring of these DISPLAY statements via the "@DEBUG" **
 *>* switch. **
 *>* **
 *>* @SOURCE=YES|NO **
 *>* == **
 *>* Use this switch to produce a source listing of the program, **
 *>* PROVIDED it compiles without errors. **
 *>* **
 *>* @XREF=YES|NO ** 
 *>* == **
 *>* Use this switch to produce a cross-reference listing of the **
 *>* program, PROVIDED it compiles without errors. **
 *>****************************************************************

 011-Init.

 MOVE 1 TO I
 .

 012-Extract-Kwd-And-Value.

 PERFORM UNTIL I NOT < LENGTH(Command-Line-Args)
   MOVE I TO J
 
   UNSTRING Command-Line-Args
     DELIMITED BY ALL SPACES
     INTO Switch-Keyword-And-Value
     WITH POINTER I
   END-UNSTRING

   IF Switch-Keyword-And-Value NOT = SPACES
     UNSTRING Switch-Keyword-And-Value
     DELIMITED BY '='
     INTO Switch-Keyword, Switch-Value
     END-UNSTRING

     PERFORM 030-Process-Keyword
   END-IF
 END-PERFORM
 .

 019-Done.
 EXIT.

 *>****************************************************************
 *>* Since this program uses the SCREEN SECTION, it cannot do **
 *>* conventional console DISPLAY operations. This routine **
 *>* (which, I admit, is like using an H-bomb to hunt rabbits) **
 *>* will submit an "ECHO" command to the system to simulate a **
 *>* DISPLAY. **
 *>****************************************************************

 021-Build-And-Issue-Command.

 DISPLAY
   Output-Message
 END-DISPLAY
 .
 
 029-Done.
 EXIT.
 *>/

 030-Process-Keyword SECTION.
 *>****************************************************************
 *>* Process a single KEYWORD=VALUE item. **
 *>****************************************************************

 031-Init.
 MOVE UPPER-CASE(Switch-Keyword) TO Switch-Keyword
 SET 88-Switch-Is-Good TO TRUE
 .

 032-Process.
 EVALUATE TRUE
   WHEN Switch-Is-EXTRA
     MOVE J TO I
     UNSTRING Command-Line-Args DELIMITED BY '='
       INTO Dummy, S-EXTRA
       WITH POINTER I
     END-UNSTRING

     MOVE LENGTH(Command-Line-Args) TO I

   WHEN Switch-Is-CONFIG
     MOVE 'CONFIG' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     EVALUATE Switch-Value
       WHEN 'BS2000'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-BS2000

       WHEN 'COBOL85'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-COBOL85

       WHEN 'COBOL2002'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-COBOL2002

       WHEN 'DEFAULT'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-DEFAULT

       WHEN 'IBM'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-IBM

       WHEN 'MF'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-MF

       WHEN 'MVS'
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-MVS

       WHEN OTHER
         MOVE 'An invalid /CONFIG switch value ' &
           'was specified on the command line ' &
           '- ignored' TO Output-Message
     END-EVALUATE

   WHEN Switch-Is-DEBUG
     MOVE 'DEBUG' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 040-Process-Yes-No-Value

     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-DEBUG
     END-IF

   WHEN Switch-Is-DLL
     MOVE 'DLL' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 040-Process-Yes-No-Value
     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-DLL
     END-IF

   WHEN Switch-Is-EXECUTE
     MOVE 'EXECUTE' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 040-Process-Yes-No-Value
     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-EXECUTE
     END-IF

   WHEN Switch-Is-NOTRUNC
     MOVE 'NOTRUNC' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 040-Process-Yes-No-Value

     IF 88-Switch-Is-Good 
       MOVE SV-1 TO S-NOTRUNC
     END-IF

   WHEN Switch-Is-SOURCE
     MOVE 'SOURCE' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 050-Process-Yes-No-All
     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-SOURCE
     END-IF

   WHEN Switch-Is-TRACE
     MOVE 'TRACE' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 050-Process-Yes-No-All

     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-TRACE
     END-IF

   WHEN Switch-Is-XREF
     MOVE 'XREF' TO Switch-Keyword
     MOVE UPPER-CASE(Switch-Value) TO Switch-Value

     PERFORM 050-Process-Yes-No-All

     IF 88-Switch-Is-Good
       MOVE SV-1 TO S-XREF
     END-IF

   WHEN OTHER
     MOVE SPACES TO Output-Message
     STRING '"'
       TRIM(Switch-Keyword)
       '" is not a valid switch ' &
       '- ignored'
       DELIMITED SIZE
       INTO Output-Message
     END-STRING
     SET 88-Switch-Is-Bad TO TRUE
 END-EVALUATE
 .

 039-Done.
 EXIT.
 *>/

 040-Process-Yes-No-Value SECTION.
 *>****************************************************************
 *>* Process a switch value of YES or NO **
 *>****************************************************************

 042-Process.
 EVALUATE SV-1
   WHEN 'Y'
     MOVE 'YES' TO Switch-Value

   WHEN 'N'
     MOVE 'NO' To Switch-Value

   WHEN OTHER
     MOVE SPACES TO Output-Message
     STRING '*ERROR: "' TRIM(Switch-Value)
       '" is not a valid value for the "'
       TRIM(Switch-Keyword) '" switch'
       DELIMITED SPACES
       INTO Output-Message
     END-STRING
     SET 88-Switch-Is-Bad TO TRUE
 END-EVALUATE
 .

 049-Done.
 EXIT.
 *>/

 050-Process-Yes-No-All SECTION.
 *>****************************************************************
 *>* Process a switch value of YES, NO or ALL **
 *>****************************************************************

 052-Process.
 IF SV-1 = 'A'
   MOVE 'ALL' TO Switch-Value
 ELSE
   PERFORM 040-Process-Yes-No-Value
 END-IF
 .

 059-Done.
 EXIT.
 *>/ 

 060-Process-Yes-No-Auto SECTION.
 *>****************************************************************
 *>* Process a switch value of YES, NO or AUTO **
 *>****************************************************************

 061-Init.
 IF SV-1 = 'A'
   PERFORM 070-Find-LINKAGE-SECTION
   IF 88-Compile-As-Subpgm
     MOVE 'Y' TO Switch-Value
   ELSE
     MOVE 'N' TO Switch-Value
   END-IF
 ELSE
   PERFORM 040-Process-Yes-No-Value
 END-IF
 .
 *>/

 070-Find-LINKAGE-SECTION SECTION.
 *>****************************************************************
 *>* Determine if the program being compiled is a MAIN program **
 *>****************************************************************

 071-Init.
 OPEN INPUT Source-Code
 SET 88-Compile-As-Mainpgm TO TRUE
 SET 88-More-To-1st-Prog TO TRUE

 PERFORM UNTIL 88-1st-Prog-Complete
   READ Source-Code AT END
     CLOSE Source-Code
     EXIT SECTION
   END-READ
   CALL 'CHECKSOURCE' USING Source-Code-Record
     F-Source-Record-Type
   END-CALL
   IF 88-Source-Rec-Ident
     SET 88-1st-Prog-Complete TO TRUE
   END-IF
 END-PERFORM
 .

 072-Process-Source.
 SET 88-Source-Rec-IgnoCOB-COLOR-RED TO TRUE

 PERFORM UNTIL 88-Source-Rec-Linkage
   OR 88-Source-Rec-Ident
   READ Source-Code AT END
     CLOSE Source-Code
     EXIT SECTION
   END-READ

   CALL 'CHECKSOURCE' USING Source-Code-Record
     F-Source-Record-Type
   END-CALL
 END-PERFORM

 CLOSE Source-Code
 IF 88-Source-Rec-Linkage
   SET 88-Compile-As-Subpgm TO TRUE
 END-IF
 .

 079-Done.
 EXIT.
 *>/

 100-Initialization SECTION.
 *>****************************************************************
 *>* Perform all program-wide initialization operations **
 *>****************************************************************

 101-Determine-OS-Type.
 CALL 'GETOSTYPE'
 END-CALL
 MOVE RETURN-CODE TO OS-Type
 
 *>DISPLAY "RETURN-CODE" RETURN-CODE
 *>RETURN-CODE=1=> WINDOWS

*> ACCEPT Dummy
*> FROM CONSOLE
*> END-ACCEPT

 EVALUATE TRUE
   WHEN OS-Unknown
     MOVE '\' TO Dir-Char
     MOVE 'Unknown' TO OS-Type-Literal
     MOVE COB-SCR-F11 TO CK-S-F1
     MOVE COB-SCR-F12 TO CK-S-F2
     MOVE COB-SCR-F13 TO CK-S-F3
     MOVE COB-SCR-F14 TO CK-S-F4
     MOVE COB-SCR-F15 TO CK-S-F5
     MOVE COB-SCR-F16 TO CK-S-F6
     MOVE COB-SCR-F17 TO CK-S-F7

   WHEN OS-Windows 
     MOVE '\' TO Dir-Char
     MOVE 'Windows' TO OS-Type-Literal
     MOVE COB-SCR-F13 TO CK-S-F1
     MOVE COB-SCR-F14 TO CK-S-F2
     MOVE COB-SCR-F15 TO CK-S-F3
     MOVE COB-SCR-F16 TO CK-S-F4
     MOVE COB-SCR-F17 TO CK-S-F5
     MOVE COB-SCR-F18 TO CK-S-F6
     MOVE COB-SCR-F19 TO CK-S-F7

 WHEN OS-Cygwin
 MOVE '/' TO Dir-Char
 MOVE 'Cygwin' TO OS-Type-Literal
 MOVE COB-SCR-F11 TO CK-S-F1
 MOVE COB-SCR-F12 TO CK-S-F2
 MOVE COB-SCR-F13 TO CK-S-F3
 MOVE COB-SCR-F14 TO CK-S-F4
 MOVE COB-SCR-F15 TO CK-S-F5
 MOVE COB-SCR-F16 TO CK-S-F6
 MOVE COB-SCR-F17 TO CK-S-F7

 WHEN OS-UNIX
 MOVE '/' TO Dir-Char
 MOVE 'UNIX ' TO OS-Type-Literal
 MOVE COB-SCR-F11 TO CK-S-F1
 MOVE COB-SCR-F12 TO CK-S-F2
 MOVE COB-SCR-F13 TO CK-S-F3
 MOVE COB-SCR-F14 TO CK-S-F4
 MOVE COB-SCR-F15 TO CK-S-F5
 MOVE COB-SCR-F16 TO CK-S-F6
 MOVE COB-SCR-F17 TO CK-S-F7
 END-EVALUATE
 .

 102-Set-Environment-Vars.
 SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
 SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
 .

 103-Generate-Cobc-Output-Fn.
 ACCEPT Env-TEMP
   FROM ENVIRONMENT "TEMP"
 END-ACCEPT
 MOVE SPACES TO Cobc-Output-File
 STRING TRIM(Env-TEMP,TRAILING)
   Dir-Char
   'OC-Messages.TXT'
   DELIMITED SIZE
   INTO Cobc-Output-File
 END-STRING
 .

 104-Generate-Banner-Line-Info.
 MOVE WHEN-COMPILED (1:12) TO OC-Compiled
 INSPECT OC-Compiled
   REPLACING ALL '/' BY ':'
   AFTER INITIAL SPACE
  .

 105-Establish-Switch-Settings.
 ACCEPT Command-Line-Args
   FROM COMMAND-LINE
 END-ACCEPT
 MOVE TRIM(Command-Line-Args, Leading) TO Command-Line-Args
 MOVE 0 TO Tally
 INSPECT Command-Line-Args TALLYING Tally FOR ALL '@'

 IF Tally = 0
   MOVE Command-Line-Args TO File-Name
   MOVE SPACES TO Command-Line-Args
 ELSE

   UNSTRING Command-Line-Args DELIMITED BY '@'
     INTO File-Name, Dummy
   END-UNSTRING

   INSPECT Command-Line-Args
     REPLACING FIRST '@' BY LOW-VALUES

   UNSTRING Command-Line-Args
     DELIMITED BY LOW-VALUES
     INTO Dummy, Cmd
   END-UNSTRING

   MOVE SPACES TO Command-Line-Args
   STRING '@' Cmd DELIMITED SIZE
     INTO Command-Line-Args
   END-STRING
 END-IF

 IF File-Name = SPACES 
   DISPLAY
     'No program filename was specified'
   END-DISPLAY
   MOVE 'No program filename was specified'
     TO Output-Message 
   PERFORM 900-Terminate
 END-IF

 PERFORM 010-Parse-Args

 IF S-SUBROUTINE = 'A'
   MOVE 'S' TO Switch-Keyword
   MOVE 'A' TO Switch-Value
   PERFORM 070-Find-LINKAGE-SECTION
   IF 88-Compile-As-Subpgm
     MOVE 'Y' TO S-SUBROUTINE
   ELSE
     MOVE 'N' TO S-SUBROUTINE
   END-IF
 END-IF

 INSPECT S-Yes-No-Switches REPLACING ALL 'Y' BY Selection-Char
 INSPECT S-Yes-No-Switches REPLACING ALL 'N' BY ' '
 .

 106-Determine-Folder-Path.
 Move 256 TO I
 IF OS-Cygwin AND File-Name (2:1) = ':'
   MOVE '\' TO Dir-Char
 END-IF

 PERFORM UNTIL I = 0 OR FN-Char (I) = Dir-Char
   SUBTRACT 1 FROM I
 END-PERFORM

 IF I = 0
   MOVE SPACES TO Prog-Folder
   MOVE File-Name TO Prog-File-Name
 ELSE
   MOVE '*' TO FN-Char (I)
   UNSTRING File-Name DELIMITED BY '*'
     INTO Prog-Folder
     Prog-File-Name
   END-UNSTRING
   MOVE Dir-Char TO FN-Char (I)
 END-IF

 UNSTRING Prog-File-Name DELIMITED BY '.'
   INTO Prog-Name, Prog-Extension
 END-UNSTRING

 IF Prog-Folder = SPACES
   ACCEPT Prog-Folder
     FROM ENVIRONMENT 'CD'
   END-ACCEPT
 ELSE
   CALL "CBL_CHANGE_DIR"
     USING TRIM(Prog-Folder,TRAILING)
   END-CALL
 END-IF

 IF OS-Cygwin AND File-Name (2:1) = ':'
   MOVE '/' TO Dir-Char
 END-IF
 .

 107-Other.
 MOVE ALL LD-Horiz-Line TO Horizontal-Line.
 MOVE CONCATENATE(' OCic for ',
   TRIM(OS-Type-Literal,Trailing),
   ' Copyright (C) 2009-2010, Gary L. Cutler,',
   ' GPL') TO Output-Message.
 .

 109-Done.
 EXIT.
 *>/

 200-Let-User-Set-Switches SECTION.
 *>****************************************************************
 *>* Show the user the current switch settings and allow them to **
 *>* be changed. **
 *>****************************************************************

 201-Init.
 SET 88-Switch-Changes TO TRUE
 .

 202-Show-And-Change-Switches.
 PERFORM UNTIL 88-No-Switch-Changes
   ACCEPT
     Switches-Screen
   END-ACCEPT

*> ENTER (0000) 以外のキー押された時
   IF COB-CRT-STATUS > 0 
     EVALUATE COB-CRT-STATUS
       WHEN COB-SCR-F1
         IF S-DEBUG = SPACE
           MOVE Selection-Char TO S-DEBUG
         ELSE
           MOVE ' ' TO S-DEBUG
         END-IF

       WHEN COB-SCR-F2
         IF S-DLL = SPACE
           MOVE Selection-Char TO S-DLL
         ELSE
           MOVE ' ' TO S-DLL
         END-IF

       WHEN COB-SCR-F3
         IF S-SUBROUTINE = SPACE
           MOVE Selection-Char TO S-SUBROUTINE
           MOVE ' ' TO S-EXECUTE
         ELSE
           MOVE ' ' TO S-SUBROUTINE
         END-IF

       WHEN COB-SCR-F4
         IF S-EXECUTE = SPACE
           AND S-SUBROUTINE = SPACE
           MOVE Selection-Char TO S-EXECUTE
         ELSE
           MOVE ' ' TO S-EXECUTE
         END-IF

       WHEN COB-SCR-F5
         IF S-NOTRUNC = SPACE
           MOVE Selection-Char TO S-NOTRUNC
         ELSE
           MOVE ' ' TO S-NOTRUNC
         END-IF

       WHEN COB-SCR-F6
         IF S-TRACE = SPACE
           MOVE Selection-Char TO S-TRACE
           MOVE ' ' TO S-TRACEALL
         ELSE
           MOVE ' ' TO S-TRACE
         END-IF

       WHEN COB-SCR-F7
         IF S-TRACEALL = SPACE
           MOVE Selection-Char TO S-TRACEALL
           MOVE ' ' TO S-TRACE
         ELSE
           MOVE ' ' TO S-TRACEALL
         END-IF

       WHEN COB-SCR-F8
         IF S-SOURCE = SPACE
           MOVE Selection-Char TO S-SOURCE
         ELSE
           MOVE ' ' TO S-SOURCE
         END-IF

       WHEN COB-SCR-F9
         IF S-XREF = SPACE
           MOVE Selection-Char TO S-XREF
         ELSE
           MOVE ' ' TO S-XREF
         END-IF

       WHEN COB-SCR-ESC
         PERFORM 900-Terminate

       WHEN CK-S-F1
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-BS2000

       WHEN CK-S-F2
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-COBOL85

       WHEN CK-S-F3
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-COBOL2002

       WHEN CK-S-F4
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-DEFAULT

       WHEN CK-S-F5
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-IBM

       WHEN CK-S-F6
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-MF

       WHEN CK-S-F7
         MOVE SPACES TO S-CfgS
         MOVE Selection-Char TO S-Cfg-MVS

       WHEN OTHER
         MOVE 'An unsupported key was pressed' TO Output-Message
     END-EVALUATE
   ELSE
     SET 88-No-Switch-Changes TO TRUE
   END-IF
 END-PERFORM
 .

 209-Done.
 EXIT.
 *>/

 210-Run-Compiler SECTION.
 *>****************************************************************
 *>* Run the compiler using the switch settings we ve prepared. **
 *>****************************************************************

 211-Init.
 MOVE SPACES TO Cmd
   Cobc-Cmd
   Output-Message
 DISPLAY
   Switches-Screen
 END-DISPLAY
 MOVE 1 TO I

 EVALUATE TRUE
   WHEN S-Cfg-BS2000 NOT = SPACES
     MOVE 'bs2000' TO Config-File

   WHEN S-Cfg-COBOL85 NOT = SPACES
     MOVE 'cobol85' TO Config-File

   WHEN S-Cfg-COBOL2002 NOT = SPACES
     MOVE 'cobol2002' TO Config-File

   WHEN S-Cfg-IBM NOT = SPACES
     MOVE 'ibm' TO Config-File

   WHEN S-Cfg-MF NOT = SPACES
     MOVE 'mf' TO Config-File

   WHEN S-Cfg-MVS NOT = SPACES
     MOVE 'mvs' TO Config-File

   WHEN OTHER
     MOVE 'default' TO Config-File
 END-EVALUATE
 .

 212-Build-Compile-Command.

 MOVE SPACES TO Cobc-Cmd
 STRING 'cobc -std='
   TRIM(Config-File,TRAILING)
   ' '
   INTO Cobc-Cmd
   WITH POINTER I
 END-STRING

 IF S-SUBROUTINE NOT = ' '
   STRING '-m '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
   END-STRING
 ELSE
   STRING '-x '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
     END-STRING
 END-IF

 IF S-DEBUG NOT = ' '
   STRING '-fdebugging-line '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
     END-STRING
 END-IF

 IF S-NOTRUNC NOT = ' '
   STRING '-fnotrunc '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
   END-STRING
 END-IF

 IF S-TRACEALL NOT = ' '
   STRING '-ftraceall '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
   END-STRING
 END-IF

 IF S-TRACE NOT = ' '
   STRING '-ftrace '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
     END-STRING 
 END-IF

 IF S-EXTRA > SPACES
   STRING ' '
     TRIM(S-Extra,TRAILING)
     ' '
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
     END-STRING
 END-IF

 STRING TRIM(Prog-File-Name,TRAILING)
     DELIMITED SIZE INTO Cobc-Cmd
     WITH POINTER I
 END-STRING
 .

 213-Run-Compiler.

 MOVE ' Compiling...' TO Output-Message

 DISPLAY
   Switches-Screen
 END-DISPLAY

 SET 88-Output-File-Avail TO TRUE
 MOVE SPACES TO Cmd

 STRING TRIM(Cobc-Cmd,TRAILING)
   ' 2>'
   TRIM(Cobc-Output-File,TRAILING)
   DELIMITED SIZE
   INTO Cmd
 END-STRING

 CALL 'SYSTEM'
   USING TRIM(Cmd,TRAILING)
 END-CALL

*>DISPLAY "RETURN-CODE=" RETURN-CODE
*>コンパイルＯＫでもRETURN-CODE＝０以外になるので、０にする
MOVE  0 TO RETURN-CODE


 IF RETURN-CODE = 0
   SET 88-Compile-OK TO TRUE
 ELSE
   SET 88-Compile-Failed TO TRUE
 END-IF

 IF 88-Compile-OK
   OPEN INPUT Cobc-Output
   READ Cobc-Output
     AT END
*> 0件はｺﾝﾊﾟｲﾙ OKにしている
       CONTINUE
     NOT AT END
*> 1件以上はｺﾝﾊﾟｲﾙ ﾜｰﾆﾝｸﾞにしている
       SET 88-Compile-OK-Warn TO TRUE
   END-READ
   CLOSE Cobc-Output
 END-IF

 MOVE SPACES TO Output-Message

*> このコンパイラーOKにならない　常にワーニングかエラー
 IF 88-Compile-OK
   MOVE ' Compilation Was Successful' TO Output-Message
   DISPLAY
     Switches-Screen
   END-DISPLAY
   CALL 'C$SLEEP'
     USING 5
   END-CALL

   MOVE SPACES TO Output-Message
   SET 88-Complete TO TRUE
 ELSE
     IF 88-Compile-OK-Warn
       MOVE ' Compilation was successful, but warnings were generated:'
         TO Output-Message
     ELSE
       MOVE 'Compilation Failed:'
         TO Output-Message
     END-IF
     DISPLAY
       Switches-Screen
     END-DISPLAY

     MOVE SPACES TO Output-Message
     SET 88-Complete TO TRUE
     GO 219-Done
*> 以下実行しない

   DISPLAY
     Blank-Screen
   END-DISPLAY

   IF 88-Compile-OK-Warn
     DISPLAY ' Compilation was successful, but warnings were generated:'
       AT LINE 24 COLUMN 1
       WITH SCROLL UP 1 LINE
     END-DISPLAY
   ELSE
     DISPLAY 'Compilation Failed:'
       AT LINE 24 COLUMN 1
       WITH SCROLL UP 1 LINE
     END-DISPLAY
   END-IF

*>   SET 88-Compile-Failed TO TRUE
*>   SET 88-Complete TO TRUE
   DISPLAY ' '
     AT LINE 24 COLUMN 1
     WITH SCROLL UP 1 LINE
   END-DISPLAY

   OPEN INPUT Cobc-Output
   PERFORM FOREVER
     READ Cobc-Output AT END
       EXIT PERFORM 
     END-READ

     DISPLAY TRIM(Cobc-Output-Rec,TRAILING)
       AT LINE 24 COLUMN 1
       WITH SCROLL UP 1 LINE
     END-DISPLAY
   END-PERFORM

   CLOSE Cobc-Output
   DISPLAY ' '
     AT LINE 24 COLUMN 1
     WITH SCROLL UP 2 LINES
   END-DISPLAY

   DISPLAY 'Press ENTER to close:'
     AT LINE 24 COLUMN 1
     WITH SCROLL UP 1 LINE
   END-DISPLAY

   ACCEPT Dummy
     FROM CONSOLE
   END-ACCEPT

   DISPLAY
     Blank-Screen
   END-DISPLAY
 END-IF
 .

 219-Done.
 IF 88-Compile-Failed
CONTINUE
*>   PERFORM 900-Terminate
 END-IF
 .
 *>/
 220-Make-Listing SECTION.
*>****************************************************************
*>* Generate a source and/or xref listing using XREF **
*>****************************************************************

 221-Init.

 MOVE ' Generating cross-reference listing...' TO Output-Message
 DISPLAY
   Switches-Screen
 END-DISPLAY

 CALL "CBL_DELETE_FILE"
   USING CONCATENATE(TRIM(Prog-Name,Trailing),".lst")
 END-CALL
 MOVE 0 TO RETURN-CODE
 .

 213-Run-OCXref.

 MOVE SPACES TO Output-Message
 CALL 'LISTING'
   USING S-SOURCE
     S-XREF
     File-Name
     Report-Filename

   ON EXCEPTION
   MOVE ' LISTING module is not available' TO Output-Message
   MOVE 1 TO RETURN-CODE
 END-CALL

 IF RETURN-CODE = 0
   MOVE ' Listing generated' TO Output-Message
   IF OS-Windows OR OS-Cygwin
     MOVE SPACES TO Cmd
     STRING
       'cmd /c '
       TRIM(Prog-Name,TRAILING)
       '.lst'
       DELIMITED SIZE INTO Cmd
     END-STRING

     CALL 'SYSTEM'
       USING TRIM(Cmd,TRAILING)
     END-CALL
   END-IF
 ELSE
   IF Output-Message = SPACES
     MOVE ' Listing generation failed' TO Output-Message
   END-IF
 END-IF
 
 
*> DISPLAY "Output-Message=" Output-Message
*> DISPLAY "RETURN-CODE=" RETURN-CODE

*> ACCEPT Dummy
*> FROM CONSOLE
*> END-ACCEPT

 DISPLAY
   Switches-Screen
 END-DISPLAY
*> CALL 'C$SLEEP'
*>   USING 5 
*> END-CALL
 .
 *>/
 230-Run-Program SECTION.
 *>****************************************************************
 *>* Run the compiled program **
 *>****************************************************************

 232-Build-Command.
 MOVE SPACES TO Cmd
 MOVE 1 TO I
 IF S-SUBROUTINE NOT = ' '
   OR S-DLL NOT = ' '
   STRING 'cobcrun ' DELIMITED SIZE
     INTO Cmd
     WITH POINTER I
   END-STRING
 END-IF

 IF Prog-Folder NOT = SPACES
   IF OS-Cygwin AND Prog-Folder (2:1) = ':'
     STRING '/cygdrive/'
       INTO Cmd
       WITH POINTER I
     END-STRING

     STRING LOWER-CASE(Prog-Folder (1:1))
       INTO Cmd
       WITH POINTER I
     END-STRING

     PERFORM VARYING J FROM 3 BY 1
       UNTIL J > LENGTH(TRIM(Prog-Folder))
       IF Prog-Folder (J:1) = '\'
         STRING '/'
           INTO Cmd
           WITH POINTER I
         END-STRING
       ELSE
         STRING Prog-Folder (J:1)
           INTO Cmd
           WITH POINTER I
         END-STRING
       END-IF
     END-PERFORM
   ELSE
     STRING '"' TRIM(Prog-Folder,TRAILING)
       INTO Cmd
       WITH POINTER I
     END-STRING
   END-IF

   STRING Dir-Char
     INTO Cmd
     WITH POINTER I
   END-STRING
 ELSE
   IF OS-Cygwin OR OS-UNIX
     STRING './'
       INTO Cmd
       WITH POINTER I
     END-STRING
   END-IF
 END-IF

 STRING TRIM(Prog-Name,TRAILING)
   INTO Cmd
   WITH POINTER I
 END-STRING

 IF S-SUBROUTINE = ' '
   AND S-DLL NOT = ' '
   STRING '.exe' DELIMITED SIZE
     INTO Cmd
     WITH POINTER I
   END-STRING
 END-IF

 IF S-ARGS NOT = SPACES
   STRING ' ' TRIM(S-ARGS,TRAILING)
     INTO Cmd
     WITH POINTER I
   END-STRING
 END-IF

 IF OS-Unknown OR OS-Windows
   STRING '"&&pause'
     INTO Cmd
     WITH POINTER I
   END-STRING
 ELSE
   STRING ';echo "Press ENTER to close...";read' 
     INTO Cmd
     WITH POINTER I
   END-STRING
 END-IF
 .

 233-Run-Program.
 DISPLAY
   Blank-Screen
 END-DISPLAY

 CALL 'SYSTEM'
   USING TRIM(Cmd,TRAILING)
 END-CALL
 PERFORM 900-Terminate
 .

 239-Done.
 EXIT.
 *>/

 900-Terminate SECTION.
 *>****************************************************************
 *>* Display a message and halt the program **
 *>****************************************************************

 901-Display-Message.

*> IF Output-Message > SPACES
   DISPLAY
     Switches-Screen
   END-DISPLAY
   CALL 'C$SLEEP'
     USING 5
   END-CALL
*> END-IF

 DISPLAY
  Blank-Screen
 END-DISPLAY

           MOVE    "X"         TO      WFD-ID
           MOVE    "BAD-F-NAME" TO     WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       Bat-File-Name

           MOVE    "X"         TO      WFD-ID
           MOVE    "Cobc=output" TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       Cobc-Output-File

           MOVE    "X"         TO      WFD-ID
           MOVE    "source=F"  TO    WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       File-Name

      *>     MOVE    "P"         TO      WFD-ID
      *>     CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *>                                 Cobc-Output-Rec

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       Cobc-Output-Rec

 .

 909-Done.
 GOBACK
 .

 END PROGRAM OCic.
