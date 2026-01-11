      *    *** TEST117 —p iQIYI ƒf[ƒ^C³
      *    *** 
      *    *** Å‰‚Ì y,z ‚ðƒJƒbƒg
      *    *** Å‰‚Ì | ‚Ì‘O‚ÌŠ¿Žš‚Éyz‚ð•t‚¯‚é
      *    *** 
      *    *** TEST124
      *    ***    |
      *    *** TEST117
      *    ***    |
      *    *** TEST104
      *    ***    |
      *    *** TEST53
      *    ***    |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST124.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** TEST103.PRM1
       SELECT PRM1-F           ASSIGN   WK-PRM1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.Get the iQIYI APP.PIN1W
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** TEST103.Get the iQIYI APP.PIN1X
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PRM1-F
           RECORD VARYING DEPENDING ON WK-PRM1-LEN.
       01  PRM1-REC.
           03                  PIC  X(080).

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  POT1-F.
       01  POT1-REC.
           03                  PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST124 ".

           03  WK-PRM1-F-NAME  PIC  X(032) VALUE "TEST103.PRM1".
           03  WK-PIN1-F-NAME  PIC  X(064) VALUE
               "TEST103.XXXXXXXX.PIN1W".
           03  WK-POT1-F-NAME  PIC  X(064) VALUE 
               "TEST124.XXXXXXXX.PIN1X".

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PRM1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PRM1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PRM1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-FILE-NAME    PIC  X(064) VALUE SPACE.
           03  WK-COUNT-1      BINARY-LONG SYNC VALUE ZERO.
           03  WK-COUNT-2      BINARY-LONG SYNC VALUE ZERO.
           03  WK-START        BINARY-LONG SYNC VALUE ZERO.
           03  WK-END          BINARY-LONG SYNC VALUE ZERO.
           03  WK-LEN          BINARY-LONG SYNC VALUE ZERO.
           03  WK-SPACE        BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-REC     PIC  X(1000) VALUE SPACE.

      *    *** ‰Šú’l MODE=AA   (ANK=>ANK)
           03  WK-MODE         PIC  X(002) VALUE "AA".
      *    *** ‰Šú’l HENKAN=US (UTF8=>SJIS)
           03  WK-HENKAN       PIC  X(006) VALUE "US".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

           COPY    CPDECODE05  REPLACING ==:##:== BY ==WDE05==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-XX           PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1
           PERFORM S020-10     THRU    S020-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

                   EVALUATE TRUE
                       WHEN WK-FILE-NAME (1:WK-PRM1-LEN)
                          = "‚‰‚p‚h‚x‚h@ƒAƒCƒ`[ƒC["
                         OR "‚‰‚p‚h‚x‚h@ƒ`ƒƒƒCƒj[ƒY@ƒVƒAƒ^["
                         OR "‚‰‚p‚h‚x‚h@ƒIƒŠƒGƒ“ƒ^ƒ‹@ƒtƒ@ƒ“ƒ^ƒW["
                         OR "‚‰‚p‚h‚x‚h@ƒƒ}ƒ“ƒX"
      *    *** •ÏŠ·
                           PERFORM S100-10     THRU    S100-EX

                       WHEN WK-FILE-NAME (1:WK-PRM1-LEN)
                          = "YOUKU ROMANCE"
      *    *** •ÏŠ·
                           PERFORM S200-10     THRU    S200-EX
                  END-EVALUATE

      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX
           END-PERFORM

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** START DISPLAY,OPEN,READ PRM1
       S010-10.

           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

           MOVE    "OPEN  "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           OPEN    INPUT       PRM1-F
           READ    PRM1-F
                   AT  END
                   DISPLAY WK-PGM-NAME " PRM1-F 0¹Ý YouTube USER Žw’è"
                   STOP    RUN
           END-READ
           ADD     1           TO      WK-PRM1-CNT

      *    *** ƒtƒ@ƒCƒ‹–¼‚ÍŠ¿Žš‚Ì‚Ý‚©A‚PƒoƒCƒgŒn‚Ì‚Ý‚Ì‚Ç‚¿‚ç‚©‚É•ÒW‚·‚é
           IF      PRM1-REC (1:1) >=   X"E0" AND <= X"EF"
                   MOVE    "CHANGE"    TO      WDE05-ID
                   MOVE    WK-HENKAN   TO      WDE05-HENKAN
                   MOVE    WK-MODE     TO      WDE05-MODE
                   MOVE    WK-PRM1-LEN TO      WDE05-BUF1-LEN
                   MOVE    WK-PRM1-CNT TO      WDE05-BUF1-CNT
      *    *** ƒtƒ@ƒCƒ‹–¼ ‚t‚s‚e‚W„‚r‚i‚h‚r‚É•ÏŠ·
                   CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                               PRM1-REC
                                               WK-FILE-NAME
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-POT1-F-NAME (1:8)
                   MOVE    WK-FILE-NAME TO     WK-PIN1-F-NAME (9:)
                                               WK-POT1-F-NAME (9:)
                   MOVE    ".PIN1W"    TO      WK-PIN1-F-NAME
                                               (WDE05-BUF2-LEN + 9:6)
                   MOVE    ".PIN1X"     TO      WK-POT1-F-NAME
                                               (WDE05-BUF2-LEN + 9:6)
           ELSE
                   MOVE    "TEST103."  TO      WK-PIN1-F-NAME (1:8)
                                               WK-POT1-F-NAME (1:8)
                   MOVE    PRM1-REC    TO      WK-PIN1-F-NAME (9:)
                                               WK-POT1-F-NAME (9:)
                   MOVE    ".PIN1W"    TO      WK-PIN1-F-NAME
                                               (WK-PRM1-LEN + 9:6)
                   MOVE    ".PIN1X"     TO      WK-POT1-F-NAME
                                               (WK-PRM1-LEN + 9:6)
                   MOVE    PRM1-REC    TO      WK-FILE-NAME
           END-IF

           OPEN    INPUT       PIN1-F
                   OUTPUT      POT1-F

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** •ÏŠ·
       S100-10.

           
           MOVE    ZERO        TO      WK-COUNT-1
           ADD     1           TO      WK-COUNT-2

      *    *** WK-COUNT-2 = 1 3 –{‚Ì“®‰æÄ¶’†...
      *    *** WK-COUNT-2 = 3 Ä¶ƒŠƒXƒg‚Ì‘S‘Ì‚ðŒ©‚é...
           IF      WK-COUNT-2  =       1 OR 3
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT
                   IF      WK-COUNT-2  =       3
                           MOVE    ZERO        TO      WK-COUNT-2
                   END-IF
                   GO  TO  S100-EX
           END-IF

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:7) =    ",https:"
                      OR J > WK-PIN1-LEN

                   EVALUATE TRUE
                       WHEN PIN1-REC (J:1) =    ","
                           MOVE    "."         TO      PIN1-REC (J:1)
                   END-EVALUATE
           END-PERFORM

      *    *** t
           INSPECT PIN1-REC TALLYING
                   WK-COUNT-1 FOR ALL X"E3808B"

           IF      WK-COUNT-1  NOT =   ZERO
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S100-EX
           END-IF

           MOVE    ZERO        TO      WK-COUNT-1
      *    *** y
           INSPECT PIN1-REC TALLYING
                   WK-COUNT-1 FOR ALL X"E38090"

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      I2

      *    *** y
           IF      WK-COUNT-1     =     ZERO

                   ADD     1           TO      I2
      *    *** y
                   MOVE    X"E38090"   TO      POT1-REC (I2:3)
                   ADD     2           TO      I2

                   PERFORM VARYING I3 FROM 1 BY 1
                           UNTIL I3 > WK-PIN1-LEN
                              OR PIN1-REC (I3:3) = " | "
                              OR PIN1-REC (I3:1) = ","
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I3:1) TO  POT1-REC (I2:1)
                   END-PERFORM

                   IF      PIN1-REC (I3:3) = " | "
                           ADD     1           TO      I2
      *    *** z
                           MOVE    X"E38091"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           COMPUTE I = I3  + 2
                   ELSE
                       IF      PIN1-REC (I3:1) = ","
                           ADD     1           TO      I2
      *    *** z
                           MOVE    X"E38091"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2

                           COMPUTE I = I3
                       ELSE
                           COMPUTE I = I3
                       END-IF
                   END-IF

                   PERFORM VARYING I3 FROM I BY 1
                           UNTIL I3 > WK-PIN1-LEN
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I3:1) TO   POT1-REC (I2:1)
                   END-PERFORM

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
                   PERFORM S110-10     THRU    S110-EX

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S100-EX
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      I2

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   EVALUATE TRUE
      *    *** yMemberz
                      WHEN PIN1-REC (I:12) = X"E380904D656D626572E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:12) TO  POT1-REC (I2:12)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                     POT1-REC (I2 + 9:3)
                           ADD     12          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     12          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yKiwi OnlybFULLz
                      WHEN PIN1-REC (I:22) =
                         X"E380904B697769204F6E6C79EFBD9C46554C4CE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:22) TO  POT1-REC (I2:22)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 19:3)
                           ADD     22          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     22          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yKiwi Only | FULLz
                      WHEN PIN1-REC (I:22) =
                         X"E380904B697769204F6E6C79207C2046554C4CE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:22) TO  POT1-REC (I2:22)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 19:3)
                           ADD     22          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     22          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yKiwi Onlyz
                      WHEN PIN1-REC (I:15) =
                           X"E380904B697769204F6E6C79E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:15) TO  POT1-REC (I2:15)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 12:3)
                           ADD     15          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     15          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yFULL EP ‘SWŠÅz
                      WHEN PIN1-REC (I:23) =
                       X"E3809046554C4C20455020E585A8E99B86E79C8BE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:23) TO  POT1-REC (I2:23)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 20:3)
                           ADD     23          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     23          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I
      *    *** yFULLz
                      WHEN PIN1-REC (I:10) = X"E3809046554C4CE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:10) TO  POT1-REC (I2:10)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                     POT1-REC (I2 + 7:3)
                           ADD     10          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     10          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

                       WHEN OTHER
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1) TO  POT1-REC (I2:1)
                   END-EVALUATE
           END-PERFORM

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
           PERFORM S110-10     THRU    S110-EX

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S100-EX.
           EXIT.

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
      *    *** yABC ‚`‚a‚bz =>@y‚`‚a‚b ABCz
       S110-10.

           MOVE    ZERO        TO      WK-START
                                       WK-END
                                       WK-SPACE
                                       WK-LEN

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I2
                      OR WK-END NOT = ZERO
      *    *** y
                   IF      POT1-REC (I:3) = X"E38090"
                           MOVE    I           TO      WK-START
                           ADD     2           TO      I
                   END-IF
      *    *** z
                   IF      POT1-REC (I:3) = X"E38091"
                           COMPUTE WK-END = I + 2
                           COMPUTE WK-LEN = WK-END - WK-START + 1
                   END-IF
      *    ***  ÅŒã‚ÌƒXƒy[ƒX‚ÌˆÊ’u
                   IF      WK-START NOT =   ZERO
                       IF      POT1-REC (I:1) = SPACE
                               MOVE    I           TO      WK-SPACE
                       END-IF
                   END-IF
           END-PERFORM

           IF      WK-START    NOT =   ZERO
               AND WK-END      NOT =   ZERO
                   MOVE    POT1-REC    TO      WK-POT1-REC
                   MOVE    SPACE       TO     POT1-REC (WK-START:WK-LEN)

                   IF      WK-SPACE    =       ZERO
                           MOVE    WK-POT1-REC (WK-START:WK-LEN) TO
                                   POT1-REC (WK-START:WK-LEN)
                   ELSE
      *    *** ÅŒã‚ªŠ¿Žš‚©H
                       IF  WK-POT1-REC (WK-END - 2:1) >= X"E0"
                       AND WK-POT1-REC (WK-END - 2:1) <= X"E9"
      *    *** y
                           MOVE    X"E38090"   TO      
                                   POT1-REC (WK-START:3)

                           COMPUTE I = WK-END - WK-SPACE - 3
                           MOVE    WK-POT1-REC (WK-SPACE + 1:I) TO
                                   POT1-REC (WK-START + 3:I)

                           COMPUTE I3 = WK-START + 3 + I + 1
                           COMPUTE I = WK-SPACE - WK-START - 1
                           MOVE    WK-POT1-REC (WK-START + 3:I) TO
                                   POT1-REC (I3:I)
      *    *** z
                           COMPUTE I = WK-END - 2
      *    *** æ‚ÉMOVE‚·‚é‚ÆA1ƒoƒCƒg–ÚŒ‡‚¯‚½
                           MOVE    X"E38091"   TO      POT1-REC (I:3)
                       ELSE
                           MOVE    WK-POT1-REC (WK-START:WK-LEN) TO
                                   POT1-REC (WK-START:WK-LEN)
                       END-IF
                   END-IF
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** •ÏŠ·
       S200-10.

           
           MOVE    ZERO        TO      WK-COUNT-1
           ADD     1           TO      WK-COUNT-2

      *    *** WK-COUNT-2 = 1 3 –{‚Ì“®‰æÄ¶’†...
      *    *** WK-COUNT-2 = 3 Ä¶ƒŠƒXƒg‚Ì‘S‘Ì‚ðŒ©‚é...
           IF      WK-COUNT-2  =       1 OR 3
                   WRITE   POT1-REC    FROM    PIN1-REC
                   ADD     1           TO      WK-POT1-CNT
                   IF      WK-COUNT-2  =       3
                           MOVE    ZERO        TO      WK-COUNT-2
                   END-IF
                   GO  TO  S200-EX
           END-IF

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL PIN1-REC (J:7) =    ",https:"
                      OR J > WK-PIN1-LEN

                   EVALUATE TRUE
                       WHEN PIN1-REC (J:1) =    ","
                           MOVE    "."         TO      PIN1-REC (J:1)
                   END-EVALUATE
           END-PERFORM


           MOVE    ZERO        TO      WK-COUNT-1
      *    *** y
           INSPECT PIN1-REC TALLYING
                   WK-COUNT-1 FOR ALL X"E38090"

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      I2

      *    *** y
           IF      WK-COUNT-1     =     ZERO

                   ADD     1           TO      I2
      *    *** y
                   MOVE    X"E38090"   TO      POT1-REC (I2:3)
                   ADD     2           TO      I2

                   PERFORM VARYING I3 FROM 1 BY 1
                           UNTIL I3 > WK-PIN1-LEN
                              OR PIN1-REC (I3:3) = " | "
                              OR PIN1-REC (I3:1) = ","
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I3:1) TO  POT1-REC (I2:1)
                   END-PERFORM

                   IF      PIN1-REC (I3:3) = " | "
                           ADD     1           TO      I2
      *    *** z
                           MOVE    X"E38091"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           COMPUTE I = I3  + 2
                   ELSE
                       IF      PIN1-REC (I3:1) = ","
                           ADD     1           TO      I2
      *    *** z
                           MOVE    X"E38091"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2

                           COMPUTE I = I3
                       ELSE
                           COMPUTE I = I3
                       END-IF
                   END-IF

                   PERFORM VARYING I3 FROM I BY 1
                           UNTIL I3 > WK-PIN1-LEN
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I3:1) TO   POT1-REC (I2:1)
                   END-PERFORM

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
                   PERFORM S210-10     THRU    S210-EX

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
                   GO  TO  S200-EX
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    ZERO        TO      I2

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN

                   EVALUATE TRUE

      *    *** yFULLz
                      WHEN PIN1-REC (I:10) = X"E3809046554C4CE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:10) TO  POT1-REC (I2:10)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                     POT1-REC (I2 + 7:3)
                           ADD     10          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     10          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** ySHORTSz
                      WHEN PIN1-REC (I:12) =
                           X"E3809053484F525453E38091"
      *    *** yShortsz
                        OR X"E3809053686F727473E38091"
      *    *** yshortsz
                        OR X"E3809073686F727473E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:12) TO  POT1-REC (I2:12)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 9:3)
                           ADD     12          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     12          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yPLAYLETz
                      WHEN PIN1-REC (I:13) =
                           X"E38090504C41594C4554E38091"
      *    *** yPREVIEWz
                        OR X"E3809050524556494557E38091"
      *    *** yPreviewz
                        OR X"E3809050726576696577E38091"
      *    *** ySPECIALz
                        OR X"E380905350454349414CE38091"
      *    *** yTRAILERz
                        OR X"E38090545241494C4552E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:13) TO  POT1-REC (I2:13)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 10:3)
                           ADD     13          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     13          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yHIGHLIGHTz
                      WHEN PIN1-REC (I:15) =
                         X"E38090484947484C49474854E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:15) TO  POT1-REC (I2:15)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 12:3)
                           ADD     15          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     15          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yTRAILER+BTSz
                      WHEN PIN1-REC (I:17) =
                           X"E38090545241494C45522B425453E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:17) TO  POT1-REC (I2:17)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 14:3)
                           ADD     17          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     17          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yESSENCE VERSIONz
                      WHEN PIN1-REC (I:21) = 
                           X"E38090455353454E43452056455253494F4EE38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:21) TO  POT1-REC (I2:21)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 18:3)
                           ADD     21          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     21          TO      I

      *    *** yESSENCE VERSIONz’¼Œã‚Ì | ‚ðƒXƒy[ƒX‚É’u‚«Š·‚¦
                           IF      PIN1-REC (22:3) =   " | "
                                   MOVE    SPACE      TO PIN1-REC (22:3)

                           END-IF
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yHIGHLIGHT+ESSENCEz
                      WHEN PIN1-REC (I:23) =
                       X"E38090484947484C494748542B455353454E4345E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:23) TO  POT1-REC (I2:23)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 20:3)
                           ADD     23          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     23          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yMembership Contentsz
                      WHEN PIN1-REC (I:25) =
                   X"E380904D656D6265727368697020436F6E74656E7473E38091"
      *    *** yTRAILER+BTS+Previewz
                OR X"E38090545241494C45522B4254532B50726576696577E38091"
      *    *** yTrailer+BTS+Previewz
                OR X"E38090547261696C65722B4254532B50726576696577E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:25) TO  POT1-REC (I2:25)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 22:3)
                           ADD     25          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     25          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yTRAILER+BTS{PREVIEWz
                      WHEN PIN1-REC (I:27) =
               X"E38090545241494C45522B425453EFBC8B50524556494557E38091"
      *    ***yBTS+TRAILER{PREVIEWz
            OR X"E380904254532B545241494C4552EFBC8B50524556494557E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:27) TO  POT1-REC (I2:27)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 24:3)
                           ADD     27          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     27          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

      *    *** yMembership Contents+BTSz
                      WHEN PIN1-REC (I:29) =
           X"E380904D656D6265727368697020436F6E74656E74732B425453E38091"
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:29) TO  POT1-REC (I2:29)
      *    *** ]
                           MOVE    X"E28090"   TO      POT1-REC (I2:3)
                                                    POT1-REC (I2 + 26:3)
                           ADD     29          TO      I2
      *    *** y
                           MOVE    X"E38090"   TO      POT1-REC (I2:3)
                           ADD     2           TO      I2
                           ADD     29          TO      I
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                      OR PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           IF      PIN1-REC (I3:3) = " | "
                                   ADD     1           TO      I2
      *    *** z
                                   MOVE    X"E38091"   TO      
                                           POT1-REC (I2:3)
                                   ADD     2           TO      I2
                           END-IF
                           COMPUTE I = I3  + 2
                           PERFORM VARYING I3 FROM I BY 1
                                   UNTIL I3 > WK-PIN1-LEN
                                   ADD     1           TO      I2
                                   MOVE    PIN1-REC (I3:1) TO  
                                           POT1-REC (I2:1)
                           END-PERFORM
                           MOVE    WK-PIN1-LEN TO      I

                       WHEN OTHER
                           ADD     1           TO      I2
                           MOVE    PIN1-REC (I:1) TO  POT1-REC (I2:1)
                   END-EVALUATE
           END-PERFORM

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
           PERFORM S210-10     THRU    S210-EX

           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT
           .
       S200-EX.
           EXIT.

      *    *** Š¿Žš‚ð‘O‚É‚·‚é
      *    *** yABC ‚`‚a‚bz =>@y‚`‚a‚b ABCz
       S210-10.

           MOVE    ZERO        TO      WK-START
                                       WK-END
                                       WK-SPACE
                                       WK-LEN

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > I2
                      OR WK-END NOT = ZERO
      *    *** y
                   IF      POT1-REC (I:3) = X"E38090"
                           MOVE    I           TO      WK-START
                           ADD     2           TO      I
                   END-IF
      *    *** z
                   IF      POT1-REC (I:3) = X"E38091"
                           COMPUTE WK-END = I + 2
                           COMPUTE WK-LEN = WK-END - WK-START + 1
                   END-IF
      *    ***  ÅŒã‚ÌƒXƒy[ƒX‚ÌˆÊ’u
                   IF      WK-START NOT =   ZERO
                       IF      POT1-REC (I:1) = SPACE
                               MOVE    I           TO      WK-SPACE
                       END-IF
                   END-IF
           END-PERFORM

           IF      WK-START    NOT =   ZERO
               AND WK-END      NOT =   ZERO
                   MOVE    POT1-REC    TO      WK-POT1-REC
                   MOVE    SPACE       TO     POT1-REC (WK-START:WK-LEN)

                   IF      WK-SPACE    =       ZERO
                           MOVE    WK-POT1-REC (WK-START:WK-LEN) TO
                                   POT1-REC (WK-START:WK-LEN)
                   ELSE
      *    *** ÅŒã‚ªŠ¿Žš‚©H
                       IF  WK-POT1-REC (WK-END - 2:1) >= X"E0"
                       AND WK-POT1-REC (WK-END - 2:1) <= X"E9"
      *    *** y
                           MOVE    X"E38090"   TO      
                                   POT1-REC (WK-START:3)

                           COMPUTE I = WK-END - WK-SPACE - 3
                           MOVE    WK-POT1-REC (WK-SPACE + 1:I) TO
                                   POT1-REC (WK-START + 3:I)

                           COMPUTE I3 = WK-START + 3 + I + 1
                           COMPUTE I = WK-SPACE - WK-START - 1
                           MOVE    WK-POT1-REC (WK-START + 3:I) TO
                                   POT1-REC (I3:I)
      *    *** z
                           COMPUTE I = WK-END - 2
      *    *** æ‚ÉMOVE‚·‚é‚ÆA1ƒoƒCƒg–ÚŒ‡‚¯‚½
                           MOVE    X"E38091"   TO      POT1-REC (I:3)
                       ELSE
                           MOVE    WK-POT1-REC (WK-START:WK-LEN) TO
                                   POT1-REC (WK-START:WK-LEN)
                       END-IF
                   END-IF
           END-IF
           .
       S210-EX.
           EXIT.

      *    *** CLOSE,END DISPLAY
       S900-10.

           CLOSE   PRM1-F
                   PIN1-F
                   POT1-F

           MOVE    "CLOSE "    TO      WDE05-ID
           CALL    "DECODE05"  USING   WDE05-DECODE05-AREA
                                       PIN1-REC
                                       POT1-REC

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PRM1-CNT TO      WK-PRM1-CNT-E
           DISPLAY WK-PGM-NAME " PRM1 Œ” = " WK-PRM1-CNT-E
                   " (" WK-PRM1-F-NAME ")"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 Œ” = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 Œ” = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
