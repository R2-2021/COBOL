      *    *** Open COBOL より改変
      *    *** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDUMP.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Addr-Number         BINARY-LONG SYNC VALUE ZERO.
       01  Addr-Pointer        REDEFINES Addr-Number
                               POINTER.

       01  Addr-Sub            BINARY-LONG SYNC VALUE ZERO.

       01  Addr-Value          BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Length       BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16
                               PIC  X(001).

       01  Left-Nibble         PIC  9(002) COMP-5 VALUE ZERO.
       01  Nibble              REDEFINES Left-Nibble BINARY-CHAR.

       01  Right-Nibble        PIC  9(002) COMP-5 VALUE ZERO.

       01  Output-Detail       VALUE SPACE.
           05  OD-Addr.
             10  OD-Addr-Hex   OCCURS 8
                               PIC  X(001).
           05  FILLER          PIC  X(001).
           05  OD-Byte         PIC  Z(4)9.
           05  FILLER          PIC  X(0001).
           05  OD-Hex          OCCURS 16.
             10  OD-Hex-1      PIC  X(001).
             10  OD-Hex-2      PIC  X(001).
             10  FILLER        PIC  X(001).
           05  OD-ASCII        OCCURS 17
                               PIC  X(001).

       01  Output-Sub          PIC  9(002) COMP-5 VALUE ZERO.

       01  Output-Header-1.
           05  FILLER          PIC  X(080) VALUE
               '<-Addr->  Byte ' &
               '<---------------- Hexadecimal ----------------> ' &
               '<---- Char ---->'. 

       01  Output-Header-2.
           05  FILLER          PIC X(080) VALUE
               '======== ===== ' &
      *         '=============================================== ' &
      *         '================'.
               '01=02=03=04=05=06=07=08=09=10=11=12=13=14=15=16 ' &
               '====5====1====5='.

       01  PIC-XX.
           05  FILLER          PIC  X(001) VALUE LOW-VALUE.
           05  PIC-X           PIC  X(001) VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX
                               PIC  9(004) COMP-X.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           05  SW-KANJI        PIC  X(001) VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

       LINKAGE SECTION.

       01  Buffer              PIC  X(001) ANY LENGTH.

       01  Buffer-Len          BINARY-LONG SYNC.

       PROCEDURE DIVISION USING Buffer, OPTIONAL Buffer-Len.

       000-COBDUMP.

      *    ***  Bufferのみは NUMBER-OF-CALL-PARAMETERS=1
           IF NUMBER-OF-CALL-PARAMETERS = 1
              MOVE LENGTH(Buffer) TO Buffer-Length
           ELSE
              MOVE Buffer-Len     TO Buffer-Length
      *    *** 指定された長さが、項目長を超えてる時、項目長にする
              IF   Buffer-Len >   LENGTH(Buffer)  OR
                   Buffer-Len =   ZERO
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           END-IF

           MOVE SPACES            TO Output-Detail
           SET Addr-Pointer       TO ADDRESS OF Buffer

           PERFORM 100-Generate-Address
           MOVE ZERO TO Output-Sub

           DISPLAY Output-Header-1 UPON SYSERR
           DISPLAY Output-Header-2 UPON SYSERR

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length

                   ADD 1 TO Output-Sub

      *    *** SJIS CHECK
                   PERFORM  S210-10 THRU S210-EX

                   IF Output-Sub = 1
                      MOVE Buffer-Sub TO OD-Byte
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** X"20"=SPACE ANK 以外SPACEセット 
                   IF    ( PIC-X <  X"20")
                      OR ( PIC-X =  X"7F")
                      OR ( PIC-X >= X"FD" AND <= X"FF")
                            MOVE SPACE TO OD-ASCII (Output-Sub)
                   ELSE
      *    *** 1行前、16:2 "0",SJISなら (1)にSPACEセット,その他ならSET
                       IF   OUTPUT-SUB = 1
                            IF   Buffer-Sub = 1
                                 MOVE PIC-X TO OD-ASCII (Output-Sub)
                            ELSE
                                 IF SW-KANJI = "1"
                                     MOVE PIC-X TO OD-ASCII (Output-Sub)
                                 ELSE
                                     MOVE SPACE TO OD-ASCII (Output-Sub)
                                 END-IF
                            END-IF
                       ELSE
                            MOVE PIC-X TO OD-ASCII (Output-Sub)
                       END-IF
                   END-IF

                   DIVIDE PIC-Halfword BY 16
                          GIVING Left-Nibble
                          REMAINDER Right-Nibble

                   ADD 1 TO Left-Nibble Right-Nibble

                   MOVE Hex-Digit (Left-Nibble)
                           TO OD-Hex-1 (Output-Sub)

                   MOVE Hex-Digit (Right-Nibble)
                           TO OD-Hex-2 (Output-Sub)

                   IF  Output-Sub = 16
                       IF SW-KANJI = "1"
                            MOVE Buffer (Buffer-Sub + 1:1) TO 
                                 OD-ASCII (17)
                       ELSE
                            MOVE SPACE TO OD-ASCII (17)
                       END-IF

                       DISPLAY Output-Detail UPON SYSERR END-DISPLAY

                       MOVE SPACES TO Output-Detail
                       MOVE ZERO TO Output-Sub

                       SET Addr-Pointer UP BY 16
                       PERFORM 100-Generate-Address
                   END-IF
           END-PERFORM

           IF  Output-Sub > ZERO
               DISPLAY Output-Detail UPON SYSERR
           END-IF 
       EXIT PROGRAM.
           EXIT.

       100-Generate-Address.
      *    *** アドレスＨＥＸ8桁分、4バイト分
           MOVE 8 TO Addr-Sub
           MOVE Addr-Number TO Addr-Value

      *    *** Addr-Valueには、ダンプ対象のアドレス入っている
           MOVE ALL '0' TO OD-Addr

           PERFORM WITH TEST BEFORE 
                   UNTIL Addr-Value = ZERO

      *    *** アドレス16で割る理由、10進数＝＞16進数に変換している、
      *    *** Nibbleは余りなので、添字に使っている

                   DIVIDE Addr-Value BY 16
                          GIVING Addr-Value
                          REMAINDER Nibble

                   ADD 1 TO Nibble
                   MOVE Hex-Digit (Nibble)
                        TO OD-Addr-Hex (Addr-Sub)
                   SUBTRACT 1 FROM Addr-Sub
           END-PERFORM.
       100-EX.
           EXIT.

      *    *** SJIS CHECK

      *    *** SJIS １バイト目からチェック、
      *    *** SJIS なら"1"、
      *    ***  既に"1"の時、"0" に戻す
      *    ***  その他の時、"1"
      *    ***  1234567890123456
      *    ***  漢字  => 1:2 漢字
      *    ***   漢字 => 2:2 漢字となる事もあるため、
      *    *** 漢字２バイト目チェックで、"0"にリセットする
      *    ***  
       S210-10.

      *    *** SJIS 漢字範囲
           IF    ( Buffer (Buffer-Sub:2) >= X"8140" AND 
                   Buffer (Buffer-Sub:2) <= X"9FFC" )   OR
                 ( Buffer (Buffer-Sub:2) >= X"E040" AND 
                   Buffer (Buffer-Sub:2) <= X"EAA4" )
               IF  SW-KANJI = "1"
                   MOVE    "0"       TO      SW-KANJI
               ELSE
                   MOVE    "1"       TO      SW-KANJI
               END-IF
           ELSE
      *    *** SJIS 漢字以外の時、"0"
      *    *** 
                   MOVE    "0"       TO      SW-KANJI
           END-IF
           .
       S210-EX.
           EXIT.
