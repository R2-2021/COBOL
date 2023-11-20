      *    *** COBDUMP TEST,他

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST05.

       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
      *    *** FILE STATUS 無の調査の為、コメントにしてテスト
      *                         STATUS   WK-PIN1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
      *                         STATUS   WK-POT1-STATUS
      *     ORGANIZATION LINE   SEQUENTIAL.
           ORGANIZATION RECORD BINARY   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING IN SIZE FROM 1 TO 1080 
      *     RECORD VARYING IN SIZE TO 100 
      *     RECORD VARYING DEPENDING ON WK-PIN1-LEN.

       01  PIN1-REC.
           03  PIN1-DATA       PIC  X(080).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
      *     RECORD VARYING IN SIZE FROM 1 TO 1080
      *     RECORD VARYING DEPENDING ON WK-POT1-LEN.
       01  POT1-REC.
           03  POT1-DATA       PIC  X(080).

       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST05  ".

      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "COBSAM08.PIN1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST38.PIN1".
      
      *    *** UTF-8 DATA
      *     03  WK-PIN1-F-NAME  PIC  X(032) 
      *         VALUE "youtube.YUIKAORI2.html".
      *    *** S-JIS DATA
           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST38.POT1".
      *     03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST05.PIN1".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST05.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-SIO1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT-E   PIC --,---,---,--9 VALUE ZERO.
           03  WK-POT1-CNT-E   PIC --,---,---,--9 VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-DATA1        PIC  X(005) VALUE "ｱｲｳｴ ".
           03  WK-DATA2        PIC S9(005) VALUE +99999.
           03  WK-DATA3        PIC  X(005) VALUE X"123456789A".
           03  WK-DATA4.
             05  FILLER        PIC  X(003) VALUE ALL "ABC".
             05  FILLER        PIC  X(006) VALUE ALL "あいう".
             05  FILLER        PIC  X(003) VALUE ALL "DEF".
           03  WK-DATA5.
             05  FILLER        PIC  X(005) VALUE ALL "ｱｲｳ".
             05  FILLER        PIC  N(003) VALUE ALL "かきく".
             05  FILLER        PIC  X(003) VALUE ALL "ｻｼｽ".
           03  WK-DATA6.
             05  FILLER        PIC  X(005) VALUE ALL "ｱｲｳ".
             05  FILLER        PIC  N(003) VALUE ALL N"かきく".
             05  FILLER        PIC  X(003) VALUE ALL "ｻｼｽ".
           03  WK-DATA7.
             05  FILLER        PIC  X(005) VALUE ALL "ｱｲｳ".
             05  FILLER        PIC  N(003) VALUE ALL NC"かきく".
             05  FILLER        PIC  X(003) VALUE ALL "ｻｼｽ".
           03  WK-DATA8.
             05  FILLER        PIC  X(005) VALUE ALL "ｱｲｳ".
             05  FILLER        PIC  X(006) VALUE ALL NC"かきく".
             05  FILLER        PIC  X(003) VALUE ALL "ｻｼｽ".
           03  WK-DATA9.
             05  FILLER        PIC  X(020) VALUE ALL "ｱｲｳ".
             05  FILLER        PIC  X(020) VALUE ALL NC"かきく".
           03  WK-DATA10.
             05  FILLER        PIC  X(016) VALUE ALL "ｱｲｳ".
      *    *** アイウエオ
             05  FILLER        PIC  X(015) VALUE 
                 ALL X"E38182E38184E38186E38188E3818A".

           03  WK-TESTNO       PIC  X(002) VALUE ZERO.

       01  WK-COB-OC-DUMP-AREA.
           03  WK-COB-OC-DUMP.
             05  WK-COB-OC-BUFFER PIC X(001) VALUE SPACE.
             05  WK-COB-OC-BYTE   PIC X(001) VALUE SPACE.

       01  WK-LISTING-AREA.
           03  WK-LISTING.
             05  WK-LISTING-SRC   PIC X(001) VALUE " ".
             05  WK-LISTING-XREF  PIC X(001) VALUE "X".
             05  WK-LISTING-FILE  PIC X(256) VALUE
      *        "C:\Users\xxxx\Documents\COBOL\TEST05.CBL".
              "TEST05.CBL".
             05  WK-LISTING-REP-FILE PIC X(256) VALUE SPACE.
      *        "C:\Users\xxxx\Documents\COBOL\COBXREF.lst".
             05  WK-LISTING-POT1-ID.
               07  WK-LISTING-POT1-OPEN  PIC X(001) VALUE "O".
               07  WK-LISTING-POT1-CLOSE PIC X(001) VALUE "C".

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  PIC-XX.
           05  FILLER          PIC X VALUE LOW-VALUE.
           05  PIC-X           PIC X VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX PIC 9(4) COMP-X.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
           03  TBL01-AREA.
             05  TBL01-I1      OCCURS 256
                               PIC  X(001) VALUE SPACE.

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX
  
           DISPLAY "TEST NO ２桁入力"
           DISPLAY "01 COBDUMP SJIS"
           DISPLAY "02 FILEDUMP"
           DISPLAY "03 FILEDUMP,COBDUMP"
           DISPLAY "04 CBL_OC_DUMP SJIS"
           DISPLAY "05 LISTING"
           DISPLAY "06 256 文字チェック"
           DISPLAY "07 FILE READ FILEDUMP UTF-8 文字チェック"
           DISPLAY "08 COBDUMP UTF8"
           ACCEPT  WK-TESTNO

           EVALUATE WK-TESTNO
               WHEN "01"
      *    *** COBDUMP SJIS TEST
                   PERFORM S110-10     THRU    S110-EX
               WHEN "02"
      *    *** FILEDUMP TEST
                   PERFORM S120-10     THRU    S120-EX
               WHEN "03"
      *    *** FILEDUMP,COBDUMP TEST
                   PERFORM S130-10     THRU    S130-EX
               WHEN "04"
      *    *** CBL_OC_DUMP TEST
                   PERFORM S140-10     THRU    S140-EX
               WHEN "05"
      *    *** CALL "LISTING" TEST
                   PERFORM S150-10     THRU    S150-EX
               WHEN "06"
      *    *** 256文字チェック TEST
                   PERFORM S160-10     THRU    S160-EX
               WHEN "07"
      *    *** FILE READ FILEDUMP UTF-8 CHECK TEST
                   PERFORM S170-10     THRU    S170-EX
                            UNTIL WK-PIN1-EOF = HIGH-VALUE
               WHEN "08"
      *    *** COBDUMP UTF8 TEST
                   PERFORM S180-10     THRU    S180-EX
           END-EVALUATE

      *    *** CLOSE
           PERFORM S900-10     THRU    S900-EX
           .
       M100-EX.
           STOP    RUN.

      *    *** OPEN
       S010-10.
           DISPLAY WK-PGM-NAME " START"

           MOVE    WK-PGM-NAME TO      WDT-DATE-TIME-PGM
           MOVE    "S"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA

      *    *** C:\Users\xxxx\OneDrive\ドキュメント\COBOL>TEST05
      *    *** TEST05   START
      *    *** libcob: File does not exist (STATUS = 35) File : 'TEST05.PIN1'
      *    *** FILE STATUS 指定しないと、ファイルない時、このエラーが出る

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "O"         TO      WFD-ID
      *     MOVE   100          TO      WFD-LEN
           MOVE    "M"         TO      WFD-TYPE
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA3
           .
       S010-EX.
           EXIT.

      *    *** 
       S100-10.

      *     MOVE    WK-PIN1-LEN TO      WK-POT1-LEN
      *     MOVE    1080          TO      WK-POT1-LEN
           MOVE    PIN1-DATA   TO      POT1-DATA
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

      *     MOVE    "P"         TO      WFD-ID
      *     MOVE    WK-POT1-CNT TO      WFD-SEQ
      *     MOVE    2           TO      WFD-SU
      *     MOVE    100         TO      WFD-LEN
      *     CALL    "FILEDUMP"   USING  WFD-FILEDUMP-AREA
      *                                 POT1-REC

           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F WRITE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF
           .
       S100-EX.
           EXIT.

      *    *** COBDUMP SJIS TEST
       S110-10.

           CALL    "COBDUMP"   USING   WK-DATA1

      *    *** LEN=0　や　LEN = 項目の長さの時、項目の長さで出力
           MOVE    0           TO      WFD-LEN
           CALL    "COBDUMP"   USING   WK-DATA2
                                       WFD-LEN

           MOVE    5           TO      WFD-LEN
           CALL    "COBDUMP"   USING   WK-DATA2
                                       WFD-LEN

      *    *** LEN < 項目の長さの時、LENの長さで出力　3バイトで出力されてる
           MOVE    3           TO      WFD-LEN
           CALL    "COBDUMP"   USING   WK-DATA2
                                       WFD-LEN

           MOVE    10          TO      WFD-LEN
           CALL    "COBDUMP"   USING   WK-DATA2
                                       WFD-LEN

           CALL    "COBDUMP"   USING   WK-DATA3

           CALL    "COBDUMP"   USING   WK-DATA4

           CALL    "COBDUMP"   USING   WK-DATA5

           CALL    "COBDUMP"   USING   WK-DATA6

           CALL    "COBDUMP"   USING   WK-DATA7

           CALL    "COBDUMP"   USING   WK-DATA8
           .
       S110-EX.
           EXIT.

      *    *** FILEDUMP TEST
       S120-10.

           MOVE    2           TO      WFD-SU
           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA1"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA1

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA2"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA2

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA3"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA3

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA4"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA4

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA5"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA5

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA6"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA6

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA7"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA7

           MOVE    "X"         TO      WFD-ID
           MOVE    "DATA8"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA8
           .
       S120-EX.
           EXIT.

      *    *** FILEDUMP,COBDUMP TEST
       S130-10.

      *    *** ID=P は本来レコード出力用 LEN=100が規定値 指定無しは項目長
           MOVE    "P"         TO      WFD-ID
           MOVE    1           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA1
      *    *** 
           MOVE    "X"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           MOVE    1           TO      WFD-SEQ
           MOVE    "DATA3"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA3
      *    *** 
           MOVE    "X"         TO      WFD-ID
           MOVE    2           TO      WFD-SU
           MOVE    2           TO      WFD-SEQ
           MOVE    "DATA9"     TO      WFD-ITEM
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       WK-DATA9

      *     CALL    "COBDUMP"   USING   TBL01-AREA
      *                                 WFD-LEN
           CALL    "COBDUMP"   USING   WK-DATA4

           CALL    "COBDUMP"   USING   WK-DATA5


           CALL    "COBDUMP"   USING   WK-DATA1 WFD-LEN

           CALL    "COBDUMP"   USING   WK-DATA2 WFD-LEN

           CALL    "COBDUMP"   USING   WK-DATA3 WFD-LEN

           CALL    "COBDUMP"   USING   WK-DATA4 WFD-LEN
           .
       S130-EX.
           EXIT.

      *    *** CBL_OC_DUMP TEST
       S140-10.

           CALL    "CBL_OC_DUMP" USING WK-DATA1

           CALL    "CBL_OC_DUMP" USING WK-DATA2

           CALL    "CBL_OC_DUMP" USING WK-DATA3

           CALL    "CBL_OC_DUMP" USING WK-DATA4

           CALL    "CBL_OC_DUMP" USING WK-DATA5

           CALL    "CBL_OC_DUMP" USING WK-DATA6

           CALL    "CBL_OC_DUMP" USING WK-DATA7

           CALL    "CBL_OC_DUMP" USING WK-DATA8
           .
       S140-EX.
           EXIT.

      *    *** CALL "LISTING" TEST
       S150-10.

           CALL    "LISTING"   USING   WK-LISTING-SRC
                                       WK-LISTING-XREF
                                       WK-LISTING-FILE
                                       WK-LISTING-REP-FILE
                                       WK-LISTING-POT1-ID
           .
       S150-EX.
           EXIT.

      *    *** 256文字チェック TEST
       S160-10.

           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I  > 256
                   MOVE    I           TO      PIC-Halfword
                   ADD     I 1         GIVING  I2

      *          IF    PIC-X >= X"E0" AND PIC-X <= X"EF"
      *                CONTINUE
      *          ELSE
                   MOVE    PIC-X       TO      TBL01-I1 (I2)
      *          END-IF
           END-PERFORM

           CALL    "COBDUMP"   USING   TBL01-AREA

      *    *** 
           MOVE    256         TO      WFD-LEN
           MOVE    "P"         TO      WFD-ID
           MOVE    "M"         TO      WFD-TYPE
           MOVE    2           TO      WFD-SU
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       TBL01-AREA
                                       WFD-LEN
           .
       S160-EX.
           EXIT.

      *    *** FILE READ FILEDUMP UTF-8 CHECK TEST
       S170-10.

      *    *** OPEN 無READ 時のエラー
      *    *** libcob: READ/START not allowed (STATUS = 47) File : 'TEST05.PIN1'

           READ    PIN1-F
               AT  END
      *    *** SELECT ASSIGN でFILE-STATUS 指定しない時は,
      *    *** AT END 句必要
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
      *             CONTINUE
                   ADD     1           TO      WK-PIN1-CNT
           END-READ

      *    *** SELECT ASSIGN でFILE-STATUS 指定時は,
      *    *** READ AT END 不要、WK-PIN1-STATUS コード毎の処理指定する
           IF      WK-PIN1-STATUS =    ZERO OR 4
                   ADD     1           TO      WK-PIN1-CNT
      *             DISPLAY "PIN1-LEN=" WK-PIN1-LEN

               IF WK-PIN1-CNT >= 1 AND <= 10
                   MOVE    "X"         TO      WFD-ID
                   MOVE    "N"         TO      WFD-HED
                   MOVE    WK-PIN1-CNT TO      WFD-SEQ
                   MOVE    2           TO      WFD-SU
                   MOVE    "ITEM-12345" TO     WFD-ITEM
                   MOVE    10          TO      WFD-LEN
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC
                                               PIN1-REC (32:20)
                                               WFD-LEN

                   MOVE    "X"         TO      WFD-ID
                   MOVE    "N"         TO      WFD-HED
                   MOVE    WK-PIN1-CNT TO      WFD-SEQ
                   MOVE    2           TO      WFD-SU
                   MOVE    "ITEM-AAAAA" TO     WFD-ITEM
      *             MOVE    10          TO      WFD-LEN
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                         PIN1-REC
                                               PIN1-REC (1:080)
      *                                         WFD-LEN

                   MOVE    "P"         TO      WFD-ID
                   MOVE    WK-PIN1-CNT TO      WFD-SEQ
                   MOVE    1           TO      WFD-SU
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               PIN1-REC

               ELSE
                   CONTINUE
               END-IF
           ELSE
                   IF      WK-PIN1-STATUS =    10
                           MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
                   ELSE
                           DISPLAY WK-PGM-NAME 
                                   " PIN1-F READ ERROR STATUS="
                                   WK-PIN1-STATUS
                           STOP    RUN
                   END-IF
           END-IF
           .
       S170-EX.
           EXIT.

      *    *** COBDUMP UTF8 TEST
       S180-10.

      *    *** CHCP 65001 (UTF8) で COBDUMP.CBL SPACE クリアーしなくても
      *    *** 漢字表示されない
           CALL    "COBDUMP"   USING   WK-DATA10

      *    *** CHCP 932 で UTF8 含む漢字出力すると、DISPLAY が出力されない
      *    *** CHCP 65001で UTF8 含む漢字出力すると、DISPLAY 内容が文字化け
      *    *** する、改行もおかしくなって、次の行がつながってしまう
           DISPLAY WK-DATA10
           .
       S180-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "C"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           DISPLAY WK-PGM-NAME " END"
           MOVE    WK-PIN1-CNT TO      WK-PIN1-CNT-E
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ = " WK-PIN1-CNT-E
                   " (" WK-PIN1-F-NAME ")"
           MOVE    WK-POT1-CNT TO      WK-POT1-CNT-E
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ = " WK-POT1-CNT-E
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
