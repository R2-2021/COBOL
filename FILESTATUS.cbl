      *    *** FILESTATUS 内容表示

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILESTATUS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WORK-AREA.
           05  WK-STATUS       PIC  9(002) VALUE ZERO.
           05  WK-MSG          PIC  X(041) VALUE SPACE.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           05  SW-X            PIC  X(001) VALUE ZERO.

      * LINKAGE SECTION.
      * 01  LINK-AREA.
      *     03  LA-STATUS       PIC  9(002).
      *     03  WK-MSG          PIC  X(041).

      * PROCEDURE DIVISION USING LINK-AREA.
      *    *** サブルーチンからメインに変更する、
      *    *** ステータス入力して、内容表示
       PROCEDURE DIVISION .
       000-FILESTATUS.
           DISPLAY 'FILESTATUS 入力（数字２桁）？'
           ACCEPT  WK-STATUS FROM CONSOLE

           MOVE    ALL "?"     TO      WK-MSG
           EVALUATE WK-STATUS
             WHEN 00 MOVE ' SUCCESS 成功'           TO WK-MSG
             WHEN 02 MOVE ' SUCCESS  (DUPLICATE RECORD KEY WRITTEN) '
                     TO WK-MSG
             WHEN 04 MOVE ' SUCCESS INCOMPLETE '    TO WK-MSG
             WHEN 05 MOVE ' SUCCESS  (OPTIONAL FILE NOT FOUND) '
                     TO WK-MSG
             WHEN 07 MOVE ' SUCCESS (NO UNIT) '       TO WK-MSG
             WHEN 10 MOVE ' END OF FILE '           TO WK-MSG
             WHEN 14 MOVE ' OUT OF KEY RANGE '      TO WK-MSG
             WHEN 21 MOVE ' KEY INVALID '           TO WK-MSG
             WHEN 22 MOVE ' ATTEMPT TO DUPLICATE KEY VALUE '
                     TO WK-MSG
             WHEN 23 MOVE ' KEY NOT FOUND '        TO WK-MSG
             WHEN 30 MOVE ' PERMANENT I/O ERROR '       TO WK-MSG
             WHEN 31 MOVE ' INCONSISTENT FILENAME ' TO WK-MSG
             WHEN 34 MOVE ' BOUNDARY VIOLATION '    TO WK-MSG
             WHEN 35 MOVE ' FILE NOT FOUND '        TO WK-MSG
             WHEN 37 MOVE ' PERMISSION DENIED 使用中?'TO WK-MSG
             WHEN 38 MOVE ' CLOSED WITH LOCK '      TO WK-MSG
             WHEN 39 MOVE ' CONFLICTING ATTRIBUTE '    TO WK-MSG
             WHEN 41 MOVE ' FILE ALREADY OPEN ２重OPEN'  TO WK-MSG
             WHEN 42 MOVE ' FILE NOT OPEN 未OPEN'        TO WK-MSG
             WHEN 43 MOVE ' READ NOT DONE '         TO WK-MSG
             WHEN 44 MOVE ' RECORD OVERFLOW '       TO WK-MSG
             WHEN 46 MOVE ' READ ERROR '            TO WK-MSG
             WHEN 47 MOVE ' OPEN INPUT DENIED 未OPEN ?'  TO WK-MSG
             WHEN 48 MOVE ' OPEN OUTPUT DENIED 未OPEN ?' TO WK-MSG
             WHEN 49 MOVE ' OPEN I/O DENIED 未OPEN ?'    TO WK-MSG
             WHEN 51 MOVE ' RECORD LOCKED '         TO WK-MSG
             WHEN 52 MOVE ' END OF PAGE '           TO WK-MSG
             WHEN 57 MOVE ' LINAGE SPECIFICATIONS INVALID '
                     TO WK-MSG
             WHEN 61 MOVE ' FILE SHARING FAILURE '  TO WK-MSG
             WHEN 91 MOVE ' FILE NOT AVAILABLE '    TO WK-MSG
           END-EVALUATE. 

           DISPLAY WK-STATUS " : " WK-MSG
           .
       EXIT PROGRAM.
           EXIT.
