       IDENTIFICATION DIVISION.
       PROGRAM-ID. db_fetch_sample01.
       DIVISION. ENVIRONMENT
       DATA DIVISION.
      ***
       WORKING-STORAGE SECTION.
      * 1. ホスト変数の定義
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME PIC X(32) VALUE "testdb".
       01 USERNAME PIC X(32) VALUE "********".
       01 PASSWORD PIC X(32) VALUE "********".
       01 TID PIC X(4).
       01 TNAME PIC X(10).
       01 SW-AREA. 03 SW-NOTFOUND PIC X(01) VALUE SPACE.
       01 CST-AREA.
         03 CST-1X PIC X(01) VALUE "1".
         03 CST-SQL-NF PIC S9(09) COMP-5 VALUE +100.
           EXEC SQL END DECLARE SECTION END-EXEC.

      * 2.共通領域の定義
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
       MAIN-RTN. SECTION.
      * 3. データベース接続

           EXEC SQL
             CONNECT :USERNAME IDENTIFIED BY :PASSWORD
             USING :DBNAME
           END-EXEC.

      * 4.データベースアクセス
      *** DB カーソルオープン処理
           PERFORM OPEN-RTN.
      *** DBFETCH処理
           PERFORM FETCH-RTN UNTIL SW-NOTFOUND = CST-1X.
      *** DB カーソルクローズ処理 PERFORM CLOSE-RTN.
           STOP RUN.
       MAIN-EXIT.
           EXIT.

      *** DB カーソルオープン処理
       OPEN-RTN SECTION.
           EXEC SQL
             DECLARE CSR01 CURSOR FOR
             SELECT tid, tname
             FROM test
           END-EXEC.

           EXEC SQL
             OPEN CSR01
           END-EXEC.
       OPEN-EXIT.
           EXIT.

      *** DB FETCH処理
       FETCH-RTN SECTION.
           EXEC SQL
             FETCH CSR01
             INTO : TID,:TNAME
           END-EXEC.
      *
           IF SQLCODE = CST-SQL-NF
           THEN
               MOVE CST-1X TO SW-NOTFOUND
           ELSE
               DISPLAY TID TNAME
           END-IF.
       FETCH-EXIT.
           EXIT.
      *** DB カーソルクローズ処理
       CLOSE-RTN SECTION.
           EXEC SQL
             CLOSE CSR01
           END-EXEC.
       CLOSE-EXIT.
       END PROGRAM db_fetch_sample01.
