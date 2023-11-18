      *    *** ＸＶＩ２　ｔａｇデータ作成
      *    *** 
      *    *** TEST78
      *    ***   |
      *    *** TEST53
      *    ***   |
      *    *** TEST54

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             TEST78.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** XVI tag のページで必要部分をコピーし、Excelの貼り付けで
      *    *** 元の書式で貼り付けを選ぶ、カンマがある時、””で囲まれる
      *    *** 項目と数字の間は、”？”になる

      *    *** CSVデータ
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN2-F           ASSIGN   WK-PIN2-F-NAME
                               STATUS   WK-PIN2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN3-F           ASSIGN   WK-PIN3-F-NAME
                               STATUS   WK-PIN3-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN4-F           ASSIGN   WK-PIN4-F-NAME
                               STATUS   WK-PIN4-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN5-F           ASSIGN   WK-PIN5-F-NAME
                               STATUS   WK-PIN5-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN6-F           ASSIGN   WK-PIN6-F-NAME
                               STATUS   WK-PIN6-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN7-F           ASSIGN   WK-PIN7-F-NAME
                               STATUS   WK-PIN7-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN8-F           ASSIGN   WK-PIN8-F-NAME
                               STATUS   WK-PIN8-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN9-F           ASSIGN   WK-PIN9-F-NAME
                               STATUS   WK-PIN9-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN10-F          ASSIGN   WK-PIN10-F-NAME
                               STATUS   WK-PIN10-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN11-F          ASSIGN   WK-PIN11-F-NAME
                               STATUS   WK-PIN11-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN12-F          ASSIGN   WK-PIN12-F-NAME
                               STATUS   WK-PIN12-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN13-F          ASSIGN   WK-PIN13-F-NAME
                               STATUS   WK-PIN13-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN14-F          ASSIGN   WK-PIN14-F-NAME
                               STATUS   WK-PIN14-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN15-F          ASSIGN   WK-PIN15-F-NAME
                               STATUS   WK-PIN15-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN16-F          ASSIGN   WK-PIN16-F-NAME
                               STATUS   WK-PIN16-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN17-F          ASSIGN   WK-PIN17-F-NAME
                               STATUS   WK-PIN17-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN18-F          ASSIGN   WK-PIN18-F-NAME
                               STATUS   WK-PIN18-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN19-F          ASSIGN   WK-PIN19-F-NAME
                               STATUS   WK-PIN19-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN20-F          ASSIGN   WK-PIN20-F-NAME
                               STATUS   WK-PIN20-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN21-F          ASSIGN   WK-PIN21-F-NAME
                               STATUS   WK-PIN21-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN22-F          ASSIGN   WK-PIN22-F-NAME
                               STATUS   WK-PIN22-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN23-F          ASSIGN   WK-PIN23-F-NAME
                               STATUS   WK-PIN23-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN24-F          ASSIGN   WK-PIN24-F-NAME
                               STATUS   WK-PIN24-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN25-F          ASSIGN   WK-PIN25-F-NAME
                               STATUS   WK-PIN25-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN26-F          ASSIGN   WK-PIN26-F-NAME
                               STATUS   WK-PIN26-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN27-F          ASSIGN   WK-PIN27-F-NAME
                               STATUS   WK-PIN27-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT PIN28-F          ASSIGN   WK-PIN28-F-NAME
                               STATUS   WK-PIN28-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

      *    *** CSVデータ
       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03                  PIC  X(1000).

       FD  PIN2-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN2-LEN.
       01  PIN2-REC.
           03                  PIC  X(1000).

       FD  PIN3-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN3-LEN.
       01  PIN3-REC.
           03                  PIC  X(1000).

       FD  PIN4-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN4-LEN.
       01  PIN4-REC.
           03                  PIC  X(1000).

       FD  PIN5-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN5-LEN.
       01  PIN5-REC.
           03                  PIC  X(1000).

       FD  PIN6-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN6-LEN.
       01  PIN6-REC.
           03                  PIC  X(1000).

       FD  PIN7-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN7-LEN.
       01  PIN7-REC.
           03                  PIC  X(1000).

       FD  PIN8-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN8-LEN.
       01  PIN8-REC.
           03                  PIC  X(1000).

       FD  PIN9-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN9-LEN.
       01  PIN9-REC.
           03                  PIC  X(1000).

       FD  PIN10-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN10-LEN.
       01  PIN10-REC.
           03                  PIC  X(1000).

       FD  PIN11-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN11-LEN.
       01  PIN11-REC.
           03                  PIC  X(1000).

       FD  PIN12-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN12-LEN.
       01  PIN12-REC.
           03                  PIC  X(1000).

       FD  PIN13-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN13-LEN.
       01  PIN13-REC.
           03                  PIC  X(1000).

       FD  PIN14-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN14-LEN.
       01  PIN14-REC.
           03                  PIC  X(1000).

       FD  PIN15-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN15-LEN.
       01  PIN15-REC.
           03                  PIC  X(1000).

       FD  PIN16-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN16-LEN.
       01  PIN16-REC.
           03                  PIC  X(1000).

       FD  PIN17-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN17-LEN.
       01  PIN17-REC.
           03                  PIC  X(1000).

       FD  PIN18-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN18-LEN.
       01  PIN18-REC.
           03                  PIC  X(1000).

       FD  PIN19-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN19-LEN.
       01  PIN19-REC.
           03                  PIC  X(1000).

       FD  PIN20-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN20-LEN.
       01  PIN20-REC.
           03                  PIC  X(1000).

       FD  PIN21-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN21-LEN.
       01  PIN21-REC.
           03                  PIC  X(1000).

       FD  PIN22-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN22-LEN.
       01  PIN22-REC.
           03                  PIC  X(1000).

       FD  PIN23-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN23-LEN.
       01  PIN23-REC.
           03                  PIC  X(1000).

       FD  PIN24-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN24-LEN.
       01  PIN24-REC.
           03                  PIC  X(1000).

       FD  PIN25-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN25-LEN.
       01  PIN25-REC.
           03                  PIC  X(1000).

       FD  PIN26-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN26-LEN.
       01  PIN26-REC.
           03                  PIC  X(1000).

       FD  PIN27-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN27-LEN.
       01  PIN27-REC.
           03                  PIC  X(1000).

       FD  PIN28-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN28-LEN.
       01  PIN28-REC.
           03                  PIC  X(1000).

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC.
           03  FILLER          PIC  X(1000).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "TEST78  ".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "TEST78.tag_all.csv".
           03  WK-PIN2-F-NAME  PIC  X(032) VALUE "TEST78.tag_kigou.csv".
           03  WK-PIN3-F-NAME  PIC  X(032) VALUE "TEST78.tag_a.csv".
           03  WK-PIN4-F-NAME  PIC  X(032) VALUE "TEST78.tag_b.csv".
           03  WK-PIN5-F-NAME  PIC  X(032) VALUE "TEST78.tag_c.csv".
           03  WK-PIN6-F-NAME  PIC  X(032) VALUE "TEST78.tag_d.csv".
           03  WK-PIN7-F-NAME  PIC  X(032) VALUE "TEST78.tag_e.csv".
           03  WK-PIN8-F-NAME  PIC  X(032) VALUE "TEST78.tag_f.csv".
           03  WK-PIN9-F-NAME  PIC  X(032) VALUE "TEST78.tag_g.csv".
           03  WK-PIN10-F-NAME PIC  X(032) VALUE "TEST78.tag_h.csv".
           03  WK-PIN11-F-NAME PIC  X(032) VALUE "TEST78.tag_i.csv".
           03  WK-PIN12-F-NAME PIC  X(032) VALUE "TEST78.tag_j.csv".
           03  WK-PIN13-F-NAME PIC  X(032) VALUE "TEST78.tag_k.csv".
           03  WK-PIN14-F-NAME PIC  X(032) VALUE "TEST78.tag_l.csv".
           03  WK-PIN15-F-NAME PIC  X(032) VALUE "TEST78.tag_m.csv".
           03  WK-PIN16-F-NAME PIC  X(032) VALUE "TEST78.tag_n.csv".
           03  WK-PIN17-F-NAME PIC  X(032) VALUE "TEST78.tag_o.csv".
           03  WK-PIN18-F-NAME PIC  X(032) VALUE "TEST78.tag_p.csv".
           03  WK-PIN19-F-NAME PIC  X(032) VALUE "TEST78.tag_q.csv".
           03  WK-PIN20-F-NAME PIC  X(032) VALUE "TEST78.tag_r.csv".
           03  WK-PIN21-F-NAME PIC  X(032) VALUE "TEST78.tag_s.csv".
           03  WK-PIN22-F-NAME PIC  X(032) VALUE "TEST78.tag_t.csv".
           03  WK-PIN23-F-NAME PIC  X(032) VALUE "TEST78.tag_u.csv".
           03  WK-PIN24-F-NAME PIC  X(032) VALUE "TEST78.tag_v.csv".
           03  WK-PIN25-F-NAME PIC  X(032) VALUE "TEST78.tag_w.csv".
           03  WK-PIN26-F-NAME PIC  X(032) VALUE "TEST78.tag_x.csv".
           03  WK-PIN27-F-NAME PIC  X(032) VALUE "TEST78.tag_y.csv".
           03  WK-PIN28-F-NAME PIC  X(032) VALUE "TEST78.tag_z.csv".
           03  WK-POT1-F-NAME  PIC  X(032) VALUE "TEST78.POT1".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN2-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN3-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN4-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN5-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN6-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN7-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN8-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN9-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-PIN10-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN11-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN12-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN13-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN14-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN15-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN16-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN17-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN18-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN19-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN20-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN21-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN22-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN23-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN24-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN25-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN26-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN27-STATUS PIC  9(002) VALUE ZERO.
           03  WK-PIN28-STATUS PIC  9(002) VALUE ZERO.
           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN2-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN3-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN4-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN5-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN6-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN7-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN8-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN9-EOF     PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN10-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN11-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN12-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN13-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN14-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN15-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN16-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN17-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN18-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN19-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN20-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN21-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN22-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN23-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN24-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN25-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN26-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN27-EOF    PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIN28-EOF    PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN6-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN7-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN8-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN9-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN10-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN11-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN12-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN13-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN14-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN15-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN16-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN17-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN18-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN19-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN20-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN21-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN22-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN23-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN24-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN25-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN26-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN27-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN28-CNT    BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN2-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN3-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN4-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN5-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN6-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN7-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN8-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN9-LEN     BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN10-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN11-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN12-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN13-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN14-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN15-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN16-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN17-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN18-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN19-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN20-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN21-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN22-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN23-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN24-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN25-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN26-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN27-LEN    BINARY-LONG SYNC VALUE ZERO.
           03  WK-PIN28-LEN    BINARY-LONG SYNC VALUE ZERO.

           03  WK-SU           PIC  X(010) VALUE SPACE.
           03  WK-SU2          PIC  9(010) VALUE ZERO.
      *     03  WK-LIMIT        PIC  9(010) VALUE 100000.
           03  WK-LIMIT        PIC  9(010) VALUE 200000.
           03  WK-REC          PIC  X(1000) VALUE SPACE.
           03  WK-LEN          BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP  REPLACING ==:##:== BY ==WFD==.

           COPY    CPDATETIME  REPLACING ==:##:== BY ==WDT==.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE 1.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  I4              BINARY-LONG SYNC VALUE 1.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-SEARCH       PIC  X(001) VALUE "N".

       PROCEDURE               DIVISION.
       M100-10.

      *    *** OPEN
           PERFORM S010-10     THRU    S010-EX

      *    *** READ PIN1 - PIN28
           PERFORM S021-10     THRU    S021-EX
           PERFORM S022-10     THRU    S022-EX
           PERFORM S023-10     THRU    S023-EX
           PERFORM S024-10     THRU    S024-EX
           PERFORM S025-10     THRU    S025-EX
           PERFORM S026-10     THRU    S026-EX
           PERFORM S027-10     THRU    S027-EX
           PERFORM S028-10     THRU    S028-EX
           PERFORM S029-10     THRU    S029-EX
           PERFORM S030-10     THRU    S030-EX
           PERFORM S031-10     THRU    S031-EX
           PERFORM S032-10     THRU    S032-EX
           PERFORM S033-10     THRU    S033-EX
           PERFORM S034-10     THRU    S034-EX
           PERFORM S035-10     THRU    S035-EX
           PERFORM S036-10     THRU    S036-EX
           PERFORM S037-10     THRU    S037-EX
           PERFORM S038-10     THRU    S038-EX
           PERFORM S039-10     THRU    S039-EX
           PERFORM S040-10     THRU    S040-EX
           PERFORM S041-10     THRU    S041-EX
           PERFORM S042-10     THRU    S042-EX
           PERFORM S043-10     THRU    S043-EX
           PERFORM S044-10     THRU    S044-EX
           PERFORM S045-10     THRU    S045-EX
           PERFORM S046-10     THRU    S046-EX
           PERFORM S047-10     THRU    S047-EX
           PERFORM S048-10     THRU    S048-EX

           PERFORM UNTIL WK-PIN1-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S101-10     THRU    S101-EX

      *    *** READ PIN1
                   PERFORM S021-10     THRU    S021-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN2-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S102-10     THRU    S102-EX

      *    *** READ PIN1
                   PERFORM S022-10     THRU    S022-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN3-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S103-10     THRU    S103-EX

      *    *** READ PIN1
                   PERFORM S023-10     THRU    S023-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN4-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S104-10     THRU    S104-EX

      *    *** READ PIN1
                   PERFORM S024-10     THRU    S024-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN5-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S105-10     THRU    S105-EX

      *    *** READ PIN1
                   PERFORM S025-10     THRU    S025-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN6-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S106-10     THRU    S106-EX

      *    *** READ PIN1
                   PERFORM S026-10     THRU    S026-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN7-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S107-10     THRU    S107-EX

      *    *** READ PIN1
                   PERFORM S027-10     THRU    S027-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN8-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S108-10     THRU    S108-EX

      *    *** READ PIN1
                   PERFORM S028-10     THRU    S028-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN9-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S109-10     THRU    S109-EX

      *    *** READ PIN1
                   PERFORM S029-10     THRU    S029-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN10-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S110-10     THRU    S110-EX

      *    *** READ PIN1
                   PERFORM S030-10     THRU    S030-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN11-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S111-10     THRU    S111-EX

      *    *** READ PIN1
                   PERFORM S031-10     THRU    S031-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN12-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S112-10     THRU    S112-EX

      *    *** READ PIN1
                   PERFORM S032-10     THRU    S032-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN13-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S113-10     THRU    S113-EX

      *    *** READ PIN1
                   PERFORM S033-10     THRU    S033-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN14-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S114-10     THRU    S114-EX

      *    *** READ PIN1
                   PERFORM S034-10     THRU    S034-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN15-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S115-10     THRU    S115-EX

      *    *** READ PIN1
                   PERFORM S035-10     THRU    S035-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN16-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S116-10     THRU    S116-EX

      *    *** READ PIN1
                   PERFORM S036-10     THRU    S036-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN17-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S117-10     THRU    S117-EX

      *    *** READ PIN1
                   PERFORM S037-10     THRU    S037-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN18-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S118-10     THRU    S118-EX

      *    *** READ PIN1
                   PERFORM S038-10     THRU    S038-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN19-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S119-10     THRU    S119-EX

      *    *** READ PIN1
                   PERFORM S039-10     THRU    S039-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN20-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S120-10     THRU    S120-EX

      *    *** READ PIN1
                   PERFORM S040-10     THRU    S040-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN21-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S121-10     THRU    S121-EX

      *    *** READ PIN1
                   PERFORM S041-10     THRU    S041-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN22-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S122-10     THRU    S122-EX

      *    *** READ PIN1
                   PERFORM S042-10     THRU    S042-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN23-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S123-10     THRU    S123-EX

      *    *** READ PIN1
                   PERFORM S043-10     THRU    S043-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN24-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S124-10     THRU    S124-EX

      *    *** READ PIN1
                   PERFORM S044-10     THRU    S044-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN25-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S125-10     THRU    S125-EX

      *    *** READ PIN1
                   PERFORM S045-10     THRU    S045-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN26-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S126-10     THRU    S126-EX

      *    *** READ PIN1
                   PERFORM S046-10     THRU    S046-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN27-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S127-10     THRU    S127-EX

      *    *** READ PIN1
                   PERFORM S047-10     THRU    S047-EX
           END-PERFORM

           PERFORM UNTIL WK-PIN28-EOF = HIGH-VALUE

      *    *** WRITE POT1
                   PERFORM S128-10     THRU    S128-EX

      *    *** READ PIN1
                   PERFORM S048-10     THRU    S048-EX
           END-PERFORM

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

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F OPEN ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F OPEN ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F OPEN ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F OPEN ERROR STATUS="
                           WK-PIN5-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN6-F
           IF      WK-PIN6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN6-F OPEN ERROR STATUS="
                           WK-PIN6-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN7-F
           IF      WK-PIN7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN7-F OPEN ERROR STATUS="
                           WK-PIN7-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN8-F
           IF      WK-PIN8-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN8-F OPEN ERROR STATUS="
                           WK-PIN8-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN9-F
           IF      WK-PIN9-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN9-F OPEN ERROR STATUS="
                           WK-PIN9-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN10-F
           IF      WK-PIN10-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN10-F OPEN ERROR STATUS="
                           WK-PIN10-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN11-F
           IF      WK-PIN11-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN11-F OPEN ERROR STATUS="
                           WK-PIN11-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN12-F
           IF      WK-PIN12-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN12-F OPEN ERROR STATUS="
                           WK-PIN12-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN13-F
           IF      WK-PIN13-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN13-F OPEN ERROR STATUS="
                           WK-PIN13-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN14-F
           IF      WK-PIN14-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN14-F OPEN ERROR STATUS="
                           WK-PIN14-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN15-F
           IF      WK-PIN15-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN15-F OPEN ERROR STATUS="
                           WK-PIN15-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN16-F
           IF      WK-PIN16-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN16-F OPEN ERROR STATUS="
                           WK-PIN16-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN17-F
           IF      WK-PIN17-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN17-F OPEN ERROR STATUS="
                           WK-PIN17-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN18-F
           IF      WK-PIN18-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN18-F OPEN ERROR STATUS="
                           WK-PIN18-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN19-F
           IF      WK-PIN19-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN19-F OPEN ERROR STATUS="
                           WK-PIN9-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN20-F
           IF      WK-PIN20-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN20-F OPEN ERROR STATUS="
                           WK-PIN20-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN21-F
           IF      WK-PIN21-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN21-F OPEN ERROR STATUS="
                           WK-PIN21-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN22-F
           IF      WK-PIN22-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN22-F OPEN ERROR STATUS="
                           WK-PIN22-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN23-F
           IF      WK-PIN23-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN23-F OPEN ERROR STATUS="
                           WK-PIN23-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN24-F
           IF      WK-PIN24-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN24-F OPEN ERROR STATUS="
                           WK-PIN24-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN25-F
           IF      WK-PIN25-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN25-F OPEN ERROR STATUS="
                           WK-PIN25-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN26-F
           IF      WK-PIN26-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN26-F OPEN ERROR STATUS="
                           WK-PIN26-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN27-F
           IF      WK-PIN27-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN27-F OPEN ERROR STATUS="
                           WK-PIN27-STATUS
                   STOP    RUN
           END-IF

           OPEN    INPUT       PIN28-F
           IF      WK-PIN28-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN28-F OPEN ERROR STATUS="
                           WK-PIN28-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           MOVE    "% XVI2,"   TO      POT1-REC
           WRITE   POT1-REC
           ADD     1           TO      WK-POT1-CNT

           MOVE    "O"         TO      WFD-ID
           CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                       POT1-REC

           MOVE    SPACE       TO      POT1-REC
      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

       S021-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN1-CNT
           END-READ
           .
       S021-EX.
           EXIT.

       S022-10.

           READ    PIN2-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN2-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN2-CNT
           END-READ
           .
       S022-EX.
           EXIT.

       S023-10.

           READ    PIN3-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN3-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN3-CNT
           END-READ
           .
       S023-EX.
           EXIT.

       S024-10.

           READ    PIN4-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN4-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN4-CNT
           END-READ
           .
       S024-EX.
           EXIT.

       S025-10.

           READ    PIN5-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN5-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN5-CNT
           END-READ
           .
       S025-EX.
           EXIT.

       S026-10.

           READ    PIN6-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN6-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN6-CNT
           END-READ
           .
       S026-EX.
           EXIT.

       S027-10.

           READ    PIN7-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN7-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN7-CNT
           END-READ
           .
       S027-EX.
           EXIT.

       S028-10.

           READ    PIN8-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN8-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN8-CNT
           END-READ
           .
       S028-EX.
           EXIT.

       S029-10.

           READ    PIN9-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN9-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN9-CNT
           END-READ
           .
       S029-EX.
           EXIT.

       S030-10.

           READ    PIN10-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN10-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN10-CNT
           END-READ
           .
       S030-EX.
           EXIT.

       S031-10.

           READ    PIN11-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN11-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN11-CNT
           END-READ
           .
       S031-EX.
           EXIT.

       S032-10.

           READ    PIN12-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN12-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN12-CNT
           END-READ
           .
       S032-EX.
           EXIT.

       S033-10.

           READ    PIN13-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN13-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN13-CNT
           END-READ
           .
       S033-EX.
           EXIT.

       S034-10.

           READ    PIN14-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN14-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN14-CNT
           END-READ
           .
       S034-EX.
           EXIT.

       S035-10.

           READ    PIN15-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN15-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN15-CNT
           END-READ
           .
       S035-EX.
           EXIT.

       S036-10.

           READ    PIN16-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN16-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN16-CNT
           END-READ
           .
       S036-EX.
           EXIT.

       S037-10.

           READ    PIN17-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN17-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN17-CNT
           END-READ
           .
       S037-EX.
           EXIT.

       S038-10.

           READ    PIN18-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN18-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN18-CNT
           END-READ
           .
       S038-EX.
           EXIT.

       S039-10.

           READ    PIN19-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN19-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN19-CNT
           END-READ
           .
       S039-EX.
           EXIT.

       S040-10.

           READ    PIN20-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN20-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN20-CNT
           END-READ
           .
       S040-EX.
           EXIT.

       S041-10.

           READ    PIN21-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN21-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN21-CNT
           END-READ
           .
       S041-EX.
           EXIT.

       S042-10.

           READ    PIN22-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN22-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN22-CNT
           END-READ
           .
       S042-EX.
           EXIT.

       S043-10.

           READ    PIN23-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN23-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN23-CNT
           END-READ
           .
       S043-EX.
           EXIT.

       S044-10.

           READ    PIN24-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN24-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN24-CNT
           END-READ
           .
       S044-EX.
           EXIT.

       S045-10.

           READ    PIN25-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN25-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN25-CNT
           END-READ
           .
       S045-EX.
           EXIT.

       S046-10.

           READ    PIN26-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN26-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN26-CNT
           END-READ
           .
       S046-EX.
           EXIT.

       S047-10.

           READ    PIN27-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN27-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN27-CNT
           END-READ
           .
       S047-EX.
           EXIT.

       S048-10.

           READ    PIN28-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN28-EOF
               NOT AT END
                   ADD     1           TO      WK-PIN28-CNT
           END-READ
           .
       S048-EX.
           EXIT.

      *    *** WRITE POT1
       S101-10.

           IF      WK-PIN1-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-all" TO     POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN1-LEN
                      OR PIN1-REC (I:1) = "?"
                   IF      PIN1-REC (I:1) NOT = '"'
                           MOVE    PIN1-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN1-REC (I:1) =    "?"
                   MOVE    PIN1-REC    TO      WK-REC
                   MOVE    WK-PIN1-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S101-EX.
           EXIT.

      *    *** WRITE POT1
       S102-10.

           IF      WK-PIN2-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-kigou" TO   POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN2-LEN
                      OR PIN2-REC (I:1) = "?"
                   IF      PIN2-REC (I:1) NOT = '"'
                           MOVE    PIN2-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN2-REC (I:1) =    "?"
                   MOVE    PIN2-REC    TO      WK-REC
                   MOVE    WK-PIN2-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S102-EX.
           EXIT.

      *    *** WRITE POT1
       S103-10.

           IF      WK-PIN3-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-a"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN3-LEN
                      OR PIN3-REC (I:1) = "?"
                   IF      PIN3-REC (I:1) NOT = '"'
                           MOVE    PIN3-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN3-REC (I:1) =    "?"
                   MOVE    PIN3-REC    TO      WK-REC
                   MOVE    WK-PIN3-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S103-EX.
           EXIT.

      *    *** WRITE POT1
       S104-10.

           IF      WK-PIN4-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-b"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN4-LEN
                      OR PIN4-REC (I:1) = "?"
                   IF      PIN4-REC (I:1) NOT = '"'
                           MOVE    PIN4-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN4-REC (I:1) =    "?"
                   MOVE    PIN4-REC    TO      WK-REC
                   MOVE    WK-PIN4-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S104-EX.
           EXIT.

      *    *** WRITE POT1
       S105-10.

           IF      WK-PIN5-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-c"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN5-LEN
                      OR PIN5-REC (I:1) = "?"
                   IF      PIN5-REC (I:1) NOT = '"'
                           MOVE    PIN5-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN5-REC (I:1) =    "?"
                   MOVE    PIN5-REC    TO      WK-REC
                   MOVE    WK-PIN5-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S105-EX.
           EXIT.

      *    *** WRITE POT1
       S106-10.

           IF      WK-PIN6-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-d"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN6-LEN
                      OR PIN6-REC (I:1) = "?"
                   IF      PIN6-REC (I:1) NOT = '"'
                           MOVE    PIN6-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN6-REC (I:1) =    "?"
                   MOVE    PIN6-REC    TO      WK-REC
                   MOVE    WK-PIN6-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S106-EX.
           EXIT.

      *    *** WRITE POT1
       S107-10.

           IF      WK-PIN7-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-e"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN7-LEN
                      OR PIN7-REC (I:1) = "?"
                   IF      PIN7-REC (I:1) NOT = '"'
                           MOVE    PIN7-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN7-REC (I:1) =    "?"
                   MOVE    PIN7-REC    TO      WK-REC
                   MOVE    WK-PIN7-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S107-EX.
           EXIT.

      *    *** WRITE POT1
       S108-10.

           IF      WK-PIN8-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-f"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN8-LEN
                      OR PIN8-REC (I:1) = "?"
                   IF      PIN8-REC (I:1) NOT = '"'
                           MOVE    PIN8-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN8-REC (I:1) =    "?"
                   MOVE    PIN8-REC    TO      WK-REC
                   MOVE    WK-PIN8-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

      *    *** WK-SU に　”？”が２つ以上入ると、WK-LIMIT を壊していた
      *    *** IF  NUMVAL(WK-SU) >=    WK-LIMIT の判定がエラーになった

      *     MOVE  SPACE TO WK-SU (1:1)
      *     IF PIN8-REC (1:04) = "fake" OR
      *        PIN8-REC (1:06) = "family"
      *         DISPLAY PIN8-REC (1:30) " WK-SU=" WK-SU 
      *                 " I=" I " I2=" I2
      *         IF      NUMVAL(WK-SU) >=    WK-LIMIT
      *             DISPLAY "NUMVAL(WK-SU)=THEN WK-LIMIT=" WK-LIMIT 
      *         ELSE
      *             DISPLAY "NUMVAL(WK-SU)=ELSE WK-LIMIT=" WK-LIMIT 
      *         END-IF
      *     END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT

                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S108-EX.
           EXIT.

      *    *** WRITE POT1
       S109-10.

           IF      WK-PIN9-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-g"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN9-LEN
                      OR PIN9-REC (I:1) = "?"
                   IF      PIN9-REC (I:1) NOT = '"'
                           MOVE    PIN9-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN9-REC (I:1) =    "?"
                   MOVE    PIN9-REC    TO      WK-REC
                   MOVE    WK-PIN9-LEN TO      WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S109-EX.
           EXIT.

      *    *** WRITE POT1
       S110-10.

           IF      WK-PIN10-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-h"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN10-LEN
                      OR PIN10-REC (I:1) = "?"
                   IF      PIN10-REC (I:1) NOT = '"'
                           MOVE    PIN10-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN10-REC (I:1) =   "?"
                   MOVE    PIN10-REC    TO     WK-REC
                   MOVE    WK-PIN10-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S110-EX.
           EXIT.

      *    *** WRITE POT1
       S111-10.

           IF      WK-PIN11-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-i"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN11-LEN
                      OR PIN11-REC (I:1) = "?"
                   IF      PIN11-REC (I:1) NOT = '"'
                           MOVE    PIN11-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN11-REC (I:1) =   "?"
                   MOVE    PIN11-REC    TO     WK-REC
                   MOVE    WK-PIN11-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S111-EX.
           EXIT.

      *    *** WRITE POT1
       S112-10.

           IF      WK-PIN12-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-j"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN12-LEN
                      OR PIN12-REC (I:1) = "?"
                   IF      PIN12-REC (I:1) NOT = '"'
                           MOVE    PIN12-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN12-REC (I:1) =   "?"
                   MOVE    PIN12-REC    TO     WK-REC
                   MOVE    WK-PIN12-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S112-EX.
           EXIT.

      *    *** WRITE POT1
       S113-10.

           IF      WK-PIN13-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-k"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN13-LEN
                      OR PIN13-REC (I:1) = "?"
                   IF      PIN13-REC (I:1) NOT = '"'
                           MOVE    PIN13-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN13-REC (I:1) =   "?"
                   MOVE    PIN13-REC    TO     WK-REC
                   MOVE    WK-PIN13-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S113-EX.
           EXIT.

      *    *** WRITE POT1
       S114-10.

           IF      WK-PIN14-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-l"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN14-LEN
                      OR PIN14-REC (I:1) = "?"
                   IF      PIN14-REC (I:1) NOT = '"'
                           MOVE    PIN14-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN14-REC (I:1) =   "?"
                   MOVE    PIN14-REC    TO     WK-REC
                   MOVE    WK-PIN14-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S114-EX.
           EXIT.

      *    *** WRITE POT1
       S115-10.

           IF      WK-PIN15-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-m"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN15-LEN
                      OR PIN15-REC (I:1) = "?"
                   IF      PIN15-REC (I:1) NOT = '"'
                           MOVE    PIN15-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN15-REC (I:1) =   "?"
                   MOVE    PIN15-REC    TO     WK-REC
                   MOVE    WK-PIN15-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S115-EX.
           EXIT.

      *    *** WRITE POT1
       S116-10.

           IF      WK-PIN16-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-n"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN16-LEN
                      OR PIN16-REC (I:1) = "?"
                   IF      PIN16-REC (I:1) NOT = '"'
                           MOVE    PIN16-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN16-REC (I:1) =   "?"
                   MOVE    PIN16-REC    TO     WK-REC
                   MOVE    WK-PIN16-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S116-EX.
           EXIT.

      *    *** WRITE POT1
       S117-10.

           IF      WK-PIN17-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-o"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN17-LEN
                      OR PIN17-REC (I:1) = "?"
                   IF      PIN17-REC (I:1) NOT = '"'
                           MOVE    PIN17-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN17-REC (I:1) =   "?"
                   MOVE    PIN17-REC    TO     WK-REC
                   MOVE    WK-PIN17-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S117-EX.
           EXIT.

      *    *** WRITE POT1
       S118-10.

           IF      WK-PIN18-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-p"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN18-LEN
                      OR PIN18-REC (I:1) = "?"
                   IF      PIN18-REC (I:1) NOT = '"'
                           MOVE    PIN18-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN18-REC (I:1) =   "?"
                   MOVE    PIN18-REC    TO     WK-REC
                   MOVE    WK-PIN18-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S118-EX.
           EXIT.

      *    *** WRITE POT1
       S119-10.

           IF      WK-PIN19-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-q"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN19-LEN
                      OR PIN19-REC (I:1) = "?"
                   IF      PIN19-REC (I:1) NOT = '"'
                           MOVE    PIN19-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN19-REC (I:1) =   "?"
                   MOVE    PIN19-REC    TO     WK-REC
                   MOVE    WK-PIN19-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S119-EX.
           EXIT.

      *    *** WRITE POT1
       S120-10.

           IF      WK-PIN20-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-r"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN20-LEN
                      OR PIN20-REC (I:1) = "?"
                   IF      PIN20-REC (I:1) NOT = '"'
                           MOVE    PIN20-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN20-REC (I:1) =   "?"
                   MOVE    PIN20-REC    TO     WK-REC
                   MOVE    WK-PIN20-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
                AND NUMVAL(WK-SU) >=    WK-LIMIT
                  ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S120-EX.
           EXIT.

      *    *** WRITE POT1
       S121-10.

           IF      WK-PIN21-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-s"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN21-LEN
                      OR PIN21-REC (I:1) = "?"
                   IF      PIN21-REC (I:1) NOT = '"'
                           MOVE    PIN21-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN21-REC (I:1) =   "?"
                   MOVE    PIN21-REC    TO     WK-REC
                   MOVE    WK-PIN21-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S121-EX.
           EXIT.

      *    *** WRITE POT1
       S122-10.

           IF      WK-PIN22-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-t"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN22-LEN
                      OR PIN22-REC (I:1) = "?"
                   IF      PIN22-REC (I:1) NOT = '"'
                           MOVE    PIN22-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN22-REC (I:1) =   "?"
                   MOVE    PIN22-REC    TO     WK-REC
                   MOVE    WK-PIN22-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
                AND NUMVAL(WK-SU) >=    WK-LIMIT
                  ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S122-EX.
           EXIT.

      *    *** WRITE POT1
       S123-10.

           IF      WK-PIN23-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-u"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN23-LEN
                      OR PIN23-REC (I:1) = "?"
                   IF      PIN23-REC (I:1) NOT = '"'
                           MOVE    PIN23-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN23-REC (I:1) =   "?"
                   MOVE    PIN23-REC    TO     WK-REC
                   MOVE    WK-PIN23-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S123-EX.
           EXIT.

      *    *** WRITE POT1
       S124-10.

           IF      WK-PIN24-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-v"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN24-LEN
                      OR PIN24-REC (I:1) = "?"
                   IF      PIN24-REC (I:1) NOT = '"'
                           MOVE    PIN24-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN24-REC (I:1) =   "?"
                   MOVE    PIN24-REC    TO     WK-REC
                   MOVE    WK-PIN24-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S124-EX.
           EXIT.

      *    *** WRITE POT1
       S125-10.

           IF      WK-PIN25-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-w"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN25-LEN
                      OR PIN25-REC (I:1) = "?"
                   IF      PIN25-REC (I:1) NOT = '"'
                           MOVE    PIN25-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN25-REC (I:1) =   "?"
                   MOVE    PIN25-REC    TO     WK-REC
                   MOVE    WK-PIN25-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S125-EX.
           EXIT.

      *    *** WRITE POT1
       S126-10.

           IF      WK-PIN26-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-x"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN26-LEN
                      OR PIN26-REC (I:1) = "?"
                   IF      PIN26-REC (I:1) NOT = '"'
                           MOVE    PIN26-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN26-REC (I:1) =   "?"
                   MOVE    PIN26-REC    TO     WK-REC
                   MOVE    WK-PIN26-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S126-EX.
           EXIT.

      *    *** WRITE POT1
       S127-10.

           IF      WK-PIN27-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-y"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN27-LEN
                      OR PIN27-REC (I:1) = "?"
                   IF      PIN27-REC (I:1) NOT = '"'
                           MOVE    PIN27-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN27-REC (I:1) =   "?"
                   MOVE    PIN27-REC   TO      WK-REC
                   MOVE    WK-PIN27-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S127-EX.
           EXIT.

      *    *** WRITE POT1
       S128-10.

           IF      WK-PIN28-CNT =       1
      *    *** ジャパリ
                   MOVE    X"E382B8E383A3E38391E383AA"
                                       TO      POT1-REC (1:12)
                   MOVE    "aduxvi-z"  TO      POT1-REC (13:)

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF

           MOVE    SPACE       TO      POT1-REC
           MOVE    1           TO      I2
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > WK-PIN28-LEN
                      OR PIN28-REC (I:1) = "?"
                   IF      PIN28-REC (I:1) NOT = '"'
                           MOVE    PIN28-REC (I:1) TO   POT1-REC (I2:1)
                           ADD     1           TO      I2
                   END-IF
           END-PERFORM

           IF      PIN28-REC (I:1) =    "?"
                   MOVE    PIN28-REC   TO      WK-REC
                   MOVE    WK-PIN28-LEN TO     WK-LEN
      *    *** 検索数　抽出
                   PERFORM S200-10     THRU    S200-EX
           END-IF

           IF      I2          >       1
               AND NUMVAL(WK-SU) >=    WK-LIMIT
                   ADD     I2 1        GIVING  J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   MOVE    "https://www.xvideos.com/?k="  
                                       TO      POT1-REC (J:27)
                   ADD     27          TO      J

                   MOVE    POT1-REC (1:I2)  
                                       TO      POT1-REC (J:I2)
                   ADD     I2          TO      J

                   MOVE    " ,OF ,"    TO      POT1-REC (J:6)
                   ADD     6           TO      J

                   MOVE    NUMVAL(WK-SU) TO    WK-SU2
                   MOVE    WK-SU2      TO      POT1-REC (J:10)
                   ADD     10          TO      J

                   MOVE    " ,"        TO      POT1-REC (J:2)
                   ADD     2           TO      J

                   WRITE   POT1-REC
                   ADD     1           TO      WK-POT1-CNT
           END-IF
           .
       S128-EX.
           EXIT.

      *    *** 検索数　抽出
       S200-10.

           MOVE    SPACE       TO      WK-SU
      *     MOVE    I           TO      I3
           ADD     I 1         GIVING  I3
           MOVE    1           TO      I4
      *    *** ? ２つ以上あると、WK-LEN まで処理しているので、
      *    *** WK-LIMIT の項目、壊していた
           PERFORM VARYING I FROM I3 BY 1
                   UNTIL I > WK-LEN
      *             IF      WK-REC (I:1) NOT = '"' AND ","
                   IF      WK-REC (I:1) NOT = '"' AND "," AND "?"
                           MOVE    WK-REC (I:1) TO   WK-SU (I4:1)
                           ADD     1           TO      I4
                   END-IF
           END-PERFORM

      *     IF WK-REC (1:6) = "family"
      *         DISPLAY WK-REC (1:30) " WK-SU=" WK-SU 
      *                 " I=" I " I3=" I3 " I4=" I4 " WK-LEN=" WK-LEN
      *     END-IF
           .
       S200-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN2-F
           IF      WK-PIN2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN2-F CLOSE ERROR STATUS="
                           WK-PIN2-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN3-F
           IF      WK-PIN3-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN3-F CLOSE ERROR STATUS="
                           WK-PIN3-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN4-F
           IF      WK-PIN4-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN4-F CLOSE ERROR STATUS="
                           WK-PIN4-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN5-F
           IF      WK-PIN5-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN5-F CLOSE ERROR STATUS="
                           WK-PIN5-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN6-F
           IF      WK-PIN6-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN6-F CLOSE ERROR STATUS="
                           WK-PIN6-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN7-F
           IF      WK-PIN7-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN7-F CLOSE ERROR STATUS="
                           WK-PIN7-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN8-F
           IF      WK-PIN8-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN8-F CLOSE ERROR STATUS="
                           WK-PIN8-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN9-F
           IF      WK-PIN9-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN9-F CLOSE ERROR STATUS="
                           WK-PIN9-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN10-F
           IF      WK-PIN10-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN10-F CLOSE ERROR STATUS="
                           WK-PIN10-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN11-F
           IF      WK-PIN11-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN11-F CLOSE ERROR STATUS="
                           WK-PIN11-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN12-F
           IF      WK-PIN12-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN12-F CLOSE ERROR STATUS="
                           WK-PIN12-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN13-F
           IF      WK-PIN13-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN13-F CLOSE ERROR STATUS="
                           WK-PIN13-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN14-F
           IF      WK-PIN14-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN14-F CLOSE ERROR STATUS="
                           WK-PIN14-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN15-F
           IF      WK-PIN15-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN15-F CLOSE ERROR STATUS="
                           WK-PIN15-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN16-F
           IF      WK-PIN16-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN16-F CLOSE ERROR STATUS="
                           WK-PIN16-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN17-F
           IF      WK-PIN17-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN17-F CLOSE ERROR STATUS="
                           WK-PIN17-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN18-F
           IF      WK-PIN18-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN18-F CLOSE ERROR STATUS="
                           WK-PIN18-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN19-F
           IF      WK-PIN19-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN19-F CLOSE ERROR STATUS="
                           WK-PIN19-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN20-F
           IF      WK-PIN20-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN20-F CLOSE ERROR STATUS="
                           WK-PIN20-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN21-F
           IF      WK-PIN21-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN21-F CLOSE ERROR STATUS="
                           WK-PIN21-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN22-F
           IF      WK-PIN22-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN22-F CLOSE ERROR STATUS="
                           WK-PIN22-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN23-F
           IF      WK-PIN23-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN23-F CLOSE ERROR STATUS="
                           WK-PIN23-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN24-F
           IF      WK-PIN24-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN24-F CLOSE ERROR STATUS="
                           WK-PIN24-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN25-F
           IF      WK-PIN25-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN25-F CLOSE ERROR STATUS="
                           WK-PIN25-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN26-F
           IF      WK-PIN26-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN26-F CLOSE ERROR STATUS="
                           WK-PIN26-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN27-F
           IF      WK-PIN27-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN27-F CLOSE ERROR STATUS="
                           WK-PIN27-STATUS
                   STOP    RUN
           END-IF

           CLOSE   PIN28-F
           IF      WK-PIN28-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN28-F CLOSE ERROR STATUS="
                           WK-PIN28-STATUS
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
           DISPLAY WK-PGM-NAME " PIN1 ｹﾝｽｳ  = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN2 ｹﾝｽｳ  = " WK-PIN2-CNT
                   " (" WK-PIN2-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN3 ｹﾝｽｳ  = " WK-PIN3-CNT
                   " (" WK-PIN3-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN4 ｹﾝｽｳ  = " WK-PIN4-CNT
                   " (" WK-PIN4-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN5 ｹﾝｽｳ  = " WK-PIN5-CNT
                   " (" WK-PIN5-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN6 ｹﾝｽｳ  = " WK-PIN6-CNT
                   " (" WK-PIN6-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN7 ｹﾝｽｳ  = " WK-PIN7-CNT
                   " (" WK-PIN7-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN8 ｹﾝｽｳ  = " WK-PIN8-CNT
                   " (" WK-PIN8-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN9 ｹﾝｽｳ  = " WK-PIN9-CNT
                   " (" WK-PIN9-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN10 ｹﾝｽｳ = " WK-PIN10-CNT
                   " (" WK-PIN10-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN11 ｹﾝｽｳ = " WK-PIN11-CNT
                   " (" WK-PIN11-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN12 ｹﾝｽｳ = " WK-PIN12-CNT
                   " (" WK-PIN12-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN13 ｹﾝｽｳ = " WK-PIN13-CNT
                   " (" WK-PIN13-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN14 ｹﾝｽｳ = " WK-PIN14-CNT
                   " (" WK-PIN14-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN15 ｹﾝｽｳ = " WK-PIN15-CNT
                   " (" WK-PIN15-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN16 ｹﾝｽｳ = " WK-PIN16-CNT
                   " (" WK-PIN16-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN17 ｹﾝｽｳ = " WK-PIN17-CNT
                   " (" WK-PIN17-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN18 ｹﾝｽｳ = " WK-PIN18-CNT
                   " (" WK-PIN18-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN19 ｹﾝｽｳ = " WK-PIN19-CNT
                   " (" WK-PIN19-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN20 ｹﾝｽｳ = " WK-PIN20-CNT
                   " (" WK-PIN20-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN21 ｹﾝｽｳ = " WK-PIN21-CNT
                   " (" WK-PIN21-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN22 ｹﾝｽｳ = " WK-PIN22-CNT
                   " (" WK-PIN22-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN23 ｹﾝｽｳ = " WK-PIN23-CNT
                   " (" WK-PIN23-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN24 ｹﾝｽｳ = " WK-PIN24-CNT
                   " (" WK-PIN24-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN25 ｹﾝｽｳ = " WK-PIN25-CNT
                   " (" WK-PIN25-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN26 ｹﾝｽｳ = " WK-PIN26-CNT
                   " (" WK-PIN26-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN27 ｹﾝｽｳ = " WK-PIN27-CNT
                   " (" WK-PIN27-F-NAME ")"
           DISPLAY WK-PGM-NAME " PIN28 ｹﾝｽｳ = " WK-PIN28-CNT
                   " (" WK-PIN28-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT1 ｹﾝｽｳ  = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"

           MOVE    "E"         TO      WDT-DATE-TIME-ID
           CALL    "DATETIME"  USING   WDT-DATETIME-AREA
           .
       S900-EX.
           EXIT.
