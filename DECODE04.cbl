      *    *** バイナリーファイル レコード取り出し サブルーチン

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE04.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE04".

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-BUF1         PIC  X(65536) VALUE SPACE.

       LINKAGE                 SECTION.


      *    *** LI-SHORI="FIRST" MAINでセット、初回CALL時
      *    *** LI-SHORI="COND " SUB でセット、レコード出力中
      *    *** LI-SHORI="READ " SUB でセット、レコード単位にセット後、
      *    ***   MAIN側にREAD依頼
      *    *** LI-SHORI="END  " SUB でセット,X"FF"見つけて、
      *    ***   レコードエンドの時
      *    *** 
       01  LI-AREA.
           03  LI-SHORI        PIC  X(005) VALUE SPACE.
           03  LI-BUF1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  LI-REC-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** 変換前 バイナリーファイル　レコードが入っているデータ
       01  LI-BUF1             PIC  X(001) ANY LENGTH.

      *    *** 変換後 レコード単位にセットする
       01  LI-BUF2.
           03  LI-BUF2-TBL     OCCURS 65536
                               PIC  X(001) VALUE SPACE.

       PROCEDURE   DIVISION    USING   LI-AREA
                                       LI-BUF1
                                       LI-BUF2
           .
       M100-10.

      *    *** L1,BUF1 取出し側、次の開始位置
      *    *** L2､BUF2 出力側、開始位置
      *    *** L3,BUF1,BUF2 取出す、セットする長さ
      *    *** L4,BUF1 取出し側、次々の開始位置

      *    *** LI-SHORI="FIRST" 最初のCALL時
           IF      LI-SHORI    =       "FIRST"
                   PERFORM S010-10     THRU    S010-EX

      *    *** X"0D0A" 見つけたので、COND セット　③へ
                   MOVE    "COND "     TO      LI-SHORI
           END-IF



      *    *** LI-SHORI="COND " レコード取り出し中
      *    *** LI-SHORI="READ " レコード取り出し、READ直後、先頭
           IF      LI-SHORI    =       "READ "
      *    *** L3=0 はBUF1 最後が X"0A0D" の時
                   IF      L3          =       ZERO

      *    *** ①
                           MOVE    LI-BUF1 (L1:LI-REC-LEN) TO
                                   LI-BUF2 (1:LI-REC-LEN)
                           MOVE    "COND "     TO      LI-SHORI

      *    *** L1= 次開始位置
      *    *** 次のレコード取り出し位置設定
                           ADD     L1 LI-REC-LEN GIVING L1
                   ELSE

      *    *** ②
      *    *** READ されてきた、残りのレコード合成する
                           MOVE    SV-BUF1 (1:L3) TO   LI-BUF2 (1:L3)
                           MOVE    LI-BUF1 ( 1:LI-REC-LEN - L3) TO
                                   LI-BUF2 (L2:LI-REC-LEN - L3)
                           MOVE    "COND "     TO      LI-SHORI

      *    *** L1= 次開始位置
      *    *** 次のレコード取り出し位置設定
                           COMPUTE L1 = 1 + ( LI-REC-LEN - L3 )

                   END-IF
           ELSE
      *    *** ③
                   MOVE    LI-BUF1 (L1:LI-REC-LEN) 
                                       TO      LI-BUF2 (1:LI-REC-LEN)
                   MOVE    "COND "     TO      LI-SHORI

      *    *** 次のレコード取り出し位置設定
                   ADD     L1 LI-REC-LEN GIVING L1

           END-IF



      *    *** ケースとして、３件しかない時、
      *    *** ①、②、③レコード内にすべて収まっていた時、次々、開始位置
      *    *** 判定でも、CONDで③レコードセットした後、L1 > LI-BUF1-LEN
      *    *** 条件が発生する

           IF      L1         >       LI-BUF1-LEN
      *    *** LI-BUF1-LEN 内に最後のレコードが　入っていた時、③で最後の
      *    *** レコードセットしてから、レコード超えるので,MAINにREAD依頼
                   MOVE    "READ "      TO      LI-SHORI
                   MOVE    1            TO      L1
                   MOVE    1            TO      L2
                   MOVE    ZERO         TO      L3
      *    *** 次のCALL時、①へ
           ELSE
      *    IF      L1         <=      LI-BUF1-LEN

      *    *** L4= 次々、開始位置
                   COMPUTE L4 = L1 + LI-REC-LEN

      *    *** L4 - 1 次々レコードも、LI-BUF1-LEN内に収まっているか、判定
                   IF      L4 - 1      >       LI-BUF1-LEN

      *    *** レコード長、LI-BUF1-LEN内に収まっていない時
      *    *** メイン処理側にＲＥＡＤ依頼する
                           MOVE    "READ "      TO      LI-SHORI

      *    *** LI-BUF1-LEN で最後のレコードが次のレコードと分かれている時
                           COMPUTE L3 = LI-BUF1-LEN - L1 + 1
                           COMPUTE L2 = L3 + 1

      *    *** レコードエンドまでのエリア退避する
                           MOVE    LI-BUF1 (L1:L3) TO   SV-BUF1 (1:L3)

      *    *** 次のCALL時、②へ
                   ELSE

      *    *** レコード長、LI-BUF1-LEN内に収まっている時
      *        IF      L4 - 1 <= LI-BUF1-LEN

                           IF      L4 - 1 = LI-BUF1-LEN
                                   CONTINUE

      *    *** レコードが丁度最終バイトで終わった時
      *    *** 次のCALL時、③へ
                           ELSE
                                   CONTINUE
      *    *** 次のCALL時、③へ
                           END-IF
                   END-IF
           END-IF



           IF      LI-BUF2 (1:1) =     X"FF"

      *    *** LI-SHORI="END  " X"FF"の時
                   MOVE    "END  "     TO      LI-SHORI
           ELSE
               IF  LI-BUF2 (LI-REC-LEN - 1:2) NOT = X"0D0A"
                   DISPLAY WK-PGM-NAME " BUF1 DATA X'0D0A' 無し ERROR"
                                       " 固定長のみ 処理可"
                   CALL    "COBDUMP" USING LI-BUF2 (LI-REC-LEN - 1:2)

                   DISPLAY WK-PGM-NAME " レコード長=" LI-REC-LEN
                   DISPLAY WK-PGM-NAME " レコード長、次開始POS=" L1
                   DISPLAY WK-PGM-NAME " レコード0D0A  POS=" LI-REC-LEN
                                       "-1"

                   CALL    "COBDUMP" USING LI-BUF2 (1:LI-REC-LEN)
                   STOP    RUN
               END-IF
           END-IF

           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** X"0D0A" 存在チェック
       S010-10.

           IF      LI-BUF1-LEN >       65536 OR
                   LI-BUF1-LEN <=      ZERO
                   DISPLAY WK-PGM-NAME " BUF1 DATA MAX-LEN OVER 65536"
                                       ",ZERO,OR,MINUS ERROR"
                   DISPLAY WK-PGM-NAME " BUF1-LEN=" LI-BUF1-LEN
                   STOP    RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I >  LI-BUF1-LEN OR
                           SW-FIRST    =       "Y"

      *    *** 最初に現れたX"0D0A"までの長さをレコード長にセット
                   IF      LI-BUF1 (I:2) =     X"0D0A"
                           ADD     1 I         GIVING  LI-REC-LEN
                           MOVE    "Y"         TO      SW-FIRST

      *    *** 1件目は1バイト目から取り出す
                           MOVE    1           TO      L1
                   END-IF
           END-PERFORM

           IF      SW-FIRST    =       "N"
                   DISPLAY WK-PGM-NAME " BUF1 DATA X'0D0A' 無し ERROR"
                   STOP    RUN
           END-IF

           .
       S010-EX.
           EXIT.
