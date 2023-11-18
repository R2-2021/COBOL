      *    *** 画面入力項目チェック変換サブルーチン
      *    *** 
      *    *** 未使用のため、未完成

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             COBENTCK.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "COBENTCK".

       01  INDEX-AREA.
           03  I               BINARY-LONG VALUE ZERO.
           03  J               BINARY-LONG VALUE ZERO.
           03  K               BINARY-LONG VALUE ZERO.

       LINKAGE                 SECTION.
       01  LINK-AREA.
      *    *** ID=X9 Ｘタイプを９タイプに変換
      *    *** ID=XX XXX
      *    *** ID=XX XXX
           03  ENTCK-ID        PIC  X(002).
           03  ENTCK-ITEMIN    PIC  X(010).
           03  ENTCK-ITEMOUT   PIC  X(010).
           03  ENTCK-ERROR     PIC  X(001).
           03  ENTCK-ERRORNO   PIC  9(002).

       PROCEDURE               DIVISION
                   USING       LINK-AREA.

       M100-10.

           IF      ENTCK-ID    =       "X9"
                   PERFORM S010-10     THRU    S010-EX
           END-IF
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** Ｘタイプを９タイプに変換
       S010-10.

      *    *** --99.55  => ERROR －が２つある
      *    *** A9..9    => ERROR ．が２つある
      *    *** 9-9.55   => ERROR －が最初か最後以外にある
      *    *** 99.-55   => ERROR －が最初か最後以外にある
      *    *** 9 9.55   => ERROR スペースが最初か最後以外にある
      *    *** 99. 55   => ERROR スペースが最初か最後以外にある
      *    *** 99.55-   => OK
      *    ***  99.55   => OK
      *    *** +99.55   => OK
      *    *** -99.55   => OK
      *    *** 99.      => OK
      *    *** .99      => OK
      *    *** 99       => OK

      *    *** 右寄せ
           CALL "C$JUSTIFY" USING ENTCK-ITEMIN "R"
      *    *** 前スペース＝＞ゼロ
           INSPECT ENTCK-ITEMIN REPLACING ALL SPACE BY ZERO
           INSPECT ENTCK-ITEMIN TALLING
           .
       S010-EX.
           EXIT.


































