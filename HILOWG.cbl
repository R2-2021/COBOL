      *    *** HI-LOW 数当てゲーム

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             HILOWG.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-ACT-NUM      PIC  9(004) VALUE ZERO.
           03  WK-HIT-EOF      PIC  X(001) VALUE LOW-VALUE.

           03  WK-CNS-1        PIC S9(5) PACKED-DECIMAL VALUE ZERO.
           03  WK-CHK-NUM      PIC S9(5) PACKED-DECIMAL VALUE 512.
           03  WK-LO-NUM       PIC S9(5) PACKED-DECIMAL VALUE +1.
           03  WK-HI-NUM       PIC S9(5) PACKED-DECIMAL VALUE +1024.
           03  WK-CNT          PIC S9(5) PACKED-DECIMAL VALUE ZERO.
           03  WK-DSP-NUM      PIC ZZZ9         VALUE ZERO.

       PROCEDURE               DIVISION.
       M100-10.

           DISPLAY "１から１０２４の数字を当てます、数を選んで下さい"
           DISPLAY " "
           PERFORM UNTIL WK-HIT-EOF = HIGH-VALUE

                   MOVE    WK-CHK-NUM  TO      WK-DSP-NUM
                   DISPLAY "あなたが選んだ数字は" WK-DSP-NUM "ですか？"
                   DISPLAY " "
                   DISPLAY "HITなら9999,大きいなら3,小さいなら1を入力"
                   ACCEPT  WK-ACT-NUM  FROM    CONSOLE

                   IF      WK-ACT-NUM  =       9999
                           MOVE    HIGH-VALUE  TO      WK-HIT-EOF
                           ADD     1           TO      WK-CNT
                   ELSE
                       IF      WK-ACT-NUM  =       1
                           MOVE    WK-CHK-NUM  TO      WK-HI-NUM
                           COMPUTE WK-CHK-NUM =
                                 ( WK-CHK-NUM - WK-LO-NUM ) / 2
                                 + WK-LO-NUM
                           ADD     1           TO      WK-CNT
                       ELSE
                           IF    WK-ACT-NUM   =    3
                               MOVE    WK-CHK-NUM  TO      WK-LO-NUM
                               COMPUTE WK-CHK-NUM ROUNDED =
                                     ( WK-HI-NUM - WK-CHK-NUM ) / 2
                                     + WK-CHK-NUM
                               ADD     1           TO      WK-CNT
                           ELSE
                               DISPLAY "1,3,9999 いずれかを入力"
                           END-IF
                       END-IF
               END-IF
      *             DISPLAY "LO=" WK-LO-NUM " HI=" WK-HI-NUM " CHK="
      *                     WK-CHK-NUM
           END-PERFORM

           MOVE    WK-CHK-NUM  TO       WK-DSP-NUM
           DISPLAY " "
           DISPLAY "あなたの選んだ数字は" WK-DSP-NUM "ですね"
           MOVE    WK-CNT      TO       WK-DSP-NUM
           DISPLAY " "
           DISPLAY WK-DSP-NUM "回でＨＩＴしました"
           .
       M100-EX.
           STOP    RUN.
