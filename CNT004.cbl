      *****  Mission 2-4 ���\��  *****  100 ��тɕ\������

       identification   division.
       program-id.      cnt004.
      *
       data             division.
       working-storage  section.
       01  wcnt         pic 9(9) value 0.
       01  wcnt-d       pic ZZZ,ZZZ,ZZ9 value zero
       01  cnt          pic 9(9) value 0.
       01  cnt-d        pic ZZZ,ZZZ,ZZ9 value zero
       01  cnt1         pic 9(9) value 0.
       01  wtime.
           03  hh       pic 9(2).
           03  mm       pic 9(2).
           03  ss       pic 9(2).
       01  stime        pic 9(6) value 0.
       01  etime        pic 9(6) value 0.
       01  xtime        pic 9(6) value 0.
      *
       procedure        division.
      *    ***** wcnt = 999,999,999 �܂ŌJ��Ԃ� *****
           perform until wcnt = 999
              display "cnt 999,999,999 �܂ł̎���,999�ŏI���"
      *       ***** wcnt �̎擾 *****
              accept wcnt
              if wcnt = 999
                then
                  continue
                else
      *           ***** ������� �̎擾 *****
                  move zero to cnt cnt1
                  accept wtime from time
                  compute stime = (hh * 3600) + (mm * 60) + ss
      *           ***** ���� �̊J�n *****
                  perform until cnt = wcnt
                    add 1 to cnt cnt1
      *
      *             add 1 to cnt
                    if cnt1 = 1000000 then
                        move  cnt to cnt-d
                        display cnt-d  move zero to cnt1
                        else continue
                     end-if
                  end-perform
      *           ***** ������� �̎擾 *****
                  move  wcnt to wcnt-d
                  display wcnt-d "���܂ł̎��Ԃ́H"
                  accept wtime from time
      *           ***** ���v�����̎Z�o��\�� *****
                  compute etime = ((hh * 3600) + (mm * 60) + ss)
                  compute xtime = etime - stime
                  compute hh    = xtime / 3600
                  compute etime = xtime - (hh * 3600)
                  compute mm    = xtime / 60
                  compute ss    = xtime - (mm * 60)
                  display "time = " hh ":" mm ":" ss
              end-if
           end-perform
           display "!!! cnt004 ended !!!"
           stop run.
