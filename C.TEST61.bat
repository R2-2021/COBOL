REM ＊＊＊　YouTube アニメ年代順、タイトル順データ作成　＊＊＊
REM Python でWebreq.animeito202101.py 最新 YYYYMMを作り、実行F5
REM 作成された html Python => COBOL へ
REM TEST10 TEST10.PIN2 の １行目 YYYYMM 変更して実行
REM TEST26 実行
REM TEST28 実行
REM TEST50.PIN1 に　最新YYYY.MM TEST28_YYYYMM.POT1 データ追加
REM TEST62.POT2 に件数あるとき、
REM TEST62.PIN3 に漢字よみデータ追加  TEST62再実行
REM TEST50.CBL,TEST51.CBL アニメイト　アニメ一覧 最新年度月変更 ID＝は変更ないみたい
REM 
REM   *    *** TEST10
REM   *    ***   |
REM   *    *** TEST28
REM   *    ***
REM   *    *** TEST61
REM   *    ***   |
REM   *    *** TEST62
REM   *    ***   |
REM   *    *** COBSORT COBSORT.T003.PRM1
REM   *    ***   |
REM   *    *** TEST63
REM   *    ***   |
REM   *    *** TEST51 アニメ 名前順 ｈｔｍｌ 作成
REM   *    ***   |
REM   *    *** COBSORT COBSORT.T009.PRM1
REM   *    ***   |
REM   *    *** TEST73
REM   *    ***   |
REM   *    *** TEST50 アニメ 年代順 ｈｔｍｌ 作成
REM 
TEST61
TEST62
COBSORT COBSORT.T003.PRM1
TEST63
TEST51
COBSORT COBSORT.T009.PRM1
TEST73
TEST50
