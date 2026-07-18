REM MISSAV 女優等拡大　自動実行 SJIS で保存する
REM コマンド プロンプトで実行の時>C.TEST140  ％ => ％％ にする必要がある
REM A:女優、M:メーカー、K;検索、J:ジャンル、L:レーベル、T:ｔａｇ,S:シリーズ
REM 更新するファイルバックアップCOPYする

copy /y TEST103.PRM1 TEST103X.PRM1
copy /y TEST54.PIN2  TEST54X.PIN2
copy /y C.TEST104_1A.bat C.TEST104_1AX.bat
copy /y C.TEST104_1K.bat C.TEST104_1KX.bat
copy /y C.TEST104_1M.bat C.TEST104_1MX.bat
copy /y C.TEST104_1J.bat C.TEST104_1JX.bat
copy /y C.TEST104_1L.bat C.TEST104_1LX.bat
copy /y C.TEST104_1T.bat C.TEST104_1TX.bat
copy /y C.TEST104_1S.bat C.TEST104_1SX.bat


REM TEST140 "" 21 女優名 A

REM TEST140 "" 21 メーカー名 M

REM TEST140 "" 21 検索名 K

REM TEST140 "" 21 ジャンル名 J

REM TEST140 "" 21 レーベル名 L

REM TEST140 "" 21 ｔａｇ名 T

REM TEST140 "" 21 シリーズ名 S
Y


TEST140 "https://missav.ai/ja/actresses/%%E8%%BC%%9D%%E6%%98%%9F%%E3%%81%%8D%%E3%%82%%89?filters=individual&sort=views&page=" 5 輝星きら A
REM TEST140 "" 12 円女交際 S



REM 下記はプログラム内で出力
REM copy /y TEST140.POT2 TEST54.PIN2
REM copy /y TEST140.POT3 TEST103.PRM1
REM copy /y TEST140.POT4 C.TEST104_1A.bat
