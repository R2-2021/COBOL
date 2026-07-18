REM MISSAV 女優等拡大　自動実行 SJIS で保存する
REM コマンド プロンプトで実行の時>C.TEST140  % => %% にする必要がある
REM A:女優、M:メーカー、K;検索

REM BACKUP からリロード

copy /y TEST103X.PRM1 TEST103.PRM1
copy /y TEST54X.PIN2  TEST54.PIN2 
copy /y C.TEST104_1AX.bat C.TEST104_1A.bat 
copy /y C.TEST104_1KX.bat C.TEST104_1K.bat
copy /y C.TEST104_1MX.bat C.TEST104_1M.bat

DIR TEST103.PRM1
DIR TEST103X.PRM1
DIR TEST54*
DIR C.TEST104_1*
