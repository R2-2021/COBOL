# 女優以外の時、メーカーや検索結果の時用

import os
import time
from selenium import webdriver

# 保存先の設定
# output_dir = "../../cobol"
# os.makedirs(output_dir, exist_ok=True)
file_path = os.path.join("MissAV.XXXX.html")
base_url = 'YYYY'

# ブラウザの初期化（1回だけ起動）
options = webdriver.ChromeOptions()

# 初期化初回のみだと、うまく動作しないため、毎回にする
# options.add_argument('--disable-blink-features=AutomationControlled')
# driver = webdriver.Chrome(options=options)

try:
    for page in range(1, PPPP):
        options.add_argument('--disable-blink-features=AutomationControlled')
        driver = webdriver.Chrome(options=options)

        # ページ移動
        driver.get(f'{base_url}{page}')

        print(f"\n--- {page}ページ目を開きました ---")
        print("Cloudflare等の確認が終わり、ページが表示されるまで待機（5秒）...")
        time.sleep(5) 

        # HTMLの取得と書き込み（1ページ目は新規 'w'、以降は追記 'a'）
        html = driver.page_source
        mode = 'w' if page == 1 else 'a'

        with open(file_path, mode, encoding='utf8') as file:
            file.write(html)

        driver.quit()

finally:
    print("\nすべてのページの保存が完了しました。")

# finally:
    # エラーが起きても確実にブラウザを閉じる
#    driver.quit()
