# YouTube 検索用
import os
import requests
file_path = os.path.join("youtube.ychアニマルプラネット.html")
res = requests.get('https://www.youtube.com/@animalplanetjapan')
with open(file_path,'w',encoding='utf8') as file:
    file.write(res.text)
