
#Bibliotecas
import pandas as pd
import psycopg2
import time
import requests
import json

def slack_notificatin(ano, hour, minutes, seconds):
    web_hook_url = 'https://hooks.slack.com/services/--------------'
    slack_msg = {'text':f'''Finalizado rem_ipea_2020 em tb_vinculos_{ano}
    Tempo: {int(hour)}h, {int(minutes)}min, {round(seconds,2)}s'''}
    requests.post(web_hook_url, data=json.dumps(slack_msg))
    return ('Notification sended')


def main():
    try:
        conn = psycopg2.connect(database="rais_xxxx", user = "i3xxx", password = "ipeaxxx", host = "xxxxxxx", port = "5432") #Conectar ao servidor PostgreSQL
        cur = conn.cursor()

        inicio = time.no
        for ano in range(2018,1984,-1):
            start_time = time.time()
            cur.execute(f'''update vinculos.tb_vinculos_{ano}
            set rem_ipea_2020 = round(rem_ipea + (rem_ipea * 0.0448),2)''')
            conn.commit()
            spend = time.time()-start_time
            spend = spend % (24 * 3600)
            hour = spend // 3600
            spend %= 3600
            minutes = spend // 60
            spend %= 60
            seconds = spend
            print(f"Finalizado ano {int(ano)} -> Time spent: {int(hour)}h, {int(minutes)}min, {round(seconds,2)}s")
            slack_notificatin(ano, hour, minutes, seconds)

    except Exception as erro:
        print(erro)


if __name__ == “main”:
    main()
