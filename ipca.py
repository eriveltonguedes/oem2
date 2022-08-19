def ipca(anoBase, anoValor, valor):
    anoBase = int(anoBase)
    anoValor = int(anoValor)
    valor = float(valor)
    conn = conn = sqlite3.connect(r'C:\Users\b11811989\Desktop\git\correcaoInflacao\database.db')
    cur = conn.cursor()
    listaIndice = []
    anoOrigem = anoValor
    while anoOrigem <= anoBase:
         cur.execute("select correcao from ipca where ano = ? ", (anoOrigem,))
         aux = cur.fetchone()
         listaIndice.append(aux[0])
         anoOrigem += 1
    return (valor*np.prod(listaIndice))