pacotes <- c("odbc","DBI","dplyr","tidyverse","dbplyr","RMySQL","sidrar","reshape2")
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#locale("us")


###############################################################################
##    Conecta a Base do IBGE via API, resgata as tabelas selecionadas
###############################################################################


###############################################################################
## Tabela 1737 - IPCA
# só o indice
# https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/p/all/d/v2266%2013
##############################################################################


ipca = get_sidra(api = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013")


