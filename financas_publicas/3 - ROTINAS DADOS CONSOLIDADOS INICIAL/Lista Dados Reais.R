#### Lista dados reais
library(reshape2)
library(stringr)
load("DADOS CONSOLIDADOS/dados_reais.RData")
dados.reais.lista <- list()
for (i in 3:dim(dados_reais)[2]) {
  dados <- dados_reais[,c(1,2,i)]
  dados.reais.lista[[i-2]] <- dcast(dados, Cod.IBGE~Ano, value.var = colnames(dados)[3])
  
}

names(dados.reais.lista) <- colnames(dados_reais)[3:dim(dados_reais)[2]]


b <- which(str_sub(names(dados.reais.lista), start=-1)=="e")

for (i in b) {
  dados.reais.lista[[i]] <- dados.reais.lista[[i]][,-c(2:6,19)]
}


save(dados.reais.lista,file="DADOS CONSOLIDADOS/dados.reais.lista.RData")
save(dados.reais.lista,file="~/Documents/IPEA/SHINY_FINAL/Dados/dados.reais.lista.RData")
