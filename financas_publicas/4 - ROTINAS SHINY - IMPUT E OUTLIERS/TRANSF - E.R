#### Transferências - SAUDE
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

## 3 - SIOPE
## 2008 - 2019 
load("Dados/dados.reais.lista.RData")

Etransf <- dados.reais.lista[["Educacao.Transf.e"]]


municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Etransf_novos <- Etransf[Etransf$Cod.IBGE %in% municipios_novos_naousar[-11],]
Etransf_novos <- Etransf_novos[order(Etransf_novos$Cod.IBGE),]
Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
         "2008","2008")
Etransf_novos[is.na(Etransf_novos)] <- 0


for (i in 1:dim(Etransf_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2019
  dados <- Etransf_novos[i,colnames(Etransf_novos) %in% cols]
  dados[dados==0] <- NA
  Etransf_novos[i,colnames(Etransf_novos) %in% cols] <- dados
}

Etransf <- Etransf[!(Etransf$Cod.IBGE %in% municipios_novos_naousar),]
Etransf[Etransf==0] <- NA





##### IMPUT #####
imput.ma <- function(basemissing){
  d3 <- basemissing
  imput <- basemissing[,-1]
  for (i in 1:dim(imput)[1]) {
    x <- as.numeric(unlist(imput[i,]))
    d3[i,-1] <- na_ma(x)
  }
  return(d3)
}


dados <- Etransf
dados[dados==0] <- NA
dados_semna <- na.omit(dados)
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
a <- list()
d <- vector()
for (j in 1:dim(dados_comna)[1]) {
  a[[j]] <- which(is.na(dados_comna[j,]))
  d[j] <- length(a[[j]])
}
table(d)
tamanho <- as.numeric(dim(dados)[2])
minimo <- tamanho - 2
dados_comna1 <- dados_comna[d>=minimo,]
dados_comna <- dados_comna[d<minimo,]
dados_imput1 <- imput.ma(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
Etransf_imput1 <-  dados_imputada1
if(dim(dados_comna1)[1]>0){
  Etransf_imput1 <-  rbind(Etransf_imput1,dados_comna1)
}





#### DADOS NOVOS ####

### IMPUT ###
imput.ma2 <- function(basemissing){
  col <- as.numeric(dim(basemissing)[2])
  ano <- as.vector(basemissing[,col])
  d3 <- basemissing[,-col]
  imput <- basemissing[,-c(1,col)]
  for (i in 1:dim(imput)[1]) {
    ass <- ano[i]
    cols <- as.numeric(ass):2019
    x <- as.numeric(unlist(imput[i,colnames(imput) %in% as.character(cols)]))
    d3[i,as.character(cols)] <- na_ma(x)
  }
  return(d3)
}


dados <- Etransf_novos
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
               "2008","2008")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_comna1 <- dados_comna[c(1:6,8:9),]
dados_comna <- dados_comna[c(7,10),]
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_imput1,dados_comna1[,-14])
Etransf_imput1.n <-  dados_imputada1


#### DADOS FINAIS ####

Etransf_bruta <- rbind(Etransf, Etransf_novos)
Etransf_bruta <- Etransf_bruta[order(Etransf_bruta$Cod.IBGE),]


save(Etransf_bruta, file = "data/Etransf_bruta.RData")


Etransf_imput_missing <- rbind(Etransf_imput1, Etransf_imput1.n)
Etransf_imput_missing <- Etransf_imput_missing[order(Etransf_imput_missing$Cod.IBGE),]


save(Etransf_imput_missing, file = "data/Etransf_imput_missing.RData")
