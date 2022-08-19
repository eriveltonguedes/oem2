#### Transferências - SAUDE
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

## 2 - SIOPS
## 2003 - 2020
load("Dados/dados.reais.lista.RData")
Stransf <- dados.reais.lista[["Saude.Transf.s"]]


municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Stransf_novos <- Stransf[Stransf$Cod.IBGE %in% municipios_novos_naousar[-11],]
Stransf_novos <- Stransf_novos[order(Stransf_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
for (i in 1:dim(Stransf_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  dados <- Stransf_novos[i,colnames(Stransf_novos) %in% cols]
  dados[dados==0] <- NA
  Stransf_novos[i,colnames(Stransf_novos) %in% cols] <- dados
}

Stransf <- Stransf[!(Stransf$Cod.IBGE %in% municipios_novos_naousar),]
Stransf[Stransf==0] <- NA





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


dados <- Stransf
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
Stransf_imput1 <-  dados_imputada1
if(dim(dados_comna1)[1]>0){
  Stransf_imput1 <-  rbind(Stransf_imput1,dados_comna1)
}





#### DADOS NOVOS ####

### IMPUT ###
imput.ma2 <- function(basemissing){
  col <- as.numeric(dim(basemissing)[2])
  ano <- as.vector(basemissing[,col])
  d3 <- basemissing[,-col]
  imput <- basemissing[,-c(1,col)]
  for (i in 1:dim(imput)[1]) {
    ass <- ano[[i]]
    cols <- as.numeric(ass):2020
    x <- as.numeric(unlist(imput[i,colnames(imput) %in% as.character(cols)]))
    d3[i,as.character(cols)] <- na_ma(x)
  }
  return(d3)
}


dados <- Stransf_novos
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_comna1 <- dados_comna[c(1:4,6,8),]
dados_comna <- dados_comna[c(5,7,9,10),]
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_imput1,dados_comna1[,-20])
Stransf_imput1.n <-  dados_imputada1


#### DADOS FINAIS ####

Stransf_bruta <- rbind(Stransf, Stransf_novos)
Stransf_bruta <- Stransf_bruta[order(Stransf_bruta$Cod.IBGE),]


save(Stransf_bruta, file = "data/Stransf_bruta.RData")


Stransf_imput_missing <- rbind(Stransf_imput1, Stransf_imput1.n)
Stransf_imput_missing <- Stransf_imput_missing[order(Stransf_imput_missing$Cod.IBGE),]


save(Stransf_imput_missing, file = "data/Stransf_imput_missing.RData")

