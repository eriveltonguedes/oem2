#### Transferências - SAUDE
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)
## 4 - FINBRA
## 2003 - 2020
load("Dados/dados.reais.lista.RData")

Transf <- dados.reais.lista[["Transf.f"]]


municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Transf_novos <- Transf[Transf$Cod.IBGE %in% municipios_novos_naousar[-11],]
Transf_novos <- Transf_novos[order(Transf_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
Transf_novos[is.na(Transf_novos)] <- 0
for (i in 1:dim(Transf_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  dados <- Transf_novos[i,colnames(Transf_novos) %in% cols]
  dados[dados==0] <- NA
  Transf_novos[i,colnames(Transf_novos) %in% cols] <- dados
}

Transf <- Transf[!(Transf$Cod.IBGE %in% municipios_novos_naousar),]
Transf[Transf==0] <- NA





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


dados <- Transf
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
Transf_imput1 <-  dados_imputada1
if(dim(dados_comna1)[1]>0){
  Transf_imput1 <-  rbind(Transf_imput1,dados_comna1)
}





#### DADOS NOVOS ####

### IMPUT ###
basemissing <- dados_comna

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


dados <- Transf_novos
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_comna1 <- dados_comna[c(1),]
dados_comna <- dados_comna[c(2:8),]
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_imput1,dados_comna1[,-20])
dados_imputada1 <- rbind(dados_imputada1, dados_semna)

Transf_imput1.n <-  dados_imputada1


#### DADOS FINAIS ####

Transf_bruta <- rbind(Transf, Transf_novos)
Transf_bruta <- Transf_bruta[order(Transf_bruta$Cod.IBGE),]


save(Transf_bruta, file = "data/Transf_bruta.RData")


Transf_imput_missing <- rbind(Transf_imput1, Transf_imput1.n)
Transf_imput_missing <- Transf_imput_missing[order(Transf_imput_missing$Cod.IBGE),]


save(Transf_imput_missing, file = "data/Transf_imput_missing.RData")

