#### Einvestimentos SAUDE
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)


# 2- SIOPS
load("Dados/dados.reais.lista.RData")

Einvest <- dados.reais.lista[["Educacao.Invest.e"]]


municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Einvest_novos <- Einvest[Einvest$Cod.IBGE %in% municipios_novos_naousar[-11],]
Einvest_novos <- Einvest_novos[order(Einvest_novos$Cod.IBGE),]
Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
         "2008","2008")
Einvest_novos[is.na(Einvest_novos)] <- 0

for (i in 1:dim(Einvest_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2019
  dados <- Einvest_novos[i,colnames(Einvest_novos) %in%cols]
  dados[dados==0] <- NA
  Einvest_novos[i,colnames(Einvest_novos) %in%cols] <- dados
}

Einvest <- Einvest[!(Einvest$Cod.IBGE %in% municipios_novos_naousar),]
Einvest[Einvest==0] <- NA

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


dados <- Einvest
dados[dados==0] <- NA
dados_semna <- na.omit(dados)
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
# a <- list()
# d <- vector()
# for (j in 1:dim(dados_comna)[1]) {
#   a[[j]] <- which(is.na(dados_comna[j,]))
#   d[j] <- length(a[[j]])
# }
# table(d)
# tamanho <- as.numeric(dim(dados)[2])
# minimo <- tamanho - 2
# dados_comna1 <- dados_comna[d>=minimo,]
# dados_comna <- dados_comna[d<minimo,]
dados_imput1 <- imput.ma(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
# dados_imputada1 <- rbind(dados_imputada1, dados_comna1)
Einvest_imput1 <-  dados_imputada1


#### OUTLIERS #####

dados <- Einvest_imput1
col <- as.numeric(dim(dados)[2])

### Variacao

variacaoreal_abs <- dados[,-col]

for (j in 2:(col-1)) {
  variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
}
nomes <- c("Cod.IBGE", "Einvest0809","Einvest0910", "Einvest1011", "Einvest1112",
           "Einvest1213", "Einvest1314", "Einvest1415","Einvest1516", "Einvest1617",
           "Einvest1718", "Einvest1819")

colnames(variacaoreal_abs) <- nomes


#### OUTLIERS EM NIVEL REAL
#Rodando outlier por linha (por municipio)
outlier_r <- function(base){
  lista <- list()
  d <- list()
  for (j in 1:dim(base)[1]) {
    d[[j]] <-  melt(base[j,], id.vars = "Cod.IBGE")
    a <- which(d[[j]]$value %in% boxplot(d[[j]]$value, plot = FALSE)$out)
    lista[[j]] <- rbind(levels(d[[j]]$variable)[a],boxplot(d[[j]]$value)$out)
  }
  
  return(lista)
}
dados_outlier_r <- outlier_r(dados)

## base de outliers
baseoutr <- function(base_real_semna, lista){
  vecs <- vector()
  xxs <- vector()
  for(j in 1:length(lista)){
    if(length(lista[[j]])>0){vecs[j]=j; xxs[i] <- 1}
  }
  municipios_comoutlier_r <- vecs[na.omit(vecs)]
  municipios_comoutlier_r
  length(municipios_comoutlier_r)
  lista_outlier_r <- list()
  for(j in 1:length(municipios_comoutlier_r)){
    lista_outlier_r[[j]] <- lista[[municipios_comoutlier_r[j]]]
    colnames(lista_outlier_r[[j]]) <- lista_outlier_r[[j]][1,]
    lista_outlier_r[[j]] <- lista_outlier_r[[j]][-1,]
  }
  for (j in 1:length(municipios_comoutlier_r)) {
    print(lista_outlier_r[[j]])
  }
  data_outlier_r <- list()
  data_outlier_r[[1]] <- rbind.fill(as.data.frame(t(as.data.frame(lista_outlier_r[[1]]))),
                                    as.data.frame(t(as.data.frame(lista_outlier_r[[2]]))))
  for (j in 1:(length(municipios_comoutlier_r)-2)) {
    data_outlier_r[[j+1]] <- rbind.fill(data_outlier_r[[j]],
                                        as.data.frame(t(as.data.frame(lista_outlier_r[[j+2]]))))
  }
  data_outliers<- data_outlier_r[[(length(municipios_comoutlier_r)-1)]]
  munrcorr <- base_real_semna$Cod.IBGE[!duplicated(base_real_semna$Cod.IBGE)]
  cod.mun_com_rcorr_outlier_r <- munrcorr[municipios_comoutlier_r]
  data_outliers$Cod.IBGE <- cod.mun_com_rcorr_outlier_r
  data_outliers
  
  return(data_outliers)
}
data_outliers <- baseoutr(dados, dados_outlier_r)

anos <- 2008:2019

x <- NA
ini <- as.numeric((anos[1]))
for (j in 1:length(anos)) {
  x[j] <- anos[j] %in% colnames(data_outliers)
  if(x[j]==F){
    data_outliers$nova <- NA
    colnames(data_outliers)[as.numeric(dim(data_outliers)[2])] <- as.character(ini+j-1)
  }
}

data_outliers <- data_outliers[,c("Cod.IBGE", anos)]
for (j in 2:dim(data_outliers)[2]){data_outliers[,j] <- as.numeric(as.vector(data_outliers[,j]))}


dados_out_melt1 <- na.omit(melt(data_outliers, id.vars = "Cod.IBGE"))
colnames(dados_out_melt1)[2] <- "Ano"
Einvest_out_rnv <- dados_out_melt1





### OUTLIER VARIA?AO REAL
baseoutv <- function(base_variacaoreal_abs){
  melt_variacaoreal_abs <- melt(base_variacaoreal_abs, id.vars = "Cod.IBGE")
  outliers_v <- boxplot(melt_variacaoreal_abs$value)$out
  melt_variacaoreal_abs$Outlier <- 0
  melt_variacaoreal_abs$Outlier[melt_variacaoreal_abs$value %in% outliers_v] <- 1
  
  data_outliersv <- melt_variacaoreal_abs[melt_variacaoreal_abs$Outlier==1,]
  return(data_outliersv)
}
data_outliersv <- baseoutv(variacaoreal_abs)

melt_variacaoreal_abs_ano1 <- data_outliersv[,1:2]
melt_variacaoreal_abs_ano1$variable <- paste0("20",str_sub(melt_variacaoreal_abs_ano1$variable,
                                                           start = -4,end=-3))
melt_variacaoreal_abs_ano2 <- data_outliersv[,1:2]
melt_variacaoreal_abs_ano2$variable <- paste0("20",str_sub(melt_variacaoreal_abs_ano2$variable,
                                                           start = -2,end=-1))

Einvest_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")

Einvest_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")

Einvest_out_var <- join(Einvest_out_var1, Einvest_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
colnames(Einvest_out_var)[2] <- "Ano"

Einvest_out_vnv <- Einvest_out_var

###### Outliers
Einvest_outliers <- join(Einvest_out_rnv, Einvest_out_vnv, by=c("Cod.IBGE","Ano"), type="full")

Einvest_comp_p_imput <- dados
for (j in 1:dim(Einvest_outliers)[1]) {
  mun <- as.numeric(as.vector(Einvest_outliers[j,1]))
  ano <- as.numeric(as.vector(Einvest_outliers[j,2]))
  col <- ano-2006
  Einvest_comp_p_imput[Einvest_comp_p_imput$Cod.IBGE == mun, col] <- NA
}

Einvest_comp_p_imput2 <- Einvest_comp_p_imput


##### IMPUT 2 #####
dados <- Einvest_comp_p_imput2
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


dados_imput2 <- imput.ma(dados_comna)
dados_imputada2 <- rbind(dados_semna,dados_imput2)
dados_imputada2 <- rbind(dados_imputada2,dados_comna1)

Einvest_imput2 <-  dados_imputada2













##### MUN NOVOS ####
imput.ma2 <- function(basemissing){
  col <- as.numeric(dim(basemissing)[2])
  ano <- as.vector(basemissing[,col])
  d3 <- basemissing[,-col]
  imput <- basemissing[,-c(1,col)]
  for (i in 1:dim(imput)[1]) {
    ass <- ano[[i]]
    cols <- as.numeric(ass):2019
    x <- as.numeric(unlist(imput[i,colnames(imput) %in% as.character(cols)]))
    d3[i,as.character(cols)] <- na_ma(x)
  }
  return(d3)
}



dados <- Einvest_novos
dados_semna <- na.omit(dados) 
dados$Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
               "2008","2008")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais

dados_imput1 <- imput.ma2(dados_comna) 
dados_imputada1 <- rbind(dados_semna,dados_imput1)
Einvest_imput1.n <- dados_imputada1









#### DADOS FINAIS ####
Einvest_bruta <- rbind(Einvest, Einvest_novos)
Einvest_bruta <- Einvest_bruta[order(Einvest_bruta$Cod.IBGE),]
save(Einvest_bruta, file = "data/Einvest_bruta.RData")


Einvest_imput_missing <- rbind(Einvest_imput1, Einvest_imput1.n)
Einvest_imput_missing <- Einvest_imput_missing[order(Einvest_imput_missing$Cod.IBGE),]
save(Einvest_imput_missing, file = "data/Einvest_imput_missing.RData")



Einvest_outliers <- Einvest_outliers
colnames(Einvest_outliers)[3] <- "Valor"
Einvest_outliers <- Einvest_outliers[order(Einvest_outliers$Cod.IBGE,
                                           Einvest_outliers$Ano),]
save(Einvest_outliers, file = "data/Einvest_outliers.RData")


Einvest_final <- rbind(Einvest_imput2, Einvest_imput1.n)
Einvest_final <- Einvest_final[order(Einvest_final$Cod.IBGE),]
save(Einvest_final, file = "data/Einvest_final.RData")














