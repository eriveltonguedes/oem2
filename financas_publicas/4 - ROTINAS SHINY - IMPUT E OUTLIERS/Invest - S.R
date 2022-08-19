#### Sinvestimentos SAUDE
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

Sinvest<- dados.reais.lista[["Saude.Invest.s"]]

municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Sinvest_novos <- Sinvest[Sinvest$Cod.IBGE %in% municipios_novos_naousar[-11],]
Sinvest_novos <- Sinvest_novos[order(Sinvest_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
Sinvest_novos[is.na(Sinvest_novos)] <- 0

for (i in 1:dim(Sinvest_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  dados <- Sinvest_novos[i,colnames(Sinvest_novos) %in%cols]
  dados[dados==0] <- NA
  Sinvest_novos[i,colnames(Sinvest_novos) %in%cols] <- dados
}

Sinvest <- Sinvest[!(Sinvest$Cod.IBGE %in% municipios_novos_naousar),]
Sinvest[Sinvest==0] <- NA

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


dados <- Sinvest
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
Sinvest_imput1 <-  dados_imputada1


#### OUTLIERS #####

dados <- Sinvest_imput1
col <- as.numeric(dim(dados)[2])

### Variacao

variacaoreal_abs <- dados[,-col]

for (j in 2:(col-1)) {
  variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
}
nomes <- c("Cod.IBGE",
           "Sinvest0304", 
           "Sinvest0405", "Sinvest0506", "Sinvest0607",
           "Sinvest0708", "Sinvest0809","Sinvest0910", "Sinvest1011", "Sinvest1112",
           "Sinvest1213", "Sinvest1314", "Sinvest1415","Sinvest1516", "Sinvest1617",
           "Sinvest1718", "Sinvest1819", "Sinvest1920")

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

anos <- 2003:2020

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
Sinvest_out_rnv <- dados_out_melt1





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

Sinvest_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                        by=c("Cod.IBGE", "variable"), type="left")

Sinvest_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                        by=c("Cod.IBGE", "variable"), type="left")

Sinvest_out_var <- join(Sinvest_out_var1, Sinvest_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
colnames(Sinvest_out_var)[2] <- "Ano"

Sinvest_out_vnv <- Sinvest_out_var

###### Outliers
Sinvest_outliers <- join(Sinvest_out_rnv, Sinvest_out_vnv, by=c("Cod.IBGE","Ano"), type="full")

Sinvest_comp_p_imput <- dados
for (j in 1:dim(Sinvest_outliers)[1]) {
  mun <- as.numeric(as.vector(Sinvest_outliers[j,1]))
  ano <- as.numeric(as.vector(Sinvest_outliers[j,2]))
  col <- ano-2001
  Sinvest_comp_p_imput[Sinvest_comp_p_imput$Cod.IBGE == mun, col] <- NA
}

Sinvest_comp_p_imput2 <- Sinvest_comp_p_imput


##### IMPUT 2 #####
dados <- Sinvest_comp_p_imput2
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


dados_imput2 <- imput.ma(dados_comna)
dados_imputada2 <- rbind(dados_semna,dados_imput2)
# dados_imputada2 <- rbind(dados_imputada2,dados_comna1)

Sinvest_imput2 <-  dados_imputada2













##### MUN NOVOS ####
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



dados <- Sinvest_novos
dados_semna <- na.omit(dados) 
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais

dados_imput1 <- imput.ma2(dados_comna) 
dados_imputada1 <- rbind(dados_semna,dados_imput1)
Sinvest_imput1.n <- dados_imputada1









#### DADOS FINAIS ####
Sinvest_bruta <- rbind(Sinvest, Sinvest_novos)
Sinvest_bruta <- Sinvest_bruta[order(Sinvest_bruta$Cod.IBGE),]
save(Sinvest_bruta, file = "data/Sinvest_bruta.RData")


Sinvest_imput_missing <- rbind(Sinvest_imput1, Sinvest_imput1.n)
Sinvest_imput_missing <- Sinvest_imput_missing[order(Sinvest_imput_missing$Cod.IBGE),]
save(Sinvest_imput_missing, file = "data/Sinvest_imput_missing.RData")



Sinvest_outliers <- Sinvest_outliers
colnames(Sinvest_outliers)[3] <- "Valor"
Sinvest_outliers <- Sinvest_outliers[order(Sinvest_outliers$Cod.IBGE,
                                           Sinvest_outliers$Ano),]
save(Sinvest_outliers, file = "data/Sinvest_outliers.RData")


Sinvest_final <- rbind(Sinvest_imput2, Sinvest_imput1.n)
Sinvest_final <- Sinvest_final[order(Sinvest_final$Cod.IBGE),]
save(Sinvest_final, file = "data/Sinvest_final.RData")














