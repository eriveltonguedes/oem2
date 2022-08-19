#### Consumo Intermediario - Saude
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
Scint <- dados.reais.lista[["Saude.CI.s"]]

municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

Scint_novos <- Scint[Scint$Cod.IBGE %in% municipios_novos_naousar[-11],]
Scint_novos <- Scint_novos[order(Scint_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
Scint_novos[is.na(Scint_novos)] <- 0

for (i in 1:dim(Scint_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  cols2 <- c("Cod.IBGE", cols)
  dados <- Scint_novos[i,colnames(Scint_novos) %in%cols]
  dados[dados==0] <- NA
  Scint_novos[i,colnames(Scint_novos) %in%cols] <- dados
  Scint_novos[i,!(colnames(Scint_novos) %in% cols2)] <- 0
  
}

Scint <- Scint[!(Scint$Cod.IBGE %in% municipios_novos_naousar),]
Scint[Scint==0] <- NA


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


dados <- Scint
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
Scint_imput1 <-  dados_imputada1


#### OUTLIERS #####

dados <- Scint_imput1
col <- as.numeric(dim(dados)[2])

### Variacao

variacaoreal_abs <- dados[,-col]

for (j in 2:(col-1)) {
  variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
}
nomes <- c("Cod.IBGE",
           "Scint0304", 
           "Scint0405", "Scint0506", "Scint0607",
           "Scint0708", "Scint0809","Scint0910", "Scint1011", "Scint1112",
           "Scint1213", "Scint1314", "Scint1415","Scint1516", "Scint1617",
           "Scint1718","Scint1819","Scint1920")

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
Scint_out_rnv <- dados_out_melt1





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

Scint_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                      by=c("Cod.IBGE", "variable"), type="left")

Scint_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                      by=c("Cod.IBGE", "variable"), type="left")

Scint_out_var <- join(Scint_out_var1, Scint_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
colnames(Scint_out_var)[2] <- "Ano"

Scint_out_vnv <- Scint_out_var

###### Outliers
Scint_outliers <- join(Scint_out_rnv, Scint_out_vnv, by=c("Cod.IBGE","Ano"), type="full")

Scint_comp_p_imput <- dados
for (j in 1:dim(Scint_outliers)[1]) {
  mun <- as.numeric(as.vector(Scint_outliers[j,1]))
  ano <- as.numeric(as.vector(Scint_outliers[j,2]))
  col <- ano-2001
  Scint_comp_p_imput[Scint_comp_p_imput$Cod.IBGE == mun, col] <- NA
}

Scint_comp_p_imput2 <- Scint_comp_p_imput


##### IMPUT 2 #####
dados <- Scint_comp_p_imput2
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

Scint_imput2 <-  dados_imputada2













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



dados <- Scint_novos
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais

dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
Scint_imput1.n <- dados[,-20]









#### DADOS FINAIS ####
Scint_bruta <- rbind(Scint, Scint_novos)
Scint_bruta <- Scint_bruta[order(Scint_bruta$Cod.IBGE),]
save(Scint_bruta, file = "data/Scint_bruta.RData")


Scint_imput_missing <- rbind(Scint_imput1, Scint_imput1.n)
Scint_imput_missing <- Scint_imput_missing[order(Scint_imput_missing$Cod.IBGE),]
save(Scint_imput_missing, file = "data/Scint_imput_missing.RData")


Scint_outliers <- Scint_outliers
colnames(Scint_outliers)[3] <- "Valor"
Scint_outliers <- Scint_outliers[order(Scint_outliers$Cod.IBGE,
                                       Scint_outliers$Ano),]
save(Scint_outliers, file = "data/Scint_outliers.RData")


Scint_final <- rbind(Scint_imput2, Scint_imput1.n)
Scint_final <- Scint_final[order(Scint_final$Cod.IBGE),]
save(Scint_final, file = "data/Scint_final.RData")













