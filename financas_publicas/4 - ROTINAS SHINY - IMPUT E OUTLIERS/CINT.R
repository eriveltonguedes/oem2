#### Consumo Intermediário
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

# 4- FINBRA
load("Dados/dados.reais.lista.RData")

cint <- dados.reais.lista[["CI.f"]]
municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")



cint_novos <- cint[cint$Cod.IBGE %in% municipios_novos_naousar,]
cint_novos <- cint_novos[order(cint_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
cint_novos[is.na(cint_novos)] <- 0

for (i in 1:dim(cint_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  dados <- cint_novos[i,colnames(cint_novos) %in%cols]
  dados[dados==0] <- NA
  cint_novos[i,colnames(cint_novos) %in%cols] <- dados
}

cint <- cint[!(cint$Cod.IBGE %in% municipios_novos_naousar),]
cint[cint==0] <- NA


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


dados <- cint
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
dados_imputada1 <- rbind(dados_imputada1, dados_comna1)
cint_imput1 <-  dados_imputada1


#### OUTLIERS #####

dados <- cint_imput1
col <- as.numeric(dim(dados)[2])

### Variacao

variacaoreal_abs <- dados[,-col]

for (j in 2:(col-1)) {
  variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
}
nomes <- c("Cod.IBGE",
           "cint0304", 
           "cint0405", "cint0506", "cint0607",
           "cint0708", "cint0809","cint0910", "cint1011", "cint1112",
           "cint1213", "cint1314", "cint1415","cint1516", "cint1617",
           "cint1718", "cint1819","cint1920")

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
cint_out_rnv <- dados_out_melt1





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

cint_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                        by=c("Cod.IBGE", "variable"), type="left")

cint_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                        by=c("Cod.IBGE", "variable"), type="left")

cint_out_var <- join(cint_out_var1, cint_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
colnames(cint_out_var)[2] <- "Ano"

cint_out_vnv <- cint_out_var

###### Outliers
cint_outliers <- join(cint_out_rnv, cint_out_vnv, by=c("Cod.IBGE","Ano"), type="full")

cint_comp_p_imput <- dados
for (j in 1:dim(cint_outliers)[1]) {
  mun <- as.numeric(as.vector(cint_outliers[j,1]))
  ano <- as.numeric(as.vector(cint_outliers[j,2]))
  col <- ano-2001
  cint_comp_p_imput[cint_comp_p_imput$Cod.IBGE == mun, col] <- NA
}

cint_comp_p_imput2 <- cint_comp_p_imput


##### IMPUT 2 #####
dados <- cint_comp_p_imput2
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
# dados_imputada2 <- rbind(dados_imputada2,dados_comna1)

cint_imput2 <-  dados_imputada2













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



dados <- cint_novos
dados_semna <- na.omit(dados) 
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais

dados_imput1 <- imput.ma2(dados_comna) 
dados_imputada1 <- rbind(dados_semna,dados_imput1)
cint_imput1.n <- dados_imputada1









#### DADOS FINAIS ####
cint_bruta <- rbind(cint, cint_novos)
cint_bruta <- cint_bruta[order(cint_bruta$Cod.IBGE),]
save(cint_bruta, file = "data/cint_bruta.RData")


cint_imput_missing <- rbind(cint_imput1, cint_imput1.n)
cint_imput_missing <- cint_imput_missing[order(cint_imput_missing$Cod.IBGE),]
save(cint_imput_missing, file = "data/cint_imput_missing.RData")



cint_outliers <- cint_outliers
colnames(cint_outliers)[3] <- "Valor"
cint_outliers <- cint_outliers[order(cint_outliers$Cod.IBGE),]
save(cint_outliers, file = "data/cint_outliers.RData")


cint_final <- rbind(cint_imput2, cint_imput1.n)
cint_final <- cint_final[order(cint_final$Cod.IBGE),]
save(cint_final, file = "data/cint_final.RData")













