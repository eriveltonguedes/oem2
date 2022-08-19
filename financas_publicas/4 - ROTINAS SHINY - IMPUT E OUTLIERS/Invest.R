#### Investimentos
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

invest <- dados.reais.lista[["Invest.f"]]

municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")

invest_novos <- invest[invest$Cod.IBGE %in% municipios_novos_naousar,]
invest_novos <- invest_novos[order(invest_novos$Cod.IBGE),]
Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
invest_novos[is.na(invest_novos)] <- 0

for (i in 1:dim(invest_novos)[1]) {
  ass <- Ano[i]
  cols <- ass:2020
  dados <- invest_novos[i,colnames(invest_novos) %in%cols]
  dados[dados==0] <- NA
  invest_novos[i,colnames(invest_novos) %in%cols] <- dados
}

invest <- invest[!(invest$Cod.IBGE %in% municipios_novos_naousar),]
invest[invest==0] <- NA


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


dados <- invest
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
invest_imput1 <-  dados_imputada1


#### OUTLIERS #####

dados <- invest_imput1
col <- as.numeric(dim(dados)[2])

### Variacao

variacaoreal_abs <- dados[,-col]

for (j in 2:(col-1)) {
  variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
}
nomes <- c("Cod.IBGE",
           "invest0304", 
           "invest0405", "invest0506", "invest0607",
           "invest0708", "invest0809","invest0910", "invest1011", "invest1112",
           "invest1213", "invest1314", "invest1415","invest1516", "invest1617",
           "invest1718", "invest1819", "invest1920")

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
invest_out_rnv <- dados_out_melt1





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

invest_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                            by=c("Cod.IBGE", "variable"), type="left")

invest_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                            by=c("Cod.IBGE", "variable"), type="left")

invest_out_var <- join(invest_out_var1, invest_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
colnames(invest_out_var)[2] <- "Ano"

invest_out_vnv <- invest_out_var

###### Outliers
invest_outliers <- join(invest_out_rnv, invest_out_vnv, by=c("Cod.IBGE","Ano"), type="full")

invest_comp_p_imput <- dados
for (j in 1:dim(invest_outliers)[1]) {
  mun <- as.numeric(as.vector(invest_outliers[j,1]))
  ano <- as.numeric(as.vector(invest_outliers[j,2]))
  col <- ano-2001
  invest_comp_p_imput[invest_comp_p_imput$Cod.IBGE == mun, col] <- NA
}

invest_comp_p_imput2 <- invest_comp_p_imput


##### IMPUT 2 #####
dados <- invest_comp_p_imput2
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

invest_imput2 <-  dados_imputada2













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



dados <- invest_novos
dados_semna <- na.omit(dados) 
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais

dados_imput1 <- imput.ma2(dados_comna) 
dados_imputada1 <- rbind(dados_semna,dados_imput1)
invest_imput1.n <- dados_imputada1









#### DADOS FINAIS ####
invest_bruta <- rbind(invest, invest_novos)
invest_bruta <- invest_bruta[order(invest_bruta$Cod.IBGE),]
save(invest_bruta, file = "data/invest_bruta.RData")


invest_imput_missing <- rbind(invest_imput1, invest_imput1.n)
invest_imput_missing <- invest_imput_missing[order(invest_imput_missing$Cod.IBGE),]
save(invest_imput_missing, file = "data/invest_imput_missing.RData")



invest_outliers <- invest_outliers
colnames(invest_outliers)[3] <- "Valor"
invest_outliers <- invest_outliers[order(invest_outliers$Cod.IBGE),]
save(invest_outliers, file = "data/invest_outliers.RData")


invest_final <- rbind(invest_imput2, invest_imput1.n)
invest_final <- invest_final[order(invest_final$Cod.IBGE),]
save(invest_final, file = "data/invest_final.RData")













