### Receitas Totais ###
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

# 1- BU
# 2- SIOPS
# 3- SIOPE
# 4- FINBRA

############################### Receitas Correntes ############################### 
load("Dados/dados.reais.lista.RData")

rcorr <- list()
rcorr[[1]] <- dados.reais.lista[[1]]
rcorr[[2]] <- dados.reais.lista[[2]]
rcorr[[3]] <- dados.reais.lista[[3]]
rcorr[[4]] <- dados.reais.lista[[4]]
names(rcorr) <- names(dados.reais.lista)[1:4]

rcorr_novos <- list()
for (j in 1:4) {
  municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                                "220672", "150475", "421265", "422000", 
                                "431454", "500627", "530010")
  
  rcorr_novos[[j]] <- rcorr[[j]][rcorr[[j]]$Cod.IBGE %in% municipios_novos_naousar,]
  rcorr_novos[[j]] <- rcorr_novos[[j]][order(rcorr_novos[[j]]$Cod.IBGE),]
  Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
           "2005","2005")
  for (i in 1:dim(rcorr_novos[[j]])[1]) {
    ass <- Ano[i]
    cols <- ass:2020
    dados <- rcorr_novos[[j]][i,colnames(rcorr_novos[[j]]) %in%cols]
    dados[dados==0] <- NA
    rcorr_novos[[j]][i,colnames(rcorr_novos[[j]]) %in% cols] <- dados
  }
  
  rcorr[[j]] <- rcorr[[j]][!(rcorr[[j]]$Cod.IBGE %in% municipios_novos_naousar),]
  rcorr[[j]][rcorr[[j]]==0] <- NA
}

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

rcorr_imput1 <- list()

for (i in 1:4) {
  dados <- rcorr[[i]]
  dados[dados==0] <- NA
  dados_semna <- na.omit(dados)
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
  if(dim(dados_comna)[1]>0){
    dados_imput1 <- imput.ma(dados_comna)
    dados_imputada1 <- rbind(dados_semna,dados_imput1)
    rcorr_imput1[[i]] <-  dados_imputada1
  }else{rcorr_imput1[[i]] <- dados_semna}

}


#### OUTLIERS #####
rcorr_outliers <- list()
rcorr_comp_p_imput2 <- list()

for (i in 1:4) {
  dados <- rcorr_imput1[[i]]
  col <- as.numeric(dim(dados)[2])

  ### Variacao

  variacaoreal_abs <- dados[,-col]
  
  for (j in 2:(col-1)) {
    variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
  }
  nomes <- c("Cod.IBGE","rcorr0304", "rcorr0405", "rcorr0506", "rcorr0607",
                                        "rcorr0708", "rcorr0809","rcorr0910", "rcorr1011", "rcorr1112",
                                        "rcorr1213", "rcorr1314", "rcorr1415","rcorr1516", "rcorr1617",
                                        "rcorr1718", "rcorr1819", "rcorr1920")
  print(i)
  if(i==3){
  colnames(variacaoreal_abs) <- nomes[-c(2:6,18)]
  }else{colnames(variacaoreal_abs) <- nomes}
  
  
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
  
  if(i!=3){
  anos <- 2003:2020
  }else{  anos <- 2008:2019}
  
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
  
  
  if(i==1){
  #### Validação
  load(file = "Dados/rcorr_basefse.RData")
  
  v1 <- join(dados_out_melt1, rcorr_basefse[[10]][,c(1,2,18)], by=c("Cod.IBGE","Ano"), type="left")
  v1$value[v1$Soma>1] <- NA
  v1 <- v1[,-4]
  rcorr_out_rnv <- na.omit(v1)
  }else{rcorr_out_rnv <- dados_out_melt1}
  

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
  
  rcorr_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")
  
  rcorr_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")
  
  rcorr_out_var <- join(rcorr_out_var1, rcorr_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
  colnames(rcorr_out_var)[2] <- "Ano"
  
  ### Validaçao
  if(i==1){
  v2 <- join(rcorr_out_var, rcorr_basefse[[10]][,c(1,2,18)], by=c("Cod.IBGE","Ano"), type="left")
  v2$value[v2$Soma>1] <- NA
  v2 <- v2[,-4]
  rcorr_out_vnv <- na.omit(v2)
  }else{rcorr_out_vnv <- rcorr_out_var}
  
  ###### Outliers
  rcorr_outliers[[i]] <- join(rcorr_out_rnv, rcorr_out_vnv, by=c("Cod.IBGE","Ano"), type="full")
  
  
  rcorr_comp_p_imput <- dados
  for (j in 1:dim(rcorr_outliers[[i]])[1]) {
    mun <- as.numeric(as.vector(rcorr_outliers[[i]][j,1]))
    ano <- as.numeric(as.vector(rcorr_outliers[[i]][j,2]))
    if(i!=3){col <- ano-2001}else{col <- ano-2006}
    rcorr_comp_p_imput[rcorr_comp_p_imput$Cod.IBGE == mun, col] <- NA
  }
  
rcorr_comp_p_imput2[[i]] <- rcorr_comp_p_imput
  print(i)
}








##### IMPUT 2 #####
rcorr_imput2 <- list()
for (i in 1:4) {
  dados <- rcorr_comp_p_imput2[[i]]
  dados[dados==0] <- NA
  dados_semna <- na.omit(dados)
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
  a <- list()
  d <- vector()
  for (j in 1:dim(dados_comna)[1]) {
    a[[j]] <- which(is.na(dados_comna[j,]))
    d[j] <- length(a[[j]])
  }
  
  tamanho <- as.numeric(dim(dados)[2])
  minimo <- tamanho - 2
  dados_comna1 <- dados_comna[d>=minimo,]
  dados_comna <- dados_comna[d<minimo,]

  if(dim(dados_comna)[1]>0){
    dados_imput2 <- imput.ma(dados_comna)
    dados_imputada2 <- rbind(dados_semna,dados_imput2)
    rcorr_imput2[[i]] <-  dados_imputada2
  }else{rcorr_imput2[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    rcorr_imput2[[i]] <-  rbind(rcorr_imput2[[i]],dados_comna1)
  }
}





#### DADOS NOVOS ####

rcorr.n <- rcorr_novos
rcorr.n[[3]]$`2020` <- NA

### IMPUT ###
imput.ma2 <- function(basemissing){
  col <- as.numeric(dim(basemissing)[2])
  ano <- basemissing[,col]
  d3 <- basemissing[,-col]
  imput <- basemissing[,-c(1,col)]
  ano[which(ano < as.numeric(colnames(imput)[1]))] <- as.numeric(colnames(imput)[1])
  for (i in 1:dim(imput)[1]) {
    ass <- ano[i]
    cols <- ass:as.numeric(colnames(imput)[dim(imput)[2]])
    x <- as.numeric(unlist(imput[i,colnames(imput) %in% as.character(cols)]))
    d3[i,as.character(cols)] <- na_ma(x)
  }
  return(d3)
}

rcorr_imput1.n <- list()
for (i in 1:4) {
  dados <- rcorr.n[[i]]
  dados <- dados[order(dados$Cod.IBGE),]
  dados_semna <- na.omit(dados) 
  dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
                  "2005","2005")
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais
  if(dim(dados_comna)[1]>0){
    dados_imput1 <- imput.ma2(dados_comna) 
    dados_imputada1 <- rbind(dados_semna,dados_imput1)
    rcorr_imput1.n[[i]] <-  dados_imputada1
  }else{rcorr_imput1.n[[i]] <- dados_semna}
}


rcorr.n[[3]] <- rcorr.n[[3]][,-14]
rcorr_imput1.n[[3]] <- rcorr_imput1.n[[3]][,-14]



#### DADOS FINAIS ####
rcorr_bruta <- list()
for (i in 1:4) {
  rcorr_bruta[[i]] <- rbind(rcorr[[i]], rcorr.n[[i]])
  rcorr_bruta[[i]] <- rcorr_bruta[[i]][order(rcorr_bruta[[i]]$Cod.IBGE),]
}

save(rcorr_bruta, file = "data/rcorr_bruta.RData")


rcorr_imput_missing <- list()
for (i in 1:4) {
  rcorr_imput_missing[[i]] <- rbind(rcorr_imput1[[i]], rcorr_imput1.n[[i]])
  rcorr_imput_missing[[i]] <- rcorr_imput_missing[[i]][order(rcorr_imput_missing[[i]]$Cod.IBGE),]
}

save(rcorr_imput_missing, file = "data/rcorr_imput_missing.RData")

for (i in 1:4) {
  colnames(rcorr_outliers[[i]])[3] <- "Valor"
  rcorr_outliers[[i]] <- rcorr_outliers[[i]][order(rcorr_outliers[[i]]$Cod.IBGE),]
}
save(rcorr_outliers, file = "data/rcorr_outliers.RData")



rcorr_p_imput_out <- list()
for (i in 1:4) {
  rcorr_p_imput_out[[i]] <- rbind(rcorr_comp_p_imput2[[i]], rcorr_imput1.n[[i]])
  rcorr_p_imput_out[[i]] <- rcorr_p_imput_out[[i]][order(rcorr_p_imput_out[[i]]$Cod.IBGE),]
}

save(rcorr_p_imput_out, file = "Dados/rcorr_p_imput_out.RData")




rcorr_final <- list()
for (i in 1:4) {
  rcorr_final[[i]] <- rbind(rcorr_imput2[[i]], rcorr_imput1.n[[i]])
  rcorr_final[[i]] <- rcorr_final[[i]][order(rcorr_final[[i]]$Cod.IBGE),]
}

save(rcorr_final, file = "data/rcorr_final.RData")









############################### Receitas de Capital ############################### 
load("Dados/dados.reais.lista.RData")

rcap <- list()
rcap[[1]] <- dados.reais.lista[[5]]
rcap[[2]] <- dados.reais.lista[[6]]
rcap[[3]] <- dados.reais.lista[[7]]
rcap[[4]] <- dados.reais.lista[[8]]
names(rcap) <- names(dados.reais.lista)[5:8]

rcap_novos <- list()
for (j in 1:4) {
  municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                                "220672", "150475", "421265", "422000", 
                                "431454", "500627", "530010")
  
  rcap_novos[[j]] <- rcap[[j]][rcap[[j]]$Cod.IBGE %in% municipios_novos_naousar,]
  rcap_novos[[j]] <- rcap_novos[[j]][order(rcap_novos[[j]]$Cod.IBGE),]
  Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
           "2005","2005")
  for (i in 1:dim(rcap_novos[[j]])[1]) {
    ass <- Ano[i]
    cols <- ass:2020
    dados <- rcap_novos[[j]][i,colnames(rcap_novos[[j]]) %in%cols]
    dados[dados==0] <- NA
    rcap_novos[[j]][i,colnames(rcap_novos[[j]]) %in% cols] <- dados
  }
  
  rcap[[j]] <- rcap[[j]][!(rcap[[j]]$Cod.IBGE %in% municipios_novos_naousar),]
  rcap[[j]][rcap[[j]]==0] <- NA
}



##### IMPUT #####
rcap_imput1 <- list()

for (i in 1:4) {
  dados <- rcap[[i]]
  dados[dados==0] <- NA
  dados_semna <- na.omit(dados)
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
  a <- list()
  d <- vector()
  for (j in 1:dim(dados_comna)[1]) {
    a[[j]] <- which(is.na(dados_comna[j,]))
    d[j] <- length(a[[j]])
  }
  
  tamanho <- as.numeric(dim(dados)[2])
  minimo <- tamanho - 2
  dados_comna1 <- dados_comna[d>=minimo,]
  dados_comna <- dados_comna[d<minimo,]
  
  if(dim(dados_comna)[1]>0){
    dados_imput1 <- imput.ma(dados_comna)
    dados_imputada1 <- rbind(dados_semna,dados_imput1)
    rcap_imput1[[i]] <-  dados_imputada1
  }else{rcap_imput1[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    rcap_imput1[[i]] <-  rbind(rcap_imput1[[i]],dados_comna1)
  }
}




#### DADOS NOVOS ####


rcap.n <- rcap_novos

### IMPUT ###
rcap_imput1.n <- list()

dados <- rcap.n[[1]]
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
dados_imputada1[is.na(dados_imputada1)] <- 0
rcap_imput1.n[[1]] <-  dados_imputada1




dados <- rcap.n[[2]]
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
rcap_imput1.n[[2]] <-  dados_imputada1


dados <- rcap.n[[3]]
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
               "2008","2008")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_comna1 <- dados_comna[1,]
dados_comna <- dados_comna[-1,]
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
dados_imputada1 <- rbind(dados_imputada1,dados_comna1[,-14])

rcap_imput1.n[[3]] <-  dados_imputada1





dados <- rcap.n[[4]]
dados <- dados[order(dados$Cod.IBGE),]
dados_semna <- na.omit(dados)
dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
               "2005","2005")
dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE)
dados_imput1 <- imput.ma2(dados_comna)
dados_imputada1 <- rbind(dados_semna,dados_imput1)
rcap_imput1.n[[4]] <-  dados_imputada1










#### DADOS FINAIS ####
rcap_bruta <- list()
for (i in 1:4) {
  rcap_bruta[[i]] <- rbind(rcap[[i]], rcap.n[[i]])
  rcap_bruta[[i]] <- rcap_bruta[[i]][order(rcap_bruta[[i]]$Cod.IBGE),]
}

save(rcap_bruta, file = "data/rcap_bruta.RData")


rcap_imput_missing <- list()
for (i in 1:4) {
  rcap_imput_missing[[i]] <- rbind(rcap_imput1[[i]], rcap_imput1.n[[i]])
  rcap_imput_missing[[i]] <- rcap_imput_missing[[i]][order(rcap_imput_missing[[i]]$Cod.IBGE),]
}

save(rcap_imput_missing, file = "data/rcap_imput_missing.RData")






