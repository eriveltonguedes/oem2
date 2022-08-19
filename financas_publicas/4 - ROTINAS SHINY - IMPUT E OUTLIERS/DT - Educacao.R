#### Gasto Total Educação
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

# 1- BU 03:20
# 2- SIOPE 08:19
# 3- FINBRA 03:20


### BASE FSE
load(file = "Dados/despeduc_basefs.RData")
despEduc_basefs <- join_all(despeduc_basefs,by = c("Cod.IBGE", "Ano"),type = "full")

############################### Despesa Total ############################### 
load("Dados/dados.reais.lista.RData")




# names(dados.reais.lista)
########## DADOS NOVOS ##############
despEduc_novos <- list()
dados_real <- list(dados.reais.lista[["Desp_Educ_BU"]], dados.reais.lista[["Desp.Total.Educacao.e"]], 
                   dados.reais.lista[["Desp.Total.Educacao.f"]])
municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")
for (j in 1:3) {
  despEduc_novos[[j]] <- dados_real[[j]][dados_real[[j]]$Cod.IBGE %in% municipios_novos_naousar[1:10],]
  despEduc_novos[[j]] <- despEduc_novos[[j]][order(despEduc_novos[[j]]$Cod.IBGE),]
  if(j!=2){
  despEduc_novos[[j]]$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
                                "2005","2005")
  }else{
    despEduc_novos[[j]]$Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
                                 "2008","2008")
  }
  despEduc_novos[[j]][is.na(despEduc_novos[[j]])] <- 0
  
  for (i in 1:dim(despEduc_novos[[j]])[1]) {
    ass <- despEduc_novos[[j]]$Ano[i]
    cols <- ass:2020
    cols2 <- c("Cod.IBGE",ass:2020)
    dados <- despEduc_novos[[j]][i,colnames(despEduc_novos[[j]]) %in% cols]
    dados[dados==0] <- NA
    despEduc_novos[[j]][i,colnames(despEduc_novos[[j]]) %in%cols] <- dados
    despEduc_novos[[j]][i,!(colnames(despEduc_novos[[j]]) %in% cols2)] <- 0
  }
  
  despEduc_novos[[j]] <- despEduc_novos[[j]][,!(colnames(despEduc_novos[[j]]) %in% "Ano")]
  dados_real[[j]] <- dados_real[[j]][!(dados_real[[j]]$Cod.IBGE %in% municipios_novos_naousar),]
  dados_real[[j]][dados_real[[j]]==0] <- NA
  
  
}


############## DADOS BRUTOS ##############
despEduc <- list()
despEduc[[1]] <- dados_real[[1]]
despEduc[[2]] <- dados_real[[2]]
despEduc[[3]] <- dados_real[[3]]


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

despEduc_imput1 <- list()

for (i in 1:3) {
  dados <- despEduc[[i]]
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
    despEduc_imput1[[i]] <-  dados_imputada1
  }else{despEduc_imput1[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    despEduc_imput1[[i]] <-  rbind(despEduc_imput1[[i]], dados_comna1)
  }
}



#### OUTLIERS #####
despEduc_outliers <- list()
despEduc_comp_p_imput2 <- list()

for (i in 1:3) {
  dados <- na.omit(despEduc_imput1[[i]])
  dados_comna <- subset(despEduc_imput1[[i]], !Cod.IBGE %in% dados$Cod.IBGE)
  col <- as.numeric(dim(dados)[2])
  
  ### Variacao
  
  variacaoreal_abs <- dados[,-col]
  
  for (j in 2:(col-1)) {
    variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
  }
  nomes <- c("Cod.IBGE", "despEduc0304","despEduc0405", "despEduc0506", "despEduc0607",
             "despEduc0708", "despEduc0809","despEduc0910", "despEduc1011", "despEduc1112",
             "despEduc1213", "despEduc1314", "despEduc1415","despEduc1516", "despEduc1617",
             "despEduc1718", "despEduc1819", "despEduc1920")
  
  if(i==2){
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
      print(j)
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
    mundespEduc <- base_real_semna$Cod.IBGE[!duplicated(base_real_semna$Cod.IBGE)]
    cod.mun_com_despEduc_outlier_r <- mundespEduc[municipios_comoutlier_r]
    data_outliers$Cod.IBGE <- cod.mun_com_despEduc_outlier_r
    data_outliers
    
    return(data_outliers)
  }
  data_outliers <- baseoutr(dados, dados_outlier_r)
  
  if(i ==2){
    anos <- 2008:2019
  }else{  anos <- 2003:2020}
  
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
    v1 <- join(dados_out_melt1, despEduc_basefs[,c("Cod.IBGE","Ano", "Soma")], by=c("Cod.IBGE","Ano"), type="left")
    v1$value[v1$Soma>1] <- NA
    v1 <- v1[,-4]
    despEduc_out_rnv <- na.omit(v1)
  }else{despEduc_out_rnv <- dados_out_melt1}
  
  
  
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
  
  despEduc_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                             by=c("Cod.IBGE", "variable"), type="left")
  
  despEduc_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                             by=c("Cod.IBGE", "variable"), type="left")
  
  despEduc_out_var <- join(despEduc_out_var1, despEduc_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
  colnames(despEduc_out_var)[2] <- "Ano"
  
  ### Validaçao
  if(i==1){
    v2 <- join(despEduc_out_var, despEduc_basefs[,c("Cod.IBGE","Ano", "Soma")], by=c("Cod.IBGE","Ano"), type="left")
    v2$value[v2$Soma>1] <- NA
    v2 <- v2[,-4]
    despEduc_out_vnv <- na.omit(v2)
  }else{despEduc_out_vnv <- despEduc_out_var}
  
  ###### Outliers
  despEduc_outliers[[i]] <- join(despEduc_out_rnv, despEduc_out_vnv, by=c("Cod.IBGE","Ano"), type="full")
  
  
  despEduc_comp_p_imput <- dados
  for (j in 1:dim(despEduc_outliers[[i]])[1]) {
    mun <- as.numeric(as.vector(despEduc_outliers[[i]][j,1]))
    ano <- as.numeric(as.vector(despEduc_outliers[[i]][j,2]))
    if(i==2){col <- ano-2006}else{col <- ano-2001}
    despEduc_comp_p_imput[despEduc_comp_p_imput$Cod.IBGE == mun, col] <- NA
  }
  
  despEduc_comp_p_imput2[[i]] <- despEduc_comp_p_imput
  
  if(dim(dados_comna)[1]>0){
    despEduc_comp_p_imput2[[i]] <-  rbind(despEduc_comp_p_imput2[[i]], dados_comna)
  }
  
  print(i)
}



##### IMPUT 2 #####
despEduc_imput2 <- list()
for (i in 1:3) {
  dados <- despEduc_comp_p_imput2[[i]]
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
    despEduc_imput2[[i]] <-  dados_imputada2
  }else{despEduc_imput2[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    despEduc_imput2[[i]] <-  rbind(despEduc_imput2[[i]],dados_comna1)
  }
}





#### DADOS NOVOS ####

despEduc.n <- list()
despEduc.n[[1]] <- despEduc_novos[[1]]
despEduc.n[[2]] <- despEduc_novos[[2]]
despEduc.n[[3]] <- despEduc_novos[[3]]

### IMPUT ###
imput.ma2 <- function(basemissing){
  col <- as.numeric(dim(basemissing)[2])
  ano <- as.vector(unlist(basemissing[,col]))
  d3 <- basemissing[,-col]
  imput <- basemissing[,-c(1,col)]
  ano[which(ano < as.numeric(colnames(imput)[1]))] <- as.numeric(colnames(imput)[1])
  for (i in 1:dim(imput)[1]) {
    ass <- as.numeric(ano[i])
    cols <- ass:2020
    x <- as.numeric(unlist(imput[i,colnames(imput) %in% as.character(cols)]))
    d3[i,colnames(d3) %in% as.character(cols)] <- na_ma(x)
  }
  return(d3)
}

despEduc_imput1.n <- list()
for (i in 1:3) {
  dados <- despEduc.n[[i]]
  dados <- dados[order(dados$Cod.IBGE),]
  dados_semna <- na.omit(dados) 
  if(i!=2){
    dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
                                 "2005","2005")
  }else{
    dados$Ano <- c("2013","2008","2009","2013","2013","2013","2008","2013",
                                 "2008","2008")
  }
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais
  if(dim(dados_comna)[1]>0){
    dados_imput1 <- imput.ma2(dados_comna) 
    dados_imputada1 <- rbind(dados_semna,dados_imput1)
    despEduc_imput1.n[[i]] <-  dados_imputada1
  }else{despEduc_imput1.n[[i]] <- dados_semna}
}






#### DADOS FINAIS ####
despEduc_bruta <- list()
for (i in 1:3) {
  despEduc_bruta[[i]] <- rbind(despEduc[[i]], despEduc.n[[i]])
  despEduc_bruta[[i]] <- despEduc_bruta[[i]][order(despEduc_bruta[[i]]$Cod.IBGE),]
}

save(despEduc_bruta, file = "data/despEduc_bruta.RData")


despEduc_imput_missing <- list()
for (i in 1:3) {
  despEduc_imput_missing[[i]] <- rbind(despEduc_imput1[[i]], despEduc_imput1.n[[i]])
  despEduc_imput_missing[[i]] <- despEduc_imput_missing[[i]][order(despEduc_imput_missing[[i]]$Cod.IBGE),]
}

save(despEduc_imput_missing, file = "data/despEduc_imput_missing.RData")

for (i in 1:3) {
  colnames(despEduc_outliers[[i]])[3] <- "Valor"
  despEduc_outliers[[i]] <- despEduc_outliers[[i]][order(despEduc_outliers[[i]]$Cod.IBGE),]
}
save(despEduc_outliers, file = "data/despEduc_outliers.RData")



despEduc_p_imput_out <- list()
for (i in 1:3) {
  despEduc_p_imput_out[[i]] <- rbind(despEduc_comp_p_imput2[[i]], despEduc_imput1.n[[i]])
  despEduc_p_imput_out[[i]] <- despEduc_p_imput_out[[i]][order(despEduc_p_imput_out[[i]]$Cod.IBGE),]
}

save(despEduc_p_imput_out, file = "Dados/despEduc_p_imput_out.RData")




despEduc_final <- list()
for (i in 1:3) {
  despEduc_final[[i]] <- rbind(despEduc_imput2[[i]], despEduc_imput1.n[[i]])
  despEduc_final[[i]] <- despEduc_final[[i]][order(despEduc_final[[i]]$Cod.IBGE),]
}

save(despEduc_final, file = "data/despEduc_final.RData")








