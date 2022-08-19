#### Gasto Total Saúde
library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(miceadds)
library(stringr)
library(imputeTS)

# 1- BU 03:20
# 2- SIOPS 03:20
# 3- FINBRA 03:20


### BASE FSE
load(file = "Dados/despsaude_basefs.RData")
despSaude_basefs <- join_all(despsaude_basefs,by = c("Cod.IBGE", "Ano"),type = "full")


############################### Despesa Total ############################### 
load("Dados/dados.reais.lista.RData")

dados_real <- list(dados.reais.lista[["Desp_Saude_BU"]], dados.reais.lista[["Desp.Total.Saude.s"]],
                   dados.reais.lista[["Desp.Total.Saude.f"]])



########## DADOS NOVOS ##############
despSaude_novos <- list()
municipios_novos_naousar <- c("220095", "500390", "510452", "510454", 
                              "220672", "150475", "421265", "422000", 
                              "431454", "500627", "530010")
for (j in 1:3) {
  
despSaude_novos[[j]] <- dados_real[[j]][dados_real[[j]]$Cod.IBGE %in% municipios_novos_naousar[-11],]
despSaude_novos[[j]] <- despSaude_novos[[j]][order(despSaude_novos[[j]]$Cod.IBGE),]
despSaude_novos[[j]]$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
         "2005","2005")
despSaude_novos[[j]][is.na(despSaude_novos[[j]])] <- 0

for (i in 1:dim(despSaude_novos[[j]])[1]) {
  ass <- despSaude_novos[[j]]$Ano[i]
  cols <- ass:2020
  cols2 <- c("Cod.IBGE",ass:2020)
  dados <- despSaude_novos[[j]][i,colnames(despSaude_novos[[j]]) %in% cols]
  dados[dados==0] <- NA
  despSaude_novos[[j]][i,colnames(despSaude_novos[[j]]) %in%cols] <- dados
  despSaude_novos[[j]][i,!(colnames(despSaude_novos[[j]]) %in% cols2)] <- 0
}

despSaude_novos[[j]] <- despSaude_novos[[j]][,!(colnames(despSaude_novos[[j]]) %in% "Ano")]
dados_real[[j]] <- dados_real[[j]][!(dados_real[[j]]$Cod.IBGE %in% municipios_novos_naousar),]
dados_real[[j]][dados_real[[j]]==0] <- NA


}




############## DADOS BRUTOS ##############
despSaude <- list()
despSaude[[1]] <- dados_real[[1]]
despSaude[[2]] <- dados_real[[2]]
despSaude[[3]] <- dados_real[[3]]


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

despSaude_imput1 <- list()

for (i in 1:3) {
  dados <- despSaude[[i]]
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
    despSaude_imput1[[i]] <-  dados_imputada1
  }else{despSaude_imput1[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    despSaude_imput1[[i]] <-  rbind(despSaude_imput1[[i]], dados_comna1)
  }
}


#### OUTLIERS #####
despSaude_outliers <- list()
despSaude_comp_p_imput2 <- list()

for (i in 1:3) {
  dados <- na.omit(despSaude_imput1[[i]])
  dados_comna <- subset(despSaude_imput1[[i]], !Cod.IBGE %in% dados$Cod.IBGE)
  col <- as.numeric(dim(dados)[2])
  
  ### Variacao
  
  variacaoreal_abs <- dados[,-col]
  
  for (j in 2:(col-1)) {
    variacaoreal_abs[,j] <- abs((dados[,j+1]/dados[,j])-1)
  }
  nomes <- c("Cod.IBGE","despSaude0304", "despSaude0405", "despSaude0506", "despSaude0607",
             "despSaude0708", "despSaude0809","despSaude0910", "despSaude1011", "despSaude1112",
             "despSaude1213", "despSaude1314", "despSaude1415","despSaude1516", "despSaude1617",
             "despSaude1718", "despSaude1819", "despSaude1920")
  
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
    mundespSaude <- base_real_semna$Cod.IBGE[!duplicated(base_real_semna$Cod.IBGE)]
    cod.mun_com_despSaude_outlier_r <- mundespSaude[municipios_comoutlier_r]
    data_outliers$Cod.IBGE <- cod.mun_com_despSaude_outlier_r
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
  
  
  if(i==1){
    #### Validação
    v1 <- join(dados_out_melt1, despSaude_basefs[,c("Cod.IBGE","Ano", "Soma")], by=c("Cod.IBGE","Ano"), type="left")
    v1$value[v1$Soma>1] <- NA
    v1 <- v1[,-4]
    despSaude_out_rnv <- na.omit(v1)
  }else{despSaude_out_rnv <- dados_out_melt1}
  
  
  
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
  
  despSaude_out_var1 <- join(melt_variacaoreal_abs_ano1, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")
  
  despSaude_out_var2 <- join(melt_variacaoreal_abs_ano2, melt(dados, id.vars = "Cod.IBGE"),
                         by=c("Cod.IBGE", "variable"), type="left")
  
  despSaude_out_var <- join(despSaude_out_var1, despSaude_out_var2,  by=c("Cod.IBGE", "variable"), type="full")
  colnames(despSaude_out_var)[2] <- "Ano"
  
  ### Validaçao
  if(i==1){
    v2 <- join(despSaude_out_var, despSaude_basefs[,c("Cod.IBGE","Ano", "Soma")], by=c("Cod.IBGE","Ano"), type="left")
    v2$value[v2$Soma>1] <- NA
    v2 <- v2[,-4]
    despSaude_out_vnv <- na.omit(v2)
  }else{despSaude_out_vnv <- despSaude_out_var}
  
  ###### Outliers
  despSaude_outliers[[i]] <- join(despSaude_out_rnv, despSaude_out_vnv, by=c("Cod.IBGE","Ano"), type="full")
  
  
  despSaude_comp_p_imput <- dados
  for (j in 1:dim(despSaude_outliers[[i]])[1]) {
    mun <- as.numeric(as.vector(despSaude_outliers[[i]][j,1]))
    ano <- as.numeric(as.vector(despSaude_outliers[[i]][j,2]))
    col <- ano-2001
    despSaude_comp_p_imput[despSaude_comp_p_imput$Cod.IBGE == mun, col] <- NA
  }
  
  despSaude_comp_p_imput2[[i]] <- despSaude_comp_p_imput
  
  if(dim(dados_comna)[1]>0){
    despSaude_comp_p_imput2[[i]] <-  rbind(despSaude_comp_p_imput2[[i]], dados_comna)
  }
  
  print(i)
}



##### IMPUT 2 #####
despSaude_imput2 <- list()
for (i in 1:3) {
  dados <- despSaude_comp_p_imput2[[i]]
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
    despSaude_imput2[[i]] <-  dados_imputada2
  }else{despSaude_imput2[[i]] <- dados_semna}
  if(dim(dados_comna1)[1]>0){
    despSaude_imput2[[i]] <-  rbind(despSaude_imput2[[i]],dados_comna1)
  }
}





#### DADOS NOVOS ####

despSaude.n <- list()
despSaude.n[[1]] <- despSaude_novos[[1]]
despSaude.n[[2]] <- despSaude_novos[[2]]
despSaude.n[[3]] <- despSaude_novos[[3]]

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
    d3[i,as.character(cols)] <- na_ma(x)
  }
  return(d3)
}

despSaude_imput1.n <- list()
for (i in 1:3) {
  dados <- despSaude.n[[i]]
  dados <- dados[order(dados$Cod.IBGE),]
  dados_semna <- na.omit(dados) 
  dados$Ano <- c("2013","2005","2009","2013","2013","2013","2005","2013",
                 "2005","2005")
  dados_comna <- subset(dados, !Cod.IBGE %in% dados_semna$Cod.IBGE) #1 coluna a mais
  if(dim(dados_comna)[1]>0){
    dados_imput1 <- imput.ma2(dados_comna) 
    dados_imputada1 <- rbind(dados_semna,dados_imput1)
    despSaude_imput1.n[[i]] <-  dados_imputada1
  }else{despSaude_imput1.n[[i]] <- dados_semna}
}






#### DADOS FINAIS ####
despSaude_bruta <- list()
for (i in 1:3) {
  despSaude_bruta[[i]] <- rbind(despSaude[[i]], despSaude.n[[i]])
  despSaude_bruta[[i]] <- despSaude_bruta[[i]][order(despSaude_bruta[[i]]$Cod.IBGE),]
}

save(despSaude_bruta, file = "data/despSaude_bruta.RData")


despSaude_imput_missing <- list()
for (i in 1:3) {
  despSaude_imput_missing[[i]] <- rbind(despSaude_imput1[[i]], despSaude_imput1.n[[i]])
  despSaude_imput_missing[[i]] <- despSaude_imput_missing[[i]][order(despSaude_imput_missing[[i]]$Cod.IBGE),]
}

save(despSaude_imput_missing, file = "data/despSaude_imput_missing.RData")

for (i in 1:3) {
  colnames(despSaude_outliers[[i]])[3] <- "Valor"
  despSaude_outliers[[i]] <- despSaude_outliers[[i]][order(despSaude_outliers[[i]]$Cod.IBGE),]
}
save(despSaude_outliers, file = "data/despSaude_outliers.RData")



despSaude_p_imput_out <- list()
for (i in 1:3) {
  despSaude_p_imput_out[[i]] <- rbind(despSaude_comp_p_imput2[[i]], despSaude_imput1.n[[i]])
  despSaude_p_imput_out[[i]] <- despSaude_p_imput_out[[i]][order(despSaude_p_imput_out[[i]]$Cod.IBGE),]
}

save(despSaude_p_imput_out, file = "Dados/despSaude_p_imput_out.RData")




despSaude_final <- list()
for (i in 1:3) {
  despSaude_final[[i]] <- rbind(despSaude_imput2[[i]], despSaude_imput1.n[[i]])
  despSaude_final[[i]] <- despSaude_final[[i]][order(despSaude_final[[i]]$Cod.IBGE),]
}

save(despSaude_final, file = "data/despSaude_final.RData")








