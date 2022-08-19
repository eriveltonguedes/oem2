##################### DESPESA TOTAL COM SAUDE E EDUCACAO ##################### 
##################### LISTAS BASEFSE ##################### 
larquivos <- list.files("DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC", full.names = T)
dados <- list()
dados_melt <- list()

for (i in 1:4) {load(file = larquivos[i])}
def <- melt(Despesa_Total_Educacao_Finbra_0320, id.vars = "Cod.IBGE", 
            value.name = "Educ.f", variable.name = "Ano")
dee <- melt(Despesa_Total_Educacao_SIOPE_0819, id.vars = "Cod.IBGE", 
            value.name = "Educ.e", variable.name = "Ano")

dsf <- melt(Despesa_Total_Saude_Finbra_0320, id.vars = "Cod.IBGE", 
            value.name = "Saude.f", variable.name = "Ano")
dss <- melt(Despesa_Total_Saude_SIOPS_0320, id.vars = "Cod.IBGE", 
            value.name = "Saude.s", variable.name = "Ano")


###### VALIDACAO DESPESA EDUCACAO
despeduc_val <- join(dee, def, by=c("Cod.IBGE","Ano"),  type="full", match="first")

###### VALIDACAO DESPESA SAUDE
despsaude_val <- join(dss, dsf, by=c("Cod.IBGE","Ano"),  type="full", match="first")

##################### DESPESA TOTAL EDUCACAO ##################### 

tabela <- list(despsaude_val, despeduc_val)
head(despsaude_val)
head(despeduc_val)

val  <- function(tabela) { #"SIOPS","FINBRA"
  basefs <- tabela
  colnames(basefs)[3:4]<- c("SIOPS","FINBRA")
  
  # 2003 a 2020
  basefs$FS <- (basefs$FINBRA/basefs$SIOPS)-1
  basefs$SF <- (basefs$SIOPS/basefs$FINBRA)-1
  
  basefs$FS <- as.numeric(basefs$FS*100)
  basefs$CompatFS <- 0
  basefs$CompatFS[abs(basefs$FS)<15] <- 1
  
  basefs$SF <- as.numeric(basefs$SF*100)
  basefs$CompatSF <- 0
  basefs$CompatSF[abs(basefs$SF)<15] <- 1
  
  
  soma <- function(x){return(sum(x,na.rm = T))}
  basefs$Soma <- apply(basefs[,7:8],1,FUN = soma)
  
  
  
  library(dplyr)
  basefs_tipo1 <- basefs[is.na(basefs$FINBRA) & is.na(basefs$SIOPS),]
  basefs_tipo3 <- basefs[!is.na(basefs$FINBRA) & !is.na(basefs$SIOPS),]
  basefs_tipo2 <- anti_join(basefs, basefs_tipo1, by=c("Cod.IBGE","Ano"))
  basefs_tipo2 <- anti_join(basefs_tipo2, basefs_tipo3, by=c("Cod.IBGE","Ano"))
  
  
  return(list(basefs_tipo1,basefs_tipo2,basefs_tipo3, basefs))
}

for (i in 1:2) {
for (j in 1:dim(tabela[[i]])[2]) {
  tabela[[i]][which(tabela[[i]][,j]==0),j] <- NA
}
}


despsaude_basefs <- val(tabela[[1]])
despeduc_basefs <- val(tabela[[2]])

for (i in 1:4) {
  colnames(despeduc_basefs[[i]]) <- c("Cod.IBGE", "Ano", "SIOPE", "FINBRA", "FE", "EF","CompatFE", 
                                 "CompatEF", "Soma")
}




save(despeduc_basefs, file = "BASES UNICAS/LISTAS BASEFSE/despeduc_basefs.RData")
save(despsaude_basefs, file = "BASES UNICAS/LISTAS BASEFSE/despsaude_basefs.RData")


save(despeduc_basefs, file = "~/Documents/IPEA/SHINY_FINAL/Dados/despeduc_basefs.RData")
save(despsaude_basefs, file = "~/Documents/IPEA/SHINY_FINAL/Dados/despsaude_basefs.RData")


##################### BASE UNICA ##################### 
######## DESP SAUDE ###########
load(file = "BASES UNICAS/LISTAS BASEFSE/despeduc_basefs.RData")
load(file = "BASES UNICAS/LISTAS BASEFSE/despsaude_basefs.RData")

for (i in 1:4) {
  colnames(despeduc_basefs[[i]]) <- c("Cod.IBGE", "Ano", "SIOPS", "FINBRA", "FS", "SF","CompatFS", 
                                      "CompatSF", "Soma")
}

consol <- function(lista) {

  consolidada_t1 <- lista[[1]][,c(1,2,3)]
  names(consolidada_t1) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  consolidada_t2 <- lista[[2]][,c(1:4)]
  consolidada_t2$FINBRA[which(is.na(consolidada_t2$FINBRA))] <- 0
  consolidada_t2$SIOPS[which(is.na(consolidada_t2$SIOPS))] <- 0
  consolidada_t2$RP_consolidada <- consolidada_t2$FINBRA + consolidada_t2$SIOPS
  consolidada_t2 <- consolidada_t2[,c(1,2,5)]
  names(consolidada_t2) <- c("Cod.IBGE","Ano","RP_consolidada")
  dim(consolidada_t2)
  RP_consolidada <- rbind(consolidada_t1, consolidada_t2)
  
  consolidada_t3_comp <- subset(lista[[3]], Soma %in% c("2"))
  consolidada_t3_comp$RP_consolidada <- (consolidada_t3_comp$FINBRA + consolidada_t3_comp$SIOPS)/2
  consolidada_t3_comp <- consolidada_t3_comp[,c(1,2,10)]
  names(consolidada_t3_comp) <- c("Cod.IBGE","Ano","RP_consolidada")
  dim(consolidada_t3_comp) #info do tipo 3 imputados 
  RP_consolidada <- rbind(RP_consolidada, consolidada_t3_comp)
  
  consolidada_t3_inc <- subset(lista[[3]], Soma %in% c("0","1"))
  consolidada_t3_inc$RP_consolidada <- NA
  consolidada_t3_inc <- consolidada_t3_inc[,c(1,2,10)]
  names(consolidada_t3_inc) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_t3_inc)
  
  
  return(RP_consolidada)
  
}


despeduc_consol <- consol(despeduc_basefs)
colnames(despeduc_consol)[3] <- "Desp_Educ_BU"

despsaude_consol <- consol(despsaude_basefs)
colnames(despsaude_consol)[3] <- "Desp_Saude_BU"

table(is.na(despeduc_consol$Desp_Educ_BU), despeduc_consol$Ano) #9217
table(is.na(despsaude_consol$Desp_Saude_BU), despsaude_consol$Ano) #9627

save(despsaude_consol,file = "BASES UNICAS/despsaude_consol.RData")
save(despeduc_consol,file = "BASES UNICAS/despeduc_consol.RData")
