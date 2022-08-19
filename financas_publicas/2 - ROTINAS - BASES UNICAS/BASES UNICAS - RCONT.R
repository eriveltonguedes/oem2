##################### REC CONTRIBUICAOP ##################### 
##################### LISTAS BASEFSE ##################### 
## 2003 a 2020

RCONT_SIOPS_2003a2020 <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPS_2003a2020.xlsx")
RCONT_SIOPE_2008a2019 <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPE_2008a2019.xlsx")
RCONT_FINBRA_2003a2020 <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/FINBRA_2003a2020.xlsx")

RCONT_SIOPS_2003a2020_mun_novos <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPS_2003a2020_mun_novos.xlsx")
RCONT_SIOPE_2008a2019_mun_novos <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPE_2008a2019_mun_novos.xlsx")
RCONT_FINBRA_2003a2020_mun_novos <- read_excel(path = "DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/FINBRA_2003a2020_mun_novos.xlsx")


cont_s <- rbind(RCONT_SIOPS_2003a2020,RCONT_SIOPS_2003a2020_mun_novos)
cont_e <- rbind(RCONT_SIOPE_2008a2019,RCONT_SIOPE_2008a2019_mun_novos)
cont_f <- rbind(RCONT_FINBRA_2003a2020,RCONT_FINBRA_2003a2020_mun_novos)


###### VALIDACAO R CORR
a <- join(cont_s[,1:3], cont_e[,1:3], by=c("Cod.IBGE","Ano"),  type="full", match="first")
rcont_val <- join(a, cont_f[,1:3], by=c("Cod.IBGE","Ano"),  type="full", match="first")


summary(rcont_val)
head(rcont_val)

tabelas <- rcont_val


val  <- function(tabela) { #"SIOPS","SIOPE","FINBRA"
  basefse <- tabela
  colnames(basefse)[3:5]<- c("SIOPS","SIOPE","FINBRA")
  basefs <- basefse[,-4]
  
  # 2003 a 2007 e 2020
  basefs$FS <- (basefs$FINBRA/basefs$SIOPS)-1
  basefs$SF <- (basefs$SIOPS/basefs$FINBRA)-1
  
  basefs$FS <- as.numeric(basefs$FS*100)
  basefs$CompatFS <- 0
  basefs$CompatFS[abs(basefs$FS)<15] <- 1
  
  basefs$SF <- as.numeric(basefs$SF*100)
  basefs$CompatSF <- 0
  basefs$CompatSF[abs(basefs$SF)<15] <- 1
  
  
  basefs <- basefs[basefs$Ano %in% c(2003:2007,2020),]
  soma <- function(x){return(sum(x,na.rm = T))}
  basefs$Soma <- apply(basefs[,7:8],1,FUN = soma)
  
  
  ##### FINBRA, SIOPE E SIOPS 
  #base de consist?ncias
  basefse$FS <- (basefse$FINBRA/basefse$SIOPS)-1
  basefse$SF <- (basefse$SIOPS/basefse$FINBRA)-1
  
  basefse$FE <- (basefse$FINBRA/basefse$SIOPE)-1
  basefse$EF <- (basefse$SIOPE/basefse$FINBRA)-1
  
  basefse$ES <- (basefse$SIOPE/basefse$SIOPS)-1
  basefse$SE <- (basefse$SIOPS/basefse$SIOPE)-1
  ##########
  
  basefse$FS <- as.numeric(basefse$FS*100)
  basefse$CompatFS <- 0
  basefse$CompatFS[abs(basefse$FS)<15] <- 1
  
  basefse$SF <- as.numeric(basefse$SF*100)
  basefse$CompatSF <- 0
  basefse$CompatSF[abs(basefse$SF)<15] <- 1
  
  basefse$FE <- as.numeric(basefse$FE*100)
  basefse$CompatFE <- 0
  basefse$CompatFE[abs(basefse$FE)<15] <- 1
  
  basefse$EF <- as.numeric(basefse$EF*100)
  basefse$CompatEF <- 0
  basefse$CompatEF[abs(basefse$EF)<15] <- 1
  
  basefse$ES <- as.numeric(basefse$ES*100)
  basefse$CompatES <- 0
  basefse$CompatES[abs(basefse$ES)<15] <- 1
  
  basefse$SE <- as.numeric(basefse$SE*100)
  basefse$CompatSE <- 0
  basefse$CompatSE[abs(basefse$SE)<15] <- 1
  
  basefse$Soma <- apply(basefse[,12:17],1,FUN = sum)
  
  basefse2 <- basefse[basefse$Ano %in% 2008:2019,]
  
  
  library(dplyr)
  basefs_tipo1 <- basefs[is.na(basefs$FINBRA) & is.na(basefs$SIOPS),]
  basefs_tipo3 <- basefs[!is.na(basefs$FINBRA) & !is.na(basefs$SIOPS),]
  basefs_tipo2 <- anti_join(basefs, basefs_tipo1, by=c("Cod.IBGE","Ano"))
  basefs_tipo2 <- anti_join(basefs_tipo2, basefs_tipo3, by=c("Cod.IBGE","Ano"))
  
  
  basefse_tipo1 <- basefse2[is.na(basefse2$FINBRA) & is.na(basefse2$SIOPS) & is.na(basefse2$SIOPE),]
  
  basefse_tipo2 <- rbind(basefse2[!is.na(basefse2$FINBRA) & is.na(basefse2$SIOPS) & is.na(basefse2$SIOPE),],
                         basefse2[is.na(basefse2$FINBRA) & !is.na(basefse2$SIOPS) & is.na(basefse2$SIOPE),],
                         basefse2[is.na(basefse2$FINBRA) & is.na(basefse2$SIOPS) & !is.na(basefse2$SIOPE),])
  
  basefse_tipo3 <- rbind(basefse2[!is.na(basefse2$FINBRA) & !is.na(basefse2$SIOPS) & is.na(basefse2$SIOPE),],
                         basefse2[is.na(basefse2$FINBRA) & !is.na(basefse2$SIOPS) & !is.na(basefse2$SIOPE),],
                         basefse2[!is.na(basefse2$FINBRA) & is.na(basefse2$SIOPS) & !is.na(basefse2$SIOPE),])
  
  basefse_tipo4 <- basefse2[!is.na(basefse2$FINBRA) & !is.na(basefse2$SIOPS) & !is.na(basefse2$SIOPE),]
  
  basefse_tipo4_pelomenosumaconsistencia <- basefse_tipo4[(basefse_tipo4$CompatFS ==1 & basefse_tipo4$CompatSF ==1)|
                                                            (basefse_tipo4$CompatFE ==1 & basefse_tipo4$CompatEF ==1)|
                                                            (basefse_tipo4$CompatES ==1 & basefse_tipo4$CompatSE ==1),]
  
  
  return(list(basefs_tipo1,basefs_tipo2,basefs_tipo3,basefse_tipo1,basefse_tipo2,
              basefse_tipo3,basefse_tipo4,basefse_tipo4_pelomenosumaconsistencia, basefs, basefse,basefse2))
}


tabelas[tabelas==0] <- NA

rcont_basefse <- val(tabelas)

head(rcont_basefse[[10]])

save(rcont_basefse, file = "BASES UNICAS/LISTAS BASEFSE/rcont_basefse.RData")



##################### BASE UNICA ##################### 
######## rcont ###########
# BASES UNICAS - NAO PODE SER ZERO
load(file = "BASES UNICAS/LISTAS BASEFSE/rcont_basefse.RData")
head(rcont_basefse[[5]])


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
  
  
  #### 2008 a 2019 - FINBRA, SIOPS, SIOPE
  #Tipo 1 - missing em todas as bases
  consolidada_fse_t1 <- lista[[4]][,c(1:3)]
  names(consolidada_fse_t1) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_t1)
  
  #Tipo 2 - missing em 2 bases/só 1 informação - mantém a informação presente
  consolidada_fse_t2 <- lista[[5]][,c(1:5)]
  consolidada_fse_t2$FINBRA[which(is.na(consolidada_fse_t2$FINBRA))] <- 0
  consolidada_fse_t2$SIOPS[which(is.na(consolidada_fse_t2$SIOPS))] <- 0
  consolidada_fse_t2$SIOPE[which(is.na(consolidada_fse_t2$SIOPE))] <- 0
  consolidada_fse_t2$RP_consolidada <- consolidada_fse_t2$FINBRA + consolidada_fse_t2$SIOPS + consolidada_fse_t2$SIOPE
  consolidada_fse_t2 <- consolidada_fse_t2[,c(1,2,6)]
  names(consolidada_fse_t2) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_t2)
  
  #Tipo 3 - missing em 1 base/informação em 2 bases - se compatível:média; se não compatível: missing
  #Compatível
  consolidada_fse_t3_comp <- subset(lista[[6]], Soma %in% c("2"))
  consolidada_fse_t3_comp$FINBRA[which(is.na(consolidada_fse_t3_comp$FINBRA))] <- 0
  consolidada_fse_t3_comp$SIOPS[which(is.na(consolidada_fse_t3_comp$SIOPS))] <- 0
  consolidada_fse_t3_comp$SIOPE[which(is.na(consolidada_fse_t3_comp$SIOPE))] <- 0
  consolidada_fse_t3_comp$RP_consolidada <- (consolidada_fse_t3_comp$FINBRA + consolidada_fse_t3_comp$SIOPS + consolidada_fse_t3_comp$SIOPE)/2
  consolidada_fse_t3_comp <- consolidada_fse_t3_comp[,c(1,2,19)]
  names(consolidada_fse_t3_comp) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_t3_comp)
  
  #Não compatível
  consolidada_fse_t3_inc <- subset(lista[[6]], Soma %in% c("0","1"))
  consolidada_fse_t3_inc$RP_consolidada <- NA
  consolidada_fse_t3_inc <- consolidada_fse_t3_inc[,c(1,2,19)]
  names(consolidada_fse_t3_inc) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_t3_inc)
  
  #Tipo 4
  library(dplyr)
  consolidada_fse_t4_inc <- anti_join(lista[[7]], lista[[8]], by=c("Cod.IBGE","Ano"))
  consolidada_fse_t4_inc$RP_consolidada <- NA
  consolidada_fse_t4_inc <- consolidada_fse_t4_inc[,c(1,2,19)]
  names(consolidada_fse_t4_inc) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_t4_inc)
  
  consolidada_fse_56 <- subset(lista[[8]], Soma %in% c("5","6"))
  consolidada_fse_56 <- consolidada_fse_56[,c(1:5)]
  consolidada_fse_56$RP_consolidada <- (consolidada_fse_56$FINBRA + consolidada_fse_56$SIOPS + consolidada_fse_56$SIOPE)/3
  consolidada_fse_56 <- consolidada_fse_56[,c(1,2,6)]
  names(consolidada_fse_56) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_56)
  
  consolidada_fse_23 <- subset(lista[[8]], Soma %in% c("2","3"))
  consolidada_fse_23$SomaFS <- consolidada_fse_23$CompatFS + consolidada_fse_23$CompatSF
  consolidada_fse_23$SomaFE <- consolidada_fse_23$CompatFE + consolidada_fse_23$CompatEF
  consolidada_fse_23$SomaSE <- consolidada_fse_23$CompatES + consolidada_fse_23$CompatSE
  
  consolidada_fse_23 <- consolidada_fse_23[,c(1:5,19:21)]
  consolidada_fse_23_FS <- subset(consolidada_fse_23, SomaFS %in% c("2"))
  consolidada_fse_23_FS$RP_consolidada <- (consolidada_fse_23_FS$FINBRA + consolidada_fse_23_FS$SIOPS)/2
  consolidada_fse_23_FS <- consolidada_fse_23_FS[,c(1,2,9)]
  names(consolidada_fse_23_FS) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_23_FS)
  
  consolidada_fse_23_FE <- subset(consolidada_fse_23, SomaFE %in% c("2"))
  consolidada_fse_23_FE$RP_consolidada <- (consolidada_fse_23_FE$FINBRA + consolidada_fse_23_FE$SIOPE)/2
  consolidada_fse_23_FE <- consolidada_fse_23_FE[,c(1,2,9)]
  names(consolidada_fse_23_FE) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_23_FE)
  
  consolidada_fse_23_SE <- subset(consolidada_fse_23, SomaSE %in% c("2"))
  consolidada_fse_23_SE$RP_consolidada <- (consolidada_fse_23_SE$SIOPE + consolidada_fse_23_SE$SIOPS)/2
  consolidada_fse_23_SE <- consolidada_fse_23_SE[,c(1,2,9)]
  names(consolidada_fse_23_SE) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_23_SE)
  
  consolidada_fse_4 <- subset(lista[[8]], Soma %in% c("4"))
  consolidada_fse_4$SomaFS <- consolidada_fse_4$CompatFS + consolidada_fse_4$CompatSF
  consolidada_fse_4$SomaFE <- consolidada_fse_4$CompatFE + consolidada_fse_4$CompatEF
  consolidada_fse_4$SomaSE <- consolidada_fse_4$CompatES + consolidada_fse_4$CompatSE
  consolidada_fse_4$Result <- consolidada_fse_4$SomaFS*consolidada_fse_4$SomaFE*consolidada_fse_4$SomaSE
  
  consolidada_fse_4_3b <- subset(consolidada_fse_4, Result %in% c("0"))
  consolidada_fse_4_3b$RP_consolidada <- (consolidada_fse_4_3b$FINBRA + consolidada_fse_4_3b$SIOPE + consolidada_fse_4_3b$SIOPS)/3                               
  consolidada_fse_4_3b <- consolidada_fse_4_3b[,c(1,2,23)]
  names(consolidada_fse_4_3b) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, consolidada_fse_4_3b)
  
  consolidada_fse_4_2b <- subset(consolidada_fse_4, Result %in% c("2"))
  fse_4_2b_SE <- subset(consolidada_fse_4_2b, SomaSE == 2)
  fse_4_2b_FE <- subset(consolidada_fse_4_2b, SomaFE == 2)
  fse_4_2b_FS <- subset(consolidada_fse_4_2b, SomaFS == 2)
  
  fse_4_2b_SE$RP_consolidada <- (fse_4_2b_SE$SIOPE + fse_4_2b_SE$SIOPS)/2
  fse_4_2b_FE$RP_consolidada <- (fse_4_2b_FE$SIOPE + fse_4_2b_FE$FINBRA)/2
  fse_4_2b_FS$RP_consolidada <- (fse_4_2b_FS$FINBRA + fse_4_2b_FS$SIOPS)/2
  
  fse_4_2b_SE <- fse_4_2b_SE[,c(1,2,23)]
  fse_4_2b_FE <- fse_4_2b_FE[,c(1,2,23)]
  fse_4_2b_FS <- fse_4_2b_FS[,c(1,2,23)]
  
  names(fse_4_2b_SE) <- c("Cod.IBGE","Ano","RP_consolidada")
  names(fse_4_2b_FE) <- c("Cod.IBGE","Ano","RP_consolidada")
  names(fse_4_2b_FS) <- c("Cod.IBGE","Ano","RP_consolidada")
  
  RP_consolidada <- rbind(RP_consolidada, fse_4_2b_SE)
  RP_consolidada <- rbind(RP_consolidada, fse_4_2b_FE)
  RP_consolidada <- rbind(RP_consolidada, fse_4_2b_FS)
  
  return(RP_consolidada)
  
}

rcont_consol <- consol(rcont_basefse)

table(is.na(rcont_consol$RP_consolidada)) 

colnames(rcont_consol)[3] <- "RCont_BU"

save(rcont_consol,file = "BASES UNICAS/rcont_consol.RData")







