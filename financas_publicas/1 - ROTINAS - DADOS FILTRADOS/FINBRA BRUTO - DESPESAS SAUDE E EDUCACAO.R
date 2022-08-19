###################### FINBRA BRUTO ###################### 
##################### DESPESAS TOTAIS SAUDE E EDUCACAO ##################### 
library(readxl)
library(readr)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(openxlsx)
options(scipen = 999)

########## DADOS ANTIGOS ########## 
larquivos <-list.files("DADOS BRUTOS/DESPESAS/FINBRA/ANEXO E/ANTIGOS",full.names=T)
antigos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})


fantigos <- antigos
length(fantigos)
ibge <- list()
for (i in 1:length(fantigos)) {
  colnames(fantigos[[i]])[1:2] <- colnames(fantigos[[1]])[1:2]
  fantigos[[i]] <- as.data.frame(fantigos[[i]])
  ibge[[i]] <- str_length(fantigos[[i]]$CD_MUN)
  ibge[[i]][ibge[[i]]=="1"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==1], "000", fantigos[[i]]$CD_MUN[ibge[[i]]==1])
  ibge[[i]][ibge[[i]]=="2"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==2], "00", fantigos[[i]]$CD_MUN[ibge[[i]]==2])
  ibge[[i]][ibge[[i]]=="3"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==3], "0", fantigos[[i]]$CD_MUN[ibge[[i]]==3])
  ibge[[i]][ibge[[i]]=="4"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==4], fantigos[[i]]$CD_MUN[ibge[[i]]==4])
  fantigos[[i]]$Cod.IBGE <- ibge[[i]]
}
head(fantigos[[3]])
for (i in 1:length(fantigos)) {
  # print(head(fantigos[[i]]))
  a <- which(colnames(fantigos[[i]])=="Cod.IBGE")
  print(a)
  fantigos[[i]]$Ano <- 2002+i
  
}

# Filtrar
colnames(fantigos[[2]]) #conferir nomes 

saude_0312 <- list()
educ_0312 <- list()
filtros_saude_antigos <- c('Cod.IBGE','Ano','Saúde')
filtros_educ_antigos <- c('Cod.IBGE','Ano','Educação')

for (i in c(1:length(fantigos))) {
  saude_0312[[i]] <- fantigos[[i]][,filtros_saude_antigos]
  saude_0312[[i]][is.na(saude_0312[[i]])] <- 0
  colnames(saude_0312[[i]])[3] <- "Valor"
  
  educ_0312[[i]] <- fantigos[[i]][,filtros_educ_antigos]
  educ_0312[[i]][is.na(educ_0312[[i]])] <- 0
  colnames(educ_0312[[i]])[3] <- "Valor"
}



saude_antigos <- bind_rows(saude_0312)
educ_antigos <- bind_rows(educ_0312)



########## DADOS NOVOS ########## 
larquivos <-list.files("DADOS BRUTOS/DESPESAS/FINBRA/ANEXO E/NOVOS",full.names=T)
novos <- lapply(larquivos, function(x) {
  arquivos <- read_delim(x, ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", grouping_mark = "", encoding = "ISO-8859-1"), 
                         trim_ws = TRUE, skip = 3)
  return(arquivos)})




fnovos <- novos

head(fnovos[[8]])
table(fnovos[[8]]$Coluna)
for (i in 1:length(fnovos)) {
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Coluna %in% c("Despesas Liquidadas"),]
  fnovos[[i]] <- fnovos[[i]][,-c(1,3,4,5)]
  fnovos[[i]]$Cod.IBGE <- str_sub(fnovos[[i]]$Cod.IBGE, end=6)
  fnovos[[i]]$Ano <- 2012+i
}


# Filtrar
table(fnovos[[1]]$Conta)
saude_1320 <- list()
educ_1320 <- list()
filtros_saude_novos <- c("10 - Saúde")
filtros_educ_novos <- c("12 - Educação")

for (i in c(1:length(fnovos))) {
  saude_1320[[i]] <- fnovos[[i]][fnovos[[i]]$Conta %in% filtros_saude_novos, ]
  saude_1320[[i]]$Valor[is.na(saude_1320[[i]]$Valor)] <- 0
  saude_1320[[i]] <- saude_1320[[i]][,c(1,4,3)]
  
  educ_1320[[i]] <- fnovos[[i]][fnovos[[i]]$Conta %in% filtros_educ_novos, ]
  educ_1320[[i]]$Valor[is.na(educ_1320[[i]]$Valor)] <- 0
  educ_1320[[i]] <- educ_1320[[i]][,c(1,4,3)]
}


saude_novos <- bind_rows(saude_1320)
educ_novos <- bind_rows(educ_1320)


saude <- rbind(saude_antigos, saude_novos)
Despesa_Total_Saude_Finbra_0320 <- dcast(saude, Cod.IBGE ~ Ano, value.var = "Valor")
save(Despesa_Total_Saude_Finbra_0320, 
     file="DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Saude_Finbra_0320.RData")



educ <- rbind(educ_antigos, educ_novos)
Despesa_Total_Educacao_Finbra_0320 <- dcast(educ, Cod.IBGE ~ Ano, value.var = "Valor")
save(Despesa_Total_Educacao_Finbra_0320, 
     file="DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Educacao_Finbra_0320.RData")










