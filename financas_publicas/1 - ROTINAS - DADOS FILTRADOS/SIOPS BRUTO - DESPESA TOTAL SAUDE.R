###################### SIOPS BRUTO ###################### 
##################### DESPESAS TOTAL SAUDE ##################### 
library(readxl)
library(readr)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(openxlsx)
options(scipen = 999)


larquivos <-list.files("DADOS BRUTOS/DESPESAS/SIOPS",full.names=T)
arquivos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x, skip = 1)
  return(arquivos)})

siops <- arquivos

names(siops) <- as.character(2003:2020)

filtros_despSaude_siops <- c('Cod.IBGE','3000000000','4000000000','33000000000','34000000000',
                            '3.0.00.00.00.00','4.0.00.00.00.00')

head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,filtros_despSaude_siops[c(1:3)]]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,filtros_despSaude_siops[c(1,4,5)]]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,filtros_despSaude_siops[c(1,6,7)]]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  colnames(sdesp[[i]])[2:3] <- c("Desp3","Desp4")
  sdesp[[i]]$Valor <- sdesp[[i]]$Desp3 + sdesp[[i]]$Desp4
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,5,4)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

DespSaude_siops <- bind_rows(sdesp)

Despesa_Total_Saude_SIOPS_0320 <- dcast(DespSaude_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Despesa_Total_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Saude_SIOPS_0320.RData")


