###################### SIOPS BRUTO ###################### 
##################### DESPESAS SAUDE ##################### 
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




## GPT - SOMA SGPA+SGPI
############### SAUDE - GASTO COM PESSOAL ATIVOS ############### 
vgat1 <- c("Cod.IBGE","3190040000","3190080000","3190090000","3190110000","3190120000","3190160000","3190170000",
           "3190340000","3190670000","3190910000","3190920000","3190940000","3190960000","3190990000",
           "3390340000","3390460000","3390490000")
vgat2 <- c("Cod.IBGE","33190040000","33190080000","33190090000","33190110000","33190120000","33190160000","33190170000",
           "33190340000","33190670000","33190910000","33190920000","33190940000","33190960000","33190990000",
           "33390340000","33390460000","33390490000")
vgat3 <- c("Cod.IBGE","3.1.90.04.00.00","3.1.90.08.00.00","3.1.90.09.00.00", "3.1.90.11.00.00","3.1.90.12.00.00",
           "3.1.90.16.00.00","3.1.90.17.00.00","3.1.90.34.00.00","3.1.90.67.00.00","3.1.90.91.00.00",
           "3.1.90.92.00.00","3.1.90.94.00.00","3.1.90.96.00.00","3.1.90.99.00.00","3.3.90.34.00.00",
           "3.3.90.46.00.00","3.3.90.49.00.00")

head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vgat1]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vgat2]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vgat3]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  tam <- dim(sdesp[[i]])[2]
  sdesp[[i]]$Valor <- apply(sdesp[[i]][,2:tam], 1, FUN = sum)
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,tam+2,tam+1)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

gpa_siops <- bind_rows(sdesp)

Gasto_Pessoal_Ativos_Saude_SIOPS_0320 <- dcast(gpa_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_Ativos_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_Ativos_Saude_SIOPS_0320.RData")






############### SAUDE - GASTO COM PESSOAL INATIVOS ############### 
vginat1 <- c("Cod.IBGE","3190010000","3190030000","3190050000",
             "3390010000","3390030000","3390050000")
vginat2 <- c("Cod.IBGE","33190010000","33190030000","33190050000",
             "33390010000","33390030000","33390050000")
vginat3 <- c("Cod.IBGE","3.1.90.01.00.00","3.1.90.03.00.00","3.1.90.05.00.00",
             "3.3.90.01.00.00","3.3.90.03.00.00","3.3.90.05.00.00")


head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vginat1]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vginat2]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vginat3]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  tam <- dim(sdesp[[i]])[2]
  sdesp[[i]]$Valor <- apply(sdesp[[i]][,2:tam], 1, FUN = sum)
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,tam+2,tam+1)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

gpi_siops <- bind_rows(sdesp)

Gasto_Pessoal_Inativos_Saude_SIOPS_0320 <- dcast(gpi_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_Inativos_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_Inativos_Saude_SIOPS_0320.RData")










############### SAUDE - GASTO COM PESSOAL 9011 ############### 
filtros_siops <- c('Cod.IBGE','3190110000','33190110000','3.1.90.11.00.00')




head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% filtros_siops[c(1,2)]]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% filtros_siops[c(1,3)]]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% filtros_siops[c(1,4)]]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  tam <- dim(sdesp[[i]])[2]
  sdesp[[i]]$Valor <- apply(sdesp[[i]][,2:tam], 1, FUN = sum)
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,tam+2,tam+1)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

gp9011_siops <- bind_rows(sdesp)

Gasto_Pessoal_9011_Saude_SIOPS_0320 <- dcast(gp9011_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_9011_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_9011_Saude_SIOPS_0320.RData")





############### SAUDE - INVESTIMENTOS ############### 
filtros_siops <- c('Cod.IBGE','4490000000','34490000000','4.4.90.00.00.00')

head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,filtros_siops[c(1:2)]]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,filtros_siops[c(1,3)]]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,filtros_siops[c(1,4)]]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,3,2)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

invest_siops <- bind_rows(sdesp)

Investimento_Saude_SIOPS_0320 <- dcast(invest_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Investimento_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Investimento_Saude_SIOPS_0320.RData")






############### SAUDE - CONSUMO INTERMEDIARIO ############### 
vcint1 <- c("Cod.IBGE","3390140000","3390150000","3390300000","3390320000","3390330000","3390350000",
            "3390360000","3390370000","3390380000","3390390000","3390910000","3390920000","3390930000",
            "3390950000","3390190000","3390400000")
vcint2 <- c("Cod.IBGE","33390140000","33390150000","33390300000","33390320000","33390330000","33390350000",
            "33390360000","33390370000","33390380000","33390390000","33390910000","33390920000","33390930000",
            "33390950000","33390190000","33390400000")
vcint3 <- c("Cod.IBGE","3.3.90.14.00.00","3.3.90.15.00.00","3.3.90.30.00.00","3.3.90.32.00.00","3.3.90.33.00.00",
            "3.3.90.35.00.00","3.3.90.36.00.00","3.3.90.37.00.00","3.3.90.38.00.00","3.3.90.39.00.00",
            "3.3.90.91.00.00","3.3.90.92.00.00","3.3.90.93.00.00","3.3.90.95.00.00","3.3.90.19.00.00","3.3.90.40.00.00")


head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vcint1]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vcint2]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vcint3]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  tam <- dim(sdesp[[i]])[2]
  sdesp[[i]]$Valor <- apply(sdesp[[i]][,2:tam], 1, FUN = sum)
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,tam+2,tam+1)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

ci_siops <- bind_rows(sdesp)

Consumo_Intermediario_Saude_SIOPS_0320 <- dcast(ci_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Consumo_Intermediario_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Consumo_Intermediario_Saude_SIOPS_0320.RData")





############### SAUDE - TRANSFERENCIAS ############### 

vtransf1 <- c("Cod.IBGE","3190010000","3190030000","3190050000","3190090000","3390010000","3390030000","3390050000",
              "3390060000","3390090000","3390100000","3390180000","3390200000","3390480000","3390590000")
vtransf2 <- c("Cod.IBGE","33190010000","33190030000","33190050000","33190090000","33390010000","33390030000","33390050000",
              "33390060000","33390090000","33390100000","33390180000","33390200000","33390480000","33390590000")
vtransf3 <- c("Cod.IBGE","3.1.90.01.00.00","3.1.90.03.00.00","3.1.90.05.00.00","3.1.90.09.00.00","3.3.90.01.00.00","3.3.90.03.00.00","3.3.90.05.00.00",
              "3.3.90.06.00.00","3.3.90.09.00.00","3.3.90.10.00.00","3.3.90.18.00.00","3.3.90.20.00.00","3.3.90.48.00.00","3.3.90.59.00.00")  


head(siops[[18]])
sdesp <- list()
for (i in 1:18) {
  colnames(siops[[i]])[1] <- "Cod.IBGE"
  if(i %in% c(1:4,14,15)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vtransf1]}
  if(i %in% c(5:13)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vtransf2]}
  if(i %in% c(16:18)){sdesp[[i]] <- siops[[i]][,colnames(siops[[i]]) %in% vtransf3]}
  
  sdesp[[i]][is.na(sdesp[[i]])] <- 0
  tam <- dim(sdesp[[i]])[2]
  sdesp[[i]]$Valor <- apply(sdesp[[i]][,2:tam], 1, FUN = sum)
  sdesp[[i]]$Ano <- 2002+i
  sdesp[[i]] <- sdesp[[i]][,c(1,tam+2,tam+1)]
  colnames(sdesp[[i]]) <- c("Cod.IBGE","Ano","Valor")
}

transf_siops <- bind_rows(sdesp)

Transferencias_Saude_SIOPS_0320 <- dcast(transf_siops, Cod.IBGE ~ Ano, value.var = "Valor")
save(Transferencias_Saude_SIOPS_0320, 
     file="DADOS FILTRADOS/DESP SAUDE/Transferencias_Saude_SIOPS_0320.RData")
