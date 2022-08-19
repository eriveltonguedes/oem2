library(readxl)
library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)

########### SIOPS
larquivos <-list.files("DADOS BRUTOS/RECEITAS/SIOPS",full.names=T)
arquivos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})



for (i in 1:18) {
  print(head(colnames(arquivos[[i]])))
}
arquivos[[16]]

arquivos_melt <- list()
receitascorrentes <- list()
receitascapital <- list()
for (i in c(1:16,18)) {
  arquivos_melt[[i]] <- melt(arquivos[[i]], id.vars = colnames(arquivos[[i]][,1:3]))
  if(i %in% c(1:4,14,15)){
    receitascorrentes[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("1000000000"),]
    receitascapital[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("2000000000"),]
  }
  if(i %in% 5:13){
    receitascorrentes[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("41000000000"),]
    receitascapital[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("42000000000"),]
    
  }
  if(i %in% c(16,18)){
    receitascorrentes[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("1.0.00.00.00.00"),]
    receitascapital[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("2.0.00.00.00.00"),]
    
    
  }
  
  receitascorrentes[[i]]$value[is.na(receitascorrentes[[i]]$value)] <- 0
  receitascorrentes[[i]]$Ano <- 2002+i
  receitascorrentes[[i]] <- receitascorrentes[[i]][,c(1,4,5,6)]
  colnames(receitascorrentes[[i]]) <- c("Cod.IBGE","Elemento","Valor","Ano")

  receitascapital[[i]]$value[is.na(receitascapital[[i]]$value)] <- 0
  receitascapital[[i]]$Ano <- 2002+i
  receitascapital[[i]] <- receitascapital[[i]][,c(1,4,5,6)]
  colnames(receitascapital[[i]]) <- c("Cod.IBGE","Elemento","Valor","Ano")

}


## 2019
i=17
arquivos_melt[[i]] <- melt(arquivos[[i]], id.vars = colnames(arquivos[[i]][,1]))
receitascorrentes[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("1.0.00.00.00.00 - Receitas Correntes"),]
receitascapital[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("2.0.00.00.00.00 - Receitas de Capital"),]

receitascorrentes[[i]]$value[is.na(receitascorrentes[[i]]$value)] <- 0
receitascorrentes[[i]]$Ano <- 2002+i
colnames(receitascorrentes[[i]]) <- c("Cod.IBGE","Elemento","Valor","Ano")

receitascapital[[i]]$value[is.na(receitascapital[[i]]$value)] <- 0
receitascapital[[i]]$Ano <- 2002+i
colnames(receitascapital[[i]]) <- c("Cod.IBGE","Elemento","Valor","Ano")


for (i in 1:18) {
  receitascorrentes[[i]]$Cod.IBGE <- as.character(receitascorrentes[[i]]$Cod.IBGE)
  receitascorrentes[[i]]$Elemento <- as.character(receitascorrentes[[i]]$Elemento)
  receitascorrentes[[i]]$Valor <- as.numeric(receitascorrentes[[i]]$Valor)
  receitascorrentes[[i]]$Ano <- as.character(receitascorrentes[[i]]$Ano)
  
  receitascapital[[i]]$Cod.IBGE <- as.character(receitascapital[[i]]$Cod.IBGE)
  receitascapital[[i]]$Elemento <- as.character(receitascapital[[i]]$Elemento)
  receitascapital[[i]]$Valor <- as.numeric(receitascapital[[i]]$Valor)
  receitascapital[[i]]$Ano <- as.character(receitascapital[[i]]$Ano)
}


melt_receitascorrentes<- bind_rows(receitascorrentes)
melt_receitascapital <- bind_rows(receitascapital)



rcorr_siops <- dcast(melt_receitascorrentes, Cod.IBGE+Ano ~ Elemento , value.var="Valor", fun.aggregate = sum)
rcap_siops <- dcast(melt_receitascapital, Cod.IBGE+Ano ~ Elemento, value.var="Valor", fun.aggregate = sum)

soma <- function(x){return(sum(x,na.rm = T))}
rcorr_siops$Receita.Corrente.s <- apply(rcorr_siops[,c(3:6)], 1, soma)
rcap_siops$Receita.Capital.s <- apply(rcap_siops[,c(3:6)], 1, soma)


rcorr_siops <- rcorr_siops[,c(1,2,7)]
rcap_siops <- rcap_siops[,c(1,2,7)]

table(rcorr_siops$Ano)
table(rcap_siops$Ano)

SIOPS_2003a2020 <- join(rcorr_siops, rcap_siops, by=c("Cod.IBGE","Ano"), type="full")


for (i in 3:4) {
  SIOPS_2003a2020[which(SIOPS_2003a2020[,i]<0),i] <- 0
}

municipios_novos_naousar <- c("220095","500390","510452","510454","220672",
                              "150475","421265","422000","431454","500627","530010")
R_SIOPS_2003a2020_mun_novos <- SIOPS_2003a2020[(SIOPS_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]

R_SIOPS_2003a2020 <- SIOPS_2003a2020[!(SIOPS_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]
table(R_SIOPS_2003a2020$Ano)



write.xlsx(R_SIOPS_2003a2020, file="DADOS FILTRADOS/RECEITAS/R_SIOPS_2003a2020.xlsx")
write.xlsx(R_SIOPS_2003a2020_mun_novos, file="DADOS FILTRADOS/RECEITAS/R_SIOPS_2003a2020_mun_novos.xlsx")


