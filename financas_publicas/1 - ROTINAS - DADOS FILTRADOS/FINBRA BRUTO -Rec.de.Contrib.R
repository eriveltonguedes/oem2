####### Finbra
####### Receitas de Contribuições
library(readxl)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(openxlsx)

############ FINBRA
larquivos <- list.files("DADOS BRUTOS/RECEITAS/FINBRA/ANTIGOS/Finbra Antigo - RContr",full.names=T)
antigos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})

head(antigos[[10]])


larquivos <-list.files("DADOS BRUTOS/RECEITAS/FINBRA/NOVOS - ANEXO C",full.names=T)
novos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})

head(novos[[1]])





### Padronizar CODIGO DO IBGE dados antigos
fantigos <- antigos
ibge <- list()
for (i in 1:10) {
  fantigos[[i]] <- as.data.frame(fantigos[[i]])
  ibge[[i]] <- str_length(fantigos[[i]]$CD_MUN)
  ibge[[i]][ibge[[i]]=="1"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==1], "000", fantigos[[i]]$CD_MUN[ibge[[i]]==1])
  ibge[[i]][ibge[[i]]=="2"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==2], "00", fantigos[[i]]$CD_MUN[ibge[[i]]==2])
  ibge[[i]][ibge[[i]]=="3"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==3], "0", fantigos[[i]]$CD_MUN[ibge[[i]]==3])
  ibge[[i]][ibge[[i]]=="4"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==4], fantigos[[i]]$CD_MUN[ibge[[i]]==4])
  fantigos[[i]]$Cod.IBGE <- ibge[[i]]
}
head(fantigos[[1]])

### Arrumando dados antigos
a <- vector()
b <- vector()
for (i in 1:10) {
  print(head(fantigos[[i]]))
  a[i] <- which(colnames(fantigos[[i]])=="Cod.IBGE")
  print(a)
  b[i] <- which(colnames(fantigos[[i]])=="Rec de Contribuição")
  fantigos[[i]]$Ano <- 2002+i

}

for (i in 1:10) {
  col <- as.numeric(a[i])
  fantigos[[i]] <- fantigos[[i]][,c(col,col+1,b[i])]
  colnames(fantigos[[i]]) <- c("Cod.IBGE", "Ano",  "Receita.de.Contribuicoes.f")
}


finbra_antigos <- bind_rows(fantigos)




### Arrumando dados novos
fnovos <- novos

table(fnovos[[1]]$Coluna)


head(fnovos[[8]])

for (i in 1:8) {
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Coluna %in% c("Receitas Realizadas",
                                                       "Receitas Brutas Realizadas"),]
  fnovos[[i]] <- fnovos[[i]][,-c(1,3,4,5)]
  fnovos[[i]]$Cod.IBGE <- str_sub(fnovos[[i]]$Cod.IBGE, end=6)
  fnovos[[i]]$Ano <- 2012+i
}


fnovos[[8]]$Valor <- str_replace(fnovos[[8]]$Valor,pattern = ",",replacement = ".")
fnovos[[8]]$Valor <- as.numeric(fnovos[[8]]$Valor)

### 2013 a 2020

filtros1320 <- c("1.2.0.0.00.00.00", "1.2.0.0.00.0.0 -")

# table(fnovos[[8]]$Conta)
for (i in 1:8) {
  fnovos[[i]]$Conta2 <- str_sub(fnovos[[i]]$Conta, end = 16)
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Conta2 %in% filtros1320,]
}


lapply(fnovos, function(x){table(x$Conta)})

lapply(fnovos, function(x){head(x)})

for (i in 1:8) {
  fnovos[[i]] <- fnovos[[i]][,c(1,4,3)]
  colnames(fnovos[[i]])[3] <- "Receita.de.Contribuicoes.f"
}

## Final
finbra_novos <- bind_rows(fnovos)
finbra_novos <- finbra_novos[,c(1,3,2)]
colnames(fnovos[[i]])[3] <- "Receita.de.Contribuicoes.f"

head(finbra_novos)

FINBRA_2003a2020 <- join(finbra_antigos, finbra_novos, by=c("Cod.IBGE","Ano"), type="full")
table(FINBRA_2003a2020$Ano)


FINBRA_2003a2020[FINBRA_2003a2020<0] <- 0
colnames(FINBRA_2003a2020)[3] <- "Receitas.de.Contribuicoes.f"

municipios_novos_naousar <- c("220095","500390","510452","510454","220672",
                              "150475","421265","422000","431454","500627","530010")

FINBRA_2003a2020_mun_novos <- FINBRA_2003a2020[(FINBRA_2003a2020$Cod.IBGE %in% municipios_novos_naousar[-11]),]
FINBRA_2003a2020 <- FINBRA_2003a2020[!(FINBRA_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]
table(FINBRA_2003a2020$Ano)



library(openxlsx)
write.xlsx(FINBRA_2003a2020,file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/FINBRA_2003a2020.xlsx")
write.xlsx(FINBRA_2003a2020_mun_novos,file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/FINBRA_2003a2020_mun_novos.xlsx")


