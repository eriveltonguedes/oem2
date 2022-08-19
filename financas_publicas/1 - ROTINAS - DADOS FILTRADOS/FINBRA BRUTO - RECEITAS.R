library(readxl)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(openxlsx)

############ FINBRA
larquivos <-list.files("DADOS BRUTOS/RECEITAS/FINBRA/ANTIGOS",full.names=T)
larquivos <- larquivos[-1]
antigos <- lapply(larquivos, function(x) {
  arquivos <- read.csv2(x)
  return(arquivos)})

head(antigos[[5]])


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
for (i in 1:10) {
  print(head(fantigos[[i]]))
  a <- which(colnames(fantigos[[i]])=="Cod.IBGE")
  print(a)
  fantigos[[i]]$Ano <- 2002+i
  
}

for (i in 1:6) { #2003 a 2008
  fantigos[[i]] <- fantigos[[i]][,c(13,14,6,12)]
  colnames(fantigos[[i]]) <- c("Cod.IBGE", "Ano",  "Receita.Corrente.f","Receita.Capital.f")
}

i=7 #2009
fantigos[[i]] <- fantigos[[i]][,c(14,15,6,13)]
colnames(fantigos[[i]]) <- c("Cod.IBGE", "Ano",  "Receita.Corrente.f","Receita.Capital.f")

for (i in 8:10) { #2010 a 2012
  fantigos[[i]] <- fantigos[[i]][,c(16,17,6,15)]
  colnames(fantigos[[i]]) <- c("Cod.IBGE", "Ano",  "Receita.Corrente.f","Receita.Capital.f")

}

i <- NA

finbra_antigos <- bind_rows(fantigos)

### Arrumando dados novos
fnovos <- novos

table(fnovos[[8]]$Coluna)


head(fnovos[[8]])

for (i in 1:8) {
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Coluna %in% c("Receitas Realizadas",
                                                       "Receitas Brutas Realizadas"),]
  fnovos[[i]] <- fnovos[[i]][,-c(1,3,4,5)]
  fnovos[[i]]$Cod.IBGE <- str_sub(fnovos[[i]]$Cod.IBGE, end=6)
  fnovos[[i]]$Ano <- 2012+i
}




### 2013 a 2017

filtros1317 <- c("1.0.0.0.00.00.00 - Receitas Correntes", 
                 "2.0.0.0.00.00.00 - Receitas de Capital")

for (i in 1:5) {
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Conta %in% filtros1317,]
}

table(fnovos[[1]]$Conta)
table(fnovos[[2]]$Conta)
table(fnovos[[3]]$Conta)
table(fnovos[[4]]$Conta)
table(fnovos[[5]]$Conta)


### Arumando 2018:2020
filtros1819 <- c("1.0.0.0.00.0.0 - Receitas Correntes",
                "2.0.0.0.00.0.0 - Receitas de Capital")

head(fnovos[[6]])

for (i in 6:8) {

fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Conta %in% filtros1819,]
table(fnovos[[i]]$Conta)
fnovos[[i]][fnovos[[i]]$Conta == filtros1819[1],2] <- filtros1317[1]
fnovos[[i]][fnovos[[i]]$Conta == filtros1819[2],2] <- filtros1317[2]

}
# fnovos[[8]]$Valor <- str_replace(fnovos[[8]]$Valor,pattern = ",",replacement = ".")
# fnovos[[8]]$Valor <- as.numeric(fnovos[[8]]$Valor)

lapply(fnovos, function(x){head(x)})
## Final
finbra_novos <- bind_rows(fnovos)
dcast_finbran <- dcast(finbra_novos, Cod.IBGE + Ano ~ Conta, value.var = "Valor")
colnames(dcast_finbran) <- c("Cod.IBGE","Ano","Receita.Corrente.f","Receita.Capital.f")


FINBRA_2003a2020 <- join(finbra_antigos, dcast_finbran, by=c("Cod.IBGE","Ano"), type="full")

for (i in 3:4) {
  FINBRA_2003a2020[which(FINBRA_2003a2020[,i]<0),i] <- 0
}




municipios_novos_naousar <- c("220095","500390","510452","510454","220672",
                              "150475","421265","422000","431454","500627","530010")

R_FINBRA_2003a2020_mun_novos <- FINBRA_2003a2020[(FINBRA_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]
R_FINBRA_2003a2020 <- FINBRA_2003a2020[!(FINBRA_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]
table(R_FINBRA_2003a2020$Ano)




write.xlsx(R_FINBRA_2003a2020,file="DADOS FILTRADOS/RECEITAS/R_FINBRA_2003a2020.xlsx")
write.xlsx(R_FINBRA_2003a2020_mun_novos,file="DADOS FILTRADOS/RECEITAS/R_FINBRA_2003a2020_mun_novos.xlsx")

