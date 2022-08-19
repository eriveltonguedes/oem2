library(readxl)
library(openxlsx)
library(plyr)
library(reshape2)
library(stringr)
options(scipen = 999)

############## SIOPE
####### Receitas de Contribuições

RECEITA_TOTAL_2008_2016 <- read.csv2("DADOS BRUTOS/RECEITAS/SIOPE/RECEITA_TOTAL_2008_2016.CSV")
rt <- RECEITA_TOTAL_2008_2016
head(rt)



rt <- rt[rt$TP_PERIODO=="ANUAL",]
rt <- rt[rt$NO_ESFERA_ADM=="MUNICIPAL",]
head(rt)

rt <- rt[,c(1,6,9,10,12)]

table(rt$AN_EXERCICIO)


for (i in 1:9) {
  a <- rt[rt$AN_EXERCICIO== (2007+i),]
  print(table(duplicated(a$CO_MUNICIPIO)))
  
}



SIOPE_2008a2016 <- rt
SIOPE_2008a2016$CO_CONTA_CONTABIL <- as.character(SIOPE_2008a2016$CO_CONTA_CONTABIL)
rownames(table(SIOPE_2008a2016$NO_CONTA_CONTABIL))

filtros <- c("RECEITAS DE CONTRIBUI\xc7\xd5ES")







SIOPE_2008a2016 <- subset(SIOPE_2008a2016, NO_CONTA_CONTABIL %in% filtros)
SIOPE_2008a2016$NO_CONTA_CONTABIL <- as.character(SIOPE_2008a2016$NO_CONTA_CONTABIL)

xx <- as.data.frame(table(SIOPE_2008a2016$NO_CONTA_CONTABIL, SIOPE_2008a2016$CO_CONTA_CONTABIL))
(xx <- xx[xx$Freq!=0,])



table(SIOPE_2008a2016$NO_CONTA_CONTABIL, SIOPE_2008a2016$AN_EXERCICIO)





RECEITA_TOTAL_2017 <- read.csv2("DADOS BRUTOS/RECEITAS/SIOPE/RECEITA_TOTAL_2017.CSV")
rt2 <- RECEITA_TOTAL_2017

rt2 <- rt2[rt2$TP_PERIODO=="ANUAL",]
rt2 <- rt2[rt2$NO_ESFERA_ADM=="MUNICIPAL",]
rt2 <- rt2[,c(1,6,9,10,12)]

table(rt2$AN_EXERCICIO)

table(duplicated(rt2$CO_MUNICIPIO))


SIOPE_2017 <- rt2
SIOPE_2017$CO_CONTA_CONTABIL <- as.character(SIOPE_2017$CO_CONTA_CONTABIL)
filtros <- c("41200000000")


SIOPE_2017 <- subset(SIOPE_2017, CO_CONTA_CONTABIL %in% filtros)
SIOPE_2017$NO_CONTA_CONTABIL <- as.character(SIOPE_2017$NO_CONTA_CONTABIL)


xx <- as.data.frame(table(SIOPE_2017$NO_CONTA_CONTABIL, SIOPE_2017$CO_CONTA_CONTABIL))
(xx <- xx[xx$Freq!=0,])











RECEITA_TOTAL_2018 <- read.csv2("DADOS BRUTOS/RECEITAS/SIOPE/RECEITA_TOTAL_2018.CSV")
ff <- colnames(RECEITA_TOTAL_2018)
rt3 <- RECEITA_TOTAL_2018

rt3 <- rt3[rt3$TP_PERIODO=="ANUAL",]
rt3 <- rt3[rt3$NO_ESFERA_ADM=="MUNICIPAL",]
rt3 <- rt3[,c(1,6,9,10,12)]

table(rt3$AN_EXERCICIO)

table(duplicated(rt3$CO_MUNICIPIO))



SIOPE_2018 <- rt3
SIOPE_2018$CO_CONTA_CONTABIL <- as.character(SIOPE_2018$CO_CONTA_CONTABIL)
filtros <- c("41200000000")


SIOPE_2018 <- subset(SIOPE_2018, CO_CONTA_CONTABIL %in% filtros)


xx <- as.data.frame(table(SIOPE_2018$NO_CONTA_CONTABIL, SIOPE_2018$CO_CONTA_CONTABIL))
(xx <- xx[xx$Freq!=0,])










library(readr)
RECEITA_TOTAL_2019 <- read_csv("DADOS BRUTOS/RECEITAS/SIOPE/RECEITA_TOTAL_2019.CSV",
                               col_names = FALSE, skip = 1)


rt4 <- RECEITA_TOTAL_2019
colnames(rt4) <- c(ff, "X14", "X15", "X16", "X17")

rt4 <- rt4[rt4$TP_PERIODO=="ANUAL",]
rt4 <- rt4[rt4$NO_ESFERA_ADM=="MUNICIPAL",]
rt4$CO_CONTA_CONTABIL <- as.character(rt4$CO_CONTA_CONTABIL)
filtros <- c("41200000000")
rt4 <- subset(rt4, CO_CONTA_CONTABIL %in% filtros)


#1 caso : 1 na 2 ok 3 ok - c1 na c2 ok c3 ok
#2 caso : 1 ok 2 na 3 ok - c1 ok c2 ok c3 na
#3 caso : 1 ok 2 ok 3 na - c1 ok c2 ok c3 ok
#4 caso : 1 na 2 na 3 ok - c1 na c2 na c3 ok
#5 caso : 1 na 2 ok 3 na - c1 na c2 ok c3 ok
#6 caso : 1 ok 2 na 3 na - c1 ok c2 ok c3 na
#7 caso : 1 na 2 na 3 na - c1 na c2 na c3 na
rt4$X18 <- NA
for (i in 1:dim(rt4)[1]){
  if(is.na(rt4$VL_RECEITA_PREVISAO_ATUALIZADA[i]) & !is.na(rt4$VL_RECEITA_REALIZADA[i])){ # Caso 1
    rt4$X18[i] <- str_c(rt4$VL_RECEITA_REALIZADA[i], rt4$VL_RECEITA_ORCADA[i], sep = ".")
  }
  if(!is.na(rt4$VL_RECEITA_PREVISAO_ATUALIZADA[i]) & !is.na(rt4$VL_RECEITA_REALIZADA[i]) & !is.na(rt4$VL_RECEITA_REALIZADA[i])){ # Caso 3
    rt4$X18[i] <- str_c(rt4$VL_RECEITA_ORCADA[i], rt4$X14[i], sep = ".")
  }

}

rt4$VL_RECEITA_REALIZADA <- rt4$X18

SIOPE_2019 <- rt4[,c(1,6,9,10,12)]





table(SIOPE_2019$AN_EXERCICIO)
table(duplicated(SIOPE_2019$CO_MUNICIPIO))





xx <- as.data.frame(table(SIOPE_2019$NO_CONTA_CONTABIL, SIOPE_2019$CO_CONTA_CONTABIL))
(xx <- xx[xx$Freq!=0,])

write.xlsx(SIOPE_2019,file="DADOS BRUTOS/SIOPE/REC_CONTR_2019.xlsx")
SIOPE_2019 <- read_excel(path="DADOS BRUTOS/SIOPE/REC_CONTR_2019.xlsx")






todos_os_anos <- rbind(SIOPE_2008a2016, SIOPE_2017)
todos_os_anos <- rbind(todos_os_anos, SIOPE_2018)
todos_os_anos <- rbind(todos_os_anos, SIOPE_2019)
table(todos_os_anos$AN_EXERCICIO)
todos_os_anos$VL_RECEITA_REALIZADA <- as.numeric(todos_os_anos$VL_RECEITA_REALIZADA)


SIOPE_2008a2019 <- todos_os_anos[,c(2,1,5)]

head(SIOPE_2008a2019)

colnames(SIOPE_2008a2019) <- c("Cod.IBGE", "Ano","Receitas.de.Contribuicoes.e")


SIOPE_2008a2019[which(SIOPE_2008a2019[,3]<0),3] <- 0




municipios_novos_naousar <- c("220095","500390","510452","510454","220672",
                              "150475","421265","422000","431454","500627","530010")
SIOPE_2008a2019_mun_novos <- SIOPE_2008a2019[(SIOPE_2008a2019$Cod.IBGE %in% municipios_novos_naousar),]
SIOPE_2008a2019 <- SIOPE_2008a2019[!(SIOPE_2008a2019$Cod.IBGE %in% municipios_novos_naousar),]



table(SIOPE_2008a2019$Ano)
write.xlsx(SIOPE_2008a2019,file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPE_2008a2019.xlsx")
write.xlsx(SIOPE_2008a2019_mun_novos,file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPE_2008a2019_mun_novos.xlsx")
