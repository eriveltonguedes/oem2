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
# View(arquivos[[1]])

arquivos_melt <- list()
receitasdecontribuicao <- list()
for (i in c(1:16,18)) {
  arquivos_melt[[i]] <- melt(arquivos[[i]], id.vars = colnames(arquivos[[i]])[1:3])
  if(i %in% c(1:4,14,15)){
    receitasdecontribuicao[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("1200000000"),]
  }
  if(i %in% 5:13){
    receitasdecontribuicao[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("41200000000"),]

  }
  if(i %in% c(16,18)){
    receitasdecontribuicao[[i]] <- arquivos_melt[[i]][arquivos_melt[[i]]$variable %in% c("1.2.00.00.00.00"),]

    
  }
  
  receitasdecontribuicao[[i]]$value[is.na(receitasdecontribuicao[[i]]$value)] <- 0
  receitasdecontribuicao[[i]]$Ano <- 2002+i
  receitasdecontribuicao[[i]] <- receitasdecontribuicao[[i]][,c(1,6,5)]
  colnames(receitasdecontribuicao[[i]]) <- c("Cod.IBGE","Ano","Valor")
  
  
}


# ## 2019
SIOPS19 <- read_excel(path = "~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/Receitas 2019 - Municipal.xlsx")
i=17
SIOPS19.c <- SIOPS19[-1,c(1,27)]
SIOPS19.c$Ano <- 2019
SIOPS19.c <- SIOPS19.c[,c(1,3,2)]
colnames(SIOPS19.c) <- c("Cod.IBGE", "Ano", "Valor")
arquivos_melt[[i]] <- SIOPS19.c
receitasdecontribuicao[[i]] <- arquivos_melt[[i]]

receitasdecontribuicao[[i]]$Valor[is.na(receitasdecontribuicao[[i]]$Valor)] <- 0



for (i in 1:18) {
  receitasdecontribuicao[[i]]$Cod.IBGE <- as.character(receitasdecontribuicao[[i]]$Cod.IBGE)
  receitasdecontribuicao[[i]]$Valor <- as.numeric(receitasdecontribuicao[[i]]$Valor)
  receitasdecontribuicao[[i]]$Ano <- as.character(receitasdecontribuicao[[i]]$Ano)
  
}


melt_receitasdecontribuicao<- bind_rows(receitasdecontribuicao)

table(melt_receitasdecontribuicao$Valor>0, melt_receitasdecontribuicao$Ano)

rcontr_siops <- melt_receitasdecontribuicao
colnames(rcontr_siops)[3] <- "Receitas.de.Contribuicoes.s" 


table(rcontr_siops$Ano)

SIOPS_2003a2020 <- rcontr_siops


SIOPS_2003a2020[SIOPS_2003a2020<0] <- 0

municipios_novos_naousar <- c("220095","500390","510452","510454","220672",
                              "150475","421265","422000","431454","500627","530010")
SIOPS_2003a2020_mun_novos <- SIOPS_2003a2020[(SIOPS_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]

SIOPS_2003a2020 <- SIOPS_2003a2020[!(SIOPS_2003a2020$Cod.IBGE %in% municipios_novos_naousar),]
table(SIOPS_2003a2020$Ano)



write.xlsx(SIOPS_2003a2020, file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPS_2003a2020.xlsx")
write.xlsx(SIOPS_2003a2020_mun_novos, file="~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/RECEITAS/CONTRIBUICAO/SIOPS_2003a2020_mun_novos.xlsx")








