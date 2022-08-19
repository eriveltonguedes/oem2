##### Dados REAIS ####
library(plyr)

load(file = "DADOS CONSOLIDADOS/dados_nominais.RData")


deflator <- c(0.39078999,0.410932781,0.444016599,0.4627857,0.47751849,0.504143242,0.530351712,
              0.558028931,0.594595293,0.624260558,0.664862216,0.707247677,
              0.767173404,0.838687024,0.868855679,0.893660513,0.93529048,0.952850444) #2003 a 2020

#Atualizado 25/03/2021 - mes/base fev/21


dados4 <- list()

for (i in 3:length(dados_nominais)) {
  dados <- dados_nominais[,c(1,2,i)]
  dados2 <- dcast(dados, Cod.IBGE ~ Ano, value.var = colnames(dados)[3])
  dados3 <- dados2
  for (j in 1:length(deflator)) {
    dados3[j+1] <- dados2[j+1]/deflator[j]
  }
  dados4[[i-2]] <- melt(dados3, id.vars = "Cod.IBGE", variable.name = "Ano", value.name = colnames(dados_nominais)[i])
}


dados_reais <- join_all(dados4, by=c("Cod.IBGE","Ano"), type = "full")

save(dados_reais, file = "DADOS CONSOLIDADOS/dados_reais.RData")
save(dados_reais, file = "~/Documents/IPEA/SHINY_FINAL/Dados/dados_reais.RData")






