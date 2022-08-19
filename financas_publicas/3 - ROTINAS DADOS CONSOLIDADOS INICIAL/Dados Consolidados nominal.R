library(plyr)
library(readxl)
library(reshape2)
############### UMA BASE SO ############
## RECEITAS ##
load("~/Documents/IPEA/PROCESSO COMPLETO/BASES UNICAS/rcorr_consol.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/BASES UNICAS/rcap_consol.RData")

larquivos <-list.files("DADOS FILTRADOS/RECEITAS",full.names=T)
arquivos <- lapply(larquivos[-1], function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})


receitas <- list()
receitas[[1]] <- rcorr_consol
receitas[[2]] <- rcap_consol
receitas[[3]] <- rbind(arquivos[[1]], arquivos[[2]])
receitas[[4]] <- rbind(arquivos[[3]], arquivos[[4]])
receitas[[5]] <- rbind(arquivos[[5]], arquivos[[6]])

dados_nominais <- join_all(receitas, by=c("Cod.IBGE","Ano"), type = "full")
dados_nominais <- dados_nominais[,c(1,2,3,9,7,5,4,10,8,6)]
colnames(dados_nominais)




## RECEITAS DE CONTRIBUICAO##
load("~/Documents/IPEA/PROCESSO COMPLETO/BASES UNICAS/rcont_consol.RData")
larquivos <-list.files("DADOS FILTRADOS/RECEITAS/CONTRIBUICAO",full.names=T)
arquivos <- lapply(larquivos[-3], function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})

receitas <- list()
receitas[[1]] <- rcont_consol
receitas[[4]] <- rbind(arquivos[[1]], arquivos[[2]])
receitas[[3]] <- rbind(arquivos[[3]], arquivos[[4]])
receitas[[2]] <- rbind(arquivos[[5]], arquivos[[6]])

rcont <- join_all(receitas, by=c("Cod.IBGE","Ano"), type = "full")
colnames(rcont)

dados_nominais <- join(dados_nominais, rcont, by=c("Cod.IBGE","Ano"), type = "full")
colnames(dados_nominais)



## DESPESAS GERAIS
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/gasto_pessoal_ativos_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/gasto_pessoal_inativos_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/gasto_pessoal_9011_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/Investimentos_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/Consumo_Intemediario_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP GERAL/Transferencias_0320.RData")

paramelt <- list(gasto_pessoal_ativos_0320, gasto_pessoal_inativos_0320,
                 gasto_pessoal_9011_0320, Investimentos_0320,
                 Consumo_Intemediario_0320, Transferencias_0320)
nomes <- c("GPA.f","GPI.f","GP9011.f","Invest.f","CI.f","Transf.f")

meltgeral <- list()
for (i in 1:length(paramelt)) {
  meltgeral[[i]] <- melt(paramelt[[i]], id.vars = "Cod.IBGE", variable.name = "Ano", value.name = nomes[i])
  
}

geral <- join_all(meltgeral, by=c("Cod.IBGE","Ano"), type = "full")
colnames(geral)

dados_nominais <- join(dados_nominais, geral, by=c("Cod.IBGE","Ano"), type = "full")
colnames(dados_nominais)







## DESPESAS SAUDE
load("~/Documents/IPEA/PROCESSO COMPLETO/BASES UNICAS/despsaude_consol.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Saude_Finbra_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Saude_SIOPS_0320.RData")

load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_Ativos_Saude_SIOPS_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_Inativos_Saude_SIOPS_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Gasto_Pessoal_9011_Saude_SIOPS_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Investimento_Saude_SIOPS_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Consumo_Intermediario_Saude_SIOPS_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP SAUDE/Transferencias_Saude_SIOPS_0320.RData")

paramelt <- list(Despesa_Total_Saude_SIOPS_0320,Despesa_Total_Saude_Finbra_0320,
                 Gasto_Pessoal_Ativos_Saude_SIOPS_0320, Gasto_Pessoal_Inativos_Saude_SIOPS_0320,
                 Gasto_Pessoal_9011_Saude_SIOPS_0320, Investimento_Saude_SIOPS_0320, 
                 Consumo_Intermediario_Saude_SIOPS_0320, Transferencias_Saude_SIOPS_0320)
nomes <- c( "Desp.Total.Saude.s","Desp.Total.Saude.f", "Saude.GPA.s",
           "Saude.GPI.s","Saude.GP9011.s","Saude.Invest.s","Saude.CI.s","Saude.Transf.s")
meltsaude <- list()
meltsaude[[1]] <- despsaude_consol
for (i in 1:length(paramelt)) {
  meltsaude[[i+1]] <- melt(paramelt[[i]], id.vars = "Cod.IBGE", variable.name = "Ano", value.name = nomes[i])
  
}

saude <- join_all(meltsaude, by=c("Cod.IBGE","Ano"), type = "full")
colnames(saude)

dados_nominais <- join(dados_nominais, saude, by=c("Cod.IBGE","Ano"), type = "full")
colnames(dados_nominais)




## DESPESAS EDUCACAO ## 
load("~/Documents/IPEA/PROCESSO COMPLETO/BASES UNICAS/despeduc_consol.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Educacao_Finbra_0320.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Educacao_SIOPE_0819.RData")

load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_Ativos_Educacao_SIOPE_0819.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_Inativos_Educacao_SIOPE_0819.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_9011_Educacao_SIOPE_0819.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Investimento_Educacao_SIOPE_0819.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Consumo_Intermediario_Educacao_SIOPE_0819.RData")
load("~/Documents/IPEA/PROCESSO COMPLETO/DADOS FILTRADOS/DESP EDUCACAO/Transferencias_Educacao_SIOPE_0819.RData")


paramelt <- list(Despesa_Total_Educacao_SIOPE_0819,Despesa_Total_Educacao_Finbra_0320,
                 Gasto_Pessoal_Ativos_Educacao_SIOPE_0819, Gasto_Pessoal_Inativos_Educacao_SIOPE_0819,
                 Gasto_Pessoal_9011_Educacao_SIOPE_0819, Investimento_Educacao_SIOPE_0819, 
                 Consumo_Intermediario_Educacao_SIOPE_0819, Transferencias_Educacao_SIOPE_0819)
nomes <- c("Desp.Total.Educacao.e","Desp.Total.Educacao.f", "Educacao.GPA.e",
           "Educacao.GPI.e","Educacao.GP9011.e","Educacao.Invest.e","Educacao.CI.e","Educacao.Transf.e")


melteduc <- list()
melteduc[[1]] <- despeduc_consol
for (i in 1:length(paramelt)) {
  melteduc[[i+1]] <- melt(paramelt[[i]], id.vars = "Cod.IBGE", variable.name = "Ano", value.name = nomes[i])
  
}

educ <- join_all(melteduc, by=c("Cod.IBGE","Ano"), type = "full")
colnames(educ)

dados_nominais <- join(dados_nominais, educ, by=c("Cod.IBGE","Ano"), type = "full")
colnames(dados_nominais)


dados_nominais <- dados_nominais[dados_nominais$Cod.IBGE != "530010",]
dados_nominais <- dados_nominais[order(dados_nominais$Cod.IBGE, dados_nominais$Ano),]

save(dados_nominais, file = "DADOS CONSOLIDADOS/dados_nominais.RData")

