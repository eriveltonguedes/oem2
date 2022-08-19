########### DADOS FINAIS CONSOLIDADOS ########### 
library(stringr)
library(plyr)
library(reshape2)

larquivos <- list.files(path = "~/Documents/IPEA/SHINY_FINAL/data", full.names = T)
larquivos.final <- str_sub(larquivos, start = -11, end = -7)
a <- which(larquivos.final=="final")
b <- c(a,52,29,68,45,66,43,23)
for (i in b) {
  load(file = larquivos[i])
}

load(larquivos[52])

arquivos.final <- list(rcorr_final[[1]], rcap_bruta[[1]], gpessoal_A_final, gpessoal_I_bruta ,gpessoal9011_final,
                       invest_final, cint_final, Transf_bruta, despSaude_final[[1]], gpessoalA_S_final,
                       gpessoalI_S_bruta, Sinvest_final, Scint_final, Stransf_bruta, despEduc_final[[1]],
                       gpessoalA_E_final, gpessoalI_E_bruta, Einvest_final, Ecint_final, Etransf_bruta)

arquivos.final2 <- list()
nomes <- c("RCORR", "RCAP", "GPA", "GPI", "GP9011",
           "Invest", "CInt", "Transf", "despSaude", "GPA_Saude",
           "GPI_Saude", "Invest_Saude", "CInt_Saude", "Transf_Saude", "despEduc",
           "GPA_Educ", "GPI_Educ", "Invest_Educ", "CInt_Educ", "Transf_Educ")

for (i in 1:length(arquivos.final)) {
  arquivos.final2[[i]] <- melt(arquivos.final[[i]], id.vars="Cod.IBGE", 
                               variable.name="Ano", value.name = nomes[i])
}

final.consolidado <- join_all(arquivos.final2, by = c("Cod.IBGE","Ano"), type = "full")
final.consolidado[is.na(final.consolidado)] <- 0
final.consolidado$RT <- final.consolidado$RCORR+final.consolidado$RCAP
final.consolidado$GPT <- final.consolidado$GPA+final.consolidado$GPI
final.consolidado$GPT_Saude <- final.consolidado$GPA_Saude+final.consolidado$GPI_Saude
final.consolidado$GPT_Educ <- final.consolidado$GPA_Educ+final.consolidado$GPI_Educ
final.consolidado[final.consolidado==0] <- NA
colnames(final.consolidado)
final.consolidado <- final.consolidado[,c(1,2,23,3:4,24,5:11,25,12:17,26,18:22)]
colnames(final.consolidado)


save(final.consolidado, file = "Dados/final.consolidado.RData")







