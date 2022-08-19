###################### SIOPE BRUTO ###################### 
##################### DESPESAS TOTAL EDUCACAO #####################



larquivos <- list.files("DADOS BRUTOS/DESPESAS/SIOPE",full.names=T)
arquivos <- lapply(larquivos, function(x) {
  arquivos <-  read_delim(x, ";", escape_double = FALSE, 
                          locale = locale(decimal_mark = ",", grouping_mark = "", 
                                          encoding = "ISO-8859-1"), trim_ws = TRUE)
  return(arquivos)})

siope <- arquivos

for (i in 1:4) {
  siope[[i]] <- siope[[i]][siope[[i]]$TP_PERIODO=="ANUAL",]
  siope[[i]] <- siope[[i]][,c(1,6,15,21)]
  colnames(siope[[i]]) <- c("Ano","Cod.IBGE","Cod.Conta","ValorLiq")
  siope[[i]]$Cod.Conta <- as.character(siope[[i]]$Cod.Conta)
}

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% c("3300000000","3400000000"),]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}



# View(edesp[[1]])
# for (i in 1:4) {
#   print(head(edesp[[i]]))
# }


DespEducacao_siope <- bind_rows(edesp)
table(DespEducacao_siope$Ano)

Despesa_Total_Educacao_SIOPE_0819 <- dcast(DespEducacao_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Despesa_Total_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP TOTAL SAUDE E EDUC/Despesa_Total_Educacao_SIOPE_0819.RData")
