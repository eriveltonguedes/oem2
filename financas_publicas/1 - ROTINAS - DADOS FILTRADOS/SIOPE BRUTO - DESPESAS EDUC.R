###################### SIOPE BRUTO ###################### 
##################### DESPESAS EDUCACAO ##################### 

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


## GPT - SOMA SGPA+SGPI
############### EDUCACAO - GASTO COM PESSOAL ATIVOS ############### 
v.gat <- c("3319004000","3319008000","3319009000","3319011000","3319012000","3319016000","3319017000",
           "3319034000","3319067000","3319091000","3319092000","3319094000","3319096000","3319099000",
           "3339034000","3339046000","3339049000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.gat,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

gpa_siope <- bind_rows(edesp)
table(gpa_siope$Ano)

Gasto_Pessoal_Ativos_Educacao_SIOPE_0819 <- dcast(gpa_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_Ativos_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_Ativos_Educacao_SIOPE_0819.RData")

############### EDUCACAO - GASTO COM PESSOAL INATIVOS ############### 
v.ginat <- c("3319001000","3319003000","3319005000","3339001000","3339003000","3339005000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.ginat,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

gpi_siope <- bind_rows(edesp)
table(gpi_siope$Ano)

Gasto_Pessoal_Inativos_Educacao_SIOPE_0819 <- dcast(gpi_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_Inativos_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_Inativos_Educacao_SIOPE_0819.RData")



############### EDUCACAO - GASTO COM PESSOAL 9011 ############### 
v.9011 <- c("3319011000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.9011,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

gp9011_siope <- bind_rows(edesp)
table(gp9011_siope$Ano)

Gasto_Pessoal_9011_Educacao_SIOPE_0819 <- dcast(gp9011_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Gasto_Pessoal_9011_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Gasto_Pessoal_9011_Educacao_SIOPE_0819.RData")


############### EDUCACAO - INVESTIMENTOS ############### 

v.inv <- c("3449000000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.inv,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

invest_siope <- bind_rows(edesp)
table(invest_siope$Ano)

Investimento_Educacao_SIOPE_0819 <- dcast(invest_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Investimento_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Investimento_Educacao_SIOPE_0819.RData")


############### EDUCACAO - CONSUMO INTERMEDIARIO ############### 

v.ci <- c("3339014000","3339015000","3339030000","3339032000","3339033000","3339035000",
          "3339036000","3339037000","3339038000","3339039000","3339091000","3339092000","3339093000",
          "3339095000","3339019000","3339040000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.ci,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

ci_siope <- bind_rows(edesp)
table(ci_siope$Ano)

Consumo_Intermediario_Educacao_SIOPE_0819 <- dcast(ci_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Consumo_Intermediario_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Consumo_Intermediario_Educacao_SIOPE_0819.RData")

############### EDUCACAO - TRANSFERENCIAS ############### 

v.tr <- c("3319001000","3319003000","3319005000","3319009000","3339001000","3339003000","3339005000",
          "3339006000","3339009000","3339010000","3339018000","3339020000","3339048000","3339059000")

edesp <- list()
for (i in 1:4) {
  edesp[[i]] <- siope[[i]]
  edesp[[i]] <- edesp[[i]][edesp[[i]]$Cod.Conta %in% v.tr,]
  edesp[[i]][is.na(edesp[[i]])] <- 0
  edesp[[i]] <- edesp[[i]] %>% group_by(Ano, Cod.IBGE) %>% summarise(Valor = sum(ValorLiq))
  edesp[[i]] <- edesp[[i]][,c(2,1,3)]
}

transf_siope <- bind_rows(edesp)
table(transf_siope$Ano)

Transferencias_Educacao_SIOPE_0819 <- dcast(transf_siope, Cod.IBGE ~ Ano, value.var = "Valor")
save(Transferencias_Educacao_SIOPE_0819, 
     file="DADOS FILTRADOS/DESP EDUCACAO/Transferencias_Educacao_SIOPE_0819.RData")

