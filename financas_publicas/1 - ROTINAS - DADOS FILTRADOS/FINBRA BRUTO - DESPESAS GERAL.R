###################### FINBRA BRUTO ###################### 
##################### DESPESAS GERAIS ##################### 
library(readxl)
library(readr)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(openxlsx)
options(scipen = 999)

########## Upload dos dados ########## 
larquivos <-list.files("DADOS BRUTOS/DESPESAS/FINBRA/ANEXO D/ANTIGOS",full.names=T)
antigos <- lapply(larquivos, function(x) {
  arquivos <- read_excel(x)
  return(arquivos)})


larquivos <-list.files("DADOS BRUTOS/DESPESAS/FINBRA/ANEXO D/NOVOS",full.names=T)
novos <- lapply(larquivos, function(x) {
  arquivos <- read_delim(x, ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", grouping_mark = "", encoding = "ISO-8859-1"), 
                         trim_ws = TRUE, skip = 3)
  return(arquivos)})

########## DADOS ANTIGOS ########## 
fantigos <- antigos
length(fantigos)
ibge <- list()
for (i in 1:length(fantigos)) {
  fantigos[[i]] <- as.data.frame(fantigos[[i]])
  ibge[[i]] <- str_length(fantigos[[i]]$CD_MUN)
  ibge[[i]][ibge[[i]]=="1"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==1], "000", fantigos[[i]]$CD_MUN[ibge[[i]]==1])
  ibge[[i]][ibge[[i]]=="2"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==2], "00", fantigos[[i]]$CD_MUN[ibge[[i]]==2])
  ibge[[i]][ibge[[i]]=="3"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==3], "0", fantigos[[i]]$CD_MUN[ibge[[i]]==3])
  ibge[[i]][ibge[[i]]=="4"] <- str_c(fantigos[[i]]$CD_UF[ibge[[i]]==4], fantigos[[i]]$CD_MUN[ibge[[i]]==4])
  fantigos[[i]]$Cod.IBGE <- ibge[[i]]
}
head(fantigos[[1]])



for (i in 1:length(fantigos)) {
  # print(head(fantigos[[i]]))
  a <- which(colnames(fantigos[[i]])=="Cod.IBGE")
  print(a)
  fantigos[[i]]$Ano <- 2002+i
  
}



########## DADOS NOVOS ########## 
fnovos <- novos
# for (i in 1:8) {
#   print(table(fnovos[[i]]$Coluna) )
# }

head(fnovos[[8]])

for (i in 1:length(fnovos)) {
  fnovos[[i]] <- fnovos[[i]][fnovos[[i]]$Coluna %in% c("Despesas Liquidadas"),]
  fnovos[[i]] <- fnovos[[i]][,-c(1,3,4,5)]
  fnovos[[i]]$Cod.IBGE <- str_sub(fnovos[[i]]$Cod.IBGE, end=6)
  fnovos[[i]]$Ano <- 2012+i
  fnovos[[i]]$Cod.conta <- str_sub(fnovos[[i]]$Conta, start = 1, end = 15)
  if(i>5) {fnovos[[i]]$Cod.conta <- str_sub(fnovos[[i]]$Conta, start = 1, end = 12)}
}



## GPT - SOMA GPA+GPI

############### GASTO COM PESSOAL ATIVOS ############### 
######## ANTIGOS ######## 

colnames(fantigos[[1]]) #conferir nomes 
gp_ativos_antigos <- list()
filtros_ativos_antigos <- c('Cod.IBGE','PESAD Contrat Tempo Determ','PESAD Salário_Família',
  'PESAD Vencimentos Pes Civil','PESAD Vencimentos Pes Mil',
  'PESAD Out Desp Pes Terceiriz','PESAD Dep Compulsórios',
  'PESAD Sentenças Judiciais','PESAD Desp Exerc Anteriores',
  'PESAD Indeniz Res Trabalhistas','PESAD Ressarc Desp Pes Req',
  'ODCAD Auxílio-Alimentação','ODCAD Auxílio-Transporte',
  'PESAD Outros Benef Assist')
for (i in c(1:length(fantigos))) {
  gp_ativos_antigos[[i]] <- fantigos[[i]][,filtros_ativos_antigos[-14]]
    
  if(i>6) {gp_ativos_antigos[[i]] <- fantigos[[i]][,filtros_ativos_antigos]}
}

#somar elementos e transformar em um unico dataframe
for (i in c(1:length(fantigos))) {
  gp_ativos_antigos[[i]][is.na(gp_ativos_antigos[[i]])] <- 0
  gp_ativos_antigos[[i]]$G.ativos <- apply(gp_ativos_antigos[[i]][,c(2:13)],1, sum)
  if(i>6) {gp_ativos_antigos[[i]]$G.ativos <- apply(gp_ativos_antigos[[i]][,c(2:14)],1, sum)}
}

gat_0312 <- list()
for (i in c(1:length(fantigos))) {
  gat_0312[[i]] <- gp_ativos_antigos[[i]][,c("Cod.IBGE", "G.ativos")]
  colnames(gat_0312[[i]])[2] <- as.character(2002+i)
}

gpa_antigos <- join_all(gat_0312, type="full")


######## NOVOS ######## 


gp_ativos_novos <- list()
filtros_ativos_novos <- c("3.1.90.04.00.00","3.1.90.08.00.00","3.1.90.09.00.00",
                            "3.1.90.11.00.00","3.1.90.12.00.00","3.1.90.16.00.00",
                            "3.1.90.17.00.00","3.1.90.34.00.00","3.1.90.67.00.00",
                            "3.1.90.91.00.00","3.1.90.92.00.00","3.1.90.94.00.00",
                            "3.1.90.96.00.00","3.1.90.99.00.00","3.3.90.34.00.00",
                            "3.3.90.46.00.00","3.3.90.49.00.00")
filtros_ativos_novos2 <- str_sub(filtros_ativos_novos, end = 12)

for (i in c(1:length(fnovos))) {
  gp_ativos_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_ativos_novos, ]
  if(i>5) {gp_ativos_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_ativos_novos2, ]}
}

#somar elementos e transformar em um unico dataframe
gat_1320 <- list()
for (i in c(1:length(fnovos))) {
  gp_ativos_novos[[i]]$Valor <- as.numeric(gp_ativos_novos[[i]]$Valor)
  gp_ativos_novos[[i]]$Valor[is.na(gp_ativos_novos[[i]]$Valor)] <- 0
  gat_1320[[i]] <- gp_ativos_novos[[i]] %>% group_by(Cod.IBGE) %>% summarise(G.ativos = sum(Valor))
  colnames(gat_1320[[i]])[2] <- as.character(2012+i)
}


gpa_novos <- join_all(gat_1320, type="full")








gasto_pessoal_ativos_0320 <- join(gpa_antigos, gpa_novos, by="Cod.IBGE", type="full")

save(gasto_pessoal_ativos_0320, file="DADOS FILTRADOS/DESP GERAL/gasto_pessoal_ativos_0320.RData")




############### GASTO COM PESSOAL INATIVOS ############### 

######## ANTIGOS ######## 
colnames(fantigos[[1]]) #conferir nomes 
gp_inativos_antigos <- list()
filtros_inativos_antigos <- c('Cod.IBGE','PESAD Aposent e Reformas','PESAD Pensões',
                               'ODCAD Aposent e Reformas',
                               'ODCAD Pensões','ODCAD Out Benef Previdenc',
                               'PESAD Outros Benef Previdenc')
for (i in c(1:10)) {
  gp_inativos_antigos[[i]] <- fantigos[[i]][,filtros_inativos_antigos[-7]]
  
  if(i>6) {gp_inativos_antigos[[i]] <- fantigos[[i]][,filtros_inativos_antigos]}
}

#somar elementos e transformar em um unico dataframe
for (i in c(1:10)) {
  gp_inativos_antigos[[i]][is.na(gp_inativos_antigos[[i]])] <- 0
  gp_inativos_antigos[[i]]$G.inativos <- apply(gp_inativos_antigos[[i]][,c(2:6)],1, sum)
  if(i>6) {gp_inativos_antigos[[i]]$G.inativos <- apply(gp_inativos_antigos[[i]][,c(2:7)],1, sum)}
}


ginat_0312 <- list()
for (i in c(1:10)) {
  ginat_0312[[i]] <- gp_inativos_antigos[[i]][,c("Cod.IBGE", "G.inativos")]
  colnames(ginat_0312[[i]])[2] <- as.character(2002+i)
}

gpi_antigos <- join_all(ginat_0312, type="full")



######## NOVOS ######## 
gp_inativos_novos <- list()
filtros_inativos_novos <- c("3.1.90.01.00.00","3.1.90.03.00.00","3.1.90.05.00.00",
                            "3.3.90.01.00.00","3.3.90.03.00.00","3.3.90.05.00.00")
filtros_inativos_novos2 <- str_sub(filtros_inativos_novos, end = 12)

for (i in c(1:length(fnovos))) {
  gp_inativos_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_inativos_novos, ]
  if(i>5) {gp_inativos_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_inativos_novos2, ]}
}

#somar elementos e transformar em um unico dataframe
ginat_1320 <- list()
for (i in c(1:length(fnovos))) {
  gp_inativos_novos[[i]]$Valor <- as.numeric(gp_inativos_novos[[i]]$Valor)
  gp_inativos_novos[[i]]$Valor[is.na(gp_inativos_novos[[i]]$Valor)] <- 0
  ginat_1320[[i]] <- gp_inativos_novos[[i]] %>% group_by(Cod.IBGE) %>% summarise(G.ativos = sum(Valor))
  colnames(ginat_1320[[i]])[2] <- as.character(2012+i)
}


gpi_novos <- join_all(ginat_1320, type="full")



gasto_pessoal_inativos_0320 <- join(gpi_antigos, gpi_novos, by="Cod.IBGE", type="full")

save(gasto_pessoal_inativos_0320, file="DADOS FILTRADOS/DESP GERAL/gasto_pessoal_inativos_0320.RData")



############### GASTO COM PESSOAL 9011 ############### 

######## ANTIGOS ######## 
gp9011_antigos <- list()
for (i in c(1:10)) {
  gp9011_antigos[[i]] <- fantigos[[i]][,c('Cod.IBGE', 'PESAD Vencimentos Pes Civil')]
  gp9011_antigos[[i]]$Ano <-  2002+i
  gp9011_antigos[[i]] <- gp9011_antigos[[i]][,c(1,3,2)]
  colnames(gp9011_antigos[[i]])[3] <- "Valor"
}

gp9011_antigos <- bind_rows(gp9011_antigos)




######## NOVOS ######## 
gp9011_novos <- list()
for (i in c(1:length(fnovos))) {
  gp9011_novos[[i]] <- subset(fnovos[[i]], Cod.conta %in% c("3.1.90.11.00.00"))
  if(i>5) {gp9011_novos[[i]] <- subset(fnovos[[i]], Cod.conta %in% c("3.1.90.11.00"))}
  gp9011_novos[[i]] <- gp9011_novos[[i]][,c(1,4,3)]
}

gp9011_novos <- bind_rows(gp9011_novos)


gp9011 <- rbind(gp9011_antigos, gp9011_novos)
gasto_pessoal_9011_0320 <- dcast(gp9011, Cod.IBGE ~ Ano, value.var = "Valor")


save(gasto_pessoal_9011_0320, file="DADOS FILTRADOS/DESP GERAL/gasto_pessoal_9011_0320.RData")


############### INVESTIMENTOS ############### 
colnames(fantigos[[2]])
filtros_invest_antigos <- c('Cod.IBGE',"Investimentos","I Aplicações Diretas")

invest_antigos <- list()
i=1
invest_antigos[[i]] <- fantigos[[i]][,filtros_invest_antigos[-3]]
invest_antigos[[i]]$Ano <-  2002+i
invest_antigos[[i]] <- invest_antigos[[i]][,c(1,3,2)]
colnames(invest_antigos[[i]])[3] <- "Valor"

for (i in c(2:10)) {
  invest_antigos[[i]] <- fantigos[[i]][,filtros_invest_antigos[-2]]
  invest_antigos[[i]]$Ano <-  2002+i
  invest_antigos[[i]] <- invest_antigos[[i]][,c(1,3,2)]
  colnames(invest_antigos[[i]])[3] <- "Valor"
}

invest_antigos <- bind_rows(invest_antigos)

######## NOVOS ######## 
invest_novos <- list()

for (i in c(1:8)) {
  invest_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% c("4.4.90.00.00.00"),]
  if(i>5) {invest_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% c("4.4.90.00.00"),]}
  invest_novos[[i]] <- invest_novos[[i]][,c(1,4,3)]
}

invest_novos <- bind_rows(invest_novos)

invest <- rbind(invest_antigos, invest_novos)
Investimentos_0320 <- dcast(invest, Cod.IBGE ~ Ano, value.var = "Valor")
save(Investimentos_0320, file="DADOS FILTRADOS/DESP GERAL/Investimentos_0320.RData")



















############### CONSUMO INTERMEDIARIO ############### 
######## ANTIGOS ######## 

colnames(fantigos[[2]]) #conferir nomes 
ci_0312 <- list()
filtros_ci_antigos <- c('Cod.IBGE','ODCAD Diárias Civil','ODCAD Diárias Militar','ODCAD Auxílio-Fardamento',
                            'ODCAD Mat Consumo','ODCAD Mat Distribuição Gratuita','ODCAD Pass Desp Locomoção',
                            'ODCAD Serv Consultoria','ODCAD Out Serviç Terceiros PF','ODCAD Locação Mão-de-Obra',
                            'ODCAD Arrendamento Mercantil','ODCAD Out Serviç Terceiros PJ','ODCAD Sentenças Judiciais',
                            'ODCAD Desp Exerc Anteriores','ODCAD Indeniz e Restituições','ODCAD Inden Trabalhos Campo')


for (i in c(1:length(fantigos))) {
  ci_0312[[i]] <- fantigos[[i]][,filtros_ci_antigos]
}


#somar elementos e transformar em um unico dataframe
for (i in c(1:length(fantigos))) {
  ci_0312[[i]][is.na(ci_0312[[i]])] <- 0
  ci_0312[[i]]$CI <- apply(ci_0312[[i]][,c(2:16)],1, sum)
  
  ci_0312[[i]] <- ci_0312[[i]][,c("Cod.IBGE", "CI")]
  ci_0312[[i]]$Ano <- 2002+i
  ci_0312[[i]] <- ci_0312[[i]][,c(1,3,2)]
  colnames(ci_0312[[i]])[3] <- "Valor"
}

ci_antigos <- bind_rows(ci_0312)



######## NOVOS ######## 
ci_novos <- list()
filtros_ci_novos <- c("3.3.90.14.00.00","3.3.90.15.00.00","3.3.90.30.00.00","3.3.90.32.00.00","3.3.90.33.00.00",
                "3.3.90.35.00.00","3.3.90.36.00.00","3.3.90.37.00.00","3.3.90.38.00.00","3.3.90.39.00.00","3.3.90.91.00.00",
                "3.3.90.92.00.00","3.3.90.93.00.00","3.3.90.95.00.00","3.3.90.19.00.00","3.3.90.40.00.00")

filtros_ci_novos2 <- str_sub(filtros_ci_novos, end = 12)
# unique(fnovos[[1]]$Cod.conta)
for (i in c(1:length(fnovos))) {
  ci_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_ci_novos, ]
  if(i>5) {ci_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_ci_novos2, ]}
}

#somar elementos e transformar em um unico dataframe
ci_1320 <- list()
for (i in c(1:length(fnovos))) {
  ci_novos[[i]]$Valor <- as.numeric(ci_novos[[i]]$Valor)
  ci_novos[[i]]$Valor[is.na(ci_novos[[i]]$Valor)] <- 0
  ci_1320[[i]] <- ci_novos[[i]] %>% group_by(Cod.IBGE) %>% summarise(CI = sum(Valor))
  ci_1320[[i]]$Ano <- 2012+i
  ci_1320[[i]] <- ci_1320[[i]][,c(1,3,2)]
  colnames(ci_1320[[i]])[3] <- "Valor"
}

ci_novos <- bind_rows(ci_1320)


ci <- rbind(ci_antigos, ci_novos)
Consumo_Intemediario_0320 <- dcast(ci, Cod.IBGE ~ Ano, value.var = "Valor")
save(Consumo_Intemediario_0320, file="DADOS FILTRADOS/DESP GERAL/Consumo_Intemediario_0320.RData")














############### TRANSFERENCIAS ############### 
######## ANTIGOS ######## 

colnames(fantigos[[2]]) #conferir nomes 
transf_0312 <- list()
filtros_transf_antigos <- c('Cod.IBGE','PESAD Aposent e Reformas','PESAD Pensões',
                        'PESAD Salário_Família','ODCAD Aposent e Reformas','ODCAD Pensões',
                        'ODCAD Out Benef Previdenc','PESAD Salário_Família','ODCAD Benef Deficiente e Idoso',
                        'ODCAD Salário_Família','ODCAD Aux Fin Estudantes','ODCAD Aux Fin Pesquisadores',
                        'ODCAD Out Aux Financeiros PF')

for (i in c(1:length(fantigos))) {
  transf_0312[[i]] <- fantigos[[i]][,filtros_transf_antigos]
}


#somar elementos e transformar em um unico dataframe
for (i in c(1:length(fantigos))) {
  transf_0312[[i]][is.na(transf_0312[[i]])] <- 0
  transf_0312[[i]]$transf <- apply(transf_0312[[i]][,c(2:13)],1, sum)
  
  transf_0312[[i]] <- transf_0312[[i]][,c("Cod.IBGE", "transf")]
  transf_0312[[i]]$Ano <- 2002+i
  transf_0312[[i]] <- transf_0312[[i]][,c(1,3,2)]
  colnames(transf_0312[[i]])[3] <- "Valor"
}

transf_antigos <- bind_rows(transf_0312)



######## NOVOS ######## 
transf_novos <- list()
filtros_transf_novos <- c("3.1.90.01.00.00","3.1.90.03.00.00","3.1.90.05.00.00","3.1.90.09.00.00",
                          "3.3.90.01.00.00","3.3.90.03.00.00",
                          "3.3.90.05.00.00","3.3.90.06.00.00","3.3.90.09.00.00","3.3.90.10.00.00",
                          "3.3.90.18.00.00","3.3.90.20.00.00","3.3.90.48.00.00","3.3.90.59.00.00")

filtros_transf_novos2 <- str_sub(filtros_transf_novos, end = 12)
# unique(fnovos[[1]]$Cod.conta)
for (i in c(1:length(fnovos))) {
  transf_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_transf_novos, ]
  if(i>5) {transf_novos[[i]] <- fnovos[[i]][fnovos[[i]]$Cod.conta %in% filtros_transf_novos2, ]}
}

#somar elementos e transformar em um unico dataframe
transf_1320 <- list()
for (i in c(1:length(fnovos))) {
  transf_novos[[i]]$Valor <- as.numeric(transf_novos[[i]]$Valor)
  transf_novos[[i]]$Valor[is.na(transf_novos[[i]]$Valor)] <- 0
  transf_1320[[i]] <- transf_novos[[i]] %>% group_by(Cod.IBGE) %>% summarise(transf = sum(Valor))
  transf_1320[[i]]$Ano <- 2012+i
  transf_1320[[i]] <- transf_1320[[i]][,c(1,3,2)]
  colnames(transf_1320[[i]])[3] <- "Valor"
}

transf_novos <- bind_rows(transf_1320)


transf <- rbind(transf_antigos, transf_novos)
Transferencias_0320 <- dcast(transf, Cod.IBGE ~ Ano, value.var = "Valor")
save(Transferencias_0320, file="DADOS FILTRADOS/DESP GERAL/Transferencias_0320.RData")



