################################################################################
# Autor: Shayane dos Santos Cordeiro                                          #
# Data:  22 de Setembro de 2021
################################################################################

# Cria registros(tuplas) com os anos dos respectivos investimentos.
# Para cada linha da variável investimento será criado a variável do ano correpondente.

# Bibliotecas Utilizadas
library(tidyverse)
library(readxl)

# Seleciona o diretório
setwd(" ")

# Dados 
PUBLICO  <- read_excel("INVESTIMENTO_PUBLICO_2015_2020.xlsx")
EDUCACAO <- read_excel("INVESTIMENTO_EDUCACAO_2015_2019.xlsx")
SAUDE    <- read_excel("INVESTIMENTO_SAUDE_2015_2020.xlsx")

options(scipen = 999)

#####################################################################################
################## DADOS SAÚDE ######################################################
#####################################################################################

# Renomeia as colunas para utilizar os tidyverse
saude    <-rename(SAUDE,
                  ano_2015= "2015",
                  ano_2016= "2016",
                  ano_2017= "2017",
                  ano_2018= "2018",
                  ano_2019= "2019",
                  ano_2020= "2020")


# Cria a coluna ano;
# Renomeia a colunas renomeadas anteriormente para todas ficarem com o mesmo nome Valor.

ano_2015 <- saude %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- saude %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- saude %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- saude %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- saude %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)
ano_2020 <- saude %>% select(Cod.IBGE, UF, Município,ano_2020) %>% mutate(Ano = "2020")%>% rename(Valor=ano_2020)


# Junta os dados
dados_saude<-rbind(ano_2015,ano_2016,ano_2017,ano_2018,ano_2019,ano_2020)

# Escreve em arquivo seperado por ;
write.csv2(dados_saude,"dados_saude.csv")

################################################################################################
################## DADOS PÚBLICO   ############################################################
################################################################################################

# Elimina uma coluna que não será usada
PUBLICO  <- select(PUBLICO,-"2014")

publico    <- rename(PUBLICO,
                     ano_2015= "2015",
                     ano_2016= "2016",
                     ano_2017= "2017",
                     ano_2018= "2018",
                     ano_2019= "2019",
                     ano_2020= "2020")


ano_2015 <- publico %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- publico %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- publico %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- publico %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- publico %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)
ano_2020 <- publico %>% select(Cod.IBGE, UF, Município,ano_2020) %>% mutate(Ano = "2020")%>% rename(Valor=ano_2020)


dados_publico<-rbind(ano_2015,ano_2016,ano_2017,ano_2018,ano_2019,ano_2020)

# Escreve em arquivo seperado por ;
write.csv2(dados_publico,"dados_publico.csv")

################################################################################################
################## DADOS EDUCAÇÃO   ############################################################
################################################################################################

educacao   <- rename(EDUCACAO,
                     ano_2015= "2015",
                     ano_2016= "2016",
                     ano_2017= "2017",
                     ano_2018= "2018",
                     ano_2019= "2019")


ano_2015 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)

dados_educacao<-rbind(ano_2015,ano_2016,ano_2017,ano_2018,ano_2019)

# Escreve em arquivo seperado por ;
write.csv2(dados_educacao,"dados_educacao.csv")

