################################################################################
# Autor: Shayane dos Santos Cordeiro                                          #
# Data:  18 de Setembro de 2021
################################################################################

# Bibliotecas utilizadas
library("tidyverse")
library("ggplot2")

################################################################################

# Diretório
setwd(" ")
options(scipen = 999)

################################################################################
# Dados

cfem_d   <- read.csv2("CFEM_Distribuicao.csv", sep = ";")
saude    <- read.csv2("dados_saude.csv")
publico  <- read.csv2("dados_publico.csv")
educacao <- read.csv2("dados_educacao.csv")

# Retira as linhas do ano de 2020 para os dados
# Investimento público e saúde

saude   <- filter(saude,    !(Ano == 2020))
publico <- filter(publico,  !(Ano == 2020))


# Une os dados: saude, educação e público segundo o ano e código do IBGE
invest   <- saude %>% full_join(publico,by=c("Ano", "Cod.IBGE")) %>% 
  full_join(educacao,by=c("Ano", "Cod.IBGE"))

####################################################################
#                      TRATAMENTOS PARA OS NA'S
####################################################################

#################
# Opção 1
#################
# Substitui os NA's por 0 e cria a coluna com o investimento total
invest.t <- invest %>% select(Cod.IBGE, Ano, UF.x,Município.x, Valor.x, Valor.y, Valor) %>%
  mutate_all(replace_na, 0) %>%
  mutate(total = Valor.x + Valor.y + Valor)

#################
# Opção 2
#################
# Retira as linhas que contenham ao menos um NA.

invest.t <- invest  %>% select(Cod.IBGE, Ano, UF.x,Município.x, Valor.x, Valor.y, Valor) %>%
  na.omit %>% mutate(total = Valor.x + Valor.y + Valor)

#####################################################################
#####################################################################
# Escolhido o procedimento de remoção dos NA's
# prossiga as analises

summary(invest.t$total)
# Selecionando dados maiores que zero 
invest.t <- invest.t %>% filter((total > 0) )
summary(invest.t$total)


# Renomeando colunas de interesse
cfem_d   <- cfem_d   %>% rename("UF" = "SiglaEstado" )
invest.t <- invest.t %>% rename("NomeEnte" = "Município.x" )
invest.t <- invest.t %>% rename("UF" = "UF.x" )

# Cria a coluna com o total distribuido por ano para cada município
cfem_total <- cfem_d  %>% group_by(Ano, UF, NomeEnte,TipoDistribuição)%>% 
  summarise(total_cfem=sum(Valor))


# Passando os nomes dos municípios para letras minúsculas
invest.t$NomeEnte   <- tolower(invest.t$NomeEnte)
cfem_total$NomeEnte <- tolower(cfem_total$NomeEnte)

# Retorna as linhas com valores correspondentes
dados_inner  <- inner_join(invest.t,cfem_total,by=c("Ano", "UF","NomeEnte"))


#####################################################################
# Tranfere os dados para um novo arquivo csv()
#####################################################################

# Caso tenha escolhido trabalhar com os NA'S
write.csv2(dados_inner,"DADOS_inner.csv")

# Caso não tenha escolhido trabalhar com os NA'S
write.csv2(dados_inner,"DADOS_inner_sem_na.csv")

