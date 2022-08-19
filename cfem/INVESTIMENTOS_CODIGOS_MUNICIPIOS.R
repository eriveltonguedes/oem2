################################################################################
# Autor: Shayane dos Santos Cordeiro                                          #
# Data:  18 de Setembro de 2021/ Ajustado no dia 29/09/2021
# União das bases investimentos e CFEM
################################################################################

# Bibliotecas utilizadas
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(maps)
list_geobr() #IPEA

################################################################################

# Diretório
setwd("Y:\Atlas\Shayane\IPEA\Análises II\DADOS\BASE INVESTIMENTO")
# setwd("C:/Users/luizg/OneDrive/Documentos/IPEA/DADOS/BASE INVESTIMENTO")
options(scipen = 999)

################################################################################
# Dados
saude    <- read.csv2("dados_saude.csv")
publico  <- read.csv2("dados_publico.csv")
educacao <- read.csv2("dados_educacao.csv")
municipios <- read_municipality(year=2020)


###############################################################################
###############################################################################

# Une os dados: saude, educação e público segundo o ano e código do IBGE
invest1   <- saude %>% full_join(publico,by=c( "Cod.IBGE", "Ano")) %>% 
  full_join(educacao,by=c( "Cod.IBGE", "Ano"))

# Seleção apenas das colunas relevantes

invest1   <- invest1 %>% select(Ano, UF.x, Município.x, Valor.x, Valor.y, Valor)

##########################################################
# Unir os dois bancos de dados:  Colocando o código do IBGE de 7 Dígitos

##########################################################

# Renomeia a coluna 
municipios <- rename(municipios, Cod.IBGE    = "code_muni")
municipios <- rename(municipios, NomeEnte    = "name_muni")
municipios <- rename(municipios, SiglaEstado = "abbrev_state")


invest1 <- rename(invest1, NomeEnte    = "Município.x")
invest1 <- rename(invest1, SiglaEstado = "UF.x")
invest1 <- rename(invest1, Saude       = "Valor.x")
invest1 <- rename(invest1, Publico     = "Valor.y")
invest1 <- rename(invest1, Educacao    = "Valor")

# Passando as letras para minúsculas
municipios$NomeEnte         <- tolower(municipios$NomeEnte)
municipios$SiglaEstado      <- tolower(municipios$SiglaEstado)
invest1$NomeEnte            <- tolower(invest1$NomeEnte)
invest1$SiglaEstado         <- tolower(invest1$SiglaEstado)

# Apresenta as primeiras linhas do banco de dados
head(municipios)
head(invest1)

# Unir as bases de dados, permanecendo os municípios da esquerda CFEM.
base_invest <- left_join(invest1,municipios, by =c("NomeEnte","SiglaEstado")) 

# Selecionando as colunas de interesse na base cfem
base_invest1 <- base_invest %>% select(Cod.IBGE, Ano,SiglaEstado, NomeEnte, Saude, Publico, Educacao)



#################################################################################
#  Analisa dos NAS da união das bases
#################################################################################

# Verifica se existe alguma variável que contém linha NA e contalibiza o percentual.
NAS <- round(colSums(is.na(base_invest1))*100/nrow(base_invest1),2)
NAS

# Quem são essas variáveis ausentes no cod.IBGE
NAS.1 <- base_invest1 %>% filter(is.na(Cod.IBGE) == TRUE)
NAS.2 <- NAS.1 %>% group_by(SiglaEstado,NomeEnte) %>% summarise(Soma= sum(Saude))


# Após verificar a nomenclatura usando a base municípios, alguns ajustes são necessários:

base_invest1$NomeEnte[base_invest1$NomeEnte == "amambaí"]                 <- "amambai"
base_invest1$NomeEnte[base_invest1$NomeEnte == "amparo de são francisco"] <- "amparo do são francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "araças"]                  <- "araçás"
base_invest1$NomeEnte[base_invest1$NomeEnte == "atilio vivacqua"]         <- "atílio vivácqua"


base_invest1$NomeEnte[base_invest1$NomeEnte == "augusto severo"]            <- "campo grande"
base_invest1$NomeEnte[base_invest1$NomeEnte == "belém de são francisco"]    <- "belém do são francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "biritiba-mirim"]            <- "biritiba mirim"



base_invest1$NomeEnte[base_invest1$NomeEnte == "brasópolis"]           <- "brazópolis"
base_invest1$NomeEnte[base_invest1$NomeEnte == "couto de magalhães"]   <- "couto magalhães"
base_invest1$NomeEnte[base_invest1$NomeEnte == "dona eusébia"]         <- "dona euzébia"
base_invest1$NomeEnte[base_invest1$NomeEnte == "eldorado dos carajás"] <- "eldorado do carajás"



base_invest1$NomeEnte[base_invest1$NomeEnte == "embu"]                 <- "embu das artes"
base_invest1$NomeEnte[base_invest1$NomeEnte == "ererê"]                <- "ereré"
base_invest1$NomeEnte[base_invest1$NomeEnte == "florínia"]             <- "florínea"
base_invest1$NomeEnte[base_invest1$NomeEnte == "fortaleza do tabocão"] <- "tabocão"


base_invest1$NomeEnte[base_invest1$NomeEnte == "grão pará"]  <- "grão-pará"
base_invest1$NomeEnte[base_invest1$NomeEnte == "iguaraci"]   <- "iguaracy"
base_invest1$NomeEnte[base_invest1$NomeEnte == "itaóca"]     <- "itaoca"
base_invest1$NomeEnte[base_invest1$NomeEnte == "iuiú"]       <- "iuiu"


base_invest1$NomeEnte[base_invest1$NomeEnte == "lagoa do itaenga"]  <- "lagoa de itaenga"
base_invest1$NomeEnte[base_invest1$NomeEnte == "lauro muller"]      <- "lauro müller"
base_invest1$NomeEnte[base_invest1$NomeEnte == "luciára"]           <- "luciara"
base_invest1$NomeEnte[base_invest1$NomeEnte == "maçambara"]         <- "maçambará"


base_invest1$NomeEnte[base_invest1$NomeEnte == "moji mirim"]                   <- "mogi mirim"
base_invest1$NomeEnte[base_invest1$NomeEnte == "muquém de são francisco"]      <- "muquém do são francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "olho-d'água do borges"]        <- "olho d'água do borges"
base_invest1$NomeEnte[base_invest1$NomeEnte == "parati"]                       <- "paraty"


base_invest1$NomeEnte[base_invest1$NomeEnte == "passa-vinte"]                 <- "passa vinte"
base_invest1$NomeEnte[base_invest1$NomeEnte == "muquém de são francisco"]     <- "muquém do são francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "poxoréo"]                     <- "poxoréu"
base_invest1$NomeEnte[base_invest1$NomeEnte == "quixabá"]                     <- "quixaba"


base_invest1$NomeEnte[base_invest1$NomeEnte == "restinga seca"]               <- "restinga sêca"
base_invest1$NomeEnte[base_invest1$NomeEnte == "santa isabel do pará"]        <- "santa izabel do pará"
base_invest1$NomeEnte[base_invest1$NomeEnte == "santana do livramento"]       <- "sant'ana do livramento"



base_invest1$NomeEnte[base_invest1$NomeEnte == "são cristovão do sul"]        <- "são cristóvão do sul"
base_invest1$NomeEnte[base_invest1$NomeEnte == "são domingos de pombal"]      <- "são domingos"
base_invest1$NomeEnte[base_invest1$NomeEnte == "são luís do paraitinga"]      <- "são luiz do paraitinga"
base_invest1$NomeEnte[base_invest1$NomeEnte == "são luíz do norte"]           <- "são luiz do norte"




base_invest1$NomeEnte[base_invest1$NomeEnte == "são thomé das letras"]        <- "são tomé das letras"
base_invest1$NomeEnte[base_invest1$NomeEnte == "são valério da natividade"]   <- "são valério"
base_invest1$NomeEnte[base_invest1$NomeEnte == "seridó"]                      <- "são vicente do seridó"




base_invest1$NomeEnte[base_invest1$NomeEnte == "trajano de morais"]        <- "trajano de moraes"
base_invest1$NomeEnte[base_invest1$NomeEnte == "vespasiano correa"]        <- "vespasiano corrêa"
base_invest1$NomeEnte[base_invest1$NomeEnte == "westfalia"]                <- "westfália"

##################################################################################################################
##################################################################################################################

base_invest1$NomeEnte[(base_invest1$NomeEnte =="santa teresinha")&(base_invest1$SiglaEstado == "ba")]     <- "santa terezinha"
base_invest1$NomeEnte[(base_invest1$NomeEnte == "são vicente ferrer")&(base_invest1$SiglaEstado == "pe")] <- "são vicente férrer"

##################################################################################################################
##################################################################################################################

# Exclunindo colunas para aplicar o left_join novamente
base_invest1 <- select(base_invest1, -Cod.IBGE)
base_invest1 <- left_join(base_invest1,municipios, by =c("NomeEnte","SiglaEstado")) 
base_invest1 <- base_invest1 %>% select(Cod.IBGE, Ano,SiglaEstado, NomeEnte, Saude, Publico, Educacao)

# Verifica se existe alguma variável que contém linha NA e contalibiza o percentual.
NAS <- round(colSums(is.na(base_invest1))*100/nrow(base_invest1),2)
NAS

# Quem são essas variáveis ausentes no cod.IBGE
NAS.1 <- base_invest1 %>% filter(is.na(Cod.IBGE) == TRUE)
NAS.2 <- NAS.1 %>% group_by(SiglaEstado,NomeEnte) %>% summarise(Soma= sum(Saude))

# Armazenando em um arquivo csv( )

write.csv2(base_invest1, "investimentos_atualizada.csv")


