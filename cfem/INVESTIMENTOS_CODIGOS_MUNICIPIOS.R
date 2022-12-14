################################################################################
# Autor: Shayane dos Santos Cordeiro                                          #
# Data:  18 de Setembro de 2021/ Ajustado no dia 29/09/2021
# Uni?o das bases investimentos e CFEM
################################################################################

# Bibliotecas utilizadas
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(maps)
list_geobr() #IPEA

################################################################################

# Diret?rio
setwd("Y:\Atlas\Shayane\IPEA\An?lises II\DADOS\BASE INVESTIMENTO")
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

# Une os dados: saude, educa??o e p?blico segundo o ano e c?digo do IBGE
invest1   <- saude %>% full_join(publico,by=c( "Cod.IBGE", "Ano")) %>% 
  full_join(educacao,by=c( "Cod.IBGE", "Ano"))

# Sele??o apenas das colunas relevantes

invest1   <- invest1 %>% select(Ano, UF.x, Munic?pio.x, Valor.x, Valor.y, Valor)

##########################################################
# Unir os dois bancos de dados:  Colocando o c?digo do IBGE de 7 D?gitos

##########################################################

# Renomeia a coluna 
municipios <- rename(municipios, Cod.IBGE    = "code_muni")
municipios <- rename(municipios, NomeEnte    = "name_muni")
municipios <- rename(municipios, SiglaEstado = "abbrev_state")


invest1 <- rename(invest1, NomeEnte    = "Munic?pio.x")
invest1 <- rename(invest1, SiglaEstado = "UF.x")
invest1 <- rename(invest1, Saude       = "Valor.x")
invest1 <- rename(invest1, Publico     = "Valor.y")
invest1 <- rename(invest1, Educacao    = "Valor")

# Passando as letras para min?sculas
municipios$NomeEnte         <- tolower(municipios$NomeEnte)
municipios$SiglaEstado      <- tolower(municipios$SiglaEstado)
invest1$NomeEnte            <- tolower(invest1$NomeEnte)
invest1$SiglaEstado         <- tolower(invest1$SiglaEstado)

# Apresenta as primeiras linhas do banco de dados
head(municipios)
head(invest1)

# Unir as bases de dados, permanecendo os munic?pios da esquerda CFEM.
base_invest <- left_join(invest1,municipios, by =c("NomeEnte","SiglaEstado")) 

# Selecionando as colunas de interesse na base cfem
base_invest1 <- base_invest %>% select(Cod.IBGE, Ano,SiglaEstado, NomeEnte, Saude, Publico, Educacao)



#################################################################################
#  Analisa dos NAS da uni?o das bases
#################################################################################

# Verifica se existe alguma vari?vel que cont?m linha NA e contalibiza o percentual.
NAS <- round(colSums(is.na(base_invest1))*100/nrow(base_invest1),2)
NAS

# Quem s?o essas vari?veis ausentes no cod.IBGE
NAS.1 <- base_invest1 %>% filter(is.na(Cod.IBGE) == TRUE)
NAS.2 <- NAS.1 %>% group_by(SiglaEstado,NomeEnte) %>% summarise(Soma= sum(Saude))


# Ap?s verificar a nomenclatura usando a base munic?pios, alguns ajustes s?o necess?rios:

base_invest1$NomeEnte[base_invest1$NomeEnte == "amamba?"]                 <- "amambai"
base_invest1$NomeEnte[base_invest1$NomeEnte == "amparo de s?o francisco"] <- "amparo do s?o francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "ara?as"]                  <- "ara??s"
base_invest1$NomeEnte[base_invest1$NomeEnte == "atilio vivacqua"]         <- "at?lio viv?cqua"


base_invest1$NomeEnte[base_invest1$NomeEnte == "augusto severo"]            <- "campo grande"
base_invest1$NomeEnte[base_invest1$NomeEnte == "bel?m de s?o francisco"]    <- "bel?m do s?o francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "biritiba-mirim"]            <- "biritiba mirim"



base_invest1$NomeEnte[base_invest1$NomeEnte == "bras?polis"]           <- "braz?polis"
base_invest1$NomeEnte[base_invest1$NomeEnte == "couto de magalh?es"]   <- "couto magalh?es"
base_invest1$NomeEnte[base_invest1$NomeEnte == "dona eus?bia"]         <- "dona euz?bia"
base_invest1$NomeEnte[base_invest1$NomeEnte == "eldorado dos caraj?s"] <- "eldorado do caraj?s"



base_invest1$NomeEnte[base_invest1$NomeEnte == "embu"]                 <- "embu das artes"
base_invest1$NomeEnte[base_invest1$NomeEnte == "erer?"]                <- "erer?"
base_invest1$NomeEnte[base_invest1$NomeEnte == "flor?nia"]             <- "flor?nea"
base_invest1$NomeEnte[base_invest1$NomeEnte == "fortaleza do taboc?o"] <- "taboc?o"


base_invest1$NomeEnte[base_invest1$NomeEnte == "gr?o par?"]  <- "gr?o-par?"
base_invest1$NomeEnte[base_invest1$NomeEnte == "iguaraci"]   <- "iguaracy"
base_invest1$NomeEnte[base_invest1$NomeEnte == "ita?ca"]     <- "itaoca"
base_invest1$NomeEnte[base_invest1$NomeEnte == "iui?"]       <- "iuiu"


base_invest1$NomeEnte[base_invest1$NomeEnte == "lagoa do itaenga"]  <- "lagoa de itaenga"
base_invest1$NomeEnte[base_invest1$NomeEnte == "lauro muller"]      <- "lauro m?ller"
base_invest1$NomeEnte[base_invest1$NomeEnte == "luci?ra"]           <- "luciara"
base_invest1$NomeEnte[base_invest1$NomeEnte == "ma?ambara"]         <- "ma?ambar?"


base_invest1$NomeEnte[base_invest1$NomeEnte == "moji mirim"]                   <- "mogi mirim"
base_invest1$NomeEnte[base_invest1$NomeEnte == "muqu?m de s?o francisco"]      <- "muqu?m do s?o francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "olho-d'?gua do borges"]        <- "olho d'?gua do borges"
base_invest1$NomeEnte[base_invest1$NomeEnte == "parati"]                       <- "paraty"


base_invest1$NomeEnte[base_invest1$NomeEnte == "passa-vinte"]                 <- "passa vinte"
base_invest1$NomeEnte[base_invest1$NomeEnte == "muqu?m de s?o francisco"]     <- "muqu?m do s?o francisco"
base_invest1$NomeEnte[base_invest1$NomeEnte == "poxor?o"]                     <- "poxor?u"
base_invest1$NomeEnte[base_invest1$NomeEnte == "quixab?"]                     <- "quixaba"


base_invest1$NomeEnte[base_invest1$NomeEnte == "restinga seca"]               <- "restinga s?ca"
base_invest1$NomeEnte[base_invest1$NomeEnte == "santa isabel do par?"]        <- "santa izabel do par?"
base_invest1$NomeEnte[base_invest1$NomeEnte == "santana do livramento"]       <- "sant'ana do livramento"



base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o cristov?o do sul"]        <- "s?o crist?v?o do sul"
base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o domingos de pombal"]      <- "s?o domingos"
base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o lu?s do paraitinga"]      <- "s?o luiz do paraitinga"
base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o lu?z do norte"]           <- "s?o luiz do norte"




base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o thom? das letras"]        <- "s?o tom? das letras"
base_invest1$NomeEnte[base_invest1$NomeEnte == "s?o val?rio da natividade"]   <- "s?o val?rio"
base_invest1$NomeEnte[base_invest1$NomeEnte == "serid?"]                      <- "s?o vicente do serid?"




base_invest1$NomeEnte[base_invest1$NomeEnte == "trajano de morais"]        <- "trajano de moraes"
base_invest1$NomeEnte[base_invest1$NomeEnte == "vespasiano correa"]        <- "vespasiano corr?a"
base_invest1$NomeEnte[base_invest1$NomeEnte == "westfalia"]                <- "westf?lia"

##################################################################################################################
##################################################################################################################

base_invest1$NomeEnte[(base_invest1$NomeEnte =="santa teresinha")&(base_invest1$SiglaEstado == "ba")]     <- "santa terezinha"
base_invest1$NomeEnte[(base_invest1$NomeEnte == "s?o vicente ferrer")&(base_invest1$SiglaEstado == "pe")] <- "s?o vicente f?rrer"

##################################################################################################################
##################################################################################################################

# Exclunindo colunas para aplicar o left_join novamente
base_invest1 <- select(base_invest1, -Cod.IBGE)
base_invest1 <- left_join(base_invest1,municipios, by =c("NomeEnte","SiglaEstado")) 
base_invest1 <- base_invest1 %>% select(Cod.IBGE, Ano,SiglaEstado, NomeEnte, Saude, Publico, Educacao)

# Verifica se existe alguma vari?vel que cont?m linha NA e contalibiza o percentual.
NAS <- round(colSums(is.na(base_invest1))*100/nrow(base_invest1),2)
NAS

# Quem s?o essas vari?veis ausentes no cod.IBGE
NAS.1 <- base_invest1 %>% filter(is.na(Cod.IBGE) == TRUE)
NAS.2 <- NAS.1 %>% group_by(SiglaEstado,NomeEnte) %>% summarise(Soma= sum(Saude))

# Armazenando em um arquivo csv( )

write.csv2(base_invest1, "investimentos_atualizada.csv")


