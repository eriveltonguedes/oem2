# Cria registros(tuplas) com os anos dos respectivos investimentos.
# Para cada linha da variável investimento será criado a variável do ano correpondente.


library(tidyverse)

setwd("C:/Users/luizg/OneDrive/Documentos/IPEA")
PUBLICO  <- read_excel("~/IPEA/INVESTIMENTO_PUBLICO.xlsx")
EDUCACAO <- read_excel("~/IPEA/INVESTIMENTO_EDUCACAO.xlsx")
SAUDE    <- read_excel("~/IPEA/INVESTIMENTO_SAUDE.xlsx")



#####################################################################################
################## DADOS SAÚDE ######################################################
#####################################################################################

# Renomeia as colunas para utilizar os tidyverse
saude    <-rename(SAUDE,
                  ano_2007= "2007",
                  ano_2008= "2008",
                  ano_2009= "2009",
                  ano_2010= "2010",
                  ano_2011= "2011",
                  ano_2012= "2012",
                  ano_2013= "2013",
                  ano_2014= "2014",
                  ano_2015= "2015",
                  ano_2016= "2016",
                  ano_2017= "2017",
                  ano_2018= "2018",
                  ano_2019= "2019",
                  ano_2020= "2020")


# Cria a coluna ano;
# Renomeia a colunas renomeadas anteriormente para todas ficarem com o mesmo nome Valor.

ano_2007 <- saude %>% select(Cod.IBGE, UF, Município,ano_2007) %>% mutate(Ano = "2007")%>% rename(Valor=ano_2007)
ano_2008 <- saude %>% select(Cod.IBGE, UF, Município,ano_2008) %>% mutate(Ano = "2008")%>% rename(Valor=ano_2008)
ano_2009 <- saude %>% select(Cod.IBGE, UF, Município,ano_2009) %>% mutate(Ano = "2009")%>% rename(Valor=ano_2009)
ano_2010 <- saude %>% select(Cod.IBGE, UF, Município,ano_2010) %>% mutate(Ano = "2010")%>% rename(Valor=ano_2010)
ano_2011 <- saude %>% select(Cod.IBGE, UF, Município,ano_2011) %>% mutate(Ano = "2011")%>% rename(Valor=ano_2011)
ano_2012 <- saude %>% select(Cod.IBGE, UF, Município,ano_2012) %>% mutate(Ano = "2012")%>% rename(Valor=ano_2012)
ano_2013 <- saude %>% select(Cod.IBGE, UF, Município,ano_2013) %>% mutate(Ano = "2013")%>% rename(Valor=ano_2013)
ano_2014 <- saude %>% select(Cod.IBGE, UF, Município,ano_2014) %>% mutate(Ano = "2014")%>% rename(Valor=ano_2014)
ano_2015 <- saude %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- saude %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- saude %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- saude %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- saude %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)
ano_2020 <- saude %>% select(Cod.IBGE, UF, Município,ano_2020) %>% mutate(Ano = "2020")%>% rename(Valor=ano_2020)


# Junta os dados
dados_saude<-rbind(ano_2007,ano_2008, ano_2009,ano_2010,ano_2011,
                   ano_2012,ano_2013,ano_2014,ano_2015,
                   ano_2016,ano_2017,ano_2018,ano_2019,ano_2020)

# Escreve em arquivo seperado por ;
write.csv2(dados_saude,"dados_saude.csv")

################################################################################################
################## DADOS PÚBLICO   ############################################################
################################################################################################

PUBLICO  <- read_excel("~/IPEA/INVESTIMENTO_PUBLICO.xlsx")


publico    <- rename(PUBLICO,
                  ano_2007= "2007",
                  ano_2008= "2008",
                  ano_2009= "2009",
                  ano_2010= "2010",
                  ano_2011= "2011",
                  ano_2012= "2012",
                  ano_2013= "2013",
                  ano_2014= "2014",
                  ano_2015= "2015",
                  ano_2016= "2016",
                  ano_2017= "2017",
                  ano_2018= "2018",
                  ano_2019= "2019",
                  ano_2020= "2020")



ano_2007 <- publico %>% select(Cod.IBGE, UF, Município,ano_2007) %>% mutate(Ano = "2007")%>% rename(Valor=ano_2007)
ano_2008 <- publico %>% select(Cod.IBGE, UF, Município,ano_2008) %>% mutate(Ano = "2008")%>% rename(Valor=ano_2008)
ano_2009 <- publico %>% select(Cod.IBGE, UF, Município,ano_2009) %>% mutate(Ano = "2009")%>% rename(Valor=ano_2009)
ano_2010 <- publico %>% select(Cod.IBGE, UF, Município,ano_2010) %>% mutate(Ano = "2010")%>% rename(Valor=ano_2010)
ano_2011 <- publico %>% select(Cod.IBGE, UF, Município,ano_2011) %>% mutate(Ano = "2011")%>% rename(Valor=ano_2011)
ano_2012 <- publico %>% select(Cod.IBGE, UF, Município,ano_2012) %>% mutate(Ano = "2012")%>% rename(Valor=ano_2012)
ano_2013 <- publico %>% select(Cod.IBGE, UF, Município,ano_2013) %>% mutate(Ano = "2013")%>% rename(Valor=ano_2013)
ano_2014 <- publico %>% select(Cod.IBGE, UF, Município,ano_2014) %>% mutate(Ano = "2014")%>% rename(Valor=ano_2014)
ano_2015 <- publico %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- publico %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- publico %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- publico %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- publico %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)
ano_2020 <- publico %>% select(Cod.IBGE, UF, Município,ano_2020) %>% mutate(Ano = "2020")%>% rename(Valor=ano_2020)


dados_publico<-rbind(ano_2007,ano_2008, ano_2009,ano_2010,ano_2011,
                   ano_2012,ano_2013,ano_2014,ano_2015,
                   ano_2016,ano_2017,ano_2018,ano_2019,ano_2020)

write.csv2(dados_publico,"dados_publico.csv")


################################################################################################
################## DADOS EDUCAÇÃO   ############################################################
################################################################################################

EDUCACAO <- read_excel("~/IPEA/INVESTIMENTO_EDUCACAO.xlsx")


educacao   <- rename(EDUCACAO,
                     ano_2008= "2008",
                     ano_2009= "2009",
                     ano_2010= "2010",
                     ano_2011= "2011",
                     ano_2012= "2012",
                     ano_2013= "2013",
                     ano_2014= "2014",
                     ano_2015= "2015",
                     ano_2016= "2016",
                     ano_2017= "2017",
                     ano_2018= "2018",
                     ano_2019= "2019")




ano_2008 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2008) %>% mutate(Ano = "2008")%>% rename(Valor=ano_2008)
ano_2009 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2009) %>% mutate(Ano = "2009")%>% rename(Valor=ano_2009)
ano_2010 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2010) %>% mutate(Ano = "2010")%>% rename(Valor=ano_2010)
ano_2011 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2011) %>% mutate(Ano = "2011")%>% rename(Valor=ano_2011)
ano_2012 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2012) %>% mutate(Ano = "2012")%>% rename(Valor=ano_2012)
ano_2013 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2013) %>% mutate(Ano = "2013")%>% rename(Valor=ano_2013)
ano_2014 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2014) %>% mutate(Ano = "2014")%>% rename(Valor=ano_2014)
ano_2015 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2015) %>% mutate(Ano = "2015")%>% rename(Valor=ano_2015)
ano_2016 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2016) %>% mutate(Ano = "2016")%>% rename(Valor=ano_2016)
ano_2017 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2017) %>% mutate(Ano = "2017")%>% rename(Valor=ano_2017)
ano_2018 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2018) %>% mutate(Ano = "2018")%>% rename(Valor=ano_2018)
ano_2019 <- educacao %>% select(Cod.IBGE, UF, Município,ano_2019) %>% mutate(Ano = "2019")%>% rename(Valor=ano_2019)

dados_educacao<-rbind(ano_2008, ano_2009,ano_2010,ano_2011,
                      ano_2012,ano_2013,ano_2014,ano_2015,
                      ano_2016,ano_2017,ano_2018,ano_2019)

write.csv2(dados_educacao,"dados_educacao.csv")




######################################################################################
#### Não será utilizada essa parte ###################################################
soma<-saude %>% group_by(Cod.IBGE, UF, Município) %>% 
  summarise(
    s_2007=sum(ano_2007),
    s_2008=sum(ano_2008),
    s_2009=sum(ano_2009),
    s_2010=sum(ano_2010),
    s_2011=sum(ano_2011),
    s_2012=sum(ano_2012),
    s_2013=sum(ano_2013),
    s_2014=sum(ano_2014),
    s_2015=sum(ano_2015),
    s_2016=sum(ano_2016),
    s_2017=sum(ano_2017),
    s_2019=sum(ano_2019),
    s_2020=sum(ano_2020))


