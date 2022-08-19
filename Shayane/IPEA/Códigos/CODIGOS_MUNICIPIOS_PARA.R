#######################################################################################
##################################### Análise descritiva das variáveis
#######################################################################################
## Bibliotecas utilizadas
library(rbcb)
library(scales)
library(ggrepel)
library(png)
library(grid)
library(gridExtra)
library(sidrar)
library(xts)
library(grDevices)
library(ggalt)

library(metan)
library(readxl)
library(tidyverse)
library(knitr)
library(kableExtra)
#######################################################################################
##################### Lendo os dados

# Escolha do diretório
setwd("C:/Users/luizg/OneDrive/Documentos/IPEA/DADOS")
# Verifica o diretório
getwd()
# Lendo os arquivos específicos
CFEM_D <- read.csv("CFEM_Distribuicao.csv", sep = ";", dec=",")
SAUDE  <- read_excel("Base Final Saúde - Investimentos - SIOPS - Base Bruta.xlsx")
# RETIRAR A NOTAÇÃO CIENTIFÍCA DOS DADOS
options(scipen = 999)

# SELEÇÃO APENAS DO MUNCÍPIO DO MARANHÃO
CFEM_D1_PA <- CFEM_D %>% filter(Ano==c("2019","2020"), SiglaEstado=="PA")


CFEM_PA = CFEM_D1_PA %>%
  mutate(
    Cod.IBGE = case_when(
      NomeEnte=="ABAETETUBA"   ~ 150010,
      NomeEnte=="ABEL FIGUEIREDO"   ~ 150013,
      NomeEnte=="ACARÁ"   ~ 150020,
      NomeEnte=="AFUÁ"   ~ 150030,
      NomeEnte=="ÁGUA AZUL DO NORTE"   ~ 150034,
      NomeEnte=="ALENQUER"   ~ 150040,
      NomeEnte=="ALMEIRIM"   ~ 150050,
      NomeEnte=="ALTAMIRA"   ~ 150060,
      NomeEnte=="ANAJÁS"   ~ 150070,
      NomeEnte=="ANANINDEUA"   ~ 150080,
      NomeEnte=="ANAPU"   ~ 150085,
      NomeEnte=="AUGUSTO CORRÊA"   ~ 150090,
      NomeEnte=="AURORA DO PARÁ"   ~ 150095,
      NomeEnte=="AVEIRO"   ~ 150100,
      NomeEnte=="BAGRE"   ~ 150110,
      NomeEnte=="BAIÃO"   ~ 150120,
      NomeEnte=="BANNACH"   ~ 150125,
      NomeEnte=="BARCARENA"   ~ 150130,
      NomeEnte=="BELÉM"   ~ 150140,
      NomeEnte=="BELTERRA"   ~ 150145,
      NomeEnte=="BENEVIDES"   ~ 150150,
      NomeEnte=="BOM JESUS DO TOCANTINS"   ~ 150157,
      NomeEnte=="BONITO"   ~ 150160,
      NomeEnte=="BRAGANÇA"   ~ 150170,
      NomeEnte=="BRASIL NOVO"   ~ 150172,
      NomeEnte=="BREJO GRANDE DO ARAGUAIA"   ~ 150175,
      NomeEnte=="BREU BRANCO"   ~ 150178,
      NomeEnte=="BREVES"   ~ 150180,
      NomeEnte=="BUJARU"   ~ 150190,
      NomeEnte=="CACHOEIRA DO ARARI"   ~ 150200,
      NomeEnte=="CACHOEIRA DO PIRIÁ"   ~ 150195,
      NomeEnte=="CAMETÁ"   ~ 150210,
      NomeEnte=="CANAÃ DOS CARAJÁS"   ~ 150215,
      NomeEnte=="CAPANEMA"   ~ 150220,
      NomeEnte=="CAPITÃO POÇO"   ~ 150230,
      NomeEnte=="CASTANHAL"   ~ 150240,
      NomeEnte=="CHAVES"   ~ 150250,
      NomeEnte=="COLARES"   ~ 150260,
      NomeEnte=="CONCEIÇÃO DO ARAGUAIA"   ~ 150270,
      NomeEnte=="CONCÓRDIA DO PARÁ"   ~ 150275,
      NomeEnte=="CUMARU DO NORTE"   ~ 150276,
      NomeEnte=="CURIONÓPOLIS"   ~ 150277,
      NomeEnte=="CURRALINHO"   ~ 150280,
      NomeEnte=="CURUÁ"   ~ 150285,
      NomeEnte=="CURUÇÁ"   ~ 150290,
      NomeEnte=="DOM ELISEU"   ~ 150293,
      NomeEnte=="ELDORADO DO CARAJÁS"   ~ 150295,
      NomeEnte=="FFARO"   ~ 150300,
      NomeEnte=="FLORESTA DO ARAGUAIA"   ~ 150304,
      NomeEnte=="GARRAFÃO DO NORTE"   ~ 150307,
      NomeEnte=="GOIANÉSIA DO PARÁ"   ~ 150309,
      NomeEnte=="GURUPÁ"   ~ 150310,
      NomeEnte=="IGARAPÉ-AÇU"   ~ 150320,
      NomeEnte=="IGARAPÉ-MIRI"   ~ 150330,
      NomeEnte=="INHANGAPI"   ~ 150340,
      NomeEnte=="IPIXUNA DO PARÁ"   ~ 150345,
      NomeEnte=="IRITUIA"   ~ 150350,
      NomeEnte=="ITAITUBA"   ~ 150360,
      NomeEnte=="ITUPIRANGA"   ~ 150370,
      NomeEnte=="JACAREACANGA"   ~ 150375,
      NomeEnte=="JACUNDÁ"   ~ 150380,
      NomeEnte=="JURUTI"   ~ 150390,
      NomeEnte=="LIMOEIRO DO AJURU"   ~ 150400,
      NomeEnte=="MÃE DO RIO"   ~ 150405,
      NomeEnte=="MAGALHÃES BARATA"   ~ 150410,
      NomeEnte=="MARABÁ"   ~ 150420,
      NomeEnte=="MARACANÃ"   ~ 150430,
      NomeEnte=="MARAPANIM"   ~ 150440,
      NomeEnte=="MARITUBA"   ~ 150442,
      NomeEnte=="MEDICILÂNDIA"   ~ 150445,
      NomeEnte=="MELGAÇO"   ~ 150450,
      NomeEnte=="MOCAJUBA"   ~ 150460,
      NomeEnte=="MOJU"   ~ 150470,
      NomeEnte=="MOJUÍ DOS CAMPOS"   ~ 150475,
      NomeEnte=="MONTE ALEGRE"   ~ 150480,
      NomeEnte=="MUANÁ"   ~ 150490,
      NomeEnte=="NOVA ESPERANÇA DO PIRIÁ"   ~ 150495,
      NomeEnte=="NOVA IPIXUNA"   ~ 150497,
      NomeEnte=="NOVA TIMBOTEUA"   ~ 150500,
      NomeEnte=="NOVO PROGRESSO"   ~ 150503,
      NomeEnte=="NOVO REPARTIMENTO"   ~ 150506,
      NomeEnte=="ÓBIDOS"   ~ 150510,
      NomeEnte=="OEIRAS DO PARÁ"   ~ 150520,
      NomeEnte=="ORIXIMINÁ"   ~ 150530,
      NomeEnte=="OURÉM"   ~ 150540,
      NomeEnte=="OURILÂNDIA DO NORTE"   ~ 150543,
      NomeEnte=="PACAJÁ"   ~ 150548,
      NomeEnte=="PALESTINA DO PARÁ"   ~ 150549,
      NomeEnte=="PARAGOMINAS"   ~ 150550,
      NomeEnte=="PARAUAPEBAS"   ~ 150553,
      NomeEnte=="PAU D'ARCO"   ~ 150555,
      NomeEnte=="PEIXE-BOI"   ~ 150560,
      NomeEnte=="PIÇARRA"   ~ 150563,
      NomeEnte=="PLACAS"   ~ 150565,
      NomeEnte=="PONTA DE PEDRAS"   ~ 150570,
      NomeEnte=="PORTEL"   ~ 150580,
      NomeEnte=="PORTO DE MOZ"   ~ 150590,
      NomeEnte=="PRAINHA"   ~ 150600,
      NomeEnte=="PRIMAVERA"   ~ 150610,
      NomeEnte=="QUATIPURU"   ~ 150611,
      NomeEnte=="REDENÇÃO"   ~ 150613,
      NomeEnte=="RIO MARIA"   ~ 150616,
      NomeEnte=="RONDON DO PARÁ"   ~ 150618,
      NomeEnte=="RURÓPOLIS"   ~ 150619,
      NomeEnte=="SALINÓPOLIS"   ~ 150620,
      NomeEnte=="SALVATERRA"   ~ 150630,
      NomeEnte=="SANTA BÁRBARA DO PARÁ"   ~ 150635,
      NomeEnte=="SANTA CRUZ DO ARARI"   ~ 150640,
      NomeEnte=="SANTA IZABEL DO PARÁ"   ~ 150650,
      NomeEnte=="SANTA LUZIA DO PARÁ"   ~ 150655,
      NomeEnte=="SANTA MARIA DAS BARREIRAS"   ~ 150658,
      NomeEnte=="SANTA MARIA DO PARÁ"   ~ 150660,
      NomeEnte=="SANTANA DO ARAGUAIA"   ~ 150670,
      NomeEnte=="SANTARÉM"   ~ 150680,
      NomeEnte=="SANTARÉM NOVO"   ~ 150690,
      NomeEnte=="SANTO ANTÔNIO DO TAUÁ"   ~ 150700,
      NomeEnte=="SÃO CAETANO DE ODIVELAS"   ~ 150710,
      NomeEnte=="SÃO DOMINGOS DO ARAGUAIA"   ~ 150715,
      NomeEnte=="SÃO DOMINGOS DO CAPIM"   ~ 150720,
      NomeEnte=="SÃO FÉLIX DO XINGU"   ~ 150730,
      NomeEnte=="SÃO FRANCISCO DO PARÁ"   ~ 150740,
      NomeEnte=="SÃO GERALDO DO ARAGUAIA"   ~ 150745,
      NomeEnte=="SÃO JOÃO DA PONTA"   ~ 150746,
      NomeEnte=="SÃO JOÃO DE PIRABAS"   ~ 150747,
      NomeEnte=="SÃO JOÃO DO ARAGUAIA"   ~ 150750,
      NomeEnte=="SÃO MIGUEL DO GUAMÁ"   ~ 150760,
      NomeEnte=="SÃO SEBASTIÃO DA BOA VISTA"   ~ 150770,
      NomeEnte=="SAPUCAIA"   ~ 150775,
      NomeEnte=="SENADOR JOSÉ PORFÍRIO"   ~ 150780,
      NomeEnte=="SOURE"   ~ 150790,
      NomeEnte=="TAILÂNDIA"   ~ 150795,
      NomeEnte=="TERRA ALTA"   ~ 150796,
      NomeEnte=="TERRA SANTA"   ~ 150797,
      NomeEnte=="TOMÉ-AÇU"   ~ 150800,
      NomeEnte=="TRACUATEUA"   ~ 150803,
      NomeEnte=="TRAIRÃO"   ~ 150805,
      NomeEnte=="TUCUMÃ"   ~ 150808,
      NomeEnte=="TUCURUÍ"   ~ 150810,
      NomeEnte=="ULIANÓPOLIS"   ~ 150812,
      NomeEnte=="URUARÁ"   ~ 150815,
      NomeEnte=="VIGIA"   ~ 150820,
      NomeEnte=="VISEU"   ~ 150830,
      NomeEnte=="VITÓRIA DO XINGU"   ~ 150835,
      NomeEnte=="XINGUARA"   ~ 150840,
    ))


########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA SAÚDE ##################

# SELEÇÃO APENAS DOS MUNICÍPIOS DO MARANHÃO
SAUDE_MA    <- SAUDE   %>% filter(UF=="MA")

# CRIANDO UMA COLUNA COM OS ANOS
SAUDE_MA_2019 <- SAUDE_MA %>% select(Cod.IBGE, Município, "2019" )%>% mutate(Ano=2019)
SAUDE_MA_2020 <- SAUDE_MA %>% select(Cod.IBGE, Município, "2020" )%>% mutate(Ano=2020)

# RENOMEANDO COLUNAS PARA JUNTAR OS DADOS
SAUDE_MA_2019 <- rename(SAUDE_MA_2019, Investimento="2019", NomeEnte="Município")
SAUDE_MA_2020 <- rename(SAUDE_MA_2020, Investimento="2020", NomeEnte="Município")

## JUNTANTO OS DATA.FRAMES
SAUDE_MA_TOTAL <- rbind(SAUDE_MA_2019, SAUDE_MA_2020)



