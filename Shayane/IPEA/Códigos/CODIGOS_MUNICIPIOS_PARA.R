#######################################################################################
##################################### An?lise descritiva das vari?veis
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

# Escolha do diret?rio
setwd("C:/Users/luizg/OneDrive/Documentos/IPEA/DADOS")
# Verifica o diret?rio
getwd()
# Lendo os arquivos espec?ficos
CFEM_D <- read.csv("CFEM_Distribuicao.csv", sep = ";", dec=",")
SAUDE  <- read_excel("Base Final Sa?de - Investimentos - SIOPS - Base Bruta.xlsx")
# RETIRAR A NOTA??O CIENTIF?CA DOS DADOS
options(scipen = 999)

# SELE??O APENAS DO MUNC?PIO DO MARANH?O
CFEM_D1_PA <- CFEM_D %>% filter(Ano==c("2019","2020"), SiglaEstado=="PA")


CFEM_PA = CFEM_D1_PA %>%
  mutate(
    Cod.IBGE = case_when(
      NomeEnte=="ABAETETUBA"   ~ 150010,
      NomeEnte=="ABEL FIGUEIREDO"   ~ 150013,
      NomeEnte=="ACAR?"   ~ 150020,
      NomeEnte=="AFU?"   ~ 150030,
      NomeEnte=="?GUA AZUL DO NORTE"   ~ 150034,
      NomeEnte=="ALENQUER"   ~ 150040,
      NomeEnte=="ALMEIRIM"   ~ 150050,
      NomeEnte=="ALTAMIRA"   ~ 150060,
      NomeEnte=="ANAJ?S"   ~ 150070,
      NomeEnte=="ANANINDEUA"   ~ 150080,
      NomeEnte=="ANAPU"   ~ 150085,
      NomeEnte=="AUGUSTO CORR?A"   ~ 150090,
      NomeEnte=="AURORA DO PAR?"   ~ 150095,
      NomeEnte=="AVEIRO"   ~ 150100,
      NomeEnte=="BAGRE"   ~ 150110,
      NomeEnte=="BAI?O"   ~ 150120,
      NomeEnte=="BANNACH"   ~ 150125,
      NomeEnte=="BARCARENA"   ~ 150130,
      NomeEnte=="BEL?M"   ~ 150140,
      NomeEnte=="BELTERRA"   ~ 150145,
      NomeEnte=="BENEVIDES"   ~ 150150,
      NomeEnte=="BOM JESUS DO TOCANTINS"   ~ 150157,
      NomeEnte=="BONITO"   ~ 150160,
      NomeEnte=="BRAGAN?A"   ~ 150170,
      NomeEnte=="BRASIL NOVO"   ~ 150172,
      NomeEnte=="BREJO GRANDE DO ARAGUAIA"   ~ 150175,
      NomeEnte=="BREU BRANCO"   ~ 150178,
      NomeEnte=="BREVES"   ~ 150180,
      NomeEnte=="BUJARU"   ~ 150190,
      NomeEnte=="CACHOEIRA DO ARARI"   ~ 150200,
      NomeEnte=="CACHOEIRA DO PIRI?"   ~ 150195,
      NomeEnte=="CAMET?"   ~ 150210,
      NomeEnte=="CANA? DOS CARAJ?S"   ~ 150215,
      NomeEnte=="CAPANEMA"   ~ 150220,
      NomeEnte=="CAPIT?O PO?O"   ~ 150230,
      NomeEnte=="CASTANHAL"   ~ 150240,
      NomeEnte=="CHAVES"   ~ 150250,
      NomeEnte=="COLARES"   ~ 150260,
      NomeEnte=="CONCEI??O DO ARAGUAIA"   ~ 150270,
      NomeEnte=="CONC?RDIA DO PAR?"   ~ 150275,
      NomeEnte=="CUMARU DO NORTE"   ~ 150276,
      NomeEnte=="CURION?POLIS"   ~ 150277,
      NomeEnte=="CURRALINHO"   ~ 150280,
      NomeEnte=="CURU?"   ~ 150285,
      NomeEnte=="CURU??"   ~ 150290,
      NomeEnte=="DOM ELISEU"   ~ 150293,
      NomeEnte=="ELDORADO DO CARAJ?S"   ~ 150295,
      NomeEnte=="FFARO"   ~ 150300,
      NomeEnte=="FLORESTA DO ARAGUAIA"   ~ 150304,
      NomeEnte=="GARRAF?O DO NORTE"   ~ 150307,
      NomeEnte=="GOIAN?SIA DO PAR?"   ~ 150309,
      NomeEnte=="GURUP?"   ~ 150310,
      NomeEnte=="IGARAP?-A?U"   ~ 150320,
      NomeEnte=="IGARAP?-MIRI"   ~ 150330,
      NomeEnte=="INHANGAPI"   ~ 150340,
      NomeEnte=="IPIXUNA DO PAR?"   ~ 150345,
      NomeEnte=="IRITUIA"   ~ 150350,
      NomeEnte=="ITAITUBA"   ~ 150360,
      NomeEnte=="ITUPIRANGA"   ~ 150370,
      NomeEnte=="JACAREACANGA"   ~ 150375,
      NomeEnte=="JACUND?"   ~ 150380,
      NomeEnte=="JURUTI"   ~ 150390,
      NomeEnte=="LIMOEIRO DO AJURU"   ~ 150400,
      NomeEnte=="M?E DO RIO"   ~ 150405,
      NomeEnte=="MAGALH?ES BARATA"   ~ 150410,
      NomeEnte=="MARAB?"   ~ 150420,
      NomeEnte=="MARACAN?"   ~ 150430,
      NomeEnte=="MARAPANIM"   ~ 150440,
      NomeEnte=="MARITUBA"   ~ 150442,
      NomeEnte=="MEDICIL?NDIA"   ~ 150445,
      NomeEnte=="MELGA?O"   ~ 150450,
      NomeEnte=="MOCAJUBA"   ~ 150460,
      NomeEnte=="MOJU"   ~ 150470,
      NomeEnte=="MOJU? DOS CAMPOS"   ~ 150475,
      NomeEnte=="MONTE ALEGRE"   ~ 150480,
      NomeEnte=="MUAN?"   ~ 150490,
      NomeEnte=="NOVA ESPERAN?A DO PIRI?"   ~ 150495,
      NomeEnte=="NOVA IPIXUNA"   ~ 150497,
      NomeEnte=="NOVA TIMBOTEUA"   ~ 150500,
      NomeEnte=="NOVO PROGRESSO"   ~ 150503,
      NomeEnte=="NOVO REPARTIMENTO"   ~ 150506,
      NomeEnte=="?BIDOS"   ~ 150510,
      NomeEnte=="OEIRAS DO PAR?"   ~ 150520,
      NomeEnte=="ORIXIMIN?"   ~ 150530,
      NomeEnte=="OUR?M"   ~ 150540,
      NomeEnte=="OURIL?NDIA DO NORTE"   ~ 150543,
      NomeEnte=="PACAJ?"   ~ 150548,
      NomeEnte=="PALESTINA DO PAR?"   ~ 150549,
      NomeEnte=="PARAGOMINAS"   ~ 150550,
      NomeEnte=="PARAUAPEBAS"   ~ 150553,
      NomeEnte=="PAU D'ARCO"   ~ 150555,
      NomeEnte=="PEIXE-BOI"   ~ 150560,
      NomeEnte=="PI?ARRA"   ~ 150563,
      NomeEnte=="PLACAS"   ~ 150565,
      NomeEnte=="PONTA DE PEDRAS"   ~ 150570,
      NomeEnte=="PORTEL"   ~ 150580,
      NomeEnte=="PORTO DE MOZ"   ~ 150590,
      NomeEnte=="PRAINHA"   ~ 150600,
      NomeEnte=="PRIMAVERA"   ~ 150610,
      NomeEnte=="QUATIPURU"   ~ 150611,
      NomeEnte=="REDEN??O"   ~ 150613,
      NomeEnte=="RIO MARIA"   ~ 150616,
      NomeEnte=="RONDON DO PAR?"   ~ 150618,
      NomeEnte=="RUR?POLIS"   ~ 150619,
      NomeEnte=="SALIN?POLIS"   ~ 150620,
      NomeEnte=="SALVATERRA"   ~ 150630,
      NomeEnte=="SANTA B?RBARA DO PAR?"   ~ 150635,
      NomeEnte=="SANTA CRUZ DO ARARI"   ~ 150640,
      NomeEnte=="SANTA IZABEL DO PAR?"   ~ 150650,
      NomeEnte=="SANTA LUZIA DO PAR?"   ~ 150655,
      NomeEnte=="SANTA MARIA DAS BARREIRAS"   ~ 150658,
      NomeEnte=="SANTA MARIA DO PAR?"   ~ 150660,
      NomeEnte=="SANTANA DO ARAGUAIA"   ~ 150670,
      NomeEnte=="SANTAR?M"   ~ 150680,
      NomeEnte=="SANTAR?M NOVO"   ~ 150690,
      NomeEnte=="SANTO ANT?NIO DO TAU?"   ~ 150700,
      NomeEnte=="S?O CAETANO DE ODIVELAS"   ~ 150710,
      NomeEnte=="S?O DOMINGOS DO ARAGUAIA"   ~ 150715,
      NomeEnte=="S?O DOMINGOS DO CAPIM"   ~ 150720,
      NomeEnte=="S?O F?LIX DO XINGU"   ~ 150730,
      NomeEnte=="S?O FRANCISCO DO PAR?"   ~ 150740,
      NomeEnte=="S?O GERALDO DO ARAGUAIA"   ~ 150745,
      NomeEnte=="S?O JO?O DA PONTA"   ~ 150746,
      NomeEnte=="S?O JO?O DE PIRABAS"   ~ 150747,
      NomeEnte=="S?O JO?O DO ARAGUAIA"   ~ 150750,
      NomeEnte=="S?O MIGUEL DO GUAM?"   ~ 150760,
      NomeEnte=="S?O SEBASTI?O DA BOA VISTA"   ~ 150770,
      NomeEnte=="SAPUCAIA"   ~ 150775,
      NomeEnte=="SENADOR JOS? PORF?RIO"   ~ 150780,
      NomeEnte=="SOURE"   ~ 150790,
      NomeEnte=="TAIL?NDIA"   ~ 150795,
      NomeEnte=="TERRA ALTA"   ~ 150796,
      NomeEnte=="TERRA SANTA"   ~ 150797,
      NomeEnte=="TOM?-A?U"   ~ 150800,
      NomeEnte=="TRACUATEUA"   ~ 150803,
      NomeEnte=="TRAIR?O"   ~ 150805,
      NomeEnte=="TUCUM?"   ~ 150808,
      NomeEnte=="TUCURU?"   ~ 150810,
      NomeEnte=="ULIAN?POLIS"   ~ 150812,
      NomeEnte=="URUAR?"   ~ 150815,
      NomeEnte=="VIGIA"   ~ 150820,
      NomeEnte=="VISEU"   ~ 150830,
      NomeEnte=="VIT?RIA DO XINGU"   ~ 150835,
      NomeEnte=="XINGUARA"   ~ 150840,
    ))


########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA SA?DE ##################

# SELE??O APENAS DOS MUNIC?PIOS DO MARANH?O
SAUDE_MA    <- SAUDE   %>% filter(UF=="MA")

# CRIANDO UMA COLUNA COM OS ANOS
SAUDE_MA_2019 <- SAUDE_MA %>% select(Cod.IBGE, Munic?pio, "2019" )%>% mutate(Ano=2019)
SAUDE_MA_2020 <- SAUDE_MA %>% select(Cod.IBGE, Munic?pio, "2020" )%>% mutate(Ano=2020)

# RENOMEANDO COLUNAS PARA JUNTAR OS DADOS
SAUDE_MA_2019 <- rename(SAUDE_MA_2019, Investimento="2019", NomeEnte="Munic?pio")
SAUDE_MA_2020 <- rename(SAUDE_MA_2020, Investimento="2020", NomeEnte="Munic?pio")

## JUNTANTO OS DATA.FRAMES
SAUDE_MA_TOTAL <- rbind(SAUDE_MA_2019, SAUDE_MA_2020)



