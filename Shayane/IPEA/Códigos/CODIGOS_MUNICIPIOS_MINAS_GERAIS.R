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
CFEM_D1_MG <- CFEM_D %>% filter(Ano==c("2019","2020"), SiglaEstado=="MG")

# COLOCANDO OS CÓDIGOS DOS MUNICÍPIOS NOS DADOS
CFEM_MG= CFEM_D1_MG %>%
  mutate(
    Cod.IBGE = case_when(
      NomeEnte=="ABADIA DOS DOURADOS"   ~ 310010,
      NomeEnte=="ABAETÉ"   ~ 310020,
      NomeEnte=="ABRE CAMPO"   ~ 310030,
      NomeEnte=="ACAIACA"   ~ 310040,
      NomeEnte=="AÇUCENA"   ~ 310050,
      NomeEnte=="ÁGUA BOA"   ~ 310060,
      NomeEnte=="ÁGUA COMPRIDA"   ~ 310070,
      NomeEnte=="AGUANIL"   ~ 310080,
      NomeEnte=="ÁGUAS FORMOSAS"   ~ 310090,
      NomeEnte=="ÁGUAS VERMELHAS"   ~ 310100,
      NomeEnte=="AIMORÉS"   ~ 310110,
      NomeEnte=="AIURUOCA"   ~ 310120,
      NomeEnte=="ALAGOA"   ~ 310130,
      NomeEnte=="ALBERTINA"   ~ 310140,
      NomeEnte=="ALÉM PARAÍBA"   ~ 310150,
      NomeEnte=="ALFENAS"   ~ 310160,
      NomeEnte=="ALFREDO VASCONCELOS"   ~ 310163,
      NomeEnte=="ALMENARA"   ~ 310170,
      NomeEnte=="ALPERCATA"   ~ 310180,
      NomeEnte=="ALPINÓPOLIS"   ~ 310190,
      NomeEnte=="ALTEROSA"   ~ 310200,
      NomeEnte=="ALTO CAPARAÓ"   ~ 310205,
      NomeEnte=="ALTO JEQUITIBÁ"   ~ 315350,
      NomeEnte=="ALTO RIO DOCE"   ~ 310210,
      NomeEnte=="ALVARENGA"   ~ 310220,
      NomeEnte=="ALVINÓPOLIS"   ~ 310230,
      NomeEnte=="ALVORADA DE MINAS"   ~ 310240,
      NomeEnte=="AMPARO DO SERRA"   ~ 310250,
      NomeEnte=="ANDRADAS"   ~ 310260,
      NomeEnte=="ANDRELÂNDIA"   ~ 310280,
      NomeEnte=="ANGELÂNDIA"   ~ 310285,
      NomeEnte=="ANTÔNIO CARLOS"   ~ 310290,
      NomeEnte=="ANTÔNIO DIAS"   ~ 310300,
      NomeEnte=="ANTÔNIO PRADO DE MINAS"   ~ 310310,
      NomeEnte=="ARAÇAÍ"   ~ 310320,
      NomeEnte=="ARACITABA"   ~ 310330,
      NomeEnte=="ARAÇUAÍ"   ~ 310340,
      NomeEnte=="ARAGUARI"   ~ 310350,
      NomeEnte=="ARANTINA"   ~ 310360,
      NomeEnte=="ARAPONGA"   ~ 310370,
      NomeEnte=="ARAPORÃ"   ~ 310375,
      NomeEnte=="ARAPUÁ"   ~ 310380,
      NomeEnte=="ARAÚJOS"   ~ 310390,
      NomeEnte=="ARAXÁ"   ~ 310400,
      NomeEnte=="ARCEBURGO"   ~ 310410,
      NomeEnte=="ARCOS"   ~ 310420,
      NomeEnte=="AREADO"   ~ 310430,
      NomeEnte=="ARGIRITA"   ~ 310440,
      NomeEnte=="ARICANDUVA"   ~ 310445,
      NomeEnte=="ARINOS"   ~ 310450,
      NomeEnte=="ASTOLFO DUTRA"   ~ 310460,
      NomeEnte=="ATALÉIA"   ~ 310470,
      NomeEnte=="AUGUSTO DE LIMA"   ~ 310480,
      NomeEnte=="BAEPENDI"   ~ 310490,
      NomeEnte=="BALDIM"   ~ 310500,
      NomeEnte=="BAMBUÍ"   ~ 310510,
      NomeEnte=="BANDEIRA"   ~ 310520,
      NomeEnte=="BANDEIRA DO SUL"   ~ 310530,
      NomeEnte=="BARÃO DE COCAIS"   ~ 310540,
      NomeEnte=="BARÃO DE MONTE ALTO"   ~ 310550,
      NomeEnte=="BARBACENA"   ~ 310560,
      NomeEnte=="BARRA LONGA"   ~ 310570,
      NomeEnte=="BARROSO"   ~ 310590,
      NomeEnte=="BELA VISTA DE MINAS"   ~ 310600,
      NomeEnte=="BELMIRO BRAGA"   ~ 310610,
      NomeEnte=="BELO HORIZONTE"   ~ 310620,
      NomeEnte=="BELO ORIENTE"   ~ 310630,
      NomeEnte=="BELO VALE"   ~ 310640,
      NomeEnte=="BERILO"   ~ 310650,
      NomeEnte=="BERIZAL"   ~ 310665,
      NomeEnte=="BERTÓPOLIS"   ~ 310660,
      NomeEnte=="BETIM"   ~ 310670,
      NomeEnte=="BIAS FORTES"   ~ 310680,
      NomeEnte=="BICAS"   ~ 310690,
      NomeEnte=="BIQUINHAS"   ~ 310700,
      NomeEnte=="BOA ESPERANÇA"   ~ 310710,
      NomeEnte=="BOCAINA DE MINAS"   ~ 310720,
      NomeEnte=="BOCAIÚVA"   ~ 310730,
      NomeEnte=="BOM DESPACHO"   ~ 310740,
      NomeEnte=="BOM JARDIM DE MINAS"   ~ 310750,
      NomeEnte=="BOM JESUS DA PENHA"   ~ 310760,
      NomeEnte=="BOM JESUS DO AMPARO"   ~ 310770,
      NomeEnte=="BOM JESUS DO GALHO"   ~ 310780,
      NomeEnte=="BOM REPOUSO"   ~ 310790,
      NomeEnte=="BOM SUCESSO"   ~ 310800,
      NomeEnte=="BONFIM"   ~ 310810,
      NomeEnte=="BONFINÓPOLIS DE MINAS"   ~ 310820,
      NomeEnte=="BONITO DE MINAS"   ~ 310825,
      NomeEnte=="BORDA DA MATA"   ~ 310830,
      NomeEnte=="BOTELHOS"   ~ 310840,
      NomeEnte=="BOTUMIRIM"   ~ 310850,
      NomeEnte=="BRÁS PIRES"   ~ 310870,
      NomeEnte=="BRASILÂNDIA DE MINAS"   ~ 310855,
      NomeEnte=="BRASÍLIA DE MINAS"   ~ 310860,
      NomeEnte=="BRAÚNAS"   ~ 310880,
      NomeEnte=="BRAZÓPOLIS"   ~ 310890,
      NomeEnte=="BRUMADINHO"   ~ 310900,
      NomeEnte=="BUENO BRANDÃO"   ~ 310910,
      NomeEnte=="BUENÓPOLIS"   ~ 310920,
      NomeEnte=="BUGRE"   ~ 310925,
      NomeEnte=="BURITIS"   ~ 310930,
      NomeEnte=="BURITIZEIRO"   ~ 310940,
      NomeEnte=="CABECEIRA GRANDE"   ~ 310945,
      NomeEnte=="CABO VERDE"   ~ 310950,
      NomeEnte=="CACHOEIRA DOURADA"   ~ 310980,
      NomeEnte=="CACHOEIRA DA PRATA"   ~ 310960,
      NomeEnte=="CACHOEIRA DE MINAS"   ~ 310970,
      NomeEnte=="CACHOEIRA DE PAJEÚ"   ~ 310270,
      NomeEnte=="CAETANÓPOLIS"   ~ 310990,
      NomeEnte=="CAETÉ"   ~ 311000,
      NomeEnte=="CAIANA"   ~ 311010,
      NomeEnte=="CAJURI"   ~ 311020,
      NomeEnte=="CALDAS"   ~ 311030,
      NomeEnte=="CAMACHO"   ~ 311040,
      NomeEnte=="CAMANDUCAIA"   ~ 311050,
      NomeEnte=="CAMBUÍ"   ~ 311060,
      NomeEnte=="CAMBUQUIRA"   ~ 311070,
      NomeEnte=="CAMPANÁRIO"   ~ 311080,
      NomeEnte=="CAMPANHA"   ~ 311090,
      NomeEnte=="CAMPESTRE"   ~ 311100,
      NomeEnte=="CAMPINA VERDE"   ~ 311110,
      NomeEnte=="CAMPO AZUL"   ~ 311115,
      NomeEnte=="CAMPO BELO"   ~ 311120,
      NomeEnte=="CAMPO FLORIDO"   ~ 311140,
      NomeEnte=="CAMPO DO MEIO"   ~ 311130,
      NomeEnte=="CAMPOS ALTOS"   ~ 311150,
      NomeEnte=="CAMPOS GERAIS"   ~ 311160,
      NomeEnte=="CANA VERDE"   ~ 311190,
      NomeEnte=="CANAÃ"   ~ 311170,
      NomeEnte=="CANÁPOLIS"   ~ 311180,
      NomeEnte=="CANDEIAS"   ~ 311200,
      NomeEnte=="CANTAGALO"   ~ 311205,
      NomeEnte=="CAPARAÓ"   ~ 311210,
      NomeEnte=="CAPELA NOVA"   ~ 311220,
      NomeEnte=="CAPELINHA"   ~ 311230,
      NomeEnte=="CAPETINGA"   ~ 311240,
      NomeEnte=="CAPIM BRANCO"   ~ 311250,
      NomeEnte=="CAPINÓPOLIS"   ~ 311260,
      NomeEnte=="CAPITÃO ANDRADE"   ~ 311265,
      NomeEnte=="CAPITÃO ENÉAS"   ~ 311270,
      NomeEnte=="CAPITÓLIO"   ~ 311280,
      NomeEnte=="CAPUTIRA"   ~ 311290,
      NomeEnte=="CARAÍ"   ~ 311300,
      NomeEnte=="CARANAÍBA"   ~ 311310,
      NomeEnte=="CARANDAÍ"   ~ 311320,
      NomeEnte=="CARANGOLA"   ~ 311330,
      NomeEnte=="CARATINGA"   ~ 311340,
      NomeEnte=="CARBONITA"   ~ 311350,
      NomeEnte=="CAREAÇU"   ~ 311360,
      NomeEnte=="CARLOS CHAGAS"   ~ 311370,
      NomeEnte=="CARMÉSIA"   ~ 311380,
      NomeEnte=="CARMO DA CACHOEIRA"   ~ 311390,
      NomeEnte=="CARMO DA MATA"   ~ 311400,
      NomeEnte=="CARMO DE MINAS"   ~ 311410,
      NomeEnte=="CARMO DO CAJURU"   ~ 311420,
      NomeEnte=="CARMO DO PARANAÍBA"   ~ 311430,
      NomeEnte=="CARMO DO RIO CLARO"   ~ 311440,
      NomeEnte=="CARMÓPOLIS DE MINAS"   ~ 311450,
      NomeEnte=="CARNEIRINHO"   ~ 311455,
      NomeEnte=="CARRANCAS"   ~ 311460,
      NomeEnte=="CARVALHÓPOLIS"   ~ 311470,
      NomeEnte=="CARVALHOS"   ~ 311480,
      NomeEnte=="CASA GRANDE"   ~ 311490,
      NomeEnte=="CASCALHO RICO"   ~ 311500,
      NomeEnte=="CÁSSIA"   ~ 311510,
      NomeEnte=="CATAGUASES"   ~ 311530,
      NomeEnte=="CATAS ALTAS"   ~ 311535,
      NomeEnte=="CATAS ALTAS DA NORUEGA"   ~ 311540,
      NomeEnte=="CATUJI"   ~ 311545,
      NomeEnte=="CATUTI"   ~ 311547,
      NomeEnte=="CAXAMBU"   ~ 311550,
      NomeEnte=="CEDRO DO ABAETÉ"   ~ 311560,
      NomeEnte=="CENTRAL DE MINAS"   ~ 311570,
      NomeEnte=="CENTRALINA"   ~ 311580,
      NomeEnte=="CHÁCARA"   ~ 311590,
      NomeEnte=="CHALÉ"   ~ 311600,
      NomeEnte=="CHAPADA GAÚCHA"   ~ 311615,
      NomeEnte=="CHAPADA DO NORTE"   ~ 311610,
      NomeEnte=="CHIADOR"   ~ 311620,
      NomeEnte=="CIPOTÂNEA"   ~ 311630,
      NomeEnte=="CLARAVAL"   ~ 311640,
      NomeEnte=="CLARO DOS POÇÕES"   ~ 311650,
      NomeEnte=="CLÁUDIO"   ~ 311660,
      NomeEnte=="COIMBRA"   ~ 311670,
      NomeEnte=="COLUNA"   ~ 311680,
      NomeEnte=="COMENDADOR GOMES"   ~ 311690,
      NomeEnte=="COMERCINHO"   ~ 311700,
      NomeEnte=="CONCEIÇÃO DA APARECIDA"   ~ 311710,
      NomeEnte=="CONCEIÇÃO DA BARRA DE MINAS"   ~ 311520,
      NomeEnte=="CONCEIÇÃO DAS ALAGOAS"   ~ 311730,
      NomeEnte=="CONCEIÇÃO DAS PEDRAS"   ~ 311720,
      NomeEnte=="CONCEIÇÃO DE IPANEMA"   ~ 311740,
      NomeEnte=="CONCEIÇÃO DO MATO DENTRO"   ~ 311750,
      NomeEnte=="CONCEIÇÃO DO PARÁ"   ~ 311760,
      NomeEnte=="CONCEIÇÃO DO RIO VERDE"   ~ 311770,
      NomeEnte=="CONCEIÇÃO DOS OUROS"   ~ 311780,
      NomeEnte=="CÔNEGO MARINHO"   ~ 311783,
      NomeEnte=="CONFINS"   ~ 311787,
      NomeEnte=="CONGONHAL"   ~ 311790,
      NomeEnte=="CONGONHAS"   ~ 311800,
      NomeEnte=="CONGONHAS DO NORTE"   ~ 311810,
      NomeEnte=="CONQUISTA"   ~ 311820,
      NomeEnte=="CONSELHEIRO LAFAIETE"   ~ 311830,
      NomeEnte=="CONSELHEIRO PENA"   ~ 311840,
      NomeEnte=="CONSOLAÇÃO"   ~ 311850,
      NomeEnte=="CONTAGEM"   ~ 311860,
      NomeEnte=="COQUEIRAL"   ~ 311870,
      NomeEnte=="CORAÇÃO DE JESUS"   ~ 311880,
      NomeEnte=="CORDISBURGO"   ~ 311890,
      NomeEnte=="CORDISLÂNDIA"   ~ 311900,
      NomeEnte=="CORINTO"   ~ 311910,
      NomeEnte=="COROACI"   ~ 311920,
      NomeEnte=="COROMANDEL"   ~ 311930,
      NomeEnte=="CORONEL FABRICIANO"   ~ 311940,
      NomeEnte=="CORONEL MURTA"   ~ 311950,
      NomeEnte=="CORONEL PACHECO"   ~ 311960,
      NomeEnte=="CORONEL XAVIER CHAVES"   ~ 311970,
      NomeEnte=="CÓRREGO DANTA"   ~ 311980,
      NomeEnte=="CÓRREGO FUNDO"   ~ 311995,
      NomeEnte=="CÓRREGO NOVO"   ~ 312000,
      NomeEnte=="CÓRREGO DO BOM JESUS"   ~ 311990,
      NomeEnte=="COUTO DE MAGALHÃES DE MINAS"   ~ 312010,
      NomeEnte=="CRISÓLITA"   ~ 312015,
      NomeEnte=="CRISTAIS"   ~ 312020,
      NomeEnte=="CRISTÁLIA"   ~ 312030,
      NomeEnte=="CRISTIANO OTONI"   ~ 312040,
      NomeEnte=="CRISTINA"   ~ 312050,
      NomeEnte=="CRUCILÂNDIA"   ~ 312060,
      NomeEnte=="CRUZEIRO DA FORTALEZA"   ~ 312070,
      NomeEnte=="CRUZÍLIA"   ~ 312080,
      NomeEnte=="CUPARAQUE"   ~ 312083,
      NomeEnte=="CURRAL DE DENTRO"   ~ 312087,
      NomeEnte=="CURVELO"   ~ 312090,
      NomeEnte=="DATAS"   ~ 312100,
      NomeEnte=="DELFIM MOREIRA"   ~ 312110,
      NomeEnte=="DELFINÓPOLIS"   ~ 312120,
      NomeEnte=="DELTA"   ~ 312125,
      NomeEnte=="DESCOBERTO"   ~ 312130,
      NomeEnte=="DESTERRO DE ENTRE RIOS"   ~ 312140,
      NomeEnte=="DESTERRO DO MELO"   ~ 312150,
      NomeEnte=="DIAMANTINA"   ~ 312160,
      NomeEnte=="DIOGO DE VASCONCELOS"   ~ 312170,
      NomeEnte=="DIONÍSIO"   ~ 312180,
      NomeEnte=="DIVINÉSIA"   ~ 312190,
      NomeEnte=="DIVINO"   ~ 312200,
      NomeEnte=="DIVINO DAS LARANJEIRAS"   ~ 312210,
      NomeEnte=="DIVINOLÂNDIA DE MINAS"   ~ 312220,
      NomeEnte=="DIVINÓPOLIS"   ~ 312230,
      NomeEnte=="DIVISA ALEGRE"   ~ 312235,
      NomeEnte=="DIVISA NOVA"   ~ 312240,
      NomeEnte=="DIVISÓPOLIS"   ~ 312245,
      NomeEnte=="DOM BOSCO"   ~ 312247,
      NomeEnte=="DOM CAVATI"   ~ 312250,
      NomeEnte=="DOM JOAQUIM"   ~ 312260,
      NomeEnte=="DOM SILVÉRIO"   ~ 312270,
      NomeEnte=="DOM VIÇOSO"   ~ 312280,
      NomeEnte=="DONA EUSÉBIA"   ~ 312290,
      NomeEnte=="DORES DE CAMPOS"   ~ 312300,
      NomeEnte=="DORES DE GUANHÃES"   ~ 312310,
      NomeEnte=="DORES DO INDAIÁ"   ~ 312320,
      NomeEnte=="DORES DO TURVO"   ~ 312330,
      NomeEnte=="DORESÓPOLIS"   ~ 312340,
      NomeEnte=="DOURADOQUARA"   ~ 312350,
      NomeEnte=="DURANDÉ"   ~ 312352,
      NomeEnte=="ELÓI MENDES"   ~ 312360,
      NomeEnte=="ENGENHEIRO CALDAS"   ~ 312370,
      NomeEnte=="ENGENHEIRO NAVARRO"   ~ 312380,
      NomeEnte=="ENTRE FOLHAS"   ~ 312385,
      NomeEnte=="ENTRE RIOS DE MINAS"   ~ 312390,
      NomeEnte=="ERVÁLIA"   ~ 312400,
      NomeEnte=="ESMERALDAS"   ~ 312410,
      NomeEnte=="ESPERA FELIZ"   ~ 312420,
      NomeEnte=="ESPINOSA"   ~ 312430,
      NomeEnte=="ESPÍRITO SANTO DO DOURADO"   ~ 312440,
      NomeEnte=="ESTIVA"   ~ 312450,
      NomeEnte=="ESTRELA DALVA"   ~ 312460,
      NomeEnte=="ESTRELA DO INDAIÁ"   ~ 312470,
      NomeEnte=="ESTRELA DO SUL"   ~ 312480,
      NomeEnte=="EUGENÓPOLIS"   ~ 312490,
      NomeEnte=="EWBANK DA CÂMARA"   ~ 312500,
      NomeEnte=="EXTREMA"   ~ 312510,
      NomeEnte=="FAMA"   ~ 312520,
      NomeEnte=="FARIA LEMOS"   ~ 312530,
      NomeEnte=="FELÍCIO DOS SANTOS"   ~ 312540,
      NomeEnte=="FELISBURGO"   ~ 312560,
      NomeEnte=="FELIXLÂNDIA"   ~ 312570,
      NomeEnte=="FERNANDES TOURINHO"   ~ 312580,
      NomeEnte=="FERROS"   ~ 312590,
      NomeEnte=="FERVEDOURO"   ~ 312595,
      NomeEnte=="FLORESTAL"   ~ 312600,
      NomeEnte=="FORMIGA"   ~ 312610,
      NomeEnte=="FORMOSO"   ~ 312620,
      NomeEnte=="FORTALEZA DE MINAS"   ~ 312630,
      NomeEnte=="FORTUNA DE MINAS"   ~ 312640,
      NomeEnte=="FRANCISCO BADARÓ"   ~ 312650,
      NomeEnte=="FRANCISCO DUMONT"   ~ 312660,
      NomeEnte=="FRANCISCO SÁ"   ~ 312670,
      NomeEnte=="FRANCISCÓPOLIS"   ~ 312675,
      NomeEnte=="FREI GASPAR"   ~ 312680,
      NomeEnte=="FREI INOCÊNCIO"   ~ 312690,
      NomeEnte=="FREI LAGONEGRO"   ~ 312695,
      NomeEnte=="FRONTEIRA"   ~ 312700,
      NomeEnte=="FRONTEIRA DOS VALES"   ~ 312705,
      NomeEnte=="FRUTA DE LEITE"   ~ 312707,
      NomeEnte=="FRUTAL"   ~ 312710,
      NomeEnte=="FUNILÂNDIA"   ~ 312720,
      NomeEnte=="GALILÉIA"   ~ 312730,
      NomeEnte=="GAMELEIRAS"   ~ 312733,
      NomeEnte=="GLAUCILÂNDIA"   ~ 312735,
      NomeEnte=="GOIABEIRA"   ~ 312737,
      NomeEnte=="GOIANÁ"   ~ 312738,
      NomeEnte=="GONÇALVES"   ~ 312740,
      NomeEnte=="GONZAGA"   ~ 312750,
      NomeEnte=="GOUVEIA"   ~ 312760,
      NomeEnte=="GOVERNADOR VALADARES"   ~ 312770,
      NomeEnte=="GRÃO MOGOL"   ~ 312780,
      NomeEnte=="GRUPIARA"   ~ 312790,
      NomeEnte=="GUANHÃES"   ~ 312800,
      NomeEnte=="GUAPÉ"   ~ 312810,
      NomeEnte=="GUARACIABA"   ~ 312820,
      NomeEnte=="GUARACIAMA"   ~ 312825,
      NomeEnte=="GUARANÉSIA"   ~ 312830,
      NomeEnte=="GUARANI"   ~ 312840,
      NomeEnte=="GUARARÁ"   ~ 312850,
      NomeEnte=="GUARDA-MOR"   ~ 312860,
      NomeEnte=="GUAXUPÉ"   ~ 312870,
      NomeEnte=="GUIDOVAL"   ~ 312880,
      NomeEnte=="GUIMARÂNIA"   ~ 312890,
      NomeEnte=="GUIRICEMA"   ~ 312900,
      NomeEnte=="GURINHATÃ"   ~ 312910,
      NomeEnte=="HELIODORA"   ~ 312920,
      NomeEnte=="IAPU"   ~ 312930,
      NomeEnte=="IBERTIOGA"   ~ 312940,
      NomeEnte=="IBIÁ"   ~ 312950,
      NomeEnte=="IBIAÍ"   ~ 312960,
      NomeEnte=="IBIRACATU"   ~ 312965,
      NomeEnte=="IBIRACI"   ~ 312970,
      NomeEnte=="IBIRITÉ"   ~ 312980,
      NomeEnte=="IBITIÚRA DE MINAS"   ~ 312990,
      NomeEnte=="IBITURUNA"   ~ 313000,
      NomeEnte=="ICARAÍ DE MINAS"   ~ 313005,
      NomeEnte=="IGARAPÉ"   ~ 313010,
      NomeEnte=="IGARATINGA"   ~ 313020,
      NomeEnte=="IGUATAMA"   ~ 313030,
      NomeEnte=="IJACI"   ~ 313040,
      NomeEnte=="ILICÍNEA"   ~ 313050,
      NomeEnte=="IMBÉ DE MINAS"   ~ 313055,
      NomeEnte=="INCONFIDENTES"   ~ 313060,
      NomeEnte=="INDAIABIRA"   ~ 313065,
      NomeEnte=="INDIANÓPOLIS"   ~ 313070,
      NomeEnte=="INGAÍ"   ~ 313080,
      NomeEnte=="INHAPIM"   ~ 313090,
      NomeEnte=="INHAÚMA"   ~ 313100,
      NomeEnte=="INIMUTABA"   ~ 313110,
      NomeEnte=="IPABA"   ~ 313115,
      NomeEnte=="IPANEMA"   ~ 313120,
      NomeEnte=="IPATINGA"   ~ 313130,
      NomeEnte=="IPIAÇU"   ~ 313140,
      NomeEnte=="IPUIÚNA"   ~ 313150,
      NomeEnte=="IRAÍ DE MINAS"   ~ 313160,
      NomeEnte=="ITABIRA"   ~ 313170,
      NomeEnte=="ITABIRINHA"   ~ 313180,
      NomeEnte=="ITABIRITO"   ~ 313190,
      NomeEnte=="ITACAMBIRA"   ~ 313200,
      NomeEnte=="ITACARAMBI"   ~ 313210,
      NomeEnte=="ITAGUARA"   ~ 313220,
      NomeEnte=="ITAIPÉ"   ~ 313230,
      NomeEnte=="ITAJUBÁ"   ~ 313240,
      NomeEnte=="ITAMARANDIBA"   ~ 313250,
      NomeEnte=="ITAMARATI DE MINAS"   ~ 313260,
      NomeEnte=="ITAMBACURI"   ~ 313270,
      NomeEnte=="ITAMBÉ DO MATO DENTRO"   ~ 313280,
      NomeEnte=="ITAMOGI"   ~ 313290,
      NomeEnte=="ITAMONTE"   ~ 313300,
      NomeEnte=="ITANHANDU"   ~ 313310,
      NomeEnte=="ITANHOMI"   ~ 313320,
      NomeEnte=="ITAOBIM"   ~ 313330,
      NomeEnte=="ITAPAGIPE"   ~ 313340,
      NomeEnte=="ITAPECERICA"   ~ 313350,
      NomeEnte=="ITAPEVA"   ~ 313360,
      NomeEnte=="ITATIAIUÇU"   ~ 313370,
      NomeEnte=="ITAÚ DE MINAS"   ~ 313375,
      NomeEnte=="ITAÚNA"   ~ 313380,
      NomeEnte=="ITAVERAVA"   ~ 313390,
      NomeEnte=="ITINGA"   ~ 313400,
      NomeEnte=="ITUETA"   ~ 313410,
      NomeEnte=="ITUIUTABA"   ~ 313420,
      NomeEnte=="ITUMIRIM"   ~ 313430,
      NomeEnte=="ITURAMA"   ~ 313440,
      NomeEnte=="ITUTINGA"   ~ 313450,
      NomeEnte=="JABOTICATUBAS"   ~ 313460,
      NomeEnte=="JACINTO"   ~ 313470,
      NomeEnte=="JACUÍ"   ~ 313480,
      NomeEnte=="JACUTINGA"   ~ 313490,
      NomeEnte=="JAGUARAÇU"   ~ 313500,
      NomeEnte=="JAÍBA"   ~ 313505,
      NomeEnte=="JAMPRUCA"   ~ 313507,
      NomeEnte=="JANAÚBA"   ~ 313510,
      NomeEnte=="JANUÁRIA"   ~ 313520,
      NomeEnte=="JAPARAÍBA"   ~ 313530,
      NomeEnte=="JAPONVAR"   ~ 313535,
      NomeEnte=="JECEABA"   ~ 313540,
      NomeEnte=="JENIPAPO DE MINAS"   ~ 313545,
      NomeEnte=="JEQUERI"   ~ 313550,
      NomeEnte=="JEQUITAÍ"   ~ 313560,
      NomeEnte=="JEQUITIBÁ"   ~ 313570,
      NomeEnte=="JEQUITINHONHA"   ~ 313580,
      NomeEnte=="JESUÂNIA"   ~ 313590,
      NomeEnte=="JOAÍMA"   ~ 313600,
      NomeEnte=="JOANÉSIA"   ~ 313610,
      NomeEnte=="JOÃO MONLEVADE"   ~ 313620,
      NomeEnte=="JOÃO PINHEIRO"   ~ 313630,
      NomeEnte=="JOAQUIM FELÍCIO"   ~ 313640,
      NomeEnte=="JORDÂNIA"   ~ 313650,
      NomeEnte=="JOSÉ GONÇALVES DE MINAS"   ~ 313652,
      NomeEnte=="JOSÉ RAYDAN"   ~ 313655,
      NomeEnte=="JOSENÓPOLIS"   ~ 313657,
      NomeEnte=="JUATUBA"   ~ 313665,
      NomeEnte=="JUIZ DE FORA"   ~ 313670,
      NomeEnte=="JURAMENTO"   ~ 313680,
      NomeEnte=="JURUAIA"   ~ 313690,
      NomeEnte=="JUVENÍLIA"   ~ 313695,
      NomeEnte=="LADAINHA"   ~ 313700,
      NomeEnte=="LAGAMAR"   ~ 313710,
      NomeEnte=="LAGOA DOURADA"   ~ 313740,
      NomeEnte=="LAGOA FORMOSA"   ~ 313750,
      NomeEnte=="LAGOA GRANDE"   ~ 313753,
      NomeEnte=="LAGOA SANTA"   ~ 313760,
      NomeEnte=="LAGOA DA PRATA"   ~ 313720,
      NomeEnte=="LAGOA DOS PATOS"   ~ 313730,
      NomeEnte=="LAJINHA"   ~ 313770,
      NomeEnte=="LAMBARI"   ~ 313780,
      NomeEnte=="LAMIM"   ~ 313790,
      NomeEnte=="LARANJAL"   ~ 313800,
      NomeEnte=="LASSANCE"   ~ 313810,
      NomeEnte=="LAVRAS"   ~ 313820,
      NomeEnte=="LEANDRO FERREIRA"   ~ 313830,
      NomeEnte=="LEME DO PRADO"   ~ 313835,
      NomeEnte=="LEOPOLDINA"   ~ 313840,
      NomeEnte=="LIBERDADE"   ~ 313850,
      NomeEnte=="LIMA DUARTE"   ~ 313860,
      NomeEnte=="LIMEIRA DO OESTE"   ~ 313862,
      NomeEnte=="LONTRA"   ~ 313865,
      NomeEnte=="LUISBURGO"   ~ 313867,
      NomeEnte=="LUISLÂNDIA"   ~ 313868,
      NomeEnte=="LUMINÁRIAS"   ~ 313870,
      NomeEnte=="LUZ"   ~ 313880,
      NomeEnte=="MACHACALIS"   ~ 313890,
      NomeEnte=="MACHADO"   ~ 313900,
      NomeEnte=="MADRE DE DEUS DE MINAS"   ~ 313910,
      NomeEnte=="MALACACHETA"   ~ 313920,
      NomeEnte=="MAMONAS"   ~ 313925,
      NomeEnte=="MANGA"   ~ 313930,
      NomeEnte=="MANHUAÇU"   ~ 313940,
      NomeEnte=="MANHUMIRIM"   ~ 313950,
      NomeEnte=="MANTENA"   ~ 313960,
      NomeEnte=="MAR DE ESPANHA"   ~ 313980,
      NomeEnte=="MARAVILHAS"   ~ 313970,
      NomeEnte=="MARIA DA FÉ"   ~ 313990,
      NomeEnte=="MARIANA"   ~ 314000,
      NomeEnte=="MARILAC"   ~ 314010,
      NomeEnte=="MÁRIO CAMPOS"   ~ 314015,
      NomeEnte=="MARIPÁ DE MINAS"   ~ 314020,
      NomeEnte=="MARLIÉRIA"   ~ 314030,
      NomeEnte=="MARMELÓPOLIS"   ~ 314040,
      NomeEnte=="MARTINHO CAMPOS"   ~ 314050,
      NomeEnte=="MARTINS SOARES"   ~ 314053,
      NomeEnte=="MATA VERDE"   ~ 314055,
      NomeEnte=="MATERLÂNDIA"   ~ 314060,
      NomeEnte=="MATEUS LEME"   ~ 314070,
      NomeEnte=="MATHIAS LOBATO"   ~ 317150,
      NomeEnte=="MATIAS BARBOSA"   ~ 314080,
      NomeEnte=="MATIAS CARDOSO"   ~ 314085,
      NomeEnte=="MATIPÓ"   ~ 314090,
      NomeEnte=="MATO VERDE"   ~ 314100,
      NomeEnte=="MATOZINHOS"   ~ 314110,
      NomeEnte=="MATUTINA"   ~ 314120,
      NomeEnte=="MEDEIROS"   ~ 314130,
      NomeEnte=="MEDINA"   ~ 314140,
      NomeEnte=="MENDES PIMENTEL"   ~ 314150,
      NomeEnte=="MERCÊS"   ~ 314160,
      NomeEnte=="MESQUITA"   ~ 314170,
      NomeEnte=="MINAS NOVAS"   ~ 314180,
      NomeEnte=="MINDURI"   ~ 314190,
      NomeEnte=="MIRABELA"   ~ 314200,
      NomeEnte=="MIRADOURO"   ~ 314210,
      NomeEnte=="MIRAÍ"   ~ 314220,
      NomeEnte=="MIRAVÂNIA"   ~ 314225,
      NomeEnte=="MOEDA"   ~ 314230,
      NomeEnte=="MOEMA"   ~ 314240,
      NomeEnte=="MONJOLOS"   ~ 314250,
      NomeEnte=="MONSENHOR PAULO"   ~ 314260,
      NomeEnte=="MONTALVÂNIA"   ~ 314270,
      NomeEnte=="MONTE ALEGRE DE MINAS"   ~ 314280,
      NomeEnte=="MONTE AZUL"   ~ 314290,
      NomeEnte=="MONTE BELO"   ~ 314300,
      NomeEnte=="MONTE CARMELO"   ~ 314310,
      NomeEnte=="MONTE FORMOSO"   ~ 314315,
      NomeEnte=="MONTE SANTO DE MINAS"   ~ 314320,
      NomeEnte=="MONTE SIÃO"   ~ 314340,
      NomeEnte=="MONTES CLAROS"   ~ 314330,
      NomeEnte=="MONTEZUMA"   ~ 314345,
      NomeEnte=="MORADA NOVA DE MINAS"   ~ 314350,
      NomeEnte=="MORRO DA GARÇA"   ~ 314360,
      NomeEnte=="MORRO DO PILAR"   ~ 314370,
      NomeEnte=="MUNHOZ"   ~ 314380,
      NomeEnte=="MURIAÉ"   ~ 314390,
      NomeEnte=="MUTUM"   ~ 314400,
      NomeEnte=="MUZAMBINHO"   ~ 314410,
      NomeEnte=="NACIP RAYDAN"   ~ 314420,
      NomeEnte=="NANUQUE"   ~ 314430,
      NomeEnte=="NAQUE"   ~ 314435,
      NomeEnte=="NATALÂNDIA"   ~ 314437,
      NomeEnte=="NATÉRCIA"   ~ 314440,
      NomeEnte=="NAZARENO"   ~ 314450,
      NomeEnte=="NEPOMUCENO"   ~ 314460,
      NomeEnte=="NINHEIRA"   ~ 314465,
      NomeEnte=="NOVA BELÉM"   ~ 314467,
      NomeEnte=="NOVA ERA"   ~ 314470,
      NomeEnte=="NOVA LIMA"   ~ 314480,
      NomeEnte=="NOVA MÓDICA"   ~ 314490,
      NomeEnte=="NOVA PONTE"   ~ 314500,
      NomeEnte=="NOVA PORTEIRINHA"   ~ 314505,
      NomeEnte=="NOVA RESENDE"   ~ 314510,
      NomeEnte=="NOVA SERRANA"   ~ 314520,
      NomeEnte=="NOVA UNIÃO"   ~ 313660,
      NomeEnte=="NOVO CRUZEIRO"   ~ 314530,
      NomeEnte=="NOVO ORIENTE DE MINAS"   ~ 314535,
      NomeEnte=="NOVORIZONTE"   ~ 314537,
      NomeEnte=="OLARIA"   ~ 314540,
      NomeEnte=="OLHOS-D'ÁGUA"   ~ 314545,
      NomeEnte=="OLÍMPIO NORONHA"   ~ 314550,
      NomeEnte=="OLIVEIRA"   ~ 314560,
      NomeEnte=="OLIVEIRA FORTES"   ~ 314570,
      NomeEnte=="ONÇA DE PITANGUI"   ~ 314580,
      NomeEnte=="ORATÓRIOS"   ~ 314585,
      NomeEnte=="ORIZÂNIA"   ~ 314587,
      NomeEnte=="OURO BRANCO"   ~ 314590,
      NomeEnte=="OURO FINO"   ~ 314600,
      NomeEnte=="OURO PRETO"   ~ 314610,
      NomeEnte=="OURO VERDE DE MINAS"   ~ 314620,
      NomeEnte=="PADRE CARVALHO"   ~ 314625,
      NomeEnte=="PADRE PARAÍSO"   ~ 314630,
      NomeEnte=="PAI PEDRO"   ~ 314655,
      NomeEnte=="PAINEIRAS"   ~ 314640,
      NomeEnte=="PAINS"   ~ 314650,
      NomeEnte=="PAIVA"   ~ 314660,
      NomeEnte=="PALMA"   ~ 314670,
      NomeEnte=="PALMÓPOLIS"   ~ 314675,
      NomeEnte=="PAPAGAIOS"   ~ 314690,
      NomeEnte=="PARÁ DE MINAS"   ~ 314710,
      NomeEnte=="PARACATU"   ~ 314700,
      NomeEnte=="PARAGUAÇU"   ~ 314720,
      NomeEnte=="PARAISÓPOLIS"   ~ 314730,
      NomeEnte=="PARAOPEBA"   ~ 314740,
      NomeEnte=="PASSA QUATRO"   ~ 314760,
      NomeEnte=="PASSA TEMPO"   ~ 314770,
      NomeEnte=="PASSA VINTE"   ~ 314780,
      NomeEnte=="PASSABÉM"   ~ 314750,
      NomeEnte=="PASSOS"   ~ 314790,
      NomeEnte=="PATIS"   ~ 314795,
      NomeEnte=="PATOS DE MINAS"   ~ 314800,
      NomeEnte=="PATROCÍNIO"   ~ 314810,
      NomeEnte=="PATROCÍNIO DO MURIAÉ"   ~ 314820,
      NomeEnte=="PAULA CÂNDIDO"   ~ 314830,
      NomeEnte=="PAULISTAS"   ~ 314840,
      NomeEnte=="PAVÃO"   ~ 314850,
      NomeEnte=="PEÇANHA"   ~ 314860,
      NomeEnte=="PEDRA AZUL"   ~ 314870,
      NomeEnte=="PEDRA BONITA"   ~ 314875,
      NomeEnte=="PEDRA DOURADA"   ~ 314900,
      NomeEnte=="PEDRA DO ANTA"   ~ 314880,
      NomeEnte=="PEDRA DO INDAIÁ"   ~ 314890,
      NomeEnte=="PEDRALVA"   ~ 314910,
      NomeEnte=="PEDRAS DE MARIA DA CRUZ"   ~ 314915,
      NomeEnte=="PEDRINÓPOLIS"   ~ 314920,
      NomeEnte=="PEDRO LEOPOLDO"   ~ 314930,
      NomeEnte=="PEDRO TEIXEIRA"   ~ 314940,
      NomeEnte=="PEQUERI"   ~ 314950,
      NomeEnte=="PEQUI"   ~ 314960,
      NomeEnte=="PERDIGÃO"   ~ 314970,
      NomeEnte=="PERDIZES"   ~ 314980,
      NomeEnte=="PERDÕES"   ~ 314990,
      NomeEnte=="PERIQUITO"   ~ 314995,
      NomeEnte=="PESCADOR"   ~ 315000,
      NomeEnte=="PIAU"   ~ 315010,
      NomeEnte=="PIEDADE DE CARATINGA"   ~ 315015,
      NomeEnte=="PIEDADE DE PONTE NOVA"   ~ 315020,
      NomeEnte=="PIEDADE DO RIO GRANDE"   ~ 315030,
      NomeEnte=="PIEDADE DOS GERAIS"   ~ 315040,
      NomeEnte=="PIMENTA"   ~ 315050,
      NomeEnte=="PINGO D'ÁGUA"   ~ 315053,
      NomeEnte=="PINTÓPOLIS"   ~ 315057,
      NomeEnte=="PIRACEMA"   ~ 315060,
      NomeEnte=="PIRAJUBA"   ~ 315070,
      NomeEnte=="PIRANGA"   ~ 315080,
      NomeEnte=="PIRANGUÇU"   ~ 315090,
      NomeEnte=="PIRANGUINHO"   ~ 315100,
      NomeEnte=="PIRAPETINGA"   ~ 315110,
      NomeEnte=="PIRAPORA"   ~ 315120,
      NomeEnte=="PIRAÚBA"   ~ 315130,
      NomeEnte=="PITANGUI"   ~ 315140,
      NomeEnte=="PIUMHI"   ~ 315150,
      NomeEnte=="PLANURA"   ~ 315160,
      NomeEnte=="POÇO FUNDO"   ~ 315170,
      NomeEnte=="POÇOS DE CALDAS"   ~ 315180,
      NomeEnte=="POCRANE"   ~ 315190,
      NomeEnte=="POMPÉU"   ~ 315200,
      NomeEnte=="PONTE NOVA"   ~ 315210,
      NomeEnte=="PONTO CHIQUE"   ~ 315213,
      NomeEnte=="PONTO DOS VOLANTES"   ~ 315217,
      NomeEnte=="PORTEIRINHA"   ~ 315220,
      NomeEnte=="PORTO FIRME"   ~ 315230,
      NomeEnte=="POTÉ"   ~ 315240,
      NomeEnte=="POUSO ALEGRE"   ~ 315250,
      NomeEnte=="POUSO ALTO"   ~ 315260,
      NomeEnte=="PRADOS"   ~ 315270,
      NomeEnte=="PRATA"   ~ 315280,
      NomeEnte=="PRATÁPOLIS"   ~ 315290,
      NomeEnte=="PRATINHA"   ~ 315300,
      NomeEnte=="PRESIDENTE BERNARDES"   ~ 315310,
      NomeEnte=="PRESIDENTE JUSCELINO"   ~ 315320,
      NomeEnte=="PRESIDENTE KUBITSCHEK"   ~ 315330,
      NomeEnte=="PRESIDENTE OLEGÁRIO"   ~ 315340,
      NomeEnte=="PRUDENTE DE MORAIS"   ~ 315360,
      NomeEnte=="QQUARTEL GERAL"   ~ 315370,
      NomeEnte=="QUELUZITO"   ~ 315380,
      NomeEnte=="RAPOSOS"   ~ 315390,
      NomeEnte=="RAUL SOARES"   ~ 315400,
      NomeEnte=="RECREIO"   ~ 315410,
      NomeEnte=="REDUTO"   ~ 315415,
      NomeEnte=="RESENDE COSTA"   ~ 315420,
      NomeEnte=="RESPLENDOR"   ~ 315430,
      NomeEnte=="RESSAQUINHA"   ~ 315440,
      NomeEnte=="RIACHINHO"   ~ 315445,
      NomeEnte=="RIACHO DOS MACHADOS"   ~ 315450,
      NomeEnte=="RIBEIRÃO VERMELHO"   ~ 315470,
      NomeEnte=="RIBEIRÃO DAS NEVES"   ~ 315460,
      NomeEnte=="RIO ACIMA"   ~ 315480,
      NomeEnte=="RIO CASCA"   ~ 315490,
      NomeEnte=="RIO DOCE"   ~ 315500,
      NomeEnte=="RIO ESPERA"   ~ 315520,
      NomeEnte=="RIO MANSO"   ~ 315530,
      NomeEnte=="RIO NOVO"   ~ 315540,
      NomeEnte=="RIO PARANAÍBA"   ~ 315550,
      NomeEnte=="RIO PARDO DE MINAS"   ~ 315560,
      NomeEnte=="RIO PIRACICABA"   ~ 315570,
      NomeEnte=="RIO POMBA"   ~ 315580,
      NomeEnte=="RIO PRETO"   ~ 315590,
      NomeEnte=="RIO VERMELHO"   ~ 315600,
      NomeEnte=="RIO DO PRADO"   ~ 315510,
      NomeEnte=="RITÁPOLIS"   ~ 315610,
      NomeEnte=="ROCHEDO DE MINAS"   ~ 315620,
      NomeEnte=="RODEIRO"   ~ 315630,
      NomeEnte=="ROMARIA"   ~ 315640,
      NomeEnte=="ROSÁRIO DA LIMEIRA"   ~ 315645,
      NomeEnte=="RUBELITA"   ~ 315650,
      NomeEnte=="RUBIM"   ~ 315660,
      NomeEnte=="SABARÁ"   ~ 315670,
      NomeEnte=="SABINÓPOLIS"   ~ 315680,
      NomeEnte=="SACRAMENTO"   ~ 315690,
      NomeEnte=="SALINAS"   ~ 315700,
      NomeEnte=="SALTO DA DIVISA"   ~ 315710,
      NomeEnte=="SANTA BÁRBARA"   ~ 315720,
      NomeEnte=="SANTA BÁRBARA DO LESTE"   ~ 315725,
      NomeEnte=="SANTA BÁRBARA DO MONTE VERDE"   ~ 315727,
      NomeEnte=="SANTA BÁRBARA DO TUGÚRIO"   ~ 315730,
      NomeEnte=="SANTA CRUZ DE MINAS"   ~ 315733,
      NomeEnte=="SANTA CRUZ DE SALINAS"   ~ 315737,
      NomeEnte=="SANTA CRUZ DO ESCALVADO"   ~ 315740,
      NomeEnte=="SANTA EFIGÊNIA DE MINAS"   ~ 315750,
      NomeEnte=="SANTA FÉ DE MINAS"   ~ 315760,
      NomeEnte=="SANTA HELENA DE MINAS"   ~ 315765,
      NomeEnte=="SANTA JULIANA"   ~ 315770,
      NomeEnte=="SANTA LUZIA"   ~ 315780,
      NomeEnte=="SANTA MARGARIDA"   ~ 315790,
      NomeEnte=="SANTA MARIA DE ITABIRA"   ~ 315800,
      NomeEnte=="SANTA MARIA DO SALTO"   ~ 315810,
      NomeEnte=="SANTA MARIA DO SUAÇUÍ"   ~ 315820,
      NomeEnte=="SANTA RITA DE CALDAS"   ~ 315920,
      NomeEnte=="SANTA RITA DE IBITIPOCA"   ~ 315940,
      NomeEnte=="SANTA RITA DE JACUTINGA"   ~ 315930,
      NomeEnte=="SANTA RITA DE MINAS"   ~ 315935,
      NomeEnte=="SANTA RITA DO ITUETO"   ~ 315950,
      NomeEnte=="SANTA RITA DO SAPUCAÍ"   ~ 315960,
      NomeEnte=="SANTA ROSA DA SERRA"   ~ 315970,
      NomeEnte=="SANTA VITÓRIA"   ~ 315980,
      NomeEnte=="SANTANA DA VARGEM"   ~ 315830,
      NomeEnte=="SANTANA DE CATAGUASES"   ~ 315840,
      NomeEnte=="SANTANA DE PIRAPAMA"   ~ 315850,
      NomeEnte=="SANTANA DO DESERTO"   ~ 315860,
      NomeEnte=="SANTANA DO GARAMBÉU"   ~ 315870,
      NomeEnte=="SANTANA DO JACARÉ"   ~ 315880,
      NomeEnte=="SANTANA DO MANHUAÇU"   ~ 315890,
      NomeEnte=="SANTANA DO PARAÍSO"   ~ 315895,
      NomeEnte=="SANTANA DO RIACHO"   ~ 315900,
      NomeEnte=="SANTANA DOS MONTES"   ~ 315910,
      NomeEnte=="SANTO ANTÔNIO DO AMPARO"   ~ 315990,
      NomeEnte=="SANTO ANTÔNIO DO AVENTUREIRO"   ~ 316000,
      NomeEnte=="SANTO ANTÔNIO DO GRAMA"   ~ 316010,
      NomeEnte=="SANTO ANTÔNIO DO ITAMBÉ"   ~ 316020,
      NomeEnte=="SANTO ANTÔNIO DO JACINTO"   ~ 316030,
      NomeEnte=="SANTO ANTÔNIO DO MONTE"   ~ 316040,
      NomeEnte=="SANTO ANTÔNIO DO RETIRO"   ~ 316045,
      NomeEnte=="SANTO ANTÔNIO DO RIO ABAIXO"   ~ 316050,
      NomeEnte=="SANTO HIPÓLITO"   ~ 316060,
      NomeEnte=="SANTOS DUMONT"   ~ 316070,
      NomeEnte=="SÃO BENTO ABADE"   ~ 316080,
      NomeEnte=="SÃO BRÁS DO SUAÇUÍ"   ~ 316090,
      NomeEnte=="SÃO DOMINGOS DAS DORES"   ~ 316095,
      NomeEnte=="SÃO DOMINGOS DO PRATA"   ~ 316100,
      NomeEnte=="SÃO FÉLIX DE MINAS"   ~ 316105,
      NomeEnte=="SÃO FRANCISCO"   ~ 316110,
      NomeEnte=="SÃO FRANCISCO DE PAULA"   ~ 316120,
      NomeEnte=="SÃO FRANCISCO DE SALES"   ~ 316130,
      NomeEnte=="SÃO FRANCISCO DO GLÓRIA"   ~ 316140,
      NomeEnte=="SÃO GERALDO"   ~ 316150,
      NomeEnte=="SÃO GERALDO DA PIEDADE"   ~ 316160,
      NomeEnte=="SÃO GERALDO DO BAIXIO"   ~ 316165,
      NomeEnte=="SÃO GONÇALO DO ABAETÉ"   ~ 316170,
      NomeEnte=="SÃO GONÇALO DO PARÁ"   ~ 316180,
      NomeEnte=="SÃO GONÇALO DO RIO ABAIXO"   ~ 316190,
      NomeEnte=="SÃO GONÇALO DO RIO PRETO"   ~ 312550,
      NomeEnte=="SÃO GONÇALO DO SAPUCAÍ"   ~ 316200,
      NomeEnte=="SÃO GOTARDO"   ~ 316210,
      NomeEnte=="SÃO JOÃO BATISTA DO GLÓRIA"   ~ 316220,
      NomeEnte=="SÃO JOÃO EVANGELISTA"   ~ 316280,
      NomeEnte=="SÃO JOÃO NEPOMUCENO"   ~ 316290,
      NomeEnte=="SÃO JOÃO DA LAGOA"   ~ 316225,
      NomeEnte=="SÃO JOÃO DA MATA"   ~ 316230,
      NomeEnte=="SÃO JOÃO DA PONTE"   ~ 316240,
      NomeEnte=="SÃO JOÃO DAS MISSÕES"   ~ 316245,
      NomeEnte=="SÃO JOÃO DEL REI"   ~ 316250,
      NomeEnte=="SÃO JOÃO DO MANHUAÇU"   ~ 316255,
      NomeEnte=="SÃO JOÃO DO MANTENINHA"   ~ 316257,
      NomeEnte=="SÃO JOÃO DO ORIENTE"   ~ 316260,
      NomeEnte=="SÃO JOÃO DO PACUÍ"   ~ 316265,
      NomeEnte=="SÃO JOÃO DO PARAÍSO"   ~ 316270,
      NomeEnte=="SÃO JOAQUIM DE BICAS"   ~ 316292,
      NomeEnte=="SÃO JOSÉ DA BARRA"   ~ 316294,
      NomeEnte=="SÃO JOSÉ DA LAPA"   ~ 316295,
      NomeEnte=="SÃO JOSÉ DA SAFIRA"   ~ 316300,
      NomeEnte=="SÃO JOSÉ DA VARGINHA"   ~ 316310,
      NomeEnte=="SÃO JOSÉ DO ALEGRE"   ~ 316320,
      NomeEnte=="SÃO JOSÉ DO DIVINO"   ~ 316330,
      NomeEnte=="SÃO JOSÉ DO GOIABAL"   ~ 316340,
      NomeEnte=="SÃO JOSÉ DO JACURI"   ~ 316350,
      NomeEnte=="SÃO JOSÉ DO MANTIMENTO"   ~ 316360,
      NomeEnte=="SÃO LOURENÇO"   ~ 316370,
      NomeEnte=="SÃO MIGUEL DO ANTA"   ~ 316380,
      NomeEnte=="SÃO PEDRO DA UNIÃO"   ~ 316390,
      NomeEnte=="SÃO PEDRO DO SUAÇUÍ"   ~ 316410,
      NomeEnte=="SÃO PEDRO DOS FERROS"   ~ 316400,
      NomeEnte=="SÃO ROMÃO"   ~ 316420,
      NomeEnte=="SÃO ROQUE DE MINAS"   ~ 316430,
      NomeEnte=="SÃO SEBASTIÃO DA BELA VISTA"   ~ 316440,
      NomeEnte=="SÃO SEBASTIÃO DA VARGEM ALEGRE"   ~ 316443,
      NomeEnte=="SÃO SEBASTIÃO DO ANTA"   ~ 316447,
      NomeEnte=="SÃO SEBASTIÃO DO MARANHÃO"   ~ 316450,
      NomeEnte=="SÃO SEBASTIÃO DO OESTE"   ~ 316460,
      NomeEnte=="SÃO SEBASTIÃO DO PARAÍSO"   ~ 316470,
      NomeEnte=="SÃO SEBASTIÃO DO RIO PRETO"   ~ 316480,
      NomeEnte=="SÃO SEBASTIÃO DO RIO VERDE"   ~ 316490,
      NomeEnte=="São Tomé das Letras"   ~ 316520,
      NomeEnte=="SÃO TIAGO"   ~ 316500,
      NomeEnte=="SÃO TOMÁS DE AQUINO"   ~ 316510,
      NomeEnte=="SÃO VICENTE DE MINAS"   ~ 316530,
      NomeEnte=="SAPUCAÍ-MIRIM"   ~ 316540,
      NomeEnte=="SARDOÁ"   ~ 316550,
      NomeEnte=="SARZEDO"   ~ 316553,
      NomeEnte=="SEM-PEIXE"   ~ 316556,
      NomeEnte=="SENADOR AMARAL"   ~ 316557,
      NomeEnte=="SENADOR CORTES"   ~ 316560,
      NomeEnte=="SENADOR FIRMINO"   ~ 316570,
      NomeEnte=="SENADOR JOSÉ BENTO"   ~ 316580,
      NomeEnte=="SENADOR MODESTINO GONÇALVES"   ~ 316590,
      NomeEnte=="SENHORA DE OLIVEIRA"   ~ 316600,
      NomeEnte=="SENHORA DO PORTO"   ~ 316610,
      NomeEnte=="SENHORA DOS REMÉDIOS"   ~ 316620,
      NomeEnte=="SERICITA"   ~ 316630,
      NomeEnte=="SERITINGA"   ~ 316640,
      NomeEnte=="SERRA AZUL DE MINAS"   ~ 316650,
      NomeEnte=="SERRA DA SAUDADE"   ~ 316660,
      NomeEnte=="SERRA DO SALITRE"   ~ 316680,
      NomeEnte=="SERRA DOS AIMORÉS"   ~ 316670,
      NomeEnte=="SERRANIA"   ~ 316690,
      NomeEnte=="SERRANÓPOLIS DE MINAS"   ~ 316695,
      NomeEnte=="SERRANOS"   ~ 316700,
      NomeEnte=="SERRO"   ~ 316710,
      NomeEnte=="SETE LAGOAS"   ~ 316720,
      NomeEnte=="SETUBINHA"   ~ 316555,
      NomeEnte=="SILVEIRÂNIA"   ~ 316730,
      NomeEnte=="SILVIANÓPOLIS"   ~ 316740,
      NomeEnte=="SIMÃO PEREIRA"   ~ 316750,
      NomeEnte=="SIMONÉSIA"   ~ 316760,
      NomeEnte=="SOBRÁLIA"   ~ 316770,
      NomeEnte=="SOLEDADE DE MINAS"   ~ 316780,
      NomeEnte=="TABULEIRO"   ~ 316790,
      NomeEnte=="TAIOBEIRAS"   ~ 316800,
      NomeEnte=="TAPARUBA"   ~ 316805,
      NomeEnte=="TAPIRA"   ~ 316810,
      NomeEnte=="TAPIRAÍ"   ~ 316820,
      NomeEnte=="TAQUARAÇU DE MINAS"   ~ 316830,
      NomeEnte=="TARUMIRIM"   ~ 316840,
      NomeEnte=="TEIXEIRAS"   ~ 316850,
      NomeEnte=="TEÓFILO OTONI"   ~ 316860,
      NomeEnte=="TIMÓTEO"   ~ 316870,
      NomeEnte=="TIRADENTES"   ~ 316880,
      NomeEnte=="TIROS"   ~ 316890,
      NomeEnte=="TOCANTINS"   ~ 316900,
      NomeEnte=="TOCOS DO MOJI"   ~ 316905,
      NomeEnte=="TOLEDO"   ~ 316910,
      NomeEnte=="TOMBOS"   ~ 316920,
      NomeEnte=="TRÊS CORAÇÕES"   ~ 316930,
      NomeEnte=="TRÊS MARIAS"   ~ 316935,
      NomeEnte=="TRÊS PONTAS"   ~ 316940,
      NomeEnte=="TUMIRITINGA"   ~ 316950,
      NomeEnte=="TUPACIGUARA"   ~ 316960,
      NomeEnte=="TURMALINA"   ~ 316970,
      NomeEnte=="TURVOLÂNDIA"   ~ 316980,
      NomeEnte=="UBÁ"   ~ 316990,
      NomeEnte=="UBAÍ"   ~ 317000,
      NomeEnte=="UBAPORANGA"   ~ 317005,
      NomeEnte=="UBERABA"   ~ 317010,
      NomeEnte=="UBERLÂNDIA"   ~ 317020,
      NomeEnte=="UMBURATIBA"   ~ 317030,
      NomeEnte=="UNAÍ"   ~ 317040,
      NomeEnte=="UNIÃO DE MINAS"   ~ 317043,
      NomeEnte=="URUANA DE MINAS"   ~ 317047,
      NomeEnte=="URUCÂNIA"   ~ 317050,
      NomeEnte=="URUCUIA"   ~ 317052,
      NomeEnte=="VARGEM ALEGRE"   ~ 317057,
      NomeEnte=="VARGEM BONITA"   ~ 317060,
      NomeEnte=="VARGEM GRANDE DO RIO PARDO"   ~ 317065,
      NomeEnte=="VARGINHA"   ~ 317070,
      NomeEnte=="VARJÃO DE MINAS"   ~ 317075,
      NomeEnte=="VÁRZEA DA PALMA"   ~ 317080,
      NomeEnte=="VARZELÂNDIA"   ~ 317090,
      NomeEnte=="VAZANTE"   ~ 317100,
      NomeEnte=="VERDELÂNDIA"   ~ 317103,
      NomeEnte=="VEREDINHA"   ~ 317107,
      NomeEnte=="VERÍSSIMO"   ~ 317110,
      NomeEnte=="VERMELHO NOVO"   ~ 317115,
      NomeEnte=="VESPASIANO"   ~ 317120,
      NomeEnte=="VIÇOSA"   ~ 317130,
      NomeEnte=="VIEIRAS"   ~ 317140,
      NomeEnte=="VIRGEM DA LAPA"   ~ 317160,
      NomeEnte=="VIRGÍNIA"   ~ 317170,
      NomeEnte=="VIRGINÓPOLIS"   ~ 317180,
      NomeEnte=="VIRGOLÂNDIA"   ~ 317190,
      NomeEnte=="VISCONDE DO RIO BRANCO"   ~ 317200,
      NomeEnte=="VOLTA GRANDE"   ~ 317210,
      NomeEnte=="WENCESLAU BRAZ"   ~ 317220
    ))

write.csv2(CFEM_MG, "DADOS_CFEM_MG.csv", dec=",")

## UTILIZAR A NOVA BASE DE MINAS 

########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA SAÚDE ##################

# SELEÇÃO APENAS DOS MUNICÍPIOS DO MARANHÃO
SAUDE_MG    <- SAUDE   %>% filter(UF=="MG")

# CRIANDO UMA COLUNA COM OS ANOS
SAUDE_MG_2019 <- SAUDE_MG %>% select(Cod.IBGE, Município, "2019" )%>% mutate(Ano=2019)
SAUDE_MG_2020 <- SAUDE_MG %>% select(Cod.IBGE, Município, "2020" )%>% mutate(Ano=2020)

# RENOMEANDO COLUNAS PARA JUNTAR OS DADOS
SAUDE_MG_2019 <- rename(SAUDE_MG_2019, Investimento="2019", NomeEnte="Município")
SAUDE_MG_2020 <- rename(SAUDE_MG_2020, Investimento="2020", NomeEnte="Município")

## JUNTANTO OS DATA.FRAMES
SAUDE_MG_TOTAL <- rbind(SAUDE_MG_2019, SAUDE_MG_2020)



########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA SAÚDE ##################

# VERIFICAR O VALOR TOTAL RECOLHIDO POR ANO SEGUNDO O MUNICIPÍO
CFEM_MG_ANO <- CFEM_MG %>% select(Ano, NomeEnte, Cod.IBGE, Valor)%>%
  group_by(Ano,NomeEnte,Cod.IBGE) %>% summarise(TotalDistribuido=sum(Valor))

# ALTERANDO O TIPO DA VARIÁVEL PARA CARACTERE
CFEM_MG_ANO <- CFEM_MG_ANO %>% mutate(Cod.IBGE=as.character(Cod.IBGE))

# TotalRecolhido = as.character(format(TotalDistribuido,scientific = FALSE)
# CFEM_MA_ANO   <- CFEM_MA_ANO %>% mutate(TotalDistribuido=as.integer(TotalDistribuido))


#######################################################################################
# UNINDO OS DOIS BANCOS DE DADOS SEGUNDO O MUNCÍPIO E O ANO
join<-inner_join(SAUDE_MG_TOTAL,CFEM_MG_ANO ,by=c("Cod.IBGE", "Ano"))

#######################################################################################
#######################################################################################
# SELECIONANDO APENAS OS DADOS DE INTERESSE BASE JOIN
DADOS  <- join %>% select(Cod.IBGE,Ano, NomeEnte.y,TotalDistribuido, Investimento )
DADOS  <- mutate(DADOS , Ano=as.character(Ano))


#######################################################################################
#######################################################################################
# ANÁLISE EXPLORATÓRIA DOS DADOS
par(mfrow=c(2, 1), mar=c(5, 4, 2, 1))

## VALORES DISTRIBUÍDOS DOS ROYALTS POR MUNICÍPIO
pdf("boxplot_Ano_MG.pdf") 
ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = Ano, y = TotalDistribuido))+
  labs(x= "Ano", y="Total Distribuído")
dev.off( ) 


## VALORES INVESTIDOS EM SAUDE POR MUNICÍPIO

pdf("boxplot_Ano_MG_saude.pdf") 
ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = Ano, y = Investimento))+
  labs(x= "Ano", y="Total Investido na Saúde")
dev.off( ) 


## ASSOCIAÇÃO ENTRE AS VARIÁVEIS
# Retirando os NAS e considerando os outliers
DATA <- DADOS[!is.na(DADOS$Investimento),]
cor(DATA$Investimento, DATA$TotalDistribuido)

# Retirando os outliers
outliers = c(boxplot.stats(DADOS$TotalDistribuido)$out,
             boxplot.stats(DADOS$Investimento)$out)

data_outliers = DADOS[-c(which(DADOS$TotalDistribuido %in% outliers),
                         which(DADOS$Investimento %in% outliers)),]

# Retirando os NAS 
data1<-data_outliers[!is.na(data_outliers$Investimento),]
cor(data1$TotalDistribuido , data1$Investimento)


#######################################################################################
#######################################################################################    

select.outliers = DADOS[DADOS$TotalDistribuido> min(boxplot.stats(DADOS$TotalDistribuido)$out) |
                          DADOS$Investimento > min(boxplot.stats(DADOS$Investimento)$out),]


pdf("ASSOCICAO_MG.pdf") 
ggplot(data_outliers, aes(x=TotalDistribuido, y=Investimento))+
  geom_point(stat='identity') +
  labs(x= "Valor Distribuído para os Municípios", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

# Considerando o log a fim de melhorar o ajuste dos dados.

pdf("ASSOCICAO_log_MG.pdf") 
ggplot(data_outliers, aes(x= log(TotalDistribuido), y= log(Investimento)))+
  geom_point(stat='identity') +
  labs(x= "LOG do Valor Distribuído para os Municípios", y='LOG do Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()








####################################################################################
####################################################################################
####################################################################################
################## selecionando apenas os município afetados #######################
####################################################################################


# SELECIONAR APENAS OS MUNICÍPIOS AFETADOS

CFEM_MG_AFETADO <- filter(CFEM_MG, TipoDistribuição == "Afetado" )

# VERIFICAR O VALOR TOTAL RECOLHIDO POR ANO SEGUNDO O MUNICIPÍO
CFEM_MG_ANO_AFETADO <- CFEM_MG_AFETADO %>% select(Ano, NomeEnte, Cod.IBGE, Valor)%>%
  group_by(Ano,NomeEnte,Cod.IBGE) %>% summarise(TotalDistribuido=sum(Valor))

# ALTERANDO O TIPO DA VARIÁVEL PARA CARACTERE
CFEM_MG_ANO_AFETADO <- CFEM_MG_ANO_AFETADO %>% mutate(Cod.IBGE=as.character(Cod.IBGE))


#######################################################################################
# UNINDO OS DOIS BANCOS DE DADOS SEGUNDO O MUNCÍPIO E O ANO
join_AFETADO<-inner_join(SAUDE_MG_TOTAL,CFEM_MG_ANO_AFETADO ,by=c("Cod.IBGE", "Ano"))

#######################################################################################
#######################################################################################
# SELECIONANDO APENAS OS DADOS DE INTERESSE BASE JOIN
DADOS_AFETADO  <- join_AFETADO %>% select(Cod.IBGE,Ano, NomeEnte.y,TotalDistribuido, Investimento )
DADOS_AFETADO  <- mutate(DADOS_AFETADO , Ano=as.character(Ano))



# REALIZANDO AS MESMAS ANÁLISES ANTERIORES PORÉM PARA OS MUNICÍPIOS AFETADOS
#######################################################################################
#######################################################################################

## VALORES DISTRIBUÍDOS DOS ROYALTS POR MUNICÍPIO
pdf("boxplot_Ano_MG_AFETADO.pdf") 
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = Ano, y = TotalDistribuido))+
  labs(x= "Ano", y="Total Distribuído")
dev.off( ) 


## VALORES INVESTIDOS EM SAUDE POR MUNICÍPIO

pdf("boxplot_Ano_MG_saude_AFETADO.pdf") 
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = Ano, y = Investimento))+
  labs(x= "Ano", y="Total Investido na Saúde")
dev.off( ) 



## ASSOCIAÇÃO ENTRE AS VARIÁVEIS
# Retirando os NAS e considerando os outliers
DATA_AFETADO <- DADOS_AFETADO[!is.na(DADOS_AFETADO$Investimento),]
cor(DATA_AFETADO$Investimento, DATA_AFETADO$TotalDistribuido)

# Retirando os outliers
outliers_A = c(boxplot.stats(DADOS_AFETADO$TotalDistribuido)$out,
               boxplot.stats(DADOS_AFETADO$Investimento)$out)

data_outliers_AFETADO = DADOS_AFETADO[-c(which(DADOS_AFETADO$TotalDistribuido %in% outliers_A),
                                         which(DADOS_AFETADO$Investimento %in% outliers_A)),]

# Retirando os NAS 
data1_AFETADO<-data_outliers_AFETADO[!is.na(data_outliers_AFETADO$Investimento),]
cor(data1_AFETADO$TotalDistribuido , data1_AFETADO$Investimento)

# Correlação apenas para os máximos diários

pdf("DISTRIBUICAO_MG_AFETADO.pdf")
datacor <- data1_AFETADO %>% select_cols(Investimento, TotalDistribuido)
pairs(datacor)
dev.off()

pdf("CORRELACAO_MG.pdf")
corr_plot(datacor)
dev.off()


#######################################################################################
#######################################################################################    

select.outliers_A = DADOS_AFETADO[DADOS_AFETADO$TotalDistribuido> min(boxplot.stats(DADOS_AFETADO$TotalDistribuido)$out) |
                                    DADOS_AFETADO$Investimento > min(boxplot.stats(DADOS_AFETADO$Investimento)$out),]

pdf("ASSOCICAO_AFETADOS_MG.pdf") 
ggplot(data_outliers_AFETADO, aes(x=TotalDistribuido, y=Investimento))+
  geom_point(stat='identity') +
  labs(x= "Valor Distribuído para os Municípios", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

# Considerando o log a fim de melhorar o ajuste dos dados.

pdf("ASSOCICAO_log_AFETADOS_MG.pdf") 
ggplot(data_outliers_AFETADO, aes(x= log(TotalDistribuido), y= log(Investimento)))+
  geom_point(stat='identity') +
  labs(x= "LOG do Valor Distribuído para os Municípios", y='LOG do Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()


################################################################################
################################################################################




