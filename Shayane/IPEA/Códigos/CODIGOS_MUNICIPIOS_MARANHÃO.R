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
CFEM_D1_MA <- CFEM_D %>% filter(Ano==c("2019","2020"), SiglaEstado=="MA")

# COLOCANDO OS CÓDIGOS DOS MUNICÍPIOS NOS DADOS
CFEM_MA = CFEM_D1_MA %>%
  mutate(
    Cod.IBGE = case_when(
      NomeEnte=="AÇAILÂNDIA"  ~ 210005,
      NomeEnte=="AFONSO CUNHA"  ~ 210010,
      NomeEnte=="ÁGUA DOCE DO MARANHÃO"  ~ 210015,
      NomeEnte=="ALCÂNTARA"  ~ 210020,
      NomeEnte=="ALDEIAS ALTAS"  ~ 210030,
      NomeEnte=="ALTAMIRA DO MARANHÃO"  ~ 210040,
      NomeEnte=="ALTO ALEGRE DO MARANHÃO"  ~ 210043,
      NomeEnte=="ALTO ALEGRE DO PINDARÉ"  ~ 210047,
      NomeEnte=="ALTO PARNAÍBA"  ~ 210050,
      NomeEnte=="AMAPÁ DO MARANHÃO"  ~ 210055,
      NomeEnte=="AMARANTE DO MARANHÃO"  ~ 210060,
      NomeEnte=="ANAJATUBA"  ~ 210070,
      NomeEnte=="ANAPURUS"  ~ 210080,
      NomeEnte=="APICUM-AÇU"  ~ 210083,
      NomeEnte=="ARAGUANÃ"  ~ 210087,
      NomeEnte=="ARAIOSES"  ~ 210090,
      NomeEnte=="ARAME"  ~ 210095,
      NomeEnte=="ARARI"  ~ 210100,
      NomeEnte=="AXIXÁ"  ~ 210110,
      NomeEnte=="BACABAL"  ~ 210120,
      NomeEnte=="BACABEIRA"  ~ 210125,
      NomeEnte=="BACURI"  ~ 210130,
      NomeEnte=="BACURITUBA"  ~ 210135,
      NomeEnte=="BALSAS"  ~ 210140,
      NomeEnte=="BARÃO DE GRAJAÚ"  ~ 210150,
      NomeEnte=="BARRA DO CORDA"  ~ 210160,
      NomeEnte=="BARREIRINHAS"  ~ 210170,
      NomeEnte=="BELA VISTA DO MARANHÃO"  ~ 210177,
      NomeEnte=="BELÁGUA"  ~ 210173,
      NomeEnte=="BENEDITO LEITE"  ~ 210180,
      NomeEnte=="BEQUIMÃO"  ~ 210190,
      NomeEnte=="BERNARDO DO MEARIM"  ~ 210193,
      NomeEnte=="BOA VISTA DO GURUPI"  ~ 210197,
      NomeEnte=="BOM JARDIM"  ~ 210200,
      NomeEnte=="BOM JESUS DAS SELVAS"  ~ 210203,
      NomeEnte=="BOM LUGAR"  ~ 210207,
      NomeEnte=="BREJO"  ~ 210210,
      NomeEnte=="BREJO DE AREIA"  ~ 210215,
      NomeEnte=="BURITI"  ~ 210220,
      NomeEnte=="BURITI BRAVO"  ~ 210230,
      NomeEnte=="BURITICUPU"  ~ 210232,
      NomeEnte=="BURITIRANA"  ~ 210235,
      NomeEnte=="CACHOEIRA GRANDE"  ~ 210237,
      NomeEnte=="CAJAPIÓ"  ~ 210240,
      NomeEnte=="CAJARI"  ~ 210250,
      NomeEnte=="CAMPESTRE DO MARANHÃO"  ~ 210255,
      NomeEnte=="CÂNDIDO MENDES"  ~ 210260,
      NomeEnte=="CANTANHEDE"  ~ 210270,
      NomeEnte=="CAPINZAL DO NORTE"  ~ 210275,
      NomeEnte=="CAROLINA"  ~ 210280,
      NomeEnte=="CARUTAPERA"  ~ 210290,
      NomeEnte=="CAXIAS"  ~ 210300,
      NomeEnte=="CEDRAL"  ~ 210310,
      NomeEnte=="CENTRAL DO MARANHÃO"  ~ 210312,
      NomeEnte=="CENTRO NOVO DO MARANHÃO"  ~ 210317,
      NomeEnte=="CENTRO DO GUILHERME"  ~ 210315,
      NomeEnte=="CHAPADINHA"  ~ 210320,
      NomeEnte=="CIDELÂNDIA"  ~ 210325,
      NomeEnte=="CODÓ"  ~ 210330,
      NomeEnte=="COELHO NETO"  ~ 210340,
      NomeEnte=="COLINAS"  ~ 210350,
      NomeEnte=="CONCEIÇÃO DO LAGO-AÇU"  ~ 210355,
      NomeEnte=="COROATÁ"  ~ 210360,
      NomeEnte=="CURURUPU"  ~ 210370,
      NomeEnte=="DAVINÓPOLIS"  ~ 210375,
      NomeEnte=="DOM PEDRO"  ~ 210380,
      NomeEnte=="DUQUE BACELAR"  ~ 210390,
      NomeEnte=="ESPERANTINÓPOLIS"  ~ 210400,
      NomeEnte=="ESTREITO"  ~ 210405,
      NomeEnte=="FEIRA NOVA DO MARANHÃO"  ~ 210407,
      NomeEnte=="FERNANDO FALCÃO"  ~ 210408,
      NomeEnte=="FORMOSA DA SERRA NEGRA"  ~ 210409,
      NomeEnte=="FORTALEZA DOS NOGUEIRAS"  ~ 210410,
      NomeEnte=="FORTUNA"  ~ 210420,
      NomeEnte=="GODOFREDO VIANA"  ~ 210430,
      NomeEnte=="GONÇALVES DIAS"  ~ 210440,
      NomeEnte=="GOVERNADOR ARCHER"  ~ 210450,
      NomeEnte=="GOVERNADOR EDISON LOBÃO"  ~ 210455,
      NomeEnte=="GOVERNADOR EUGÊNIO BARROS"  ~ 210460,
      NomeEnte=="GOVERNADOR LUIZ ROCHA"  ~ 210462,
      NomeEnte=="GOVERNADOR NEWTON BELLO"  ~ 210465,
      NomeEnte=="GOVERNADOR NUNES FREIRE"  ~ 210467,
      NomeEnte=="GRAÇA ARANHA"  ~ 210470,
      NomeEnte=="GRAJAÚ"  ~ 210480,
      NomeEnte=="GUIMARÃES"  ~ 210490,
      NomeEnte=="HUMBERTO DE CAMPOS"  ~ 210500,
      NomeEnte=="ICATU"  ~ 210510,
      NomeEnte=="IGARAPÉ GRANDE"  ~ 210520,
      NomeEnte=="IGARAPÉ DO MEIO"  ~ 210515,
      NomeEnte=="IMPERATRIZ"  ~ 210530,
      NomeEnte=="ITAIPAVA DO GRAJAÚ"  ~ 210535,
      NomeEnte=="ITAPECURU MIRIM"  ~ 210540,
      NomeEnte=="ITINGA DO MARANHÃO"  ~ 210542,
      NomeEnte=="JATOBÁ"  ~ 210545,
      NomeEnte=="JENIPAPO DOS VIEIRAS"  ~ 210547,
      NomeEnte=="JOÃO LISBOA"  ~ 210550,
      NomeEnte=="JOSELÂNDIA"  ~ 210560,
      NomeEnte=="JUNCO DO MARANHÃO"  ~ 210565,
      NomeEnte=="LAGO VERDE"  ~ 210590,
      NomeEnte=="LAGO DA PEDRA"  ~ 210570,
      NomeEnte=="LAGO DO JUNCO"  ~ 210580,
      NomeEnte=="LAGO DOS RODRIGUES"  ~ 210594,
      NomeEnte=="LAGOA GRANDE DO MARANHÃO"  ~ 210596,
      NomeEnte=="LAGOA DO MATO"  ~ 210592,
      NomeEnte=="LAJEADO NOVO"  ~ 210598,
      NomeEnte=="LIMA CAMPOS"  ~ 210600,
      NomeEnte=="LORETO"  ~ 210610,
      NomeEnte=="LUÍS DOMINGUES"  ~ 210620,
      NomeEnte=="MAGALHÃES DE ALMEIDA"  ~ 210630,
      NomeEnte=="MARACAÇUMÉ"  ~ 210632,
      NomeEnte=="MARAJÁ DO SENA"  ~ 210635,
      NomeEnte=="MARANHÃOZINHO"  ~ 210637,
      NomeEnte=="MATA ROMA"  ~ 210640,
      NomeEnte=="MATINHA"  ~ 210650,
      NomeEnte=="MATÕES"  ~ 210660,
      NomeEnte=="MATÕES DO NORTE"  ~ 210663,
      NomeEnte=="MILAGRES DO MARANHÃO"  ~ 210667,
      NomeEnte=="MIRADOR"  ~ 210670,
      NomeEnte=="MIRANDA DO NORTE"  ~ 210675,
      NomeEnte=="MIRINZAL"  ~ 210680,
      NomeEnte=="MONÇÃO"  ~ 210690,
      NomeEnte=="MONTES ALTOS"  ~ 210700,
      NomeEnte=="MORROS"  ~ 210710,
      NomeEnte=="NINA RODRIGUES"  ~ 210720,
      NomeEnte=="NOVA COLINAS"  ~ 210725,
      NomeEnte=="NOVA IORQUE"  ~ 210730,
      NomeEnte=="NOVA OLINDA DO MARANHÃO"  ~ 210735,
      NomeEnte=="OLHO D'ÁGUA DAS CUNHÃS"  ~ 210740,
      NomeEnte=="OLINDA NOVA DO MARANHÃO"  ~ 210745,
      NomeEnte=="PAÇO DO LUMIAR"  ~ 210750,
      NomeEnte=="PALMEIRÂNDIA"  ~ 210760,
      NomeEnte=="PARAIBANO"  ~ 210770,
      NomeEnte=="PARNARAMA"  ~ 210780,
      NomeEnte=="PASSAGEM FRANCA"  ~ 210790,
      NomeEnte=="PASTOS BONS"  ~ 210800,
      NomeEnte=="PAULINO NEVES"  ~ 210805,
      NomeEnte=="PAULO RAMOS"  ~ 210810,
      NomeEnte=="PEDREIRAS"  ~ 210820,
      NomeEnte=="PEDRO DO ROSÁRIO"  ~ 210825,
      NomeEnte=="PENALVA"  ~ 210830,
      NomeEnte=="PERI MIRIM"  ~ 210840,
      NomeEnte=="PERITORÓ"  ~ 210845,
      NomeEnte=="PINDARÉ-MIRIM"  ~ 210850,
      NomeEnte=="PINHEIRO"  ~ 210860,
      NomeEnte=="PIO XII"  ~ 210870,
      NomeEnte=="PIRAPEMAS"  ~ 210880,
      NomeEnte=="POÇÃO DE PEDRAS"  ~ 210890,
      NomeEnte=="PORTO FRANCO"  ~ 210900,
      NomeEnte=="PORTO RICO DO MARANHÃO"  ~ 210905,
      NomeEnte=="PRESIDENTE DUTRA"  ~ 210910,
      NomeEnte=="PRESIDENTE JUSCELINO"  ~ 210920,
      NomeEnte=="PRESIDENTE MÉDICI"  ~ 210923,
      NomeEnte=="PRESIDENTE SARNEY"  ~ 210927,
      NomeEnte=="PRESIDENTE VARGAS"  ~ 210930,
      NomeEnte=="PRIMEIRA CRUZ"  ~ 210940,
      NomeEnte=="RAPOSA"  ~ 210945,
      NomeEnte=="RIACHÃO"  ~ 210950,
      NomeEnte=="RIBAMAR FIQUENE"  ~ 210955,
      NomeEnte=="ROSÁRIO"  ~ 210960,
      NomeEnte=="SAMBAÍBA"  ~ 210970,
      NomeEnte=="SANTA FILOMENA DO MARANHÃO"  ~ 210975,
      NomeEnte=="SANTA HELENA"  ~ 210980,
      NomeEnte=="SANTA INÊS"  ~ 210990,
      NomeEnte=="SANTA LUZIA"  ~ 211000,
      NomeEnte=="SANTA LUZIA DO PARUÁ"  ~ 211003,
      NomeEnte=="SANTA QUITÉRIA DO MARANHÃO"  ~ 211010,
      NomeEnte=="SANTA RITA"  ~ 211020,
      NomeEnte=="SANTANA DO MARANHÃO"  ~ 211023,
      NomeEnte=="SANTO AMARO DO MARANHÃO"  ~ 211027,
      NomeEnte=="SANTO ANTÔNIO DOS LOPES"  ~ 211030,
      NomeEnte=="SÃO BENEDITO DO RIO PRETO"  ~ 211040,
      NomeEnte=="SÃO BENTO"  ~ 211050,
      NomeEnte=="SÃO BERNARDO"  ~ 211060,
      NomeEnte=="SÃO DOMINGOS DO AZEITÃO"  ~ 211065,
      NomeEnte=="SÃO DOMINGOS DO MARANHÃO"  ~ 211070,
      NomeEnte=="SÃO FÉLIX DE BALSAS"  ~ 211080,
      NomeEnte=="SÃO FRANCISCO DO BREJÃO"  ~ 211085,
      NomeEnte=="SÃO FRANCISCO DO MARANHÃO"  ~ 211090,
      NomeEnte=="SÃO JOÃO BATISTA"  ~ 211100,
      NomeEnte=="SÃO JOÃO DO CARÚ"  ~ 211102,
      NomeEnte=="SÃO JOÃO DO PARAÍSO"  ~ 211105,
      NomeEnte=="SÃO JOÃO DO SOTER"  ~ 211107,
      NomeEnte=="SÃO JOÃO DOS PATOS"  ~ 211110,
      NomeEnte=="SÃO JOSÉ DE RIBAMAR"  ~ 211120,
      NomeEnte=="SÃO JOSÉ DOS BASÍLIOS"  ~ 211125,
      NomeEnte=="SÃO LUÍS"  ~ 211130,
      NomeEnte=="SÃO LUÍS GONZAGA DO MARANHÃO"  ~ 211140,
      NomeEnte=="SÃO MATEUS DO MARANHÃO"  ~ 211150,
      NomeEnte=="SÃO PEDRO DA ÁGUA BRANCA"  ~ 211153,
      NomeEnte=="SÃO PEDRO DOS CRENTES"  ~ 211157,
      NomeEnte=="SÃO RAIMUNDO DAS MANGABEIRAS"  ~ 211160,
      NomeEnte=="SÃO RAIMUNDO DO DOCA BEZERRA"  ~ 211163,
      NomeEnte=="SÃO ROBERTO"  ~ 211167,
      NomeEnte=="SÃO VICENTE FERRER"  ~ 211170,
      NomeEnte=="SATUBINHA"  ~ 211172,
      NomeEnte=="SENADOR ALEXANDRE COSTA"  ~ 211174,
      NomeEnte=="SENADOR LA ROCQUE"  ~ 211176,
      NomeEnte=="SERRANO DO MARANHÃO"  ~ 211178,
      NomeEnte=="SÍTIO NOVO"  ~ 211180,
      NomeEnte=="SUCUPIRA DO NORTE"  ~ 211190,
      NomeEnte=="SUCUPIRA DO RIACHÃO"  ~ 211195,
      NomeEnte=="TASSO FRAGOSO"  ~ 211200,
      NomeEnte=="TIMBIRAS"  ~ 211210,
      NomeEnte=="TIMON"  ~ 211220,
      NomeEnte=="TRIZIDELA DO VALE"  ~ 211223,
      NomeEnte=="TUFILÂNDIA"  ~ 211227,
      NomeEnte=="TUNTUM"  ~ 211230,
      NomeEnte=="TURIAÇU"  ~ 211240,
      NomeEnte=="TURILÂNDIA"  ~ 211245,
      NomeEnte=="TUTÓIA"  ~ 211250,
      NomeEnte=="URBANO SANTOS"  ~ 211260,
      NomeEnte=="VARGEM GRANDE"  ~ 211270,
      NomeEnte=="VIANA"  ~ 211280,
      NomeEnte=="VILA NOVA DOS MARTÍRIOS"  ~ 211285,
      NomeEnte=="VITÓRIA DO MEARIM"  ~ 211290,
      NomeEnte=="VITORINO FREIRE"  ~ 211300,
      NomeEnte=="ZÉ DOCA"  ~ 211400,

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



########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA SAÚDE ##################

# VERIFICAR O VALOR TOTAL RECOLHIDO POR ANO SEGUNDO O MUNICIPÍO
CFEM_MA_ANO <- CFEM_MA %>% select(Ano, NomeEnte, Cod.IBGE, Valor)%>%
  group_by(Ano,NomeEnte,Cod.IBGE) %>% summarise(TotalDistribuido=sum(Valor))

# ALTERANDO O TIPO DA VARIÁVEL PARA CARACTERE
CFEM_MA_ANO <- CFEM_MA_ANO %>% mutate(Cod.IBGE=as.character(Cod.IBGE))

# TotalRecolhido = as.character(format(TotalDistribuido,scientific = FALSE)
# CFEM_MA_ANO   <- CFEM_MA_ANO %>% mutate(TotalDistribuido=as.integer(TotalDistribuido))


#######################################################################################
# UNINDO OS DOIS BANCOS DE DADOS SEGUNDO O MUNCÍPIO E O ANO
join<-inner_join(SAUDE_MA_TOTAL,CFEM_MA_ANO ,by=c("Cod.IBGE", "Ano"))

#######################################################################################
#######################################################################################
# SELECIONANDO APENAS OS DADOS DE INTERESSE BASE JOIN
DADOS  <- join %>% select(Cod.IBGE,Ano, NomeEnte.y,TotalDistribuido, Investimento )
DADOS  <- mutate(DADOS , Ano=as.character(Ano))

# UTILIZANDO APENAS 65% DISTRIBUÍDO PARA O MUNICÍPIO
DADOS  <- mutate(DADOS, TotalDistribuidoM = 0.65*TotalDistribuido)

#######################################################################################
#######################################################################################
# ANÁLISE EXPLORATÓRIA DOS DADOS
par(mfrow=c(2, 1), mar=c(5, 4, 2, 1))

## VALORES DISTRIBUÍDOS DOS ROYALTS POR MUNICÍPIO
pdf("boxplot_Ano_MA.pdf") 
  ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = Ano, y = TotalDistribuidoM))+
  labs(x= "Ano", y="Total Distribuído")
dev.off( ) 

pdf("boxplot_Municipios_MA1.pdf")
ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x =NomeEnte.y, y = TotalDistribuidoM))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7))+
  labs(x= "Municípios do Maranhão", y="Total Distribuído") 
dev.off()  

pdf("boxplot_Municipios_MA1.pdf")
  ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = reorder(NomeEnte.y, TotalDistribuidoM, FUN=median), y = TotalDistribuidoM))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7), axis.text.y = element_text(size=5))+
  labs(x= "Municípios do Maranhão", y="Total Distribuído") + coord_flip()
dev.off()  

## VALORES INVESTIDOS EM SAUDE POR MUNICÍPIO

pdf("boxplot_Ano_MA_saude.pdf") 
ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = Ano, y = Investimento))+
  labs(x= "Ano", y="Total Investido na Saúde")
dev.off( ) 


pdf("boxplot_Municipios_MA1_saude.pdf")
ggplot(data = DADOS) + 
  geom_boxplot(mapping = aes(x = reorder(NomeEnte.y, Investimento, FUN=median), y = Investimento))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7), axis.text.y = element_text(size=5))+
  labs(x= "Municípios do Maranhão", y="Total Investido na Saúde") + coord_flip()
dev.off()  


## ASSOCIAÇÃO ENTRE AS VARIÁVEIS
  # Retirando os NAS e considerando os outliers
  DATA <- DADOS[!is.na(DADOS$Investimento),]
  cor(DATA$Investimento, DATA$TotalDistribuidoM)
  
  # Retirando os outliers
  outliers = c(boxplot.stats(DADOS$TotalDistribuidoM)$out,
               boxplot.stats(DADOS$Investimento)$out)
  
  data_outliers = DADOS[-c(which(DADOS$TotalDistribuidoM %in% outliers),
                           which(DADOS$Investimento %in% outliers)),]
  
  # Retirando os NAS 
  data1<-data_outliers[!is.na(data_outliers$Investimento),]
  cor(data1$TotalDistribuidoM , data1$Investimento)
  

  
  
#######################################################################################
#######################################################################################    

select.outliers = DADOS[DADOS$TotalDistribuidoM> min(boxplot.stats(DADOS$TotalDistribuidoM)$out) |
                        DADOS$Investimento > min(boxplot.stats(DADOS$Investimento)$out),]



pdf("ASSOCICAO_OUTLIERS.pdf") 
  ggplot(DADOS, aes(x=TotalDistribuidoM, y=Investimento))+
  geom_point(stat='identity')+
  geom_encircle(aes(x=TotalDistribuidoM, y=Investimento),
                data=select.outliers,
                color="red",
                size=2,
                expand=0.0008)+
  annotate('text', x=100, y=600,
           label='',
           colour='darkblue', size=4.5)+
  labs(x= "Valor Distribuído para os Municípios", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

pdf("ASSOCICAO.pdf") 
ggplot(data_outliers, aes(x=TotalDistribuidoM, y=Investimento))+
  geom_point(stat='identity') +
  labs(x= "Valor Distribuído para os Municípios", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

# Considerando o log a fim de melhorar o ajuste dos dados.

pdf("ASSOCICAO_log.pdf") 
ggplot(data_outliers, aes(x= log(TotalDistribuidoM), y= log(Investimento)))+
geom_point(stat='identity') +
labs(x= "LOG do Valor Distribuído para os Municípios", y='LOG do Investimento em Saúde nos Municípios',
     title='',
     caption='')
dev.off()


####################################################################################
################## selecionando apenas os município afetados #######################
####################################################################################


# SELECIONAR APENAS OS MUNICÍPIOS AFETADOS

CFEM_MA_AFETADO <- filter(CFEM_MA, TipoDistribuição == "Afetado" )

# VERIFICAR O VALOR TOTAL RECOLHIDO POR ANO SEGUNDO O MUNICIPÍO
CFEM_MA_ANO_AFETADO <- CFEM_MA_AFETADO %>% select(Ano, NomeEnte, Cod.IBGE, Valor)%>%
  group_by(Ano,NomeEnte,Cod.IBGE) %>% summarise(TotalDistribuido=sum(Valor))

# ALTERANDO O TIPO DA VARIÁVEL PARA CARACTERE
CFEM_MA_ANO_AFETADO <- CFEM_MA_ANO_AFETADO %>% mutate(Cod.IBGE=as.character(Cod.IBGE))


#######################################################################################
# UNINDO OS DOIS BANCOS DE DADOS SEGUNDO O MUNCÍPIO E O ANO
join_AFETADO<-inner_join(SAUDE_MA_TOTAL,CFEM_MA_ANO_AFETADO ,by=c("Cod.IBGE", "Ano"))

#######################################################################################
#######################################################################################
# SELECIONANDO APENAS OS DADOS DE INTERESSE BASE JOIN
DADOS_AFETADO  <- join_AFETADO %>% select(Cod.IBGE,Ano, NomeEnte.y,TotalDistribuido, Investimento )
DADOS_AFETADO  <- mutate(DADOS_AFETADO , Ano=as.character(Ano))

# UTILIZANDO APENAS 65% DISTRIBUÍDO PARA O MUNICÍPIO
DADOS_AFETADO  <- mutate(DADOS_AFETADO, TotalDistribuidoM = 0.65*TotalDistribuido)



# REALIZANDO AS MESMAS ANÁLISES ANTERIORES PORÉM PARA OS MUNICÍPIOS AFETADOS
#######################################################################################
#######################################################################################

## VALORES DISTRIBUÍDOS DOS ROYALTS POR MUNICÍPIO
pdf("boxplot_Ano_MA_AFETADO.pdf") 
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = Ano, y = TotalDistribuidoM))+
  labs(x= "Ano", y="Total Distribuído")
dev.off( ) 

pdf("boxplot_Municipios_MA1_AFETADO.pdf")
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x =NomeEnte.y, y = TotalDistribuidoM))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7))+
  labs(x= "Municípios do Maranhão", y="Total Distribuído") 
dev.off()  

pdf("boxplot_Municipios_MA1_AFETADO1.pdf")
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = reorder(NomeEnte.y, TotalDistribuidoM, FUN=median), y = TotalDistribuidoM))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7), axis.text.y = element_text(size=5))+
  labs(x= "Municípios do Maranhão", y="Total Distribuído") + coord_flip()
dev.off()  

## VALORES INVESTIDOS EM SAUDE POR MUNICÍPIO

pdf("boxplot_Ano_MA_saude_AFETADO.pdf") 
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = Ano, y = Investimento))+
  labs(x= "Ano", y="Total Investido na Saúde")
dev.off( ) 


pdf("boxplot_Municipios_MA1_saude_AFETADO.pdf")
ggplot(data = DADOS_AFETADO) + 
  geom_boxplot(mapping = aes(x = reorder(NomeEnte.y, Investimento, FUN=median), y = Investimento))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7), axis.text.y = element_text(size=5))+
  labs(x= "Municípios do Maranhão", y="Total Investido na Saúde") + coord_flip()
dev.off()  


## ASSOCIAÇÃO ENTRE AS VARIÁVEIS
# Retirando os NAS e considerando os outliers
DATA_AFETADO <- DADOS_AFETADO[!is.na(DADOS_AFETADO$Investimento),]
cor(DATA_AFETADO$Investimento, DATA_AFETADO$TotalDistribuidoM)

# Retirando os outliers
outliers_A = c(boxplot.stats(DADOS_AFETADO$TotalDistribuidoM)$out,
             boxplot.stats(DADOS_AFETADO$Investimento)$out)

data_outliers_AFETADO = DADOS_AFETADO[-c(which(DADOS_AFETADO$TotalDistribuidoM %in% outliers_A),
                                         which(DADOS_AFETADO$Investimento %in% outliers_A)),]

# Retirando os NAS 
data1_AFETADO<-data_outliers_AFETADO[!is.na(data_outliers_AFETADO$Investimento),]
cor(data1_AFETADO$TotalDistribuidoM , data1_AFETADO$Investimento)




#######################################################################################
#######################################################################################    

select.outliers_A = DADOS_AFETADO[DADOS_AFETADO$TotalDistribuidoM> min(boxplot.stats(DADOS_AFETADO$TotalDistribuidoM)$out) |
                          DADOS_AFETADO$Investimento > min(boxplot.stats(DADOS_AFETADO$Investimento)$out),]



pdf("ASSOCICAO_OUTLIERS_AFETADOS.pdf") 
ggplot(DADOS_AFETADO, aes(x=TotalDistribuidoM, y=Investimento))+
  geom_point(stat='identity')+
  geom_encircle(aes(x=TotalDistribuidoM, y=Investimento),
                data=select.outliers_A,
                color="red",
                size=2,
                expand=0.0008)+
  annotate('text', x=100, y=600,
           label='',
           colour='darkblue', size=4.5)+
  labs(x= "Valor Distribuído para os Municípios Afetados", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

pdf("ASSOCICAO_AFETADOS.pdf") 
ggplot(data_outliers_AFETADO, aes(x=TotalDistribuidoM, y=Investimento))+
  geom_point(stat='identity') +
  labs(x= "Valor Distribuído para os Municípios", y='Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()

# Considerando o log a fim de melhorar o ajuste dos dados.

pdf("ASSOCICAO_log_AFETADOS.pdf") 
ggplot(data_outliers_AFETADO, aes(x= log(TotalDistribuidoM), y= log(Investimento)))+
  geom_point(stat='identity') +
  labs(x= "LOG do Valor Distribuído para os Municípios", y='LOG do Investimento em Saúde nos Municípios',
       title='',
       caption='')
dev.off()




####################################################################################
################## Alterar o nome das colunas ######################################
# Outra forma de renomear nomes das colunas
CFEM_D1 <- CFEM_D %>% 
  rename_with(.data = ., .cols = 5:6, 
              .fn = str_replace, pattern = ".*", 
              replacement = str_c(c("Estado", "Município" )))
####################################################################################
####################################################################################

## ANÁLISE UTILIZANDO DADOS DA EDUCAÇÃO PARA OS MUNICÍPIOS AFETADOS

EDUCACAO  <- read_excel("Base Final Educação - Investimentos - SIOPE - Base Bruta.xlsx")


########################################################################################
############### ARRUMANDO O BANCO DE DADOS DOS INVESTIMENTOS NA EDUCAÇÃO##################

# SELEÇÃO APENAS DOS MUNICÍPIOS DO MARANHÃO
EDUCACAO_MA   <- EDUCACAO   %>% filter(UF=="MA")

# CRIANDO UMA COLUNA COM OS ANOS
EDUCACAO_MA_2019  <- EDUCACAO_MA  %>% select(Cod.IBGE, Município, "2019" )%>% mutate(Ano=2019)

# RENOMEANDO COLUNAS PARA JUNTAR OS DADOS
EDUCACAO_MA_2019  <- rename(EDUCACAO_MA_2019 , Investimento="2019", NomeEnte="Município")

# FILTRANDO APENAS O ANO DE 2019
CFEM_MA_ANO_AFETADO_2019 <- filter(CFEM_MA_ANO_AFETADO,Ano==2019)


#######################################################################################
# UNINDO OS DOIS BANCOS DE DADOS SEGUNDO O MUNCÍPIO E O ANO
join_EDUCACAO_AFETADO<-inner_join(EDUCACAO_MA_2019,CFEM_MA_ANO_AFETADO_2019 ,by=c("Cod.IBGE"))

DATA_EDUCACAO <- join_EDUCACAO_AFETADO[!is.na(join_EDUCACAO_AFETADO$Investimento),]

cor(DATA_EDUCACAO$Investimento, DATA_EDUCACAO$TotalDistribuido)

# NÃO APRESENTOU UM COMPORTAMENTO INTERESSANTE
pdf("boxplot_Municipios_MA1_EDUCACAO_AFETADO.pdf")
  ggplot(data = DATA_EDUCACAO ) + 
  geom_boxplot(mapping = aes(x = reorder(NomeEnte.y, Investimento, FUN=median), y = Investimento))+
  theme(axis.text.x = element_text(angle=90,vjust=2,size=7), axis.text.y = element_text(size=5))+
  labs(x= "Municípios do Maranhão", y="Total Investido na Saúde") + coord_flip()
dev.off()  









