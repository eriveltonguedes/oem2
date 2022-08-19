# Bibliotecas utilizadas 
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(maps)

########################################################################
# Utilizar essa opção caso a notação cientifíca seja um problema
options(scipen = 999)
########################################################################

list_geobr() #IPEA ----

# Carregados os municípios do estado do Pará no ano de 2016.
mun_PA <- read_municipality(code_muni="PA", year=2016)

# As 6 primeiras linhas do data.frame()
head(mun_PA)

##############################################################################
# Primeiro gráfico: Verificação simples dos municípios do Estado do Pará
##############################################################################

# remover o plot axis
no_axis <- theme(axis.title = element_blank(),
                 axis.text  = element_blank(),
                 axis.ticks = element_blank())
# nao fiz nada relevante

# criação do gráfico
ggplot() +
  geom_sf(data = mun_PA, fill="#2D3E50", color="#FEBF57",
          size=.15,show.legend = FALSE) + labs(subtitle = "Municípios do Pará - 2016.", size=8)+
  theme_minimal() + no_axis


####################################################################################
# A seguir serão utilizados os dados públicos da distribuição da CFEM.
####################################################################################

# Cria o data.frame( ) cfem_d
cfem_d<-read.csv2("CFEM_Distribuicao.csv")

# Exibe as primeiras 6 linhas 
head(cfem_d)

# Elimina as linhas com valores ausentes na coluna valor.
cfem_d <- cfem_d[!is.na(cfem_d$Valor), ]

#########################################
# Compatibilização das linhas para unir os data.frames()
#########################################

# Coloca todas as letras dos nomes dos municipíos em minúscula.
mun_PA$NomeEnte      <- tolower(mun_PA$name_muni)
head(mun_PA)

# Coloca todas as letras dos nomes dos municipíos em minúscula.
cfem_d$NomeEnte     <- tolower(cfem_d$NomeEnte)
head(cfem_d)

# Seleciona apenas os municípios do Estado do Pará
cfem_pa <-subset(cfem_d, SiglaEstado=="PA")

# Calcula o valor médio de CFEM distribuída para cada município por ano
cfem_pa1 <- cfem_pa %>% select(Ano,NomeEnte,Valor)%>% group_by(Ano,NomeEnte) %>% 
  summarise(media = mean(Valor))

# Seleciona apenas o ano de 2020 para o mapa.
cfem_pa1 <- subset(cfem_pa1, Ano==2020)

##############################################################################
# Segundo gráfico: Valor médio distribuído da CFEM no Estado Pará.
##############################################################################

# Sugestão 1
mun_PA %>% 
  left_join(cfem_pa1, by ="NomeEnte") %>% 
  ggplot(aes(fill = media), color = "black") +
  geom_sf() +
  labs(subtitle = "Valor médio da distribuição da CFEM - Pará, 2020", size=8)



# Sugestão 2
mun_PA %>% 
  left_join(cfem_pa1, by ="NomeEnte") %>% 
  ggplot( aes(fill= media),color=NA,size=.15) +
  geom_sf() +
  labs(subtitle = "Estados", size=8) +
  scale_fill_distiller(palette = "Blues", name="media")+
  labs(subtitle = "Valor médio da distribuição da CFEM - Pará, 2020", size=8)+
  theme_minimal() + no_axis



##############################################################################
# Terceiro gráfico: Verifica municípios limitrofes que não arrecadam CFEM
##############################################################################

# Cria um data.frame( ) com os principais municípios
data<- mun_PA %>% filter(code_muni %in% c("1502152", "1505536", "1504208",
                                          "1508407","1500347","1502772",
                                          "1507755", "1501253"))

head(data)

mun_PA %>% 
  left_join(cfem_pa1, by ="NomeEnte") %>% 
  ggplot() +
  geom_sf()+
  geom_sf(data = data, mapping = aes(fill=NomeEnte)) +
  labs(subtitle = "Municípios do Pará", size=8, name="Municípios") +
  theme_minimal() + no_axis


##############################################################################
# Quarto gráfico: Realizar gráficos a cada ano.
##############################################################################






