
# Escolha do diretório
setwd("C:/Users/luizg/OneDrive/Documentos/IPEA/DADOS")

# Lendo os arquivos específicos
SAUDE  <- read_excel("Base Final Saúde - Investimentos - SIOPS - Base Bruta.xlsx")

# COLOCANDO OS CÓDIGOS DOS MUNICÍPIOS NOS DADOS
SAUDE_ESTADOS = SAUDE %>% 
  mutate(
    ESTADO = case_when(
      UF=="AC" ~ "Acre",
      UF=="AL" ~ "Alagoas",
      UF=="AP" ~ "Amapa",
      UF=="AM" ~ "Amazonas",
      UF=="BA" ~ "Bahia",
      UF=="CE" ~ "Ceara",
      UF=="DF" ~ "Distrito Federal",
      UF=="ES" ~ "Espirito Santo",
      UF=="GO" ~ "Goias",
      UF=="MA" ~ "Maranhao",
      UF=="MT" ~ "Mato Grosso",
      UF=="MS" ~ "Mato Grosso do Sul",
      UF=="MG" ~ "Minas Gerais",
      UF=="PA" ~ "Para",
      UF=="PB" ~ "Paraiba",
      UF=="PR" ~ "Parana",
      UF=="PE" ~ "Pernambuco",
      UF=="PI" ~ "Piaui",
      UF=="RJ" ~ "Rio de Janeiro",
      UF=="RN" ~ "Rio Grande do Norte",
      UF=="RS" ~ "Rio Grande do Sul",
      UF=="RO" ~ "Rondonia",
      UF=="RR" ~ "Roraima",
      UF=="SC" ~ "Santa Catarina",
      UF=="SP" ~ "Sao Paulo",
      UF=="SE" ~ "Sergipe",
      UF=="TO" ~ "Tocantins",
      
    ))  

write.csv2(SAUDE_ESTADOS, "SAUDE_ESTADOS.CSV")
getwd()
