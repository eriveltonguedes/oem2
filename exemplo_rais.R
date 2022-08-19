##COLOCAR PARTE INICIAL CONETANDO NA RAIS
library("RPostgreSQL")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "rais_2019", host = "psql10-rj", port = 5432,
                user = "i3geoconsulta", password = "i3geoconsulta")

qryPatentesKo = paste0("
  SELECT codigo, titulo
  FROM cbo
")

qryPatentesKo = paste0('
SELECT ano, cod_familia, descricao_familia, total_de_vinculos
FROM cbo.tb_cbo_familia_v3
')

t0=Sys.time()
patentesko=dbGetQuery(con,qryPatentesKo)
t1=Sys.time()
duracao  = t1-t0
duracao

head(patentesko)
dim(patentesko)

###exercico cpf
library("RPostgreSQL")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "rais_2019", host = "psql10-rj", port = 5432,
                user = "i3geoconsulta", password = "i3geoconsulta")

# sort(dbListTables(con))

lim1000=' limit 1000'

qryPatentesKo = paste0(
  "SELECT cpf, nome_trab 
FROM vinculos.tb_vinculos where cpf='09532828729'",lim1000
)

t0=Sys.time()
patentesko=dbGetQuery(con,qryPatentesKo)
t1=Sys.time()
duracao  = t1-t0
duracao

head(patentesko)
dim(patentesko)
