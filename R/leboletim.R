library(dplyr)
require(rgr)
options(OutDec= ",")
#Ler Dados na planilha csvss
#as planilhas da GEOSOL vem com formato numérico com decimal . e separador ;
#substitui no editor-MS antes (I.S. e N.A. são mantidos)t

folha <- "Lavras do Sul"
list_bol <- paste0( "Data/Boletins/", list.files("Data/Boletins/"))

#list_bol <- c( "Data/Boletins/GQ2003731.CSV","Data/Boletins/GQ2003733.CSV","Data/Boletins/GQ2003734.CSV","Data/Boletins/GQ2003735.CSV", "Data/Boletins/GQ2003736.CSV")
datalist = list()
datalist2 = list()
datalist3 = list()
for (i in 1:5) {

df_tudo = read.csv(list_bol[i],  header = F,
              as.is = T, sep=";", fill = TRUE, encoding = "UTF-8")
n <- ncol(df_tudo)
r <- nrow(df_tudo)
status <- df_tudo[1,6]
laboratorio <- df_tudo[1,1]
cliente <- df_tudo[2,2]
data_criacao_arquivo <- df_tudo[3,2]
n_job <- df_tudo[4,2]
no_amostras <- df_tudo[5,2]
projeto <- df_tudo[6,2]
ship <- df_tudo[7,2]
numero_po <- df_tudo[8,2]
metodo <- t(df_tudo[11, 4:n])
analito <- t(df_tudo[12, 4:n])
unidades <- t(df_tudo[13, 4:n])
MDL <- t(df_tudo[14, 4:n])
boletim <- df_tudo[16:r, 1:n]
job_boletim <- rep(n_job, nrow(boletim))
boletim <- cbind(boletim, job_boletim)

#cria tabela das condições analíticas
condicoes_analiticas <- data.frame(metodo, analito, unidades, MDL, n_job)
var.name <- c( 'metodo', 'analito', 'unidades', 'MDL', 'boletim')
colnames(condicoes_analiticas) <- var.name
colnames(boletim) <- c("amostra", "cod_am", "classe_am", analito)
#cria tabela com informações do boletim
info_boletim <- data.frame(status, laboratorio, cliente, data_criacao_arquivo, n_job, no_amostras, projeto, ship)
colnames(info_boletim) <- c("status", "laboratorio", "cliente", "data do arquivo", "boletim" , "no. de amostras", "projeto", "entrega dos resultados")
datalist[[i]] <- info_boletim # add it to your list
datalist2[[i]] <- condicoes_analiticas # add it to your list
datalist3[[i]] <- boletim
}
ib = do.call(rbind, datalist)
ca = do.call(rbind, datalist2)
df = do.call(rbind, datalist3)
colnames(df)[ncol(df)] <- "boletim"
df_sc <- filter(df, classe_am == "SEDIMENTO CORRENTE")
colnames(df_sc) <- colnames(df)
df_bk <- filter(df, classe_am == "BRANCO_PREP")
colnames(df_bk) <- colnames(df)
df_rp <- filter(df, classe_am == "REP")
colnames(df_rp) <- colnames(df)

df_sd <- filter(df, classe_am == "STD")

df_sc_peneira <- df_sc[, 1:9]
df_sc_analitos <- df_sc[, -(2:9)]
QAQC_orig <- rbind(df_rp,df_bk, df_sd)
colnames(df_sd) <- colnames(df)

##Criar base para tratamento de dados
#transformação I.S. para NA
df_valida <- df_sc_analitos
df_valida[df_valida == "I.S."] <- -9999


df_sc_analitos[df_sc_analitos == "I.S."] <- ""

# transformação < para -
# Substitui dados qualificados
### Substitui srting < por - e elimina >
df_sc_transf <- data.frame(lapply(df_sc_analitos, function(x) {
  gsub("<", "-", x)
}))
df_valida <- data.frame(lapply(df_valida, function(x) {
  gsub("<", "-", x)
}))

QAQC_transf <- data.frame(lapply(QAQC_orig, function(x) {
  gsub("<", "-", x)
}))
df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub(">", "", x)
}))
QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
  gsub(">", "", x)
}))

#### Salva planilha com sinais -
write.table(df_sc_transf, paste0("Results/", folha,"_analises_prep.csv"), sep=";", dec="," , row.names = FALSE )
### Substitui valores <LD por 0.5LD e > LD pelo valor de LD
write.table(QAQC_transf, paste0("Results/",folha,"_qaqc_prep.csv"), sep=";", dec="," , row.names = FALSE )
## Salva plhanilha sinais e codigo
write.table(df_valida, paste0("Results/",folha,"_analises_prep_code.csv"), sep=";", dec="," , row.names = FALSE )


## Replace any missing data coded as -9999 with NAs and any remaining
## negative values representing less than detects with Abs(value)/2
df_sc_transf <- read.csv2("Results/Lavras do Sul_analises_prep.csv", sep=";", dec=",")
df_valida <- read.csv2("Results/Lavras do Sul_analises_prep_code.csv", sep=";", dec=",")
#cria tabela de dados transformados
df_sc_05ld <- ltdl.fix.df(df_sc_transf)
write.table(df_sc_05ld, paste0("Results/", folha,"_analises_05ld.csv"), sep=";", dec="," , row.names = FALSE )
df_zero <- df_sc_transf
df_zero[df_zero < 0] <- 0
df_zero[is.na(df_zero)] <- 0

