library(tidyverse)
library(MASS)
library(rgr)
library(StatDA)
require(ggplot2)
library(stats)
library(dplyr)
library(data.table)
library(compositions)
library(kableExtra)
library(gridExtra)
library(grid)

options(OutDec= ",")
# Carregar dados
datarpvc <- read.table(paste0("/Results/", folha,"_analises_05ld.csv"), head=T, sep=";", dec=",")
datarpvc_b <- read.table(paste0("/Results/",folha,"_analises_prep_code.csv"), head=T, sep=";", dec=",")
filtro_bruto <- filter(datarpvc_b, COD == "SMP")
ref <- read.table("ref_ucc_ld.csv", head=T, sep=";", dec=",")
w_el <- c( "Bi", "Co", "Cr", "Cu", "Fe", "La", "Mn", "Mo", "Ni", "P", "Sb", "Sc", "Th", "Ti", "Tl", "V", "Y")
#w_el <- c("Ba", "Bi", "Ce", "Co", "Cr","Cs", "Cu", "Fe", "La", "Mn", "Mo", "Ni", "P", "Rb", "Sb", "Sc", "Th", "Ti", "Tl", "U", "V", "Y")
# seleciona elementos da planilha de dados
folha <- "Lavras do Sul"
select <- datarpvc[, w_el]
row.names(ref) <-  ref$ELEM

select_ref <- ref [w_el, ]
select_bruto <- filtro_bruto[, w_el]
#
# #Transpoe a tabela
# select_ref_t <- t(select_ref)
# Escolha do Elemento

UN <- select_ref[, 5 ]
EL <- w_el
LD <- select_ref[, 3]
UCC <- select_ref[, 3]
dig <- select_ref[, 4]

# Log-transformação dos dados

logselect <- log10(select)

#Declara referencias


# (i) quantas colunas no data.frame
n <- ncol(select)

# (ii) vetor para armazenar resultados
MD <- numeric(n)
MDN <- numeric(n)
DP <- numeric(n)
CV <- numeric(n)
MIN <- numeric(n)
Q1 <- numeric(n)
Q3 <- numeric(n)
LS1 <- numeric(n)
LS2 <- numeric(n)
LS3 <- numeric(n)
LI1 <- numeric(n)
LI2 <- numeric(n)
LI3 <- numeric(n)
MAX <- numeric(n)
UCC <- numeric(n)
LD <- numeric(n)
PLD <- numeric(n)
NO <- numeric(n)
MAD <- numeric(n)
# (iii) nomeando vetor com nomes das colunas
names(MD) <- colnames(select)
names(MDN) <- colnames(select)
names(DP) <- colnames(select)
names(CV) <- colnames(select)
names(MIN) <- colnames(select)
names(Q1) <- colnames(select)
names(Q3) <- colnames(select)
names(LS1) <- colnames(select)
names(LS2) <- colnames(select)
names(LS3) <- colnames(select)
names(LI1) <- colnames(select)
names(LI2) <- colnames(select)
names(LI3) <- colnames(select)
names(MAX) <- colnames(select)
names(PLD) <- colnames(select)
names(NO) <- colnames(select)
names(MAD) <- colnames(select)

# (iv) loop para cada coluna
# Limpeza da tabela
## Tirar valores < MIN ou valores > MAX
for(i in seq_along(select)){
  LD[i] <- round(select_ref$LD[i], digits = dig[i])
  LD[i] <- ifelse(select_ref$LD[i] == 0, NA, select_ref$LD[i])
  MD[i] <- round(mean(select[,i]), dig[i])
  MDN[i] <- round(10^(median(logselect[,i])), dig[i])
  DP[i] <- round(sd(select[,i]), dig[i])
  CV[i] <- round(DP[i]/MD[i]*100, 0)
  MIN[i] <- round(min(select[,i],na.rm = TRUE), dig[i])
  MIN[i] <- ifelse(MIN[i] == 0, paste0("<", LD[i]), MIN[i])
  MIN[i] <- ifelse(MIN[i] == round(select_ref$LD[i]/2, dig[i]), paste0("<", select_ref$LD[i]), MIN[i])
  Q1[i] <- round(10^(quantile(logselect[,i], c(.25))), dig[i])
  Q3[i] <- round(10^(quantile(logselect[,i], c(.75))), dig[i])
  MAX[i] <- max(select[,i])
  MAX[i] <- ifelse(MAX[i] == 0, LD[i]*1.2, MAX[i])
  LS1[i] <- round(10^(quantile(logselect[,i], c(.75)) + 4.5 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LS1[i] <- ifelse(LS1[i] > MAX[i], NA, LS1[i])
  LS1[i] <- ifelse(LS1[i] == 0, NA, LS1[i])
  LS2[i] <- round(10^(quantile(logselect[,i], c(.75)) + 3 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LS2[i] <- ifelse(LS2[i] > MAX[i] , NA, LS2[i])
  LS2[i] <- ifelse(LS2[i] == 0 , NA, LS2[i])
  LS3[i] <- round(10^(quantile(logselect[,i], c(.75)) + 1.5 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LS3[i] <- ifelse(LS3[i] > MAX[i] , NA, LS3[i])
  LS3[i] <- ifelse(LS3[i] == 0 , NA, LS3[i])
  LI1[i] <- round(10^(quantile(logselect[,i], c(.25)) - 4.5 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LI1[i] <- ifelse(LI1[i] < MIN[i], NA, LI1[i])
  LI1[i] <- ifelse(LI1[i] == 0, NA, LI1[i])
  LI2[i] <- round(10^(quantile(logselect[,i], c(.25)) - 3 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LI2[i] <- ifelse(LI2[i] < MIN[i], NA, LI2[i])
  LI2[i] <- ifelse(LI2[i] == 0, NA, LI2[i])
  LI3[i] <- round(10^(quantile(logselect[,i], c(.25)) - 1.5 * (quantile(logselect[,i], c(.75)) - quantile(logselect[,i], c(.25)))), dig[i])
  LI3[i] <- ifelse(LI3[i] < MIN[i], NA, LI3[i])
  LI3[i] <- ifelse(LI3[i] == 0, NA, LI3[i])
  UCC[i] <- round(select_ref$UCC[i], digits = dig[i])
  UCC[i] <- ifelse(select_ref$UCC[i] == 0, NA, select_ref$UCC[i])
  MAD[i] <- round(10^mad(logselect[,i], na.rm = TRUE), dig[i])
  MAD[i] <- ifelse(MAD[i] == 0, NA, MAD[i])
}

for(i in seq_along(select)){
  LD[i] <-format(LD[i], digits = dig[i])
  MD[i] <- format(MD[i], digits = dig[i])
  MDN[i] <- format(MDN[i], digits = dig[i])
  DP[i] <- format(DP[i], digits = dig[i])
  CV[i] <- format(CV[i], digits = dig[i])
  MIN[i] <- format(MIN[i], digits = dig[i])
  Q1[i] <- format(Q1[i], digits = dig[i])
  Q3[i] <- format(Q3[i], digits = dig[i])
  MAX[i] <- format(MAX[i], digits = dig[i])
  LS1[i] <-  format(LS1[i], digits = dig[i])
  LS2[i] <- format(LS2[i], digits = dig[i])
  LS3[i] <- format(LS3[i], digits = dig[i])
  LI1[i] <- format(LI1[i], digits = dig[i])
  LI2[i] <- format(LI2[i], digits = dig[i])
  LI3[i] <- format(LI3[i], digits = dig[i])
  UCC[i] <- format(UCC[i], digits = dig[i])
  MAD[i] <- format(MAD[i], digits = dig[i])
}


# calculo dos valores qualificados e valores reais
xnew <- simulateMissings(select_bruto)
miss <- missingSummary(xnew)
NV <- miss[ ,1:1]
NQ <- miss[ ,2:2]
NO <- NV + NQ
PLD <- round(NQ/NO*100,0)
# NV[NV == 0] <- "-"
# NQ[NQ == 0] <- "-"
#
# PLD[PLD == 0] <- "-"
#Cria a tabela de dados


df <- data.frame(EL, UN, LD,
                 NV, NQ, PLD,
                 MIN, MD, LI1,
                 LI2, LI3, Q1,
                 MDN, Q3, LS3,
                 LS2, LS1, MAX,
                 CV, MAD, UCC) #adicionar MAD
# df[colSums(!is.na(df)) > 0]
df[df == "NA"] <- NA

n_col_df <- c("Elemento", "Unidade", "Limite de detecção (LD)",
              "Valores válidos", "Valores abaixo do LD", "Percentual de valores abaixo do LD",
              "Mínimo", "Média aritmética",
              "Limiar inferior de 1a. ordem ", "Limiar inferior de 2a. ordem", "Limiar inferior de 3a. ordem",
              "1o. Quartil", "Mediana", "3o. Quartil",
              "Limiar superior de 3a. ordem", "Limiar superior de 2a. ordem", "Limiar superior de 1a. ordem",
              "Máximo", "Coeficiente de variação (%)", "Desvio médio absoluto", "UCC")
colnames(df) <-  n_col_df
df <-df[, colSums(is.na(df)) != nrow(df)]
#df_select <- data.frame(LD, NO, NQ, PLD, CV, MIN, MD, MDN, MAX)
df[df == "NA"] <- "-"
df_t <- t(df)
#tabela elementar

library(knitr)
library(sparkline)
library(formattable)
library(ggplot2)

#Salvando sumários
write.table(df, file = "\Results\sum_stat_param_4.csv", na = "", row.names = FALSE, quote = FALSE, sep = ";", dec = ",")
write.table(df_t, file = "\Results\sum_stat_el_4.csv", na = "", col.names = FALSE, quote = FALSE, sep = ";", dec = ",")
#Salvando tabela
png(filename = paste0("\Figures\tab_sum_stat_", "_4_sum_res_final.png"), units = "cm", width=32,height=18,bg = "white", res = 300)

m <- format(df_t, digits = 1, scientific=F, big.mark = ",")

# mytheme <- ttheme_default(core = list(fg_params = list(hjust=0, x=0,
#                                                        fontsize=7, col = "black")),
#                           colhead = list(fg_params = list(fontsize=7,fontface="bold") )
# )
mytheme <- ttheme_default()
mytheme[["core"]][["bg_params"]][["fill"]] = c("white", "white")
mytheme[["core"]][["bg_params"]][["col"]] = "black"
mytheme[["colhead"]][["bg_params"]][["col"]] = "black"
df_t[is.na(df_t)] <- "-"
g1 <- tableGrob(df_t[-1,], theme = mytheme)

grid.newpage()

grid.draw(g1)

dev.off()
