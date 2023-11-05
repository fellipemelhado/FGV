#####################

#Variacao do emprego, juros real e variacao ipca
df$jurosr = (1+df$juros/100)/(1+df$ipca/100)

# https://www.sfu.ca/~dsignori/buec333/lecture%2017.pdf
#correlacao serial

###########################################

raw_juros = GetBCBData::gbcbd_get_series(
  id = 1178,
  first.date = "1986-06-04"
)
raw_juros$ref.date = as.Date(raw_juros$ref.date)
raw_juros = raw_juros[-c(3,4)]
meta_juros = GetBCBData::gbcbd_get_series(
  id = 432,
  first.date = '1999-03-05')
meta_juros$ref.date = as.Date(meta_juros$ref.date)
meta_juros = meta_juros[-c(3,4)]
juros = inner_join(meta_juros, raw_juros, by = 'ref.date')

#####################################3

ts8 = ts(df$ddesemprego, start = c(2012, 4),end = c(2022, 9), frequency = 12)
xxx = decompose(ts8, type = 'additive') # acho q esse nao precisa
plot(xxx)

zzz = xxx$x - xxx$trend - xxx$seasonal
plot(zzz)
zzz = na.omit(zzz)
adf.test(zzz)

##################33

ts9 = ts(df_desocupado$Valor, start = c(2012, 4),end = c(2022, 9), frequency = 12)
ts10 = xts(df_desocupado[,-1], order.by = as.Date(df_desocupado$date), frequency = 12)
aaa = a$call
a = hpfilter(ts10, freq = 12)
b= a$trend

plot(b)

#######################3

# Tirando a sazonalidade do serie de juros e desemprego
sjuros = seasadj(decompose(ts2))
plot(sjuros)
adf.test(na.omit(sjuros))
dsjuros = diff(sjuros)
plot(dsjuros)
adf.test(dsjuros)

sdesemprego = seasadj(decompose(ts7))
plot(sdesemprego)
adf.test(na.omit(sdesemprego))
dsdesemprego = diff(sdesemprego)
plot(dsdesemprego)
adf.test(dsdesemprego)

##############################3

df = mutate(df, ddjuros = juros - 2*lag(juros) - lag(lag(juros)))
df2 = df

teste = mutate(df, teste = diff(df$juros, differences = 2))
teste = diff(df$juros, differences = 2)
adf.test(teste)
plot(teste)
plot(df$djuros)

#############################################3

#Juros geral pos 2000
ggplot(df_juros, aes(ref.date, value)) + geom_line()
ggplot(df_juros, aes(ref.date, diff)) + geom_line()
ts4 =  xts(df_juros[,-1], order.by = as.Date(df_juros$ref.date), frequency = 12)
dygraph(ts4$value, main = "Juros mensal 2000-2022", xlab = 'data', 
        ylab = "taxa de juros")
dygraph(ts4$diff, main = "1 diferenca juros mensal 2000-2022", xlab = 'data', 
        ylab = "taxa de juros")
ts5 = ts(df_juros$value, start = c(2000, 1),end = c(2022, 10), frequency = 12)
plot(decompose(ts5, type = 'additive'))
ts6 = ts(df_juros$diff, start = c(2000, 1),end = c(2022, 10), frequency = 12)
plot1 = decompose(ts6)# n sei se precisa
c = as.data.frame(plot1$random)
plot(c)

################################################
#Script do belleza

#Abrindo os pacotes
library(readxl)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(dynlm)
library(gridExtra)
library(bayesforecast)
library(vars)
library(VARsignR)
library(xts)


#Abrindo a base
base.df <- read_excel("C:/Users/lucas/Downloads/data_ea2.xlsx")
base.df <- base.df %>% mutate(dates = as.yearqtr(dates))

#Fazendo as modificações necessárias
base.df$rgdp <- log(base.df$rgdp)
base.df$pgdp <- log(base.df$pgdp)
base.df$pcomm <- log(base.df$pcomm)
base.df$nbres <- log(base.df$nbres)
base.df$totres <- log(base.df$totres)
base.df$rcons <- log(base.df$rcons)
base.df$rinv <- log(base.df$rinv)

#Plotando as séries
gráfico_rgdp <- ggplot(data=base.df) + geom_line(aes(dates, rgdp, group = 1)) +
  labs(
    x = "Ano",
    y = "PIB",
    title = "Produto",
    subtitle = "Real Gross Domestic Product")

gráfico_pgdp <- ggplot(data=base.df) + geom_line(aes(dates, pgdp, group = 1)) +
  labs(
    x = "Ano",
    y = "PIB Deflator",
    title = "PIB Deflator",
    subtitle = "GDP Implicit Price Deflator")

gráfico_pcomm <- ggplot(data=base.df) + geom_line(aes(dates, pcomm, group = 1)) +
  labs(
    x = "Ano",
    y = "Indíce de Preços",
    title = "Índice de Preços do Produtor Commodities",
    subtitle = "Producer Price Index for All Commodities")

gráfico_ff <- ggplot(data=base.df) + geom_line(aes(dates, ff, group = 1)) +
  labs(
    x = "Ano",
    y = "Fundos Efetivos FED",
    title = "Fundos Efetivos FED",
    subtitle = "Effective Federal Funds Rate")

gráfico_totres<- ggplot(data=base.df) + geom_line(aes(dates, totres, group = 1)) +
  labs(
    x = "Ano",
    y = "Reservas Totais",
    title = "Reservas Totais",
    subtitle = "Total Reserves of Depository Institutions")

gráfico_nbres<- ggplot(data=base.df) + geom_line(aes(dates, nbres, group = 1)) +
  labs(
    x = "Ano",
    y = "Reservas",
    title = "Reservas Não Emprestadas",
    subtitle = "Nonborrowed Reserves of Depository Institutions")

grid.arrange(gráfico_rgdp, gráfico_pgdp, gráfico_pcomm, gráfico_ff, gráfico_totres, gráfico_nbres)

#-------------------------------- PAPER --------------------------------------------------------

#Criando a base do paper
base_paper <- base.df %>% dplyr::select(rgdp, pgdp, pcomm, ff, nbres, totres)

#Fazendo VAR 
var_paper <- VAR(base_paper, p = 4, type = "none")
summary(var_paper)

#Adicionando as datas na base e restringindo o tempo para equivalente ao paper
base_paper$dates <- base.df$dates
series_paper <- as.ts(base_paper, start = c(1965, 1), end = c(2005, 4))

#Fazendo as restrições de sinal
restrição <- c(+4,-3,-2,-5)

#Fazendo o modelo
modelo_paper <- uhlig.reject(Y = series_paper[ , 1:6],
                             nlags = 4 ,
                             draws = 2000 ,
                             subdraws = 1000 ,
                             nkeep = 1000 ,
                             KMIN = 1 ,
                             KMAX = 6 ,
                             constrained = restrição,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_paper$IRFS, type = "median", labels = c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais"), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_paper$FEVDS, table = F, labels =  c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais"), bands = c (0.16, 0.84), grid = T, bw = F)

#-------------------------------- NOVA ESPECIFICAÇÃO PAPER -----------------------------------------

#Fazendo as restrições de sinal
restrição <- c(+4,-3,-2,-5, -1)

#Fazendo o modelo
modelo_paper <- uhlig.reject(Y = series_paper[ , 1:6],
                             nlags = 4 ,
                             draws = 2000 ,
                             subdraws = 1000 ,
                             nkeep = 1000 ,
                             KMIN = 1 ,
                             KMAX = 6 ,
                             constrained = restrição,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_paper$IRFS, type = "median", labels = c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais"), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_paper$FEVDS, table = F, labels =  c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais"), bands = c (0.16, 0.84), grid = T, bw = F)





#------------------------------- ACRESCENTANDO AS NOVAS VARIÁVEIS ----------------------------------------

#Utilizando a base completa
gráfico_rcons <- ggplot(data=base.df) + geom_line(aes(dates, rcons, group = 1)) +
  labs(
    x = "Ano",
    y = "Gasto",
    title = "Gasto em Consumo",
    subtitle = "Real Personal Consumption Expenditures")

gráfico_pcomm <- ggplot(data=base.df) + geom_line(aes(dates, pcomm, group = 1)) +
  labs(
    x = "Ano",
    y = "Crescimento Investimento",
    title = "Crescimento Investimento Doméstico",
    subtitle = "Real Gross Private Domestic Investment")


gráfico_spread <- ggplot(data=base.df) + geom_line(aes(dates, spread, group = 1)) +
  labs(
    x = "Ano",
    y = "Spread",
    title = "Spread",
    subtitle = "Aaa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity")

grid.arrange(gráfico_rcons, gráfico_pcomm, gráfico_spread)

#--------------------------- MODELO SEM RESTRIÇÃO EM CONSUMO E INVESTIMENTO ---------------------------

#Adicionando as datas na base e restringindo o tempo para equivalente ao paper
base <- base.df[ , 2:9]
series <- as.ts(base, start = c(1965, 1), end = c(2005, 4))

#Fazendo as restrições de sinal
restrição <- c(+4,-3,-2,-5)

#Fazendo o modelo
modelo_total <- uhlig.reject(Y = series,
                             nlags = 4 ,
                             draws = 2000 ,
                             subdraws = 1000 ,
                             nkeep = 1000 ,
                             KMIN = 1 ,
                             KMAX = 6 ,
                             constrained = restrição,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_total$IRFS, type = "median", labels = c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento"), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_total$FEVDS, table = F, labels =  c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento"), bands = c (0.16, 0.84), grid = T, bw = F)


#--------------------------- MODELO COM RESTRIÇÃO EM CONSUMO E INVESTIMENTO ---------------------------
#Fazendo as restrições de sinal
restrição <- c(+4,-3,-2,-5, -7, -8)

#Fazendo o modelo
modelo_total <- uhlig.reject(Y = series,
                             nlags = 4 ,
                             draws = 2000 ,
                             subdraws = 1000 ,
                             nkeep = 1000 ,
                             KMIN = 1 ,
                             KMAX = 6 ,
                             constrained = restrição,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_total$IRFS, type = "median", labels = c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento"), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_total$FEVDS, table = F, labels =  c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento"), bands = c (0.16, 0.84), grid = T, bw = F)

#--------------------------- MODELO COM RESTRIÇÃO EM CONSUMO E INVESTIMENTO E SPREAD ---------------------------
#Colocando o spread
series_spread <- as.ts(base.df[73:164 , 2:10], start = c(1983, 1), end = c(2005, 4))

#Fazendo a restrição
restrição <- c(+4,-3,-2,-5)

#Fazendo o modelo
ts = as.ts(df, start = c(2012, 3), end = c(2022, 9))
modelo_spread <- uhlig.reject(Y = series_spread,
                              nlags = 4 ,
                              draws = 20000 ,
                              subdraws = 10000 ,
                              nkeep = 2000 ,
                              KMIN = 1 ,
                              KMAX = 6 ,
                              constrained = restrição,
                              constant = FALSE,
                              steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_spread$IRFS, type = "median", labels = c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento", "Spread"), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_spread$FEVDS, table = F, labels =  c("PIB", "PIB Deflator", "Commodities", "Fed Fund Rate", "Reservas NB", "Reservas Totais", "Consumo", "Investimento", "Spread"), bands = c (0.16, 0.84), grid = T, bw = F)

###################
install.packages("C:/Users/admin/Downloads/VARsignR_0.1.2.tar.gz", repos = NULL)
install.packages("C:/Users/admin/Downloads/HI_0.5.tar.gz", repos = NULL)
install.packages("minqa")
install.packages("HI")
install.packages("mvnfast")
install.packages('RCurl')
library(remotes)
install_version(package = 'HI', '0.5')

#####################

install.packages("webshot")
webshot::install_phantomjs()
install.packages("tinytex")
