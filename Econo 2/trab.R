rm(list=ls())
load.lib <- c('readxl', 'tidyverse', 'reshape2', "pals", "paletteer", "ggthemes",
              'GetBCBData', 'sidrar', 'xts', 'bayesforecast', 'aTSA' , 'vars', 'tseries',
              'dygraphs', 'stats', 'mFilter', 'forecast', 'urca', 'bruceR',
              'VARsignR')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

#Baixando as bases
df_desocupado = get_sidra(api = '/t/6381/n1/all/v/4099/p/all/d/v4099%201')%>%
  mutate(date = parse_date(`Trimestre Móvel (Código)`, 
                           format='%Y%m')) %>%
  dplyr::select(date, Valor)

df_juros = GetBCBData::gbcbd_get_series(
  id = 4390,
  first.date = "2000-01-01"
) %>% dplyr::select(ref.date, value) %>% rename(date = ref.date)
  
df_ipca = get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202')%>%
  mutate(date = parse_date(`Mês (Código)`, format = "%Y%m")) %>%
  dplyr::select(date, Valor)

df = inner_join(df_juros, df_desocupado, by = 'date')
df = inner_join(df, df_ipca, by = 'date')
colnames(df) = c('date', 'juros', 'desemprego', 'ipca')
df$date = as.Date(df$date)
df$jurosr = (1+df$juros)/(1+df$ipca) - 1
df = mutate(df, desemprego = log(desemprego))
df = mutate(df, djurosr = jurosr - lag(jurosr), ddesemprego = desemprego - lag(desemprego),
            dipca = ipca - lag(ipca))


## Graficos
#Juros que batem com as datas de desemprego
ggplot(df, aes(date, juros)) + geom_line()
ggplot(df, aes(date, jurosr)) + geom_line()
ggplot(df, aes(date, djurosr)) + geom_line()
ts1 = xts(df[,-1], order.by = as.Date(df$date), frequency = 12)
dygraph(ts1$jurosr, main = "log Juros mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
dygraph(ts1$djurosr, main = "1 diferenca log juros mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
ts2 = ts(df$jurosr, start = c(2012, 3),end = c(2022, 9), frequency = 12)
plot(decompose(ts2, type = 'additive'))
ts3 = ts(df$djurosr, start = c(2012, 3),end = c(2022, 9), frequency = 12)
plot(decompose(ts3)) # acho q esse nao precisa


#Desemprego
ggplot(df, aes(date, desemprego)) + geom_line()
ggplot(df, aes(date, ddesemprego)) + geom_line()
dygraph(ts1$desemprego, main = "Desemprego mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
dygraph(ts1$ddesemprego, main = "1 diferenca desemprego mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
ts7 = ts(df$desemprego, start = c(2012, 4),end = c(2022, 9), frequency = 12)
plot(decompose(ts7, type = 'additive'))
ts8 = ts(df$ddesemprego, start = c(2012, 4),end = c(2022, 9), frequency = 12)
plot(decompose(ts8))

#IPCA
ggplot(df, aes(date, ipca)) + geom_line()
ggplot(df, aes(date, dipca)) + geom_line()
dygraph(ts1$ipca, main = "Inflacao mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
dygraph(ts1$dipca, main = "1 diferenca inflacao mensal 2012-2022", xlab = 'data', 
        ylab = "taxa de juros")
ts9 = ts(df$ipca, start = c(2012, 4),end = c(2022, 9), frequency = 12)
plot(decompose(ts9, type = 'additive'))
ts10 = ts(df$dipca, start = c(2012, 4),end = c(2022, 9), frequency = 12)
plot(decompose(ts10))

# Grafico geral
dygraph(ts1[,c(4,2,3)], main = "Series 2012-2022", xlab = 'data', 
        ylab = "Valores")
dygraph(ts1[,c(5,6,7)], main = "Series 2012-2022", xlab = 'data', 
        ylab = "Valores")

#Analise estatistica
#acf
ggacf(df$djurosr)
ggacf(df$ddesemprego)
ggacf(df$dipca)

#pacf
ggpacf(df$djurosr)
ggpacf(df$ddesemprego)
ggpacf(df$dipca)

#matriz de corelacao
corr.matrix = cor(na.omit(df[,c(6:8)]))
stargazer::stargazer(corr.matrix, type = "text", style = 'aer')


#Partindo para iniciar a ideia do modelo
#ADF
df = df %>% drop_na()
aTSA::adf.test(na.omit(df$djurosr)) # deu mt ruim
aTSA::adf.test(dfteste$ddesemprego)
aTSA::adf.test(dfteste$dipca)




#Teste de cointegracao
jotest = ca.jo(df[,c(3:5)], type = 'trace', K=4, ecdet = 'none', 
               spec = 'longrun')
summary(jotest)

#var select
dfteste = df
dfteste$desemprego = seasadj(decompose(ts7)) %>% as.vector()
dfteste$ipca = seasadj(decompose(ts9)) %>% as.vector()
dfteste = mutate(dfteste, ddesemprego = desemprego - lag(desemprego), dipca = ipca - lag(ipca))
dfteste = na.omit(dfteste)
vselect = VARselect(data.frame(df$djurosr, df$ddesemprego, df$dipca), 
                    lag.max = 5,type = c('none'))
vselect
#stg = stargazer::stargazer(vselect, type = 'html')

#writeClipboard(stg)




#Regressao Var
reg1 = vars::VAR(df[,c(6:8)], p = 4, type = 'none')
summary(reg1)

#teste de correlacao serial de dados nao precisa usar
serial.test(reg1)

#Granger-C
granger_causality(reg1)



#Fazendo o modelo 1 (choque negativo no juros)
restrição <- c(-2)

ts = as.ts(df[,c(7,6,8)], start = c(2012, 3), end = c(2022, 9))
modelo_paper <- uhlig.reject(Y = ts,
                             nlags = 4,
                             draws = 50000 ,
                             subdraws = 2000 ,
                             nkeep = 1000 ,
                             KMIN = 1 ,
                             KMAX = 4 ,
                             constrained = restrição,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_paper$IRFS, type = "median", labels = c('Desemprego', 'Juros real', 'IPCA'), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_paper$FEVDS, table = F, labels =  c('Desemprego', 'Juros real', 'IPCA'), bands = c (0.16, 0.84), grid = T, bw = F)


#Fazendo o modelo 2 esse nao precisa (choque negativo no IPCA)
restrição2 <- c(-3)
modelo_paper2 <- uhlig.reject(Y = ts,
                             nlags = 4,
                             draws = 50000 ,
                             subdraws = 20000 ,
                             nkeep = 10000 ,
                             KMIN = 1 ,
                             KMAX = 4 ,
                             constrained = restrição2,
                             constant = FALSE,
                             steps = 60)

#Plotando as funções resposta impulso
irfplot(irfdraws = modelo_paper2$IRFS, type = "median", labels = c('Desemprego', 'Juros real', 'IPCA'), bands = c(0.16, 0.84), grid = T, bw = F)

#Analisando a decomposição da variância
fevdplot(modelo_paper2$FEVDS, table = F, labels =  c('Desemprego', 'Juros real', 'IPCA'), bands = c (0.16, 0.84), grid = T, bw = F)


