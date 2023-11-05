graphics.off()
rm(list=ls())
load.lib <- c('readxl', 'tidyverse', 'reshape2', "pals", "paletteer", "ggthemes",
              'GetBCBData', 'sidrar', 'xts', 'bayesforecast', 'aTSA' , 'vars', 'tseries',
              'dygraphs', 'stats', 'mFilter', 'forecast', 'urca', 'bruceR',
              'imfr', 'janitor', 'lubridate', 'zoo', 'dynlm')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

pib = imf_data(database_id = 'IFS', indicator ="NGDP_R_SA_XDC", country = "GB", freq = "Q", start = 2010, end = 2019, return_raw = T)
pib = pib$CompactData$DataSet$Series$Obs %>% rename(year_quarter = `@TIME_PERIOD`, pib = `@OBS_VALUE`)
ir = read_excel("ir.xlsx") %>% rename(year_quarter = date)
cpi = imf_data(database_id = 'IFS', indicator ="PCPI_IX", country = "GB", freq = "Q", start = 2010, end = 2019)
cambio = imf_data(database_id = 'IFS', indicator ="ENDE_XDC_USD_RATE", country = "GB", freq = "Q", start = 2010, end = 2019)
bp = imf_data(database_id = 'IFS', indicator =c("GG_GR_G01_XDC", "GG_GE_G01_XDC"), country = "GB", freq = "Q", start = 2010, end = 2019)
bp$fd = bp$GG_GR_G01_XDC - bp$GG_GE_G01_XDC #deficit fiscal
df = as.data.frame(cbind(pib$year_quarter,pib$pib ,ir$juros, cpi$PCPI_IX, cambio$ENDE_XDC_USD_RATE, bp$fd))
colnames(df) = c("date", 'gdp', 'juros', 'cpi', 'cambio', 'deficit')
df$date = as.Date(as.yearqtr(df$date, format = "%Y-Q%q"))
df$date = as.yearqtr(df$date)
df2 = df[,-1] %>% mutate_all( function(x) as.numeric(as.character(x)))
hp = hpfilter(df2$gdp, freq = 4)
df2 = cbind(df2, hp$cycle, hp$trend)
colnames(df2) = c('gdp', 'juros', 'cpi', 'cambio', 'deficit', 'outgap', 'trend')
df2 = mutate(df2, cpiv = cpi/lag(cpi) - 1,cambior = (1+cambio)/(1+cpiv) - 1, jurosr = (1+juros)/(1+cpiv) - 1,
             dcambior = cambior - lag(cambior), dcambior = cambior - lag(cambior))
#############################
#Graficos
df = cbind(df$date, df2) %>% rename(date = `df$date`)
dfg = melt(df[,c(1,2,8)], id = 'date')
dfg2 = melt(df[,c(1,3,11)], id = 'date')
dfg3 = melt(df[,c(1,5,10)], id = 'date')
#pib
ggplot(dfg) + geom_line(aes(x = date, y = value, color = variable)) 
ggplot(df) + geom_line(aes(x = date, y = outgap), color = 'red')

#juros
ggplot(dfg2, aes(x = date)) + geom_line(aes(y = value, color = variable))
ggplot(df, aes(x = date)) + geom_line(aes(y = jurosr))

#Inflacao
ggplot(df, aes(x = date)) + geom_line(aes(y = cpi))
ggplot(df, aes(x = date)) + geom_line(aes(y = cpiv))


#Cambio
ggplot(dfg3, aes(x = date)) + geom_line(aes(y = value, color = variable))
ggplot(df, aes(date)) + geom_line(aes(y = dcambior))

#deficit
ggplot(df, aes(date)) + geom_line(aes(y = deficit))



##########################
#Modelos


################
dfm1 = df[,c(7,11,12,6)] %>% na.omit()
model1 = VAR(y = dfm1, p = 1, type = "none")
model1dlm = dynlm(formula = outgap ~ -1 + L(jurosr,1) + L(outgap,1) + L(deficit,1) + L(dcambior,1), data = dfm1, )
model1lm = lm(formula = outgap ~ -1 + lag(jurosr) + lag(outgap) + lag(deficit) + lag(dcambior), data = dfm1, )
model1$varresult$outgap
model1dlm
model1lm

granger_causality(model1)



df = mutate(df, dcambio = cambio - lag(cambio))
dfm2 = df[,c(9,7,13)] %>% na.omit()
model2 = VAR(y = dfm2, p = 1, type = "none")
model2
granger_causality(model2)

