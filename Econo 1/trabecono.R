rm(list=ls())
load.lib <- c("data.table","foreign","lmtest","stargazer","sandwich","car", 'dplyr', 'ggplot2', 'MASS', 'AER', 'RCurl', 'readr', 'usmap', 'scale', 'ggthemes')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


url1 = 'https://raw.githubusercontent.com/fellipemelhado/Trab-de-econo-1/main/base.csv'
df = read_csv(url(url1))

df = df %>% mutate(ln_pop = log(Pop), ln_pib = log(PIB_mi)) #N preciso ajustar o valor para 0
df = rename(df, state = Estado)
df$pol = ifelse(df$Sal_min == 7.25, 0, 1) #Se o estado tem politica de salario minimo = 1
summary(df)


#sera feito uma escadinha
form = as.formula('Desemprego_p ~ Sal_min')
form1 = as.formula('Desemprego_p ~ Sal_min + ln_pop')
form2 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib')
form3 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p')
form4 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p')
form5 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + maxbenefit')
form6 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + maxbenefit + terceirog_p')



OLS = lm(form, df)
OLS1 = lm(form1, df)
OLS2 = lm(form2, df)
OLS3 = lm(form3, df)
OLS4 = lm(form4, df)
OLS5 = lm(form5, df)
OLS6 = lm(form6, df)
stargazer(OLS, OLS1, OLS2, OLS3, OLS4, OLS5, OLS6, title = "Resultados", align = TRUE, omit.stat = c("LL","ser","f"), no.space = TRUE)

COV = vcovHC(OLS, type = 'HC0')
robust = sqrt(diag(COV))
COV1 = vcovHC(OLS1, type = 'HC0')
robust1 = sqrt(diag(COV1))
COV2 = vcovHC(OLS2, type = 'HC0')
robust2 = sqrt(diag(COV2))
COV3 = vcovHC(OLS3, type = 'HC0')
robust3 = sqrt(diag(COV3))
COV4 = vcovHC(OLS4, type = 'HC0')
robust4 = sqrt(diag(COV4))
COV5 = vcovHC(OLS5, type = 'HC0')
robust5 = sqrt(diag(COV5))
COV6 = vcovHC(OLS6, type = 'HC0')
robust6 = sqrt(diag(COV6))
stargazer(OLS, OLS1, OLS2, OLS3, OLS4, OLS5, OLS6, se = list(robust, robust1, robust2, robust3, robust4, robust5, robust6),
          title = "Resultados", align = TRUE, omit.stat = c("LL","ser","f"), no.space = TRUE)

coeftest(OLS6, vcov = vcovHC(OLS6, "HC0"))


rest1 = c('Sal_min', 'ln_pop', 'ln_pib', 'urbano_p', 'maxbenefit')
rest2 = c('Em_p', 'terceirog_p')
rest3 = c('Sal_min', 'ln_pop')
rest4 = c('ln_pib','urbano_p', 'maxbenefit')
a = linearHypothesis(OLS6, rest1, vcov. = hccm(OLS6, type = "hc0"))
b = linearHypothesis(OLS6, rest2, vcov. = hccm(OLS6, type = "hc0"))
c = linearHypothesis(OLS6, rest3, vcov. = hccm(OLS6, type = "hc0"))
d = linearHypothesis(OLS6, rest4, vcov. = hccm(OLS6, type = "hc0"))
a
b
c

stargazer(a,b, type = 'text')

stargazer(as.data.frame(df), type = 'text')

plot_usmap(data = df, values = "Desemprego_p", regions = 'states', color = "blue") + 
  scale_fill_gradient2(low = "white", mid = "deepskyblue", high = "darkblue", 
                       name = "Porcentagme de desemprego",midpoint = 3.5, label = scales::percent_format(scale = 1)) + 
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

plot_usmap(data = df, values = "Sal_min", regions = 'states', color = "blue") + 
  scale_fill_gradient2(low = "white", mid = "deepskyblue", high = "darkblue", 
                       name = "Salario minimo", midpoint = 8.8) + 
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

reg1 <- ggplot(data = df) + geom_point(aes(x=Sal_min, y=Desemprego_p), color = "#1F1B1B", size = 1, shape = 20) + 
  geom_smooth(aes(x=Sal_min, y=Desemprego_p), color = "#9C1C2D", size = 1, method = "lm", se=T) +
  labs(x = 'Salario Minimo',y = 'Desemprego') + 
  theme_economist()
reg1
reg2 <- ggplot(data = df) + geom_point(aes(x=Em_p, y=Desemprego_p), color = "#1F1B1B", size = 1, shape = 20) + 
  geom_smooth(aes(x=Em_p, y=Desemprego_p), color = "#9C1C2D", size = 1, method = "lm", se=T) +
  labs(x = 'Porcentagem Ensino Medio',y = 'Desemprego') + 
  theme_economist()
reg2
