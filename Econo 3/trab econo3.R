rm(list = ls())
load.lib <- c("tidyverse", "ggplot2", "haven", "vtable", "foreign",
              "plm","fixest", 'wbstats', 'panelr', 'countrycode', 'knitr',
              'skimr')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


# Baixando e arrumando as bases
  # Baixando
paises = read.csv('paises.csv', header = F) #Lista do Paises de analise
paises2 = as.data.frame(t(paises))
paises = as.character(as.vector(paises2$V1))
paises = gsub(' ', '', paises)

gdp.pc.g = wb_data("NY.GDP.PCAP.KD.ZG") %>% 
  select(c(3:5)) %>% rename(gdp.pc = 3)
governance = wb_data('IQ.CPA.PROP.XQ')%>% 
  select(c(3:5)) %>% rename(governance.rating = 3)
inflation = wb_data("FP.CPI.TOTL.ZG")%>% 
  select(c(3:5)) %>% rename(inflation = 3)
netmigration = wb_data("SM.POP.NETM")%>% 
  select(c(3:5)) %>% rename(net.migration = 3)
pop = wb_data("SP.POP.TOTL")%>% 
  select(c(3:5)) %>% rename(pop = 3)
school.e = wb_data("SE.ENR.PRSC.FM.ZS")%>% 
  select(c(3:5)) %>% rename(school.enrolment = 3)
unemployment = wb_data("SL.UEM.TOTL.ZS")%>% 
  select(c(3:5)) %>% rename(unemployment = 3)
gdp.1s = wb_data("NV.AGR.TOTL.ZS") %>% 
  select(c(3:5)) %>% rename(gdp.1s = 3)
mrate5 = wb_data("SH.DYN.MORT") %>%
  select(c(3:5)) %>% rename(mortality.rate = 3)
childemp = wb_data("SL.TLF.0714.ZS") %>% 
  select(c(3:5)) %>% rename(child.employment = 3)

listdf = list(netmigration, unemployment, gdp.pc.g, governance, inflation, pop, school.e, gdp.1s,
              mrate5, childemp)
rm(gdp.pc.g, governance, inflation, netmigration, pop, school.e, unemployment, gdp.1s,
   mrate5, childemp)

  # Arrumando
df = listdf %>% reduce(inner_join, by = c('country', 'date'))
df = filter(df, country %in% paises) %>% 
  filter(!is.na(unemployment))
df$country = gsub('Turkiye', 'Turkey', df$country)
df$continent <- countrycode(sourcevar = df$country, origin = "country.name",
                            destination = "continent")
df$decade = round(df$date, -1)
df = df %>% relocate(c(continent, decade), .after = date)
df$unemployment.m = ave(df$unemployment, df$country)
df$unemployment.fe = df$unemployment - df$unemployment.m
df = df %>% relocate(c(unemployment.m, unemployment.fe), .after = unemployment)
pdf1 = pdata.frame(df)
pdf2 = panel_data(df,id = country, wave = date)
#write.csv(x = df, file = 'base.csv')


# Estat descritiva das variaveis principais (netmigration e desemprego)

  # Net migration por continente (pra n ficar tao extensa a tabela)
tabl1 = df %>% 
  group_by(continent) %>% 
  summarise(n = n(),
            Nas = sum(is.na(net.migration)),
            media = mean(net.migration),
            min = min(net.migration),
            median = median(net.migration),
            max = max(net.migration),
            var = var(net.migration)
            )
kable(tabl1, format = 'pipe')
tabl1.2 = df %>% 
  group_by(continent, decade) %>% 
  summarise(n = n(),
            Nas = sum(is.na(net.migration)),
            media = mean(net.migration),
            min = min(net.migration),
            median = median(net.migration),
            max = max(net.migration),
            var = var(net.migration)
  )
kable(tabl1.2, format = 'pipe')


tabl2 = df %>% 
  group_by(continent) %>% 
  summarise(n = n(),
            Nas = sum(is.na(unemployment)),
            media = mean(unemployment, na.rm = T),
            min = min(unemployment, na.rm = T),
            median = median(unemployment, na.rm = T),
            max = max(unemployment, na.rm = T),
            var = var(unemployment, na.rm = T)
  )
kable(tabl2, format = 'pipe')

tabl2.2 = df %>% 
  group_by(continent, decade) %>% 
  summarise(n = n(),
            Nas = sum(is.na(unemployment)),
            media = mean(unemployment, na.rm = T),
            min = min(unemployment, na.rm = T),
            median = median(unemployment, na.rm = T),
            max = max(unemployment, na.rm = T),
            var = var(unemployment, na.rm = T)
  )
kable(tabl2.2, format = 'pipe')

tabl3 = df %>% 
  group_by(continent) %>% 
  summarise(n = n(),
            Nas = sum(is.na(unemployment.fe)),
            media = mean(unemployment.fe, na.rm = T),
            min = min(unemployment.fe, na.rm = T),
            median = median(unemployment.fe, na.rm = T),
            max = max(unemployment.fe, na.rm = T),
            var = var(unemployment.fe, na.rm = T)
  )
kable(tabl3, format = 'pipe')

tabl3.2 = df %>% 
  group_by(continent, decade) %>% 
  summarise(n = n(),
            Nas = sum(is.na(unemployment.fe)),
            media = mean(unemployment.fe, na.rm = T),
            min = min(unemployment.fe, na.rm = T),
            median = median(unemployment.fe, na.rm = T),
            max = max(unemployment.fe, na.rm = T),
            var = var(unemployment.fe, na.rm = T)
  )
kable(tabl3.2, format = 'pipe')

# Correlacao
cor1 = cor(df$net.migration, df$unemployment)
cor2 = cor(df$net.migration, df$unemployment.fe)

tabl4 = df %>% 
  group_by(date) %>% 
  summarise(cor = cor(net.migration, unemployment))
kable(tabl4, format = 'pipe')


# Graficos

ggplot(df) + geom_line(aes(x = date, y = net.migration, color = continent)) +
  facet_grid(~continent)

ggplot(df) + geom_line(aes(x = date, y = unemployment, color = country)) +
  facet_grid(~continent)

ggplot(df) + geom_line(aes(x = date, y = unemployment.fe, color = country)) +
  facet_grid(~continent)


tablgraph = df %>% group_by(continent, date) %>% 
  summarise(net.migration = mean(net.migration),
            unemployment = mean(unemployment),
            unemployment.fe = mean(unemployment.fe))

ggplot(tablgraph) + geom_line(aes(x = date, y = net.migration, color = continent)) 

ggplot(tablgraph) + geom_line(aes(x = date, y = unemployment, color = continent)) 

ggplot(tablgraph) + geom_line(aes(x = date, y = unemployment.fe, color = continent)) 


# Regressoes

  # Teste de Haussman  para escolher o modelo


