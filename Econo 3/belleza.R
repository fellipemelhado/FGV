library(haven)
library(plm)
library(ggplot2)
library(tidyr)
library(dplyr)
library(fastDummies)
library(lmtest)
library(readxl)
library(data.table)
library(sandwich)
library(readxl)
library(lmtest)
library(lfe)
library(stargazer)
library(geobr)
library(ggdag)

#-------------------------------- ABRINDO AS BASES PARA 2016 ----------------------------------------------------
cand <- list.files("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III", "consulta_cand")
vot <- list.files("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III", "vot")

c<-apply(fread(paste0("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III/",cand[1])), 2, class)
candidatos.df <- NULL
for (i in cand) {
  temp <- fread(paste0("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III/", i), colClasses = c)
  candidatos.df <- bind_rows(candidatos.df, temp)
}


c<-apply(fread(paste0("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III/",vot[1])), 2, class)
votos.df <- NULL
for (i in vot) {
  temp <- fread(paste0("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III/", i), colClasses = c)
  votos.df <- bind_rows(votos.df, temp)
}


candidatos.df <- filter(candidatos.df, DS_CARGO == "VEREADOR")
candidatos.df <- filter(candidatos.df, DS_SITUACAO_CANDIDATURA == "APTO")
candidatos.df$MULHER <- ifelse(candidatos.df$DS_GENERO == "FEMININO", 1, 0)
sexos.df <- data
sexos.df <-data.frame(NM_CANDIDATO = candidatos.df$NM_CANDIDATO,
                      MULHER = candidatos.df$MULHER) 
votos.df <- filter(votos.df, DS_CARGO == "Vereador")
votos.df <- votos.df %>% left_join(sexos.df, by = c("NM_CANDIDATO"))

base_2016 <- read.csv("C:/Users/lucas/OneDrive/Documentos/FGV/Matérias/Econo III/base_2016.csv", sep = ",")
colnames(base_2016)[3] <- "SG_UE"
colnames(base_2016)[5] <- "SG_UF"
colnames(base_2016)[10] <- "NR_PARTIDO"
colnames(base_2016)[7] <- "NM_CANDIDATO"
base_2016$NUM_TURNO <- NULL
base_2016$COD_MUN_IBGE <- NULL
base_2016$DESCRICAO_CARGO <- NULL
base_2016$NUMERO_CANDIDATO <- NULL
base_2016$CPF_CANDIDATO <- NULL
base_2016$SIGLA_PARTIDO <- NULL
base_2016$NUM_TITULO_ELEITORAL_CANDIDATO <- NULL
base_2016$MULHER <- ifelse(base_2016$DESCRICAO_SEXO == "FEMININO", 1, 0)
base_2016$DESCRICAO_SEXO <- NULL
base_2016$DESC_SIT_TOT_TURNO <- NULL

#--------------------------------- LIMPANDO AS BASES PARA 2016 ----------------------------------------------

#Criando as bases de votos dos candidatos e de candidatos para se analisar a porcentagem de mulheres de 
votos_brasil.df <- votos.df%>% group_by(ANO_ELEICAO, SG_UF, SG_UE, NR_PARTIDO, NM_CANDIDATO, MULHER) %>% summarise(qntd_voto = sum(as.numeric(QT_VOTOS_NOMINAIS)))
temp1 <- candidatos.df %>% group_by(ANO_ELEICAO, SG_UF, SG_UE, NR_PARTIDO) %>% summarise(prop_mulher = mean(MULHER))


temp2 <- unique(temp1[temp1$ANO_ELEICAO == 2016, 2:5])
temp2$TRATADO <- 1*(temp2$prop_mulher<0.3)
temp2$prop_mulher <- NULL

candidatos_brasil.df <- temp1 %>% left_join(temp2)


votos_brasil.df$votos_mulheres <- votos_brasil.df$MULHER*votos_brasil.df$qntd_voto
votos_brasil.df$UE_PARTIDO <- paste(votos_brasil.df$ANO_ELEICAO, votos_brasil.df$SG_UE, votos_brasil.df$NR_PARTIDO, sep = "")
candidatos_brasil.df$UE_PARTIDO <- paste(candidatos_brasil.df$ANO_ELEICAO, candidatos_brasil.df$SG_UE, candidatos_brasil.df$NR_PARTIDO, sep = "")
votos_mulheres.df <- aggregate(votos_mulheres ~ UE_PARTIDO, data = votos_brasil.df, FUN = sum)
base_2016 <- filter(base_2016, MULHER == 1)
base_2016$UE_PARTIDO <- paste(base_2016$ANO_ELEICAO, base_2016$SG_UE, base_2016$NR_PARTIDO, sep = "")
votos_mulheres_2016.df <- aggregate(QTDE_VOTOS ~ UE_PARTIDO, data = base_2016, FUN = sum)
colnames(votos_mulheres_2016.df )[2] <- "votos_mulheres"
votos_mulheres.df <- rbind(votos_mulheres_2016.df,votos_mulheres.df )

#Juntando na base geral
base_geral.df<- candidatos_brasil.df %>% left_join(votos_mulheres.df, by = c("UE_PARTIDO"))

base_geral.df$ANO_TRATADO <- paste(base_geral.df$ANO_ELEICAO, base_geral.df$TRATADO, sep = "")


#Fazendo e ajeitando a tabela para fazer o gráfico DD para analise da hipótese de identificação
tabela_geral <- aggregate(votos_mulheres ~ ANO_TRATADO, data = base_geral.df, FUN = mean)
tabela_1 <- tabela_geral[1:2, ]
tabela_2 <-  tabela_geral[4:7, ]
tabela_geral <- rbind(tabela_1, tabela_2)

tabela_geral$ANO <- c(2012, 2012, 2016, 2016, 2020, 2020)
tabela_geral$GRUPO <- c("CONTROLE","TRATADO","CONTROLE","TRATADO", "CONTROLE", "TRATADO")
tabela_geral <- tabela_geral[ ,2:4]
tabela_cf <- data.frame(votos_mulheres = c(166.3599, 845.5611 - 466.6577 + 166.3599),
                        ANO = c(2016,2020),
                        GRUPO = c("CONTRA-FACTUAL", "CONTRA-FACTUAL"))

tabela_geral <- rbind(tabela_geral, tabela_cf)

ggplot(tabela_geral, aes(ANO, votos_mulheres, group= GRUPO, colour = GRUPO)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete(limits = c(2012, 2016,2020)) +
  labs(title = "Efeito Causal da Lei de Cotas para Mulheres") +
  xlab("Ano") +
  ylab("Média de Votos em Mulheres") +
  geom_vline(xintercept = 2016)


#Ajustando a base para fazer a regressão
base_geral.df$ANO2020 <- ifelse(base_geral.df$ANO_ELEICAO == "2020",1,0) 
base_geral.df$ID <- paste(base_geral.df$SG_UE, base_geral.df$NR_PARTIDO, sep = "")

base_geral_painel.df <- pdata.frame(base_geral.df, index = c("ID", "ANO_ELEICAO"))
base_geral_painel.df$TRATADO_ANO2020 <- base_geral_painel.df$TRATADO*base_geral_painel.df$ANO2020
base_geral_painel.df$X2020 <- NULL


#Fazendo as regressões
base_geral_painel.df$UF_PART <- paste(base_geral_painel.df$NR_PARTIDO , base_geral_painel.df$SG_UF, sep = "")
regres2 <- felm(votos_mulheres ~ ANO2020 + TRATADO + TRATADO_ANO2020 + prop_mulher | NR_PARTIDO + SG_UF |0| UF_PART, data = base_geral_painel.df)
summary(regres2)
stargazer(regres2)



#Fazendo os data frames para as estatísticas descritivas 
states <- read_state(year = "2019")

colnames(states)[2] <- "SG_UF"
base_geral_painel.df <- base_geral_painel.df %>% left_join(states, by = c("SG_UF"))
base_2012 <- filter(base_geral.df, ANO_ELEICAO == 2012)
base_2016 <- filter(base_geral.df, ANO_ELEICAO == 2016)
base_2020 <- filter(base_geral.df, ANO_ELEICAO == 2020)
votos_2016 <- filter(votos_brasil.df, ANO_ELEICAO == 2016)
votos_2020 <- filter(votos_brasil.df, ANO_ELEICAO == 2020)

stargazer(base_geral_painel.df)


medias_2012.df <- aggregate(prop_mulher ~ SG_UF, data = base_2012, FUN = mean)
medias_2012.df <- medias_2012.df  %>% left_join(states, by = c("SG_UF"))

medias_2016.df <- aggregate(votos_mulheres ~ SG_UF, data = base_2016, FUN = sum)
medias_2016.df <- medias_2016.df  %>% left_join(states, by = c("SG_UF"))
votos_2016 <- aggregate(qntd_voto ~ SG_UF, data = votos_2016, FUN = sum)
medias_2016.df <- medias_2016.df  %>% left_join(votos_2016, by = c("SG_UF"))
medias_2016.df$votos_100 <- (medias_2016.df$votos_mulheres/medias_2016.df$qntd_voto)*100000


media_2020.df <- aggregate(prop_mulher ~ SG_UF, data = base_2020, FUN = mean)
media_2020.df <- media_2020.df  %>% left_join(states, by = c("SG_UF"))
votos_2020 <- aggregate(qntd_voto ~ SG_UF, data = votos_2020, FUN = sum)
media_2020.df <- media_2020.df  %>% left_join(votos_2020, by = c("SG_UF"))
media_2020.df$votos_100 <- (media_2020.df$votos_mulheres/media_2020.df$qntd_voto)*100000

no_axis <- theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())


#Fazendo os gráficos
ggplot() +
  geom_sf(data=medias_2012.df, aes(fill = prop_mulher, geometry = geom), color = NA, size=.15, show.legend = T) +
  labs(title="Média da Proporção de Mulheres em 2012", size=8) +
  scale_fill_distiller(palette = "Reds", name="Média", trans = "reverse") +
  theme_minimal() +
  no_axis

ggplot() +
  geom_sf(data=medias_2016.df, aes(fill = votos_100, geometry = geom), color = NA, size=.15, show.legend = T) +
  labs(title="Votos em Mulheres por 100 Mil Votos em 2016", size=8) +
  scale_fill_distiller(palette = "Reds", name="Média", trans = "reverse") +
  theme_minimal() +
  no_axis

#Relação entre proporção de mulheres e votos
ggplot(base_geral.df, aes(x = votos_mulheres, y = prop_mulher)) +
  geom_point() +  
  geom_smooth(method = "lm") + labs(title = "Votos em Mulheres e Proporção de Mulheres") +
  ylab("Proporção de Mulheres") +
  xlab("Votos em Mulheres") 

#Analisando a média de votos de cada ano
mean(filter(base_geral_painel.df, ANO_ELEICAO == 2012)$votos_mulheres, na.rm = T)
mean(filter(base_geral_painel.df, ANO_ELEICAO == 2016)$votos_mulheres, na.rm = T)
mean(filter(base_geral_painel.df, ANO_ELEICAO == 2020)$votos_mulheres, na.rm = T)


#Analisando um município específico
mean(filter(base_geral_painel.df, SG_UE== 62731)$votos_mulheres, na.rm = T)
base_tratados <- filter(base_tratados, votos_mulheres < 233.0667)

base_2016 <- filter(base_geral_painel.df, ANO_ELEICAO == 2016)
base_2016$Contagem <- 1
base_2020 <- filter(base_geral_painel.df, ANO_ELEICAO == 2020 & TRATADO == 1)

partidos <- aggregate(Contagem ~ NR_PARTIDO, data = base_2016, FUN = sum)


#Fazendo regressão para testar a hipótese de identificação
base_sem_2020 <- base_geral.df %>% filter(ANO_ELEICAO %in% c("2012", "2016"))
regres3 <- felm(votos_mulheres ~   TRATADO*factor(ANO_ELEICAO)  + prop_mulher | NR_PARTIDO + SG_UF |0| UF_PART, data =base_sem_2020 )
base_sem_2020$UF_PART <- paste(base_sem_2020$NR_PARTIDO , base_sem_2020$SG_UF, sep = "")
summary(regres3)    


#Fazendo o DAG
dag <- dagify(Vto ~ Qntd,
              Qntd ~ Pat,
              Qntd ~ Est,
              Vto ~ Pat,
              Vto ~ Est,
              Vto ~ Temp,
              Vto ~ Trat,
              Vto ~ Lei,
              
              exposure = c("Qntd", "Pat", "Est", "Temp", "Trat", "Lei"),
              outcome = "Vto")

ggdag(dag, layout = "circle")
ggdag_parents(dag, "Vto")
dag %>% 
  node_parents("Vto") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_color_hue(breaks  = c("paren", "child"))