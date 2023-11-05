# Carregando as bibliotecas necessárias
rm(list = ls())
load.lib <- c("tidyverse","cepespR","stargazer","sandwich","car",
              'knitr', 'lmtest', 'coalitions', 'paletteer', 'gridExtra')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
set.seed(666)

# premissas
NBRL = 20000000 #nocional
St0 = 5.0 # BRL/USD
B = 6.0 # BRL/USD
r = 0.10 # 10%
q = 0.04 # 4% 
s = 0.20 # 20%
T = 1 # 1 ano
mu = r - q # drift para o modelo
dt = 1/252 # step


# Função para simulação de Monte Carlo para o payoff do underlying
monte_carlo_simulation = function(N) {
  W = rnorm(N, mean = 0, sd = sqrt(T / N)) # 
  St = St0 * exp(cumsum((mu - (s^2)*0.5) * (T / N) + s * W))
  St = tail(St, n = 1)
  payoffs = ifelse(St > B, ((NBRL * St / St0)*exp(q - r) - (NBRL * (1 + r)*exp(-r))), 0)
  return(payoffs)
}

# numero de simulacao e loop para o monte carlo
n_sims = seq(1000, 1000000, length.out = 150)
results = data.frame()
for (i in n_sims) {
  sim_result = replicate(i, mean(monte_carlo_simulation(252))) # Aqui estamos usando 252 (252 dias do ano) passos de tempo na simulação
  t_test = t.test(sim_result, conf.level = 0.95) # teste
  results = rbind(results, data.frame(n_sims=i, price=mean(sim_result), lower=t_test$conf.int[1], upper=t_test$conf.int[2])) # jogando os outputs em uma dataframe
}


# montecarlo para o preco final do underlying
monte_carlo_simulation2 = function(N) {
  W = rnorm(N, mean = 0, sd = sqrt(T / N))
  St = St0 * exp(cumsum((mu - (s^2)*0.5) * (T / N) + s * W))
  St = tail(St, n = 1)
  return(St)
}





# Preco da acao, pela questao 2
ST = mean(replicate(1000000, mean(monte_carlo_simulation2(252))))

d1 = (log(St0^2/(ST-St0)) + (q + (s^2)/2)*T) / (s*sqrt(T))
d2 = d1 - s*sqrt(T)
d = (log(ST/(1+r))+(q-(s^2)/2)*T)/s*sqrt(T)

c = St0*exp(q-r)*pnorm(d1) - ((ST-St0)/St0)*exp(-q*T)*pnorm(d2)
C = (1+r)*exp(-r*T)*pnorm(d)
P = -(NBRL/St0)*c + NBRL*C

# Gerando o gráfico
ggplot(results, aes(x=n_sims)) +
  geom_line(aes(y=price), color="blue") +
   geom_ribbon(aes(ymin=lower, ymax=upper), fill="blue", alpha=0.1) +
  labs(x="Número de Simulações", y="Preço Médio do Operacao") +
  geom_line(aes(y = P), color = 'red')
