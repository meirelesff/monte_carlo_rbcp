# --- 
# REPLICACAO 'SIMULACOES DE MONTE CARLO NO ENSINO DE CIENCIA POLITICA
# ---


# Carrega os pacotes necessarios
library(stargazer)
library(gridExtra)
library(ggplot2)
library(MASS)


# Fixa um seed para reproduzir os resultados
set.seed(2)


# ---
# FUNCOES USADAS
# ---

# Funcao para simular N regressoes com uma variavel omitida (retorna um data.frame com as estimativas)
lmbias <- function(sim = 1000, N = 100, sd.x = 1, beta = 1, beta0 = 1, bias = 0.5){
  res_bias <- res <- numeric(sim)
  for(i in 1:sim){
    x_bias <- mvrnorm(N, mu = c(0, 0), matrix(c(1, bias, bias, 1), nrow = 2), empirical = T)
    x <- x_bias[, 1]
    x_bias <- x_bias[, 2]
    y <- beta0 + beta * x + x_bias + rnorm(N, mean = 0, sd = 1)
    res_bias[i] <- coef(lm(y ~ x))[2]
    res[i] <- coef(lm(y ~ x + x_bias))[2]
  }
  out <- data.frame('X com viés' = res_bias, 'X sem viés' = res)
  out
}


# Funcao para simular uma votacao com diferentes tipos de regra de maioria
votacao <- function(legisladores, prob_sim, regra = c("unanimidade", "maioria_simples")){
  
  regra <- match.arg(regra)
  resultado <- rbinom(legisladores, 1, prob = prob_sim)
  if(regra == "unanimidade") resultado <- as.numeric(sum(resultado) == legisladores)
  else resultado <- as.numeric(sum(resultado) >= (legisladores %/% 2) + 1)
  resultado <- ifelse(resultado == 1, "Aprovadas", "Rejeitadas")
  factor(resultado, levels = c("Rejeitadas", "Aprovadas"))
}


# ---
# FIGURAS UTILIZADAS NO PAPER
# ---


# GRAFICO 1 - Simulando a media de altura dos brasileiros
# Define os parametros usados (podem ser alterados)
altura_media <- 170
desvio <- 15
sims <- 1000
amostra <- 10

# Roda a simulacao
resultado <- replicate(sims, mean(rnorm(amostra, altura_media, desvio)))

# Salva o grafico no diretorio corrente
jpeg("graf1.jpeg", width = 6, height = 4, res = 300, units = "in")
qplot(resultado, geom = "histogram", binwidth = 1, ylab = "Frequência", xlab = "Média de altura", colour = I("gray"), fill = I("gray90")) + 
  geom_vline(xintercept = mean(resultado), size = 0.7, linetype = 2) +
  scale_y_continuous(expand = c(0, 0))
dev.off()


# GRAFICO 2 - Simulando um processo legislativo
# Define os parametros usados (podem ser alterados)
legisladores <- 5
prob_sim <- .5
sims <- 1000

# Simula 1000 votacoes com 5 legisladores por unanimidade
unam <- replicate(sims, votacao(legisladores, prob_sim, "unanimidade"))

# Simula 1000 votacoes com 5 legisladores por maioria simples
maioria <- replicate(sims, votacao(legisladores, prob_sim, "maioria_simples"))

# Gera os graficos
g2a <- qplot(factor(unam), fill = I("gray90"), col = I("gray"), ylab = "Freqência", xlab = "Votações", main = "Unanimidade") + 
  scale_y_continuous(expand = c(0, 0))

g2b <- qplot(factor(maioria), fill = I("gray90"), col = I("gray"), ylab = NULL, xlab = "Votações", main = "Maioria simples") + 
  scale_y_continuous(expand = c(0, 0))

# Salva o grafico no diretorio corrente
jpeg("graf2.jpeg", width = 6, height = 3, res = 300, units = "in")
grid.arrange(g2a, g2b, ncol = 2)
dev.off()


# GRAFICO 3 - Simulando um processo legislativo com polarizacao (10.000 simulacoes)
sims <- 10000
legisladores <- 10
polarizacao <- seq(0.0, 0.5, by = 0.05)
prob_sim <- vector("list", length(polarizacao))
for(i in 1:length(polarizacao)) {
  prob_sim[[i]] <- rep(0.5, 10)
  prob_sim[[i]][1:5] <- prob_sim[[i]][1:5] - polarizacao[i]
  prob_sim[[i]][6:10] <- prob_sim[[i]][6:10] + polarizacao[i]
}

res <- lapply(prob_sim, function(x){
  
  data.frame(replicate(sims, votacao(legisladores, x, "maioria_simples")), polar = x[6] - x[5])
  
})

res <- do.call("rbind", res)
names(res) <- c("Resultado", "Polarizacao")

jpeg("graf3.jpeg", width = 6, height = 3.5, res = 300, units = "in")
ggplot(res, aes(x = as.factor(Polarizacao), fill = Resultado)) + geom_bar(color = "gray") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("gray90", "gray20")) +
  labs(fill = NULL, 
       x = "Grau de polarização\n(na prob. de votar sim)",
       y = "Frequência")
dev.off()


# GRAFICO 4 - Simulando variaveis omitidas
# Roda 1000 simulacoes
x <- lmbias()

# Gera os graficos
vies <- qplot(x$X.com.viés, geom = "histogram", ylab = "Frequência", xlab = "Coeficientes", colour = I("gray"), fill = I("gray90"), main = "Estimativas enviesadas") + 
  geom_vline(xintercept = 1, size = 0.7, linetype = 2) +
  scale_y_continuous(expand = c(0, 0))

sem_vies <- qplot(x$X.sem.viés, geom = "histogram", ylab = "Frequência", xlab = "Coeficientes", colour = I("gray"), fill = I("gray90"), main = "Estimativas não-enviesadas") + 
  geom_vline(xintercept = 1, size = 0.7, linetype = 2) +
  scale_y_continuous(expand = c(0, 0))

# Salva o grafico no diretorio corrente
jpeg("graf4.jpeg", width = 6, height = 3, res = 300, units = "in")
grid.arrange(sem_vies, vies, ncol = 2)
dev.off()


# GRAFICO 5 - Simulando variaveis omitidas (outros graus de correlacao entre x e z)
# Roda 1000 simulacoes
x <- lmbias(bias = -.5)

# Gera os graficos
vies1 <- qplot(x$X.com.viés, geom = "histogram", ylab = "Frequência", xlab = "Coeficientes", colour = I("gray"), fill = I("gray90"), main = "Corr. -0.5") + 
  geom_vline(xintercept = 1, size = 0.7, linetype = 2) +
  scale_y_continuous(expand = c(0, 0))

# Roda 1000 simulacoes
x2 <- lmbias(bias = .3)

# Gera os graficos
vies2 <- qplot(x2$X.com.viés, geom = "histogram", ylab = "Frequência", xlab = "Coeficientes", colour = I("gray"), fill = I("gray90"), main = "Corr. 0.3") + 
  geom_vline(xintercept = 1, size = 0.7, linetype = 2) +
  scale_y_continuous(expand = c(0, 0))


# Salva o grafico no diretorio corrente
jpeg("graf5.jpeg", width = 6, height = 3, res = 300, units = "in")
grid.arrange(vies1, vies2, ncol = 2)
dev.off()


# GRAFICO 6 - Simulando variaveis omitidas (resultados extendidos)
cors <- lapply(seq(-1, 1, by = 0.1), function(x) as.data.frame(cbind(lmbias(bias = x), bias = x)))
cors <- do.call("rbind", cors)

jpeg("graf6.jpeg", width = 6.5, height = 3.5, res = 300, units = "in")
ggplot(cors, aes(x = as.factor(bias), y = `X.com.viés`)) + 
  stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Grau de viés\n(correlação entre X e Z)", y = "Estimativa do efeito de X")
dev.off()


rm(list = ls())
