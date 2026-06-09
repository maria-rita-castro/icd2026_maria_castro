#_______________________________________________________________________________


# Arquivo: 02-avaliacao-resolucao.R
# Integrante 1: Giovanna Nicole Natale Cardoso
# Integrante 2: Maria Rita de Castro
# Integrante 3: Mirelle I. Silva
# Data: 09/06/2026
# Objetivo: Resolução da Avaliação 2 — Introdução à Ciência de Dados

#_______________________________________________________________________________

# Configurações globais ---------------------------------------
options(digits = 5, scipen = 999)

#_______________________________________________________________________________


# Exercício 1 -------------------------------------------------

# a) S = {renova contrato, não renova contrato}

# b) A = {renova contrato}

# c) P(A) ou P(cliente renova o contrato)

# d)  Antes de definir uma variável aleatória, o resultado observado não é
# diretamente numérico.
# Ele é qualitativo/binário: renova contrato ou não renova contrato
# Ele só passa a ser representado numericamente depois que definimos X

# e) X = 1 se o cliente renova o contrato
# X = 0 se o cliente não renova o contrato

# f) A distribuição de probabilidade mais adequada é a distribuição
# de Bernoulli
# Justificativa: A distribuição de Bernoulli é adequada porque o cliente só pode
# ter dois resultados: renovar o contrato ou não renovar o contrato

#_______________________________________________________________________________


#_______________________________________________________________________________

# Exercício 2 -------------------------------------------------

#_______________________________________________________________________________

# a) X pode assumir dois valores:
# X = 12, quando o cliente utiliza o cupom
# X = 0, quando o cliente não utiliza o cupom

# b) P(X = 12) = 0,10

# c) P(X = 0) = 1 - 0,10 = 0,90

# d) E(X) = X1 * P(X = 12) + X2 * P(X = 0)
# E(X) = 12 * 0,10 + 0 * 0,90
# E(X) = R$ 1,20

# e) O custo esperado do desconto é R$ 1,20 por cliente que recebe o cupom
# Isso não significa que cada cliente gerará um custo de R$ 1,20
# Individualmente, o custo será R$ 0 ou R$ 12
# O valor esperado representa o custo médio por cliente quando
# consideramos muitos clientes recebendo o cupom

#_______________________________________________________________________________


#_______________________________________________________________________________

# Exercício 3 ------------------------------------------------

#_______________________________________________________________________________

# a) define os parâmetros da simulação

# fixa a semente para reprodutibilidade
set.seed(123)

# média de clientes por hora de pico
lambda <- 10

# capacidade atual da unidade
capacidade <- 13

# número de valores simulados
n_simulacoes <- 1000

# b) simula 1.000 realizações de X
# cada valor é uma contagem de clientes em uma hora de pico
clientes <- rpois(n = n_simulacoes, lambda = lambda)

# exibe as primeiras contagens simuladas
head(clientes)

# c) calcula a média simulada de clientes por hora de pico
media_simulada <- mean(clientes)
media_simulada

# d) calcula a proporção de simulações em que a capacidade
# de 13 clientes foi excedida
prop_acima_capacidade <- mean(clientes > capacidade)
prop_acima_capacidade

# e) calcula o percentil 95 da contagem de clientes por hora de pico
percentil_95 <- quantile(clientes, 0.95)
percentil_95

# f) interpretação:
# A média simulada ficou próximaa de 10 clientes por hora,
# mostrando que a simulação esta coerente com o modelo.
# Aproximadamente 14% das simulações tiveram mais de 13 cliente,
# ultrapassando a capacidade atual da unidade.
# O percentil 95 foi aproximadamente 15 clientes.
# Isso significa que em 95% das horas de pico simuladas
# a quantidade de clientes foi de até 15.
# Como esse valor é maior que a capacidade atual de 13 clientes,
# podem ocorrer momentos em que a unidade não consiga atender
# toda a demanda.
# Portanto, a capacidade atual parece suficiente para a maior
# parte das situações de pico, mas pode ser limitada nos horários
# de maior movimento.
#_______________________________________________________________________________

# Digite e execute o código necessário e escreva a 
# interpretação em comentários.