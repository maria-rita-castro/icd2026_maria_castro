# Arquivo: 04-lista-resolucao.R
# Autor(a): seu nome
# Data: dd/mm/aaaa
# Objetivo: Resolução da Lista de Exercícios 4

# Configurações globais --------------------------------------

# Configura o número de dígitos exibidos
options(digits = 5, scipen = 999)


# Exercício 2 ------------------------------------------------

# a) 
# total de conjuntos diferentes de 20 números sorteados entre 100
total_resultados <- choose(100, 20)
total_resultados


# b)
# resultados favoráveis: os 20 números sorteados precisam estar
# dentro dos 50 números escolhidos na aposta
resultados_favoraveis <- choose(50,20)
resultados_favoraveis


# c)
# probabilidade de acertar os 20 números
prob_acertar_20 <-choose(50, 20) / choose(100, 20)
prob_acertar_20

# forma alternativa de interpretação: uma chance em quantas apostas?
uma_chance_em <- 1 / prob_acertar_20
uma_chance_em


# d)
# Escreva sua interpretação como comentário.
# É baixa, pois O valor encontrado foi aproximadamente de 0,000000793
# Isso significa que a chance de uma aposta acertar os 20 números sorteados é 
# muito baixa.


# Exercício 3 ------------------------------------------------

# probabilidade teórica do evento A: obter 5 ou 6
prob_teorica <- 2 / 6
prob_teorica

# resultados possíveis do dado
dado <- 1:6

# número de lançamentos
n <- 100

# fixa a semente para reprodutibilidade
set.seed(123)

# simula os lançamentos
# dica: em um dado equilibrado, todas as faces têm a mesma probabilidade
lancamentos <- sample(
  x = dado,
  size = n,
  replace = TRUE
)


# evento A: resultado maior ou igual a 5
evento_A <- lancamentos >= 5

# número de lançamentos favoráveis ao evento A
favoraveis <- sum(evento_A)
favoraveis

# frequência relativa do evento A
freq_relativa <- mean(evento_A)
freq_relativa

# Depois de completar para n = 100, altere n para 1000 e 10000.

#----------------------------------------------------------------
# N = 1000

# probabilidade teórica do evento A: obter 5 ou 6
prob_teorica <- 2 / 6
prob_teorica

# resultados possíveis do dado
dado <- 1:6

# número de lançamentos
n <- 1000

# fixa a semente para reprodutibilidade
set.seed(123)

# simula os lançamentos
# dica: em um dado equilibrado, todas as faces têm a mesma probabilidade
lancamentos <- sample(
  x = dado,
  size = n,
  replace = TRUE
)


# evento A: resultado maior ou igual a 5
evento_A <- lancamentos >= 5

# número de lançamentos favoráveis ao evento A
favoraveis <- sum(evento_A)
favoraveis

# frequência relativa do evento A
freq_relativa <- mean(evento_A)
freq_relativa


#----------------------------------------------------------------
# N = 10000

# probabilidade teórica do evento A: obter 5 ou 6
prob_teorica <- 2 / 6
prob_teorica

# resultados possíveis do dado
dado <- 1:6

# número de lançamentos
n <- 10000

# fixa a semente para reprodutibilidade
set.seed(123)

# simula os lançamentos
# dica: em um dado equilibrado, todas as faces têm a mesma probabilidade
lancamentos <- sample(
  x = dado,
  size = n,
  replace = TRUE
)


# evento A: resultado maior ou igual a 5
evento_A <- lancamentos >= 5

# número de lançamentos favoráveis ao evento A
favoraveis <- sum(evento_A)
favoraveis

# frequência relativa do evento A
freq_relativa <- mean(evento_A)
freq_relativa



#----------------------------------------------------------------

# Exercício 5 ------------------------------------------------


# Cálculo do valor esperado com R

# parâmetros do modelo
prob_incendio <- 0.01
indenizacao <- 150000
carregamento <- 0.25

# valor esperado de indenização por residência
valor_esperado <- prob_incendio * indenizacao

# exibe o resultado
valor_esperado

# prêmio anual por residência com acréscimo
premio <- valor_esperado * (1 + carregamento)

# exibe o resultado
premio


# a) 
# O valor esperado de indenização representa o custo médio esperado
# pela seguradora para cada residência segurada em um ano. Nesse caso,
# considerando a probabilidade de incêndio e o valor da indenização,
# a seguradora espera pagar, em média, R$ 1.500 por residência.

# b) 
# O valor esperado calculado não significa que toda residência terá
# exatamente esse valor de indenização, porque o incêndio é um evento
# aleatório. Na prática, a maioria das residências não sofrerá incêndio
# e receberá R$ 0, enquanto uma pequena parte poderá receber os
# R$ 150.000 da indenização. O valor esperado representa apenas a média
# do conjunto de segurados.

# c) 
# O carregamento de 25% é uma simplificação porque considera apenas
# um único acréscimo sobre o valor esperado da indenização. Em uma
# seguradora real, o cálculo do prêmio incluiria diversos outros fatores,
# como despesas administrativas, impostos, corretagem, resseguro,
# margem de lucro, reservas financeiras e análise de diferentes níveis
# de risco entre os segurados.


# Exercício 6 ------------------------------------------------

# fixa a semente para obter os mesmos resultados ao reexecutar
set.seed(2)

# tamanho da carteira de residências seguradas
n <- 100

# simula a indenização de cada residência: 0 ou valor total da indenização
indenizacoes <- sample(
  x = c(0, indenizacao),
  size = n,
  replace = TRUE,
  prob = c(1 - prob_incendio, prob_incendio)
)

# média das indenizações por residência na carteira simulada
media_indenizacoes <- mean(indenizacoes)
media_indenizacoes


# total de indenizações pagas pela seguradora
total_indenizacoes <- sum(indenizacoes)
total_indenizacoes


# total arrecadado com os prêmios cobrados
total_premios <- premio * n
total_premios

# resultado simplificado da carteira: prêmios recebidos menos indenizações pagas
resultado_carteira <- total_premios - total_indenizacoes
resultado_carteira

# Depois de completar para n = 100, altere n para 10000 e 100000.

#----------------------------------------------------------------
# N = 10000

# fixa a semente para obter os mesmos resultados ao reexecutar
set.seed(2)

# tamanho da carteira de residências seguradas
n <- 10000

# simula a indenização de cada residência: 0 ou valor total da indenização
indenizacoes <- sample(
  x = c(0, indenizacao),
  size = n,
  replace = TRUE,
  prob = c(1 - prob_incendio, prob_incendio)
)

# média das indenizações por residência na carteira simulada
media_indenizacoes <- mean(indenizacoes)
media_indenizacoes


# total de indenizações pagas pela seguradora
total_indenizacoes <- sum(indenizacoes)
total_indenizacoes


# total arrecadado com os prêmios cobrados
total_premios <- premio * n
total_premios

# resultado simplificado da carteira: prêmios recebidos menos indenizações pagas
resultado_carteira <- total_premios - total_indenizacoes
resultado_carteira

#----------------------------------------------------------------
# N = 100000

# fixa a semente para obter os mesmos resultados ao reexecutar
set.seed(2)

# tamanho da carteira de residências seguradas
n <- 100000

# simula a indenização de cada residência: 0 ou valor total da indenização
indenizacoes <- sample(
  x = c(0, indenizacao),
  size = n,
  replace = TRUE,
  prob = c(1 - prob_incendio, prob_incendio)
)

# média das indenizações por residência na carteira simulada
media_indenizacoes <- mean(indenizacoes)
media_indenizacoes


# total de indenizações pagas pela seguradora
total_indenizacoes <- sum(indenizacoes)
total_indenizacoes


# total arrecadado com os prêmios cobrados
total_premios <- premio * n
total_premios

# resultado simplificado da carteira: prêmios recebidos menos indenizações pagas
resultado_carteira <- total_premios - total_indenizacoes
resultado_carteira

#----------------------------------------------------------------

# a) 
# Quando n aumenta, a variável media_indenizacoes fica mais estável e apresenta 
# menos oscilações.Com poucas residências seguradas (n = 100), o resultado pode
# variar bastante. Já com n = 100000, a média tende a ficar muito próxima do 
# valor esperado.


# b) 
# Ela tende a se aproximar do valor teórico esperado da indenização média, que é:
# E(X)=prob\_incendio \times indenizacao
# Ou seja, a probabilidade de incêndio multiplicada pelo valor da indenização.


# c) 
# Uma carteira maior torna a média das indenizações mais estável porque, pela 
# Lei dos Grandes Números, os resultados aleatórios individuais começam a se 
# compensar quando há muitos segurados. Assim, a média observada fica cada vez 
# mais próxima do valor esperado, reduzindo as oscilações.

# d) Isso não elimina completamente o risco da seguradora porque eventos 
# inesperados ainda podem acontecer, como muitos incêndios ao mesmo tempo ou 
# grandes catástrofes. Além disso, sempre existe alguma variabilidade aleatória
# nas perdas, mesmo em carteiras muito grandes.