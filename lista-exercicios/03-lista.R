# Arquivo: 03-lista.R
# Autor(a): Maria Rita
# Data: 14/04/2026
# Objetivo: Resolução da Lista de Exercícios 3

# Configurações globais --------------------------------------

# Configura o número de dígitos exibidos
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(here)
library(tidyverse)

dados_receitas <- read_csv(here("dados/brutos/receitas_trimestres.csv"))


# Exercício 1 ------------------------------------------------

## a
library(here)
dados_receitas <- read_csv(here("dados/brutos/receitas_trimestres.csv"))

## b
glimpse(dados_receitas)

## c
# A tabela está em formato desorganizado porque os valores de uma mesma variável
# (trimestres) estão distribuídos em várias colunas (T1, T2, T3, T4),
# o que dificulta análises, gráficos e manipulação dos dados.
# O formato ideal (longo) teria uma coluna para "trimestre" e outra para "receita".

## d
receitas_longas <- dados_receitas |>
  pivot_longer(
    cols = starts_with("T"),
    names_to = "trimestre",
    values_to = "receita"
  )

## e
glimpse(receitas_longas)
View(receitas_longas)


# Exercício 2 ------------------------------------------------


## a
library(here)
dados_desempenho <- read_csv(here("dados/brutos/desempenho-empresa.csv"))

## b
glimpse(dados_desempenho)


## c
# Os nomes das colunas misturam duas informações:
# - o tipo de indicador (receita ou despesa)
# - o trimestre (T1 ou T2)
# Ex: receita_T1, despesa_T2

## d
desempenho_longo <- dados_desempenho |>
  pivot_longer(
    cols = -empresa,
    names_to = c("indicador", "trimestre"),
    names_sep = "_",
    values_to = "valor"
  )

## e
glimpse(desempenho_longo)
View(desempenho_longo)



# Exercício 3 ------------------------------------------------


## a
library(here)
dados_produtos <- read_csv(here("dados/brutos/produtos.csv"))
dados_vendas <- read_csv(here("dados/brutos/vendas.csv"))
dados_clientes <- read_csv(here("dados/brutos/clientes.csv"))

## b
glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

## c
#Juntar vendas com produtos (left_join)
vendas_produtos <- dados_vendas |>
  left_join(dados_produtos, by = "codigo_produto")

glimpse(vendas_produtos)
View(vendas_produtos)

## d
#Juntar com clientes
relatorio_vendas <- vendas_produtos |>
  left_join(dados_clientes, by = "id_cliente")

glimpse(relatorio_vendas)
View(relatorio_vendas)

## e
#Selecionar colunas específicas
relatorio_final <- relatorio_vendas |>
  select(
    id_venda,
    codigo_produto,
    id_cliente,
    data_venda,
    nome_produto,
    categoria,
    quantidade,
    nome_cliente,
    cidade
  )

glimpse(relatorio_final)

## f
View(relatorio_final)

## g
#Comentário sobre NA:
# Os valores NA podem ocorrer porque o left_join mantém todos os registros de vendas,
# mesmo quando não encontra correspondência em produtos ou clientes.
# Ex: produto ou cliente não cadastrado na outra tabela.

## h
#inner_join
vendas_produtos_inner <- dados_vendas |>
  inner_join(dados_produtos, by = "codigo_produto")

View(vendas_produtos_inner)

# Comentário:
# O inner_join mantém apenas os registros que possuem correspondência nas duas tabelas.
# Ou seja, remove vendas com produtos inexistentes.

## i
#full_join
vendas_produtos_full <- dados_vendas |>
  full_join(dados_produtos, by = "codigo_produto")

View(vendas_produtos_full)

# Comentário:
# O full_join mantém TODOS os registros das duas tabelas.
# Isso pode gerar mais valores NA, pois inclui dados sem correspondência de ambos os lados.
