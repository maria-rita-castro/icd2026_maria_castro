# Arquivo: 02-lista.R
# Autor(a): Maria Rita
# Data: 06/04/2026
# Objetivo:

# 1. Resolver os exercícios da lista 2


# Configurações globais ---------------------------------------------------

# Configura o número de dígitos a serem exibidos
options(digits = 5, scipen = 999)

# carrega os pacotes necessários


# Exercicio 1 -------------------------------------------------------------


## a)

# Configurações globais ---------------------------------------------------

options(digits = 5, scipen = 999)

library(here)
library(tidyverse)
library(janitor)

## b)

# Carregar pacotes

library(here)
library(readr)

# Importar os dados
dados_marketing <- read_csv(
  here("dados", "brutos", "dados-marketing.csv")
)

## c)

# glimpse(dados_marketing)


# Exercicio 2 -------------------------------------------------------------

## a)
# Cada linha (observação) representa:
# Uma semana de observação com informações sobre
# gastos em marketing (TV, rádio, redes sociais e email),
# ocorrência de promoção, atividade da concorrência
# e receita de vendas.


## b)
# Número de observações:
# 156

# Número de variáveis:
# 9


# Exercicio 3 -------------------------------------------------------------

# a)
dados_marketing_limpos <- dados_marketing  |> 
  clean_names()


## b)  

#names(dados_marketing_limpos)


# Exercicio 4 -------------------------------------------------------------

dados_marketing_limpos |>
  select (data,
          mes,
          gasto_tv,
          gasto_radio,
          promocao,
          receita_vendas)


# Exercicio 5 -------------------------------------------------------------

dados_marketing_limpos <- dados_marketing_limpos |>
  mutate(
    gasto_total = gasto_tv + gasto_radio + gasto_redes_sociais + gasto_email
  )

dados_marketing_limpos |>
  select(data, mes, gasto_total, receita_vendas)

#View(dados_marketing_limpos)


# Exercicio 6 -------------------------------------------------------------

dados_marketing_limpos <- dados_marketing_limpos |>
  mutate(
    status_promocao = ifelse(promocao == 1, "Com promoção", "Sem promoção"),
    status_concorrencia = ifelse(atividade_concorrente == 1, "Com concorrência", "Sem concorrência")
  )


# Exercicio 7 -------------------------------------------------------------

# salva os dados limpos em um arquivo rds para análises
# futuras sem precisar repetir a preparação dos dados

## define o caminho relativo para salvar o arquivo rds
caminho_rds <- here("dados/limpos/dados_marketing_limpos.rds")

## salva o objeto dados_vendas_limpos no formato rds
readr::write_rds(dados_marketing_limpos, caminho_rds)



# Exercicio 8 -------------------------------------------------------------

dados_marketing_limpos |>
  filter(promocao == 1 & receita_vendas > 1000) |>
  select(data, mes, receita_vendas, status_promocao)

# Exercicio 9 -------------------------------------------------------------
# Criar resumo mensal

resumo_mensal <- dados_marketing_limpos |>
  group_by(mes) |>
  summarise(
    receita_media = mean(receita_vendas, na.rm = TRUE),
    receita_total = sum(receita_vendas, na.rm = TRUE),
    gasto_total_medio = mean(gasto_total, na.rm = TRUE),
    semanas_com_promocao = sum(promocao == 1, na.rm = TRUE)
  ) |>
  arrange(desc(receita_media))


# Visualizar o resultado
View(resumo_mensal)
