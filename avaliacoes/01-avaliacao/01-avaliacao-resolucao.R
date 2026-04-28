# Arquivo: 01-avaliacao-resolucao.R
# Integrante 1: Maria Rita de Castro
# Integrante 2: Mirelle Istefine da Silva
# Integrante 3: Giovanna Nicole Natale Cardoso

# Data: 28/04/2026
# Objetivo: Resolução da Avaliação 1 — Introdução à Ciência de Dados


# Configurações globais -----------------------------------------------

options(digits = 5, scipen = 999)

# carrega os pacotes usados (Exercício 1)

# Configurações globais ---------------------------------------------------

options(digits = 5, scipen = 999)

library(here)
library(tidyverse)
library(janitor)
# Carregar pacotes

library(here)
library(readr)

# Exercício 1 -----------------------------------------------------------

# importa o arquivo agencias.csv

# define o caminho relativo do arquivo usando a função here():
caminho_agencias <- here("dados", "brutos", "agencias.csv")
  
  # importa o arquivo com a função read_csv:
  dados_agencias <- read_csv(caminho_agencias)
  
  
  # inspeciona a estrutura do objeto
  glimpse(dados_agencias)
  
  # importa o arquivo credito_trimestral.csv
  
  # define o caminho relativo do arquivo usando a função here():
  caminho_credito <-here("dados", "brutos", "credito_trimestral.csv")
  
  # importa o arquivo com a função read_csv:
  dados_credito <- read_csv(caminho_credito)
  
  # inspeciona a estrutura do objeto
  glimpse(dados_credito)
  
  
  # Exercício 2 ----------------------------------------------------------

# 2.a)
  
  dados_agencias_plenas <- 
    filter(dados_agencias, tipo_agencia == "Plena")
  
  #imprimir
  dados_agencias_plenas

# 2.b)
  
  dados_agencias |>
    select(nome_agencia, cidade, num_cooperados) |>
    arrange(desc(num_cooperados))


# 2.c)
  
  dados_agencias |>
    filter(cidade == "Divinópolis" & num_cooperados > 1000)



# Exercício 3 ---------------------------------------------------------

# 3.a) pivot_longer

# reorganiza os dados de crédito em trimestre e volume_credito
dados_credito_longo <- dados_credito |>
  pivot_longer(
    cols = -codigo_agencia,
    names_to = "trimestre",
    values_to = "volume_credito"
  )
  
  # 3.b) left_join
  
  # combina `dados_credito_longo`com `dados_agencias`
  dados_completos <- dados_credito_longo |>
    left_join(dados_agencias, by = "codigo_agencia")
  
  glimpse(dados_completos)
  
  # Exercício 4 ---------------------------------------------------------

# cria dados_analise com credito_por_cooperado
dados_analise <- dados_completos |>
  mutate(credito_por_cooperado = volume_credito / num_cooperados) 
  
  # resume por cidade e ordena por volume_total
dados_analise |>
  group_by(cidade) |>
  summarise(
    volume_total = sum(volume_credito, na.rm = TRUE),
    media_dos_creditos_por_cooperado = mean(credito_por_cooperado, na.rm = TRUE)
  ) |>
  arrange(desc(volume_total))
  
  
  # Resposta do Exercício 4:
  
  # Cidade com maior volume_total:Divinópolis com 16591000
  # Cidade com maior media_dos_creditos_por_cooperado:Fomrmiga com 1632
  
  
  
  # Exercício 5 ---------------------------------------------------------

# classifica nivel_credito e resume por tipo_agencia
resumo_por_tipo <- dados_analise |>
  mutate(
    nivel_credito = case_when(
      credito_por_cooperado < 1400 ~ "Baixo",
      credito_por_cooperado >= 1400 & credito_por_cooperado < 1700 ~ "Médio",
      credito_por_cooperado >= 1700 ~ "Alto"
    )
  ) |>
  group_by(tipo_agencia, nivel_credito) |>
  summarise(
    volume_total = sum(volume_credito, na.rm = TRUE),
    n_obs = n()
  ) |>
  arrange(desc(volume_total))

glimpse (resumo_por_tipo)

  
  
  
  
  
  
  
  
  
  
  
  
  
  