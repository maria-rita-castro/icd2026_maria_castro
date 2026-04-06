# Arquivo: 02-importacao-manipulacao.R
# Autor(a): <seu nome>
# Data: <dd/mm/aaaa>
# Objetivos:
# 1. Importar um arquivo csv de dados
# 2. Preparar os dados para análise
# 3. Aprender as funções básicas de manipulação de dados do pacote dplyr

# Configurações globais ---------------------------------------------------
options(digits = 5, scipen = 999)

library(here)
library(tidyverse)
library(janitor)

# Aquisição dos dados ----------------------------------------------------
caminho_csv <- here("dados/brutos/dados_vendas.csv")
dados_vendas <- read_csv(caminho_csv)

# Entendimento dos dados --------------------------------------------------
glimpse(dados_vendas)
head(dados_vendas)
tail(dados_vendas)
summary(dados_vendas)

# Preparação dos dados ----------------------------------------------------
dados_vendas_limpos <- dados_vendas |>
  clean_names() |>   # deixa tudo minúsculo e padronizado
  mutate(
    cidade = as.factor(cidade),
    representante = as.factor(representante),
    produto = as.factor(produto),
    receita = unidades * preco_unitario
  )

# Conferência
glimpse(dados_vendas_limpos)
names(dados_vendas_limpos)

# Salvando os dados limpos ------------------------------------------------
caminho_rds <- here("dados/limpos/dados_vendas_limpos.rds")
readr::write_rds(dados_vendas_limpos, caminho_rds)

# Lendo os dados limpos ---------------------------------------------------
dados_vendas_limpos <- readr::read_rds(caminho_rds)

# Filtros -------------------------------------------------------
# vendas em Formiga
dados_vendas_limpos |> 
  filter(cidade == "Formiga")

# vendas de um representante específico
dados_vendas_limpos |> 
  filter(representante == "Representante 1")

# vendas em Formiga por um representante específico
dados_vendas_limpos |> 
  filter(cidade == "Formiga" & representante == "Representante 1")

# Filtra as vendas realizadas em Formiga ou Arcos
dados_vendas_limpos |> 
  filter(cidade %in% c("Formiga", "Arcos"))

# Salva o resultado em um novo objeto 
dados_vendas_formiga_arcos <- dados_vendas_limpos |> 
  filter(cidade %in% c("Formiga", "Arcos"))

# Exibe o resultado
dados_vendas_formiga_arcos

#===============================================================================
# A FUNÇÃO SELECT ---------------------------------------------------------
#===============================================================================

# seleciona apenas as colunas cidade, produto e receita
dados_vendas_limpos |> 
  select(cidade, produto, receita)

# remove as colunas representante e cidade 
dados_vendas_limpos |> 
  select(-cidade, -representante)

# salvando o resultado em um novo objeto
dados_vendas_selecionados <- dados_vendas_limpos |> 
  select(cidade, produto, receita)

# exibe o resultado
dados_vendas_selecionados

#===============================================================================
# A FUNÇÃO MUTATE ---------------------------------------------------------
#===============================================================================

# Cria a variável preco_desconto (10% sobre o preco_unitario)
dados_vendas_limpos |> 
  mutate(preco_desconto = preco_unitario * 0.9)

# Cria a variável receita_total
dados_vendas_limpos |> 
  mutate(receita_total = unidades * preco_unitario)

# Receita total por cidade
dados_vendas_limpos |> 
  mutate(receita_total = unidades * preco_unitario) |> 
  group_by(cidade) |> 
  summarise(receita_total_cidade = sum(receita_total)) |> 
  arrange(desc(receita_total_cidade))

# Cria a variável categoria_receita simples
dados_vendas_limpos |> 
  mutate(categoria_receita = ifelse(receita > 1000, "Alta", "Baixa")) |> 
  select(cidade, produto, categoria_receita)

# Cria a variável categoria_receita com múltiplas categorias
dados_vendas_limpos |> 
  mutate(categoria_receita = case_when(
    receita > 1000 ~ "Alta",
    receita > 500 & receita <= 1000 ~ "Média",
    receita > 0 & receita <= 500 ~ "Baixa",
    TRUE ~ "Sem Receita"
  )) |> 
  select(cidade, produto, categoria_receita)

#===============================================================================
# AS FUNÇÕES SUMMARISE E GROUP_BY ----------------------------------------
#===============================================================================

# Calcula a receita média
dados_vendas_limpos |> summarise(receita_media = mean(receita))

# Calcula a receita total
dados_vendas_limpos |> summarise(receita_total = sum(receita))

# Calcula o número de representantes distintos
dados_vendas_limpos |> summarise(numero_representantes = n_distinct(representante))

# Calcula o número total de vendas realizadas
dados_vendas_limpos |> summarise(total_vendas = n())

# Receita média por cidade 
dados_vendas_limpos |> 
  group_by(cidade) |> 
  summarise(receita_media = mean(receita))

# Receita média por produto
dados_vendas_limpos |> 
  group_by(produto) |> 
  summarise(receita_media = mean(receita))

# Receita média por cidade e produto
dados_vendas_limpos |> 
  group_by(cidade, produto) |> 
  summarise(receita_media = mean(receita))

#===============================================================================
# A função ARRANGE --------------------------------------------------------
#===============================================================================

# ordena os dados por receita em ordem crescente
dados_vendas_limpos |> arrange(receita)

# ordena os dados por receita em ordem decrescente
dados_vendas_limpos |> arrange(desc(receita))

# ordena a receita média por cidade em ordem crescente
dados_vendas_limpos |> 
  group_by(cidade) |> 
  summarise(receita_media = mean(receita)) |> 
  arrange(receita_media)

#ordena a receita media por cidade em ordem descrescente
#salva o resultado em um novo objeto
receita_media_cidade <- 
  dados_vendas_limpos |> 
  group_by(cidade) |> 
  summarise(receita_media = mean(receita)) |> 
  arrange(desc (receita_media))

#exibe o resultado
receita_media_cidade

