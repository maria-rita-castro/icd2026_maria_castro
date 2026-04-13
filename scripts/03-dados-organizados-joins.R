# Arquivo: 03-dados-organizados-joins.R
# Autora: Maria Rita
# Data: 13/04/26
# Objetivos:
# 1. Aprender a organizar dados com a função pivot_longer() do pacote tidyr
# 2. Aprender a combinar tabelas com a função left_join() do pacote dplyr


# Configurações globais ---------------------------------------------------

# Configura o número de dígitos a serem exibidos
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # carrega tidyr, tibble, dplyr...


# Organizando dados com pivot_longer() ------------------------------------