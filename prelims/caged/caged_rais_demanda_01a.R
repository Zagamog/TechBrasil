###############################################################################
# caged_rais_demanda_01a.R
#
# PROPAG / Juros por Educação — Análise Exploratória
# Análise CAGED-RAIS ao nível CBO2 × Região Intermediária (2023-2024)
#
# RENOMEADO DE: caged_rais_demanda.R
# AUTOR ORIGINAL: Equipe FGV
#
# NOTA: Versão exploratória anterior à pipeline de produção
#   (caged_rais_01a.R → caged_rais_02a.R). Opera em nível CBO2 × região
#   intermediária com período mais curto (2023-2024 apenas).
#   A pipeline de produção usa CBO6 × município × 2020-2024.
#
# SAÍDA:
#   working/rais_caged_cbo2_rgint_2023_2024.csv
#
# DEPENDÊNCIAS: tidyverse, readxl, here, zoo, scales, dlookr
###############################################################################
rm(list = ls())
gc()
options(scipen = 999)

library(tidyverse)
library(readxl)
library(here)
library(zoo)
library(scales)
library(dlookr)

input_folder <- here("rawdata")

# --- Importar regiões geográficas IBGE ---
regioes_ibge <- read_xlsx(here(input_folder,
                               "regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")) %>%
  rename(id_municipio = CD_GEOCODI) %>%
  select(id_municipio, cod_rgint, nome_rgint) %>%
  mutate(id_municipio = as.numeric(id_municipio))

# --- Importar CAGED ---
caged <- read_csv(here(input_folder, "caged_cbo_mun_2023_2024.csv")) %>%
  left_join(regioes_ibge, by = "id_municipio") %>%
  relocate(cod_rgint, nome_rgint, .before = id_municipio) %>%
  mutate(micro_regiao = str_sub(id_municipio, 1, 5),
         cbo1 = str_sub(cbo_2002, 1, 1),
         cbo2 = str_sub(cbo_2002, 1, 2),
         cbo3 = str_sub(cbo_2002, 1, 3)) %>%
  filter(cbo1 == 3) %>%
  group_by(ano, mes, sigla_uf, nome_rgint, cbo2, saldo_movimentacao) %>%
  summarise(
    media_salario = weighted.mean(media_salario_ponderada, w = n_movimentacoes, na.rm = TRUE),
    n_movimentacoes = sum(n_movimentacoes_ponderada, na.rm = TRUE),
    .groups = "drop"
  )
gc()

# --- Importar RAIS ---
rais <- read_csv(here(input_folder, "rais_cbo_mun_2022_2023.csv")) %>%
  left_join(regioes_ibge, by = "id_municipio") %>%
  relocate(cod_rgint, nome_rgint, .before = id_municipio) %>%
  mutate(ano = ano + 1,
         micro_regiao = str_sub(id_municipio, 1, 5),
         cbo1 = str_sub(cbo_2002, 1, 1),
         cbo2 = str_sub(cbo_2002, 1, 2),
         cbo3 = str_sub(cbo_2002, 1, 3)) %>%
  filter(cbo1 == 3) %>%
  group_by(ano, sigla_uf, nome_rgint, cbo2) %>%
  summarise(
    total_vinculo_ativo_3112 = sum(total_vinculo_ativo_3112_ponderado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  relocate(cbo2, .before = total_vinculo_ativo_3112)

# --- Agregar CAGED (admitidos vs desligados) ---
caged <- caged %>%
  mutate(adm = if_else(saldo_movimentacao == 1, 1, NA_real_),
         des = if_else(saldo_movimentacao == -1, 1, NA_real_)) %>%
  group_by(ano, mes, sigla_uf, nome_rgint, cbo2) %>%
  summarise(
    across(
      .cols = c(adm, des),
      .fns = list(media_sal = ~ weighted.mean(media_salario * .x, w = n_movimentacoes, na.rm = TRUE),
                  soma_mov = ~ sum(.x * n_movimentacoes, na.rm = TRUE)),
      .names = "{.fn}_{.col}"),
    dif_sal_adm_des = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
    rotatividade = soma_mov_adm - soma_mov_des,
    .groups = "drop"
  )

# --- Juntar CAGED + RAIS ---
window_mean <- function(vec, lag = 6, lead = 5) {
  n <- length(vec)
  sapply(seq_along(vec), function(i) {
    window <- vec[max(1, i - lag):min(n, i + lead)]
    mean(window, na.rm = TRUE)
  })
}

caged_rais <- left_join(caged, rais,
                        by = c("ano", "sigla_uf", "nome_rgint", "cbo2")) %>%
  mutate(tx_rotatividade = rotatividade / total_vinculo_ativo_3112) %>%
  arrange(ano, mes) %>%
  group_by(nome_rgint, cbo2) %>%
  mutate(
    across(.cols = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
           .fns = ~ if_else(.x == Inf, NA, .x)),
    across(.cols = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
           .fns = ~ window_mean(.x),
           .names = "{.col}_m12")
  ) %>%
  ungroup()

diagnostic_cagedrais <- dlookr::diagnose(caged_rais)
descstats_cegedrais  <- dlookr::describe(caged_rais)

write_csv(caged_rais, here("working", "rais_caged_cbo2_rgint_2023_2024.csv"))

message("=== caged_rais_demanda_01a.R concluído ===")