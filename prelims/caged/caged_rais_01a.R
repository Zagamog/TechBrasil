###############################################################################
# caged_rais_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Importação e Construção do Painel CAGED-RAIS (CBO6 × Município, 2020-2024)
#
# USO NAS ABAS: E2 (Escassez — via caged_rais_02a.R)
#
# RENOMEADO DE: 1_importa_caged_rais_cria_df_mun_cbo6.R
# AUTOR ORIGINAL: Equipe FGV
#
# OBJETIVO:
#   Importar microdados CAGED (admissões/desligamentos mensais) e RAIS
#   (estoque de vínculos ativos em 31/12), ambos filtrados para CBO
#   Grande Grupo 3 (Técnicos de Nível Médio). Construir painel balanceado
#   no nível CBO6 × município × mês, calcular indicadores de fluxo:
#     - Diferença salarial admissão vs desligamento (absoluta e %)
#     - Rotatividade (soma movimentações / estoque)
#     - Estoque líquido (RAIS + acumulado CAGED)
#     - Médias móveis de 12 meses
#
# INSUMOS (S3 → rawdata/mintraemp/):
#   caged_cbo_mun_2020.csv, caged_cbo_mun_2021_2022.csv,
#   caged_cbo_mun_2023_2024.csv
#   rais_cbo_mun_2019_2021.csv, rais_cbo_mun_2022_2023.csv
#   working/ibge/df_codes_ibge.rda
#
# PROVENIÊNCIA DOS CSVs:
#   Os CSVs em rawdata/mintraemp/ são dados pré-agregados do CAGED e RAIS
#   obtidos via consultas ao Google BigQuery na plataforma Base dos Dados
#   (https://basedosdados.org). As consultas extraíram:
#     - CAGED: admissões e desligamentos por CBO × município × mês
#       Tabela: `basedosdados.br_me_caged.microdados_movimentacao`
#       Variáveis: ano, mes, sigla_uf, id_municipio, cbo_2002,
#                  saldo_movimentacao, media_salario_ponderada,
#                  n_movimentacoes_ponderada
#     - RAIS: vínculos ativos em 31/12 por CBO × município × ano
#       Tabela: `basedosdados.br_me_rais.microdados_vinculos`
#       Variáveis: ano, sigla_uf, id_municipio, cbo_2002,
#                  total_vinculo_ativo_3112_ponderado
#   Os CSVs foram gerados pela equipe FGV e uploadados manualmente ao S3.
#   Data do upload: 22 de agosto de 2025.
#   Ver README.txt em s3://techbrazildata/rawdata/mintraemp/ para mais detalhes.
#
# SAÍDA (S3):
#   working/mintraemp/rais_caged_cbo6_mun_2020_2024.rda
#   working/mintraemp/rais_caged_cbo6_mun_2020_2024.csv
#
# DEPENDÊNCIAS: tidyverse, readxl, here, zoo, scales, dlookr, aws.s3, dotenv
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

# AWS / env
library(aws.s3)
library(dotenv)
library(digest)

# -----------------------------------------------------------------------------
# 1) AWS credentials + helpers
# -----------------------------------------------------------------------------
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

update_data_from_s3 <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) {
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      save_object(object = s3_key, bucket = bucket, file = local_path)
      message(">> Baixado do S3: ", s3_key, " -> ", local_path)
    }, error = function(e) {
      stop("Falha ao baixar do S3: ", s3_key, " — ", e$message)
    })
  } else {
    message(">> Usando versão local: ", local_path)
  }
}

upload_if_missing_or_changed <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) stop("Arquivo local não encontrado: ", local_path)
  temp_s3 <- tempfile(fileext = ".bin")
  s3_exists <- tryCatch({
    save_object(object = s3_key, bucket = bucket, file = temp_s3)
    TRUE
  }, error = function(e) FALSE)
  
  if (!s3_exists) {
    message(">> Não encontrado no S3 — enviando: ", s3_key)
    put_object(file = local_path, object = s3_key, bucket = bucket)
    return(invisible(TRUE))
  }
  
  local_hash <- digest(local_path, algo = "md5")
  s3_hash    <- digest(temp_s3,   algo = "md5")
  if (local_hash != s3_hash) {
    message(">> Arquivo alterado — enviando: ", s3_key)
    put_object(file = local_path, object = s3_key, bucket = bucket)
  } else {
    message(">> S3 atualizado: ", s3_key)
  }
}

# -----------------------------------------------------------------------------
# 2) Input lists + ensure local mirrors from S3
# -----------------------------------------------------------------------------
input_folder <- here("rawdata", "mintraemp")
dir.create(input_folder, recursive = TRUE, showWarnings = FALSE)

caged_files <- c(
  "caged_cbo_mun_2020.csv",
  "caged_cbo_mun_2021_2022.csv",
  "caged_cbo_mun_2023_2024.csv"
)
rais_files <- c(
  "rais_cbo_mun_2019_2021.csv",
  "rais_cbo_mun_2022_2023.csv"
)

# download CAGED inputs
walk(caged_files, function(fname) {
  local <- file.path(input_folder, fname)
  s3key <- paste0("rawdata/mintraemp/", fname)
  update_data_from_s3(local, s3key, bucket_name)
})
# download RAIS inputs
walk(rais_files, function(fname) {
  local <- file.path(input_folder, fname)
  s3key <- paste0("rawdata/mintraemp/", fname)
  update_data_from_s3(local, s3key, bucket_name)
})
# IBGE codes
ibge_local <- here("working", "ibge", "df_codes_ibge.rda")
ibge_s3key <- "working/ibge/df_codes_ibge.rda"
update_data_from_s3(ibge_local, ibge_s3key, bucket_name)

# -----------------------------------------------------------------------------
# 3) Load + process CAGED
# -----------------------------------------------------------------------------
caged <- map_dfr(caged_files, ~ {
  read_csv(file.path(input_folder, .x), show_col_types = FALSE) %>%
    mutate(
      micro_regiao = str_sub(id_municipio, 1, 5),
      cbo1 = str_sub(cbo_2002, 1, 1),
      cbo2 = str_sub(cbo_2002, 1, 2),
      cbo3 = str_sub(cbo_2002, 1, 3),
      cbo_2002 = as.character(cbo_2002)
    ) %>%
    rename(cbo6 = cbo_2002) %>%
    filter(cbo1 == "3") %>%
    group_by(ano, mes, sigla_uf, id_municipio, cbo6, cbo3, cbo2, cbo1, saldo_movimentacao) %>%
    summarise(
      media_salario = weighted.mean(media_salario_ponderada, w = n_movimentacoes, na.rm = TRUE),
      n_movimentacoes = sum(n_movimentacoes_ponderada, na.rm = TRUE),
      .groups = "drop"
    )
})
gc()

# Collapse to admitted vs dismissed columns at (ano,mes,mun,cbo)
caged <- caged %>%
  mutate(
    adm = if_else(saldo_movimentacao == 1, 1, NA_real_),
    des = if_else(saldo_movimentacao == -1, 1, NA_real_)
  ) %>%
  group_by(ano, mes, sigla_uf, id_municipio, cbo6, cbo3, cbo2, cbo1) %>%
  summarise(
    across(
      .cols = c(adm, des),
      .fns = list(
        media_sal = ~ weighted.mean(media_salario * .x, w = n_movimentacoes, na.rm = TRUE),
        soma_mov  = ~ sum(.x * n_movimentacoes, na.rm = TRUE)
      ),
      .names = "{.fn}_{.col}"
    ),
    dif_sal_adm_des    = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
    rotatividade       = soma_mov_adm + soma_mov_des,
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 4) Load + process RAIS
# -----------------------------------------------------------------------------
rais <- map_dfr(rais_files, ~ {
  read_csv(file.path(input_folder, .x), show_col_types = FALSE) %>%
    mutate(
      ano = ano + 1,  # estoque de dezembro como denominador do ano seguinte
      micro_regiao = str_sub(id_municipio, 1, 5),
      cbo1 = str_sub(cbo_2002, 1, 1),
      cbo2 = str_sub(cbo_2002, 1, 2),
      cbo3 = str_sub(cbo_2002, 1, 3),
      cbo_2002 = as.character(cbo_2002)
    ) %>%
    rename(cbo6 = cbo_2002) %>%
    filter(cbo1 == "3") %>%
    group_by(ano, sigla_uf, id_municipio, cbo6, cbo3, cbo2, cbo1) %>%
    summarise(
      total_vinculo_ativo_3112 = sum(total_vinculo_ativo_3112_ponderado, na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  relocate(cbo6, .before = total_vinculo_ativo_3112)

gc()

# -----------------------------------------------------------------------------
# 5) Merge + balance panel + compute estoque
# -----------------------------------------------------------------------------
caged_rais <- left_join(
  caged, rais, by = c("ano", "sigla_uf", "id_municipio", "cbo6", "cbo3", "cbo2", "cbo1")
)

caged_rais_preenchido <- complete(
  caged_rais,
  ano,
  mes,
  nesting(sigla_uf, id_municipio, cbo6, cbo3, cbo2, cbo1),
  fill = list(
    soma_mov_adm = 0,
    soma_mov_des = 0,
    rotatividade = 0,
    total_vinculo_ativo_3112 = 0
  )
) %>%
  group_by(ano, id_municipio, cbo6) %>%
  mutate(total_vinculo_ativo_3112 = max(total_vinculo_ativo_3112, na.rm = TRUE)) %>%
  filter(!(ano == 2024 & mes > 9)) %>%
  arrange(id_municipio, cbo6, ano, mes) %>%
  ungroup()

# Estoque líquido
caged_rais_preenchido <- caged_rais_preenchido %>%
  group_by(ano, id_municipio, cbo6) %>%
  mutate(
    estoque_liquido = total_vinculo_ativo_3112 + cumsum(soma_mov_adm) - cumsum(soma_mov_des)
  ) %>%
  ungroup() %>%
  mutate(estoque_liquido = if_else(estoque_liquido < 0, 0, estoque_liquido))

# -----------------------------------------------------------------------------
# 6) Rolling means + indicators
# -----------------------------------------------------------------------------
window_mean <- function(vec, lag = 6, lead = 5) {
  n <- length(vec)
  sapply(seq_along(vec), function(i) {
    window <- vec[max(1, i - lag):min(n, i + lead)]
    mean(window, na.rm = TRUE)
  })
}

caged_rais_preenchido <- caged_rais_preenchido %>%
  mutate(
    tx_rotatividade = rotatividade / estoque_liquido
  ) %>%
  arrange(ano, mes) %>%
  group_by(id_municipio, cbo6, cbo3, cbo2, cbo1) %>%
  mutate(
    across(
      .cols = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
      .fns  = ~ if_else(is.infinite(.x), NA_real_, .x)
    ),
    across(
      .cols  = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
      .fns   = ~ window_mean(.x),
      .names = "{.col}_m12"
    )
  ) %>%
  ungroup() %>%
  rename(
    CO_MUN = id_municipio,
    SG_UF  = sigla_uf,
    ANO    = ano,
    MES    = mes
  )

# Diagnostics (optional)
diagnostic_cagedrais <- dlookr::diagnose(caged_rais)
descstats_cegedrais  <- dlookr::describe(caged_rais)

# -----------------------------------------------------------------------------
# 7) Join IBGE codes
# -----------------------------------------------------------------------------
load(ibge_local) # loads df_codes_ibge

df_codes_ibge <- df_codes_ibge %>%
  filter(Ano == 2021) %>%
  select(contains("UF"), ends_with("MUN"), contains("RGIMED"), ends_with("INTM"))

caged_rais_preenchido <- left_join(
  caged_rais_preenchido, df_codes_ibge, by = c("CO_MUN", "SG_UF")
) %>%
  relocate(
    NM_RGIMED, CO_RGIMED, NM_RGIINTM, CO_RGINTM, NM_UF, SG_UF, CO_UF, NM_MUN, CO_MUN,
    .before = cbo6
  )

# -----------------------------------------------------------------------------
# 8) Save locally + upload to S3
# -----------------------------------------------------------------------------
out_dir <- here("working", "mintraemp")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv_local <- file.path(here("working"), "rais_caged_cbo6_mun_2020_2024.csv")
out_rda_local <- file.path(out_dir, "rais_caged_cbo6_mun_2020_2024.rda")

readr::write_csv(caged_rais_preenchido, out_csv_local)
save(caged_rais_preenchido, file = out_rda_local)

upload_if_missing_or_changed(
  out_csv_local,
  "working/mintraemp/rais_caged_cbo6_mun_2020_2024.csv",
  bucket_name
)
upload_if_missing_or_changed(
  out_rda_local,
  "working/mintraemp/rais_caged_cbo6_mun_2020_2024.rda",
  bucket_name
)

message("=== caged_rais_01a.R concluído ===")