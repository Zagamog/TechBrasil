###############################################################################
# caged_rais_02a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Pré-processamento CAGED-RAIS → Dados para Shiny Aba E2
#
# USO NAS ABAS: E2 (Escassez de Profissionais Técnicos)
#
# RENOMEADO DE: 2_preprocess_caged_rais_shiny.R
# AUTOR ORIGINAL: Equipe FGV
#
# OBJETIVO:
#   Ler o painel CAGED-RAIS (de caged_rais_01a.R), mapear CBO6 → Cursos
#   CNCT via crosswalk "Curso_CBO V6 Base.xlsx", agregar ao nível
#   UF × Curso × mês, construir linhas "Brasil", e gerar tabela de
#   ranking de escassez com tipologia:
#     - Alerta de Escassez
#     - Tendência de Escassez
#     - Situação Estável
#     - Sinal de Excesso
#
# INSUMOS (S3):
#   working/mintraemp/rais_caged_cbo6_mun_2020_2024.rda  (de caged_rais_01a.R)
#   working/Curso_CBO V6 Base.xlsx  (crosswalk CBO → Curso CNCT)
#
# SAÍDAS (S3):
#   working/mintraemp/caged_rais_cnct_2020_2024_shiny.rda  → caged_rais_curso
#   working/mintraemp/df_ranking_cursos_caged_rais.rda  → tabela_ranking_cursos
#
# NOTA: O Shiny carrega estes arquivos diretamente na inicialização:
#   load("caged_rais_cnct_2020_2024_shiny.rda")
#   load("df_ranking_cursos_caged_rais.rda")
#
# DEPENDÊNCIAS: tidyverse, readxl, here, dlookr, scales, aws.s3, dotenv
###############################################################################
rm(list = ls())
gc()
options(scipen = 999)

library(tidyverse)
library(readxl)
library(here)
library(dlookr)
library(scales)

# AWS / env
library(aws.s3)
library(dotenv)
library(digest)

# Média móvel centrada
window_mean <- function(vec, lag = 6, lead = 5) {
  n <- length(vec)
  sapply(seq_along(vec), function(i) {
    window <- vec[max(1, i - lag):min(n, i + lead)]
    mean(window, na.rm = TRUE)
  })
}

# -----------------------------------------------------------------------------
# 1) AWS helpers
# -----------------------------------------------------------------------------
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

update_data_from_s3 <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message(">> Baixado do S3: ", s3_path, " -> ", local_path)
    }, error = function(e) {
      stop("Falha ao baixar do S3: ", s3_path, " — ", e$message)
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
# 2) Source paths
# -----------------------------------------------------------------------------
# Excel crosswalk CBO → Curso CNCT
xl_s3    <- "working/Curso_CBO V6 Base.xlsx"
xl_local <- here("working", "Curso_CBO V6 Base.xlsx")
update_data_from_s3(xl_local, xl_s3, bucket_name)

# RAIS-CAGED painel municipal (de caged_rais_01a.R)
rda_in_s3    <- "working/mintraemp/rais_caged_cbo6_mun_2020_2024.rda"
rda_in_local <- here("working", "mintraemp", "rais_caged_cbo6_mun_2020_2024.rda")
update_data_from_s3(rda_in_local, rda_in_s3, bucket_name)

# Output folder
out_local_dir <- here("working", "oferta_demanda_ept")
dir.create(out_local_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 3) Read inputs
# -----------------------------------------------------------------------------
# Crosswalk Área–CBO e Eixo–Área–Curso
cnct_cbo <- read_xlsx(xl_local) %>%
  filter(Final >= 0.3) %>%
  select(Cod_Curso, Curso, Co_Ocupacao, Ocupacao, Final)

eixo_area_curso <- read_xlsx(xl_local, sheet = "Curso") %>%
  select(Eixo_Tecnologico, Area_Tecnologica, Cod_Curso)

cnct_cbo <- inner_join(eixo_area_curso, cnct_cbo, by = "Cod_Curso")

# Painel RAIS-CAGED
load(rda_in_local) # → caged_rais_preenchido

if (!exists("caged_rais_preenchido")) {
  stop("Objeto `caged_rais_preenchido` não encontrado em: ", rda_in_local)
}

# -----------------------------------------------------------------------------
# 4) Truncate outliers
# -----------------------------------------------------------------------------
caged_rais_preenchido <- caged_rais_preenchido %>%
  mutate(media_sal_adm = pmax(pmin(media_sal_adm, 10000), 1000),
         media_sal_des = pmax(pmin(media_sal_des, 10000), 1000))

# -----------------------------------------------------------------------------
# 5) Aggregate at UF–CBO6, smooth indicators
# -----------------------------------------------------------------------------
caged_rais_preenchido <- group_by(caged_rais_preenchido, ANO, MES, CO_UF, NM_UF, cbo6) %>%
  summarise(
    media_sal_adm = weighted.mean(media_sal_adm, w = soma_mov_adm, na.rm = TRUE),
    media_sal_des = weighted.mean(media_sal_des, w = soma_mov_des, na.rm = TRUE),
    dif_sal_adm_des = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
    across(c(contains("soma_mov"), total_vinculo_ativo_3112, estoque_liquido),
           ~ sum(.x, na.rm = TRUE)),
    rotatividade = soma_mov_adm + soma_mov_des,
    tx_rotatividade_rais = rotatividade / estoque_liquido,
    tx_rotatividade = rotatividade / soma_mov_des
  ) %>%
  group_by(ANO, MES, CO_UF, NM_UF, cbo6) %>%
  mutate(across(.cols = c(dif_sal_adm_des_pc, tx_rotatividade, tx_rotatividade_rais),
                ~ if_else(.x == Inf, NA, .x)),
         across(.cols = c(dif_sal_adm_des_pc, tx_rotatividade, tx_rotatividade_rais),
                ~ window_mean(.x),
                .names = "{.col}_m12")) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 6) Map to Cursos and aggregate by Curso
# -----------------------------------------------------------------------------
inner_join(caged_rais_preenchido, cnct_cbo, by = c("cbo6" = "Co_Ocupacao")) %>%
  rename(cbo6_nome = Ocupacao, Proximidade = Final) %>%
  select(ANO, MES, CO_UF, NM_UF, Eixo_Tecnologico, Area_Tecnologica, Cod_Curso,
         Curso, Proximidade, cbo6, cbo6_nome, total_vinculo_ativo_3112, media_sal_adm,
         soma_mov_adm, media_sal_des, soma_mov_des, dif_sal_adm_des_pc_m12,
         tx_rotatividade_m12, estoque_liquido) -> caged_rais_curso

# Aggregate by (ANO,MES,UF,Curso)
caged_rais_curso <- caged_rais_curso %>%
  group_by(ANO, MES, NM_UF, Eixo_Tecnologico, Area_Tecnologica, Curso) %>%
  summarise(
    media_sal_adm = weighted.mean(media_sal_adm, w = soma_mov_adm, na.rm = TRUE),
    media_sal_des = weighted.mean(media_sal_des, w = soma_mov_des, na.rm = TRUE),
    dif_sal_adm_des = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
    across(c(contains("soma_mov"), total_vinculo_ativo_3112, estoque_liquido),
           ~ sum(.x, na.rm = TRUE)),
    rotatividade = soma_mov_adm + soma_mov_des,
    tx_rotatividade_rais = rotatividade / estoque_liquido,
    tx_rotatividade = rotatividade / soma_mov_des
  ) %>%
  group_by(NM_UF, Curso) %>%
  mutate(across(c(dif_sal_adm_des_pc, tx_rotatividade, tx_rotatividade_rais),
                ~ if_else(is.infinite(.x), NA, .x)),
         across(c(dif_sal_adm_des_pc, tx_rotatividade, tx_rotatividade_rais),
                ~ window_mean(.x),
                .names = "{.col}_m12")) %>%
  group_by(ANO, MES, Eixo_Tecnologico, Curso) %>%
  mutate(estoque_liquido_br = sum(if_else(NM_UF == "Brasil", NA, estoque_liquido),
                                  na.rm = TRUE)) %>%
  ungroup() %>%
  relocate(estoque_liquido_br, .after = estoque_liquido)

# -----------------------------------------------------------------------------
# 7) Build "Brasil" rows and bind
# -----------------------------------------------------------------------------
caged_rais_curso_brasil <- caged_rais_curso %>%
  group_by(ANO, MES, Eixo_Tecnologico, Area_Tecnologica, Curso) %>%
  summarise(
    NM_UF = "Brasil",
    media_sal_adm = weighted.mean(media_sal_adm, w = soma_mov_adm, na.rm = TRUE),
    media_sal_des = weighted.mean(media_sal_des, w = soma_mov_des, na.rm = TRUE),
    dif_sal_adm_des = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
    across(c(contains("soma_mov"), total_vinculo_ativo_3112, estoque_liquido),
           ~ sum(.x, na.rm = TRUE)),
    rotatividade = soma_mov_adm + soma_mov_des,
    tx_rotatividade = rotatividade / estoque_liquido
  ) %>%
  group_by(NM_UF, Curso) %>%
  mutate(across(c(dif_sal_adm_des_pc, tx_rotatividade),
                ~ if_else(is.infinite(.x), NA, .x)),
         across(c(dif_sal_adm_des_pc, tx_rotatividade),
                ~ window_mean(.x),
                .names = "{.col}_m12")) %>%
  mutate(estoque_liquido_br = estoque_liquido) %>%
  relocate(estoque_liquido_br, .after = estoque_liquido) %>%
  ungroup()

# Junta com a base nível UF
caged_rais_curso <- bind_rows(caged_rais_curso, caged_rais_curso_brasil)

# -----------------------------------------------------------------------------
# 8) Save + upload main .rda and .csv
# -----------------------------------------------------------------------------
out_rda_local <- file.path(out_local_dir, "caged_rais_cnct_2020_2024_shiny.rda")
save(caged_rais_curso, file = out_rda_local)

out_csv_local <- file.path(out_local_dir, "caged_rais_cnct_2020_2024_shiny.csv")
readr::write_csv(caged_rais_curso, out_csv_local)

upload_if_missing_or_changed(
  out_rda_local,
  "working/mintraemp/caged_rais_cnct_2020_2024_shiny.rda",
  bucket_name
)
upload_if_missing_or_changed(
  out_csv_local,
  "working/mintraemp/caged_rais_cnct_2020_2024_shiny.csv",
  bucket_name
)

# -----------------------------------------------------------------------------
# 9) Build ranking table
# -----------------------------------------------------------------------------
tabela_ranking_cursos <- caged_rais_curso %>%
  group_by(ANO, MES, NM_UF) %>%
  mutate(across(.cols = c(dif_sal_adm_des_pc_m12, tx_rotatividade_m12, estoque_liquido),
                .fns = ~ percent_rank(.x) * 100,
                .names = 'p_{.col}')) %>%
  group_by(Curso, NM_UF) %>%
  mutate(
    dif_sal_adm_des_pc = if_else(is.infinite(dif_sal_adm_des_pc), NA, dif_sal_adm_des_pc),
    dif_sal_adm_des_pc_m12 = window_mean(dif_sal_adm_des_pc, lag = 11, lead = 0),
    soma_mov_adm_media_ano = window_mean(soma_mov_adm, lag = 11, lead = 0),
    `Estimativa Demanda Vagas` = window_mean(estoque_liquido, lag = 11, lead = 0) * 0.15,
    `Demanda de vagas em relação ao total Brasil` =
      `Estimativa Demanda Vagas` / (window_mean(estoque_liquido_br, lag = 11, lead = 0) * 0.15),
    tipologia_escassez = case_when(
      p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 60
      ~ "Alerta de Escassez",
      (p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido < 60) |
        (between(p_dif_sal_adm_des_pc_m12, 74.9999, 50) & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 50)
      ~ "Tendência de Escassez",
      (between(p_dif_sal_adm_des_pc_m12, 49.999, 25) | p_estoque_liquido >= 50)
      ~ "Situação Estável",
      p_dif_sal_adm_des_pc_m12 < 25
      ~ "Sinal de Excesso",
      soma_mov_adm_media_ano < 30
      ~ "Sem fluxo suficiente - INDICADORES DEIXAM DE SER INFORMATIVOS",
      TRUE ~ "Indisponível/Indeterminado"
    )
  ) %>%
  group_by(NM_UF) %>%
  filter(ANO == max(ANO, na.rm = TRUE)) %>%
  filter(MES == max(MES, na.rm = TRUE)) %>%
  mutate(dif_sal_adm_des_pc_pond_m12 = scales::rescale(
    dif_sal_adm_des_pc_m12 * estoque_liquido, to = c(0, 1000))) %>%
  arrange(desc(dif_sal_adm_des_pc_pond_m12)) %>%
  filter(tipologia_escassez %in% c("Alerta de Escassez", "Tendência de Escassez")) %>%
  slice_head(n = 10) %>%
  transmute(
    `Eixo - Curso` = paste(Eixo_Tecnologico, Curso, sep = " - "),
    `Situação da Escassez` = tipologia_escassez,
    `Estimativa Demanda Vagas` = round(`Estimativa Demanda Vagas`),
    `Demanda de vagas em relação ao total Brasil (%)` =
      `Demanda de vagas em relação ao total Brasil` * 100,
    `Indicador de Escassez (escala 0 a 1000)` = round(dif_sal_adm_des_pc_pond_m12)
  )

# -----------------------------------------------------------------------------
# 10) Save + upload ranking table
# -----------------------------------------------------------------------------
rank_rda_local <- file.path(out_local_dir, "df_ranking_cursos_caged_rais.rda")
save(tabela_ranking_cursos, file = rank_rda_local)

rank_csv_local <- file.path(out_local_dir, "df_ranking_cursos_caged_rais.csv")
readr::write_csv(tabela_ranking_cursos, rank_csv_local)

upload_if_missing_or_changed(
  rank_rda_local,
  "working/mintraemp/df_ranking_cursos_caged_rais.rda",
  bucket_name
)
upload_if_missing_or_changed(
  rank_csv_local,
  "working/mintraemp/df_ranking_cursos_caged_rais.csv",
  bucket_name
)

message("=== caged_rais_02a.R concluído ===")