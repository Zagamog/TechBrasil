###############################################################################
# rais_apo_03MUNI.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Matching APLs × Cursos CNCT — Nível Municipal
#
# USO NAS ABAS: D2 (APLs — tabela final carregada pelo Shiny)
#
# OBJETIVO:
#   Para cada APL municipal persistente (de rais_apo_02MUNI.R), encontrar
#   os cursos técnicos (CNCT) mais relevantes usando o matching semântico
#   CBO↔CNCT (de qbq_cnct_matches2.rda, gerado por AI no pipeline
#   qbq_cnct_*.py), e verificar se esses cursos são ofertados no município.
#
#   O resultado permite identificar GAPS de oferta: onde há demanda
#   por ocupações (APL presente) mas não há curso técnico correspondente.
#
# INSUMOS:
#   working/rais/indices/dft_apl_MUN_final.rda  (de rais_apo_02MUNI.R)
#   working/qbq/qbq_cnct_matches2.rda  (matching AI CBO↔CNCT)
#   working/mec_inep/df_censo_supl_tec23.rda  (matrículas 2023)
#   working/mec_inep/df_censo_supl_tec24.rda  (matrículas 2024)
#   working/ibge/df_codes_ibge.rda  (CO_MUN → CO_MUN6)
#
# SAÍDA:
#   working/rais/indices/apl_matri_MUN.rda
#     → CO_MUN6, cbo_4dig, cbo_familia, nome_curso_clean,
#       Denominação do Curso, E_mun_cbo, LQ,
#       QT_MAT_CURSO_TEC_FED/EST/MUN/PRI/TOT
#     Matrículas = 0 quando o curso não é ofertado no município.
#
# DEPENDÊNCIAS: data.table, dplyr, stringr
###############################################################################

library(data.table)
library(dplyr)
library(stringr)

###############################################################################
# CARREGAR DADOS
###############################################################################

message("=== Matching APLs × Cursos CNCT — Nível Municipal ===")

# APLs persistentes municipais
load("working/rais/indices/dft_apl_MUN_final.rda")
message(">> APLs municipais: ", nrow(dft_apl_MUN_final))

# Matching CBO↔CNCT (gerado por AI)
load("working/qbq/qbq_cnct_matches2.rda")

# Matrículas por estabelecimento (Censo Escolar)
load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")

# Geo keys (CO_MUN 7 dígitos → CO_MUN6)
load("working/ibge/df_codes_ibge.rda")
geo_lookup <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN, CO_MUN6)])

###############################################################################
# PASSO 1: AGREGAR MATRÍCULAS POR MUNICÍPIO × CURSO × DEPENDÊNCIA
###############################################################################

message(">> Agregando matrículas municipais...")

dt_cursos <- rbindlist(list(
  as.data.table(df_censo_supl_tec23),
  as.data.table(df_censo_supl_tec24)
), use.names = TRUE)

# Mapear CO_MUN (7 dígitos) → CO_MUN6
dt_cursos <- merge(dt_cursos, geo_lookup, by = "CO_MUN")

# Pivot: uma coluna por dependência administrativa
dt_cursos_agg <- dcast(
  dt_cursos,
  CO_MUN6 + NO_CURSO_EDUC_PROFISSIONAL ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum,
  drop = FALSE
)

setnames(dt_cursos_agg, old = c("1", "2", "3", "4"),
         new = c("QT_MAT_CURSO_TEC_FED", "QT_MAT_CURSO_TEC_EST",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

dt_cursos_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
              .SDcols = patterns("^QT_MAT_CURSO_TEC_")]

message(">> Cursos × município: ", format(nrow(dt_cursos_agg), big.mark = "."))

###############################################################################
# PASSO 2: PREPARAR MATCHING CBO → CNCT (TOP 5 POR CBO 4 DÍGITOS)
###############################################################################

message(">> Preparando matching CBO → CNCT...")

qbq_cnct_matches3 <- as.data.table(qbq_cnct_matches2)
qbq_cnct_matches3[, cbo_4dig := substr(CodCBO, 1, 4)]
qbq_cnct_matches3[, CodCBO := NULL]
setorder(qbq_cnct_matches3, cbo_4dig, -final_score)

# Limpar nome do curso (remover prefixo "Técnico em ")
qbq_cnct_matches3[, nome_curso_clean := str_remove(
  `Denominação do Curso`, "^Técnico em\\s+"
)]

# Top 5 matches por família CBO
top5_qbq_cnct <- qbq_cnct_matches3[, .SD[1:5], by = cbo_4dig]

message(">> Matches CBO → CNCT: ", nrow(top5_qbq_cnct),
        " (", uniqueN(top5_qbq_cnct$cbo_4dig), " famílias)")

###############################################################################
# PASSO 3: JUNTAR APLs → CURSOS → MATRÍCULAS
###############################################################################

message(">> Juntando APLs × cursos × matrículas...")

# APL → top 5 cursos correspondentes
apl_to_cnct_MUN <- merge(
  dft_apl_MUN_final[, .(CO_MUN6, cbo_4dig, E_mun_cbo, LQ, cbo_familia)],
  top5_qbq_cnct,
  by = "cbo_4dig",
  allow.cartesian = TRUE
) |> unique()

# Juntar com matrículas reais
apl_matri_MUN <- merge(
  apl_to_cnct_MUN,
  dt_cursos_agg,
  by.x = c("CO_MUN6", "nome_curso_clean"),
  by.y = c("CO_MUN6", "NO_CURSO_EDUC_PROFISSIONAL"),
  all.x = TRUE
)

# Remover colunas de score (não necessárias na saída final)
cols_remover <- intersect(
  names(apl_matri_MUN),
  c("IDX_EIXARECUR", "final_score", "semantic", "tfidf")
)
if (length(cols_remover) > 0) {
  apl_matri_MUN[, (cols_remover) := NULL]
}

apl_matri_MUN <- unique(apl_matri_MUN)
setorder(apl_matri_MUN, CO_MUN6, nome_curso_clean)

# NAs em matrículas → 0 (curso não ofertado no município)
enrollment_cols <- c("QT_MAT_CURSO_TEC_FED", "QT_MAT_CURSO_TEC_EST",
                     "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI",
                     "QT_MAT_CURSO_TEC_TOT")
apl_matri_MUN[, (enrollment_cols) := lapply(.SD, function(x)
  fifelse(is.na(x), 0, x)), .SDcols = enrollment_cols]

###############################################################################
# SALVAR
###############################################################################

save(apl_matri_MUN, file = "working/rais/indices/apl_matri_MUN.rda")

message(">> apl_matri_MUN: ", format(nrow(apl_matri_MUN), big.mark = "."), " linhas")
message(">> Municípios: ", uniqueN(apl_matri_MUN$CO_MUN6))
message(">> Famílias CBO: ", uniqueN(apl_matri_MUN$cbo_4dig))
message(">> Cursos distintos: ", uniqueN(apl_matri_MUN$nome_curso_clean))
message(">> Salvo: working/rais/indices/apl_matri_MUN.rda")
message("=== rais_apo_03MUNI.R concluído ===")