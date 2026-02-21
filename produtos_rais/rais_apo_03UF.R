###############################################################################
# rais_apo_03UF.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Matching APLs × Cursos CNCT — Nível UF
#
# USO NAS ABAS: D2 (APLs — tabela final carregada pelo Shiny)
#
# OBJETIVO:
#   Para cada APL estadual persistente (de rais_apo_02UF.R), encontrar
#   os cursos técnicos (CNCT) mais relevantes e verificar matrículas
#   existentes naquela UF.
#
#   Usa Tecnico_Forma_Cursos_Garabed1.rda (matrículas agregadas por UF)
#   em vez dos microdados de estabelecimento.
#
# INSUMOS:
#   working/rais/indices/dft_apl_UF_final.rda  (de rais_apo_02UF.R)
#   working/qbq/qbq_cnct_matches2.rda  (matching AI CBO↔CNCT)
#   working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda  (matrículas UF)
#
# SAÍDA:
#   working/rais/indices/apl_matri_UF.rda
#     → SG_UF, cbo_4dig, cbo_familia, nome_curso_clean,
#       Denominação do Curso, E_uf_cbo, LQ,
#       QT_MAT_CURSO_TEC_FED/EST/MUN/PRI/TOT
#
# DEPENDÊNCIAS: data.table, dplyr, stringr
###############################################################################

library(data.table)
library(dplyr)
library(stringr)

###############################################################################
# CARREGAR DADOS
###############################################################################

message("=== Matching APLs × Cursos CNCT — Nível UF ===")

# APLs persistentes UF
load("working/rais/indices/dft_apl_UF_final.rda")
message(">> APLs estaduais: ", nrow(dft_apl_UF_final))

# Matching CBO↔CNCT (gerado por AI)
load("working/qbq/qbq_cnct_matches2.rda")

# Matrículas agregadas por UF (de Censo_UF_garabed1b.R)
load("working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda")

###############################################################################
# PASSO 1: AGREGAR MATRÍCULAS POR UF × CURSO × DEPENDÊNCIA
###############################################################################

message(">> Agregando matrículas UF...")

dt_garabed <- as.data.table(Tecnico_Forma_Cursos_Garabed1)
dt_garabed[, nome_curso := str_trim(NOME_CURSO)]

dt_garabed_agg <- dcast(
  dt_garabed,
  SG_UF + nome_curso ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum,
  drop = FALSE
)

setnames(dt_garabed_agg, old = c("Estadual", "Federal", "Municipal", "Privada"),
         new = c("QT_MAT_CURSO_TEC_EST", "QT_MAT_CURSO_TEC_FED",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

dt_garabed_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
               .SDcols = patterns("^QT_MAT_CURSO_TEC_")]

message(">> Cursos × UF: ", format(nrow(dt_garabed_agg), big.mark = "."))

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
apl_to_cnct_UF <- merge(
  dft_apl_UF_final[, .(SG_UF, cbo_4dig, E_uf_cbo, LQ, cbo_familia)],
  top5_qbq_cnct,
  by = "cbo_4dig",
  allow.cartesian = TRUE
) |> unique()

# Juntar com matrículas reais
apl_matri_UF <- merge(
  apl_to_cnct_UF,
  dt_garabed_agg,
  by.x = c("SG_UF", "nome_curso_clean"),
  by.y = c("SG_UF", "nome_curso"),
  all.x = TRUE
)

# Remover colunas de score
cols_remover <- intersect(
  names(apl_matri_UF),
  c("IDX_EIXARECUR", "final_score", "semantic", "tfidf")
)
if (length(cols_remover) > 0) {
  apl_matri_UF[, (cols_remover) := NULL]
}

apl_matri_UF <- unique(apl_matri_UF)
setorder(apl_matri_UF, SG_UF, nome_curso_clean)

###############################################################################
# SALVAR
###############################################################################

save(apl_matri_UF, file = "working/rais/indices/apl_matri_UF.rda")

message(">> apl_matri_UF: ", nrow(apl_matri_UF), " linhas")
message(">> UFs: ", uniqueN(apl_matri_UF$SG_UF))
message(">> Famílias CBO: ", uniqueN(apl_matri_UF$cbo_4dig))
message(">> Cursos distintos: ", uniqueN(apl_matri_UF$nome_curso_clean))
message(">> Salvo: working/rais/indices/apl_matri_UF.rda")
message("=== rais_apo_03UF.R concluído ===")