# rais_apo3RGINTM.R
library(data.table)
library(dplyr)
library(stringr)

# Load course data and geo keys
load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")
load("working/ibge/df_codes_ibge.rda")

# Use proper geo mapping (not substr)
geo_lookup <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN, CO_MUN6, CO_RGINTM)])

dt_cursos <- rbindlist(list(as.data.table(df_censo_supl_tec23), as.data.table(df_censo_supl_tec24)))
dt_cursos <- merge(dt_cursos, geo_lookup, by = "CO_MUN")

# Aggregate by CO_RGINTM
dt_cursos_rgintm <- dt_cursos[, .(
  QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE)
), by = .(CO_RGINTM, NO_CURSO_EDUC_PROFISSIONAL, TP_DEPENDENCIA)]

dt_cursos_agg <- dcast(
  dt_cursos_rgintm,
  CO_RGINTM + NO_CURSO_EDUC_PROFISSIONAL ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum, drop = FALSE
)

setnames(dt_cursos_agg, old = c("1", "2", "3", "4"),
         new = c("QT_MAT_CURSO_TEC_FED", "QT_MAT_CURSO_TEC_EST",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

dt_cursos_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
              .SDcols = patterns("^QT_MAT_CURSO_TEC_")]

# Load APL data and CBO matching
load("working/rais/indices/dft_apl_RGINTM_final.rda")

# Fix the CBO matching logic
qbq_cnct_matches3_ <- as.data.table(qbq_cnct_matches2)
qbq_cnct_matches3_[, cbo_4dig := substr(CodCBO, 1, 4)]
qbq_cnct_matches3 <- qbq_cnct_matches3_[, CodCBO := NULL][order(cbo_4dig, -final_score)]  # Missing line
qbq_cnct_matches3[, nome_curso_clean := str_remove(`Denominação do Curso`, "^Técnico em\\s+")]
top5_qbq_cnct <- qbq_cnct_matches3[order(cbo_4dig, -final_score)][, .SD[1:5], by = cbo_4dig]

# Create APL-to-course mapping (was missing)
apl_to_cnct_RGINTM <- merge(
  dft_apl_RGINTM_final[, .(CO_RGINTM, cbo_4dig, E_rgintm_cbo, LQ, cbo_familia)],
  top5_qbq_cnct, by = "cbo_4dig", allow.cartesian = TRUE
) |> unique()

# Final merge
apl_matri_RGINTM <- merge(
  apl_to_cnct_RGINTM, dt_cursos_agg,
  by.x = c("CO_RGINTM", "nome_curso_clean"),
  by.y = c("CO_RGINTM", "NO_CURSO_EDUC_PROFISSIONAL"),
  all.x = TRUE
)

# Clean and save
apl_matri_RGINTM[, c("IDX_EIXARECUR", "final_score", "semantic", "tfidf") := NULL]
apl_matri_RGINTM <- unique(apl_matri_RGINTM)

save(apl_matri_RGINTM, file = "working/rais/indices/apl_matri_RGINTM.rda")
#write.csv(apl_matri_RGINTM, "working/rais/indices/apl_matri_RGINTM.csv", row.names = FALSE)