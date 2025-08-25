# rais_apo3MUN.R
library(data.table)
library(dplyr)
library(stringr)

# Load establishment-level course data
load("working/mec_inep/df_censo_supl_tec23.rda") 
load("working/mec_inep/df_censo_supl_tec24.rda")

# Load geo keys
load("working/ibge/df_codes_ibge.rda")
geo_lookup <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN, CO_MUN6)])

# Combine years and merge with geo keys
dt_cursos <- rbindlist(list(
  as.data.table(df_censo_supl_tec23),
  as.data.table(df_censo_supl_tec24)
), use.names = TRUE)

# Use existing geo mapping instead of substr conversion
dt_cursos <- merge(dt_cursos, geo_lookup, by = "CO_MUN")

# Aggregate by CO_MUN6, course, and dependency
dt_cursos_agg <- dcast(
  dt_cursos,
  CO_MUN6 + NO_CURSO_EDUC_PROFISSIONAL ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum,
  drop = FALSE
)

# Rename dependency columns
setnames(dt_cursos_agg, old = c("1", "2", "3", "4"),
         new = c("QT_MAT_CURSO_TEC_FED", "QT_MAT_CURSO_TEC_EST",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

# Add total
dt_cursos_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
              .SDcols = patterns("^QT_MAT_CURSO_TEC_")]

# Load APL data and CBO-course matches
load("working/rais/indices/dft_apl_MUN_final.rda")
qbq_cnct_matches3_ <- as.data.table(qbq_cnct_matches2)
qbq_cnct_matches3_[, cbo_4dig := substr(CodCBO, 1, 4)]
qbq_cnct_matches3 <- qbq_cnct_matches3_[, CodCBO := NULL][order(cbo_4dig, -final_score)]
qbq_cnct_matches3[, nome_curso_clean := str_remove(`Denominação do Curso`, "^Técnico em\\s+")]

# Keep top 5 matches per CBO
top5_qbq_cnct <- qbq_cnct_matches3[order(cbo_4dig, -final_score)][, .SD[1:5], by = cbo_4dig]

# Create APL-to-course mapping
apl_to_cnct_MUN <- merge(
  dft_apl_MUN_final[, .(CO_MUN6, cbo_4dig, E_mun_cbo, LQ, cbo_familia)],
  top5_qbq_cnct,
  by = "cbo_4dig",
  allow.cartesian = TRUE
) |> unique()

# Final merge with course enrollment data
apl_matri_MUN <- merge(
  apl_to_cnct_MUN,
  dt_cursos_agg,
  by.x = c("CO_MUN6", "nome_curso_clean"),
  by.y = c("CO_MUN6", "NO_CURSO_EDUC_PROFISSIONAL"),
  all.x = TRUE
)

# Clean and save
apl_matri_MUN[, c("IDX_EIXARECUR", "final_score", "semantic", "tfidf") := NULL]
apl_matri_MUN <- unique(apl_matri_MUN)
setorder(apl_matri_MUN, CO_MUN6, nome_curso_clean)

save(apl_matri_MUN, file = "working/rais/indices/apl_matri_MUN.rda")
#write.csv(apl_matri_MUN, "working/rais/indices/apl_matri_MUN.csv", row.names = FALSE)
