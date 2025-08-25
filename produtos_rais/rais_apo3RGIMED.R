# rais_apo3RGIMED.R
library(data.table)
library(dplyr)

# Load course data and geo keys
load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")
load("working/ibge/df_codes_ibge.rda")

# For região imediata (rais_apo3RGIMED.R)
geo_lookup <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN, CO_MUN6, CO_RGIMED)])

dt_cursos <- rbindlist(list(as.data.table(df_censo_supl_tec23), as.data.table(df_censo_supl_tec24)))
dt_cursos <- merge(dt_cursos, geo_lookup, by = "CO_MUN")

# Then aggregate by CO_RGIMED instead of CO_MUN6
dt_cursos_rgimed <- dt_cursos[, .(
  QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE)
), by = .(CO_RGIMED, NO_CURSO_EDUC_PROFISSIONAL, TP_DEPENDENCIA)]

# Create dependency columns
dt_cursos_agg <- dcast(
  dt_cursos_rgimed,
  CO_RGIMED + NO_CURSO_EDUC_PROFISSIONAL ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum,
  drop = FALSE
)



setnames(dt_cursos_agg, old = c("1", "2", "3", "4"),
         new = c("QT_MAT_CURSO_TEC_FED", "QT_MAT_CURSO_TEC_EST",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

dt_cursos_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
              .SDcols = patterns("^QT_MAT_CURSO_TEC_")]

# Load APL data and merge
# Load APL data and merge
load("working/rais/indices/dft_apl_RGIMED_final.rda")
qbq_cnct_matches3_ <- as.data.table(qbq_cnct_matches2)
qbq_cnct_matches3_[, cbo_4dig := substr(CodCBO, 1, 4)]
qbq_cnct_matches3 <- qbq_cnct_matches3_[, CodCBO := NULL][order(cbo_4dig, -final_score)]  # Missing line
qbq_cnct_matches3[, nome_curso_clean := str_remove(`Denominação do Curso`, "^Técnico em\\s+")]
top5_qbq_cnct <- qbq_cnct_matches3[order(cbo_4dig, -final_score)][, .SD[1:5], by = cbo_4dig]  # Remove underscore

apl_to_cnct_RGIMED <- merge(
  dft_apl_RGIMED_final[, .(CO_RGIMED, cbo_4dig, E_rgimed_cbo, LQ, cbo_familia)],
  top5_qbq_cnct, by = "cbo_4dig", allow.cartesian = TRUE
) |> unique()

apl_matri_RGIMED <- merge(
  apl_to_cnct_RGIMED, dt_cursos_agg,
  by.x = c("CO_RGIMED", "nome_curso_clean"),
  by.y = c("CO_RGIMED", "NO_CURSO_EDUC_PROFISSIONAL"),
  all.x = TRUE
)

apl_matri_RGIMED[, c("IDX_EIXARECUR", "final_score", "semantic", "tfidf") := NULL]
apl_matri_RGIMED <- unique(apl_matri_RGIMED)

save(apl_matri_RGIMED, file = "working/rais/indices/apl_matri_RGIMED.rda")
#write.csv(apl_matri_RGIMED, "working/rais/indices/apl_matri_RGIMED.csv", row.names = FALSE)