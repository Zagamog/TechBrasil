# rais_apo3UF.R

# Join the APL and cnct data

library(data.table)
library(dplyr)
library(stringr)


load("D:/Country/Brazil/TechBrazil/working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches2.rda")

# Get AI created matched from 6 to 4 digits
qbq_cnct_matches3_ <- as.data.table(qbq_cnct_matches2)
qbq_cnct_matches3_[, cbo_4dig := substr(CodCBO, 1, 4)]
qbq_cnct_matches3 <- qbq_cnct_matches3_[, CodCBO := NULL][order(cbo_4dig, -final_score)]
qbq_cnct_matches3 <- qbq_cnct_matches3 %>% dplyr::relocate(cbo_4dig, .before = IDX_EIXARECUR)



# Strip prefix from CNCT names
qbq_cnct_matches3[, nome_curso_clean := str_remove(`Denominação do Curso`, "^Técnico em\\s+")]

# Keep top 5 matches per CBO (across all UFs)
top5_qbq_cnct <- qbq_cnct_matches3[
  order(cbo_4dig, -final_score)
][
  , .SD[1:5], by = cbo_4dig
]

load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_apl_UF_final.rda")

# Merge with APL data

apl_to_cnct_UF <- merge(
  dft_apl_UF_final[, .(SG_UF, cbo_4dig, E_uf_cbo, LQ, cbo_familia)],
  top5_qbq_cnct,
  by = "cbo_4dig",
  allow.cartesian = TRUE
) |> unique()

#write.csv(apl_to_cnct_UF, "D:/Country/Brazil/TechBrazil/working/rais/indices/apl_to_cnct_UF.csv", row.names = FALSE)
load("D:/Country/Brazil/TechBrazil/working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda")

# Convert to data.table if not already
dt_garabed <- as.data.table(Tecnico_Forma_Cursos_Garabed1)

# Optional: clean up course name
dt_garabed[, nome_curso := str_trim(NOME_CURSO)]

# Step 1: Create one-hot columns by TP_DEPENDENCIA
dt_garabed_agg <- dcast(
  dt_garabed,
  SG_UF + nome_curso ~ TP_DEPENDENCIA,
  value.var = "QT_MAT_CURSO_TEC",
  fun.aggregate = sum,
  drop = FALSE
)

# Step 2: Rename for clarity
setnames(dt_garabed_agg, old = c("Estadual", "Federal", "Municipal", "Privada"),
         new = c("QT_MAT_CURSO_TEC_EST", "QT_MAT_CURSO_TEC_FED",
                 "QT_MAT_CURSO_TEC_MUN", "QT_MAT_CURSO_TEC_PRI"))

# Optional total
dt_garabed_agg[, QT_MAT_CURSO_TEC_TOT := rowSums(.SD, na.rm = TRUE),
               .SDcols = patterns("^QT_MAT_CURSO_TEC_")]



apl_matri_UF <- merge(
  apl_to_cnct_UF,
  dt_garabed_agg,
  by.x = c("SG_UF", "nome_curso_clean"),
  by.y = c("SG_UF", "nome_curso"),
  all.x = TRUE
)

# 870 obs

apl_matri_UF[, c("IDX_EIXARECUR", "final_score", "semantic", "tfidf") := NULL]
apl_matri_UF <- unique(apl_matri_UF)          # Deduplicate rows
setorder(apl_matri_UF, SG_UF, nome_curso_clean)  # Optional: sort

# 584 obs 


save(apl_matri_UF, file="D:/Country/Brazil/TechBrazil/working/rais/indices/apl_matri_UF.rda")
write.csv(apl_matri_UF, "D:/Country/Brazil/TechBrazil/working/rais/indices/apl_matri_UF.csv", row.names = FALSE)

