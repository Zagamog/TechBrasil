##############################################################################################################
# DETALHE DA OFERTA DATA LOADING (CORRECTED)
##############################################################################################################

# Load all Censo Escolar technical education data
load("df_censo_supl_tec23.rda")  # 2023 enrollment data
load("df_censo_supl_tec24.rda")  # 2024 enrollment data  
load("df_censo_notin_cnct.rda")  # Additional enrollment data (1048 rows)

dft_informality_geo_codes <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, CO_MUN, SG_UF, NM_UF, CO_UF, NM_MUN,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_informality_geo_codes <- unique(dft_informality_geo_codes, by = "CO_MUN6")



# ===== COLUMN LABELS FOR DT DISPLAY =====
column_labels_censo <- list(
  "QT_CURSO_TEC" = "Cursos",
  "QT_MAT_CURSO_TEC" = "Matrículas Total",
  "QT_MAT_CURSO_TEC_CT" = "Integrado",
  "QT_MAT_CURSO_TEC_NM" = "Normal/Magistério",
  "QT_MAT_CURSO_TEC_CONC" = "Concomitante", 
  "QT_MAT_TEC_SUBS" = "Subsequente",
  "QT_MAT_TEC_EJA" = "EJA Nível Médio"
)

# ===== SELECT COMMON COLUMNS FOR RBIND =====
common_cols <- c(
  "CO_MUN", "ANO", "TP_DEPENDENCIA", 
  "NO_AREA_CURSO_PROFISSIONAL", "NO_CURSO_EDUC_PROFISSIONAL",
  "QT_CURSO_TEC", "QT_MAT_CURSO_TEC",
  "QT_MAT_CURSO_TEC_CT", "QT_MAT_CURSO_TEC_NM", 
  "QT_MAT_CURSO_TEC_CONC", "QT_MAT_TEC_SUBS", "QT_MAT_TEC_EJA"
)

# Select and combine all datasets
df_censo_combined <- bind_rows(
  df_censo_supl_tec23[, common_cols],
  df_censo_supl_tec24[, common_cols], 
  df_censo_notin_cnct[, common_cols]
) 

# ===== JOIN WITH COURSE HIERARCHY AND GEOGRAPHY =====
censo_with_hierarchy <- df_censo_combined %>%
  left_join(
    df_exarcu %>% 
      mutate(curso_clean = str_trim(str_to_upper(`Denominação do Curso`))),
    by = "curso_clean"
  ) %>%
  left_join(dft_informality_geo_codes, by = "CO_MUN")  # Join on CO_MUN, not CO_MUN6

# ===== DEFAULTS =====
default_uf_censo <- "Rio Grande do Sul"
default_year_censo <- 2024
uf_choices_censo <- sort(unique(dft_informality_geo_codes$NM_UF))
tp_dependencia_choices <- tp_dependencia_labels