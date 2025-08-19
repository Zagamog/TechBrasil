# check_data.R

library(purrr)

anos_validos <- 2007:2024

# Enrollment variables you expect
qt_vars <- c(
  "QT_MAT_PROF_TEC_PROPAG", "QT_MAT_PROF_TEC",
  "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC",
  "QT_MAT_MED", "QT_MAT_BAS", "QT_MAT_FUND",
  "QT_MAT_EJA", "QT_MAT_INF", "QT_MAT_ESP"
)

missing_by_year <- map(anos_validos, function(ano) {
  short <- substr(as.character(ano), 3, 4)
  file <- file.path("working/mec_inep", paste0("df_censo", short, ".rda"))
  
  if (!file.exists(file)) return(tibble(ANO = ano, MISSING = "File not found"))
  
  load(file)
  df <- get(paste0("df_censo", short))
  
  missing_vars <- setdiff(qt_vars, names(df))
  
  if (length(missing_vars) == 0) {
    return(NULL)
  } else {
    tibble(ANO = ano, MISSING = missing_vars)
  }
}) |> bind_rows()

print(missing_by_year,n=32)
