library(dplyr)
library(purrr)

anos_validos <- 2007:2024
ept_vars <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_PROF_TEC", "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC")
other_vars <- c("QT_MAT_MED", "QT_MAT_BAS", "QT_MAT_FUND", "QT_MAT_EJA")

# Define the pair you want as default
default_ept <- "QT_MAT_PROF_TEC_PROPAG"
default_other <- "QT_MAT_MED"

# Load and aggregate for each year
df_censo_estados1a <- map_dfr(anos_validos, function(ano) {
  short <- substr(as.character(ano), 3, 4)
  file <- paste0("df_censo", short, ".rda")
  
  if (file.exists(file)) {
    load(file)  # loads df_censoXX
    df <- get(paste0("df_censo", short))
    
    df %>%
      group_by(NO_UF, SG_UF) %>%
      summarise(
        ANO = ano,
        EPT = sum(.data[[default_ept]], na.rm = TRUE),
        OUTRA = sum(.data[[default_other]], na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    NULL
  }
})

# Save for later use
save(df_censo_estados1a, file="D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_estados1a.rda")


