# modelo_ept3a.R 

library(fixest)
library(dplyr)
library(data.table)

load("D:/Country/Brazil/TechBrazil/working/rais/df_model_ept1a.rda")
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

model_ols_simple <- feols(log_QT_MAT_CURSO_TEC ~ 
                            log(pib_per_capita) + sector_alignment |
                            economic_sector + SG_UF + TP_DEPENDENCIA + ANO,
                          data = df_model_ept1a, cluster = ~SG_UF)


residuals_ols <- residuals(model_ols_simple)

# Create residuals dataframe
residuals_df_ols <- df_model_ept1a %>%
  mutate(residual = residuals_ols) %>%
  select(ANO, SG_UF, TP_DEPENDENCIA, economic_sector, 
         QT_MAT_CURSO_TEC, residual, sector_alignment, pib_per_capita)


dft_geo_keys <- as.data.table(df_codes_ibge)[
  , .(SG_UF, NM_UF)
]
dft_geo_keys <- unique(dft_geo_keys, by = "SG_UF")

df_residuals_ols <- residuals_df_ols %>%
  left_join(
    dft_geo_keys %>% distinct(SG_UF, NM_UF), 
    by = "SG_UF"
  )

glimpse(df_residuals_ols)


save(df_residuals_ols, file="D:/Country/Brazil/TechBrazil/working/rais/df_residuals_ols.rda")

summary(model_ols_simple)


